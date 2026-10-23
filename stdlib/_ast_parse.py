"""Convert Mule's owned syntax into Python's public AST node interface."""

import _ast as api
from mule import ast as syntax
from mule.parser import parse as _parse
from mule.parser.lexer import lex

_BINARY = {
    "+": api.Add,
    "-": api.Sub,
    "*": api.Mult,
    "/": api.Div,
    "//": api.FloorDiv,
    "%": api.Mod,
    "@": api.MatMult,
    "**": api.Pow,
    "<<": api.LShift,
    ">>": api.RShift,
    "&": api.BitAnd,
    "^": api.BitXor,
    "|": api.BitOr,
}
_UNARY = {"+": api.UAdd, "-": api.USub, "~": api.Invert, "not": api.Not}
_BOOLEAN = {"and": api.And, "or": api.Or}
_COMPARISON = {
    "==": api.Eq,
    "!=": api.NotEq,
    "<": api.Lt,
    "<=": api.LtE,
    ">": api.Gt,
    ">=": api.GtE,
    "is": api.Is,
    "is not": api.IsNot,
    "in": api.In,
    "not in": api.NotIn,
}


class _Locations:
    def __init__(self, source, filename):
        self.lines = source.split("\n")
        self.ascii = source.isascii()

    def column(self, position):
        if self.ascii:
            return position.column
        line = self.lines[position.line - 1]
        if line.isascii():
            return position.column
        return len(line[: position.column].encode("utf-8"))

    def set(self, node, result, start=None, end=None):
        if "lineno" in result._attributes:
            start = node.span.start if start is None else start
            end = node.span.end if end is None else end
            if self.ascii:
                result.lineno, result.col_offset = start.line, start.column
                result.end_lineno, result.end_col_offset = end.line, end.column
            else:
                result.lineno, result.col_offset = start.line, self.column(start)
                result.end_lineno, result.end_col_offset = end.line, self.column(end)
        return result

    def definition_start(self, node, keyword):
        if not node.decorators:
            return node.span.start
        prefixes = ("async ", "async\t", "def ", "def\t") if keyword == "def" else (
            "class ",
            "class\t",
            "class(",
            "class[",
            "class:",
        )
        line_no = node.decorators[-1].span.end.line + 1
        while line_no <= len(self.lines):
            line = self.lines[line_no - 1]
            stripped = line.lstrip()
            if stripped.startswith(prefixes):
                return syntax.Position(line_no, len(line) - len(stripped))
            line_no += 1
        raise RuntimeError("parsed definition has no definition token")


class _SimpleName(syntax.Visitor):
    def visit_Node(self, node):
        return False

    def visit_Name(self, node):
        return True


class _Convert(syntax.Visitor):
    def __init__(self, locations, context=None):
        self.locations = locations
        self.context = api.Load() if context is None else context

    def visit_Node(self, node):
        raise NotImplementedError(f"AST conversion for {type(node).__name__}")

    def convert(self, node):
        return None if node is None else node.accept(self)

    def many(self, nodes):
        return [self.convert(node) for node in nodes]

    def target(self, node, deleting=False):
        return (
            None
            if node is None
            else node.accept(
                _Convert(self.locations, api.Del() if deleting else api.Store())
            )
        )

    def loaded(self, node):
        return node.accept(_Convert(self.locations))

    def finish(self, node, result):
        return self.locations.set(node, result)

    def visit_Module(self, node):
        return api.Module(self.many(node.statements), self.many(node.type_ignores))

    def visit_ExpressionInput(self, node):
        return api.Expression(self.convert(node.expression))

    def visit_InteractiveInput(self, node):
        return api.Interactive(self.many(node.statements))

    def visit_TypeIgnore(self, node):
        return api.TypeIgnore(node.line, node.tag)

    def visit_Name(self, node):
        return self.finish(node, api.Name(node.identifier, self.context))

    def visit_Literal(self, node):
        return self.finish(node, api.Constant(node.value))

    visit_IntegerLiteral = visit_Literal
    visit_FloatLiteral = visit_Literal
    visit_ComplexLiteral = visit_Literal
    visit_BooleanLiteral = visit_Literal
    visit_NoneLiteral = visit_Literal
    visit_BytesLiteral = visit_Literal
    visit_EllipsisLiteral = visit_Literal

    def visit_TextLiteral(self, node):
        first = self.locations.lines[node.span.start.line - 1][
            node.span.start.column :
        ][:1]
        return self.finish(
            node, api.Constant(node.value, "u" if first in ("u", "U") else None)
        )

    def visit_Binary(self, node):
        return self.finish(
            node,
            api.BinOp(
                self.convert(node.left),
                _BINARY[node.operator](),
                self.convert(node.right),
            ),
        )

    def visit_Unary(self, node):
        return self.finish(
            node, api.UnaryOp(_UNARY[node.operator](), self.convert(node.operand))
        )

    def visit_Boolean(self, node):
        return self.finish(
            node, api.BoolOp(_BOOLEAN[node.operator](), self.many(node.operands))
        )

    def visit_Comparison(self, node):
        return self.finish(
            node,
            api.Compare(
                self.convert(node.left),
                [_COMPARISON[operator]() for operator in node.operators],
                self.many(node.comparators),
            ),
        )

    def visit_Conditional(self, node):
        return self.finish(
            node,
            api.IfExp(
                self.convert(node.condition),
                self.convert(node.then),
                self.convert(node.otherwise),
            ),
        )

    def visit_Parameter(self, node):
        result = api.arg(node.name, self.convert(node.annotation), node.type_comment)
        start = node.span.start
        if node.kind in (
            syntax.ParameterKind.VAR_POSITIONAL,
            syntax.ParameterKind.VAR_KEYWORD,
        ):
            count = 1 if node.kind == syntax.ParameterKind.VAR_POSITIONAL else 2
            start = syntax.Position(start.line, start.column + count)
        end = (
            node.annotation.span.end
            if node.annotation is not None
            else syntax.Position(start.line, start.column + len(node.name))
        )
        return self.locations.set(node, result, start, end)

    def arguments(self, parameters):
        positional, normal, keywords, defaults, keyword_defaults = [], [], [], [], []
        vararg, kwarg = None, None
        for parameter in parameters:
            converted = self.convert(parameter)
            if parameter.kind == syntax.ParameterKind.POSITIONAL_ONLY:
                positional.append(converted)
            elif parameter.kind == syntax.ParameterKind.POSITIONAL_OR_KEYWORD:
                normal.append(converted)
            elif parameter.kind == syntax.ParameterKind.KEYWORD_ONLY:
                keywords.append(converted)
                keyword_defaults.append(self.convert(parameter.default))
            elif parameter.kind == syntax.ParameterKind.VAR_POSITIONAL:
                vararg = converted
            elif parameter.kind == syntax.ParameterKind.VAR_KEYWORD:
                kwarg = converted
            if parameter.default is not None and parameter.kind in (
                syntax.ParameterKind.POSITIONAL_ONLY,
                syntax.ParameterKind.POSITIONAL_OR_KEYWORD,
            ):
                defaults.append(self.convert(parameter.default))
        return api.arguments(
            positional, normal, vararg, keywords, keyword_defaults, kwarg, defaults
        )

    def visit_Lambda(self, node):
        return self.finish(
            node, api.Lambda(self.arguments(node.parameters), self.convert(node.body))
        )

    def visit_NamedExpression(self, node):
        return self.finish(
            node, api.NamedExpr(self.target(node.target), self.convert(node.value))
        )

    def visit_Yield(self, node):
        cls = api.YieldFrom if node.from_iterable else api.Yield
        return self.finish(node, cls(self.convert(node.value)))

    def visit_Await(self, node):
        return self.finish(node, api.Await(self.convert(node.value)))

    def visit_Attribute(self, node):
        return self.finish(
            node, api.Attribute(self.loaded(node.value), node.attribute, self.context)
        )

    def call_arguments(self, arguments):
        positional, keywords = [], []
        for argument in arguments:
            value = self.convert(argument.value)
            if argument.kind == syntax.ArgumentKind.POSITIONAL:
                positional.append(value)
            elif argument.kind == syntax.ArgumentKind.STARRED:
                positional.append(self.finish(argument, api.Starred(value, api.Load())))
            else:
                name = (
                    argument.keyword
                    if argument.kind == syntax.ArgumentKind.KEYWORD
                    else None
                )
                keywords.append(self.finish(argument, api.keyword(name, value)))
        return positional, keywords

    def visit_Call(self, node):
        positional, keywords = self.call_arguments(node.arguments)
        return self.finish(
            node, api.Call(self.convert(node.function), positional, keywords)
        )

    def visit_Subscript(self, node):
        return self.finish(
            node,
            api.Subscript(
                self.loaded(node.value), self.loaded(node.index), self.context
            ),
        )

    def visit_Slice(self, node):
        return self.finish(
            node,
            api.Slice(
                self.convert(node.lower),
                self.convert(node.upper),
                self.convert(node.step),
            ),
        )

    def visit_Unpack(self, node):
        return self.finish(node, api.Starred(self.convert(node.value), self.context))

    def visit_Tuple(self, node):
        return self.finish(node, api.Tuple(self.many(node.elements), self.context))

    def visit_List(self, node):
        return self.finish(node, api.List(self.many(node.elements), self.context))

    def visit_Set(self, node):
        return self.finish(node, api.Set(self.many(node.elements)))

    def visit_Dict(self, node):
        return self.finish(
            node,
            api.Dict(
                [self.convert(item.key) for item in node.entries],
                [self.convert(item.value) for item in node.entries],
            ),
        )

    def visit_ComprehensionClause(self, node):
        return api.comprehension(
            self.target(node.target),
            self.convert(node.iterable),
            self.many(node.conditions),
            int(node.is_async),
        )

    def visit_Comprehension(self, node):
        generators = self.many(node.clauses)
        if node.kind == syntax.ComprehensionKind.DICT:
            result = api.DictComp(
                self.convert(node.key), self.convert(node.element), generators
            )
        else:
            classes = {
                syntax.ComprehensionKind.LIST: api.ListComp,
                syntax.ComprehensionKind.SET: api.SetComp,
                syntax.ComprehensionKind.GENERATOR: api.GeneratorExp,
            }
            result = classes[node.kind](self.convert(node.element), generators)
        return self.finish(node, result)

    def visit_Interpolation(self, node):
        conversion = -1 if node.conversion is None else ord(node.conversion)
        return self.finish(
            node,
            api.FormattedValue(
                self.convert(node.expression),
                conversion,
                self.convert(node.format_spec),
            ),
        )

    def visit_FormattedString(self, node):
        return self.finish(node, api.JoinedStr(self.many(node.parts)))

    def visit_TemplateString(self, node):
        parts = []
        for item in node.parts:
            parts.append(item.accept(_TemplatePart(self.locations)))
        return self.finish(node, api.TemplateStr(parts))

    def visit_ExpressionStatement(self, node):
        return self.finish(node, api.Expr(self.convert(node.expression)))

    def visit_Assignment(self, node):
        return self.finish(
            node,
            api.Assign(
                [self.target(target) for target in node.targets],
                self.convert(node.value),
                node.type_comment,
            ),
        )

    def visit_AnnotatedAssignment(self, node):
        simple = node.target.accept(_SimpleName()) and not node.parenthesized
        return self.finish(
            node,
            api.AnnAssign(
                self.target(node.target),
                self.convert(node.annotation),
                self.convert(node.value),
                int(simple),
            ),
        )

    def visit_AugmentedAssignment(self, node):
        return self.finish(
            node,
            api.AugAssign(
                self.target(node.target),
                _BINARY[node.operator](),
                self.convert(node.value),
            ),
        )

    def visit_Delete(self, node):
        return self.finish(
            node, api.Delete([self.target(target, True) for target in node.targets])
        )

    def visit_ImportItem(self, node):
        return self.finish(node, api.alias(node.name, node.alias))

    def visit_Import(self, node):
        return self.finish(node, api.Import(self.many(node.items)))

    def visit_ImportFrom(self, node):
        return self.finish(
            node, api.ImportFrom(node.module, self.many(node.items), node.level)
        )

    def visit_If(self, node):
        return self.finish(
            node,
            api.If(
                self.convert(node.condition),
                self.many(node.body),
                self.many(node.else_body),
            ),
        )

    def visit_While(self, node):
        return self.finish(
            node,
            api.While(
                self.convert(node.condition),
                self.many(node.body),
                self.many(node.else_body),
            ),
        )

    def visit_For(self, node):
        cls = api.AsyncFor if node.is_async else api.For
        return self.finish(
            node,
            cls(
                self.target(node.target),
                self.convert(node.iterable),
                self.many(node.body),
                self.many(node.else_body),
                node.type_comment,
            ),
        )

    def visit_WithItem(self, node):
        return api.withitem(self.convert(node.context), self.target(node.target))

    def visit_With(self, node):
        cls = api.AsyncWith if node.is_async else api.With
        return self.finish(
            node, cls(self.many(node.items), self.many(node.body), node.type_comment)
        )

    def visit_ExceptionHandler(self, node):
        return self.finish(
            node,
            api.ExceptHandler(
                self.convert(node.exception), node.binding, self.many(node.body)
            ),
        )

    def visit_Try(self, node):
        cls = api.TryStar if node.exception_groups else api.Try
        return self.finish(
            node,
            cls(
                self.many(node.body),
                self.many(node.handlers),
                self.many(node.else_body),
                self.many(node.finally_body),
            ),
        )

    def visit_TypeParameter(self, node):
        default = self.convert(node.default)
        if node.kind == syntax.TypeParameterKind.TYPE_VAR_TUPLE:
            result = api.TypeVarTuple(node.name, default)
        elif node.kind == syntax.TypeParameterKind.PARAM_SPEC:
            result = api.ParamSpec(node.name, default)
        else:
            bound = (
                api.Tuple(self.many(node.constraints), api.Load())
                if node.constraints
                else self.convert(node.bound)
            )
            if node.constraints:
                self.finish(node, bound)
            result = api.TypeVar(node.name, bound, default)
        return self.finish(node, result)

    def visit_FunctionDefinition(self, node):
        cls = api.AsyncFunctionDef if node.is_async else api.FunctionDef
        result = cls(
            node.name,
            self.arguments(node.parameters),
            self.many(node.body),
            self.many(node.decorators),
            self.convert(node.return_annotation),
            node.type_comment,
            self.many(node.type_parameters),
        )
        return self.locations.set(
            node, result, self.locations.definition_start(node, "def")
        )

    def visit_ClassDefinition(self, node):
        bases, keywords = self.call_arguments(node.arguments)
        result = api.ClassDef(
            node.name,
            bases,
            keywords,
            self.many(node.body),
            self.many(node.decorators),
            self.many(node.type_parameters),
        )
        return self.locations.set(
            node, result, self.locations.definition_start(node, "class")
        )

    def visit_TypeAlias(self, node):
        name = self.finish(node, api.Name(node.name, api.Store()))
        return self.finish(
            node,
            api.TypeAlias(
                name, self.many(node.type_parameters), self.convert(node.value)
            ),
        )

    def visit_Pass(self, node):
        return self.finish(node, api.Pass())

    def visit_Break(self, node):
        return self.finish(node, api.Break())

    def visit_Continue(self, node):
        return self.finish(node, api.Continue())

    def visit_Return(self, node):
        return self.finish(node, api.Return(self.convert(node.value)))

    def visit_Raise(self, node):
        return self.finish(
            node, api.Raise(self.convert(node.exception), self.convert(node.cause))
        )

    def visit_Assert(self, node):
        return self.finish(
            node, api.Assert(self.convert(node.condition), self.convert(node.message))
        )

    def visit_Global(self, node):
        return self.finish(node, api.Global(node.identifiers.copy()))

    def visit_Nonlocal(self, node):
        return self.finish(node, api.Nonlocal(node.identifiers.copy()))

    def visit_CapturePattern(self, node):
        return self.finish(node, api.MatchAs(None, node.name))

    def visit_LiteralPattern(self, node):
        return node.value.accept(_LiteralPattern(self.locations, node))

    def visit_ValuePattern(self, node):
        return self.finish(node, api.MatchValue(self.convert(node.value)))

    def visit_SequencePattern(self, node):
        return self.finish(node, api.MatchSequence(self.many(node.elements)))

    def visit_StarPattern(self, node):
        return self.finish(node, api.MatchStar(node.name))

    def visit_MappingPattern(self, node):
        return self.finish(
            node,
            api.MatchMapping(
                [self.convert(item.key) for item in node.entries],
                [self.convert(item.pattern) for item in node.entries],
                node.rest,
            ),
        )

    def visit_ClassPattern(self, node):
        return self.finish(
            node,
            api.MatchClass(
                self.convert(node.class_name),
                self.many(node.positional),
                [item.name for item in node.keywords],
                [self.convert(item.pattern) for item in node.keywords],
            ),
        )

    def visit_OrPattern(self, node):
        return self.finish(node, api.MatchOr(self.many(node.alternatives)))

    def visit_AsPattern(self, node):
        return self.finish(node, api.MatchAs(self.convert(node.pattern), node.name))

    def visit_MatchCase(self, node):
        return api.match_case(
            self.convert(node.pattern), self.convert(node.guard), self.many(node.body)
        )

    def visit_Match(self, node):
        return self.finish(
            node, api.Match(self.convert(node.subject), self.many(node.cases))
        )


class _LiteralPattern(_Convert):
    def __init__(self, locations, pattern):
        super().__init__(locations)
        self.pattern = pattern

    def visit_Node(self, node):
        return self.finish(
            self.pattern, api.MatchValue(node.accept(_Convert(self.locations)))
        )

    def visit_NoneLiteral(self, node):
        return self.finish(self.pattern, api.MatchSingleton(None))

    def visit_BooleanLiteral(self, node):
        return self.finish(self.pattern, api.MatchSingleton(node.value))

    def visit_IntegerLiteral(self, node):
        return self.visit_Node(node)

    def visit_FloatLiteral(self, node):
        return self.visit_Node(node)

    def visit_ComplexLiteral(self, node):
        return self.visit_Node(node)

    def visit_TextLiteral(self, node):
        return self.visit_Node(node)

    def visit_BytesLiteral(self, node):
        return self.visit_Node(node)

    def visit_Unary(self, node):
        return self.visit_Node(node)

    def visit_Binary(self, node):
        return self.visit_Node(node)


class _TemplatePart(_Convert):
    def visit_Interpolation(self, node):
        conversion = -1 if node.conversion is None else ord(node.conversion)
        return self.finish(
            node,
            api.Interpolation(
                self.convert(node.expression),
                node.expression_text,
                conversion,
                self.convert(node.format_spec),
            ),
        )


_EXPECTED_ROOT = {
    "exec": (api.Module, "Module"),
    "eval": (api.Expression, "Expression"),
    "single": (api.Interactive, "Interactive"),
}
_KNOWN_FLAGS = (
    0x0010
    | 0x0100
    | 0x0200
    | api.PyCF_ONLY_AST
    | 0x0800
    | api.PyCF_TYPE_COMMENTS
    | api.PyCF_ALLOW_TOP_LEVEL_AWAIT
    | 0x4000
    | api.PyCF_OPTIMIZED_AST
    | 0x1FE0000
)


def _normalize_filename(filename):
    if not isinstance(filename, (str, bytes)):
        fspath = getattr(type(filename), "__fspath__", None)
        if fspath is None:
            raise TypeError(
                f"compile() expected string, bytes or os.PathLike object, not {type(filename).__name__}"
            )
        filename = fspath(filename)
        if not isinstance(filename, (str, bytes)):
            raise TypeError(
                f"expected __fspath__() to return str or bytes, not {type(filename).__name__}"
            )
    if isinstance(filename, bytes):
        return filename.decode("utf-8", "surrogateescape")
    return filename


def _validate_ast(tree, mode):
    expected = _EXPECTED_ROOT.get(mode)
    if expected is None:
        raise ValueError("compile() mode must be 'exec', 'eval' or 'single'")
    root_type, root_name = expected
    if not isinstance(tree, root_type):
        raise TypeError(f"expected {root_name} node, got {type(tree).__name__}")
    if mode == "eval" and not isinstance(tree.body, api.expr):
        raise TypeError(f"expected expr node, got {type(tree.body).__name__}")
    return tree


def parse(
    source, filename, mode, *, type_comments=False, feature_version=None, optimize=-1
):
    filename = _normalize_filename(filename)
    if not isinstance(mode, str):
        raise TypeError(f"compile() arg 3 must be str, not {type(mode).__name__}")
    if mode not in _EXPECTED_ROOT:
        raise ValueError("compile() mode must be 'exec', 'eval' or 'single'")
    if feature_version is not None:
        version = (
            feature_version[1]
            if isinstance(feature_version, tuple)
            else feature_version
        )
        if isinstance(feature_version, tuple) and feature_version[0] != 3:
            raise ValueError(f"Unsupported major version: {feature_version[0]}")
        if version not in (-1, 14):
            raise NotImplementedError(
                "runtime AST parsing supports Python 3.14 grammar"
            )
    if optimize not in (-1, 0):
        raise NotImplementedError("optimized runtime AST parsing is not implemented")
    if isinstance(source, api.AST):
        return _validate_ast(source, mode)
    if isinstance(source, (bytes, bytearray, memoryview)):
        source = bytes(source).decode("utf-8-sig")
    if not isinstance(source, str):
        raise TypeError("compile() arg 1 must be a string, bytes or AST object")
    source = source.replace("\r\n", "\n").replace("\r", "\n")
    tree = _parse(source, filename=filename, mode=mode, type_comments=type_comments)
    return tree.accept(_Convert(_Locations(source, filename)))


class _ScopeState:
    def __init__(self, kind, params=()):
        self.kind = kind
        self.params = set(params)
        for p in self.params:
            if p == "__debug__":
                raise SyntaxError("cannot assign to __debug__")
        self.seen = set()
        self.bound = set()
        self.globals = set()
        self.nonlocals = set()


def _param_names(args):
    names = []
    for a in args.posonlyargs:
        names.append(a.arg)
    for a in args.args:
        names.append(a.arg)
    if args.vararg is not None:
        names.append(args.vararg.arg)
    for a in args.kwonlyargs:
        names.append(a.arg)
    if args.kwarg is not None:
        names.append(args.kwarg.arg)
    return names


def _collect_target_names(node, out):
    if isinstance(node, api.Name):
        if node.id == "__debug__":
            raise SyntaxError("cannot assign to __debug__")
        out.add(node.id)
        return
    for field in getattr(node, "_fields", ()):
        value = getattr(node, field, None)
        if isinstance(value, list):
            for item in value:
                if isinstance(item, api.AST):
                    _collect_target_names(item, out)
        elif isinstance(value, api.AST):
            _collect_target_names(value, out)


def _contains_named_expr(node):
    if isinstance(node, api.NamedExpr):
        return True
    if isinstance(
        node, (api.FunctionDef, api.AsyncFunctionDef, api.Lambda, api.ClassDef)
    ):
        return False
    for field in getattr(node, "_fields", ()):
        value = getattr(node, field, None)
        if isinstance(value, list):
            for item in value:
                if isinstance(item, api.AST) and _contains_named_expr(item):
                    return True
        elif isinstance(value, api.AST) and _contains_named_expr(value):
            return True
    return False


def _validate_scope_body(
    nodes, state, ancestors, deferred, comp_targets=frozenset(), in_comp=False
):
    for node in nodes:
        _validate_scope_item(
            node, state, ancestors, deferred, comp_targets, in_comp
        )


def _validate_scope_item(
    node, state, ancestors, deferred, comp_targets=frozenset(), in_comp=False
):
    if node is None or not isinstance(node, api.AST):
        return
    if isinstance(node, api.MatchMapping):
        seen = []
        for key in node.keys:
            if isinstance(key, api.Constant):
                if key.value in seen:
                    raise SyntaxError(
                        f"mapping pattern checks duplicate key ({key.value!r})"
                    )
                seen.append(key.value)
        if node.rest is not None:
            if node.rest == "__debug__":
                raise SyntaxError("cannot assign to __debug__")
            state.seen.add(node.rest)
            state.bound.add(node.rest)
    if isinstance(node, (api.MatchAs, api.MatchStar)):
        if node.name is not None:
            if node.name == "__debug__":
                raise SyntaxError("cannot assign to __debug__")
            state.seen.add(node.name)
            state.bound.add(node.name)
    if isinstance(node, api.Name):
        if node.id == "__debug__" and isinstance(node.ctx, (api.Store, api.Del)):
            raise SyntaxError("cannot assign to __debug__")
        if not in_comp:
            state.seen.add(node.id)
            if isinstance(node.ctx, (api.Store, api.Del)):
                state.bound.add(node.id)
        return
    if isinstance(node, api.NamedExpr):
        _validate_scope_item(
            node.value, state, ancestors, deferred, comp_targets, in_comp
        )
        target_name = node.target.id
        if target_name == "__debug__":
            raise SyntaxError("cannot assign to __debug__")
        if in_comp:
            if target_name in comp_targets:
                raise SyntaxError(
                    f"assignment expression cannot rebind comprehension iteration variable {target_name!r}"
                )
            if state.kind == "class":
                raise SyntaxError(
                    "assignment expression within a comprehension cannot be used in a class body"
                )
        state.seen.add(target_name)
        state.bound.add(target_name)
        return
    if isinstance(
        node, (api.ListComp, api.SetComp, api.DictComp, api.GeneratorExp)
    ):
        local_targets = set()
        for gen in node.generators:
            if _contains_named_expr(gen.iter):
                raise SyntaxError(
                    "assignment expression cannot be used in a comprehension iterable expression"
                )
            _collect_target_names(gen.target, local_targets)
        if local_targets & comp_targets:
            pass
        all_targets = comp_targets | frozenset(local_targets)
        _validate_scope_item(
            node.generators[0].iter,
            state,
            ancestors,
            deferred,
            comp_targets,
            in_comp,
        )
        for idx, gen in enumerate(node.generators):
            if idx > 0:
                _validate_scope_item(
                    gen.iter, state, ancestors, deferred, all_targets, True
                )
            for cond in gen.ifs:
                _validate_scope_item(
                    cond, state, ancestors, deferred, all_targets, True
                )
        if isinstance(node, api.DictComp):
            _validate_scope_item(
                node.key, state, ancestors, deferred, all_targets, True
            )
            _validate_scope_item(
                node.value, state, ancestors, deferred, all_targets, True
            )
        else:
            _validate_scope_item(
                node.elt, state, ancestors, deferred, all_targets, True
            )
        return
    if isinstance(node, api.Global):
        for name in node.names:
            if name == "__debug__":
                raise SyntaxError("cannot assign to __debug__")
            if name in state.params:
                raise SyntaxError(f"name {name!r} is parameter and global")
            if name in state.nonlocals:
                raise SyntaxError(f"name {name!r} is nonlocal and global")
            if name in state.seen:
                raise SyntaxError(
                    f"name {name!r} is used prior to global declaration"
                )
            state.globals.add(name)
        return
    if isinstance(node, api.Nonlocal):
        if state.kind == "module":
            raise SyntaxError("nonlocal declaration not allowed at module level")
        for name in node.names:
            if name == "__debug__":
                raise SyntaxError("cannot assign to __debug__")
            if name in state.params:
                raise SyntaxError(f"name {name!r} is parameter and nonlocal")
            if name in state.globals:
                raise SyntaxError(f"name {name!r} is nonlocal and global")
            if name in state.seen:
                raise SyntaxError(
                    f"name {name!r} is used prior to nonlocal declaration"
                )
            state.nonlocals.add(name)
        return
    if isinstance(node, (api.FunctionDef, api.AsyncFunctionDef)):
        for dec in node.decorator_list:
            _validate_scope_item(
                dec, state, ancestors, deferred, comp_targets, in_comp
            )
        for default in node.args.defaults:
            _validate_scope_item(
                default, state, ancestors, deferred, comp_targets, in_comp
            )
        for default in node.args.kw_defaults:
            if default is not None:
                _validate_scope_item(
                    default, state, ancestors, deferred, comp_targets, in_comp
                )
        if node.name == "__debug__":
            raise SyntaxError("cannot assign to __debug__")
        if not in_comp:
            state.seen.add(node.name)
            state.bound.add(node.name)
        deferred.append((node, (*ancestors, state)))
        return
    if isinstance(node, api.Lambda):
        for default in node.args.defaults:
            _validate_scope_item(
                default, state, ancestors, deferred, comp_targets, in_comp
            )
        for default in node.args.kw_defaults:
            if default is not None:
                _validate_scope_item(
                    default, state, ancestors, deferred, comp_targets, in_comp
                )
        deferred.append((node, (*ancestors, state)))
        return
    if isinstance(node, api.ClassDef):
        for dec in node.decorator_list:
            _validate_scope_item(
                dec, state, ancestors, deferred, comp_targets, in_comp
            )
        for base in node.bases:
            _validate_scope_item(
                base, state, ancestors, deferred, comp_targets, in_comp
            )
        for kw in node.keywords:
            _validate_scope_item(
                kw.value, state, ancestors, deferred, comp_targets, in_comp
            )
        if node.name == "__debug__":
            raise SyntaxError("cannot assign to __debug__")
        if not in_comp:
            state.seen.add(node.name)
            state.bound.add(node.name)
        deferred.append((node, (*ancestors, state)))
        return
    if isinstance(node, (api.Import, api.ImportFrom)):
        for alias in node.names:
            bound = alias.asname or alias.name.split(".")[0]
            if bound == "__debug__":
                raise SyntaxError("cannot assign to __debug__")
            if not in_comp:
                state.seen.add(bound)
                state.bound.add(bound)
        return
    if isinstance(node, api.ExceptHandler):
        if node.type is not None:
            _validate_scope_item(
                node.type, state, ancestors, deferred, comp_targets, in_comp
            )
        if node.name is not None:
            if node.name == "__debug__":
                raise SyntaxError("cannot assign to __debug__")
            state.seen.add(node.name)
            state.bound.add(node.name)
        _validate_scope_body(
            node.body, state, ancestors, deferred, comp_targets, in_comp
        )
        return
    for field in getattr(node, "_fields", ()):
        value = getattr(node, field, None)
        if isinstance(value, list):
            for item in value:
                if isinstance(item, api.AST):
                    _validate_scope_item(
                        item, state, ancestors, deferred, comp_targets, in_comp
                    )
        elif isinstance(value, api.AST):
            _validate_scope_item(
                value, state, ancestors, deferred, comp_targets, in_comp
            )


def _validate_scope_root(node, ancestors=()):
    if isinstance(node, (api.Module, api.Interactive)):
        state = _ScopeState("module")
        body = node.body
    elif isinstance(node, api.Expression):
        state = _ScopeState("module")
        body = [node.body]
    elif isinstance(node, (api.FunctionDef, api.AsyncFunctionDef)):
        state = _ScopeState("function", _param_names(node.args))
        body = node.body
    elif isinstance(node, api.Lambda):
        state = _ScopeState("function", _param_names(node.args))
        body = [node.body]
    elif isinstance(node, api.ClassDef):
        state = _ScopeState("class")
        body = node.body
    else:
        state = _ScopeState("module")
        body = [node]
    deferred = []
    _validate_scope_body(body, state, ancestors, deferred)
    for name in state.nonlocals:
        found = False
        for anc in reversed(ancestors):
            if anc.kind == "function":
                if (
                    name in anc.params
                    or name in anc.bound
                    or name in anc.nonlocals
                ) and name not in anc.globals:
                    found = True
                    break
        if not found:
            raise SyntaxError(f"no binding for nonlocal {name!r} found")
    for child, child_ancestors in deferred:
        _validate_scope_root(child, child_ancestors)


def _validate_compile_node(node):
    _validate_scope_root(node)


def compile(
    source,
    filename,
    mode,
    flags=0,
    dont_inherit=False,
    optimize=-1,
    *,
    _feature_version=-1,
):
    from _object import code

    filename = _normalize_filename(filename)
    if not isinstance(mode, str):
        raise TypeError(f"compile() arg 3 must be str, not {type(mode).__name__}")
    if mode not in _EXPECTED_ROOT:
        raise ValueError("compile() mode must be 'exec', 'eval' or 'single'")
    if not isinstance(flags, int):
        raise TypeError(
            f"'{type(flags).__name__}' object cannot be interpreted as an integer"
        )
    if flags & ~_KNOWN_FLAGS:
        raise ValueError("compile(): unrecognised flags")
    if not isinstance(optimize, int):
        raise TypeError(
            f"'{type(optimize).__name__}' object cannot be interpreted as an integer"
        )
    if optimize not in (-1, 0, 1, 2):
        raise ValueError("compile(): invalid optimize value")
    if flags & (api.PyCF_OPTIMIZED_AST ^ api.PyCF_ONLY_AST):
        ast_optimize = 1 if optimize <= 0 else optimize
    elif flags & api.PyCF_ONLY_AST:
        ast_optimize = optimize
    else:
        ast_optimize = -1
    tree = parse(
        source,
        filename,
        mode,
        type_comments=bool(flags & api.PyCF_TYPE_COMMENTS),
        feature_version=_feature_version,
        optimize=ast_optimize,
    )
    if flags & api.PyCF_ONLY_AST:
        return tree
    _validate_compile_node(tree)
    firstlineno = 0
    if mode == "eval":
        firstlineno = getattr(tree.body, "lineno", 1)
    elif getattr(tree, "body", None):
        firstlineno = getattr(tree.body[0], "lineno", 1)
    return code(
        "<module>",
        filename,
        "<module>",
        firstlineno,
        flags=flags & 0x1FE0010,
        ast=tree,
        mode=mode,
    )


class _ReturnSignal( BaseException):
    def __init__(self, value):
        self.value = value


class _BreakSignal(BaseException):
    pass


class _ContinueSignal(BaseException):
    pass


def _scan_function_scope(nodes, bound, globals_set, nonlocals_set):
    for node in nodes:
        if node is None or not isinstance(node, api.AST):
            continue
        if isinstance(
            node,
            (
                api.FunctionDef,
                api.AsyncFunctionDef,
                api.Lambda,
                api.ClassDef,
                api.ListComp,
                api.SetComp,
                api.DictComp,
                api.GeneratorExp,
            ),
        ):
            if isinstance(
                node, (api.FunctionDef, api.AsyncFunctionDef, api.ClassDef)
            ):
                bound.add(node.name)
            continue
        if isinstance(node, api.Global):
            globals_set.update(node.names)
            continue
        if isinstance(node, api.Nonlocal):
            nonlocals_set.update(node.names)
            continue
        if isinstance(node, api.Name) and isinstance(
            node.ctx, (api.Store, api.Del)
        ):
            bound.add(node.id)
            continue
        if isinstance(node, (api.Import, api.ImportFrom)):
            for alias in node.names:
                bound.add(alias.asname or alias.name.split(".")[0])
            continue
        if isinstance(node, api.ExceptHandler) and node.name is not None:
            bound.add(node.name)
        for field in getattr(node, "_fields", ()):
            value = getattr(node, field, None)
            if isinstance(value, list):
                _scan_function_scope(
                    [x for x in value if isinstance(x, api.AST)],
                    bound,
                    globals_set,
                    nonlocals_set,
                )
            elif isinstance(value, api.AST):
                _scan_function_scope(
                    [value], bound, globals_set, nonlocals_set
                )


class _ExpressionEvaluator:
    """Evaluate compiled expression and statement forms in explicit namespaces,
    following SBCL ``src/code/eval.lisp``'s ``simple-eval-in-lexenv``."""

    def __init__(
        self,
        globals_ns,
        locals_ns,
        *,
        enclosing_frames=(),
        is_function=False,
        walrus_frame=None,
    ):
        self.globals = globals_ns
        self.locals = locals_ns
        self.enclosing_frames = enclosing_frames
        self.is_function = is_function
        self.walrus_frame = walrus_frame
        self.global_names = set()
        self.nonlocal_names = set()
        self.bound_names = set()

    def eval(self, node):
        method = getattr(self, "visit_" + type(node).__name__, None)
        if method is None:
            raise NotImplementedError(
                f"runtime evaluation of {type(node).__name__} is not implemented"
            )
        return method(node)

    def exec_module(self, node):
        if isinstance(node, (api.Module, api.Interactive)):
            self.exec_suite(node.body)
        elif isinstance(node, api.Expression):
            self.eval(node.body)
        else:
            self.eval(node)

    def exec_suite(self, statements):
        for stmt in statements:
            self.eval(stmt)

    def _lookup_builtin(self, name):
        builtins_ns = self.globals.get("__builtins__")
        if builtins_ns is not None:
            if hasattr(type(builtins_ns), "__getitem__"):
                try:
                    return True, builtins_ns[name]
                except KeyError:
                    pass
            else:
                try:
                    return True, getattr(builtins_ns, name)
                except AttributeError:
                    pass
        return False, None

    def load_name(self, name):
        if name in self.global_names:
            try:
                return self.globals[name]
            except KeyError:
                pass
            found, val = self._lookup_builtin(name)
            if found:
                return val
            raise NameError(f"name {name!r} is not defined")
        if name in self.nonlocal_names:
            for frame in reversed(self.enclosing_frames):
                if name not in frame.global_names and frame.locals is not None:
                    try:
                        return frame.locals[name]
                    except KeyError:
                        pass
            raise NameError(
                f"free variable {name!r} referenced before assignment in enclosing scope"
            )
        if self.locals is not None:
            try:
                return self.locals[name]
            except KeyError:
                pass
        if self.is_function and name in self.bound_names:
            raise UnboundLocalError(
                f"cannot access local variable {name!r} where it is not associated with a value"
            )
        for frame in reversed(self.enclosing_frames):
            if name not in frame.global_names and frame.locals is not None:
                try:
                    return frame.locals[name]
                except KeyError:
                    pass
        if self.locals is None or self.locals is not self.globals:
            try:
                return self.globals[name]
            except KeyError:
                pass
        found, val = self._lookup_builtin(name)
        if found:
            return val
        raise NameError(f"name {name!r} is not defined")

    def store_name(self, name, value):
        if name in self.global_names:
            self.globals[name] = value
            return
        if name in self.nonlocal_names:
            for frame in reversed(self.enclosing_frames):
                if (
                    name in frame.bound_names
                    or (frame.locals is not None and name in frame.locals)
                ) and name not in frame.global_names:
                    frame.locals[name] = value
                    return
            if self.enclosing_frames:
                self.enclosing_frames[-1].locals[name] = value
                return
        self.locals[name] = value

    def delete_name(self, name):
        if name in self.global_names:
            if name in self.globals:
                del self.globals[name]
                return
            raise NameError(f"name {name!r} is not defined")
        if name in self.nonlocal_names:
            for frame in reversed(self.enclosing_frames):
                if frame.locals is not None and name in frame.locals:
                    del frame.locals[name]
                    return
            raise NameError(
                f"free variable {name!r} referenced before assignment in enclosing scope"
            )
        if self.locals is not None and name in self.locals:
            del self.locals[name]
            return
        if self.is_function and name in self.bound_names:
            raise UnboundLocalError(
                f"cannot access local variable {name!r} where it is not associated with a value"
            )
        raise NameError(f"name {name!r} is not defined")

    def assign_target(self, target, value):
        if isinstance(target, api.Name):
            self.store_name(target.id, value)
            return
        if isinstance(target, (api.Tuple, api.List)):
            items = list(value)
            star_idx = None
            for idx, elt in enumerate(target.elts):
                if isinstance(elt, api.Starred):
                    star_idx = idx
                    break
            if star_idx is None:
                if len(items) != len(target.elts):
                    raise ValueError(
                        f"not enough values to unpack (expected {len(target.elts)}, got {len(items)})"
                        if len(items) < len(target.elts)
                        else f"too many values to unpack (expected {len(target.elts)})"
                    )
                for elt, item in zip(target.elts, items):
                    self.assign_target(elt, item)
            else:
                min_len = len(target.elts) - 1
                if len(items) < min_len:
                    raise ValueError(
                        f"not enough values to unpack (expected at least {min_len}, got {len(items)})"
                    )
                after_count = len(target.elts) - star_idx - 1
                split_end = (
                    len(items) - after_count if after_count else len(items)
                )
                for idx in range(star_idx):
                    self.assign_target(target.elts[idx], items[idx])
                self.assign_target(
                    target.elts[star_idx].value, items[star_idx:split_end]
                )
                for offset in range(after_count):
                    self.assign_target(
                        target.elts[star_idx + 1 + offset],
                        items[split_end + offset],
                    )
            return
        if isinstance(target, api.Attribute):
            setattr(self.eval(target.value), target.attr, value)
            return
        if isinstance(target, api.Subscript):
            self.eval(target.value)[self.eval(target.slice)] = value
            return
        raise NotImplementedError(
            f"unsupported assignment target {type(target).__name__}"
        )

    def _elements(self, elts):
        items = []
        for elt in elts:
            if type(elt) is api.Starred:
                items.extend(self.eval(elt.value))
            else:
                items.append(self.eval(elt))
        return items

    def visit_Expression(self, node):
        return self.eval(node.body)

    def visit_Constant(self, node):
        return node.value

    def visit_Name(self, node):
        if not isinstance(node.ctx, api.Load):
            raise ValueError("expression must have Load context")
        return self.load_name(node.id)

    def visit_NamedExpr(self, node):
        value = self.eval(node.value)
        target_frame = self.walrus_frame if self.walrus_frame is not None else self
        target_frame.store_name(node.target.id, value)
        return value

    def visit_UnaryOp(self, node):
        operand = self.eval(node.operand)
        op = type(node.op)
        if op is api.Not:
            return not operand
        if op is api.UAdd:
            return +operand
        if op is api.USub:
            return -operand
        if op is api.Invert:
            return ~operand
        raise NotImplementedError(f"unsupported unary operator {op.__name__}")

    def visit_BoolOp(self, node):
        op = type(node.op)
        if op is api.And:
            result = True
            for value in node.values:
                result = self.eval(value)
                if not result:
                    return result
            return result
        if op is api.Or:
            result = False
            for value in node.values:
                result = self.eval(value)
                if result:
                    return result
            return result
        raise NotImplementedError(f"unsupported boolean operator {op.__name__}")

    def _apply_binop(self, op_node, left, right):
        op = type(op_node)
        if op is api.Add:
            return left + right
        if op is api.Sub:
            return left - right
        if op is api.Mult:
            return left * right
        if op is api.Div:
            return left / right
        if op is api.FloorDiv:
            return left // right
        if op is api.Mod:
            return left % right
        if op is api.Pow:
            return left**right
        if op is api.LShift:
            return left << right
        if op is api.RShift:
            return left >> right
        if op is api.BitOr:
            return left | right
        if op is api.BitXor:
            return left ^ right
        if op is api.BitAnd:
            return left & right
        if op is api.MatMult:
            return left @ right
        raise NotImplementedError(f"unsupported binary operator {op.__name__}")

    def visit_BinOp(self, node):
        left = self.eval(node.left)
        right = self.eval(node.right)
        return self._apply_binop(node.op, left, right)

    def visit_Compare(self, node):
        left = self.eval(node.left)
        for op_node, comparator in zip(node.ops, node.comparators):
            right = self.eval(comparator)
            op = type(op_node)
            if op is api.Eq:
                matched = left == right
            elif op is api.NotEq:
                matched = left != right
            elif op is api.Lt:
                matched = left < right
            elif op is api.LtE:
                matched = left <= right
            elif op is api.Gt:
                matched = left > right
            elif op is api.GtE:
                matched = left >= right
            elif op is api.Is:
                matched = left is right
            elif op is api.IsNot:
                matched = left is not right
            elif op is api.In:
                matched = left in right
            elif op is api.NotIn:
                matched = left not in right
            else:
                raise NotImplementedError(
                    f"unsupported comparison operator {op.__name__}"
                )
            if not matched:
                return False
            left = right
        return True

    def visit_IfExp(self, node):
        return self.eval(node.body) if self.eval(node.test) else self.eval(node.orelse)

    def visit_Attribute(self, node):
        if not isinstance(node.ctx, api.Load):
            raise ValueError("expression must have Load context")
        return getattr(self.eval(node.value), node.attr)

    def visit_Subscript(self, node):
        if not isinstance(node.ctx, api.Load):
            raise ValueError("expression must have Load context")
        return self.eval(node.value)[self.eval(node.slice)]

    def visit_Slice(self, node):
        lower = None if node.lower is None else self.eval(node.lower)
        upper = None if node.upper is None else self.eval(node.upper)
        step = None if node.step is None else self.eval(node.step)
        return slice(lower, upper, step)

    def visit_Tuple(self, node):
        if not isinstance(node.ctx, api.Load):
            raise ValueError("expression must have Load context")
        return tuple(self._elements(node.elts))

    def visit_List(self, node):
        if not isinstance(node.ctx, api.Load):
            raise ValueError("expression must have Load context")
        return self._elements(node.elts)

    def visit_Set(self, node):
        return set(self._elements(node.elts))

    def visit_Dict(self, node):
        result = {}
        for key_node, value_node in zip(node.keys, node.values):
            if key_node is None:
                result.update(self.eval(value_node))
            else:
                result[self.eval(key_node)] = self.eval(value_node)
        return result

    def visit_FormattedValue(self, node):
        value = self.eval(node.value)
        if node.conversion == 115:
            value = str(value)
        elif node.conversion == 114:
            value = repr(value)
        elif node.conversion == 97:
            value = ascii(value)
        spec = "" if node.format_spec is None else self.eval(node.format_spec)
        return format(value, spec)

    def visit_JoinedStr(self, node):
        return "".join(str(self.eval(part)) for part in node.values)

    def visit_Call(self, node):
        func = self.eval(node.func)
        args = self._elements(node.args)
        kwargs = {}
        for keyword in node.keywords:
            if keyword.arg is None:
                mapping = self.eval(keyword.value)
                for key in mapping:
                    if key in kwargs:
                        raise TypeError(
                            f"{func}() got multiple values for keyword argument {key!r}"
                        )
                    kwargs[key] = mapping[key]
            else:
                if keyword.arg in kwargs:
                    raise TypeError(
                        f"{func}() got multiple values for keyword argument {keyword.arg!r}"
                    )
                kwargs[keyword.arg] = self.eval(keyword.value)
        return func(*args, **kwargs)

    def _comp_frame(self):
        enclosing = (
            (*self.enclosing_frames, self)
            if self.is_function
            else self.enclosing_frames
        )
        return _ExpressionEvaluator(
            self.globals,
            {},
            enclosing_frames=enclosing,
            is_function=True,
            walrus_frame=self.walrus_frame or self,
        )

    def _run_generators(self, frame, generators, index, first_iter, emit):
        gen = generators[index]
        iterator = first_iter if index == 0 else iter(frame.eval(gen.iter))
        for item in iterator:
            frame.assign_target(gen.target, item)
            matched = True
            for cond in gen.ifs:
                if not frame.eval(cond):
                    matched = False
                    break
            if not matched:
                continue
            if index + 1 < len(generators):
                self._run_generators(
                    frame, generators, index + 1, None, emit
                )
            else:
                emit(frame)

    def visit_ListComp(self, node):
        first_iter = iter(self.eval(node.generators[0].iter))
        frame = self._comp_frame()
        out = []
        self._run_generators(
            frame,
            node.generators,
            0,
            first_iter,
            lambda f: out.append(f.eval(node.elt)),
        )
        return out

    def visit_SetComp(self, node):
        first_iter = iter(self.eval(node.generators[0].iter))
        frame = self._comp_frame()
        out = set()
        self._run_generators(
            frame,
            node.generators,
            0,
            first_iter,
            lambda f: out.add(f.eval(node.elt)),
        )
        return out

    def visit_DictComp(self, node):
        first_iter = iter(self.eval(node.generators[0].iter))
        frame = self._comp_frame()
        out = {}

        def _add(f):
            k = f.eval(node.key)
            v = f.eval(node.value)
            out[k] = v

        self._run_generators(frame, node.generators, 0, first_iter, _add)
        return out

    def visit_GeneratorExp(self, node):
        first_iter = iter(self.eval(node.generators[0].iter))
        frame = self._comp_frame()
        out = []
        self._run_generators(
            frame,
            node.generators,
            0,
            first_iter,
            lambda f: out.append(f.eval(node.elt)),
        )
        return (x for x in out)

    def _make_callable(self, name, args_spec, body_node, is_lambda=False):
        from _mule_native import function_attributes, namespace_bind

        posonly = [a.arg for a in args_spec.posonlyargs]
        pos_or_kw = [a.arg for a in args_spec.args]
        all_pos = posonly + pos_or_kw
        vararg_name = args_spec.vararg.arg if args_spec.vararg else None
        kwonly = [a.arg for a in args_spec.kwonlyargs]
        kwarg_name = args_spec.kwarg.arg if args_spec.kwarg else None
        pos_defaults = [self.eval(d) for d in args_spec.defaults]
        kw_defaults = {
            k.arg: self.eval(d)
            for k, d in zip(args_spec.kwonlyargs, args_spec.kw_defaults)
            if d is not None
        }
        enclosing = (
            (*self.enclosing_frames, self)
            if self.is_function
            else self.enclosing_frames
        )
        bound = set(_param_names(args_spec))
        globals_set = set()
        nonlocals_set = set()
        if not is_lambda:
            _scan_function_scope(body_node, bound, globals_set, nonlocals_set)
            bound -= globals_set | nonlocals_set

        def _dynamic_function(*call_args, **call_kwargs):
            local_ns = {}
            kw_remaining = dict(call_kwargs)
            num_pos = len(all_pos)
            default_start = num_pos - len(pos_defaults)
            for idx, param in enumerate(all_pos):
                if idx < len(call_args):
                    local_ns[param] = call_args[idx]
                    if idx >= len(posonly) and param in kw_remaining:
                        raise TypeError(
                            f"{name}() got multiple values for argument {param!r}"
                        )
                elif idx >= len(posonly) and param in kw_remaining:
                    local_ns[param] = kw_remaining.pop(param)
                elif idx >= default_start:
                    local_ns[param] = pos_defaults[idx - default_start]
                else:
                    raise TypeError(
                        f"{name}() missing required positional argument: {param!r}"
                    )
            if len(call_args) > num_pos:
                if vararg_name is not None:
                    local_ns[vararg_name] = call_args[num_pos:]
                else:
                    raise TypeError(
                        f"{name}() takes {num_pos} positional arguments but {len(call_args)} were given"
                    )
            elif vararg_name is not None:
                local_ns[vararg_name] = ()
            for param in kwonly:
                if param in kw_remaining:
                    local_ns[param] = kw_remaining.pop(param)
                elif param in kw_defaults:
                    local_ns[param] = kw_defaults[param]
                else:
                    raise TypeError(
                        f"{name}() missing required keyword-only argument: {param!r}"
                    )
            if kwarg_name is not None:
                local_ns[kwarg_name] = kw_remaining
            elif kw_remaining:
                unexpected = next(iter(kw_remaining))
                raise TypeError(
                    f"{name}() got an unexpected keyword argument {unexpected!r}"
                )
            fn_frame = _ExpressionEvaluator(
                self.globals,
                local_ns,
                enclosing_frames=enclosing,
                is_function=True,
            )
            fn_frame.bound_names = bound
            fn_frame.global_names = globals_set
            fn_frame.nonlocal_names = nonlocals_set
            if is_lambda:
                return fn_frame.eval(body_node)
            try:
                fn_frame.exec_suite(body_node)
            except _ReturnSignal as sig:
                return sig.value
            return None

        _dynamic_function.__name__ = name
        _dynamic_function.__qualname__ = name
        namespace_bind(
            function_attributes(_dynamic_function), "__globals__", self.globals
        )
        return _dynamic_function

    def visit_Lambda(self, node):
        return self._make_callable("<lambda>", node.args, node.body, True)

    def visit_FunctionDef(self, node):
        decorators = [self.eval(d) for d in node.decorator_list]
        fn = self._make_callable(node.name, node.args, node.body, False)
        for dec in reversed(decorators):
            fn = dec(fn)
        self.store_name(node.name, fn)

    def visit_ClassDef(self, node):
        decorators = [self.eval(d) for d in node.decorator_list]
        bases = tuple(self._elements(node.bases))
        keywords = {}
        for kw in node.keywords:
            if kw.arg is None:
                keywords.update(self.eval(kw.value))
            else:
                keywords[kw.arg] = self.eval(kw.value)
        metaclass = keywords.pop("metaclass", None)
        if metaclass is None:
            metaclass = type(bases[0]) if bases else type
        prep = getattr(metaclass, "__prepare__", None)
        class_ns = prep(node.name, bases, **keywords) if prep is not None else {}
        if "__name__" in self.globals:
            class_ns["__module__"] = self.globals["__name__"]
        else:
            class_ns["__module__"] = "builtins"
        class_ns["__qualname__"] = node.name
        enclosing = (
            (*self.enclosing_frames, self)
            if self.is_function
            else self.enclosing_frames
        )
        cls_frame = _ExpressionEvaluator(
            self.globals,
            class_ns,
            enclosing_frames=enclosing,
            is_function=False,
        )
        cls_frame.exec_suite(node.body)
        cls = metaclass(node.name, bases, class_ns, **keywords)
        for dec in reversed(decorators):
            cls = dec(cls)
        self.store_name(node.name, cls)

    def visit_Expr(self, node):
        self.eval(node.value)

    def visit_Pass(self, node):
        pass

    def visit_Assign(self, node):
        value = self.eval(node.value)
        for target in node.targets:
            self.assign_target(target, value)

    def visit_AnnAssign(self, node):
        if node.value is not None:
            self.assign_target(node.target, self.eval(node.value))

    def visit_AugAssign(self, node):
        rhs = self.eval(node.value)
        target = node.target
        if isinstance(target, api.Name):
            lhs = self.load_name(target.id)
            self.store_name(target.id, self._apply_binop(node.op, lhs, rhs))
            return
        if isinstance(target, api.Attribute):
            owner = self.eval(target.value)
            lhs = getattr(owner, target.attr)
            setattr(owner, target.attr, self._apply_binop(node.op, lhs, rhs))
            return
        if isinstance(target, api.Subscript):
            owner = self.eval(target.value)
            key = self.eval(target.slice)
            lhs = owner[key]
            owner[key] = self._apply_binop(node.op, lhs, rhs)
            return
        raise NotImplementedError(
            f"unsupported augmented assignment target {type(target).__name__}"
        )

    def visit_Delete(self, node):
        for target in node.targets:
            if isinstance(target, api.Name):
                self.delete_name(target.id)
            elif isinstance(target, api.Attribute):
                delattr(self.eval(target.value), target.attr)
            elif isinstance(target, api.Subscript):
                del self.eval(target.value)[self.eval(target.slice)]
            else:
                raise NotImplementedError(
                    f"unsupported delete target {type(target).__name__}"
                )

    def visit_Return(self, node):
        val = None if node.value is None else self.eval(node.value)
        raise _ReturnSignal(val)

    def visit_Break(self, node):
        raise _BreakSignal()

    def visit_Continue(self, node):
        raise _ContinueSignal()

    def visit_Global(self, node):
        self.global_names.update(node.names)

    def visit_Nonlocal(self, node):
        self.nonlocal_names.update(node.names)

    def visit_If(self, node):
        if self.eval(node.test):
            self.exec_suite(node.body)
        else:
            self.exec_suite(node.orelse)

    def visit_While(self, node):
        while self.eval(node.test):
            try:
                self.exec_suite(node.body)
            except _BreakSignal:
                return
            except _ContinueSignal:
                continue
        self.exec_suite(node.orelse)

    def visit_For(self, node):
        for item in self.eval(node.iter):
            self.assign_target(node.target, item)
            try:
                self.exec_suite(node.body)
            except _BreakSignal:
                return
            except _ContinueSignal:
                continue
        self.exec_suite(node.orelse)

    def visit_Raise(self, node):
        if node.exc is None:
            raise
        exc = self.eval(node.exc)
        if node.cause is not None:
            raise exc from self.eval(node.cause)
        raise exc

    def visit_Assert(self, node):
        if not self.eval(node.test):
            if node.msg is not None:
                raise AssertionError(self.eval(node.msg))
            raise AssertionError()

    def visit_Try(self, node):
        try:
            try:
                self.exec_suite(node.body)
            except BaseException as exc:
                if isinstance(
                    exc, (_ReturnSignal, _BreakSignal, _ContinueSignal)
                ):
                    raise
                handled = False
                for handler in node.handlers:
                    if handler.type is None or isinstance(
                        exc, self.eval(handler.type)
                    ):
                        handled = True
                        if handler.name is not None:
                            self.store_name(handler.name, exc)
                        try:
                            self.exec_suite(handler.body)
                        finally:
                            if handler.name is not None:
                                try:
                                    self.delete_name(handler.name)
                                except NameError:
                                    pass
                        break
                if not handled:
                    raise
            else:
                self.exec_suite(node.orelse)
        finally:
            self.exec_suite(node.finalbody)

    def visit_Import(self, node):
        import builtins

        imp = builtins.__import__
        for alias in node.names:
            mod = imp(alias.name, self.globals, self.locals, (), 0)
            if alias.asname is not None:
                for part in alias.name.split(".")[1:]:
                    mod = getattr(mod, part)
                self.store_name(alias.asname, mod)
            else:
                self.store_name(alias.name.split(".")[0], mod)

    def visit_ImportFrom(self, node):
        import builtins

        imp = builtins.__import__
        names = tuple(alias.name for alias in node.names)
        mod = imp(
            node.module or "", self.globals, self.locals, names, node.level
        )
        for alias in node.names:
            if alias.name == "*":
                exported = getattr(
                    mod,
                    "__all__",
                    [k for k in vars(mod) if not k.startswith("_")],
                )
                for k in exported:
                    self.store_name(k, getattr(mod, k))
            else:
                self.store_name(
                    alias.asname or alias.name, getattr(mod, alias.name)
                )


def _prepare_eval_namespaces(globals_ns, locals_ns, caller_name):
    from _object import _GlobalsDict, _InstanceDict

    if globals_ns is None:
        import sys

        globals_ns = sys._getframe(3).f_globals
    elif not isinstance(globals_ns, (dict, _GlobalsDict, _InstanceDict)):
        raise TypeError(
            f"{caller_name}() globals must be a dict, not {type(globals_ns).__name__}"
        )
    if "__builtins__" not in globals_ns:
        import builtins

        globals_ns["__builtins__"] = builtins.__dict__
    if locals_ns is None:
        locals_ns = globals_ns
    elif not hasattr(type(locals_ns), "__getitem__"):
        raise TypeError(
            f"locals must be a mapping or None, not {type(locals_ns).__name__}"
        )
    return globals_ns, locals_ns


def evaluate(expression, globals_ns=None, locals_ns=None):
    from _object import code

    globals_ns, locals_ns = _prepare_eval_namespaces(
        globals_ns, locals_ns, "eval"
    )
    if isinstance(expression, (bytes, bytearray, memoryview)):
        expression = bytes(expression).decode("utf-8-sig")
    if isinstance(expression, str):
        expression = compile(expression.lstrip(" \t"), "<string>", "eval")
    elif not isinstance(expression, code):
        raise TypeError("eval() arg 1 must be a string, bytes or code object")
    if expression._ast is None:
        raise NotImplementedError(
            "runtime evaluation of non-AST code objects is not implemented"
        )
    evaluator = _ExpressionEvaluator(globals_ns, locals_ns)
    if expression._mode == "eval":
        return evaluator.eval(expression._ast)
    evaluator.exec_module(expression._ast)
    return None


def execute(source, globals_ns=None, locals_ns=None, *, closure=None):
    from _object import code

    if closure is not None:
        raise NotImplementedError("exec() with closure is not implemented")
    globals_ns, locals_ns = _prepare_eval_namespaces(
        globals_ns, locals_ns, "exec"
    )
    if isinstance(source, (bytes, bytearray, memoryview)):
        source = bytes(source).decode("utf-8-sig")
    if isinstance(source, str):
        source = compile(source, "<string>", "exec")
    elif not isinstance(source, code):
        raise TypeError("exec() arg 1 must be a string, bytes or code object")
    if source._ast is None:
        raise NotImplementedError(
            "runtime execution of non-AST code objects is not implemented"
        )
    evaluator = _ExpressionEvaluator(globals_ns, locals_ns)
    if source._mode == "eval":
        evaluator.eval(source._ast)
        return None
    evaluator.exec_module(source._ast)
    return None
