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
        self.definitions = {"def": [], "class": []}
        previous = None
        for token in lex(source, filename):
            if token.kind == "NAME" and token.text in self.definitions:
                position = token.start
                start = position
                if (
                    token.text == "def"
                    and previous is not None
                    and previous.text == "async"
                ):
                    start = previous.start
                self.definitions[token.text].append(
                    ((position.line, position.column), start)
                )
            previous = token

    def column(self, position):
        return len(self.lines[position.line - 1][: position.column].encode("utf-8"))

    def set(self, node, result, start=None, end=None):
        if "lineno" in result._attributes:
            start = node.span.start if start is None else start
            end = node.span.end if end is None else end
            result.lineno, result.col_offset = start.line, self.column(start)
            result.end_lineno, result.end_col_offset = end.line, self.column(end)
        return result

    def definition_start(self, node, keyword):
        definitions = self.definitions[keyword]
        key = (node.span.start.line, node.span.start.column)
        low, high = 0, len(definitions)
        while low < high:
            middle = (low + high) // 2
            if definitions[middle][0] < key:
                low = middle + 1
            else:
                high = middle
        if low == len(definitions):
            raise RuntimeError("parsed definition has no definition token")
        return definitions[low][1]


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


def parse(
    source, filename, mode, *, type_comments=False, feature_version=None, optimize=-1
):
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
    if isinstance(source, bytes):
        source = source.decode("utf-8-sig")
    if not isinstance(source, str):
        raise TypeError("compile() arg 1 must be a string, bytes or AST object")
    source = source.replace("\r\n", "\n").replace("\r", "\n")
    tree = _parse(source, filename=filename, mode=mode, type_comments=type_comments)
    return tree.accept(_Convert(_Locations(source, filename)))
