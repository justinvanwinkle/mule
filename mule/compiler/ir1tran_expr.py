"""Expression, display/comprehension, call target, and structural pattern matching IR1 lowering."""

from __future__ import annotations

from dataclasses import replace

from mule import ast

from . import fndb
from .ir1tran_scope import (
    _BASE_CLASS_NAME,
    _IS_REJECTED_SUM_START,
    _IS_UNPACK_NODE,
    _PLAIN_TUPLE_ELEMENTS,
    _TEXT_LITERAL_VALUE,
    AttributeInfo,
    CallInfo,
    FormatInfo,
    _comprehension_assignments,
    _ComprehensionAssignment,
    _ContainsAsync,
    _PlainPairElements,
    _simple_percent_pieces,
)
from .node import (
    Ctran,
    LambdaVar,
    Lvar,
)


class _Expressions(ast.Visitor):
    """Expression translators: each delivers its value to the frame's result."""

    def __init__(self, function):
        self.function = function

    @property
    def frame(self):
        return self.function.frames[-1]

    def visit_Node(self, node):
        self.function.error(
            "syntax is not supported by the native compiler yet",
            node,
            unsupported=True,
        )

    def literal(self, node):
        start, next_, result = self.frame
        self.function.constant(start, next_, result, node)

    visit_IntegerLiteral = literal
    visit_FloatLiteral = literal
    visit_ComplexLiteral = literal
    visit_BooleanLiteral = literal
    visit_NoneLiteral = literal
    visit_TextLiteral = literal
    visit_BytesLiteral = literal

    def visit_EllipsisLiteral(self, node):
        start, next_, result = self.frame
        self.function.protocol(start, next_, result, "ellipsis_value", (), node)

    def visit_Name(self, node):
        function = self.function
        constant = function.converter.named_constants.get(node.identifier)
        if constant is not None:
            start, next_, result = self.frame
            function.constant(start, next_, result, replace(constant, span=node.span))
            return
        if node.identifier in function.converter.compiler_bindings:
            function.error(
                f"compiler-only import '{node.identifier}' cannot be used as a runtime value",
                node,
                unsupported=True,
            )
        start, next_, result = self.frame
        function.load(start, next_, result, node.identifier, node)

    def visit_Subscript(self, node):
        function = self.function
        start, next_, result = self.frame
        ctran, value = function.value(start, node.value)
        ctran, index = function.value(ctran, node.index)
        function.protocol(ctran, next_, result, "getitem", (value, index), node)

    def visit_Slice(self, node):
        function = self.function
        start, next_, result = self.frame
        bounds = tuple(
            bound if bound is not None else ast.NoneLiteral(None, span=node.span)
            for bound in (node.lower, node.upper, node.step)
        )
        function.protocol(start, next_, result, "make_slice", bounds, node)

    def visit_Dict(self, node):
        """Keys and values are evaluated left to right, then inserted in order."""
        function = self.function
        start, next_, result = self.frame
        if any(entry.key is None for entry in node.entries):
            self.unpacking_dict(node)
            return
        flat = []
        for entry in node.entries:
            flat.extend((entry.key, entry.value))
        ctran, items = Ctran(), Lvar()
        function.known(start, ctran, items, fndb.BUILD_TUPLE, tuple(flat), node)
        function.protocol(ctran, next_, result, "build_dict", (items,), node)

    def visit_Set(self, node):
        self.display(node, self.set_display, "build_set")

    def set_display(self, node):
        function = self.function
        start, next_, result = self.frame
        ctran, items = Ctran(), Lvar()
        function.known(
            start, ctran, items, fndb.BUILD_TUPLE, tuple(node.elements), node
        )
        function.protocol(ctran, next_, result, "build_set", (items,), node)

    def visit_Yield(self, node):
        function = self.function
        start, next_, result = self.frame
        if function.is_module or function.class_scopes or function.comprehension_scopes:
            function.error("'yield' outside function", node)
        value = node.value if node.value is not None else ast.NoneLiteral(None)
        if function.body_kind == "async_generator":
            if node.from_iterable:
                function.error("'yield from' inside async function", node)
            function.protocol(
                start, next_, result, "async_generator_yield", (value,), node
            )
        elif node.from_iterable:
            function.protocol(start, next_, result, "yield_from", (value,), node)
        else:
            function.known(start, next_, result, fndb.YIELD, (value,), node)

    def visit_Await(self, node):
        function = self.function
        start, next_, result = self.frame
        if (
            function.body_kind not in ("coroutine", "async_generator")
            or function.class_scopes
        ):
            function.error("'await' outside async function", node)
        function.protocol(start, next_, result, "await_value", (node.value,), node)

    def generator_expression(self, node):
        """A generator expression is a generator function called with the
        iterator of its first iterable, which is obtained in the enclosing
        scope when the expression runs; CPython 3.14 still compiles it so."""
        function = self.function
        start, next_, result = self.frame
        span = node.span
        body: list[ast.Statement] = [
            ast.ExpressionStatement(ast.Yield(node.element, span=span), span=span)
        ]
        for index in range(len(node.clauses) - 1, -1, -1):
            clause = node.clauses[index]
            for condition in reversed(clause.conditions):
                body = [ast.If(condition, body, span=span)]
            source = ast.Name("$iterable", span=span) if index == 0 else clause.iterable
            body = [
                ast.For(
                    clause.target, source, body, is_async=clause.is_async, span=span
                )
            ]
        is_async = any(stmt.accept(_ContainsAsync()) for stmt in body)
        # Assignment expressions bind in the enclosing scope (PEP 572).
        assigned = _comprehension_assignments(node)
        if assigned:
            declaration = ast.Global if function.is_module else ast.Nonlocal
            body = [declaration(assigned, span=span), *body]
        definition = ast.FunctionDefinition(
            "<genexpr>",
            [ast.Parameter("$iterable", ast.ParameterKind.POSITIONAL_ONLY, span=span)],
            body,
            is_async=is_async,
            span=span,
        )
        lambda_ = function.converter.function(
            definition, function.lambda_.policy, parent=function
        )
        ctran, created = Ctran(), Lvar()
        function.define_function(
            start, ctran, created, lambda_, definition, definition.name
        )
        ctran, iterable = function.value(ctran, node.clauses[0].iterable)
        if node.clauses[0].is_async:
            following, iterator = ctran, iterable
        else:
            following, iterator = Ctran(), Lvar()
            function.protocol(ctran, following, iterator, "iterate", (iterable,), node)
        function.full_call(
            following,
            next_,
            result,
            created,
            (iterator,),
            node,
            CallInfo((), function.converter.location(node)),
        )

    def visit_Comprehension(self, node):
        """A list, set or dict comprehension, inlined in the enclosing function
        as Python 3.12 and later do (PEP 709). The first iterable is evaluated
        in the enclosing scope. The clause targets are bound to hidden
        variables, so they neither see nor change enclosing bindings of the
        same names, and inside a class body the comprehension's other names
        skip the class namespace, as they did when comprehensions were
        functions. SBCL likewise expands iteration forms into its ordinary
        variable and loop constructs rather than adding a special form."""
        function = self.function
        start, next_, result = self.frame
        if node.accept(_ComprehensionAssignment()) and function.class_scopes:
            function.error(
                "assignment expression within a comprehension cannot be used in a class body",
                node,
            )
        if node.kind == ast.ComprehensionKind.GENERATOR:
            self.generator_expression(node)
            return
        function.collect_comprehension(
            start,
            next_,
            result,
            node,
            node.kind,
            key=node.key,
        )

    def visit_List(self, node):
        self.display(
            node, lambda display: self.sequence(display, fndb.BUILD_LIST), None
        )

    def visit_Tuple(self, node):
        self.display(
            node,
            lambda display: self.sequence(display, fndb.BUILD_TUPLE),
            "tuple_from_list",
        )

    def display(self, node, build, conversion):
        lowering = _DisplayLowering(self, build, conversion)
        for element in node.elements:
            element.accept(lowering)
        lowering.build(node)

    def unpacking_display(self, node, conversion):
        """A display with ``*iterable`` elements: the elements, in order, are
        collected into a new list, then converted to the display's type."""
        function = self.function
        start, next_, result = self.frame
        items = function.temporary()
        ctran, value = function.value(start, ast.List([], span=node.span))
        current = Ctran()
        function.set_var(ctran, current, items, value, node)
        for element in node.elements:
            following = Ctran()
            element.accept(_DisplayItem(function, current, following, items))
            current = following
        if conversion is None:
            function.ref(current, next_, result, items, node, final=True)
            return
        ctran, held = Ctran(), Lvar()
        function.ref(current, ctran, held, items, node, final=True)
        function.protocol(ctran, next_, result, conversion, (held,), node)

    def unpacking_dict(self, node):
        """A dict display with ``**mapping`` entries, merged in order, later
        keys replacing earlier ones."""
        function = self.function
        start, next_, result = self.frame
        mapping = function.temporary()
        ctran, value = function.value(start, ast.Dict([], span=node.span))
        current = Ctran()
        function.set_var(ctran, current, mapping, value, node)
        for entry in node.entries:
            ctran, held = Ctran(), Lvar()
            function.ref(current, ctran, held, mapping, entry)
            following = Ctran()
            if entry.key is None:
                function.protocol(
                    ctran, following, None, "dict_merge", (held, entry.value), entry
                )
            else:
                function.protocol(
                    ctran,
                    following,
                    None,
                    "setitem",
                    (held, entry.key, entry.value),
                    entry,
                )
            current = following
        function.ref(current, next_, result, mapping, node, final=True)

    def sequence(self, node, info):
        function = self.function
        start, next_, result = self.frame
        function.known(start, next_, result, info, tuple(node.elements), node)

    def visit_FormattedString(self, node):
        function = self.function
        start, next_, result = self.frame
        if not node.parts:
            function.constant(start, next_, result, ast.TextLiteral(""))
            return
        if len(node.parts) == 1:
            function.convert(start, next_, result, node.parts[0])
            return
        # Every part is evaluated before any concatenation, as the previous
        # lowering did; concatenation then folds left.
        parts = []
        current = start
        for part in node.parts:
            current, value = function.value(current, part)
            parts.append(value)
        joined = parts[0]
        for index, part in enumerate(parts[1:]):
            last = index == len(parts) - 2
            ctran = next_ if last else Ctran()
            value = result if last else Lvar()
            function.known(
                current,
                ctran,
                value,
                fndb.BINARY[ast.BinaryOperator.ADD],
                (joined, part),
                node,
            )
            current, joined = ctran, value

    def visit_TemplateString(self, node):
        """``t"..."``: the literal parts and an Interpolation per field,
        evaluated left to right, then combined into a Template."""
        function = self.function
        start, next_, result = self.frame
        current = start
        parts = []
        for part in node.parts:
            current, value = part.accept(_TemplatePart(function, current))
            parts.append(value)
        ctran, items = Ctran(), Lvar()
        function.known(current, ctran, items, fndb.BUILD_TUPLE, tuple(parts), node)
        function.protocol(ctran, next_, result, "build_template", (items,), node)

    def visit_Interpolation(self, node):
        function = self.function
        start, next_, result = self.frame
        if node.conversion not in (None, "s", "r", "a"):
            function.error("unsupported f-string conversion", node, unsupported=True)
        ctran, value = function.value(start, node.expression)
        if node.format_spec is not None:
            ctran, specification = function.value(ctran, node.format_spec)
        else:
            following, specification = Ctran(), Lvar()
            function.constant(ctran, following, specification, ast.TextLiteral(""))
            ctran = following
        # Debug fields without an explicit format specification imply !r.
        conversion = node.conversion
        if node.expression_text is not None and node.format_spec is None:
            conversion = conversion or "r"
        function.known(
            ctran,
            next_,
            result,
            fndb.FORMAT,
            (value, specification),
            node,
            FormatInfo(conversion),
        )

    def visit_Attribute(self, node):
        function = self.function
        start, next_, result = self.frame
        ctran, receiver = function.value(start, node.value)
        function.known(
            ctran,
            next_,
            result,
            fndb.GETATTR,
            (receiver,),
            node,
            AttributeInfo(node.attribute, function.converter.location(node)),
        )

    def visit_Binary(self, node):
        function = self.function
        start, next_, result = self.frame
        if node.operator is ast.BinaryOperator.MODULO and self.percent_format(node):
            return
        ctran, left = function.value(start, node.left)
        ctran, right = function.value(ctran, node.right)
        function.known(
            ctran, next_, result, fndb.BINARY[node.operator], (left, right), node
        )

    def percent_format(self, node) -> bool:
        """Convert ``"..." % (a, b)`` with a constant pattern of ``%s``, ``%r``,
        ``%a``, and ``%%`` as the equivalent formatted string, as SBCL's
        ``format`` transform in ``srctran.lisp`` expands a constant control
        string. A text literal's ``%`` is ``str.__mod__``, and a tuple display
        is the exact argument tuple, so the conversion keeps the order of
        effects: every element is evaluated, then each is converted in turn.
        Other patterns, other operands, and arity mismatches keep the general
        operation, which raises the runtime's errors."""
        pattern = node.left.accept(_TEXT_LITERAL_VALUE)
        elements = node.right.accept(_PLAIN_TUPLE_ELEMENTS)
        if pattern is None or elements is None:
            return False
        pieces = _simple_percent_pieces(pattern)
        if pieces is None or sum(1 for _, field in pieces if field) != len(elements):
            return False
        function = self.function
        start, next_, result = self.frame
        current = start
        values = []
        for element in elements:
            current, value = function.value(current, element)
            values.append(value)
        operands = iter(values)
        parts = [
            (text, next(operands) if conversion else None, conversion)
            for text, conversion in pieces
        ]
        if not parts:
            function.constant(current, next_, result, ast.TextLiteral(""))
            return True
        joined = None
        for index, (text, value, conversion) in enumerate(parts):
            last = index == len(parts) - 1
            single = len(parts) == 1
            ctran = next_ if single else Ctran()
            part = result if single else Lvar()
            if value is None:
                function.constant(current, ctran, part, ast.TextLiteral(text))
            else:
                following, specification = Ctran(), Lvar()
                function.constant(
                    current, following, specification, ast.TextLiteral("")
                )
                function.known(
                    following,
                    ctran,
                    part,
                    fndb.FORMAT,
                    (value, specification),
                    node,
                    FormatInfo(conversion),
                )
            current = ctran
            if joined is None:
                joined = part
                continue
            ctran = next_ if last else Ctran()
            value = result if last else Lvar()
            function.known(
                current,
                ctran,
                value,
                fndb.BINARY[ast.BinaryOperator.ADD],
                (joined, part),
                node,
            )
            current, joined = ctran, value
        return True

    def visit_Unary(self, node):
        function = self.function
        start, next_, result = self.frame
        ctran, operand = function.value(start, node.operand)
        function.known(
            ctran, next_, result, fndb.UNARY[node.operator], (operand,), node
        )

    def visit_Call(self, node):
        start, next_, result = self.frame
        self.function.call(start, next_, result, node)

    def visit_NamedExpression(self, node):
        function = self.function
        start, next_, result = self.frame
        temporary = function.temporary()
        ctran, value = function.value(start, node.value)
        following = Ctran()
        function.set_var(ctran, following, temporary, value, node)
        ctran, stored = Ctran(), Lvar()
        function.ref(following, ctran, stored, temporary, node)
        following = Ctran()
        function.store(ctran, following, node.target.identifier, stored, node.target)
        function.ref(following, next_, result, temporary, node, final=True)

    def visit_Boolean(self, node):
        start, next_, result = self.frame
        self.function.boolean(start, next_, result, node.operator, node.operands, node)

    def visit_Conditional(self, node):
        function = self.function
        start, next_, result = self.frame
        then, other, join = function.block(), function.block(), function.block()
        temporary = function.temporary()
        function.condition(start, node.condition, then, other)
        ctran, value = function.value(then.start, node.then)
        function.set_var(ctran, join.start, temporary, value, node.then)
        ctran, value = function.value(other.start, node.otherwise)
        function.set_var(ctran, join.start, temporary, value, node.otherwise)
        function.ref(join.start, next_, result, temporary, node, final=True)

    def visit_Comparison(self, node):
        start, next_, result = self.frame
        self.function.comparison(start, next_, result, node)

    def visit_Lambda(self, node):
        """A lambda is an anonymous function whose body returns one expression,
        converted like ``def`` as SBCL converts ``lambda`` and named functions
        through the same ``ir1-convert-lambda`` path."""
        function = self.function
        start, next_, result = self.frame
        definition = ast.FunctionDefinition(
            "<lambda>",
            node.parameters,
            [ast.Return(node.body, span=node.body.span)],
            span=node.span,
        )
        lambda_ = function.converter.function(
            definition,
            function.lambda_.policy,
            parent=function,
            defining_class=function.defining_class
            if function.annotation_scope
            else None,
            annotation_scope=function.annotation_scope,
            source_identity=node,
        )
        function.define_function(
            start, next_, result, lambda_, definition, definition.name
        )


class _DisplayLowering(ast.Visitor):
    """How a display is built: by ``build`` over its elements, or, once an
    element is ``*iterable``, by collecting the elements into a list that
    ``conversion`` turns into the display's type."""

    def __init__(self, expressions: _Expressions, build, conversion):
        self.expressions = expressions
        self.build = build
        self.conversion = conversion

    def visit_Unpack(self, node):
        self.build = self.unpacking

    def unpacking(self, node):
        self.expressions.unpacking_display(node, self.conversion)


class _DisplayItem(ast.Visitor):
    """Adds one display element to the list in ``items``."""

    def __init__(self, function, start, next_, items: LambdaVar):
        self.function = function
        self.start = start
        self.next = next_
        self.items = items

    def visit_Node(self, node):
        ctran, held = Ctran(), Lvar()
        self.function.ref(self.start, ctran, held, self.items, node)
        self.function.protocol(
            ctran, self.next, None, "append_argument", (held, node), node
        )

    def visit_Unpack(self, node):
        genexpr = self.function.fusible_generator_expression(node.value)
        if genexpr is not None:
            items_name = ast.Name(self.items.name, span=node.span)
            self.function.drive_iterable(
                self.start,
                self.next,
                genexpr,
                lambda elem, s: [
                    ast.ExpressionStatement(
                        ast.Call(
                            ast.Attribute(items_name, "append", span=s),
                            [ast.CallArgument(elem, span=s)],
                            span=s,
                        ),
                        span=s,
                    )
                ],
                node,
            )
            return
        ctran, held = Ctran(), Lvar()
        self.function.ref(self.start, ctran, held, self.items, node)
        self.function.protocol(
            ctran,
            self.next,
            None,
            "extend_arguments",
            (held, node.value),
            node,
        )


class _TemplatePart(ast.Visitor):
    """Evaluates one part of a template string from ``start``: a literal
    part to its value, a field to an Interpolation. Returns the following
    ctran and the value."""

    def __init__(self, function, start):
        self.function = function
        self.start = start

    def visit_Node(self, node):
        return self.function.value(self.start, node)

    def visit_Interpolation(self, node):
        function = self.function
        current, value = function.value(self.start, node.expression)
        operands = [value]
        for operand in (
            ast.TextLiteral((node.expression_text or "").rstrip(), span=node.span),
            ast.TextLiteral(node.conversion, span=node.span)
            if node.conversion is not None
            else ast.NoneLiteral(None, span=node.span),
            node.format_spec
            if node.format_spec is not None
            else ast.TextLiteral("", span=node.span),
        ):
            current, value = function.value(current, operand)
            operands.append(value)
        ctran, interpolation = Ctran(), Lvar()
        function.protocol(
            current, ctran, interpolation, "make_interpolation", tuple(operands), node
        )
        return ctran, interpolation


_SELF_MATCHING_BUILTINS = frozenset(
    {
        "bool",
        "bytearray",
        "bytes",
        "dict",
        "float",
        "frozenset",
        "int",
        "list",
        "set",
        "str",
        "tuple",
    }
)


class _MatchLowering:
    """A ``match`` statement as tests and assignments over hidden variables,
    the decision sequence CPython's compiler emits. Each pattern sets a flag
    and binds its names as it matches; later cases run only when earlier
    ones fail. The ``_match`` module supplies the sequence, mapping and class
    checks CPython performs natively."""

    def __init__(self, function, span):
        self.function = function
        self.span = span
        self.helpers = None
        self.missing = None

    def temporary(self):
        return self.function.temporary().name

    def name(self, identifier):
        return ast.Name(identifier, span=self.span)

    def assign(self, target, value):
        return ast.Assignment([self.name(target)], value, span=self.span)

    def constant(self, value):
        return ast.BooleanLiteral(value, span=self.span)

    def call(self, helper, *arguments):
        if self.helpers is None:
            self.helpers = self.temporary()
        return ast.Call(
            ast.Attribute(self.name(self.helpers), helper, span=self.span),
            [ast.CallArgument(argument, span=self.span) for argument in arguments],
            span=self.span,
        )

    def missing_sentinel(self):
        if self.helpers is None:
            self.helpers = self.temporary()
        if self.missing is None:
            self.missing = self.temporary()
        return self.name(self.missing)

    def compare(self, left, operator, right):
        return ast.Comparison(left, [operator], [right], span=self.span)

    def when(self, flag, body):
        return ast.If(self.name(flag), body, span=self.span)

    def index(self, sequence, position):
        return ast.Subscript(
            self.name(sequence),
            ast.IntegerLiteral(position, span=self.span),
            span=self.span,
        )

    def unshadowed_global(self, name: str) -> bool:
        return self.function.unshadowed_global(name)

    def unshadowed_builtin(self, name: str) -> bool:
        return self.function.unshadowed_builtin(name)

    def statements(self, node):
        elements = node.subject.accept(_UNPACK_FREE_SEQUENCE_SUBJECT)
        if elements is not None and all(
            case.pattern.accept(_FixedTuplePattern(len(elements)))
            for case in node.cases
        ):
            subject_items = [self.temporary() for _ in elements]
            result = [
                *[
                    self.assign(target, element)
                    for target, element in zip(subject_items, elements, strict=True)
                ],
                *self.tuple_cases(node.cases, 0, subject_items),
            ]
        else:
            subject = self.temporary()
            result = [
                self.assign(subject, node.subject),
                *self.cases(node.cases, 0, subject),
            ]
        if self.missing is not None:
            result.insert(
                0,
                self.assign(
                    self.missing,
                    ast.Attribute(self.name(self.helpers), "_MISSING", span=self.span),
                ),
            )
        if self.helpers is not None:
            result.insert(
                0,
                ast.Import(
                    [ast.ImportItem("_match", self.helpers, span=self.span)],
                    span=self.span,
                ),
            )
        return result

    def tuple_cases(self, cases, position, subject_items):
        if position == len(cases):
            return []
        case = cases[position]
        flag = self.temporary()
        test = self.name(flag)
        if case.guard is not None:
            test = ast.Boolean(
                ast.BooleanOperator.AND, [test, case.guard], span=self.span
            )
        return [
            *self.tuple_pattern(case.pattern, subject_items, flag),
            ast.If(
                test,
                list(case.body) or [ast.Pass(span=self.span)],
                self.tuple_cases(cases, position + 1, subject_items),
                span=self.span,
            ),
        ]

    def tuple_pattern(self, pattern, subject_items, flag):
        return pattern.accept(_TuplePatternTest(self, subject_items, flag))

    def cases(self, cases, position, subject):
        if position == len(cases):
            return []
        case = cases[position]
        flag = self.temporary()
        test = self.name(flag)
        if case.guard is not None:
            test = ast.Boolean(
                ast.BooleanOperator.AND, [test, case.guard], span=self.span
            )
        return [
            *self.pattern(case.pattern, subject, flag),
            ast.If(
                test,
                list(case.body) or [ast.Pass(span=self.span)],
                self.cases(cases, position + 1, subject),
                span=self.span,
            ),
        ]

    def pattern(self, pattern, subject, flag):
        """Statements leaving ``flag`` true when ``pattern`` matches the
        value in the variable ``subject``."""
        return pattern.accept(_PatternTest(self, subject, flag))

    def direct_items(self, expressions, patterns, flag, *, flag_is_true: bool = False):
        """Match each pattern against the corresponding expression in order,
        stopping at the first failure without building an intermediate sequence."""
        result = []
        for expr, pattern in reversed(list(zip(expressions, patterns, strict=True))):
            is_capture, capture_name = pattern.accept(_CAPTURE_PATTERN_INFO)
            if is_capture:
                bind = [] if capture_name is None else [self.assign(capture_name, expr)]
                if result:
                    result = [*bind, *result]
                elif flag_is_true:
                    result = bind
                else:
                    result = [*bind, self.assign(flag, self.constant(True))]
            else:
                item = self.temporary()
                result = [
                    self.assign(item, expr),
                    *self.pattern(pattern, item, flag),
                    *([self.when(flag, result)] if result else []),
                ]
        return result

    def items(self, values, patterns, flag, positions):
        """Match each pattern against the item at its position, in order,
        stopping at the first failure."""
        return self.direct_items(
            [self.index(values, position) for position in positions],
            patterns,
            flag,
        )

    def sequence(self, pattern, subject, flag):
        values = self.temporary()
        elements = pattern.elements
        stars = _star_positions(elements)
        if self.unshadowed_builtin("len"):
            length = ast.Call(
                self.name("len"),
                [ast.CallArgument(self.name(values), span=self.span)],
                span=self.span,
            )
        else:
            length = ast.Call(
                ast.Attribute(self.name(values), "__len__", span=self.span),
                span=self.span,
            )
        count = ast.IntegerLiteral(len(elements) - len(stars), span=self.span)
        size_check = self.compare(
            length,
            ast.ComparisonOperator.GREATER_EQUAL
            if stars
            else ast.ComparisonOperator.EQUAL,
            count,
        )
        split = _SequenceElements(self, values, elements, stars)
        patterns, positions, captures = split.patterns, split.positions, split.captures
        matched = [
            self.assign(flag, self.constant(True)),
            *self.direct_items(
                [self.index(values, position) for position in positions],
                patterns,
                flag,
                flag_is_true=True,
            ),
            *(
                ([self.when(flag, captures)] if patterns else captures)
                if captures
                else []
            ),
        ]
        return [
            self.assign(values, self.call("sequence", self.name(subject))),
            self.assign(
                flag,
                self.compare(
                    self.name(values),
                    ast.ComparisonOperator.IS_NOT,
                    ast.NoneLiteral(None, span=self.span),
                ),
            ),
            self.when(flag, [self.assign(flag, size_check), self.when(flag, matched)]),
        ]

    def mapping(self, pattern, subject, flag):
        seen_literals = []
        key_visitor = _MappingLiteralKey()
        for entry in pattern.entries:
            is_literal, literal_value = entry.key.accept(key_visitor)
            if is_literal:
                if literal_value in seen_literals:
                    self.function.error(
                        f"mapping pattern checks duplicate key ({literal_value!r})",
                        entry.key,
                    )
                seen_literals.append(literal_value)
        keys = ast.Tuple([entry.key for entry in pattern.entries], span=self.span)
        rest = []
        if pattern.rest is not None:
            rest = [
                self.assign(
                    pattern.rest, self.call("mapping_rest", self.name(subject), keys)
                )
            ]
        if len(seen_literals) == len(pattern.entries):
            inner: list[ast.Statement] = list(rest)
            for entry in reversed(pattern.entries):
                val = self.temporary()
                is_capture, capture_name = entry.pattern.accept(_CAPTURE_PATTERN_INFO)
                if is_capture:
                    bind = (
                        []
                        if capture_name is None
                        else [self.assign(capture_name, self.name(val))]
                    )
                    after = [*bind, *inner]
                else:
                    after = [
                        *self.pattern(entry.pattern, val, flag),
                        *([self.when(flag, inner)] if inner else []),
                    ]
                inner = [
                    self.assign(
                        val, self.call("mapping_get", self.name(subject), entry.key)
                    ),
                    self.assign(
                        flag,
                        self.compare(
                            self.name(val),
                            ast.ComparisonOperator.IS_NOT,
                            self.missing_sentinel(),
                        ),
                    ),
                    *([self.when(flag, after)] if after else []),
                ]
            if not inner:
                inner = [ast.Pass(span=self.span)]
            return [
                self.assign(flag, self.call("mapping", self.name(subject))),
                self.when(flag, inner),
            ]
        values = self.temporary()
        inner = [
            self.assign(values, self.call("mapping_values", self.name(subject), keys)),
            self.assign(
                flag,
                self.compare(
                    self.name(values),
                    ast.ComparisonOperator.IS_NOT,
                    ast.NoneLiteral(None, span=self.span),
                ),
            ),
            self.when(
                flag,
                [
                    *self.items(
                        values,
                        [entry.pattern for entry in pattern.entries],
                        flag,
                        list(range(len(pattern.entries))),
                    ),
                    *([self.when(flag, rest)] if rest else []),
                ]
                if pattern.entries
                else rest or [ast.Pass(span=self.span)],
            ),
        ]
        return [
            self.assign(flag, self.call("mapping", self.name(subject))),
            self.when(flag, inner),
        ]

    def class_pattern(self, pattern, subject, flag):
        cls_name = pattern.class_name.accept(_BASE_CLASS_NAME)
        if (
            cls_name is not None
            and cls_name in _SELF_MATCHING_BUILTINS
            and self.unshadowed_builtin(cls_name)
            and self.unshadowed_builtin("isinstance")
            and len(pattern.positional) <= 1
            and not pattern.keywords
        ):
            isinstance_call = ast.Call(
                self.name("isinstance"),
                [
                    ast.CallArgument(self.name(subject), span=self.span),
                    ast.CallArgument(
                        ast.Tuple([pattern.class_name], span=self.span),
                        span=self.span,
                    ),
                ],
                span=self.span,
            )
            sub_stmts = (
                self.direct_items(
                    [self.name(subject)],
                    [pattern.positional[0]],
                    flag,
                    flag_is_true=True,
                )
                if len(pattern.positional) == 1
                and pattern.positional[0].accept(_CAPTURE_PATTERN_INFO) != (True, None)
                else []
            )
            return [
                self.assign(flag, isinstance_call),
                *([self.when(flag, sub_stmts)] if sub_stmts else []),
            ]
        match_info = (
            self.function.converter.declared_class_match_info.get(cls_name)
            if cls_name is not None
            else None
        )
        if (
            match_info is not None
            and self.unshadowed_global(cls_name)
            and self.unshadowed_builtin("isinstance")
        ):
            match_args, guaranteed_fields = match_info
            pos_count = len(pattern.positional)
            if (match_args is not None or pos_count == 0) and (
                pos_count <= len(match_args or ())
            ):
                field_names = [
                    *(match_args or ())[:pos_count],
                    *(keyword.name for keyword in pattern.keywords),
                ]
                if len(set(field_names)) == len(field_names) and all(
                    field in guaranteed_fields for field in field_names
                ):
                    patterns = [
                        *pattern.positional,
                        *(keyword.pattern for keyword in pattern.keywords),
                    ]
                    active_pairs = [
                        (field, subpattern)
                        for field, subpattern in zip(field_names, patterns, strict=True)
                        if subpattern.accept(_CAPTURE_PATTERN_INFO) != (True, None)
                    ]
                    isinstance_call = ast.Call(
                        self.name("isinstance"),
                        [
                            ast.CallArgument(self.name(subject), span=self.span),
                            ast.CallArgument(
                                ast.Tuple([pattern.class_name], span=self.span),
                                span=self.span,
                            ),
                        ],
                        span=self.span,
                    )
                    sub_stmts = (
                        self.direct_items(
                            [
                                ast.Attribute(self.name(subject), field, span=self.span)
                                for field, _ in active_pairs
                            ],
                            [subpattern for _, subpattern in active_pairs],
                            flag,
                            flag_is_true=True,
                        )
                        if active_pairs
                        else []
                    )
                    return [
                        self.assign(flag, isinstance_call),
                        *([self.when(flag, sub_stmts)] if sub_stmts else []),
                    ]
        values = self.temporary()
        names = ast.Tuple(
            [
                ast.TextLiteral(keyword.name, span=self.span)
                for keyword in pattern.keywords
            ],
            span=self.span,
        )
        patterns = [
            *pattern.positional,
            *(keyword.pattern for keyword in pattern.keywords),
        ]
        return [
            self.assign(
                values,
                self.call(
                    "class_attributes",
                    self.name(subject),
                    pattern.class_name,
                    ast.IntegerLiteral(len(pattern.positional), span=self.span),
                    names,
                ),
            ),
            self.assign(
                flag,
                self.compare(
                    self.name(values),
                    ast.ComparisonOperator.IS_NOT,
                    ast.NoneLiteral(None, span=self.span),
                ),
            ),
            *(
                [
                    self.when(
                        flag,
                        self.items(values, patterns, flag, list(range(len(patterns)))),
                    )
                ]
                if patterns
                else []
            ),
        ]


class _MappingLiteralKey(ast.Visitor):
    """Extracts a compile-time literal key from a mapping pattern entry."""

    def visit_Node(self, node):
        return (False, None)

    def visit_NoneLiteral(self, node):
        return (True, None)

    def visit_BooleanLiteral(self, node):
        return (True, node.value)

    def visit_IntegerLiteral(self, node):
        return (True, node.value)

    def visit_FloatLiteral(self, node):
        return (True, node.value)

    def visit_ComplexLiteral(self, node):
        return (True, node.value)

    def visit_TextLiteral(self, node):
        return (True, node.value)

    def visit_BytesLiteral(self, node):
        return (True, node.value)

    def visit_Unary(self, node):
        if node.operator is ast.UnaryOperator.NEGATIVE:
            is_literal, value = node.operand.accept(self)
            if is_literal:
                return (True, -value)
        return (False, None)

    def visit_Binary(self, node):
        left_literal, left_value = node.left.accept(self)
        right_literal, right_value = node.right.accept(self)
        if left_literal and right_literal:
            if node.operator is ast.BinaryOperator.ADD:
                return (True, left_value + right_value)
            if node.operator is ast.BinaryOperator.SUB:
                return (True, left_value - right_value)
        return (False, None)


class _PatternTest(ast.Visitor):
    """One pattern's statements for ``_MatchLowering.pattern``."""

    def __init__(self, lowering: _MatchLowering, subject, flag):
        self.lowering = lowering
        self.subject = subject
        self.flag = flag

    def visit_Node(self, node):
        self.lowering.function.error("unsupported pattern", node, unsupported=True)

    def visit_CapturePattern(self, node):
        lowering, flag = self.lowering, self.flag
        bind = (
            []
            if node.name is None
            else [lowering.assign(node.name, lowering.name(self.subject))]
        )
        return [*bind, lowering.assign(flag, lowering.constant(True))]

    def visit_LiteralPattern(self, node):
        lowering = self.lowering
        operator = node.value.literal_pattern_operator()
        return [
            lowering.assign(
                self.flag,
                lowering.compare(lowering.name(self.subject), operator, node.value),
            )
        ]

    def visit_ValuePattern(self, node):
        lowering = self.lowering
        return [
            lowering.assign(
                self.flag,
                lowering.compare(
                    lowering.name(self.subject),
                    ast.ComparisonOperator.EQUAL,
                    node.value,
                ),
            )
        ]

    def visit_AsPattern(self, node):
        lowering, subject, flag = self.lowering, self.subject, self.flag
        return [
            *lowering.pattern(node.pattern, subject, flag),
            lowering.when(flag, [lowering.assign(node.name, lowering.name(subject))]),
        ]

    def visit_OrPattern(self, node):
        lowering, subject, flag = self.lowering, self.subject, self.flag
        result = list(lowering.pattern(node.alternatives[0], subject, flag))
        for alternative in node.alternatives[1:]:
            result.append(
                ast.If(
                    ast.Unary(
                        ast.UnaryOperator.NOT, lowering.name(flag), span=lowering.span
                    ),
                    lowering.pattern(alternative, subject, flag),
                    span=lowering.span,
                )
            )
        return result

    def visit_SequencePattern(self, node):
        return self.lowering.sequence(node, self.subject, self.flag)

    def visit_MappingPattern(self, node):
        return self.lowering.mapping(node, self.subject, self.flag)

    def visit_ClassPattern(self, node):
        return self.lowering.class_pattern(node, self.subject, self.flag)


class _CapturePatternInfo(ast.Visitor):
    def visit_Node(self, node):
        return (False, None)

    def visit_CapturePattern(self, node: ast.CapturePattern):
        return (True, node.name)


_CAPTURE_PATTERN_INFO = _CapturePatternInfo()


class _HasNamedStar(ast.Visitor):
    def visit_Node(self, node):
        return False

    def visit_StarPattern(self, node: ast.StarPattern):
        return node.name is not None


_HAS_NAMED_STAR = _HasNamedStar()


class _UnpackFreeSequenceSubject(ast.Visitor):
    def visit_Node(self, node):
        return None

    def visit_Tuple(self, node: ast.Tuple):
        if any(element.accept(_IS_UNPACK_NODE) for element in node.elements):
            return None
        return list(node.elements)

    def visit_List(self, node: ast.List):
        if any(element.accept(_IS_UNPACK_NODE) for element in node.elements):
            return None
        return list(node.elements)


_UNPACK_FREE_SEQUENCE_SUBJECT = _UnpackFreeSequenceSubject()


class _FixedTuplePattern(ast.Visitor):
    def __init__(self, arity: int):
        self.arity = arity

    def visit_Node(self, node):
        return False

    def visit_CapturePattern(self, node: ast.CapturePattern):
        return node.name is None

    def visit_OrPattern(self, node: ast.OrPattern):
        return all(alt.accept(self) for alt in node.alternatives)

    def visit_SequencePattern(self, node: ast.SequencePattern):
        return len(node.elements) == self.arity and not _star_positions(node.elements)


class _TuplePatternTest(ast.Visitor):
    """Matches a fixed-arity sequence pattern directly against pre-evaluated
    tuple/list subject item temporaries."""

    def __init__(self, lowering: _MatchLowering, subject_items: list[str], flag: str):
        self.lowering = lowering
        self.subject_items = subject_items
        self.flag = flag

    def visit_Node(self, node):
        self.lowering.function.error("unsupported pattern", node, unsupported=True)

    def visit_CapturePattern(self, node: ast.CapturePattern):
        lowering = self.lowering
        return [lowering.assign(self.flag, lowering.constant(True))]

    def visit_OrPattern(self, node: ast.OrPattern):
        lowering, flag = self.lowering, self.flag
        result = list(node.alternatives[0].accept(self))
        for alternative in node.alternatives[1:]:
            result.append(
                ast.If(
                    ast.Unary(
                        ast.UnaryOperator.NOT, lowering.name(flag), span=lowering.span
                    ),
                    alternative.accept(self),
                    span=lowering.span,
                )
            )
        return result

    def visit_SequencePattern(self, node: ast.SequencePattern):
        lowering, flag = self.lowering, self.flag
        sub_stmts = lowering.direct_items(
            [lowering.name(item) for item in self.subject_items],
            node.elements,
            flag,
            flag_is_true=True,
        )
        return [
            lowering.assign(flag, lowering.constant(True)),
            *sub_stmts,
        ]


class _StarPositions(ast.Visitor):
    """Collects the indices of starred elements (``*target`` or ``*pattern``)."""

    def __init__(self):
        self.index = 0
        self.positions: list[int] = []

    def visit_Unpack(self, node):
        self.positions.append(self.index)

    def visit_StarPattern(self, node):
        self.positions.append(self.index)


def _star_positions(elements) -> list[int]:
    collector = _StarPositions()
    for index, element in enumerate(elements):
        collector.index = index
        element.accept(collector)
    return collector.positions


class _SequenceElements(ast.Visitor):
    """Splits a sequence pattern into the subpatterns matched by position
    (negative after the star pattern) and the star pattern's capture."""

    def __init__(self, lowering: _MatchLowering, values, elements, stars):
        self.lowering = lowering
        self.values = values
        self.count = len(elements)
        self.stars = stars
        self.patterns = []
        self.positions = []
        self.captures = []
        self.index = 0
        for index, element in enumerate(elements):
            self.index = index
            element.accept(self)

    def visit_Node(self, node):
        index, stars = self.index, self.stars
        self.patterns.append(node)
        self.positions.append(
            index if not stars or index < stars[0] else index - self.count
        )

    def visit_StarPattern(self, node):
        if node.name is None:
            return
        lowering, index = self.lowering, self.index
        span = lowering.span
        after = self.count - index - 1
        self.captures.append(
            lowering.assign(
                node.name,
                lowering.call(
                    "sequence_slice",
                    lowering.name(self.values),
                    ast.IntegerLiteral(index, span=span),
                    ast.IntegerLiteral(after, span=span),
                ),
            )
        )


class _CallTarget(ast.Visitor):
    def __init__(self, function, start, next_, result, call: ast.Call):
        self.function = function
        self.start = start
        self.next = next_
        self.result = result
        self.call = call

    def keywords(self):
        names = tuple(argument.keyword for argument in self.call.arguments)
        return names if any(name is not None for name in names) else ()

    def arguments(self):
        return tuple(argument.value for argument in self.call.arguments)

    def visit_Node(self, node):
        function = self.function
        ctran, callee = function.value(self.start, node)
        function.full_call(
            ctran,
            self.next,
            self.result,
            callee,
            self.arguments(),
            self.call,
            CallInfo(self.keywords(), function.converter.location(self.call)),
        )

    def visit_Name(self, node):
        """``globals()`` and zero-argument ``super()`` receive an argument the
        source does not write: the calling function's globals namespace, or
        the defining class's ``__class__`` cell and the calling function's
        first argument, as CPython's compiler supplies them. The name itself
        is still looked up normally."""
        function = self.function
        if (
            node.identifier == "globals"
            and not self.call.arguments
            and not function.local("globals")
        ):
            ctran, callee = function.value(self.start, node)
            following, namespace = Ctran(), Lvar()
            function.known(ctran, following, namespace, fndb.CURRENT_GLOBALS, (), node)
            function.full_call(
                following,
                self.next,
                self.result,
                callee,
                (namespace,),
                self.call,
                CallInfo((), function.converter.location(self.call)),
            )
            return
        if (
            node.identifier in ("locals", "vars", "dir")
            and not self.call.arguments
            and not function.local(node.identifier)
            and function.comprehension_variable(node.identifier) is None
            and function.enclosing_local(node.identifier) is None
        ):
            ctran, callee = function.value(self.start, node)
            after_snapshot, snapshot = function.snapshot_current_locals(ctran, node)
            function.protocol(
                after_snapshot,
                self.next,
                self.result,
                "call_with_locals",
                (callee, snapshot),
                self.call,
            )
            return
        if (
            node.identifier in ("eval", "exec")
            and len(self.call.arguments) == 1
            and self.call.arguments[0].kind == ast.ArgumentKind.POSITIONAL
            and not function.local(node.identifier)
            and function.comprehension_variable(node.identifier) is None
            and function.enclosing_local(node.identifier) is None
        ):
            function.component.mutates_module_namespace = True
            ctran, callee = function.value(self.start, node)
            ctran, source_arg = function.value(ctran, self.call.arguments[0].value)
            after_globals, namespace = Ctran(), Lvar()
            function.known(
                ctran, after_globals, namespace, fndb.CURRENT_GLOBALS, (), node
            )
            after_gdict, globals_dict = Ctran(), Lvar()
            function.protocol(
                after_globals,
                after_gdict,
                globals_dict,
                "globals_dict",
                (namespace,),
                node,
            )
            after_ldict, locals_dict = function.snapshot_current_locals(
                after_gdict, node
            )
            function.protocol(
                after_ldict,
                self.next,
                self.result,
                "call_with_eval_context",
                (callee, source_arg, globals_dict, locals_dict),
                self.call,
            )
            return
        if self.fuse_comprehension_call(node):
            return
        if node.identifier != "super" or self.call.arguments:
            return self.visit_Node(node)
        parameters = function.lambda_.vars
        if (
            function.defining_class is None
            or not parameters
            or function.lambda_.signature.kinds[0] == ast.ParameterKind.VAR_POSITIONAL
            or function.local("super")
        ):
            return self.visit_Node(node)
        cell = function.class_cell()
        ctran, callee = function.value(self.start, node)
        following, owner = Ctran(), Lvar()
        function.ref(ctran, following, owner, cell, node)
        ctran, receiver = Ctran(), Lvar()
        function.ref(following, ctran, receiver, parameters[0], node)
        function.full_call(
            ctran,
            self.next,
            self.result,
            callee,
            (owner, receiver),
            self.call,
            CallInfo((), function.converter.location(self.call)),
        )

    def fuse_comprehension_call(self, node: ast.Name) -> bool:
        """Open-code a builtin sequence consumer applied to a generator
        expression, following SBCL's ``src/code/quantifiers.lisp`` and
        ``src/compiler/seqtran.lisp``."""
        name = node.identifier
        if name not in (
            "list",
            "set",
            "tuple",
            "frozenset",
            "sorted",
            "dict",
            "any",
            "all",
            "sum",
            "min",
            "max",
        ):
            return False
        function = self.function
        arguments = self.call.arguments
        if (
            not arguments
            or arguments[0].kind != ast.ArgumentKind.POSITIONAL
            or not function.unshadowed_builtin(name)
        ):
            return False
        genexpr = function.fusible_generator_expression(arguments[0].value)
        if genexpr is None:
            return False
        span = self.call.span
        if name == "list" and len(arguments) == 1:
            function.collect_comprehension(
                self.start,
                self.next,
                self.result,
                genexpr,
                ast.ComprehensionKind.LIST,
            )
            return True
        if name == "set" and len(arguments) == 1:
            function.collect_comprehension(
                self.start,
                self.next,
                self.result,
                genexpr,
                ast.ComprehensionKind.SET,
            )
            return True
        if name == "tuple" and len(arguments) == 1:
            ctran, items = Ctran(), Lvar()
            function.collect_comprehension(
                self.start,
                ctran,
                items,
                genexpr,
                ast.ComprehensionKind.LIST,
            )
            function.protocol(
                ctran, self.next, self.result, "tuple_from_list", (items,), self.call
            )
            return True
        if name == "frozenset" and len(arguments) == 1:
            ctran, callee = function.value(self.start, node)
            following, items = Ctran(), Lvar()
            function.collect_comprehension(
                ctran,
                following,
                items,
                genexpr,
                ast.ComprehensionKind.SET,
            )
            function.full_call(
                following,
                self.next,
                self.result,
                callee,
                (items,),
                self.call,
                CallInfo((), function.converter.location(self.call)),
            )
            return True
        if name == "sorted":
            ctran, callee = function.value(self.start, node)
            iteration = None
            operands = []
            if len(arguments) > 1:
                ctran, iteration = function.prepare_generator_iteration(
                    ctran, genexpr, genexpr
                )
                for argument in arguments[1:]:
                    ctran, value = function.value(ctran, argument.value)
                    operands.append(value)
            following, items = Ctran(), Lvar()
            function.collect_comprehension(
                ctran,
                following,
                items,
                genexpr,
                ast.ComprehensionKind.LIST,
                iteration=iteration,
            )
            function.full_call(
                following,
                self.next,
                self.result,
                callee,
                (items, *operands),
                self.call,
                CallInfo(self.keywords(), function.converter.location(self.call)),
            )
            return True
        if name == "dict" and len(arguments) == 1:
            pair = genexpr.element.accept(_PlainPairElements())
            if pair is not None:
                key_expr, value_expr = pair
                function.collect_comprehension(
                    self.start,
                    self.next,
                    self.result,
                    replace(genexpr, element=value_expr),
                    ast.ComprehensionKind.DICT,
                    key=key_expr,
                )
                return True
            accumulator = function.temporary()
            key_temp = function.temporary()
            value_temp = function.temporary()
            result_name = ast.Name(accumulator.name, span=span)
            key_name = ast.Name(key_temp.name, span=span)
            value_name = ast.Name(value_temp.name, span=span)
            ctran = Ctran()
            function.drive_iterable(
                self.start,
                ctran,
                genexpr,
                lambda elem, s: [
                    ast.Assignment(
                        [ast.Tuple([key_name, value_name], span=s)], elem, span=s
                    ),
                    ast.Assignment(
                        [ast.Subscript(result_name, key_name, span=s)],
                        value_name,
                        span=s,
                    ),
                ],
                self.call,
                pre_statements=[
                    ast.Assignment([result_name], ast.Dict([], span=span), span=span)
                ],
            )
            function.ref(
                ctran, self.next, self.result, accumulator, self.call, final=True
            )
            return True
        if name in ("any", "all") and len(arguments) == 1:
            short_circuit_on_true = name == "any"
            accumulator = function.temporary()
            result_name = ast.Name(accumulator.name, span=span)

            def quantifier_consumer(
                elem: ast.Expression, s: ast.Span
            ) -> list[ast.Statement]:
                test = (
                    elem
                    if short_circuit_on_true
                    else ast.Unary(ast.UnaryOperator.NOT, elem, span=s)
                )
                return [
                    ast.If(
                        test,
                        [
                            ast.Assignment(
                                [result_name],
                                ast.BooleanLiteral(short_circuit_on_true, span=s),
                                span=s,
                            ),
                            ast.Break(span=s),
                        ],
                        span=s,
                    )
                ]

            def stop_condition(s: ast.Span) -> ast.Expression:
                return (
                    result_name
                    if short_circuit_on_true
                    else ast.Unary(ast.UnaryOperator.NOT, result_name, span=s)
                )

            ctran = Ctran()
            function.drive_iterable(
                self.start,
                ctran,
                genexpr,
                quantifier_consumer,
                self.call,
                pre_statements=[
                    ast.Assignment(
                        [result_name],
                        ast.BooleanLiteral(not short_circuit_on_true, span=span),
                        span=span,
                    )
                ],
                short_circuit_stop=stop_condition,
            )
            function.ref(
                ctran, self.next, self.result, accumulator, self.call, final=True
            )
            return True
        if name == "sum" and len(arguments) in (1, 2):
            if len(arguments) == 2:
                second = arguments[1]
                if not (
                    second.kind == ast.ArgumentKind.POSITIONAL
                    or (
                        second.kind == ast.ArgumentKind.KEYWORD
                        and second.keyword == "start"
                    )
                ) or second.value.accept(_IS_REJECTED_SUM_START):
                    return False
            accumulator = function.temporary()
            result_name = ast.Name(accumulator.name, span=span)
            current = self.start
            iteration = None
            pre_statements: list[ast.Statement] = []
            if len(arguments) == 2:
                current, iteration = function.prepare_generator_iteration(
                    current, genexpr, genexpr
                )
                ctran, init_val = function.value(current, arguments[1].value)
                current = Ctran()
                function.set_var(ctran, current, accumulator, init_val, self.call)
            else:
                pre_statements.append(
                    ast.Assignment(
                        [result_name], ast.IntegerLiteral(0, span=span), span=span
                    )
                )
            ctran = Ctran()
            function.drive_iterable(
                current,
                ctran,
                genexpr,
                lambda elem, s: [
                    ast.Assignment(
                        [result_name],
                        ast.Binary(ast.BinaryOperator.ADD, result_name, elem, span=s),
                        span=s,
                    )
                ],
                self.call,
                iteration=iteration,
                pre_statements=pre_statements,
            )
            function.ref(
                ctran, self.next, self.result, accumulator, self.call, final=True
            )
            return True
        if name in ("min", "max"):
            if any(
                arg.kind != ast.ArgumentKind.KEYWORD
                or arg.keyword not in ("key", "default")
                for arg in arguments[1:]
            ):
                return False
            has_default_arg = any(arg.keyword == "default" for arg in arguments[1:])
            if not has_default_arg and not function.unshadowed_builtin("ValueError"):
                return False
            greatest = name == "max"
            accumulator = function.temporary()
            has_best = function.temporary()
            candidate = function.temporary()
            result_name = ast.Name(accumulator.name, span=span)
            has_best_name = ast.Name(has_best.name, span=span)
            candidate_name = ast.Name(candidate.name, span=span)
            current = self.start
            iteration = None
            key_temp: LambdaVar | None = None
            if len(arguments) > 1:
                current, iteration = function.prepare_generator_iteration(
                    current, genexpr, genexpr
                )
                for arg in arguments[1:]:
                    ctran, kw_val = function.value(current, arg.value)
                    current = Ctran()
                    if arg.keyword == "key":
                        key_temp = function.temporary()
                        function.set_var(ctran, current, key_temp, kw_val, arg)
                    else:
                        function.set_var(ctran, current, accumulator, kw_val, arg)
            pre_statements = []
            if not has_default_arg:
                pre_statements.append(
                    ast.Assignment(
                        [result_name], ast.NoneLiteral(None, span=span), span=span
                    )
                )
            pre_statements.append(
                ast.Assignment(
                    [has_best_name], ast.BooleanLiteral(False, span=span), span=span
                )
            )
            op = (
                ast.ComparisonOperator.GREATER
                if greatest
                else ast.ComparisonOperator.LESS
            )
            if key_temp is None:

                def extreme_consumer(
                    elem: ast.Expression, s: ast.Span
                ) -> list[ast.Statement]:
                    return [
                        ast.Assignment([candidate_name], elem, span=s),
                        ast.If(
                            has_best_name,
                            [
                                ast.If(
                                    ast.Comparison(
                                        candidate_name, [op], [result_name], span=s
                                    ),
                                    [
                                        ast.Assignment(
                                            [result_name], candidate_name, span=s
                                        )
                                    ],
                                    span=s,
                                )
                            ],
                            [
                                ast.Assignment([result_name], candidate_name, span=s),
                                ast.Assignment(
                                    [has_best_name],
                                    ast.BooleanLiteral(True, span=s),
                                    span=s,
                                ),
                            ],
                            span=s,
                        ),
                    ]

            else:
                best_key = function.temporary()
                candidate_key = function.temporary()
                best_key_name = ast.Name(best_key.name, span=span)
                candidate_key_name = ast.Name(candidate_key.name, span=span)
                key_fn_name = ast.Name(key_temp.name, span=span)
                pre_statements.append(
                    ast.Assignment(
                        [best_key_name], ast.NoneLiteral(None, span=span), span=span
                    )
                )

                def extreme_consumer(
                    elem: ast.Expression, s: ast.Span
                ) -> list[ast.Statement]:
                    compute_key = ast.Conditional(
                        ast.Comparison(
                            key_fn_name,
                            [ast.ComparisonOperator.IS],
                            [ast.NoneLiteral(None, span=s)],
                            span=s,
                        ),
                        candidate_name,
                        ast.Call(
                            key_fn_name,
                            [ast.CallArgument(candidate_name, span=s)],
                            span=s,
                        ),
                        span=s,
                    )
                    return [
                        ast.Assignment([candidate_name], elem, span=s),
                        ast.Assignment([candidate_key_name], compute_key, span=s),
                        ast.If(
                            has_best_name,
                            [
                                ast.If(
                                    ast.Comparison(
                                        candidate_key_name,
                                        [op],
                                        [best_key_name],
                                        span=s,
                                    ),
                                    [
                                        ast.Assignment(
                                            [result_name], candidate_name, span=s
                                        ),
                                        ast.Assignment(
                                            [best_key_name],
                                            candidate_key_name,
                                            span=s,
                                        ),
                                    ],
                                    span=s,
                                )
                            ],
                            [
                                ast.Assignment([result_name], candidate_name, span=s),
                                ast.Assignment(
                                    [best_key_name], candidate_key_name, span=s
                                ),
                                ast.Assignment(
                                    [has_best_name],
                                    ast.BooleanLiteral(True, span=s),
                                    span=s,
                                ),
                            ],
                            span=s,
                        ),
                    ]

            post_statements: list[ast.Statement] = []
            if not has_default_arg:
                post_statements.append(
                    ast.If(
                        ast.Unary(ast.UnaryOperator.NOT, has_best_name, span=span),
                        [
                            ast.Raise(
                                ast.Call(
                                    ast.Name("ValueError", span=span),
                                    [
                                        ast.CallArgument(
                                            ast.TextLiteral(
                                                "min/max arg is an empty sequence",
                                                span=span,
                                            ),
                                            span=span,
                                        )
                                    ],
                                    span=span,
                                ),
                                span=span,
                            )
                        ],
                        span=span,
                    )
                )
            ctran = Ctran()
            function.drive_iterable(
                current,
                ctran,
                genexpr,
                extreme_consumer,
                self.call,
                iteration=iteration,
                pre_statements=pre_statements,
                post_statements=post_statements,
            )
            function.ref(
                ctran, self.next, self.result, accumulator, self.call, final=True
            )
            return True
        return False

    def visit_Attribute(self, node):
        function = self.function
        super_name = node.value.accept(_SuperCallee(function))
        if super_name is not None:
            cell = function.class_cell()
            ctran, callee = function.value(self.start, super_name)
            following, owner = Ctran(), Lvar()
            function.ref(ctran, following, owner, cell, node.value)
            ctran, receiver = Ctran(), Lvar()
            function.ref(
                following, ctran, receiver, function.lambda_.vars[0], node.value
            )
            following, resolved = Ctran(), Lvar()
            function.known(
                ctran,
                following,
                resolved,
                fndb.RESOLVE_SUPER_METHOD,
                (callee, owner, receiver),
                node,
                AttributeInfo(node.attribute, function.converter.location(node)),
            )
        else:
            ctran, receiver = function.value(self.start, node.value)
            following, resolved = Ctran(), Lvar()
            function.known(
                ctran,
                following,
                resolved,
                fndb.RESOLVE_METHOD,
                (receiver,),
                node,
                AttributeInfo(node.attribute, function.converter.location(node)),
            )
        function.known(
            following,
            self.next,
            self.result,
            fndb.CALL_RESOLVED,
            (resolved, *self.arguments()),
            self.call,
            CallInfo(self.keywords(), None),
        )


class _SuperCallee(ast.Visitor):
    """The ``super`` name node of a zero-argument ``super()`` receiver in a method."""

    def __init__(self, function):
        self.function = function
        self.in_call = False

    def visit_Node(self, node):
        return None

    def visit_Call(self, node: ast.Call):
        if self.in_call or node.arguments:
            return None
        self.in_call = True
        return node.function.accept(self)

    def visit_Name(self, node: ast.Name):
        if not self.in_call or node.identifier != "super":
            return None
        function = self.function
        parameters = function.lambda_.vars
        if (
            function.defining_class is None
            or not parameters
            or function.lambda_.signature.kinds[0] == ast.ParameterKind.VAR_POSITIONAL
            or function.local("super")
        ):
            return None
        return node
