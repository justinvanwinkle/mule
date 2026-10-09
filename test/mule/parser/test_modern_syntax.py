"""Explicit Mule trees for Python 3.12–3.14 syntax and lexical boundaries.

Grammar: https://docs.python.org/3.14/reference/grammar.html
Modern syntax: https://docs.python.org/3.14/whatsnew/3.14.html
Expected trees are authored by hand, independently of a host Python parser.
"""

import pytest

from mule import ast as a
from test.mule.parser.ast_query import assert_ast_equal, at, nodes, one


def name(identifier):
    return a.Name(identifier)


def subscript(identifier, *elements):
    index = elements[0] if len(elements) == 1 else a.Tuple(list(elements))
    return a.Subscript(name(identifier), index)


def call(identifier, *arguments):
    return a.Call(
        name(identifier), [a.CallArgument(argument) for argument in arguments]
    )


def assigned(value, target="result"):
    return a.Module([a.Assignment([name(target)], value)])


def alias(identifier, value, *parameters):
    return a.Module([a.TypeAlias(identifier, value, list(parameters))])


def generic_identity(*, is_async=False, default=None):
    return a.Module(
        [
            a.FunctionDefinition(
                "identity",
                [a.Parameter("value", annotation=name("T"))],
                [a.Return(name("value"))],
                return_annotation=name("T"),
                type_parameters=[a.TypeParameter("T", default=default)],
                is_async=is_async,
            )
        ]
    )


def matched(pattern, *, guard=None):
    return a.Module(
        [a.Match(name("subject"), [a.MatchCase(pattern, [a.Pass()], guard)])]
    )


def handled(*, groups=False, binding=None):
    arguments = [] if binding is None else [name(binding)]
    return a.Module(
        [
            a.Try(
                [a.ExpressionStatement(call("run"))],
                [
                    a.ExceptionHandler(
                        a.Tuple([name("ValueError"), name("TypeError")]),
                        binding,
                        [a.ExpressionStatement(call("recover", *arguments))],
                    )
                ],
                exception_groups=groups,
            )
        ]
    )


def field(
    identifier="value", *, conversion=None, format_spec=None, expression_text=None
):
    return a.Interpolation(name(identifier), conversion, format_spec, expression_text)


def formatted(*parts):
    return assigned(a.FormattedString(list(parts)))


def template(*parts):
    return assigned(a.TemplateString(list(parts)))


def precision_spec():
    return a.FormattedString(
        [field("width"), a.Literal("."), field("precision"), a.Literal("f")]
    )


MODERN_CASES = [
    (
        "type-alias",
        "type Number = int | float",
        alias("Number", a.Binary(a.BinaryOperator.BIT_OR, name("int"), name("float"))),
    ),
    (
        "type-alias-generic",
        "type Pair[T] = tuple[T, T]",
        alias("Pair", subscript("tuple", name("T"), name("T")), a.TypeParameter("T")),
    ),
    (
        "type-alias-bound",
        "type Sized[T: int] = list[T]",
        alias(
            "Sized",
            subscript("list", name("T")),
            a.TypeParameter("T", bound=name("int")),
        ),
    ),
    (
        "type-alias-constraints",
        "type Choice[T: (str, bytes)] = T",
        alias(
            "Choice",
            name("T"),
            a.TypeParameter("T", constraints=[name("str"), name("bytes")]),
        ),
    ),
    (
        "type-alias-variadic",
        "type Shape[*Ts] = tuple[*Ts]",
        alias(
            "Shape",
            subscript("tuple", a.Tuple([a.Unpack(name("Ts"))])),
            a.TypeParameter("Ts", a.TypeParameterKind.TYPE_VAR_TUPLE),
        ),
    ),
    (
        "type-alias-paramspec",
        "type Callback[**P] = Callable[P, int]",
        alias(
            "Callback",
            subscript("Callable", name("P"), name("int")),
            a.TypeParameter("P", a.TypeParameterKind.PARAM_SPEC),
        ),
    ),
    (
        "type-alias-trailing-comma",
        "type Pair[T,] = tuple[T, T]",
        alias("Pair", subscript("tuple", name("T"), name("T")), a.TypeParameter("T")),
    ),
    (
        "generic-function",
        "def identity[T](value: T) -> T:\n    return value",
        generic_identity(),
    ),
    (
        "generic-async-function",
        "async def identity[T](value: T) -> T:\n    return value",
        generic_identity(is_async=True),
    ),
    (
        "generic-class",
        "class Box[T]:\n    value: T",
        a.Module(
            [
                a.ClassDefinition(
                    "Box",
                    [a.AnnotatedAssignment(name("value"), name("T"))],
                    type_parameters=[a.TypeParameter("T")],
                )
            ]
        ),
    ),
    (
        "generic-class-bases",
        "class Box[T](Base[T], metaclass=Meta):\n    pass",
        a.Module(
            [
                a.ClassDefinition(
                    "Box",
                    [a.Pass()],
                    arguments=[
                        a.CallArgument(subscript("Base", name("T"))),
                        a.CallArgument(
                            name("Meta"), a.ArgumentKind.KEYWORD, "metaclass"
                        ),
                    ],
                    type_parameters=[a.TypeParameter("T")],
                )
            ]
        ),
    ),
    (
        "generic-function-variadic",
        "def pack[*Ts](*values: *Ts):\n    return values",
        a.Module(
            [
                a.FunctionDefinition(
                    "pack",
                    [
                        a.Parameter(
                            "values",
                            a.ParameterKind.VAR_POSITIONAL,
                            a.Unpack(name("Ts")),
                        )
                    ],
                    [a.Return(name("values"))],
                    type_parameters=[
                        a.TypeParameter("Ts", a.TypeParameterKind.TYPE_VAR_TUPLE)
                    ],
                )
            ]
        ),
    ),
    (
        "generic-function-paramspec",
        "def wrap[**P](f: Callable[P, int]):\n    return f",
        a.Module(
            [
                a.FunctionDefinition(
                    "wrap",
                    [
                        a.Parameter(
                            "f",
                            annotation=subscript("Callable", name("P"), name("int")),
                        )
                    ],
                    [a.Return(name("f"))],
                    type_parameters=[
                        a.TypeParameter("P", a.TypeParameterKind.PARAM_SPEC)
                    ],
                )
            ]
        ),
    ),
    (
        "type-param-default",
        "type Box[T = int] = list[T]",
        alias(
            "Box",
            subscript("list", name("T")),
            a.TypeParameter("T", default=name("int")),
        ),
    ),
    (
        "type-param-bound-default",
        "type Box[T: int = bool] = list[T]",
        alias(
            "Box",
            subscript("list", name("T")),
            a.TypeParameter("T", bound=name("int"), default=name("bool")),
        ),
    ),
    (
        "type-param-constraints-default",
        "type Box[T: (int, str) = int] = list[T]",
        alias(
            "Box",
            subscript("list", name("T")),
            a.TypeParameter(
                "T", constraints=[name("int"), name("str")], default=name("int")
            ),
        ),
    ),
    (
        "typevartuple-default",
        "type Shape[*Ts = *tuple[int, str]] = tuple[*Ts]",
        alias(
            "Shape",
            subscript("tuple", a.Tuple([a.Unpack(name("Ts"))])),
            a.TypeParameter(
                "Ts",
                a.TypeParameterKind.TYPE_VAR_TUPLE,
                default=a.Unpack(subscript("tuple", name("int"), name("str"))),
            ),
        ),
    ),
    (
        "paramspec-default",
        "type Callback[**P = [int, str]] = Callable[P, int]",
        alias(
            "Callback",
            subscript("Callable", name("P"), name("int")),
            a.TypeParameter(
                "P",
                a.TypeParameterKind.PARAM_SPEC,
                default=a.List([name("int"), name("str")]),
            ),
        ),
    ),
    (
        "generic-function-default",
        "def identity[T = int](value: T) -> T:\n    return value",
        generic_identity(default=name("int")),
    ),
    (
        "generic-class-default",
        "class Box[T = int]:\n    pass",
        a.Module(
            [
                a.ClassDefinition(
                    "Box",
                    [a.Pass()],
                    type_parameters=[a.TypeParameter("T", default=name("int"))],
                )
            ]
        ),
    ),
    (
        "dependent-type-default",
        "type Pair[T, U = T] = tuple[T, U]",
        alias(
            "Pair",
            subscript("tuple", name("T"), name("U")),
            a.TypeParameter("T"),
            a.TypeParameter("U", default=name("T")),
        ),
    ),
    (
        "multiline-type-params",
        "type Pair[\n    T,\n    U = T,\n] = tuple[T, U]",
        alias(
            "Pair",
            subscript("tuple", name("T"), name("U")),
            a.TypeParameter("T"),
            a.TypeParameter("U", default=name("T")),
        ),
    ),
    (
        "type-soft-keyword",
        "type = 1\ntype.attr = 2",
        a.Module(
            [
                a.Assignment([name("type")], a.Literal(1)),
                a.Assignment([a.Attribute(name("type"), "attr")], a.Literal(2)),
            ]
        ),
    ),
    (
        "match-case-soft-keywords",
        "match = 1\ncase = match + 2",
        a.Module(
            [
                a.Assignment([name("match")], a.Literal(1)),
                a.Assignment(
                    [name("case")],
                    a.Binary(a.BinaryOperator.ADD, name("match"), a.Literal(2)),
                ),
            ]
        ),
    ),
    (
        "fstring-quote-reuse",
        'result = f"{mapping["key"]}"',
        formatted(a.Interpolation(a.Subscript(name("mapping"), a.Literal("key")))),
    ),
    (
        "fstring-comment",
        'result = f"""{value  # field comment\n}"""',
        formatted(field()),
    ),
    (
        "fstring-backslash",
        'result = f"{"\\n".join(lines)}"',
        formatted(
            a.Interpolation(
                a.Call(
                    a.Attribute(a.Literal("\n"), "join"),
                    [a.CallArgument(name("lines"))],
                )
            )
        ),
    ),
    (
        "fstring-nested",
        'result = f"{f"{value}"}"',
        formatted(a.Interpolation(a.FormattedString([field()]))),
    ),
    (
        "fstring-debug",
        'result = f"{value=}"',
        formatted(a.Literal("value="), field(conversion="r")),
    ),
    (
        "fstring-debug-spaces",
        'result = f"{ value = }"',
        formatted(a.Literal(" value = "), field(conversion="r")),
    ),
    (
        "fstring-conversions",
        'result = f"{value!s} {value!r} {value!a}"',
        formatted(
            field(conversion="s"),
            a.Literal(" "),
            field(conversion="r"),
            a.Literal(" "),
            field(conversion="a"),
        ),
    ),
    (
        "fstring-format-fields",
        'result = f"{value:{width}.{precision}f}"',
        formatted(field(format_spec=precision_spec())),
    ),
    (
        "fstring-escaped-braces",
        'result = f"{{literal}} {value}"',
        formatted(a.Literal("{literal} "), field()),
    ),
    (
        "fstring-raw-prefix",
        'result = rf"\\path\\{value}"',
        formatted(a.Literal("\\path\\"), field()),
    ),
    (
        "fstring-adjacent",
        'result = "prefix" f"{value}" "suffix"',
        formatted(a.Literal("prefix"), field(), a.Literal("suffix")),
    ),
    (
        "fstring-lambda",
        'result = f"{(lambda x: x + 1)(value)}"',
        formatted(
            a.Interpolation(
                a.Call(
                    a.Lambda(
                        [a.Parameter("x")],
                        a.Binary(a.BinaryOperator.ADD, name("x"), a.Literal(1)),
                    ),
                    [a.CallArgument(name("value"))],
                )
            )
        ),
    ),
    (
        "fstring-walrus",
        'result = f"{(value := compute())}"',
        formatted(a.Interpolation(a.NamedExpression(name("value"), call("compute")))),
    ),
    (
        "fstring-comprehension",
        'result = f"{[x for x in values]}"',
        formatted(
            a.Interpolation(
                a.Comprehension(
                    a.ComprehensionKind.LIST,
                    name("x"),
                    [a.ComprehensionClause(name("x"), name("values"))],
                )
            )
        ),
    ),
    (
        "fstring-await",
        'async def render():\n    return f"{await fetch()}"',
        a.Module(
            [
                a.FunctionDefinition(
                    "render",
                    [],
                    [
                        a.Return(
                            a.FormattedString([a.Interpolation(a.Await(call("fetch")))])
                        )
                    ],
                    is_async=True,
                )
            ]
        ),
    ),
    (
        "tstring-basic",
        'result = t"hello {name}"',
        template(a.Literal("hello "), field("name", expression_text="name")),
    ),
    (
        "tstring-debug",
        'result = t"{value=}"',
        template(a.Literal("value="), field(conversion="r", expression_text="value")),
    ),
    (
        "tstring-conversion",
        'result = t"{value!r}"',
        template(field(conversion="r", expression_text="value")),
    ),
    (
        "tstring-format",
        'result = t"{value:{width}.{precision}f}"',
        template(field(format_spec=precision_spec(), expression_text="value")),
    ),
    (
        "tstring-raw",
        'result = rt"\\path\\{name}"',
        template(a.Literal("\\path\\"), field("name", expression_text="name")),
    ),
    (
        "tstring-prefix-order",
        'result = tr"{name}\\n"',
        template(field("name", expression_text="name"), a.Literal("\\n")),
    ),
    (
        "tstring-concatenation",
        'result = t"{first}" t"{last}"',
        template(
            field("first", expression_text="first"),
            field("last", expression_text="last"),
        ),
    ),
    (
        "tstring-quote-reuse",
        'result = t"{mapping["key"]}"',
        template(
            a.Interpolation(
                a.Subscript(name("mapping"), a.Literal("key")),
                expression_text='mapping["key"]',
            )
        ),
    ),
    (
        "except-unparenthesized",
        "try:\n    run()\nexcept ValueError, TypeError:\n    recover()",
        handled(),
    ),
    (
        "except-star-unparenthesized",
        "try:\n    run()\nexcept* ValueError, TypeError:\n    recover()",
        handled(groups=True),
    ),
    (
        "except-parenthesized-as",
        "try:\n    run()\nexcept (ValueError, TypeError) as error:\n    recover(error)",
        handled(binding="error"),
    ),
    (
        "match-singleton",
        "match subject:\n    case None:\n        pass\n    case True:\n        pass\n    case False:\n        pass",
        a.Module(
            [
                a.Match(
                    name("subject"),
                    [
                        a.MatchCase(a.LiteralPattern(a.Literal(value)), [a.Pass()])
                        for value in (None, True, False)
                    ],
                )
            ]
        ),
    ),
    (
        "match-signed-literal",
        "match subject:\n    case -3:\n        pass",
        matched(a.LiteralPattern(a.Unary(a.UnaryOperator.NEGATIVE, a.Literal(3)))),
    ),
    (
        "match-complex-literal",
        "match subject:\n    case 1 + 2j:\n        pass",
        matched(
            a.LiteralPattern(
                a.Binary(a.BinaryOperator.ADD, a.Literal(1), a.Literal(2j))
            )
        ),
    ),
    (
        "match-or",
        "match subject:\n    case 1 | 2 | 3:\n        pass",
        matched(
            a.OrPattern([a.LiteralPattern(a.Literal(value)) for value in (1, 2, 3)])
        ),
    ),
    (
        "match-as",
        "match subject:\n    case [first, *rest] as sequence:\n        pass",
        matched(
            a.AsPattern(
                a.SequencePattern([a.CapturePattern("first"), a.StarPattern("rest")]),
                "sequence",
            )
        ),
    ),
    (
        "match-guard",
        "match subject:\n    case value if value > 0:\n        pass",
        matched(
            a.CapturePattern("value"),
            guard=a.Comparison(
                name("value"), [a.ComparisonOperator.GREATER], [a.Literal(0)]
            ),
        ),
    ),
    (
        "match-mapping",
        'match subject:\n    case {"key": value, **rest}:\n        pass',
        matched(
            a.MappingPattern(
                [a.PatternEntry(a.Literal("key"), a.CapturePattern("value"))], "rest"
            )
        ),
    ),
    (
        "match-class",
        "match subject:\n    case Point(x, y=y):\n        pass",
        matched(
            a.ClassPattern(
                name("Point"),
                [a.CapturePattern("x")],
                [a.KeywordPattern("y", a.CapturePattern("y"))],
            )
        ),
    ),
    (
        "match-qualified-value",
        "match subject:\n    case Color.RED:\n        pass",
        matched(a.ValuePattern(a.Attribute(name("Color"), "RED"))),
    ),
    (
        "match-open-sequence",
        "match subject:\n    case first, second:\n        pass",
        matched(
            a.SequencePattern([a.CapturePattern("first"), a.CapturePattern("second")])
        ),
    ),
    (
        "match-nested-pattern",
        'match subject:\n    case {"key": [Point(x), *_]}:\n        pass',
        matched(
            a.MappingPattern(
                [
                    a.PatternEntry(
                        a.Literal("key"),
                        a.SequencePattern(
                            [
                                a.ClassPattern(name("Point"), [a.CapturePattern("x")]),
                                a.StarPattern(None),
                            ]
                        ),
                    )
                ]
            )
        ),
    ),
    (
        "match-soft-keyword-captures",
        "match subject:\n    case [match, case, type]:\n        pass",
        matched(
            a.SequencePattern(
                [
                    a.CapturePattern(identifier)
                    for identifier in ("match", "case", "type")
                ]
            )
        ),
    ),
    (
        "unicode-identifiers",
        "π = 3\n变量 = π\ncafé = 变量",
        a.Module(
            [
                a.Assignment([name("π")], a.Literal(3)),
                a.Assignment([name("变量")], name("π")),
                a.Assignment([name("café")], name("变量")),
            ]
        ),
    ),
    (
        "unicode-normalization",
        "K = 1\nresult = K",
        a.Module(
            [
                a.Assignment([name("K")], a.Literal(1)),
                a.Assignment([name("result")], name("K")),
            ]
        ),
    ),
    (
        "explicit-line-continuation",
        "result = 1 + \\\n    2",
        assigned(a.Binary(a.BinaryOperator.ADD, a.Literal(1), a.Literal(2))),
    ),
    (
        "implicit-line-continuation",
        "result = (\n    1 +  # comment\n    2\n)",
        assigned(a.Binary(a.BinaryOperator.ADD, a.Literal(1), a.Literal(2))),
    ),
    (
        "crlf",
        "if ready:\r\n    run()\r\n",
        a.Module([a.If(name("ready"), [a.ExpressionStatement(call("run"))])]),
    ),
    (
        "formfeed-indentation",
        "if ready:\n\f    run()\n",
        a.Module([a.If(name("ready"), [a.ExpressionStatement(call("run"))])]),
    ),
    (
        "string-adjacency",
        "result = 'first' \"second\" '''third'''",
        assigned(a.Literal("firstsecondthird")),
    ),
    (
        "bytes-adjacency",
        "result = b'first' br'\\second'",
        assigned(a.Literal(b"first\\second")),
    ),
    (
        "triple-quoted-string",
        "result = '''first\nsecond\nthird'''",
        assigned(a.Literal("first\nsecond\nthird")),
    ),
    (
        "raw-string",
        'result = r"C:\\folder\\file"',
        assigned(a.Literal("C:\\folder\\file")),
    ),
    (
        "encoding-cookie",
        "# coding: utf-8\nmessage = 'café'",
        assigned(a.Literal("café"), "message"),
    ),
    (
        "comment-with-no-final-newline",
        "value = 1 # final comment",
        assigned(a.Literal(1), "value"),
    ),
    (
        "semicolon-newline",
        "type Pair[T] = tuple[T, T]; result = 1\n",
        a.Module(
            [
                a.TypeAlias(
                    "Pair",
                    subscript("tuple", name("T"), name("T")),
                    [a.TypeParameter("T")],
                ),
                a.Assignment([name("result")], a.Literal(1)),
            ]
        ),
    ),
]


@pytest.mark.parametrize(
    "case,source,expected", MODERN_CASES, ids=[case for case, _, _ in MODERN_CASES]
)
def test_modern_syntax_builds_mule_tree(parse, case, source, expected):
    assert_ast_equal(parse(source, filename="modern.py"), expected, locations=False)


def test_alias_name_is_a_declaration_not_an_expression(parse):
    declaration = one(parse("type Pair[T] = tuple[T, T]"), a.TypeAlias)
    assert declaration.name == "Pair"
    assert [parameter.name for parameter in declaration.type_parameters] == ["T"]
    assert not nodes(declaration, a.Name, identifier="Pair")


def test_generic_bound_and_default_have_distinct_fields(parse):
    parameter = one(parse("type Box[T: int = bool] = list[T]"), a.TypeParameter)
    assert parameter.name == "T"
    assert parameter.bound.identifier == "int"
    assert parameter.default.identifier == "bool"


def test_variadic_type_parameters_keep_their_kinds(parse):
    parameters = nodes(
        parse("type CallableShape[*Ts, **P] = tuple[*Ts]"), a.TypeParameter
    )
    assert [(parameter.name, parameter.kind) for parameter in parameters] == [
        ("Ts", a.TypeParameterKind.TYPE_VAR_TUPLE),
        ("P", a.TypeParameterKind.PARAM_SPEC),
    ]


def test_generic_function_type_parameters_are_separate_from_arguments(parse):
    function = one(
        parse("def identity[T](value: T) -> T:\n    return value"), a.FunctionDefinition
    )
    assert [parameter.name for parameter in function.type_parameters] == ["T"]
    assert [parameter.name for parameter in function.parameters] == ["value"]
    assert function.parameters[0].annotation.identifier == "T"
    assert function.return_annotation.identifier == "T"


def test_fstring_debug_conversion_defaults_to_repr(parse):
    tree = parse('f"{ value = }"')
    interpolation = one(tree, a.Interpolation)
    assert interpolation.conversion == "r"
    assert interpolation.expression.identifier == "value"
    assert one(tree, a.Literal).value == " value = "


def test_fstring_dynamic_format_spec_contains_nested_fields(parse):
    tree = parse('f"{value:{width}.{precision}f}"')
    assert [part.expression.identifier for part in nodes(tree, a.Interpolation)] == [
        "value",
        "width",
        "precision",
    ]
    assert isinstance(
        at(tree, "statements.0.expression.parts.0.format_spec"), a.FormattedString
    )


def test_template_string_preserves_interpolation_expression_text(parse):
    tree = parse('t"hello { name.upper() !r}"')
    assert isinstance(at(tree, "statements.0.expression"), a.TemplateString)
    interpolation = one(tree, a.Interpolation)
    assert interpolation.expression_text == " name.upper() "
    assert interpolation.conversion == "r"
    assert isinstance(interpolation.expression, a.Call)


def test_unparenthesized_exception_types_become_tuple(parse):
    handler = one(
        parse("try:\n    run()\nexcept ValueError, TypeError:\n    pass"),
        a.ExceptionHandler,
    )
    assert isinstance(handler.exception, a.Tuple)
    assert [value.identifier for value in handler.exception.elements] == [
        "ValueError",
        "TypeError",
    ]
    assert handler.binding is None


def test_mapping_pattern_keeps_rest_capture_outside_entries(parse):
    pattern = one(
        parse('match value:\n    case {"key": item, **rest}:\n        pass'),
        a.MappingPattern,
    )
    assert [entry.key.value for entry in pattern.entries] == ["key"]
    assert [entry.pattern.name for entry in pattern.entries] == ["item"]
    assert pattern.rest == "rest"


def test_unicode_names_normalize_and_use_character_locations(parse):
    tree = parse("K = π\n")
    target = one(tree, a.Name, identifier="K")
    assert target.span == a.Span(a.Position(1, 0), a.Position(1, 1))
    value = one(tree, a.Name, identifier="π")
    assert value.span == a.Span(a.Position(1, 4), a.Position(1, 5))
