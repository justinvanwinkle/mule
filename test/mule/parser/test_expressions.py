"""Handwritten Mule expression trees and Pratt binding-power contracts."""

import pytest

from mule import ast
from mule.ast import (
    ArgumentKind,
    Attribute,
    Await,
    Binary,
    BinaryOperator,
    Boolean,
    BooleanOperator,
    Call,
    CallArgument,
    Comparison,
    ComparisonOperator,
    Comprehension,
    ComprehensionClause,
    ComprehensionKind,
    Conditional,
    Dict,
    DictEntry,
    FormattedString,
    Interpolation,
    Lambda,
    List,
    Name,
    NamedExpression,
    Parameter,
    ParameterKind,
    Set,
    Slice,
    Subscript,
    Tuple,
    Unary,
    UnaryOperator,
    Unpack,
    Yield,
)
from test.mule.parser import ast_query as q
from test.mule.parser.ast_query import assert_ast_equal, at, expect, nodes, one


def case(name, source, expected):
    return pytest.param(source, expected, id=name)


@pytest.mark.parametrize(
    "source, expected",
    [
        case("none", "None", ast.NoneLiteral(None)),
        case("true", "True", ast.BooleanLiteral(True)),
        case("false", "False", ast.BooleanLiteral(False)),
        case("ellipsis", "...", ast.EllipsisLiteral(Ellipsis)),
        case("decimal-zero", "0", ast.IntegerLiteral(0)),
        case("decimal-underscores", "1_000_000", ast.IntegerLiteral(1000000)),
        case("binary", "0b1010_0101", ast.IntegerLiteral(165)),
        case("octal", "0o7_55", ast.IntegerLiteral(493)),
        case("hexadecimal", "0xCA_FE", ast.IntegerLiteral(51966)),
        case("float-fraction", "12.34", ast.FloatLiteral(12.34)),
        case("float-leading-dot", ".5", ast.FloatLiteral(0.5)),
        case("float-trailing-dot", "5.", ast.FloatLiteral(5.0)),
        case("float-exponent", "1e-9", ast.FloatLiteral(1e-9)),
        case("float-exponent-underscores", "1_2.3_4e+5_6", ast.FloatLiteral(12.34e56)),
        case("imaginary-integer", "3j", ast.ComplexLiteral(3j)),
        case("imaginary-float", ".5J", ast.ComplexLiteral(0.5j)),
        case("single-quoted-string", "'hello'", ast.TextLiteral("hello")),
        case("double-quoted-string", '"hello"', ast.TextLiteral("hello")),
        case("escaped-string", r"'a\n\t\x41\u03b1'", ast.TextLiteral("a\n\tA\u03b1")),
        case("raw-string", r"r'\d+\s*'", ast.TextLiteral(r"\d+\s*")),
        case("unicode-prefix", "u'hello'", ast.TextLiteral("hello")),
        case("bytes", "b'hello'", ast.BytesLiteral(b"hello")),
        case("raw-bytes", r"rb'\x41'", ast.BytesLiteral(rb"\x41")),
        case("triple-string", "'''first\nsecond'''", ast.TextLiteral("first\nsecond")),
        case("adjacent-strings", "'one' \"two\"", ast.TextLiteral("onetwo")),
        case("adjacent-bytes", "b'one' b'two'", ast.BytesLiteral(b"onetwo")),
        case("ascii-name", "identifier", Name("identifier")),
        case("unicode-name", "\u03c0", Name("\u03c0")),
        case("normalized-unicode-name", "\u212a", Name("K")),
        case("soft-keyword-name", "match", Name("match")),
    ],
)
def test_literals_and_names(parse, source, expected):
    assert_ast_equal(parse(source, mode="eval"), ast.ExpressionInput(expected))


@pytest.mark.parametrize(
    "source, expected",
    [
        case("addition", "a + b", Binary(BinaryOperator.ADD, Name("a"), Name("b"))),
        case(
            "subtraction",
            "a - b",
            Binary(BinaryOperator.SUBTRACT, Name("a"), Name("b")),
        ),
        case(
            "multiplication",
            "a * b",
            Binary(BinaryOperator.MULTIPLY, Name("a"), Name("b")),
        ),
        case(
            "true-division",
            "a / b",
            Binary(BinaryOperator.DIVIDE, Name("a"), Name("b")),
        ),
        case(
            "floor-division",
            "a // b",
            Binary(BinaryOperator.FLOOR_DIVIDE, Name("a"), Name("b")),
        ),
        case("modulo", "a % b", Binary(BinaryOperator.MODULO, Name("a"), Name("b"))),
        case(
            "matrix-multiply",
            "a @ b",
            Binary(BinaryOperator.MATRIX_MULTIPLY, Name("a"), Name("b")),
        ),
        case("power", "a ** b", Binary(BinaryOperator.POWER, Name("a"), Name("b"))),
        case(
            "left-shift",
            "a << b",
            Binary(BinaryOperator.LEFT_SHIFT, Name("a"), Name("b")),
        ),
        case(
            "right-shift",
            "a >> b",
            Binary(BinaryOperator.RIGHT_SHIFT, Name("a"), Name("b")),
        ),
        case(
            "bitwise-and", "a & b", Binary(BinaryOperator.BIT_AND, Name("a"), Name("b"))
        ),
        case(
            "bitwise-xor", "a ^ b", Binary(BinaryOperator.BIT_XOR, Name("a"), Name("b"))
        ),
        case(
            "bitwise-or", "a | b", Binary(BinaryOperator.BIT_OR, Name("a"), Name("b"))
        ),
        case("unary-positive", "+a", Unary(UnaryOperator.POSITIVE, Name("a"))),
        case("unary-negative", "-a", Unary(UnaryOperator.NEGATIVE, Name("a"))),
        case("unary-invert", "~a", Unary(UnaryOperator.INVERT, Name("a"))),
        case("logical-not", "not a", Unary(UnaryOperator.NOT, Name("a"))),
        case(
            "nested-unary",
            "-~+a",
            Unary(
                UnaryOperator.NEGATIVE,
                Unary(UnaryOperator.INVERT, Unary(UnaryOperator.POSITIVE, Name("a"))),
            ),
        ),
        case(
            "logical-and",
            "a and b",
            Boolean(BooleanOperator.AND, [Name("a"), Name("b")]),
        ),
        case(
            "logical-or", "a or b", Boolean(BooleanOperator.OR, [Name("a"), Name("b")])
        ),
        case(
            "equal",
            "a == b",
            Comparison(Name("a"), [ComparisonOperator.EQUAL], [Name("b")]),
        ),
        case(
            "not-equal",
            "a != b",
            Comparison(Name("a"), [ComparisonOperator.NOT_EQUAL], [Name("b")]),
        ),
        case(
            "less-than",
            "a < b",
            Comparison(Name("a"), [ComparisonOperator.LESS], [Name("b")]),
        ),
        case(
            "less-equal",
            "a <= b",
            Comparison(Name("a"), [ComparisonOperator.LESS_EQUAL], [Name("b")]),
        ),
        case(
            "greater-than",
            "a > b",
            Comparison(Name("a"), [ComparisonOperator.GREATER], [Name("b")]),
        ),
        case(
            "greater-equal",
            "a >= b",
            Comparison(Name("a"), [ComparisonOperator.GREATER_EQUAL], [Name("b")]),
        ),
        case(
            "identity",
            "a is b",
            Comparison(Name("a"), [ComparisonOperator.IS], [Name("b")]),
        ),
        case(
            "negative-identity",
            "a is not b",
            Comparison(Name("a"), [ComparisonOperator.IS_NOT], [Name("b")]),
        ),
        case(
            "membership",
            "a in b",
            Comparison(Name("a"), [ComparisonOperator.IN], [Name("b")]),
        ),
        case(
            "negative-membership",
            "a not in b",
            Comparison(Name("a"), [ComparisonOperator.NOT_IN], [Name("b")]),
        ),
        case(
            "arithmetic-precedence",
            "a + b * c ** d - e / f",
            Binary(
                BinaryOperator.SUBTRACT,
                Binary(
                    BinaryOperator.ADD,
                    Name("a"),
                    Binary(
                        BinaryOperator.MULTIPLY,
                        Name("b"),
                        Binary(BinaryOperator.POWER, Name("c"), Name("d")),
                    ),
                ),
                Binary(BinaryOperator.DIVIDE, Name("e"), Name("f")),
            ),
        ),
        case(
            "multiplicative-associativity",
            "a * b / c // d % e @ f",
            Binary(
                BinaryOperator.MATRIX_MULTIPLY,
                Binary(
                    BinaryOperator.MODULO,
                    Binary(
                        BinaryOperator.FLOOR_DIVIDE,
                        Binary(
                            BinaryOperator.DIVIDE,
                            Binary(BinaryOperator.MULTIPLY, Name("a"), Name("b")),
                            Name("c"),
                        ),
                        Name("d"),
                    ),
                    Name("e"),
                ),
                Name("f"),
            ),
        ),
        case(
            "power-unary-right",
            "a ** -b",
            Binary(
                BinaryOperator.POWER,
                Name("a"),
                Unary(UnaryOperator.NEGATIVE, Name("b")),
            ),
        ),
        case(
            "negative-power-base",
            "-a ** b",
            Unary(
                UnaryOperator.NEGATIVE,
                Binary(BinaryOperator.POWER, Name("a"), Name("b")),
            ),
        ),
        case(
            "parenthesized-negative-base",
            "(-a) ** b",
            Binary(
                BinaryOperator.POWER,
                Unary(UnaryOperator.NEGATIVE, Name("a")),
                Name("b"),
            ),
        ),
        case(
            "shift-addition-precedence",
            "a + b << c - d",
            Binary(
                BinaryOperator.LEFT_SHIFT,
                Binary(BinaryOperator.ADD, Name("a"), Name("b")),
                Binary(BinaryOperator.SUBTRACT, Name("c"), Name("d")),
            ),
        ),
        case(
            "bitwise-hierarchy",
            "a | b ^ c & d",
            Binary(
                BinaryOperator.BIT_OR,
                Name("a"),
                Binary(
                    BinaryOperator.BIT_XOR,
                    Name("b"),
                    Binary(BinaryOperator.BIT_AND, Name("c"), Name("d")),
                ),
            ),
        ),
        case(
            "boolean-hierarchy",
            "not a and b or c",
            Boolean(
                BooleanOperator.OR,
                [
                    Boolean(
                        BooleanOperator.AND,
                        [Unary(UnaryOperator.NOT, Name("a")), Name("b")],
                    ),
                    Name("c"),
                ],
            ),
        ),
        case(
            "comparison-chain",
            "a < b <= c != d is not e not in f",
            Comparison(
                Name("a"),
                [
                    ComparisonOperator.LESS,
                    ComparisonOperator.LESS_EQUAL,
                    ComparisonOperator.NOT_EQUAL,
                    ComparisonOperator.IS_NOT,
                    ComparisonOperator.NOT_IN,
                ],
                [Name("b"), Name("c"), Name("d"), Name("e"), Name("f")],
            ),
        ),
        case(
            "comparison-bitwise-operands",
            "a | b == c & d",
            Comparison(
                Binary(BinaryOperator.BIT_OR, Name("a"), Name("b")),
                [ComparisonOperator.EQUAL],
                [Binary(BinaryOperator.BIT_AND, Name("c"), Name("d"))],
            ),
        ),
    ],
)
def test_operators(parse, source, expected):
    assert_ast_equal(parse(source, mode="eval"), ast.ExpressionInput(expected))


@pytest.mark.parametrize(
    "source, expected",
    [
        case("empty-tuple", "()", Tuple()),
        case("singleton-tuple", "(a,)", Tuple([Name("a")])),
        case("unparenthesized-tuple", "a, b", Tuple([Name("a"), Name("b")])),
        case("tuple-trailing-comma", "a, b,", Tuple([Name("a"), Name("b")])),
        case("parenthesized-expression", "(a)", Name("a")),
        case(
            "nested-parentheses",
            "(((a + b)))",
            Binary(BinaryOperator.ADD, Name("a"), Name("b")),
        ),
        case("starred-tuple", "(*a, b)", Tuple([Unpack(Name("a")), Name("b")])),
        case(
            "nested-starred-tuple",
            "((*a, b), c)",
            Tuple([Tuple([Unpack(Name("a")), Name("b")]), Name("c")]),
        ),
        case(
            "multiple-tuple-unpacks",
            "(*a, *b, c)",
            Tuple([Unpack(Name("a")), Unpack(Name("b")), Name("c")]),
        ),
        case("empty-list", "[]", List()),
        case("list-items", "[a, b, c]", List([Name("a"), Name("b"), Name("c")])),
        case("list-trailing-comma", "[a,]", List([Name("a")])),
        case(
            "list-unpack", "[a, *b, c]", List([Name("a"), Unpack(Name("b")), Name("c")])
        ),
        case(
            "nested-lists",
            "[[a], [b, c]]",
            List([List([Name("a")]), List([Name("b"), Name("c")])]),
        ),
        case("empty-dict", "{}", Dict()),
        case("dict-item", "{a: b}", Dict([DictEntry(Name("a"), Name("b"))])),
        case("dict-trailing-comma", "{a: b,}", Dict([DictEntry(Name("a"), Name("b"))])),
        case(
            "dict-multiple-items",
            "{a: b, c: d}",
            Dict([DictEntry(Name("a"), Name("b")), DictEntry(Name("c"), Name("d"))]),
        ),
        case("dict-unpack", "{**a}", Dict([DictEntry(None, Name("a"))])),
        case(
            "dict-mixed-unpacks",
            "{a: b, **c, d: e, **f}",
            Dict(
                [
                    DictEntry(Name("a"), Name("b")),
                    DictEntry(None, Name("c")),
                    DictEntry(Name("d"), Name("e")),
                    DictEntry(None, Name("f")),
                ]
            ),
        ),
        case("set-singleton", "{a}", Set([Name("a")])),
        case("set-items", "{a, b, c}", Set([Name("a"), Name("b"), Name("c")])),
        case("set-trailing-comma", "{a,}", Set([Name("a")])),
        case(
            "set-unpack",
            "{*a, b, *c}",
            Set([Unpack(Name("a")), Name("b"), Unpack(Name("c"))]),
        ),
        case(
            "nested-displays",
            "{'x': ([a, *b], {c, d}), **e}",
            Dict(
                [
                    DictEntry(
                        ast.TextLiteral("x"),
                        Tuple(
                            [
                                List([Name("a"), Unpack(Name("b"))]),
                                Set([Name("c"), Name("d")]),
                            ]
                        ),
                    ),
                    DictEntry(None, Name("e")),
                ]
            ),
        ),
    ],
)
def test_container_displays(parse, source, expected):
    assert_ast_equal(parse(source, mode="eval"), ast.ExpressionInput(expected))


@pytest.mark.parametrize(
    "source, expected",
    [
        case("attribute", "a.b", Attribute(Name("a"), "b")),
        case("attribute-chain", "a.b.c", Attribute(Attribute(Name("a"), "b"), "c")),
        case("empty-call", "f()", Call(Name("f"))),
        case(
            "positional-call",
            "f(a, b)",
            Call(Name("f"), [CallArgument(Name("a")), CallArgument(Name("b"))]),
        ),
        case(
            "call-trailing-comma", "f(a,)", Call(Name("f"), [CallArgument(Name("a"))])
        ),
        case(
            "keyword-call",
            "f(a=1, b=2)",
            Call(
                Name("f"),
                [
                    CallArgument(ast.IntegerLiteral(1), ArgumentKind.KEYWORD, "a"),
                    CallArgument(ast.IntegerLiteral(2), ArgumentKind.KEYWORD, "b"),
                ],
            ),
        ),
        case(
            "starred-call",
            "f(*a)",
            Call(Name("f"), [CallArgument(Name("a"), ArgumentKind.STARRED)]),
        ),
        case(
            "double-starred-call",
            "f(**a)",
            Call(Name("f"), [CallArgument(Name("a"), ArgumentKind.DOUBLE_STARRED)]),
        ),
        case(
            "mixed-call",
            "f(a, *b, c=d, **e)",
            Call(
                Name("f"),
                [
                    CallArgument(Name("a")),
                    CallArgument(Name("b"), ArgumentKind.STARRED),
                    CallArgument(Name("d"), ArgumentKind.KEYWORD, "c"),
                    CallArgument(Name("e"), ArgumentKind.DOUBLE_STARRED),
                ],
            ),
        ),
        case(
            "multiple-call-unpacks",
            "f(*a, *b, **c, **d)",
            Call(
                Name("f"),
                [
                    CallArgument(Name("a"), ArgumentKind.STARRED),
                    CallArgument(Name("b"), ArgumentKind.STARRED),
                    CallArgument(Name("c"), ArgumentKind.DOUBLE_STARRED),
                    CallArgument(Name("d"), ArgumentKind.DOUBLE_STARRED),
                ],
            ),
        ),
        case(
            "star-after-keyword",
            "f(a=1, *b)",
            Call(
                Name("f"),
                [
                    CallArgument(ast.IntegerLiteral(1), ArgumentKind.KEYWORD, "a"),
                    CallArgument(Name("b"), ArgumentKind.STARRED),
                ],
            ),
        ),
        case(
            "keyword-after-double-star",
            "f(**a, b=2)",
            Call(
                Name("f"),
                [
                    CallArgument(Name("a"), ArgumentKind.DOUBLE_STARRED),
                    CallArgument(ast.IntegerLiteral(2), ArgumentKind.KEYWORD, "b"),
                ],
            ),
        ),
        case(
            "generator-call",
            "f(x for x in items)",
            Call(
                Name("f"),
                [
                    CallArgument(
                        Comprehension(
                            ComprehensionKind.GENERATOR,
                            Name("x"),
                            [ComprehensionClause(Name("x"), Name("items"))],
                        )
                    )
                ],
            ),
        ),
        case("index", "a[b]", Subscript(Name("a"), Name("b"))),
        case(
            "tuple-index",
            "a[b, c]",
            Subscript(Name("a"), Tuple([Name("b"), Name("c")])),
        ),
        case(
            "singleton-tuple-index", "a[b,]", Subscript(Name("a"), Tuple([Name("b")]))
        ),
        case("full-slice", "a[:]", Subscript(Name("a"), Slice())),
        case("start-slice", "a[b:]", Subscript(Name("a"), Slice(Name("b")))),
        case("stop-slice", "a[:b]", Subscript(Name("a"), Slice(upper=Name("b")))),
        case(
            "bounded-slice", "a[b:c]", Subscript(Name("a"), Slice(Name("b"), Name("c")))
        ),
        case(
            "stepped-slice",
            "a[b:c:d]",
            Subscript(Name("a"), Slice(Name("b"), Name("c"), Name("d"))),
        ),
        case(
            "negative-step",
            "a[::-1]",
            Subscript(
                Name("a"),
                Slice(step=Unary(UnaryOperator.NEGATIVE, ast.IntegerLiteral(1))),
            ),
        ),
        case(
            "mixed-dimensional-slice",
            "a[b:c, d, ::e]",
            Subscript(
                Name("a"),
                Tuple([Slice(Name("b"), Name("c")), Name("d"), Slice(step=Name("e"))]),
            ),
        ),
        case(
            "starred-index",
            "a[*b, c]",
            Subscript(Name("a"), Tuple([Unpack(Name("b")), Name("c")])),
        ),
        case(
            "mixed-trailers",
            "a.b(c)[d:e].f(*g)",
            Call(
                Attribute(
                    Subscript(
                        Call(Attribute(Name("a"), "b"), [CallArgument(Name("c"))]),
                        Slice(Name("d"), Name("e")),
                    ),
                    "f",
                ),
                [CallArgument(Name("g"), ArgumentKind.STARRED)],
            ),
        ),
    ],
)
def test_primary_trailers(parse, source, expected):
    assert_ast_equal(parse(source, mode="eval"), ast.ExpressionInput(expected))


@pytest.mark.parametrize(
    "source, expected",
    [
        case(
            "conditional", "a if b else c", Conditional(Name("b"), Name("a"), Name("c"))
        ),
        case(
            "nested-conditional-else",
            "a if b else c if d else e",
            Conditional(
                Name("b"), Name("a"), Conditional(Name("d"), Name("c"), Name("e"))
            ),
        ),
        case(
            "nested-conditional-body",
            "(a if b else c) if d else e",
            Conditional(
                Name("d"), Conditional(Name("b"), Name("a"), Name("c")), Name("e")
            ),
        ),
        case(
            "conditional-boolean-parts",
            "a or b if c and d else e or f",
            Conditional(
                Boolean(BooleanOperator.AND, [Name("c"), Name("d")]),
                Boolean(BooleanOperator.OR, [Name("a"), Name("b")]),
                Boolean(BooleanOperator.OR, [Name("e"), Name("f")]),
            ),
        ),
        case("lambda-empty-arguments", "lambda: a", Lambda([], Name("a"))),
        case(
            "lambda-positional",
            "lambda a, b: a + b",
            Lambda(
                [Parameter("a"), Parameter("b")],
                Binary(BinaryOperator.ADD, Name("a"), Name("b")),
            ),
        ),
        case(
            "lambda-defaults",
            "lambda a=1, b=2: a + b",
            Lambda(
                [
                    Parameter("a", default=ast.IntegerLiteral(1)),
                    Parameter("b", default=ast.IntegerLiteral(2)),
                ],
                Binary(BinaryOperator.ADD, Name("a"), Name("b")),
            ),
        ),
        case(
            "lambda-positional-only",
            "lambda a, /, b: a + b",
            Lambda(
                [Parameter("a", ParameterKind.POSITIONAL_ONLY), Parameter("b")],
                Binary(BinaryOperator.ADD, Name("a"), Name("b")),
            ),
        ),
        case(
            "lambda-keyword-only",
            "lambda *, a, b=2: a + b",
            Lambda(
                [
                    Parameter("a", ParameterKind.KEYWORD_ONLY),
                    Parameter(
                        "b", ParameterKind.KEYWORD_ONLY, default=ast.IntegerLiteral(2)
                    ),
                ],
                Binary(BinaryOperator.ADD, Name("a"), Name("b")),
            ),
        ),
        case(
            "lambda-variadic",
            "lambda *a, **b: (a, b)",
            Lambda(
                [
                    Parameter("a", ParameterKind.VAR_POSITIONAL),
                    Parameter("b", ParameterKind.VAR_KEYWORD),
                ],
                Tuple([Name("a"), Name("b")]),
            ),
        ),
        case(
            "lambda-full-arguments",
            "lambda a, /, b=2, *c, d, e=3, **f: a",
            Lambda(
                [
                    Parameter("a", ParameterKind.POSITIONAL_ONLY),
                    Parameter("b", default=ast.IntegerLiteral(2)),
                    Parameter("c", ParameterKind.VAR_POSITIONAL),
                    Parameter("d", ParameterKind.KEYWORD_ONLY),
                    Parameter(
                        "e", ParameterKind.KEYWORD_ONLY, default=ast.IntegerLiteral(3)
                    ),
                    Parameter("f", ParameterKind.VAR_KEYWORD),
                ],
                Name("a"),
            ),
        ),
        case(
            "lambda-conditional-body",
            "lambda: a if b else c",
            Lambda([], Conditional(Name("b"), Name("a"), Name("c"))),
        ),
        case(
            "conditional-lambda-else",
            "a if b else lambda: c",
            Conditional(Name("b"), Name("a"), Lambda([], Name("c"))),
        ),
        case(
            "immediate-lambda-call",
            "(lambda x: x + 1)(2)",
            Call(
                Lambda(
                    [Parameter("x")],
                    Binary(BinaryOperator.ADD, Name("x"), ast.IntegerLiteral(1)),
                ),
                [CallArgument(ast.IntegerLiteral(2))],
            ),
        ),
        case("named-expression", "(x := y)", NamedExpression(Name("x"), Name("y"))),
        case(
            "named-expression-comparison",
            "(x := y < z)",
            NamedExpression(
                Name("x"), Comparison(Name("y"), [ComparisonOperator.LESS], [Name("z")])
            ),
        ),
        case(
            "named-expression-call-argument",
            "f(x := g())",
            Call(
                Name("f"), [CallArgument(NamedExpression(Name("x"), Call(Name("g"))))]
            ),
        ),
        case("yield-expression", "(yield a, b)", Yield(Tuple([Name("a"), Name("b")]))),
        case(
            "yield-from-expression",
            "(yield from items)",
            Yield(Name("items"), from_iterable=True),
        ),
        case(
            "await-expression",
            "await f() + a",
            Binary(BinaryOperator.ADD, Await(Call(Name("f"))), Name("a")),
        ),
    ],
)
def test_low_binding_and_suspension_expressions(parse, source, expected):
    assert_ast_equal(parse(source, mode="eval"), ast.ExpressionInput(expected))


@pytest.mark.parametrize(
    "source, expected",
    [
        case(
            "list-comprehension",
            "[x for x in items]",
            Comprehension(
                ComprehensionKind.LIST,
                Name("x"),
                [ComprehensionClause(Name("x"), Name("items"))],
            ),
        ),
        case(
            "set-comprehension",
            "{x for x in items}",
            Comprehension(
                ComprehensionKind.SET,
                Name("x"),
                [ComprehensionClause(Name("x"), Name("items"))],
            ),
        ),
        case(
            "dict-comprehension",
            "{x: f(x) for x in items}",
            Comprehension(
                ComprehensionKind.DICT,
                Call(Name("f"), [CallArgument(Name("x"))]),
                [ComprehensionClause(Name("x"), Name("items"))],
                key=Name("x"),
            ),
        ),
        case(
            "generator-expression",
            "(x for x in items)",
            Comprehension(
                ComprehensionKind.GENERATOR,
                Name("x"),
                [ComprehensionClause(Name("x"), Name("items"))],
            ),
        ),
        case(
            "filtered-comprehension",
            "[x for x in items if x]",
            Comprehension(
                ComprehensionKind.LIST,
                Name("x"),
                [ComprehensionClause(Name("x"), Name("items"), [Name("x")])],
            ),
        ),
        case(
            "multiple-filters",
            "[x for x in items if x > 0 if x < 10]",
            Comprehension(
                ComprehensionKind.LIST,
                Name("x"),
                [
                    ComprehensionClause(
                        Name("x"),
                        Name("items"),
                        [
                            Comparison(
                                Name("x"),
                                [ComparisonOperator.GREATER],
                                [ast.IntegerLiteral(0)],
                            ),
                            Comparison(
                                Name("x"),
                                [ComparisonOperator.LESS],
                                [ast.IntegerLiteral(10)],
                            ),
                        ],
                    )
                ],
            ),
        ),
        case(
            "nested-for",
            "[(x, y) for x in xs for y in ys]",
            Comprehension(
                ComprehensionKind.LIST,
                Tuple([Name("x"), Name("y")]),
                [
                    ComprehensionClause(Name("x"), Name("xs")),
                    ComprehensionClause(Name("y"), Name("ys")),
                ],
            ),
        ),
        case(
            "filter-between-for-clauses",
            "[(x, y) for x in xs if x for y in ys if y]",
            Comprehension(
                ComprehensionKind.LIST,
                Tuple([Name("x"), Name("y")]),
                [
                    ComprehensionClause(Name("x"), Name("xs"), [Name("x")]),
                    ComprehensionClause(Name("y"), Name("ys"), [Name("y")]),
                ],
            ),
        ),
        case(
            "tuple-target",
            "[a + b for a, b in pairs]",
            Comprehension(
                ComprehensionKind.LIST,
                Binary(BinaryOperator.ADD, Name("a"), Name("b")),
                [ComprehensionClause(Tuple([Name("a"), Name("b")]), Name("pairs"))],
            ),
        ),
        case(
            "list-target",
            "[a + b for [a, b] in pairs]",
            Comprehension(
                ComprehensionKind.LIST,
                Binary(BinaryOperator.ADD, Name("a"), Name("b")),
                [ComprehensionClause(List([Name("a"), Name("b")]), Name("pairs"))],
            ),
        ),
        case(
            "starred-target",
            "[tail for head, *tail in rows]",
            Comprehension(
                ComprehensionKind.LIST,
                Name("tail"),
                [
                    ComprehensionClause(
                        Tuple([Name("head"), Unpack(Name("tail"))]), Name("rows")
                    )
                ],
            ),
        ),
        case(
            "parenthesized-target",
            "[a for (a,) in rows]",
            Comprehension(
                ComprehensionKind.LIST,
                Name("a"),
                [ComprehensionClause(Tuple([Name("a")]), Name("rows"))],
            ),
        ),
        case(
            "conditional-element",
            "[a if flag else b for a, b in pairs]",
            Comprehension(
                ComprehensionKind.LIST,
                Conditional(Name("flag"), Name("a"), Name("b")),
                [ComprehensionClause(Tuple([Name("a"), Name("b")]), Name("pairs"))],
            ),
        ),
        case(
            "conditional-iterable",
            "[x for x in (a if flag else b)]",
            Comprehension(
                ComprehensionKind.LIST,
                Name("x"),
                [
                    ComprehensionClause(
                        Name("x"), Conditional(Name("flag"), Name("a"), Name("b"))
                    )
                ],
            ),
        ),
        case(
            "nested-comprehension",
            "[[y for y in x] for x in rows]",
            Comprehension(
                ComprehensionKind.LIST,
                Comprehension(
                    ComprehensionKind.LIST,
                    Name("y"),
                    [ComprehensionClause(Name("y"), Name("x"))],
                ),
                [ComprehensionClause(Name("x"), Name("rows"))],
            ),
        ),
        case(
            "named-expression-element",
            "[(y := f(x)) for x in items]",
            Comprehension(
                ComprehensionKind.LIST,
                NamedExpression(Name("y"), Call(Name("f"), [CallArgument(Name("x"))])),
                [ComprehensionClause(Name("x"), Name("items"))],
            ),
        ),
        case(
            "named-expression-filter",
            "[y for x in items if (y := f(x))]",
            Comprehension(
                ComprehensionKind.LIST,
                Name("y"),
                [
                    ComprehensionClause(
                        Name("x"),
                        Name("items"),
                        [
                            NamedExpression(
                                Name("y"), Call(Name("f"), [CallArgument(Name("x"))])
                            )
                        ],
                    )
                ],
            ),
        ),
        case(
            "async-comprehension",
            "[x async for x in items]",
            Comprehension(
                ComprehensionKind.LIST,
                Name("x"),
                [ComprehensionClause(Name("x"), Name("items"), is_async=True)],
            ),
        ),
        case(
            "mixed-async-generators",
            "[(x, y) async for x in xs for y in x]",
            Comprehension(
                ComprehensionKind.LIST,
                Tuple([Name("x"), Name("y")]),
                [
                    ComprehensionClause(Name("x"), Name("xs"), is_async=True),
                    ComprehensionClause(Name("y"), Name("x")),
                ],
            ),
        ),
        case(
            "await-comprehension-element",
            "[await f(x) async for x in items if x]",
            Comprehension(
                ComprehensionKind.LIST,
                Await(Call(Name("f"), [CallArgument(Name("x"))])),
                [
                    ComprehensionClause(
                        Name("x"), Name("items"), [Name("x")], is_async=True
                    )
                ],
            ),
        ),
    ],
)
def test_comprehensions(parse, source, expected):
    assert_ast_equal(parse(source, mode="eval"), ast.ExpressionInput(expected))


@pytest.mark.parametrize(
    "source, expected",
    [
        case("empty-f-string", "f''", FormattedString()),
        case(
            "f-string-literal", "f'hello'", FormattedString([ast.TextLiteral("hello")])
        ),
        case(
            "f-string-replacement",
            "f'{name}'",
            FormattedString([Interpolation(Name("name"))]),
        ),
        case(
            "f-string-text-and-expression",
            "f'Hello {name}!'",
            FormattedString(
                [
                    ast.TextLiteral("Hello "),
                    Interpolation(Name("name")),
                    ast.TextLiteral("!"),
                ]
            ),
        ),
        case(
            "f-string-escaped-braces",
            "f'{{{value}}}'",
            FormattedString(
                [
                    ast.TextLiteral("{"),
                    Interpolation(Name("value")),
                    ast.TextLiteral("}"),
                ]
            ),
        ),
        case(
            "f-string-repr",
            "f'{value!r}'",
            FormattedString([Interpolation(Name("value"), conversion="r")]),
        ),
        case(
            "f-string-str",
            "f'{value!s}'",
            FormattedString([Interpolation(Name("value"), conversion="s")]),
        ),
        case(
            "f-string-ascii",
            "f'{value!a}'",
            FormattedString([Interpolation(Name("value"), conversion="a")]),
        ),
        case(
            "f-string-format-spec",
            "f'{value:08.2f}'",
            FormattedString(
                [
                    Interpolation(
                        Name("value"),
                        format_spec=FormattedString([ast.TextLiteral("08.2f")]),
                    )
                ]
            ),
        ),
        case(
            "f-string-nested-format-spec",
            "f'{value:{width}.{precision}f}'",
            FormattedString(
                [
                    Interpolation(
                        Name("value"),
                        format_spec=FormattedString(
                            [
                                Interpolation(Name("width")),
                                ast.TextLiteral("."),
                                Interpolation(Name("precision")),
                                ast.TextLiteral("f"),
                            ]
                        ),
                    )
                ]
            ),
        ),
        case(
            "f-string-debug",
            "f'{value=}'",
            FormattedString(
                [
                    ast.TextLiteral("value="),
                    Interpolation(Name("value"), conversion="r"),
                ]
            ),
        ),
        case(
            "f-string-debug-whitespace",
            "f'{ value = }'",
            FormattedString(
                [
                    ast.TextLiteral(" value = "),
                    Interpolation(Name("value"), conversion="r"),
                ]
            ),
        ),
        case(
            "f-string-debug-format",
            "f'{value=:04}'",
            FormattedString(
                [
                    ast.TextLiteral("value="),
                    Interpolation(
                        Name("value"),
                        format_spec=FormattedString([ast.TextLiteral("04")]),
                    ),
                ]
            ),
        ),
        case(
            "f-string-debug-conversion",
            "f'{value=!s}'",
            FormattedString(
                [
                    ast.TextLiteral("value="),
                    Interpolation(Name("value"), conversion="s"),
                ]
            ),
        ),
        case(
            "adjacent-f-strings",
            "f'{a}' f'{b}'",
            FormattedString([Interpolation(Name("a")), Interpolation(Name("b"))]),
        ),
        case(
            "adjacent-ordinary-and-f-strings",
            "'prefix' f'{value}' 'suffix'",
            FormattedString(
                [
                    ast.TextLiteral("prefix"),
                    Interpolation(Name("value")),
                    ast.TextLiteral("suffix"),
                ]
            ),
        ),
        case(
            "raw-f-string",
            r"rf'\n{value}'",
            FormattedString([ast.TextLiteral(r"\n"), Interpolation(Name("value"))]),
        ),
        case(
            "pep701-reused-quote",
            'f"{mapping["key"]}"',
            FormattedString(
                [Interpolation(Subscript(Name("mapping"), ast.TextLiteral("key")))]
            ),
        ),
        case(
            "pep701-backslash-in-field",
            r'''f"{'\n'.join(items)}"''',
            FormattedString(
                [
                    Interpolation(
                        Call(
                            Attribute(ast.TextLiteral("\n"), "join"),
                            [CallArgument(Name("items"))],
                        )
                    )
                ]
            ),
        ),
        case(
            "pep701-comment-in-field",
            'f"{value # a field comment\n}"',
            FormattedString([Interpolation(Name("value"))]),
        ),
    ],
)
def test_formatted_strings(parse, source, expected):
    assert_ast_equal(parse(source, mode="eval"), ast.ExpressionInput(expected))


def test_addition_and_subtraction_associate_left(parse):
    expression = at(parse("a - b + c", mode="eval"), "expression")
    assert expression.operator is BinaryOperator.ADD
    assert expression.left.operator is BinaryOperator.SUBTRACT
    assert [
        expression.left.left.identifier,
        expression.left.right.identifier,
        expression.right.identifier,
    ] == ["a", "b", "c"]


def test_exponentiation_associates_right(parse):
    expression = at(parse("a ** b ** c", mode="eval"), "expression")
    assert expression.operator is BinaryOperator.POWER
    assert expression.right.operator is BinaryOperator.POWER
    assert [
        expression.left.identifier,
        expression.right.left.identifier,
        expression.right.right.identifier,
    ] == ["a", "b", "c"]


def test_multiplication_binds_above_addition(parse):
    expression = at(parse("a + b * c", mode="eval"), "expression")
    assert expression.operator is BinaryOperator.ADD
    assert expression.right.operator is BinaryOperator.MULTIPLY


def test_power_binds_above_unary_minus_on_left(parse):
    expression = at(parse("-a ** b", mode="eval"), "expression")
    assert_ast_equal(
        expression,
        Unary(
            UnaryOperator.NEGATIVE, Binary(BinaryOperator.POWER, Name("a"), Name("b"))
        ),
    )
    assert expression.operator is UnaryOperator.NEGATIVE
    assert expression.operand.operator is BinaryOperator.POWER


def test_power_accepts_unary_minus_on_right(parse):
    expression = at(parse("a ** -b", mode="eval"), "expression")
    assert expression.operator is BinaryOperator.POWER
    assert_ast_equal(expression.right, Unary(UnaryOperator.NEGATIVE, Name("b")))
    assert expression.right.operator is UnaryOperator.NEGATIVE


def test_shift_binds_below_addition(parse):
    expression = at(parse("a + b << c + d", mode="eval"), "expression")
    assert expression.operator is BinaryOperator.LEFT_SHIFT
    assert expression.left.operator is BinaryOperator.ADD
    assert expression.right.operator is BinaryOperator.ADD


def test_bitwise_operator_binding_order(parse):
    expression = at(parse("a | b ^ c & d", mode="eval"), "expression")
    assert expression.operator is BinaryOperator.BIT_OR
    assert expression.right.operator is BinaryOperator.BIT_XOR
    assert expression.right.right.operator is BinaryOperator.BIT_AND


def test_comparison_chain_is_one_node(parse):
    comparison = one(parse("a < b is not c not in d", mode="eval"), q.Comparison)
    assert expect(comparison.left, Name).identifier == "a"
    assert comparison.operators == [
        ComparisonOperator.LESS,
        ComparisonOperator.IS_NOT,
        ComparisonOperator.NOT_IN,
    ]
    assert [expect(operand, Name).identifier for operand in comparison.comparators] == [
        "b",
        "c",
        "d",
    ]


def test_not_binds_below_comparisons(parse):
    expression = at(parse("not a in b", mode="eval"), "expression")
    assert expression.operator is UnaryOperator.NOT
    assert_ast_equal(
        expression.operand, Comparison(Name("a"), [ComparisonOperator.IN], [Name("b")])
    )
    assert expression.operand.operators == [ComparisonOperator.IN]


def test_boolean_chains_flatten_at_their_own_precedence(parse):
    expression = at(parse("a or b and c and d or e", mode="eval"), "expression")
    assert expression.operator is BooleanOperator.OR
    assert len(expression.operands) == 3
    conjunction = expression.operands[1]
    assert conjunction.operator is BooleanOperator.AND
    assert [value.identifier for value in conjunction.operands] == ["b", "c", "d"]


def test_parentheses_preserve_nested_boolean_nodes(parse):
    expression = at(parse("a and (b and c)", mode="eval"), "expression")
    assert len(expression.operands) == 2
    assert_ast_equal(
        expression.operands[1], Boolean(BooleanOperator.AND, [Name("b"), Name("c")])
    )
    assert [value.identifier for value in expression.operands[1].operands] == ["b", "c"]


def test_conditional_expression_associates_right(parse):
    expression = at(parse("a if b else c if d else e", mode="eval"), "expression")
    assert_ast_equal(
        expression,
        Conditional(Name("b"), Name("a"), Conditional(Name("d"), Name("c"), Name("e"))),
    )
    assert expression.then.identifier == "a"
    assert expression.condition.identifier == "b"
    assert_ast_equal(expression.otherwise, Conditional(Name("d"), Name("c"), Name("e")))
    assert expression.otherwise.condition.identifier == "d"


def test_lambda_parameters_keep_source_order_and_categories(parse):
    parameters = one(
        parse("lambda a, /, b=2, *rest, c, d=3, **kw: a", mode="eval"), q.Lambda
    ).parameters
    assert [parameter.name for parameter in parameters] == [
        "a",
        "b",
        "rest",
        "c",
        "d",
        "kw",
    ]
    assert [parameter.kind for parameter in parameters] == [
        ParameterKind.POSITIONAL_ONLY,
        ParameterKind.POSITIONAL_OR_KEYWORD,
        ParameterKind.VAR_POSITIONAL,
        ParameterKind.KEYWORD_ONLY,
        ParameterKind.KEYWORD_ONLY,
        ParameterKind.VAR_KEYWORD,
    ]
    assert parameters[0].default is None
    assert expect(parameters[1].default, ast.IntegerLiteral).value == 2
    assert parameters[3].default is None
    assert expect(parameters[4].default, ast.IntegerLiteral).value == 3


def test_named_expression_separates_target_from_complete_rhs(parse):
    expression = one(parse("(result := a or b)", mode="eval"), q.NamedExpression)
    assert expression.target.identifier == "result"
    assert_ast_equal(
        expression.value, Boolean(BooleanOperator.OR, [Name("a"), Name("b")])
    )
    assert expect(expression.value, Boolean).operator is BooleanOperator.OR


def test_named_expression_in_list_and_set_displays_and_comprehensions(parse):
    list_display = one(parse("[a := 1, b := 2]", mode="eval"), q.List)
    assert [
        expect(el, NamedExpression).target.identifier for el in list_display.elements
    ] == [
        "a",
        "b",
    ]
    list_comp = one(
        parse("[total := total + n for n in items]", mode="eval"), q.Comprehension
    )
    assert expect(list_comp.element, NamedExpression).target.identifier == "total"
    set_display = one(parse("{a := 1, b := 2}", mode="eval"), q.Set)
    assert [
        expect(el, NamedExpression).target.identifier for el in set_display.elements
    ] == [
        "a",
        "b",
    ]
    set_comp = one(
        parse("{total := total + n for n in items}", mode="eval"), q.Comprehension
    )
    assert expect(set_comp.element, NamedExpression).target.identifier == "total"
    dict_comp = one(parse("{(k := n): n for n in items}", mode="eval"), q.Comprehension)
    assert expect(dict_comp.key, NamedExpression).target.identifier == "k"


def test_call_preserves_source_order_across_argument_kinds(parse):
    call = one(parse("f(key=b, *items, **options)", mode="eval"), q.Call)
    assert [argument.kind for argument in call.arguments] == [
        ArgumentKind.KEYWORD,
        ArgumentKind.STARRED,
        ArgumentKind.DOUBLE_STARRED,
    ]
    assert [argument.keyword for argument in call.arguments] == ["key", None, None]
    assert [expect(argument.value, Name).identifier for argument in call.arguments] == [
        "b",
        "items",
        "options",
    ]


def test_multidimensional_subscript_mixes_slice_and_index(parse):
    subscript = one(parse("matrix[1:limit:2, index, ...]", mode="eval"), q.Subscript)
    assert_ast_equal(
        subscript.index,
        Tuple(
            [
                Slice(ast.IntegerLiteral(1), Name("limit"), ast.IntegerLiteral(2)),
                Name("index"),
                ast.EllipsisLiteral(),
            ]
        ),
    )
    first, second, third = expect(subscript.index, Tuple).elements
    assert_ast_equal(
        first, Slice(ast.IntegerLiteral(1), Name("limit"), ast.IntegerLiteral(2))
    )
    first_slice = expect(first, Slice)
    assert expect(first_slice.lower, ast.IntegerLiteral).value == 1
    assert expect(first_slice.upper, Name).identifier == "limit"
    assert expect(first_slice.step, ast.IntegerLiteral).value == 2
    assert expect(second, Name).identifier == "index"
    assert expect(third, ast.EllipsisLiteral).value is Ellipsis


def test_comprehension_destructuring_belongs_to_clause_target(parse):
    comprehension = one(
        parse("[a for a, *rest in rows if a]", mode="eval"), q.Comprehension
    )
    clause = comprehension.clauses[0]
    assert_ast_equal(clause.target, Tuple([Name("a"), Unpack(Name("rest"))]))
    target = expect(clause.target, Tuple)
    assert expect(target.elements[0], Name).identifier == "a"
    assert_ast_equal(target.elements[1], Unpack(Name("rest")))
    assert expect(expect(target.elements[1], Unpack).value, Name).identifier == "rest"
    assert expect(comprehension.element, Name).identifier == "a"
    assert expect(clause.iterable, Name).identifier == "rows"
    assert expect(clause.conditions[0], Name).identifier == "a"


def test_comprehension_async_flags_and_filters_belong_to_each_clause(parse):
    comprehension = one(
        parse("[y async for x in xs if x for y in x if y]", mode="eval"),
        q.Comprehension,
    )
    assert [clause.is_async for clause in comprehension.clauses] == [True, False]
    assert [
        [expect(condition, Name).identifier for condition in clause.conditions]
        for clause in comprehension.clauses
    ] == [["x"], ["y"]]


def test_yield_delegation_is_explicit(parse):
    tree = parse("def f():\n    yield a, b\n    yield from items\n")
    yielded = one(tree, q.Yield, from_iterable=False)
    delegated = one(tree, q.Yield, from_iterable=True)
    yielded_tuple = expect(yielded.value, Tuple)
    assert_ast_equal(yielded_tuple, Tuple([Name("a"), Name("b")]))
    assert [expect(element, Name).identifier for element in yielded_tuple.elements] == [
        "a",
        "b",
    ]
    assert expect(delegated.value, Name).identifier == "items"


def test_await_wraps_primary_before_power(parse):
    tree = parse("await f().value ** exponent", mode="eval")
    expression = at(tree, "expression")
    assert expression.operator is BinaryOperator.POWER
    assert_ast_equal(expression.left, Await(Attribute(Call(Name("f"), []), "value")))
    assert_ast_equal(expression.left.value, Attribute(Call(Name("f"), []), "value"))
    assert expression.left.value.attribute == "value"
    assert len(nodes(tree, q.Call)) == 1
