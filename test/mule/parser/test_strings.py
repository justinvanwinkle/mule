"""Manual expected values and trees for literal and interpolation regressions."""

import pytest

from mule import ast
from test.mule.parser import ast_query as q
from test.mule.parser.ast_query import assert_ast_equal, one


@pytest.mark.parametrize(
    "prefix, expected",
    [(prefix, ast.TextLiteral("hello")) for prefix in ("", "u", "U", "r", "R")]
    + [
        (prefix, ast.BytesLiteral(b"hello"))
        for prefix in ("b", "B", "br", "bR", "Br", "BR", "rb", "rB", "Rb", "RB")
    ],
)
def test_plain_literal_prefixes_accept_all_supported_case_combinations(
    parse, prefix, expected
):
    assert_ast_equal(
        parse(prefix + "'hello'", mode="eval"),
        ast.ExpressionInput(expected),
    )


@pytest.mark.parametrize(
    "prefix, expected",
    [
        (prefix, ast.FormattedString([ast.Interpolation(ast.Name("name"))]))
        for prefix in ("f", "F", "fr", "fR", "Fr", "FR", "rf", "rF", "Rf", "RF")
    ]
    + [
        (
            prefix,
            ast.TemplateString(
                [ast.Interpolation(ast.Name("name"), expression_text="name")]
            ),
        )
        for prefix in ("t", "T", "tr", "tR", "Tr", "TR", "rt", "rT", "Rt", "RT")
    ],
)
def test_interpolated_literal_prefixes_accept_all_supported_case_combinations(
    parse, prefix, expected
):
    assert_ast_equal(
        parse(prefix + "'{name}'", mode="eval"), ast.ExpressionInput(expected)
    )


@pytest.mark.parametrize(
    "source,expected",
    [
        (r"'café\N{GRINNING FACE}'", ast.TextLiteral("café😀")),
        (r"'\U0001f600\101\0'", ast.TextLiteral("😀A\x00")),
        (r"b'\xff\101\0'", ast.BytesLiteral(b"\xffA\x00")),
        (r"b'\u03b1'", ast.BytesLiteral(b"\\u03b1")),
        (r"r'\N{GRINNING FACE}'", ast.TextLiteral("\\N{GRINNING FACE}")),
        ("'first\\\nsecond'", ast.TextLiteral("firstsecond")),
        (r"'unknown\q'", ast.TextLiteral("unknown\\q")),
    ],
)
def test_escape_decoding_preserves_unicode_and_bytes(parse, source, expected):
    assert_ast_equal(parse(source, mode="eval"), ast.ExpressionInput(expected))


@pytest.mark.parametrize(
    "prefix, constructor, expression_text",
    [("f", ast.FormattedString, None), ("t", ast.TemplateString, "value")],
)
@pytest.mark.parametrize("spec", ["#x", "'", "!r", ">10"])
def test_format_specification_text_is_not_expression_syntax(
    parse, prefix, constructor, expression_text, spec
):
    source = prefix + '"{value:' + spec + '}"'
    field = ast.Interpolation(
        ast.Name("value"),
        format_spec=ast.FormattedString([ast.TextLiteral(spec)]),
        expression_text=expression_text,
    )
    assert_ast_equal(
        parse(source, mode="eval"), ast.ExpressionInput(constructor([field]))
    )


def test_nested_format_field_can_have_its_own_literal_format_specification(parse):
    expected = ast.FormattedString(
        [
            ast.Interpolation(
                ast.Name("value"),
                format_spec=ast.FormattedString(
                    [
                        ast.Interpolation(
                            ast.Name("width"),
                            format_spec=ast.FormattedString([ast.TextLiteral("05")]),
                        )
                    ]
                ),
            )
        ]
    )
    assert_ast_equal(
        parse("f'{value:{width:05}}'", mode="eval"), ast.ExpressionInput(expected)
    )


def test_comparison_operators_are_not_debug_or_conversion_delimiters(parse):
    expression = ast.Comparison(
        ast.Name("left"), [ast.ComparisonOperator.NOT_EQUAL], [ast.Name("right")]
    )
    assert_ast_equal(
        parse("f'{left != right}'", mode="eval"),
        ast.ExpressionInput(ast.FormattedString([ast.Interpolation(expression)])),
    )


def test_template_expression_text_retains_whitespace_before_format_specification(parse):
    field = one(parse('t"{ value :#x}"', mode="eval"), q.Interpolation)
    assert field.expression_text == " value "
    assert field.format_spec.parts[0].value == "#x"


@pytest.mark.parametrize(
    "source",
    [
        r"'\U00110000'",
        r"'\N{NOT A CHARACTER NAME}'",
        r"'\Nmissing'",
        "f'{ }'",
        "f'{value!}'",
        "f'{value!rr}'",
        "f'{value!r extra}'",
        'f"{value:\n}"',
        "t'{value}' f'{other}'",
        "b'value' f'{other}'",
    ],
)
def test_malformed_literals_and_fields_raise_syntax_error(parse, source):
    with pytest.raises(SyntaxError):
        parse(source, mode="eval")


def test_pep701_requires_two_levels_of_nested_format_fields(parse):
    # https://peps.python.org/pep-0701/#specification
    expected = ast.FormattedString(
        [
            ast.Interpolation(
                ast.Name("value"),
                format_spec=ast.FormattedString(
                    [
                        ast.Interpolation(
                            ast.Name("width"),
                            format_spec=ast.FormattedString(
                                [
                                    ast.Interpolation(ast.Name("precision")),
                                ]
                            ),
                        ),
                    ]
                ),
            ),
        ]
    )
    assert_ast_equal(
        parse("f'{value:{width:{precision}}}'", mode="eval"),
        ast.ExpressionInput(expected),
    )


def test_field_comments_after_debug_specifier_preserve_raw_debug_text(parse):
    expected = ast.FormattedString(
        [
            ast.TextLiteral("value= # debug comment\n"),
            ast.Interpolation(ast.Name("value"), conversion="r"),
        ]
    )
    assert_ast_equal(
        parse('f"{value= # debug comment\n}"', mode="eval"),
        ast.ExpressionInput(expected),
    )


def test_field_comments_after_conversion_are_expression_trivia(parse):
    expected = ast.FormattedString(
        [
            ast.Interpolation(ast.Name("value"), conversion="r"),
        ]
    )
    assert_ast_equal(
        parse('f"{value!r # conversion comment\n}"', mode="eval"),
        ast.ExpressionInput(expected),
    )
