import pytest

from mule import ast

from .ast_query import assert_ast_equal, at


@pytest.mark.parametrize(
    ("source", "mode", "expected"),
    [
        pytest.param("", "exec", ast.Module(), id="empty-module"),
        pytest.param("# comment\n", "exec", ast.Module(), id="comment-only-module"),
        pytest.param("\n\n", "exec", ast.Module(), id="blank-module"),
        pytest.param(
            "x = 1",
            "exec",
            ast.Module([ast.Assignment([ast.Name("x")], ast.IntegerLiteral(1))]),
            id="module-without-final-newline",
        ),
        pytest.param(
            "x = 1\ny = 2\n",
            "exec",
            ast.Module(
                [
                    ast.Assignment([ast.Name("x")], ast.IntegerLiteral(1)),
                    ast.Assignment([ast.Name("y")], ast.IntegerLiteral(2)),
                ]
            ),
            id="multiple-statements",
        ),
        pytest.param(
            "x + 1",
            "eval",
            ast.ExpressionInput(
                ast.Binary(ast.BinaryOperator.ADD, ast.Name("x"), ast.IntegerLiteral(1))
            ),
            id="expression",
        ),
        pytest.param(
            "\n(x + 1)\n",
            "eval",
            ast.ExpressionInput(
                ast.Binary(ast.BinaryOperator.ADD, ast.Name("x"), ast.IntegerLiteral(1))
            ),
            id="padded-expression",
        ),
        pytest.param(
            "1, 2",
            "eval",
            ast.ExpressionInput(
                ast.Tuple([ast.IntegerLiteral(1), ast.IntegerLiteral(2)])
            ),
            id="expression-tuple",
        ),
        pytest.param(
            "value\n",
            "single",
            ast.InteractiveInput([ast.ExpressionStatement(ast.Name("value"))]),
            id="interactive-expression",
        ),
        pytest.param(
            "x = 1\n",
            "single",
            ast.InteractiveInput(
                [ast.Assignment([ast.Name("x")], ast.IntegerLiteral(1))]
            ),
            id="interactive-assignment",
        ),
        pytest.param(
            "a = 1; b = 2\n",
            "single",
            ast.InteractiveInput(
                [
                    ast.Assignment([ast.Name("a")], ast.IntegerLiteral(1)),
                    ast.Assignment([ast.Name("b")], ast.IntegerLiteral(2)),
                ]
            ),
            id="interactive-semicolons",
        ),
        pytest.param(
            "if x:\n    pass\n\n",
            "single",
            ast.InteractiveInput([ast.If(ast.Name("x"), [ast.Pass()])]),
            id="interactive-suite",
        ),
    ],
)
def test_parse_mode_returns_mule_root_and_expected_structure(
    parse, source, mode, expected
):
    assert_ast_equal(parse(source, mode=mode), expected)


@pytest.mark.parametrize(
    ("source", "path", "start", "end"),
    [
        pytest.param(
            "α = β + 1",
            "statements.0.value.left",
            (1, 4),
            (1, 5),
            id="greek-character-columns",
        ),
        pytest.param(
            "名字 = '值'",
            "statements.0.targets.0",
            (1, 0),
            (1, 2),
            id="cjk-character-columns",
        ),
        pytest.param(
            "answer = '🙂' + tail",
            "statements.0.value.right",
            (1, 15),
            (1, 19),
            id="emoji-character-columns",
        ),
        pytest.param(
            "result = (\n    first +\n    second\n)",
            "statements.0.value.right",
            (3, 4),
            (3, 10),
            id="multiline-expression",
        ),
        pytest.param(
            "if ready:\n    value = item\n",
            "statements.0.body.0.value",
            (2, 12),
            (2, 16),
            id="indented-statement",
        ),
        pytest.param(
            "a = 1\r\nb = 2\r\n",
            "statements.1.targets.0",
            (2, 0),
            (2, 1),
            id="crlf-newlines",
        ),
        pytest.param(
            "a = 1\rb = 2\r", "statements.1.targets.0", (2, 0), (2, 1), id="cr-newlines"
        ),
        pytest.param(
            "x = 1; y = 2",
            "statements.1.targets.0",
            (1, 7),
            (1, 8),
            id="semicolon-columns",
        ),
    ],
)
def test_source_spans_use_lines_and_unicode_character_columns(
    parse, source, path, start, end
):
    node = at(parse(source), path)
    assert node.span == ast.Span(ast.Position(*start), ast.Position(*end))


@pytest.mark.parametrize("type_comments", [False, True], ids=["ignored", "retained"])
def test_type_comment_option_controls_comment_fields(parse, type_comments):
    source = "item = []  # type: list[int]\n# type: ignore[assignment]\n"
    expected = ast.Module(
        [
            ast.Assignment(
                [ast.Name("item")],
                ast.List(),
                type_comment="list[int]" if type_comments else None,
            )
        ],
        type_ignores=[ast.TypeIgnore(2, "[assignment]")] if type_comments else [],
    )
    assert_ast_equal(parse(source, type_comments=type_comments), expected)


def test_syntax_error_preserves_supplied_filename(parse):
    with pytest.raises(SyntaxError) as error:
        parse("answer =", filename="example.mule.py")
    assert error.value.filename == "example.mule.py"


def test_single_mode_rejects_multiple_independent_statements(parse):
    with pytest.raises(SyntaxError):
        parse("x = 1\ny = 2\n", mode="single")


def test_eval_mode_rejects_assignment_statement(parse):
    with pytest.raises(SyntaxError):
        parse("x = 1", mode="eval")


@pytest.mark.parametrize("source", [None, 3, b"x = 1", ["x = 1"], {"source": "x = 1"}])
def test_invalid_source_reports_public_type_error(parse, source):
    with pytest.raises(TypeError, match="source must be a string"):
        parse(source)


def test_source_error_precedes_invalid_mode_error(parse):
    with pytest.raises(TypeError, match="source must be a string"):
        parse(None, mode="invalid")


def test_valid_source_with_invalid_mode_reports_mode_error(parse):
    with pytest.raises(ValueError, match="mode must be"):
        parse("x = 1\r\n", mode="invalid")
