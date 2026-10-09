"""Scanner regressions from Python's lexical rules, independently authored."""

import pytest

from mule.ast import Position
from mule.parser.lexer import lex


@pytest.mark.parametrize(
    "source", ["x\x00", "# comment\x00\n", "'text\x00'", 'f"{name\x00}"']
)
def test_null_characters_are_forbidden_throughout_source(source):
    with pytest.raises(SyntaxError) as error:
        lex(source, filename="null.py")
    assert error.value.filename == "null.py"
    assert error.value.offset == source.index("\x00") + 1


def test_operator_tokens_choose_longest_spelling():
    tokens = lex("a **= b; c //= d; e >>= f; g := h; ...")
    assert [token.text for token in tokens if token.kind == "OP"] == [
        "**=",
        ";",
        "//=",
        ";",
        ">>=",
        ";",
        ":=",
        ";",
        "...",
    ]


def test_blank_lines_and_bracket_continuations_do_not_create_indentation():
    tokens = lex("value = (\n\tfirst,\n  # comment\n second\n)\n\n")
    assert [token.kind for token in tokens] == [
        "NAME",
        "OP",
        "OP",
        "NAME",
        "OP",
        "NAME",
        "OP",
        "NEWLINE",
        "EOF",
    ]


def test_fragment_origin_relocates_unicode_columns_and_following_lines():
    tokens = lex(" α +\n β", start=Position(7, 12), fragment=True)
    assert [(token.text, token.start) for token in tokens] == [
        ("α", Position(7, 13)),
        ("+", Position(7, 15)),
        ("β", Position(8, 1)),
        ("", Position(8, 2)),
    ]


def test_parameter_type_comments_survive_bracket_continuations():
    source = "def f(\n    value, # type: int\n):\n    pass\n"
    assert [
        token.text
        for token in lex(source, type_comments=True)
        if token.kind == "TYPE_COMMENT"
    ] == ["# type: int"]
    assert not any(token.kind == "TYPE_COMMENT" for token in lex(source))


@pytest.mark.parametrize(
    "source", ["0x__1", "0o8", "0b_2", "1._0", "1e_2", ".1_", "2j_", "0x1j"]
)
def test_numeric_tokens_reject_invalid_digit_boundaries(source):
    with pytest.raises(SyntaxError):
        lex(source)


@pytest.mark.parametrize(
    "source",
    ["$name", "`name`", "value \\ # comment\n", "value \\\n", "value = (]", "r'odd\\'"],
)
def test_invalid_lexical_boundaries_raise_syntax_error(source):
    with pytest.raises(SyntaxError):
        lex(source)
