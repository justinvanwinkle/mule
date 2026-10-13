"""Dependency-light lexical definitions shared with compiled tokenization."""

import re

_PREFIXES = {"", "r", "u", "b", "f", "t", "br", "rb", "fr", "rf", "tr", "rt"}
_DIGITS = r"[0-9](?:_?[0-9])*"
NUMBER = re.compile(
    rf"(?:0[xX](?:_?[0-9a-fA-F])+|0[oO](?:_?[0-7])+|0[bB](?:_?[01])+|"
    rf"(?:(?:{_DIGITS}\.(?:{_DIGITS})?|\.{_DIGITS})(?:[eE][+-]?{_DIGITS})?|"
    rf"{_DIGITS}[eE][+-]?{_DIGITS}|{_DIGITS})[jJ]?)"
)
OPERATORS = sorted(
    {
        "+",
        "-",
        "*",
        "**",
        "/",
        "//",
        "%",
        "@",
        "<<",
        ">>",
        "&",
        "|",
        "^",
        "~",
        ":=",
        "<",
        ">",
        "<=",
        ">=",
        "==",
        "!=",
        "=",
        "+=",
        "-=",
        "*=",
        "**=",
        "/=",
        "//=",
        "%=",
        "@=",
        "<<=",
        ">>=",
        "&=",
        "|=",
        "^=",
        "->",
        "...",
        ":",
        ".",
        ",",
        ";",
        "(",
        ")",
        "[",
        "]",
        "{",
        "}",
        "!",
    },
    key=len,
    reverse=True,
)


def string_header(source: str, index: int) -> tuple[str, str, int] | None:
    """Return prefix, quote delimiter, and body start for a literal at index."""
    end = index
    while end < len(source) and source[end] in "rRuUbBfFtT":
        end += 1
    if end >= len(source) or source[end] not in "'\"":
        return None
    prefix = source[index:end]
    if prefix.lower() not in _PREFIXES:
        return None
    quote = source[end]
    delimiter = quote * 3 if source.startswith(quote * 3, end) else quote
    return prefix, delimiter, end + len(delimiter)


def name_start(character: str) -> bool:
    return character == "_" or character.isidentifier()


def name_continue(character: str) -> bool:
    return ("a" + character).isidentifier()
