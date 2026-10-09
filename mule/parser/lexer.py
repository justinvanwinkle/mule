"""Handwritten Python lexical scanner with Unicode character coordinates."""

import re
from bisect import bisect_right
from dataclasses import dataclass

from mule.ast import Position


@dataclass(frozen=True, slots=True)
class Token:
    kind: str
    text: str
    start: Position
    end: Position


_ORIGIN = Position(1, 0)

_PREFIXES = {"", "r", "u", "b", "f", "t", "br", "rb", "fr", "rf", "tr", "rt"}
_DIGITS = r"[0-9](?:_?[0-9])*"
_NUMBER = re.compile(
    rf"(?:0[xX](?:_?[0-9a-fA-F])+|0[oO](?:_?[0-7])+|0[bB](?:_?[01])+|"
    rf"(?:(?:{_DIGITS}\.(?:{_DIGITS})?|\.{_DIGITS})(?:[eE][+-]?{_DIGITS})?|"
    rf"{_DIGITS}[eE][+-]?{_DIGITS}|{_DIGITS})[jJ]?)"
)
_OPERATORS = sorted(
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


def position_at(source: str, index: int, start: Position = _ORIGIN) -> Position:
    prefix = source[:index]
    lines = prefix.count("\n")
    return Position(
        start.line + lines,
        len(prefix.rsplit("\n", 1)[-1]) if lines else start.column + index,
    )


def lexical_error(
    message: str,
    source: str,
    index: int,
    filename: str,
    start: Position = _ORIGIN,
    error_type: type[SyntaxError] = SyntaxError,
):
    position = position_at(source, index, start)
    line = source.split("\n")[position.line - start.line]
    if position.line == start.line:
        line = " " * start.column + line
    raise error_type(message, (filename, position.line, position.column + 1, line))


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


def scan_string(
    source: str,
    index: int,
    filename: str = "<unknown>",
    start: Position = _ORIGIN,
) -> int:
    """Find one literal's end, including PEP 701 nested replacement fields."""
    header = string_header(source, index)
    if header is None:
        lexical_error("invalid string prefix", source, index, filename, start)
    prefix, delimiter, cursor = header
    formatted = "f" in prefix.lower() or "t" in prefix.lower()
    while cursor < len(source):
        if source.startswith(delimiter, cursor):
            return cursor + len(delimiter)
        character = source[cursor]
        if character == "\n" and len(delimiter) == 1:
            lexical_error(
                "unterminated string literal", source, cursor, filename, start
            )
        if character == "\\":
            # Braces remain special even immediately after a literal backslash.
            cursor += (
                1 if formatted and source[cursor + 1 : cursor + 2] in ("{", "}") else 2
            )
        elif formatted and character == "{":
            if source.startswith("{{", cursor):
                cursor += 2
            else:
                cursor = (
                    scan_replacement(source, cursor + 1, filename, start, delimiter) + 1
                )
        elif formatted and character == "}":
            if not source.startswith("}}", cursor):
                lexical_error(
                    "single closing brace in formatted string",
                    source,
                    cursor,
                    filename,
                    start,
                )
            cursor += 2
        else:
            cursor += 1
    lexical_error("unterminated string literal", source, index, filename, start)


def scan_replacement(
    source: str,
    index: int,
    filename: str = "<unknown>",
    start: Position = _ORIGIN,
    outer_quote: str | None = None,
) -> int:
    """Find a field's closing brace without mistaking strings/comments for syntax."""
    stack: list[str] = []
    cursor = index
    while cursor < len(source):
        character = source[cursor]
        if character == ":" and not stack:
            return scan_format(source, cursor + 1, filename, start, outer_quote)
        header = string_header(source, cursor)
        if header is not None:
            cursor = scan_string(source, cursor, filename, start)
        elif character == "#":
            newline = source.find("\n", cursor)
            cursor = len(source) if newline < 0 else newline + 1
        elif character in "([{":
            stack.append(character)
            cursor += 1
        elif character in ")]}":
            if character == "}" and not stack:
                return cursor
            if not stack or stack[-1] != {")": "(", "]": "[", "}": "{"}[character]:
                lexical_error(
                    "mismatched delimiter in replacement field",
                    source,
                    cursor,
                    filename,
                    start,
                )
            stack.pop()
            cursor += 1
        elif character == "\\" and source[cursor + 1 : cursor + 2] == "\n":
            cursor += 2
        else:
            cursor += 1
    lexical_error("unterminated replacement field", source, index - 1, filename, start)


def scan_format(source, index, filename, start, outer_quote):
    """Format text treats hashes and quotes as text, rather than expressions."""
    cursor = index
    while cursor < len(source):
        character = source[cursor]
        if outer_quote and source.startswith(outer_quote, cursor):
            lexical_error(
                "unterminated format specification", source, cursor, filename, start
            )
        if character == "\n" and outer_quote and len(outer_quote) == 1:
            lexical_error(
                "newline in format specification", source, cursor, filename, start
            )
        if character == "}":
            return cursor
        if character == "{":
            cursor = (
                scan_replacement(source, cursor + 1, filename, start, outer_quote) + 1
            )
        elif character == "\\" and source[cursor + 1 : cursor + 2] not in ("{", "}"):
            cursor += 2
        else:
            cursor += 1
    lexical_error("unterminated format specification", source, index, filename, start)


def _name_start(character: str) -> bool:
    return character == "_" or character.isidentifier()


def _name_continue(character: str) -> bool:
    return ("a" + character).isidentifier()


def lex(
    source: str,
    filename: str = "<unknown>",
    *,
    start: Position = _ORIGIN,
    type_comments: bool = False,
    fragment: bool = False,
) -> list[Token]:
    """Tokenize logical lines; fragment mode suppresses indentation and newlines."""
    source = source.replace("\r\n", "\n").replace("\r", "\n")
    if "\x00" in source:
        lexical_error(
            "source cannot contain null characters",
            source,
            source.index("\x00"),
            filename,
            start,
        )
    line_starts = [0] + [
        index + 1 for index, character in enumerate(source) if character == "\n"
    ]
    tokens: list[Token] = []
    cursor = 0
    indentation = [(0, 0)]
    brackets: list[tuple[str, int]] = []
    beginning = True
    continued = False
    line_has_code = False

    def position(index):
        line_index = bisect_right(line_starts, index) - 1
        column = index - line_starts[line_index]
        return Position(
            start.line + line_index, column + (start.column if line_index == 0 else 0)
        )

    def emit(kind, text, first, last):
        tokens.append(
            Token(
                kind,
                text,
                position(first),
                position(last),
            )
        )

    def error(message, index, error_type=SyntaxError):
        lexical_error(message, source, index, filename, start, error_type)

    while cursor < len(source):
        if beginning:
            first = cursor
            column = alternate = 0
            while cursor < len(source) and source[cursor] in " \t\f":
                character = source[cursor]
                if character == " ":
                    column += 1
                    alternate += 1
                elif character == "\t":
                    column = (column // 8 + 1) * 8
                    alternate += 1
                else:
                    column = alternate = 0
                cursor += 1
            blank = cursor >= len(source) or source[cursor] in "\n#"
            if not fragment and not brackets and not continued and not blank:
                previous, previous_alternate = indentation[-1]
                if column == previous:
                    if alternate != previous_alternate:
                        error(
                            "inconsistent use of tabs and spaces in indentation",
                            cursor,
                            TabError,
                        )
                elif column > previous:
                    if alternate <= previous_alternate:
                        error(
                            "inconsistent use of tabs and spaces in indentation",
                            cursor,
                            TabError,
                        )
                    indentation.append((column, alternate))
                    emit("INDENT", source[first:cursor], first, cursor)
                else:
                    while indentation[-1][0] > column:
                        indentation.pop()
                        emit("DEDENT", "", cursor, cursor)
                    if indentation[-1][0] != column:
                        error(
                            "unindent does not match any outer indentation level",
                            cursor,
                            IndentationError,
                        )
                    if indentation[-1][1] != alternate:
                        error(
                            "inconsistent use of tabs and spaces in indentation",
                            cursor,
                            TabError,
                        )
            beginning = False
            continued = False
            if cursor >= len(source):
                break
        character = source[cursor]
        if character in " \t\f":
            cursor += 1
            continue
        if character == "#":
            end = source.find("\n", cursor)
            if end < 0:
                end = len(source)
            comment = source[cursor:end]
            if type_comments and re.match(r"#\s*type:\s*", comment):
                emit("TYPE_COMMENT", comment, cursor, end)
                if not brackets:
                    line_has_code = True
            cursor = end
            continue
        if character == "\n":
            if line_has_code and not brackets and not fragment:
                emit("NEWLINE", "\n", cursor, cursor + 1)
                line_has_code = False
            cursor += 1
            beginning = True
            continue
        if character == "\\":
            if source[cursor + 1 : cursor + 2] != "\n":
                error("unexpected character after line continuation", cursor)
            cursor += 2
            beginning = True
            continued = True
            continue
        first = cursor
        if string_header(source, cursor) is not None:
            cursor = scan_string(source, cursor, filename, start)
            emit("STRING", source[first:cursor], first, cursor)
        elif _name_start(character):
            cursor += 1
            while cursor < len(source) and _name_continue(source[cursor]):
                cursor += 1
            emit("NAME", source[first:cursor], first, cursor)
        elif (
            character in "0123456789"
            or character == "."
            and source[cursor + 1 : cursor + 2].isdigit()
        ):
            match = _NUMBER.match(source, cursor)
            if match is None:
                error("invalid numeric literal", cursor)
            cursor = match.end()
            text = source[first:cursor]
            if cursor < len(source) and _name_continue(source[cursor]):
                error("invalid numeric literal", cursor)
            plain = text.replace("_", "")
            if (
                plain.isdecimal()
                and plain[0] == "0"
                and any(digit != "0" for digit in plain)
            ):
                error(
                    "leading zeros in decimal integer literals are not permitted", first
                )
            emit("NUMBER", text, first, cursor)
        else:
            operator = next(
                (item for item in _OPERATORS if source.startswith(item, cursor)), None
            )
            if operator is None:
                error("invalid character", cursor)
            cursor += len(operator)
            if operator in "([{":
                brackets.append((operator, first))
            elif operator in ")]}":
                if (
                    not brackets
                    or brackets[-1][0] != {")": "(", "]": "[", "}": "{"}[operator]
                ):
                    error("unmatched closing delimiter", first)
                brackets.pop()
            emit("OP", operator, first, cursor)
        line_has_code = True
    if brackets:
        error("unclosed delimiter", brackets[-1][1])
    if continued:
        error("unexpected end after line continuation", max(0, len(source) - 1))
    if line_has_code and not fragment:
        emit("NEWLINE", "", cursor, cursor)
    while len(indentation) > 1:
        indentation.pop()
        emit("DEDENT", "", cursor, cursor)
    emit("EOF", "", cursor, cursor)
    return tokens
