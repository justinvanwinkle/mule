"""Python's source-preserving tokenizer, implemented in compiled Python.

This module is independent of the compiler lexer. The public tokenize module
owns encoding detection and TokenInfo; this iterator supplies raw token tuples.
"""

import re
from token import (
    COMMENT,
    DEDENT,
    ENDMARKER,
    EXACT_TOKEN_TYPES,
    FSTRING_END,
    FSTRING_MIDDLE,
    FSTRING_START,
    INDENT,
    NAME,
    NEWLINE,
    NL,
    NUMBER,
    OP,
    STRING,
    TSTRING_END,
    TSTRING_MIDDLE,
    TSTRING_START,
)


_DIGITS = r"[0-9](?:_?[0-9])*"
_EXPONENT = r"[eE][+-]?" + _DIGITS
_POINT = r"(?:" + _DIGITS + r"\.(?:" + _DIGITS + r")?|\." + _DIGITS + r")"
_NUMBER = re.compile(
    r"(?:0[xX](?:_?[0-9a-fA-F])+|0[bB](?:_?[01])+|0[oO](?:_?[0-7])+|"
    r"(?:"
    + _POINT
    + r"(?:"
    + _EXPONENT
    + r")?|"
    + _DIGITS
    + _EXPONENT
    + r"|"
    + _DIGITS
    + r")[jJ]?)"
)
_OPERATORS = sorted(EXACT_TOKEN_TYPES, key=len, reverse=True)
_PREFIXES = ("", "r", "u", "b", "f", "t", "br", "rb", "fr", "rf", "tr", "rt")
_OPENING = {")": "(", "]": "[", "}": "{"}


class _LineReader:
    """Own one physical line and the context of the token being scanned."""

    def __init__(self, source, encoding):
        self.source = source
        self.encoding = encoding
        self.line = ""
        self.text = ""
        self.row = 0
        self.column = 0
        self.ended = False
        self.synthetic = False
        self.context = None

    def peek(self):
        if self.column < len(self.text):
            return self.text[self.column]
        if self.ended:
            return ""
        try:
            line = self.source()
        except StopIteration:
            line = b"" if self.encoding is not None else ""
        if self.encoding is None:
            if not isinstance(line, str):
                raise TypeError("readline() returned a non-string object")
        else:
            if not isinstance(line, bytes):
                raise TypeError("readline() returned a non-bytes object")
            line = line.decode(self.encoding, "replace")
        if not line:
            self.ended = True
            return ""
        self.line = line
        self.synthetic = not line.endswith(("\n", "\r"))
        self.text = line + "\n" if self.synthetic else line
        self.row += 1
        self.column = 0
        if "\x00" in line:
            raise SyntaxError(
                "source code cannot contain null bytes",
                ("<string>", self.row, 0, line.split("\x00", 1)[0], self.row, 0),
            )
        if self.context is not None:
            self.context.append(line)
        return self.text[0]

    def startswith(self, text):
        self.peek()
        return self.text.startswith(text, self.column)

    def take(self, count=1):
        text = self.text[self.column : self.column + count]
        self.column += count
        return text

    def position(self):
        return self.row, self.column

    def begin(self):
        self.context = [self.line]
        return self.position()

    def finish(self, kind, text, start, end=None):
        context = "".join(self.context)
        self.context = None
        return kind, text, start, self.position() if end is None else end, context


class _CodeMode:
    def __init__(self):
        self.brackets = []

    def next(self, scanner):
        if scanner.beginning:
            scanner.indent(self.brackets)
            if scanner.pending:
                return scanner.pending.pop(0)
        return scanner.code_token(self)

    def newline(self, scanner, start, text, end):
        logical = scanner.has_code and not self.brackets
        scanner.beginning = True
        scanner.last_code_line = scanner.reader.line if scanner.has_code else ""
        if logical:
            scanner.has_code = False
        if scanner.extra:
            return scanner.reader.finish(NEWLINE if logical else NL, text, start, end)
        if logical:
            if scanner.comment_start is not None:
                start = scanner.comment_start
            suffix = "\r" if text.startswith("\r") else ""
            last = (end[0], end[1] - 1) if text else start
            return scanner.reader.finish(
                NEWLINE, scanner.comment_text + suffix, start, last
            )
        scanner.reader.context = None
        return None


class _ExpressionMode(_CodeMode):
    def __init__(self, string):
        super().__init__()
        self.string = string

    def next(self, scanner):
        reader = scanner.reader
        character = reader.peek()
        if not self.brackets and character == "}":
            start = reader.begin()
            reader.take()
            scanner.modes.pop()
            return reader.finish(scanner.operator_type("}"), "}", start)
        if not self.brackets and character == ":" and not reader.startswith(":="):
            start = reader.begin()
            reader.take()
            scanner.modes.append(_FormatMode(self.string))
            return reader.finish(scanner.operator_type(":"), ":", start)
        if not character:
            scanner.error("f-string: expecting '}'")
        return scanner.code_token(self)

    def newline(self, scanner, start, text, end):
        if scanner.extra:
            return scanner.reader.finish(NL, text, start, end)
        scanner.reader.context = None
        return None


class _StringMode:
    def __init__(self, prefix, delimiter, start):
        self.delimiter = delimiter
        self.start = start
        self.raw = "r" in prefix.lower()
        template = "t" in prefix.lower()
        self.middle = TSTRING_MIDDLE if template else FSTRING_MIDDLE
        self.end = TSTRING_END if template else FSTRING_END

    def next(self, scanner):
        return scanner.string_text(self)

    def closing(self, scanner, parts, start):
        reader = scanner.reader
        if parts:
            return reader.finish(self.middle, "".join(parts), start)
        text = reader.take(len(self.delimiter))
        scanner.modes.pop()
        return reader.finish(self.end, text, start)

    def brace(self, scanner, parts, start):
        scanner.error("f-string: single '}' is not allowed")

    def doubled_closing(self, reader):
        return reader.startswith("}}")


class _FormatMode:
    def __init__(self, string):
        self.string = string
        self.delimiter = string.delimiter
        self.start = string.start
        self.raw = string.raw
        self.middle = string.middle
        self.closing_brace = False

    def next(self, scanner):
        if self.closing_brace:
            reader = scanner.reader
            start = reader.begin()
            reader.take()
            scanner.modes.pop()
            scanner.modes.pop()
            return reader.finish(scanner.operator_type("}"), "}", start)
        return scanner.string_text(self)

    def closing(self, scanner, parts, start):
        scanner.error("f-string: expecting '}'")

    def brace(self, scanner, parts, start):
        self.closing_brace = True
        return scanner.reader.finish(self.middle, "".join(parts), start)

    def doubled_closing(self, reader):
        return False


class TokenizerIter:
    """Yield raw five-tuples from a string or encoded-byte readline callback."""

    def __init__(self, source, extra_tokens, *, encoding=None):
        self.reader = _LineReader(source, encoding)
        self.extra = bool(extra_tokens)
        self.modes = [_CodeMode()]
        self.indentation = [(0, 0)]
        self.pending = []
        self.beginning = True
        self.continued = False
        self.has_code = False
        self.finished = False
        self.last_code_line = ""
        self.comment_start = None
        self.comment_text = ""

    def __iter__(self):
        return self

    def __next__(self):
        if self.finished and not self.pending:
            raise StopIteration
        try:
            while True:
                if self.pending:
                    return self.pending.pop(0)
                if self.finished:
                    raise StopIteration
                if len(self.modes) == 1 and not self.reader.peek():
                    self.finish()
                    continue
                result = self.modes[-1].next(self)
                if result is not None:
                    return result
        except StopIteration:
            raise
        except Exception:
            self.finished = True
            raise

    def operator_type(self, text):
        return OP if self.extra else EXACT_TOKEN_TYPES[text]

    def error(self, message, start=None, error_type=SyntaxError):
        reader = self.reader
        row, column = reader.position() if start is None else start
        raise error_type(
            message,
            ("<string>", row, column + 1, reader.line.rstrip("\r\n"), row, column + 1),
        )

    def layout_error(self, message, error_type):
        self.error(
            message, (self.reader.row, len(self.reader.line.rstrip("\r\n"))), error_type
        )

    def indent(self, brackets):
        reader = self.reader
        reader.peek()
        column = alternate = 0
        first = reader.column
        while reader.peek() and reader.peek() in " \t\f":
            character = reader.take()
            if character == " ":
                column += 1
                alternate += 1
            elif character == "\t":
                column = (column // 8 + 1) * 8
                alternate += 1
            else:
                column = alternate = 0
        blank = not reader.peek() or reader.peek() in "\r\n#"
        if not blank and not brackets and not self.continued:
            previous, previous_alternate = self.indentation[-1]
            if column == previous:
                if alternate != previous_alternate:
                    self.layout_error(
                        "inconsistent use of tabs and spaces in indentation", TabError
                    )
            elif column > previous:
                if alternate <= previous_alternate:
                    self.layout_error(
                        "inconsistent use of tabs and spaces in indentation", TabError
                    )
                if len(self.indentation) >= 100:
                    self.error(
                        "too many levels of indentation", error_type=IndentationError
                    )
                self.indentation.append((column, alternate))
                start = (reader.row, first) if self.extra else (reader.row, -1)
                end = reader.position() if self.extra else start
                text = reader.line[first : reader.column] if self.extra else ""
                self.pending.append((INDENT, text, start, end, reader.line))
            else:
                if not any(width == column for width, _ in self.indentation):
                    self.layout_error(
                        "unindent does not match any outer indentation level",
                        IndentationError,
                    )
                while self.indentation[-1][0] > column:
                    self.indentation.pop()
                    position = reader.position() if self.extra else (reader.row, -1)
                    self.pending.append((DEDENT, "", position, position, reader.line))
                if self.indentation[-1][1] != alternate:
                    self.layout_error(
                        "inconsistent use of tabs and spaces in indentation", TabError
                    )
        self.beginning = False
        self.continued = False
        self.comment_start = None
        self.comment_text = ""

    def finish(self):
        if self.modes[0].brackets or self.continued:
            offset = self.reader.column if self.continued else 0
            raise SyntaxError(
                "unexpected EOF in multi-line statement",
                (
                    "<string>",
                    self.reader.row,
                    offset,
                    self.reader.line,
                    self.reader.row,
                    None,
                ),
            )
        reader = self.reader
        position = (reader.row + 1, 0) if self.extra else (reader.row, -1)
        while len(self.indentation) > 1:
            self.indentation.pop()
            self.pending.append(
                (
                    DEDENT,
                    "",
                    position,
                    position,
                    "" if self.extra else self.last_code_line,
                )
            )
        if self.extra or reader.row:
            self.pending.append(
                (
                    ENDMARKER,
                    "",
                    position,
                    position,
                    "" if self.extra else self.last_code_line,
                )
            )
        self.finished = True

    def code_token(self, mode):
        reader = self.reader
        character = reader.peek()
        if not character:
            return None
        if character in " \t\f":
            reader.take()
            return None
        start = reader.begin()
        if character == "\x00":
            self.error("source code string cannot contain null bytes", start)
        if character == "#":
            parts = []
            while reader.peek() and reader.peek() not in "\r\n":
                parts.append(reader.take())
            text = "".join(parts)
            if self.extra:
                return reader.finish(COMMENT, text, start)
            self.comment_start = start
            self.comment_text = text
            reader.context = None
            return None
        if character in "\r\n":
            synthetic = reader.synthetic
            text = reader.take()
            if text == "\r" and reader.text.startswith("\n", reader.column):
                text += reader.take()
            end = reader.position()
            if text == "\r":
                end = end[0], end[1] + 1
            if synthetic:
                text = ""
            return mode.newline(self, start, text, end)
        if character == "\\":
            reader.take()
            if reader.peek() not in ("\r", "\n") or reader.synthetic:
                self.layout_error(
                    "unexpected character after line continuation character",
                    SyntaxError,
                )
            if reader.take() == "\r" and reader.text.startswith("\n", reader.column):
                reader.take()
            self.beginning = True
            self.continued = True
            reader.context = None
            return None
        header = self.string_header()
        if header is not None:
            prefix, delimiter, count = header
            text = reader.take(count)
            self.has_code = True
            if "f" in prefix.lower() or "t" in prefix.lower():
                self.modes.append(_StringMode(prefix, delimiter, start))
                kind = TSTRING_START if "t" in prefix.lower() else FSTRING_START
                return reader.finish(kind, text, start)
            return self.ordinary_string(prefix, delimiter, text, start)
        if character == "_" or character.isidentifier() or ord(character) >= 128:
            parts = [reader.take()]
            while reader.peek() and (
                ("a" + reader.peek()).isidentifier() or ord(reader.peek()) >= 128
            ):
                parts.append(reader.take())
            text = "".join(parts)
            if not self.extra and not text.isidentifier():
                self.error("invalid character in identifier", start)
            self.has_code = True
            return reader.finish(NAME, text, start)
        if (
            character in "0123456789"
            or character == "."
            and reader.text[reader.column + 1 : reader.column + 2] in "0123456789"
            and reader.text[reader.column + 1 : reader.column + 2]
        ):
            match = _NUMBER.match(reader.text, reader.column)
            if match is None:
                self.error("invalid decimal literal", start)
            text = match.group()
            reader.take(len(text))
            following = reader.peek()
            if following and ("a" + following).isidentifier():
                message = "invalid decimal literal"
                if text == "0" and following in "xXbBoO":
                    message = (
                        "invalid hexadecimal literal"
                        if following in "xX"
                        else "invalid base literal"
                    )
                    reader.take()
                self.error(message)
            plain = text.replace("_", "")
            if (
                not self.extra
                and plain.isdecimal()
                and plain[0] == "0"
                and any(char != "0" for char in plain)
            ):
                self.error(
                    "leading zeros in decimal integer literals are not permitted", start
                )
            self.has_code = True
            return reader.finish(NUMBER, text, start)
        operator = None
        for candidate in _OPERATORS:
            if reader.startswith(candidate):
                operator = candidate
                break
        if operator is None:
            if not self.extra:
                self.error("invalid character", start)
            operator = character
        reader.take(len(operator))
        if operator in "([{":
            if len(mode.brackets) >= 200:
                self.error("too many nested parentheses", start)
            mode.brackets.append(operator)
        elif operator in ")]}":
            if not self.extra and (
                not mode.brackets or mode.brackets[-1] != _OPENING[operator]
            ):
                self.error("unmatched '" + operator + "'", start)
            if mode.brackets:
                mode.brackets.pop()
        self.has_code = True
        return reader.finish(self.operator_type(operator), operator, start)

    def string_header(self):
        reader = self.reader
        cursor = reader.column
        while cursor < len(reader.text) and reader.text[cursor] in "rRuUbBfFtT":
            cursor += 1
        if cursor >= len(reader.text) or reader.text[cursor] not in "'\"":
            return None
        prefix = reader.text[reader.column : cursor]
        if prefix.lower() not in _PREFIXES:
            return None
        quote = reader.text[cursor]
        delimiter = quote * 3 if reader.text.startswith(quote * 3, cursor) else quote
        return prefix, delimiter, cursor - reader.column + len(delimiter)

    def ordinary_string(self, prefix, delimiter, text, start):
        reader = self.reader
        parts = [text]
        while True:
            character = reader.peek()
            if not character:
                message = (
                    "unterminated triple-quoted string literal"
                    if len(delimiter) == 3
                    else "unterminated string literal"
                )
                self.error(message, start)
            if reader.startswith(delimiter):
                parts.append(reader.take(len(delimiter)))
                return reader.finish(STRING, "".join(parts), start)
            if character in "\r\n" and len(delimiter) == 1:
                self.error("unterminated string literal", start)
            parts.append(reader.take())
            if character == "\\":
                if reader.peek():
                    parts.append(reader.take())

    def string_text(self, mode):
        reader = self.reader
        reader.peek()
        start = reader.begin()
        parts = []
        while True:
            character = reader.peek()
            if not character or character in "\r\n" and len(mode.delimiter) == 1:
                self.error("unterminated f-string literal", mode.start)
            if reader.startswith(mode.delimiter):
                return mode.closing(self, parts, start)
            if character == "{":
                if reader.startswith("{{"):
                    parts.append(reader.take())
                    end = reader.position()
                    reader.take()
                    return reader.finish(mode.middle, "".join(parts), start, end)
                if parts:
                    return reader.finish(mode.middle, "".join(parts), start)
                reader.take()
                self.modes.append(_ExpressionMode(mode))
                return reader.finish(self.operator_type("{"), "{", start)
            if character == "}":
                if mode.doubled_closing(reader):
                    parts.append(reader.take())
                    end = reader.position()
                    reader.take()
                    return reader.finish(mode.middle, "".join(parts), start, end)
                return mode.brace(self, parts, start)
            parts.append(reader.take())
            if character == "\\" and reader.peek() and reader.peek() not in "{}":
                escaped = reader.take()
                parts.append(escaped)
                if escaped == "N" and not mode.raw and reader.peek() == "{":
                    while reader.peek() and reader.peek() != "}":
                        parts.append(reader.take())
                    if reader.peek():
                        parts.append(reader.take())
