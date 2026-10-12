"""Manual literal decoding and formatted/template string construction."""

import unicodedata
from typing import TYPE_CHECKING

from mule import ast

from .lexer import Token, position_at, scan_string, string_header

if TYPE_CHECKING:
    from .core import Parser

_ESCAPES = {
    "a": "\a",
    "b": "\b",
    "f": "\f",
    "n": "\n",
    "r": "\r",
    "t": "\t",
    "v": "\v",
    "\\": "\\",
    "'": "'",
    '"': '"',
}
_HEX = frozenset("0123456789abcdefABCDEF")


class StringParserMixin:
    def parse_strings(self: Parser) -> ast.Expression:
        """Consume one adjacent literal group, merging its text segments."""
        first = self.current
        parts: list[ast.Literal | ast.Interpolation] = []
        # The decoded segments of a group without replacement fields.
        texts: list[str] = []
        byte_strings: list[bytes] = []
        family = None
        while self.current.kind == "STRING":
            token = self.advance()
            header = string_header(token.text, 0)
            if header is None:
                self.error("invalid string prefix", token)
            prefix, delimiter, body_index = header
            prefix = prefix.lower()
            body = token.text[body_index : -len(delimiter)]
            is_bytes = "b" in prefix
            is_template = "t" in prefix
            is_formatted = "f" in prefix
            current_family = (
                "bytes"
                if is_bytes
                else "template"
                if is_template
                else "formatted"
                if is_formatted
                else "text"
            )
            if family is None:
                family = current_family
            elif (
                family == "bytes"
                and current_family != "bytes"
                or current_family == "bytes"
                and family != "bytes"
            ):
                self.error("cannot mix bytes and text literals", token)
            elif (
                family == "template"
                and current_family != "template"
                or current_family == "template"
                and family != "template"
            ):
                self.error(
                    "template strings must be adjacent only to template strings", token
                )
            elif current_family == "formatted":
                family = "formatted"
            body_start = position_at(token.text, body_index, token.start)
            if is_formatted or is_template:
                literal_parts, _ = self._formatted_parts(
                    body,
                    body_start,
                    token,
                    raw="r" in prefix,
                    template=is_template,
                )
                for part in literal_parts:
                    self._append_string_part(parts, part)
            else:
                span = ast.Span(token.start, token.end)
                if is_bytes:
                    data = self._decode_bytes(body, token, raw="r" in prefix)
                    byte_strings.append(data)
                    self._append_string_part(parts, ast.BytesLiteral(data, span=span))
                else:
                    text = self._decode_text(body, token, raw="r" in prefix)
                    texts.append(text)
                    self._append_string_part(parts, ast.TextLiteral(text, span=span))
        if family == "template":
            result = ast.TemplateString(parts)
        elif family == "formatted":
            result = ast.FormattedString(parts)
        elif family == "bytes":
            result = ast.BytesLiteral(b"".join(byte_strings))
        else:
            result = ast.TextLiteral("".join(texts))
        return self.finish(result, first)

    @staticmethod
    def _append_string_part(parts, part):
        part.append_string_part(parts)

    def _decode_text(self: Parser, body, token, *, raw=False) -> str:
        return body if raw else self._decode_escapes(body, token, is_bytes=False)

    def _decode_bytes(self: Parser, body, token, *, raw=False) -> bytes:
        if any(ord(character) > 127 for character in body):
            self.error("bytes literals may contain only ASCII source characters", token)
        if raw:
            return body.encode("ascii")
        # Each decoded character holds one byte value.
        decoded = self._decode_escapes(body, token, is_bytes=True)
        return bytes(ord(character) for character in decoded)

    def _decode_escapes(self: Parser, body, token, *, is_bytes) -> str:
        """``body`` with its backslash escapes decoded."""
        decoded = []
        index = 0
        while index < len(body):
            character = body[index]
            if character != "\\" or index + 1 == len(body):
                decoded.append(character)
                index += 1
                continue
            escaped = body[index + 1]
            if escaped in _ESCAPES:
                decoded.append(_ESCAPES[escaped])
                index += 2
            elif escaped == "\n":
                index += 2
            elif escaped in "01234567":
                end = index + 2
                while end < min(len(body), index + 4) and body[end] in "01234567":
                    end += 1
                value = int(body[index + 1 : end], 8)
                decoded.append(chr(value % 256 if is_bytes else value))
                index = end
            elif escaped == "x" or not is_bytes and escaped in "uU":
                count = {"x": 2, "u": 4, "U": 8}[escaped]
                digits = body[index + 2 : index + 2 + count]
                if len(digits) != count or any(digit not in _HEX for digit in digits):
                    self.error("invalid hexadecimal escape", token)
                value = int(digits, 16)
                if value > 0x10FFFF:
                    self.error("Unicode escape is outside the Unicode range", token)
                decoded.append(chr(value))
                index += count + 2
            elif escaped == "N" and not is_bytes:
                if body[index + 2 : index + 3] != "{":
                    self.error("malformed named Unicode escape", token)
                end = body.find("}", index + 3)
                if end < 0:
                    self.error("malformed named Unicode escape", token)
                try:
                    decoded.append(unicodedata.lookup(body[index + 3 : end]))
                except KeyError:
                    self.error("unknown Unicode character name", token)
                index = end + 1
            else:
                decoded.extend(("\\", escaped))
                index += 2
        return "".join(decoded)

    def _formatted_parts(
        self: Parser,
        body,
        origin,
        token,
        *,
        raw,
        template,
        index=0,
        format_spec=False,
    ):
        parts: list[ast.Literal | ast.Interpolation] = []
        literal = []
        literal_start = index

        def flush(end):
            if not literal:
                return
            text = self._decode_text("".join(literal), token, raw=raw)
            self._append_string_part(
                parts,
                ast.TextLiteral(
                    text,
                    span=ast.Span(
                        position_at(body, literal_start, origin),
                        position_at(body, end, origin),
                    ),
                ),
            )
            literal.clear()

        while index < len(body):
            character = body[index]
            if character == "}" and format_spec:
                flush(index)
                return parts, index
            if character == "{" and body[index : index + 2] == "{{" and not format_spec:
                literal.append("{")
                index += 2
            elif character == "}" and body[index : index + 2] == "}}":
                literal.append("}")
                index += 2
            elif character == "{":
                flush(index)
                field_parts, index = self._replacement(
                    body,
                    index,
                    origin,
                    token,
                    raw=raw,
                    template=template,
                )
                for part in field_parts:
                    self._append_string_part(parts, part)
                literal_start = index
            elif character == "}":
                self.error(
                    "single closing brace in formatted string",
                    self._string_location(token, position_at(body, index, origin)),
                )
            elif (
                character == "\\"
                and index + 1 < len(body)
                and body[index + 1] not in "{}"
            ):
                if (
                    not raw
                    and body[index + 1] == "N"
                    and body[index + 2 : index + 3] == "{"
                ):
                    end = body.find("}", index + 3)
                    if end < 0:
                        self.error("malformed named Unicode escape", token)
                    literal.append(body[index : end + 1])
                    index = end + 1
                else:
                    literal.append(body[index : index + 2])
                    index += 2
            else:
                literal.append(character)
                index += 1
        flush(index)
        if format_spec:
            self.error("unterminated format specification", token)
        return parts, index

    @staticmethod
    def _string_location(token, position):
        return Token("STRING", token.text, position, position)

    @staticmethod
    def _skip_field_trivia(body, index):
        while index < len(body):
            if body[index].isspace():
                index += 1
            elif body[index] == "#":
                end = body.find("\n", index)
                index = len(body) if end < 0 else end + 1
            else:
                break
        return index

    def _replacement(self: Parser, body, opening, origin, token, *, raw, template):
        expression_start = opening + 1
        index = expression_start
        brackets = []
        while index < len(body):
            character = body[index]
            if string_header(body, index) is not None:
                index = scan_string(body, index, self.filename, origin)
                continue
            if character == "#":
                newline = body.find("\n", index)
                index = len(body) if newline < 0 else newline + 1
                continue
            if character in "([{":
                brackets.append(character)
            elif character in ")]}":
                if character == "}" and not brackets:
                    break
                if (
                    not brackets
                    or brackets[-1] != {")": "(", "]": "[", "}": "{"}[character]
                ):
                    self.error("mismatched replacement field delimiter", token)
                brackets.pop()
            elif not brackets:
                if (
                    character == ":"
                    or character == "!"
                    and body[index + 1 : index + 2] != "="
                ):
                    break
                if (
                    character == "="
                    and body[index + 1 : index + 2] != "="
                    and body[index - 1 : index] not in "=!<>:"
                ):
                    break
            index += 1
        if index >= len(body):
            self.error("unterminated replacement field", token)
        expression_text = body[expression_start:index]
        if not expression_text.strip():
            self.error(
                "empty replacement field",
                self._string_location(
                    token, position_at(body, expression_start, origin)
                ),
            )
        expression = self.parse_fragment(
            expression_text, start=position_at(body, expression_start, origin)
        )
        parts = []
        debug = body[index] == "="
        if debug:
            index = self._skip_field_trivia(body, index + 1)
            parts.append(
                ast.TextLiteral(
                    body[expression_start:index],
                    span=ast.Span(
                        position_at(body, expression_start, origin),
                        position_at(body, index, origin),
                    ),
                )
            )
        conversion = None
        if index < len(body) and body[index] == "!":
            index += 1
            if index >= len(body) or body[index] not in "sra":
                self.error("invalid replacement field conversion", token)
            conversion = body[index]
            index = self._skip_field_trivia(body, index + 1)
        specification = None
        if index < len(body) and body[index] == ":":
            spec_start = index + 1
            spec_parts, index = self._formatted_parts(
                body,
                origin,
                token,
                raw=raw,
                template=False,
                index=spec_start,
                format_spec=True,
            )
            specification = ast.FormattedString(
                spec_parts,
                span=ast.Span(
                    position_at(body, spec_start, origin),
                    position_at(body, index, origin),
                ),
            )
        if index >= len(body) or body[index] != "}":
            self.error(
                "expected closing brace in replacement field",
                self._string_location(
                    token, position_at(body, min(index, len(body)), origin)
                ),
            )
        if debug and conversion is None and specification is None:
            conversion = "r"
        parts.append(
            ast.Interpolation(
                expression,
                conversion,
                specification,
                expression_text if template else None,
                span=ast.Span(
                    position_at(body, opening, origin),
                    position_at(body, index + 1, origin),
                ),
            )
        )
        return parts, index + 1
