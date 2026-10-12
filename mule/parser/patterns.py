"""Structural pattern grammar and match statements."""

from typing import TYPE_CHECKING

from mule import ast

if TYPE_CHECKING:
    from .core import Parser


class PatternParserMixin:
    def match_statement(self: Parser) -> ast.Match:
        start = self.expect("match")
        subject = self.expression_list(allow_named=True, stops=(":",))
        subject.require_collection(self, "starred match subject requires a comma")
        self.expect(":")
        self.expect_kind("NEWLINE")
        self.expect_kind("INDENT")
        cases = []
        while self.current.kind != "DEDENT":
            case_start = self.expect("case")
            pattern = self.pattern()
            guard = None
            if self.match("if"):
                guard = self.expression(allow_named=True)
            body = self.suite()
            cases.append(
                self.finish(
                    ast.MatchCase(pattern, body, guard),
                    case_start,
                    end=body[-1].source_end(),
                )
            )
        if not cases:
            self.error("match statement requires at least one case")
        self.advance()
        return self.finish(ast.Match(subject, cases), start, end=cases[-1].source_end())

    def pattern(self: Parser) -> ast.Pattern:
        """Parse a case pattern, including an unparenthesized sequence."""
        start = self.current
        first = self._pattern_item(allow_star=True)
        if not self.match(","):
            first.validate_unsequenced(self)
            return first
        elements = [first]
        while self.current.text not in (":", "if"):
            elements.append(self._pattern_item(allow_star=True))
            if not self.match(","):
                break
        return self.finish(ast.SequencePattern(elements), start)

    def _pattern_item(self: Parser, *, allow_star: bool = False) -> ast.Pattern:
        if allow_star and self.current.text == "*":
            start = self.advance()
            name = self.name_token().text
            return self.finish(ast.StarPattern(None if name == "_" else name), start)
        start = self.current
        alternatives = [self._closed_pattern()]
        while self.match("|"):
            alternatives.append(self._closed_pattern())
        result = alternatives[0]
        if len(alternatives) > 1:
            result = self.finish(ast.OrPattern(alternatives), start)
        if self.match("as"):
            capture = self.name_token()
            if capture.text == "_":
                self.error("wildcard cannot be an as-pattern capture")
            result = self.finish(ast.AsPattern(result, capture.text), start)
        return result

    def _closed_pattern(self: Parser) -> ast.Pattern:
        start = self.current
        if start.text == "_":
            self.advance()
            return self.finish(ast.CapturePattern(None), start)
        if start.text == "[":
            return self._sequence_pattern("[")
        if start.text == "(":
            return self._sequence_pattern("(")
        if start.text == "{":
            return self._mapping_pattern()
        if start.kind in ("NUMBER", "STRING") or start.text in (
            "-",
            "None",
            "True",
            "False",
        ):
            return self.finish(ast.LiteralPattern(self._pattern_literal()), start)
        if start.kind != "NAME":
            self.error("expected a pattern")
        value = self._pattern_name()
        if self.current.text == "(":
            return self._class_pattern(value, start)
        return self.finish(value.pattern_reference(), start)

    def _pattern_name(self: Parser) -> ast.Expression:
        start = self.name_token()
        value = self.finish(ast.Name(start.text), start)
        while self.match("."):
            attribute = self.name_token()
            value = self.finish(ast.Attribute(value, attribute.text), start)
        return value

    def _sequence_pattern(self: Parser, opener: str) -> ast.Pattern:
        start = self.expect(opener)
        closer = "]" if opener == "[" else ")"
        if self.match(closer):
            return self.finish(ast.SequencePattern([]), start)
        elements = [self._pattern_item(allow_star=True)]
        has_comma = False
        while self.match(","):
            has_comma = True
            if self.current.text == closer:
                break
            elements.append(self._pattern_item(allow_star=True))
        self.expect(closer)
        if opener == "(" and not has_comma:
            elements[0].validate_unsequenced(self)
            return self.finish(elements[0], start)
        return self.finish(ast.SequencePattern(elements), start)

    def _mapping_pattern(self: Parser) -> ast.MappingPattern:
        start = self.expect("{")
        entries = []
        rest = None
        while self.current.text != "}":
            if self.match("**"):
                capture = self.name_token()
                if capture.text == "_":
                    self.error("wildcard cannot capture a mapping rest")
                rest = capture.text
                self.match(",")
                if self.current.text != "}":
                    self.error("mapping rest capture must be last")
                break
            entry_start = self.current
            key = self._mapping_pattern_key()
            self.expect(":")
            pattern = self._pattern_item()
            entries.append(self.finish(ast.PatternEntry(key, pattern), entry_start))
            if not self.match(","):
                break
        self.expect("}")
        return self.finish(ast.MappingPattern(entries, rest), start)

    def _mapping_pattern_key(self: Parser) -> ast.Expression:
        if self.current.kind in ("NUMBER", "STRING") or self.current.text in (
            "-",
            "None",
            "True",
            "False",
        ):
            return self._pattern_literal()
        value = self._pattern_name()
        return value.mapping_pattern_key(self)

    def _class_pattern(
        self: Parser, class_name: ast.Expression, start
    ) -> ast.ClassPattern:
        self.expect("(")
        positional = []
        keywords = []
        while self.current.text != ")":
            if self.current.kind == "NAME" and self.peek().text == "=":
                keyword_start = self.name_token()
                self.expect("=")
                pattern = self._pattern_item()
                keywords.append(
                    self.finish(
                        ast.KeywordPattern(keyword_start.text, pattern), keyword_start
                    )
                )
            else:
                if keywords:
                    self.error(
                        "positional class pattern cannot follow keyword patterns"
                    )
                positional.append(self._pattern_item())
            if not self.match(","):
                break
        self.expect(")")
        return self.finish(ast.ClassPattern(class_name, positional, keywords), start)

    def _pattern_literal(self: Parser) -> ast.Expression:
        start = self.current
        if start.kind == "STRING":
            value = self.parse_strings()
            value.validate_pattern_literal(self)
            return value
        if start.text in ("None", "True", "False"):
            self.advance()
            node = (
                ast.NoneLiteral()
                if start.text == "None"
                else ast.BooleanLiteral(start.text == "True")
            )
            return self.finish(node, start)
        negative = bool(self.match("-"))
        if self.current.kind != "NUMBER":
            self.error("expected a numeric pattern literal")
        value = self.atom()
        value.validate_numeric_pattern(self)
        real = value
        if negative:
            value = self.finish(ast.Unary(ast.UnaryOperator.NEGATIVE, value), start)
        if self.current.text in ("+", "-"):
            real.validate_real_pattern(self)
            operator = self.advance()
            if self.current.kind != "NUMBER":
                self.error("complex pattern literal requires an imaginary operand")
            imaginary = self.atom()
            imaginary.validate_imaginary_pattern(self)
            value = self.finish(
                ast.Binary(ast.BinaryOperator(operator.text), value, imaginary), start
            )
        return value
