"""Pratt expression parsing, including Python's asymmetric power binding."""

import re
from typing import TYPE_CHECKING

from mule import ast

if TYPE_CHECKING:
    from .core import Parser

BINARY_POWER = {
    "|": 40,
    "^": 45,
    "&": 50,
    "<<": 55,
    ">>": 55,
    "+": 60,
    "-": 60,
    "*": 70,
    "@": 70,
    "/": 70,
    "//": 70,
    "%": 70,
    "**": 90,
}
COMPARISONS = frozenset(("==", "!=", "<", "<=", ">", ">=", "in", "is"))
END_EXPRESSIONS = frozenset(
    (")", "]", "}", ":", ";", "=", "in", "for", "if", "else", "as")
)
DIGITS = r"[0-9](?:_?[0-9])*"
FLOAT = rf"(?:(?:(?:{DIGITS})?\.{DIGITS}|{DIGITS}\.)(?:[eE][+-]?{DIGITS})?|{DIGITS}[eE][+-]?{DIGITS})"
INTEGER = r"(?:0(?:_?0)*|[1-9](?:_?[0-9])*)"
BASE_INTEGER = r"(?:0[bB](?:_?[01])+|0[oO](?:_?[0-7])+|0[xX](?:_?[0-9a-fA-F])+)"


class ExpressionParserMixin:
    def expression(self: Parser, min_bp=0, *, allow_named=False, allow_yield=False):
        start = self.current
        if start.text == "yield" and not allow_yield:
            self.error("Yield requires parentheses in this expression")
        if start.text in ("+", "-", "~") and min_bp > 90:
            self.error("Unary operator requires parentheses here")
        if start.text == "await" and min_bp > 95:
            self.error("Await requires parentheses here")
        if self.current.text == "lambda" and min_bp > 5:
            self.error("Lambda requires parentheses here")
        if self.current.text == "not" and min_bp > 25:
            self.error("'not' requires parentheses here")
        left = self.atom()
        while True:
            text = self.current.text
            if text in ("(", "[", ".") and 110 >= min_bp:
                begin = left.source_start()
                if self.match("("):
                    left = self.finish(ast.Call(left, self.call_arguments()), begin)
                elif self.match("["):
                    index = self.subscript_index()
                    self.expect("]")
                    left = self.finish(ast.Subscript(left, index), begin)
                else:
                    self.advance()
                    attribute = self.name_token()
                    left = self.finish(ast.Attribute(left, attribute.text), begin)
                continue
            if text == ":=" and min_bp <= 1:
                if not allow_named or self.is_parenthesized(left):
                    self.error("Invalid assignment expression target or context")
                target = left.validate_named_target(self)
                self.advance()
                right = self.expression(2)
                left = self.finish(
                    ast.NamedExpression(target, right), target.source_start()
                )
                continue
            if text == "if" and min_bp <= 5:
                self.advance()
                condition = self.expression(6)
                self.expect("else")
                otherwise = self.expression(5)
                left = self.finish(
                    ast.Conditional(condition, left, otherwise), left.source_start()
                )
                continue
            if text in ("or", "and"):
                power = 10 if text == "or" else 20
                if power < min_bp:
                    break
                self.advance()
                right = self.expression(power + 1)
                operator = ast.BooleanOperator(text)
                left = left.append_boolean(
                    operator, right, grouped=self.is_parenthesized(left)
                )
                continue
            if text in COMPARISONS or (text == "not" and self.peek().text == "in"):
                if min_bp > 30:
                    break
                operator = self.advance().text
                if operator == "is" and self.match("not"):
                    operator = "is not"
                elif operator == "not":
                    self.expect("in")
                    operator = "not in"
                right = self.expression(31)
                left = left.append_comparison(
                    ast.ComparisonOperator(operator),
                    right,
                    grouped=self.is_parenthesized(left),
                )
                continue
            power = BINARY_POWER.get(text)
            if power is None or power < min_bp:
                break
            self.advance()
            right = self.expression(80 if text == "**" else power + 1)
            left = self.finish(
                ast.Binary(ast.BinaryOperator(text), left, right), left.source_start()
            )
        return left

    def atom(self: Parser):
        start = self.current
        if start.kind == "NUMBER":
            self.advance()
            raw = start.text
            clean = raw.replace("_", "")
            if raw[-1:] in ("j", "J") and re.fullmatch(
                rf"(?:{DIGITS}|{FLOAT})[jJ]", raw
            ):
                node = ast.ComplexLiteral(complex(clean))
            elif re.fullmatch(BASE_INTEGER, raw):
                node = ast.IntegerLiteral(int(clean, 0))
            elif re.fullmatch(INTEGER, raw):
                node = ast.IntegerLiteral(int(clean, 10))
            elif re.fullmatch(FLOAT, raw):
                node = ast.FloatLiteral(float(clean))
            else:
                self.error("Invalid numeric literal", start)
            return self.finish(node, start)
        if start.kind == "STRING":
            return self.parse_strings()
        if start.text in ("None", "True", "False", "..."):
            self.advance()
            if start.text == "None":
                node = ast.NoneLiteral()
            elif start.text == "...":
                node = ast.EllipsisLiteral()
            else:
                node = ast.BooleanLiteral(start.text == "True")
            return self.finish(node, start)
        if start.text in ("+", "-", "~", "not"):
            self.advance()
            operand = self.expression(25 if start.text == "not" else 80)
            return self.finish(ast.Unary(ast.UnaryOperator(start.text), operand), start)
        if self.match("await"):
            return self.finish(ast.Await(self.expression(100)), start)
        if self.match("lambda"):
            parameters = self.parameters(":", lambda_mode=True)
            return self.finish(ast.Lambda(parameters, self.expression()), start)
        if self.match("yield"):
            delegated = bool(self.match("from"))
            value = None
            if not self.at_expression_end():
                value = self.expression_list()
            elif delegated:
                self.error("Expected iterable after 'yield from'")
            return self.finish(ast.Yield(value, from_iterable=delegated), start)
        if self.match("("):
            if self.match(")"):
                return self.finish(ast.Tuple(), start)
            first = self.star_expression(allow_named=True, allow_yield=True)
            if self.current.text in ("for", "async"):
                result = self.comprehension(ast.ComprehensionKind.GENERATOR, first)
                self.expect(")")
                return self.finish(result, start)
            elements = [first]
            comma = bool(self.match(","))
            if comma:
                while self.current.text != ")":
                    elements.append(self.star_expression(allow_named=True))
                    if not self.match(","):
                        break
                self.expect(")")
                result = self.finish(ast.Tuple(elements), start)
                self.grouped.add(id(result))
                return result
            self.expect(")")
            first.require_collection(
                self, "A starred expression must be in a collection", start
            )
            self.grouped.add(id(first))
            return first
        if self.match("["):
            if self.match("]"):
                return self.finish(ast.List(), start)
            first = self.star_expression()
            if self.current.text in ("for", "async"):
                result = self.comprehension(ast.ComprehensionKind.LIST, first)
                self.expect("]")
                return self.finish(result, start)
            elements = [first]
            while self.match(",") and self.current.text != "]":
                elements.append(self.star_expression())
            self.expect("]")
            return self.finish(ast.List(elements), start)
        if self.match("{"):
            return self.brace_display(start)
        if start.kind == "NAME":
            token = self.name_token()
            return self.finish(ast.Name(token.text), start)
        self.error("Expected an expression")

    def at_expression_end(self: Parser, stops=()):
        return (
            self.current.kind in ("NEWLINE", "EOF", "DEDENT", "TYPE_COMMENT")
            or self.current.text in END_EXPRESSIONS
            or self.current.text in stops
        )

    def star_expression(
        self: Parser, min_bp=0, *, allow_named=False, allow_star=True, allow_yield=False
    ):
        start = self.current
        if allow_star and self.match("*"):
            return self.finish(ast.Unpack(self.expression(max(min_bp, 40))), start)
        return self.expression(min_bp, allow_named=allow_named, allow_yield=allow_yield)

    def expression_list(
        self: Parser,
        *,
        allow_star=True,
        min_bp=0,
        allow_named=False,
        allow_yield=False,
        stops=(),
    ):
        start = self.current
        first = self.star_expression(
            min_bp,
            allow_star=allow_star,
            allow_named=allow_named,
            allow_yield=allow_yield,
        )
        if not self.match(","):
            return first
        elements = [first]
        while not self.at_expression_end(stops):
            elements.append(
                self.star_expression(
                    min_bp, allow_star=allow_star, allow_named=allow_named
                )
            )
            if not self.match(","):
                break
        return self.finish(ast.Tuple(elements), start)

    def subscript_index(self: Parser):
        start = self.current
        elements = [self.slice_item()]
        if not self.match(","):
            return elements[0].subscript_item(self, start)
        while self.current.text != "]":
            elements.append(self.slice_item())
            if not self.match(","):
                break
        return self.finish(ast.Tuple(elements), start)

    def slice_item(self: Parser):
        start = self.current
        lower = (
            None if self.current.text == ":" else self.star_expression(allow_named=True)
        )
        if not self.match(":"):
            return lower
        upper = None if self.current.text in (":", ",", "]") else self.expression()
        step = None
        if self.match(":") and self.current.text not in (",", "]"):
            step = self.expression()
        return self.finish(ast.Slice(lower, upper, step), start)

    def call_arguments(self: Parser, end=")"):
        arguments = []
        keyword_seen = False
        double_star_seen = False
        while self.current.text != end:
            start = self.current
            if self.match("**"):
                double_star_seen = True
                argument = ast.CallArgument(
                    self.expression(), ast.ArgumentKind.DOUBLE_STARRED
                )
            elif self.match("*"):
                if double_star_seen:
                    self.error(
                        "Iterable unpacking cannot follow keyword unpacking", start
                    )
                argument = ast.CallArgument(self.expression(), ast.ArgumentKind.STARRED)
            elif self.current.kind == "NAME" and self.peek().text == "=":
                keyword = self.name_token().text
                self.expect("=")
                keyword_seen = True
                argument = ast.CallArgument(
                    self.expression(), ast.ArgumentKind.KEYWORD, keyword
                )
            else:
                if keyword_seen or double_star_seen:
                    self.error("Positional argument follows keyword argument")
                value = self.expression(allow_named=True)
                if self.current.text in ("for", "async"):
                    if arguments:
                        self.error("Generator expression requires parentheses")
                    value = self.comprehension(ast.ComprehensionKind.GENERATOR, value)
                    argument = self.finish(ast.CallArgument(value), start)
                    arguments.append(argument)
                    self.expect(end)
                    return arguments
                argument = ast.CallArgument(value)
            arguments.append(self.finish(argument, start))
            if not self.match(","):
                break
        self.expect(end)
        return arguments

    def brace_display(self: Parser, start):
        if self.match("}"):
            return self.finish(ast.Dict(), start)
        if self.match("**"):
            entries = [
                self.finish(ast.DictEntry(None, self.expression()), self.previous.start)
            ]
            dictionary = True
        else:
            first = self.star_expression()
            dictionary = bool(self.match(":"))
            if dictionary:
                first.require_collection(self, "Dictionary keys cannot be unpacked")
                value = self.expression()
                if self.current.text in ("for", "async"):
                    result = self.comprehension(
                        ast.ComprehensionKind.DICT, value, key=first
                    )
                    self.expect("}")
                    return self.finish(result, start)
                entries = [
                    self.finish(ast.DictEntry(first, value), first.source_start())
                ]
            else:
                if self.current.text in ("for", "async"):
                    result = self.comprehension(ast.ComprehensionKind.SET, first)
                    self.expect("}")
                    return self.finish(result, start)
                elements = [first]
        while self.match(",") and self.current.text != "}":
            begin = self.current
            if dictionary:
                if self.match("**"):
                    key = None
                else:
                    key = self.expression()
                    self.expect(":")
                value = self.expression()
                entries.append(self.finish(ast.DictEntry(key, value), begin))
            else:
                elements.append(self.star_expression())
        self.expect("}")
        return self.finish(
            ast.Dict(entries) if dictionary else ast.Set(elements), start
        )

    def comprehension(self: Parser, kind, element, *, key=None):
        element.require_collection(self, "Comprehensions cannot unpack their element")
        clauses = []
        while self.current.text in ("for", "async"):
            start = self.current
            asynchronous = bool(self.match("async"))
            self.expect("for")
            target = self.expression_list(min_bp=31)
            self.validate_target(target)
            self.expect("in")
            iterable = self.expression(6)
            conditions = []
            while self.match("if"):
                conditions.append(self.expression(6))
            clauses.append(
                self.finish(
                    ast.ComprehensionClause(target, iterable, conditions, asynchronous),
                    start,
                )
            )
        return self.finish(
            ast.Comprehension(kind, element, clauses, key), element.source_start()
        )

    def validate_target(self: Parser, node, *, delete=False):
        node.validate_target(self, delete=delete)
