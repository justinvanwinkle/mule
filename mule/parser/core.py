"""Token cursor and public entry points for Mule's parser."""

import unicodedata
from dataclasses import replace
from typing import NoReturn

from mule import ast

from .expressions import ExpressionParserMixin
from .lexer import Token, lex
from .patterns import PatternParserMixin
from .statements import StatementParserMixin
from .strings import StringParserMixin

SOURCE_START = ast.Position(1, 0)

HARD_KEYWORDS = frozenset(
    [
        "False",
        "None",
        "True",
        "and",
        "as",
        "assert",
        "async",
        "await",
        "break",
        "class",
        "continue",
        "def",
        "del",
        "elif",
        "else",
        "except",
        "finally",
        "for",
        "from",
        "global",
        "if",
        "import",
        "in",
        "is",
        "lambda",
        "nonlocal",
        "not",
        "or",
        "pass",
        "raise",
        "return",
        "try",
        "while",
        "with",
        "yield",
    ]
)


class Parser(
    ExpressionParserMixin, StatementParserMixin, PatternParserMixin, StringParserMixin
):
    def __init__(
        self,
        source: str,
        filename: str = "<unknown>",
        *,
        type_comments: bool = False,
        start: ast.Position = SOURCE_START,
        fragment: bool = False,
    ):
        self.source = source.replace("\r\n", "\n").replace("\r", "\n")
        self.filename = filename
        self.type_comments = type_comments
        self.origin = start
        self.tokens = lex(
            self.source,
            filename,
            start=start,
            type_comments=type_comments,
            fragment=fragment,
        )
        self.type_ignores = []
        retained = []
        for token in self.tokens:
            if token.kind == "TYPE_COMMENT":
                payload = token.text.partition("type:")[2].strip()
                if payload == "ignore" or payload.startswith("ignore["):
                    self.type_ignores.append(
                        ast.TypeIgnore(
                            token.start.line,
                            payload[len("ignore") :],
                            span=ast.Span(token.start, token.end),
                        )
                    )
                    continue
            retained.append(token)
        self.tokens = retained
        self.index = 0
        self.previous = self.tokens[0]
        self.grouped = set()

    @property
    def current(self) -> Token:
        return self.tokens[self.index]

    def peek(self, distance: int = 1) -> Token:
        return self.tokens[min(self.index + distance, len(self.tokens) - 1)]

    def advance(self) -> Token:
        token = self.current
        self.previous = token
        if self.index < len(self.tokens) - 1:
            self.index += 1
        return token

    def match(self, text: str) -> Token | None:
        if self.current.text == text or self.current.kind == text:
            return self.advance()
        return None

    def match_kind(self, kind: str) -> Token | None:
        if self.current.kind == kind:
            return self.advance()
        return None

    def expect(self, text: str) -> Token:
        token = self.match(text)
        if token is None:
            self.error(f"Expected {text!r}")
        return token

    def expect_kind(self, kind: str) -> Token:
        if self.current.kind != kind:
            self.error(f"Expected {kind.lower()}")
        return self.advance()

    def name_token(self) -> Token:
        if self.current.kind != "NAME" or self.current.text in HARD_KEYWORDS:
            self.error("Expected an identifier")
        token = self.advance()
        return replace(token, text=unicodedata.normalize("NFKC", token.text))

    def finish[T: ast.Node](
        self, node: T, start: Token | ast.Position, *, end: ast.Position | None = None
    ) -> T:
        position = start.start if isinstance(start, Token) else start
        node.span = ast.Span(position, self.previous.end if end is None else end)
        return node

    def is_parenthesized(self, node: ast.Node) -> bool:
        return id(node) in self.grouped

    def error(self, message: str, token: Token | None = None) -> NoReturn:
        token = self.current if token is None else token
        lines = self.source.split("\n")
        line_index = token.start.line - self.origin.line
        text = lines[line_index] if 0 <= line_index < len(lines) else ""
        if line_index == 0:
            text = " " * self.origin.column + text
        raise SyntaxError(
            message, (self.filename, token.start.line, token.start.column + 1, text)
        )

    def parse_fragment(self, source: str, start: ast.Position) -> ast.Expression:
        parser = Parser(source, self.filename, start=start, fragment=True)
        while parser.match_kind("NEWLINE"):
            pass
        result = parser.expression_list(allow_named=True, allow_yield=True)
        if isinstance(result, ast.Unpack):
            parser.error("A starred expression must be in a tuple")
        while parser.match_kind("NEWLINE"):
            pass
        parser.expect_kind("EOF")
        return result


def parse(
    source: str,
    filename: str = "<unknown>",
    mode: str = "exec",
    *,
    type_comments: bool = False,
) -> ast.Node:
    """Parse Python source into Mule nodes without invoking the host parser."""
    if not isinstance(source, str):
        raise TypeError("source must be a string")
    if mode not in ("exec", "eval", "single"):
        raise ValueError("mode must be 'exec', 'eval', or 'single'")
    parser = Parser(source, filename, type_comments=type_comments)
    start = ast.Position(1, 0)
    if mode == "eval":
        while parser.match_kind("NEWLINE"):
            pass
        expression = parser.expression_list(allow_star=False)
        while parser.match_kind("NEWLINE"):
            pass
        parser.expect_kind("EOF")
        return parser.finish(ast.ExpressionInput(expression), start)
    if mode == "single":
        while parser.match_kind("NEWLINE"):
            pass
        statements = [] if parser.current.kind == "EOF" else parser.statement()
        while parser.match_kind("NEWLINE"):
            pass
        parser.expect_kind("EOF")
        return parser.finish(ast.InteractiveInput(statements), start)
    statements = parser.statements()
    parser.expect_kind("EOF")
    return parser.finish(ast.Module(statements, parser.type_ignores), start)
