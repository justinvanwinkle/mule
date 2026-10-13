"""Plan source highlights from Mule ASTs, beside compiled debug metadata."""

from mule import ast
from mule.parser import parse

from .inputs import read_source, source_exists


class SourceHighlights:
    def __init__(self):
        self._sources = {}
        self._suppressed = {}

    def marker(self, filename, node):
        if node is None or node.span is None:
            return ""
        if filename not in self._sources:
            self._sources[filename] = (
                read_source(filename).splitlines() if source_exists(filename) else []
            )
        lines = self._sources[filename]
        span = node.span
        if span.start.line != span.end.line or not 1 <= span.start.line <= len(lines):
            return ""
        line = lines[span.start.line - 1]
        leading = len(line) - len(line.lstrip())
        key = (filename, span.start.line)
        if key not in self._suppressed:
            try:
                statements = parse(line.strip(), filename=filename).statements
            except SyntaxError:
                statements = []
            self._suppressed[key] = (
                statements[0].accept(_SuppressedCall()) if statements else None
            )
        return node.accept(_Marker(line, leading, self._suppressed[key]))


class _Name(ast.Visitor):
    def visit_Node(self, node):
        return False

    def visit_Name(self, node):
        return True


class _NamedCall(ast.Visitor):
    def visit_Node(self, node):
        return None

    def visit_Call(self, node):
        return node.span if node.function.accept(_Name()) else None


class _SuppressedCall(ast.Visitor):
    def visit_Node(self, node):
        return None

    def visit_Return(self, node):
        return node.value.accept(_NamedCall()) if node.value is not None else None

    def visit_Assignment(self, node):
        if len(node.targets) == 1 and node.targets[0].accept(_Name()):
            return node.value.accept(_NamedCall())
        return None


class _Marker(ast.Visitor):
    def __init__(self, line, leading, suppressed):
        self.line = line
        self.leading = leading
        self.suppressed = suppressed

    def visit_Node(self, node):
        start, end = node.span.start.column, node.span.end.column
        if not self.line[:start].strip() and not self.line[end:].strip():
            return ""
        return " " * max(0, start - self.leading) + "^" * (end - start)

    def visit_Call(self, node):
        start, end = node.span.start.column, node.span.end.column
        if self.suppressed is not None and (
            start - self.leading == self.suppressed.start.column
            and end - self.leading == self.suppressed.end.column
        ):
            return ""
        bracket = self.line.find("(", node.function.span.end.column, end)
        if bracket < 0:
            return self.visit_Node(node)
        return (
            " " * max(0, start - self.leading)
            + "~" * (bracket - start)
            + "^" * (end - bracket)
        )
