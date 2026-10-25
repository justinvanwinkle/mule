"""Plan source highlights from Mule ASTs, beside compiled debug metadata."""

from mule import ast
from mule.parser import parse

from .inputs import read_source, source_exists


class SourceHighlights:
    def __init__(self):
        self._sources = {}
        self._suppressed = {}
        self._node_cache = {}
        self._marker = _Marker()

    def marker(self, filename, node):
        if node is None or node.span is None:
            return ""
        cache_key = (filename, id(node))
        cached = self._node_cache.get(cache_key)
        if cached is not None:
            return cached
        if filename not in self._sources:
            self._sources[filename] = (
                read_source(filename).splitlines() if source_exists(filename) else []
            )
        lines = self._sources[filename]
        span = node.span
        if span.start.line != span.end.line or not 1 <= span.start.line <= len(lines):
            self._node_cache[cache_key] = ""
            return ""
        line = lines[span.start.line - 1]
        leading = len(line) - len(line.lstrip())
        m = self._marker
        m.line = line
        m.leading = leading
        m.filename = filename
        m.line_no = span.start.line
        m.suppressed_cache = self._suppressed
        res = node.accept(m)
        self._node_cache[cache_key] = res
        return res


class _Name(ast.Visitor):
    def visit_Node(self, node):
        return False

    def visit_Name(self, node):
        return True


_NAME = _Name()


class _NamedCall(ast.Visitor):
    def visit_Node(self, node):
        return None

    def visit_Call(self, node):
        return node.span if node.function.accept(_NAME) else None


_NAMED_CALL = _NamedCall()


class _SuppressedCall(ast.Visitor):
    def visit_Node(self, node):
        return None

    def visit_Module(self, node):
        return node.statements[0].accept(self) if node.statements else None

    def visit_Return(self, node):
        return node.value.accept(_NAMED_CALL) if node.value is not None else None

    def visit_Assignment(self, node):
        if len(node.targets) == 1 and node.targets[0].accept(_NAME):
            return node.value.accept(_NAMED_CALL)
        return None


_SUPPRESSED_CALL = _SuppressedCall()


class _Marker(ast.Visitor):
    def __init__(self):
        self.line = ""
        self.leading = 0
        self.filename = ""
        self.line_no = 0
        self.suppressed_cache = {}

    def visit_Node(self, node):
        start, end = node.span.start.column, node.span.end.column
        if not self.line[:start].strip() and not self.line[end:].strip():
            return ""
        return " " * max(0, start - self.leading) + "^" * (end - start)

    def visit_Call(self, node):
        start, end = node.span.start.column, node.span.end.column
        if start > self.leading and node.function.accept(_NAME):
            prefix = self.line[self.leading : start]
            if prefix.startswith("return") or "=" in prefix:
                key = (self.filename, self.line_no)
                if key not in self.suppressed_cache:
                    try:
                        module = parse(self.line.strip(), filename=self.filename)
                    except SyntaxError:
                        self.suppressed_cache[key] = None
                    else:
                        self.suppressed_cache[key] = module.accept(_SUPPRESSED_CALL)
                suppressed = self.suppressed_cache[key]
                if suppressed is not None and (
                    start - self.leading == suppressed.start.column
                    and end - self.leading == suppressed.end.column
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
