"""Conservative artifact dependencies for literal and prefixed import calls.

This is syntax dependency discovery, not evaluation or a replacement for IR1
constant folding. Unknown expressions stay unknown; package prefixes reserve
immediate artifacts so runtime importlib may select among them.
"""

from mule import ast


class _Assignments(ast.Visitor):
    def __init__(self):
        self.bindings = {}

    def visit_Node(self, node):
        for child in node.children():
            child.accept(self)

    def visit_Expression(self, node):
        pass

    def visit_FunctionDefinition(self, node):
        pass

    def visit_ClassDefinition(self, node):
        pass

    def visit_Assignment(self, node):
        for target in node.targets:
            name = target.binding_name()
            if name is not None:
                self.bindings.setdefault(name, []).append(node.value)

    def visit_AnnotatedAssignment(self, node):
        name = node.target.binding_name()
        if name is not None and node.value is not None:
            self.bindings.setdefault(name, []).append(node.value)


class _ImportCall(ast.Visitor):
    def visit_Node(self, node):
        return False

    def visit_Name(self, node):
        return node.identifier in ("__import__", "import_module")

    def visit_Attribute(self, node):
        return node.attribute == "import_module"


_IMPORT_CALL = _ImportCall()


class _Patterns(ast.Visitor):
    def __init__(self, scopes, active=()):
        self.scopes = scopes
        self.active = active

    def visit_Node(self, node):
        return set()

    def visit_TextLiteral(self, node):
        return {node.value}

    def visit_Name(self, node):
        if node.identifier in self.active:
            return set()
        for scope in reversed(self.scopes):
            if node.identifier in scope:
                visitor = _Patterns(self.scopes, (*self.active, node.identifier))
                return set().union(
                    *(value.accept(visitor) for value in scope[node.identifier])
                )
        return set()

    def visit_Conditional(self, node):
        return node.then.accept(self) | node.otherwise.accept(self)

    def visit_Binary(self, node):
        if node.operator is not ast.BinaryOperator.ADD:
            return set()
        left = node.left.accept(self)
        right = node.right.accept(self)
        if right:
            return {prefix + suffix for prefix in left for suffix in right}
        return {prefix for prefix in left if prefix.endswith(".")}


class ImportDependencies(ast.Visitor):
    def __init__(self):
        self.scopes = []
        self.patterns = set()

    def visit_Node(self, node):
        for child in node.children():
            child.accept(self)

    def scope(self, statements, parameters=()):
        assignments = _Assignments()
        for statement in statements:
            statement.accept(assignments)
        for parameter in parameters:
            assignments.bindings.setdefault(parameter.name, [])
        self.scopes.append(assignments.bindings)
        try:
            for statement in statements:
                statement.accept(self)
        finally:
            self.scopes.pop()

    def visit_Module(self, node):
        self.scope(node.statements)

    def visit_FunctionDefinition(self, node):
        for decorator in node.decorators:
            decorator.accept(self)
        for parameter in node.parameters:
            parameter.accept(self)
        self.scope(node.body, node.parameters)

    def visit_ClassDefinition(self, node):
        for argument in node.arguments:
            argument.accept(self)
        for decorator in node.decorators:
            decorator.accept(self)
        self.scope(node.body)

    def visit_Call(self, node):
        if node.function.accept(_IMPORT_CALL) and node.arguments:
            argument = node.arguments[0]
            if argument.kind is ast.ArgumentKind.POSITIONAL:
                self.patterns.update(argument.value.accept(_Patterns(self.scopes)))
        self.visit_Node(node)


def discover(tree):
    visitor = ImportDependencies()
    tree.accept(visitor)
    return sorted(visitor.patterns)
