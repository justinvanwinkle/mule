"""Conservative artifact dependencies for literal and prefixed import calls.

This is syntax dependency discovery, not evaluation or a replacement for IR1
constant folding. Unknown expressions stay unknown; package prefixes reserve
immediate artifacts so runtime importlib may select among them.
"""

from mule import ast


class _TargetNames(ast.Visitor):
    def visit_Node(self, node):
        return ()

    def visit_Name(self, node):
        return (node.identifier,)

    def visit_Attribute(self, node):
        if node.value.binding_name() == "self":
            return (node.attribute,)
        return ()

    def visit_Tuple(self, node):
        return tuple(name for element in node.elements for name in element.accept(self))

    def visit_List(self, node):
        return tuple(name for element in node.elements for name in element.accept(self))


_TARGET_NAMES = _TargetNames()


class _Assignments(ast.Visitor):
    def __init__(self, *, class_body=False):
        self.bindings = {}
        self.class_body = class_body

    def visit_Node(self, node):
        for child in node.children():
            child.accept(self)

    def visit_Expression(self, node):
        pass

    def visit_Comprehension(self, node):
        for clause in node.clauses:
            clause.iterable.accept(self)
            for name in clause.target.accept(_TARGET_NAMES):
                self.bindings.setdefault(name, []).append(clause.iterable)
        if node.key is not None:
            node.key.accept(self)
        node.element.accept(self)

    def visit_FunctionDefinition(self, node):
        if self.class_body:
            for statement in node.body:
                statement.accept(self)

    def visit_ClassDefinition(self, node):
        pass

    def visit_Assignment(self, node):
        node.value.accept(self)
        for target in node.targets:
            for name in target.accept(_TARGET_NAMES):
                self.bindings.setdefault(name, []).append(node.value)

    def visit_AnnotatedAssignment(self, node):
        if node.value is not None:
            node.value.accept(self)
            for name in node.target.accept(_TARGET_NAMES):
                self.bindings.setdefault(name, []).append(node.value)

    def visit_AugmentedAssignment(self, node):
        node.value.accept(self)
        for name in node.target.accept(_TARGET_NAMES):
            self.bindings.setdefault(name, []).append(node.value)

    def visit_For(self, node):
        node.iterable.accept(self)
        for name in node.target.accept(_TARGET_NAMES):
            self.bindings.setdefault(name, []).append(node.iterable)
        for statement in node.body:
            statement.accept(self)
        for statement in node.else_body:
            statement.accept(self)


class _ImportCall(ast.Visitor):
    def visit_Node(self, node):
        return False

    def visit_Name(self, node):
        return node.identifier in (
            "__import__",
            "import_module",
            "import_library",
            "import_string",
        )

    def visit_Attribute(self, node):
        return node.attribute in (
            "import_module",
            "import_library",
            "import_string",
        )


_IMPORT_CALL = _ImportCall()


class _Patterns(ast.Visitor):
    def __init__(self, scopes, active=()):
        self.scopes = scopes
        self.active = active

    def visit_Node(self, node):
        return set()

    def visit_TextLiteral(self, node):
        return {node.value}

    def lookup(self, identifier):
        if identifier in self.active:
            return set()
        found = set()
        for scope in reversed(self.scopes):
            if scope.get(identifier):
                visitor = _Patterns(self.scopes, (*self.active, identifier))
                found.update(*(value.accept(visitor) for value in scope[identifier]))
        return found

    def visit_Name(self, node):
        return self.lookup(node.identifier)

    def visit_Attribute(self, node):
        if node.value.binding_name() == "self":
            return self.lookup(node.attribute)
        return set()

    def visit_List(self, node):
        return set().union(*(element.accept(self) for element in node.elements))

    def visit_Tuple(self, node):
        return set().union(*(element.accept(self) for element in node.elements))

    def visit_Set(self, node):
        return set().union(*(element.accept(self) for element in node.elements))

    def visit_Dict(self, node):
        return set().union(*(entry.value.accept(self) for entry in node.entries))

    def visit_Call(self, node):
        if node.function.accept(_DICT_VALUES_CALL):
            return node.function.value.accept(self)
        return set()

    def visit_Conditional(self, node):
        return node.then.accept(self) | node.otherwise.accept(self)

    def visit_Binary(self, node):
        if node.operator is not ast.BinaryOperator.ADD:
            return set()
        left = node.left.accept(self)
        right = node.right.accept(self)
        if not left and not right:
            return set()
        if all(
            "." in item and not item.endswith(".") for item in (*left, *right) if item
        ):
            return left | right
        if right:
            return {prefix + suffix for prefix in left for suffix in right}
        return {prefix for prefix in left if prefix.endswith(".")}


class _DictValuesCall(ast.Visitor):
    def visit_Node(self, node):
        return False

    def visit_Attribute(self, node):
        return node.attribute in ("items", "values")


_DICT_VALUES_CALL = _DictValuesCall()


class ImportDependencies(ast.Visitor):
    def __init__(self):
        self.scopes = []
        self.patterns = set()

    def visit_Node(self, node):
        for child in node.children():
            child.accept(self)

    def scope(self, statements, parameters=(), *, class_body=False):
        assignments = _Assignments(class_body=class_body)
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
        self.scope(node.body, class_body=True)

    def add_patterns(self, items):
        for item in items:
            self.patterns.add(item)
            if "." in item and not item.endswith("."):
                self.patterns.add(item.rpartition(".")[0])

    def visit_Call(self, node):
        if node.function.accept(_IMPORT_CALL) and node.arguments:
            argument = node.arguments[0]
            if argument.kind is ast.ArgumentKind.POSITIONAL:
                self.add_patterns(argument.value.accept(_Patterns(self.scopes)))
        for argument in node.arguments:
            if argument.keyword in ("libraries", "builtins"):
                self.add_patterns(argument.value.accept(_Patterns(self.scopes)))
        self.visit_Node(node)


def discover(tree):
    visitor = ImportDependencies()
    tree.accept(visitor)
    return sorted(visitor.patterns)
