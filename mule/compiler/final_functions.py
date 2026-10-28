"""Source declarations for immutable module function bindings.

The declaration follows an unshadowed import, rather than a decorator's
spelling. Runtime decorator evaluation remains the frontend's responsibility.
"""

from mule import ast


class FinalFunctions(ast.Visitor):
    def __init__(self, binding_counts):
        self.binding_counts = binding_counts
        self.imports = {}
        self.definitions = {}
        self.decorators = {}
        self.depth = 0
        self.qualified_mutation = False
        self.source_nodes = set()

    @classmethod
    def scan(cls, statements, binding_counts):
        declarations = cls(binding_counts)
        sources = _SourceNodes()
        for statement in statements:
            statement.accept(sources)
        declarations.source_nodes = sources.nodes
        mutations = _FinalAttributeMutations()
        for statement in statements:
            statement.accept(mutations)
        declarations.qualified_mutation = mutations.found
        for statement in statements:
            statement.accept(declarations)
        return declarations

    def visit_Node(self, node):
        self.depth += 1
        for child in node.children():
            child.accept(self)
        self.depth -= 1

    def visit_Import(self, node):
        if self.depth or "*" in self.binding_counts:
            return
        for item in node.items:
            name = item.alias or item.name
            if (
                item.name == "typing"
                and not self.qualified_mutation
                and self.binding_counts.get(name) == 1
            ):
                self.imports[name] = "typing"

    def visit_ImportFrom(self, node):
        if (
            self.depth
            or "*" in self.binding_counts
            or self.qualified_mutation
            or node.level
            or node.module != "typing"
        ):
            return
        for item in node.items:
            name = item.alias or item.name
            if item.name == "final" and self.binding_counts.get(name) == 1:
                self.imports[name] = "final"

    def visit_FunctionDefinition(self, node):
        declarations = tuple(
            decorator
            for decorator in node.decorators
            if decorator.accept(_FinalReference(self.imports))
        )
        if declarations:
            previous = self.definitions.get(node.name)
            if previous is not None:
                # Source validation reports the extra definition at its store.
                return
            self.definitions[node.name] = node
            self.decorators[id(node)] = declarations

    def visit_ClassDefinition(self, node):
        pass

    def declares(self, definition):
        return self.definitions.get(definition.name) is definition

    def steps(self, definition):
        return self.decorators.get(id(definition), ())

    def allows_store(self, name, source):
        definition = self.definitions.get(name)
        return (
            definition is None
            or definition is source
            or id(source) not in self.source_nodes
        )


class _SourceNodes(ast.Visitor):
    """Distinguish source writes from generated namespace initialization."""

    def __init__(self):
        self.nodes = set()

    def visit_Node(self, node):
        self.nodes.add(id(node))
        for child in node.children():
            child.accept(self)


class _FinalReference(ast.Visitor):
    def __init__(self, imports):
        self.imports = imports

    def visit_Node(self, node):
        return False

    def visit_Name(self, node):
        return self.imports.get(node.identifier) == "final"

    def visit_Attribute(self, node):
        return node.attribute == "final" and node.value.accept(
            _TypingReference(self.imports)
        )


class _TypingReference(ast.Visitor):
    def __init__(self, imports):
        self.imports = imports

    def visit_Node(self, node):
        return False

    def visit_Name(self, node):
        return self.imports.get(node.identifier) == "typing"


class _FinalAttributeMutations(ast.Visitor):
    """Track writes through imported typing modules and their source aliases."""

    def __init__(self):
        self.found = False
        self.aliases = {}

    def visit_Node(self, node):
        for target in node.assigned_targets():
            target.accept(_FinalAttributeTarget(self))
        for child in node.children():
            child.accept(self)

    def visit_Import(self, node):
        for item in node.items:
            name = item.alias or item.name.split(".")[0]
            self.aliases.pop(name, None)
            if item.name == "typing":
                self.aliases[name] = "typing"

    def visit_ImportFrom(self, node):
        for item in node.items:
            self.aliases.pop(item.alias or item.name, None)

    def visit_Assignment(self, node):
        node.value.accept(self)
        module_alias = node.value.accept(_TypingReference(self.aliases))
        for target in node.targets:
            target.accept(_FinalAttributeTarget(self))
            target.accept(_TypingAliasTarget(self.aliases, module_alias))

    def visit_AnnotatedAssignment(self, node):
        if node.value is not None:
            node.value.accept(self)
        module_alias = node.value is not None and node.value.accept(
            _TypingReference(self.aliases)
        )
        node.target.accept(_FinalAttributeTarget(self))
        node.target.accept(_TypingAliasTarget(self.aliases, module_alias))

    def visit_FunctionDefinition(self, node):
        for decorator in node.decorators:
            decorator.accept(self)
        outer = self.aliases
        self.aliases = outer.copy()
        for parameter in node.parameters:
            self.aliases.pop(parameter.name, None)
        for statement in node.body:
            statement.accept(self)
        self.aliases = outer
        self.aliases.pop(node.name, None)

    def visit_ClassDefinition(self, node):
        outer = self.aliases
        self.aliases = outer.copy()
        for statement in node.body:
            statement.accept(self)
        self.aliases = outer
        self.aliases.pop(node.name, None)

    def visit_Call(self, node):
        if (
            node.function.accept(_AttributeMutationFunction())
            and len(node.arguments) >= 2
            and node.arguments[0].value.accept(_TypingReference(self.aliases))
            and node.arguments[1].value.accept(_MayNameFinal())
        ):
            self.found = True
        self.visit_Node(node)

    def visit_Delete(self, node):
        for target in node.targets:
            target.accept(_FinalAttributeTarget(self))
        self.visit_Node(node)


class _FinalAttributeTarget(ast.Visitor):
    def __init__(self, mutations):
        self.mutations = mutations

    def visit_Node(self, node):
        for child in node.children():
            child.accept(self)

    def visit_Attribute(self, node):
        self.mutations.found |= node.attribute == "final" and node.value.accept(
            _TypingReference(self.mutations.aliases)
        )


class _TypingAliasTarget(ast.Visitor):
    def __init__(self, aliases, module_alias):
        self.aliases = aliases
        self.module_alias = module_alias

    def visit_Node(self, node):
        for child in node.children():
            child.accept(self)

    def visit_Name(self, node):
        self.aliases.pop(node.identifier, None)
        if self.module_alias:
            self.aliases[node.identifier] = "typing"

    def visit_Attribute(self, node):
        pass

    def visit_Subscript(self, node):
        pass


class _AttributeMutationFunction(ast.Visitor):
    def visit_Node(self, node):
        return False

    def visit_Name(self, node):
        return node.identifier in ("setattr", "delattr")


class _MayNameFinal(ast.Visitor):
    def visit_Node(self, node):
        return True

    def visit_TextLiteral(self, node):
        return node.value == "final"
