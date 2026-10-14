"""Interface declarations and their imported abstract-decorator bindings.

The declaration is retained separately from the implementation's inferred
result. This analysis never supplies a result type to local-call propagation.
"""

from mule import ast


class _Binding:
    def abstract_decorator(self):
        return False

    def member(self, name):
        return _UNKNOWN


class _AbstractDecorator(_Binding):
    def abstract_decorator(self):
        return True


class _AbcModule(_Binding):
    def member(self, name):
        return _ABC_EXPORTS.get(name, _UNKNOWN)


_UNKNOWN = _Binding()
_ABSTRACT = _AbstractDecorator()
_ABC = _AbcModule()
_ABC_EXPORTS = dict.fromkeys(
    (
        "abstractmethod",
        "abstractclassmethod",
        "abstractstaticmethod",
        "abstractproperty",
    ),
    _ABSTRACT,
)


def imported_bindings(statements):
    visitor = _Imports()
    for statement in statements:
        statement.accept(visitor)
    if "*" in visitor.shadowed:
        return {}
    return {
        name: binding
        for name, binding in visitor.bindings.items()
        if name not in visitor.shadowed
    }


class _Imports(ast.Visitor):
    """Conservative module imports: rebinding prevents special recognition."""

    def __init__(self):
        self.bindings = {}
        self.shadowed = set()

    def bind(self, name, binding):
        previous = self.bindings.get(name)
        if previous is not None and previous is not binding:
            self.shadowed.add(name)
        self.bindings[name] = binding

    def visit_Import(self, node):
        for item in node.items:
            self.bind(
                item.alias or item.name.split(".")[0],
                _ABC if item.name == "abc" else _UNKNOWN,
            )

    def visit_ImportFrom(self, node):
        for item in node.items:
            if item.name == "*":
                self.shadowed.add("*")
            else:
                self.bind(
                    item.alias or item.name,
                    _ABC.member(item.name)
                    if node.module == "abc" and not node.level
                    else _UNKNOWN,
                )

    def visit_Node(self, node):
        for target in node.assigned_targets():
            target.accept(_Shadows(self.shadowed))
        for child in node.children():
            child.accept(self)

    def visit_FunctionDefinition(self, node):
        self.shadowed.add(node.name)

    def visit_ClassDefinition(self, node):
        self.shadowed.add(node.name)

    def visit_Lambda(self, node):
        pass


class _Shadows(ast.Visitor):
    def __init__(self, names):
        self.names = names

    def visit_Node(self, node):
        for child in node.children():
            child.accept(self)

    def visit_Name(self, node):
        self.names.add(node.identifier)


class _Reference(ast.Visitor):
    def __init__(self, bindings, parent):
        self.bindings = bindings
        self.parent = parent

    def visit_Node(self, node):
        return _UNKNOWN

    def visit_Name(self, node):
        if self.parent is not None and (
            self.parent.local(node.identifier)
            or self.parent.enclosing_local(node.identifier) is not None
            or self.parent.class_binding(node.identifier) is not None
        ):
            return _UNKNOWN
        return self.bindings.get(node.identifier, _UNKNOWN)

    def visit_Attribute(self, node):
        return node.value.accept(self).member(node.attribute)


class _EllipsisBody(ast.Visitor):
    def visit_Node(self, node):
        return False

    def visit_ExpressionStatement(self, node):
        return node.expression.accept(self)

    def visit_EllipsisLiteral(self, node):
        return True


def declaration(converter, definition, parent):
    from .ir1tran import _Docstring

    body = definition.body
    if body and body[0].accept(_Docstring()):
        body = body[1:]
    if len(body) == 1 and body[0].accept(_EllipsisBody()):
        return True
    references = _Reference(converter.interface_bindings, parent)
    return any(
        decorator.accept(references).abstract_decorator()
        for decorator in definition.decorators
    )
