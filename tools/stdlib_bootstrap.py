"""Discover the explicit unittest subset executable before the full loader works."""

from dataclasses import dataclass

from mule import ast
from mule.parser import parse


class NeedsFullLoader(Exception):
    pass


class Reference(ast.Visitor):
    def visit_Name(self, node):
        return node.identifier

    def visit_Attribute(self, node):
        base = node.value.accept(self)
        return base + "." + node.attribute if base is not None else None


class MainGuard(ast.Visitor):
    def visit_Comparison(self, node):
        return (
            node.left.accept(Reference()) == "__name__"
            and node.operators == [ast.ComparisonOperator.EQUAL]
            and len(node.comparators) == 1
            and node.comparators[0].accept(MainName())
        )


class MainName(ast.Visitor):
    def visit_TextLiteral(self, node):
        return node.value == "__main__"


class Docstring(ast.Visitor):
    def visit_TextLiteral(self, node):
        return True


@dataclass(frozen=True)
class Case:
    class_name: str
    method_name: str

    @property
    def name(self):
        return self.class_name + "." + self.method_name


class Cases(ast.Visitor):
    def __init__(self):
        self.testcase_bindings = set()
        self.cases = {}

    def visit_Node(self, node):
        raise NeedsFullLoader

    def visit_Module(self, node):
        for statement in node.statements:
            statement.accept(self)
        return tuple(sorted(self.cases.values(), key=lambda case: case.name))

    def visit_ExpressionStatement(self, node):
        if not node.expression.accept(Docstring()):
            raise NeedsFullLoader

    def visit_Import(self, node):
        for item in node.items:
            if item.name != "unittest":
                raise NeedsFullLoader
            self.testcase_bindings.add((item.alias or item.name) + ".TestCase")

    def visit_ImportFrom(self, node):
        if node.module != "unittest" or node.level:
            raise NeedsFullLoader
        for item in node.items:
            if item.name != "TestCase":
                raise NeedsFullLoader
            self.testcase_bindings.add(item.alias or item.name)

    def visit_If(self, node):
        if not node.condition.accept(MainGuard()) or node.else_body:
            raise NeedsFullLoader

    def visit_ClassDefinition(self, node):
        if node.decorators or len(node.arguments) != 1:
            raise NeedsFullLoader
        base = node.arguments[0]
        if (
            base.kind != ast.ArgumentKind.POSITIONAL
            or base.value.accept(Reference()) not in self.testcase_bindings
        ):
            raise NeedsFullLoader
        # Repeated definitions follow last-binding-wins, like module execution.
        self.cases = {
            name: case
            for name, case in self.cases.items()
            if case.class_name != node.name
        }
        node_body = Methods(node.name)
        for statement in node.body:
            statement.accept(node_body)
        self.cases.update({case.name: case for case in node_body.cases.values()})


class Methods(ast.Visitor):
    def __init__(self, class_name):
        self.class_name = class_name
        self.cases = {}

    def visit_Node(self, node):
        raise NeedsFullLoader

    def visit_Pass(self, node):
        pass

    def visit_ExpressionStatement(self, node):
        if not node.expression.accept(Docstring()):
            raise NeedsFullLoader

    def visit_FunctionDefinition(self, node):
        if node.decorators or node.is_async or len(node.parameters) != 1:
            raise NeedsFullLoader
        if node.name in (
            "setUp",
            "tearDown",
            "setUpClass",
            "tearDownClass",
            "__init__",
        ):
            raise NeedsFullLoader
        if node.name.startswith("test"):
            self.cases[node.name] = Case(self.class_name, node.name)


def discover_cases(source, filename):
    try:
        cases = parse(source, filename=filename).accept(Cases())
    except NeedsFullLoader, SyntaxError:
        return None
    return cases or None


def driver_source(module_name, cases):
    source = f"import {module_name} as tests\nselected = input()\nmatched = False\n"
    for case in cases:
        source += (
            f"if selected == {case.name!r}:\n"
            "    matched = True\n"
            f"    instance = tests.{case.class_name}()\n"
            f"    print({'MULE_TEST_START ' + case.name!r})\n"
            f"    instance.{case.method_name}()\n"
            f"    print({'MULE_TEST_PASS ' + case.name!r})\n"
        )
    return source + "assert matched, 'unknown test selection'\n"
