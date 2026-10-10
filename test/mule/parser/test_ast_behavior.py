"""Node behavior owns traversal, literal dispatch, and syntactic target roles."""

import pytest

from mule import ast


@pytest.mark.parametrize(
    ("source", "constructor", "value"),
    [
        ("12", ast.IntegerLiteral, 12),
        ("1.5", ast.FloatLiteral, 1.5),
        ("2j", ast.ComplexLiteral, 2j),
        ("True", ast.BooleanLiteral, True),
        ("False", ast.BooleanLiteral, False),
        ("None", ast.NoneLiteral, None),
        ("'text'", ast.TextLiteral, "text"),
        ("b'bytes'", ast.BytesLiteral, b"bytes"),
        ("...", ast.EllipsisLiteral, Ellipsis),
    ],
)
def test_parser_selects_literal_class_at_its_grammar_boundary(
    parse, source, constructor, value
):
    expression = parse(source, mode="eval").expression
    assert type(expression) is constructor
    assert expression.value == value
    assert list(expression.children()) == []


def test_node_accept_uses_overridden_concrete_visit_and_explicit_fallback():
    class Visitor(ast.Visitor):
        def visit_Node(self, node):
            return "fallback"

        def visit_Name(self, node):
            return node.identifier

        def visit_IntegerLiteral(self, node):
            return node.value + 1

    visitor = Visitor()
    assert ast.Name("variable").accept(visitor) == "variable"
    assert ast.IntegerLiteral(7).accept(visitor) == 8
    assert ast.FloatLiteral(1.0).accept(visitor) == "fallback"
    assert ast.Literal(7).accept(visitor) == "fallback"
    assert ast.Node().accept(visitor) == "fallback"


def test_literal_subclasses_dispatch_independently_of_base_literal_visit():
    class Visitor(ast.Visitor):
        def visit_Node(self, node):
            return "other"

        def visit_Literal(self, node):
            return "base"

        def visit_BooleanLiteral(self, node):
            return "boolean"

    visitor = Visitor()
    assert ast.Literal(True).accept(visitor) == "base"
    assert ast.IntegerLiteral(1).accept(visitor) == "other"
    assert ast.BooleanLiteral(True).accept(visitor) == "boolean"


def test_assignment_owners_expose_targets_without_treating_reads_as_bindings(parse):
    tree = parse(
        "a = (b := value)\nfor c in items:\n    del d\nwith manager as e:\n    pass\n"
    )
    targets = [target for node in ast.walk(tree) for target in node.assigned_targets()]
    assert [target.binding_name() for target in targets] == ["a", "b", "c", "d", "e"]
    assert ast.Name("read").assigned_targets() == ()
    assert ast.Name("read").binding_name() == "read"
    assert ast.Attribute(ast.Name("obj"), "field").binding_name() is None


def test_comprehension_and_annotation_expose_only_syntactic_targets(parse):
    tree = parse("result: list = [value for value in items]\nresult += extra\n")
    targets = [target for node in ast.walk(tree) for target in node.assigned_targets()]
    assert [target.binding_name() for target in targets] == [
        "result",
        "value",
        "result",
    ]


def test_custom_node_controls_traversal_without_dataclass_field_inspection():
    class Children(ast.Node):
        def children(self):
            yield ast.Name("owned")

    tree = Children()
    assert [child.identifier for child in ast.children(tree)] == ["owned"]
    assert [node.binding_name() for node in ast.walk(tree)] == [None, "owned"]


def test_custom_expression_can_own_target_validation():
    from mule.parser.core import Parser

    class Target(ast.Expression):
        def validate_target(self, parser, *, delete=False):
            parser.validated = delete

    parser = Parser("")
    parser.validate_target(Target(), delete=True)
    assert parser.validated is True


def test_literal_singleton_constructors_do_not_need_raw_value_arguments():
    assert ast.NoneLiteral().value is None
    assert ast.EllipsisLiteral().value is Ellipsis


def test_position_and_token_supply_start_positions_without_runtime_type_dispatch():
    from mule.parser.lexer import Token

    position = ast.Position(3, 4)
    token = Token("NAME", "name", position, ast.Position(3, 8))
    assert position.source_start() is position
    assert token.source_start() is position
