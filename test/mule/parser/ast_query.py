"""Visitor-based queries and structural assertions for Mule syntax trees."""

from copy import deepcopy
from typing import Any

from mule import ast


class Query(ast.Visitor):
    def __init__(self, **fields: object):
        self.fields = fields
        self.matches = []

    def collect(self, node):
        if all(getattr(node, key) == value for key, value in self.fields.items()):
            self.matches.append(node)


class Node(Query):
    def visit_Node(self, node):
        self.collect(node)


class TypeIgnore(Query):
    def visit_TypeIgnore(self, node):
        self.collect(node)


class Module(Query):
    def visit_Module(self, node):
        self.collect(node)


class ExpressionInput(Query):
    def visit_ExpressionInput(self, node):
        self.collect(node)


class InteractiveInput(Query):
    def visit_InteractiveInput(self, node):
        self.collect(node)


class Name(Query):
    def visit_Name(self, node):
        self.collect(node)


class Literal(Query):
    def visit_Literal(self, node):
        self.collect(node)

    def visit_IntegerLiteral(self, node):
        self.collect(node)

    def visit_FloatLiteral(self, node):
        self.collect(node)

    def visit_ComplexLiteral(self, node):
        self.collect(node)

    def visit_BooleanLiteral(self, node):
        self.collect(node)

    def visit_NoneLiteral(self, node):
        self.collect(node)

    def visit_TextLiteral(self, node):
        self.collect(node)

    def visit_BytesLiteral(self, node):
        self.collect(node)

    def visit_EllipsisLiteral(self, node):
        self.collect(node)


class IntegerLiteral(Query):
    def visit_IntegerLiteral(self, node):
        self.collect(node)


class FloatLiteral(Query):
    def visit_FloatLiteral(self, node):
        self.collect(node)


class ComplexLiteral(Query):
    def visit_ComplexLiteral(self, node):
        self.collect(node)


class BooleanLiteral(Query):
    def visit_BooleanLiteral(self, node):
        self.collect(node)


class NoneLiteral(Query):
    def visit_NoneLiteral(self, node):
        self.collect(node)


class TextLiteral(Query):
    def visit_TextLiteral(self, node):
        self.collect(node)


class BytesLiteral(Query):
    def visit_BytesLiteral(self, node):
        self.collect(node)


class EllipsisLiteral(Query):
    def visit_EllipsisLiteral(self, node):
        self.collect(node)


class Binary(Query):
    def visit_Binary(self, node):
        self.collect(node)


class Unary(Query):
    def visit_Unary(self, node):
        self.collect(node)


class Boolean(Query):
    def visit_Boolean(self, node):
        self.collect(node)


class Comparison(Query):
    def visit_Comparison(self, node):
        self.collect(node)


class Conditional(Query):
    def visit_Conditional(self, node):
        self.collect(node)


class Parameter(Query):
    def visit_Parameter(self, node):
        self.collect(node)


class Lambda(Query):
    def visit_Lambda(self, node):
        self.collect(node)


class NamedExpression(Query):
    def visit_NamedExpression(self, node):
        self.collect(node)


class Yield(Query):
    def visit_Yield(self, node):
        self.collect(node)


class Await(Query):
    def visit_Await(self, node):
        self.collect(node)


class Attribute(Query):
    def visit_Attribute(self, node):
        self.collect(node)


class CallArgument(Query):
    def visit_CallArgument(self, node):
        self.collect(node)


class Call(Query):
    def visit_Call(self, node):
        self.collect(node)


class Subscript(Query):
    def visit_Subscript(self, node):
        self.collect(node)


class Slice(Query):
    def visit_Slice(self, node):
        self.collect(node)


class Unpack(Query):
    def visit_Unpack(self, node):
        self.collect(node)


class Tuple(Query):
    def visit_Tuple(self, node):
        self.collect(node)


class List(Query):
    def visit_List(self, node):
        self.collect(node)


class Set(Query):
    def visit_Set(self, node):
        self.collect(node)


class DictEntry(Query):
    def visit_DictEntry(self, node):
        self.collect(node)


class Dict(Query):
    def visit_Dict(self, node):
        self.collect(node)


class ComprehensionClause(Query):
    def visit_ComprehensionClause(self, node):
        self.collect(node)


class Comprehension(Query):
    def visit_Comprehension(self, node):
        self.collect(node)


class Interpolation(Query):
    def visit_Interpolation(self, node):
        self.collect(node)


class FormattedString(Query):
    def visit_FormattedString(self, node):
        self.collect(node)


class TemplateString(Query):
    def visit_TemplateString(self, node):
        self.collect(node)


class ExpressionStatement(Query):
    def visit_ExpressionStatement(self, node):
        self.collect(node)


class Assignment(Query):
    def visit_Assignment(self, node):
        self.collect(node)


class AnnotatedAssignment(Query):
    def visit_AnnotatedAssignment(self, node):
        self.collect(node)


class AugmentedAssignment(Query):
    def visit_AugmentedAssignment(self, node):
        self.collect(node)


class Delete(Query):
    def visit_Delete(self, node):
        self.collect(node)


class ImportItem(Query):
    def visit_ImportItem(self, node):
        self.collect(node)


class Import(Query):
    def visit_Import(self, node):
        self.collect(node)


class ImportFrom(Query):
    def visit_ImportFrom(self, node):
        self.collect(node)


class If(Query):
    def visit_If(self, node):
        self.collect(node)


class While(Query):
    def visit_While(self, node):
        self.collect(node)


class For(Query):
    def visit_For(self, node):
        self.collect(node)


class WithItem(Query):
    def visit_WithItem(self, node):
        self.collect(node)


class With(Query):
    def visit_With(self, node):
        self.collect(node)


class ExceptionHandler(Query):
    def visit_ExceptionHandler(self, node):
        self.collect(node)


class Try(Query):
    def visit_Try(self, node):
        self.collect(node)


class TypeParameter(Query):
    def visit_TypeParameter(self, node):
        self.collect(node)


class FunctionDefinition(Query):
    def visit_FunctionDefinition(self, node):
        self.collect(node)


class ClassDefinition(Query):
    def visit_ClassDefinition(self, node):
        self.collect(node)


class TypeAlias(Query):
    def visit_TypeAlias(self, node):
        self.collect(node)


class Pass(Query):
    def visit_Pass(self, node):
        self.collect(node)


class Break(Query):
    def visit_Break(self, node):
        self.collect(node)


class Continue(Query):
    def visit_Continue(self, node):
        self.collect(node)


class Return(Query):
    def visit_Return(self, node):
        self.collect(node)


class Raise(Query):
    def visit_Raise(self, node):
        self.collect(node)


class Assert(Query):
    def visit_Assert(self, node):
        self.collect(node)


class Global(Query):
    def visit_Global(self, node):
        self.collect(node)


class Nonlocal(Query):
    def visit_Nonlocal(self, node):
        self.collect(node)


class CapturePattern(Query):
    def visit_CapturePattern(self, node):
        self.collect(node)


class LiteralPattern(Query):
    def visit_LiteralPattern(self, node):
        self.collect(node)


class ValuePattern(Query):
    def visit_ValuePattern(self, node):
        self.collect(node)


class SequencePattern(Query):
    def visit_SequencePattern(self, node):
        self.collect(node)


class StarPattern(Query):
    def visit_StarPattern(self, node):
        self.collect(node)


class PatternEntry(Query):
    def visit_PatternEntry(self, node):
        self.collect(node)


class MappingPattern(Query):
    def visit_MappingPattern(self, node):
        self.collect(node)


class KeywordPattern(Query):
    def visit_KeywordPattern(self, node):
        self.collect(node)


class ClassPattern(Query):
    def visit_ClassPattern(self, node):
        self.collect(node)


class OrPattern(Query):
    def visit_OrPattern(self, node):
        self.collect(node)


class AsPattern(Query):
    def visit_AsPattern(self, node):
        self.collect(node)


class MatchCase(Query):
    def visit_MatchCase(self, node):
        self.collect(node)


class Match(Query):
    def visit_Match(self, node):
        self.collect(node)


def nodes(tree: ast.Node, selector: type[Query], **fields: object) -> list[ast.Node]:
    visitor = selector(**fields)
    for node in ast.walk(tree):
        node.accept(visitor)
    return visitor.matches


def one(tree: ast.Node, selector: type[Query], **fields: object) -> ast.Node:
    matches = nodes(tree, selector, **fields)
    assert len(matches) == 1, (
        f"Expected one matching node for {fields!r}; found {len(matches)}"
    )
    return matches[0]


def at(tree: ast.Node, path: str) -> Any:
    """Follow fields and list indices, e.g. 'statements.0.value.left'."""
    value: Any = tree
    for segment in path.split("."):
        value = value[int(segment)] if segment.isdecimal() else getattr(value, segment)
    return value


def assert_ast_equal(
    actual: ast.Node, expected: ast.Node, *, locations: bool = False
) -> None:
    if not locations:
        actual = deepcopy(actual)
        expected = deepcopy(expected)
        for tree in (actual, expected):
            for node in ast.walk(tree):
                node.span = None
    assert actual == expected
