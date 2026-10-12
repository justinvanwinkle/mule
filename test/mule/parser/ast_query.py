"""Visitor-based queries and structural assertions for Mule syntax trees."""

from copy import deepcopy
from typing import Any

from mule import ast


class Query[N: ast.Node](ast.Visitor):
    def __init__(self, **fields: object):
        self.fields = fields
        self.matches: list[N] = []

    def collect(self, node: N):
        if all(getattr(node, key) == value for key, value in self.fields.items()):
            self.matches.append(node)


class Node(Query[ast.Node]):
    def visit_Node(self, node):
        self.collect(node)


class TypeIgnore(Query[ast.TypeIgnore]):
    def visit_TypeIgnore(self, node):
        self.collect(node)


class Module(Query[ast.Module]):
    def visit_Module(self, node):
        self.collect(node)


class ExpressionInput(Query[ast.ExpressionInput]):
    def visit_ExpressionInput(self, node):
        self.collect(node)


class InteractiveInput(Query[ast.InteractiveInput]):
    def visit_InteractiveInput(self, node):
        self.collect(node)


class Name(Query[ast.Name]):
    def visit_Name(self, node):
        self.collect(node)


class Literal(Query[ast.Literal]):
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


class IntegerLiteral(Query[ast.IntegerLiteral]):
    def visit_IntegerLiteral(self, node):
        self.collect(node)


class FloatLiteral(Query[ast.FloatLiteral]):
    def visit_FloatLiteral(self, node):
        self.collect(node)


class ComplexLiteral(Query[ast.ComplexLiteral]):
    def visit_ComplexLiteral(self, node):
        self.collect(node)


class BooleanLiteral(Query[ast.BooleanLiteral]):
    def visit_BooleanLiteral(self, node):
        self.collect(node)


class NoneLiteral(Query[ast.NoneLiteral]):
    def visit_NoneLiteral(self, node):
        self.collect(node)


class TextLiteral(Query[ast.TextLiteral]):
    def visit_TextLiteral(self, node):
        self.collect(node)


class BytesLiteral(Query[ast.BytesLiteral]):
    def visit_BytesLiteral(self, node):
        self.collect(node)


class EllipsisLiteral(Query[ast.EllipsisLiteral]):
    def visit_EllipsisLiteral(self, node):
        self.collect(node)


class Binary(Query[ast.Binary]):
    def visit_Binary(self, node):
        self.collect(node)


class Unary(Query[ast.Unary]):
    def visit_Unary(self, node):
        self.collect(node)


class Boolean(Query[ast.Boolean]):
    def visit_Boolean(self, node):
        self.collect(node)


class Comparison(Query[ast.Comparison]):
    def visit_Comparison(self, node):
        self.collect(node)


class Conditional(Query[ast.Conditional]):
    def visit_Conditional(self, node):
        self.collect(node)


class Parameter(Query[ast.Parameter]):
    def visit_Parameter(self, node):
        self.collect(node)


class Lambda(Query[ast.Lambda]):
    def visit_Lambda(self, node):
        self.collect(node)


class NamedExpression(Query[ast.NamedExpression]):
    def visit_NamedExpression(self, node):
        self.collect(node)


class Yield(Query[ast.Yield]):
    def visit_Yield(self, node):
        self.collect(node)


class Await(Query[ast.Await]):
    def visit_Await(self, node):
        self.collect(node)


class Attribute(Query[ast.Attribute]):
    def visit_Attribute(self, node):
        self.collect(node)


class CallArgument(Query[ast.CallArgument]):
    def visit_CallArgument(self, node):
        self.collect(node)


class Call(Query[ast.Call]):
    def visit_Call(self, node):
        self.collect(node)


class Subscript(Query[ast.Subscript]):
    def visit_Subscript(self, node):
        self.collect(node)


class Slice(Query[ast.Slice]):
    def visit_Slice(self, node):
        self.collect(node)


class Unpack(Query[ast.Unpack]):
    def visit_Unpack(self, node):
        self.collect(node)


class Tuple(Query[ast.Tuple]):
    def visit_Tuple(self, node):
        self.collect(node)


class List(Query[ast.List]):
    def visit_List(self, node):
        self.collect(node)


class Set(Query[ast.Set]):
    def visit_Set(self, node):
        self.collect(node)


class DictEntry(Query[ast.DictEntry]):
    def visit_DictEntry(self, node):
        self.collect(node)


class Dict(Query[ast.Dict]):
    def visit_Dict(self, node):
        self.collect(node)


class ComprehensionClause(Query[ast.ComprehensionClause]):
    def visit_ComprehensionClause(self, node):
        self.collect(node)


class Comprehension(Query[ast.Comprehension]):
    def visit_Comprehension(self, node):
        self.collect(node)


class Interpolation(Query[ast.Interpolation]):
    def visit_Interpolation(self, node):
        self.collect(node)


class FormattedString(Query[ast.FormattedString]):
    def visit_FormattedString(self, node):
        self.collect(node)


class TemplateString(Query[ast.TemplateString]):
    def visit_TemplateString(self, node):
        self.collect(node)


class ExpressionStatement(Query[ast.ExpressionStatement]):
    def visit_ExpressionStatement(self, node):
        self.collect(node)


class Assignment(Query[ast.Assignment]):
    def visit_Assignment(self, node):
        self.collect(node)


class AnnotatedAssignment(Query[ast.AnnotatedAssignment]):
    def visit_AnnotatedAssignment(self, node):
        self.collect(node)


class AugmentedAssignment(Query[ast.AugmentedAssignment]):
    def visit_AugmentedAssignment(self, node):
        self.collect(node)


class Delete(Query[ast.Delete]):
    def visit_Delete(self, node):
        self.collect(node)


class ImportItem(Query[ast.ImportItem]):
    def visit_ImportItem(self, node):
        self.collect(node)


class Import(Query[ast.Import]):
    def visit_Import(self, node):
        self.collect(node)


class ImportFrom(Query[ast.ImportFrom]):
    def visit_ImportFrom(self, node):
        self.collect(node)


class If(Query[ast.If]):
    def visit_If(self, node):
        self.collect(node)


class While(Query[ast.While]):
    def visit_While(self, node):
        self.collect(node)


class For(Query[ast.For]):
    def visit_For(self, node):
        self.collect(node)


class WithItem(Query[ast.WithItem]):
    def visit_WithItem(self, node):
        self.collect(node)


class With(Query[ast.With]):
    def visit_With(self, node):
        self.collect(node)


class ExceptionHandler(Query[ast.ExceptionHandler]):
    def visit_ExceptionHandler(self, node):
        self.collect(node)


class Try(Query[ast.Try]):
    def visit_Try(self, node):
        self.collect(node)


class TypeParameter(Query[ast.TypeParameter]):
    def visit_TypeParameter(self, node):
        self.collect(node)


class FunctionDefinition(Query[ast.FunctionDefinition]):
    def visit_FunctionDefinition(self, node):
        self.collect(node)


class ClassDefinition(Query[ast.ClassDefinition]):
    def visit_ClassDefinition(self, node):
        self.collect(node)


class TypeAlias(Query[ast.TypeAlias]):
    def visit_TypeAlias(self, node):
        self.collect(node)


class Pass(Query[ast.Pass]):
    def visit_Pass(self, node):
        self.collect(node)


class Break(Query[ast.Break]):
    def visit_Break(self, node):
        self.collect(node)


class Continue(Query[ast.Continue]):
    def visit_Continue(self, node):
        self.collect(node)


class Return(Query[ast.Return]):
    def visit_Return(self, node):
        self.collect(node)


class Raise(Query[ast.Raise]):
    def visit_Raise(self, node):
        self.collect(node)


class Assert(Query[ast.Assert]):
    def visit_Assert(self, node):
        self.collect(node)


class Global(Query[ast.Global]):
    def visit_Global(self, node):
        self.collect(node)


class Nonlocal(Query[ast.Nonlocal]):
    def visit_Nonlocal(self, node):
        self.collect(node)


class CapturePattern(Query[ast.CapturePattern]):
    def visit_CapturePattern(self, node):
        self.collect(node)


class LiteralPattern(Query[ast.LiteralPattern]):
    def visit_LiteralPattern(self, node):
        self.collect(node)


class ValuePattern(Query[ast.ValuePattern]):
    def visit_ValuePattern(self, node):
        self.collect(node)


class SequencePattern(Query[ast.SequencePattern]):
    def visit_SequencePattern(self, node):
        self.collect(node)


class StarPattern(Query[ast.StarPattern]):
    def visit_StarPattern(self, node):
        self.collect(node)


class PatternEntry(Query[ast.PatternEntry]):
    def visit_PatternEntry(self, node):
        self.collect(node)


class MappingPattern(Query[ast.MappingPattern]):
    def visit_MappingPattern(self, node):
        self.collect(node)


class KeywordPattern(Query[ast.KeywordPattern]):
    def visit_KeywordPattern(self, node):
        self.collect(node)


class ClassPattern(Query[ast.ClassPattern]):
    def visit_ClassPattern(self, node):
        self.collect(node)


class OrPattern(Query[ast.OrPattern]):
    def visit_OrPattern(self, node):
        self.collect(node)


class AsPattern(Query[ast.AsPattern]):
    def visit_AsPattern(self, node):
        self.collect(node)


class MatchCase(Query[ast.MatchCase]):
    def visit_MatchCase(self, node):
        self.collect(node)


class Match(Query[ast.Match]):
    def visit_Match(self, node):
        self.collect(node)


def nodes[N: ast.Node](
    tree: ast.Node, selector: type[Query[N]], **fields: object
) -> list[N]:
    visitor = selector(**fields)
    for node in ast.walk(tree):
        node.accept(visitor)
    return visitor.matches


def one[N: ast.Node](tree: ast.Node, selector: type[Query[N]], **fields: object) -> N:
    matches = nodes(tree, selector, **fields)
    assert len(matches) == 1, (
        f"Expected one matching node for {fields!r}; found {len(matches)}"
    )
    return matches[0]


def expect[N: ast.Node](value: object, kind: type[N]) -> N:
    """Return value after asserting it is the syntax node class a test expects."""
    assert isinstance(value, kind), (
        f"Expected {kind.__name__}; found {type(value).__name__}"
    )
    return value


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
