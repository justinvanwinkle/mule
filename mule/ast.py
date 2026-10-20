"""Mule's syntax tree, independent of the host interpreter's parser.

Targets use the same expression nodes as values; their role follows from the
owning statement or comprehension clause. Scope validation belongs to the
compiler. Source columns count Unicode characters, starting at zero. Lines
start at one, and spans exclude their end position.
"""

from collections.abc import Iterator
from dataclasses import dataclass, field
from enum import StrEnum
from types import EllipsisType
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from mule.parser.core import Parser


@dataclass(frozen=True, slots=True)
class Position:
    line: int
    column: int

    def source_start(self) -> Position:
        return self


@dataclass(frozen=True, slots=True)
class Span:
    start: Position
    end: Position


@dataclass(slots=True, kw_only=True)
class Node:
    span: Span | None = None

    def source_start(self) -> Position:
        """Where the node starts in its source; a parsed node always has one."""
        return self._source_span().start

    def source_end(self) -> Position:
        return self._source_span().end

    def _source_span(self) -> Span:
        if self.span is None:
            raise ValueError(f"{type(self).__name__} node has no source span")
        return self.span

    def children(self) -> Iterator[Node]:
        return iter(())

    def accept(self, visitor):
        return visitor.visit_Node(self)

    def consume_signature_comment(self, parser):
        pass

    def assigned_targets(self) -> tuple[Expression, ...]:
        return ()

    def binding_name(self) -> str | None:
        return None

    def append_string_part(self, parts):
        parts.append(self)

    def merge_string_fragment(self, parts, fragment):
        parts.append(fragment)


class Expression(Node):
    __slots__ = ()

    def validate_target(self, parser, *, delete=False):
        parser.error(
            "Invalid deletion target" if delete else "Invalid assignment target"
        )

    def validate_single_target(self, parser, message, start):
        parser.error(message, start)

    def validate_named_target(self, parser: Parser) -> Name:
        """The target of ``target := value``, which must be a name."""
        parser.error("Invalid assignment expression target or context")

    def require_collection(self, parser, message, start=None):
        pass

    def subscript_item(self, parser, start):
        return self

    def delete_operands(self, *, grouped):
        return [self]

    def type_parameter_bound(self):
        return self, []

    def validate_exception_binding(self, parser, *, grouped):
        pass

    def append_boolean(self, operator, right, *, grouped):
        return Boolean(
            operator, [self, right], span=Span(self.source_start(), right.source_end())
        )

    def append_comparison(self, operator, right, *, grouped):
        return Comparison(
            self,
            [operator],
            [right],
            span=Span(self.source_start(), right.source_end()),
        )

    def pattern_reference(self):
        return ValuePattern(self, span=self.span)

    def mapping_pattern_key(self, parser):
        parser.error("mapping pattern key requires a literal or qualified name")

    def validate_pattern_literal(self, parser):
        parser.error("pattern literal cannot be an interpolated string")

    def validate_numeric_pattern(self, parser):
        parser.error("expected a numeric pattern literal")

    def validate_real_pattern(self, parser):
        parser.error("complex pattern literal requires a real left operand")

    def validate_imaginary_pattern(self, parser):
        parser.error("complex pattern literal requires an imaginary operand")

    def literal_pattern_operator(self) -> ComparisonOperator:
        """How a literal pattern compares its subject with this value."""
        return ComparisonOperator.EQUAL


class Statement(Node):
    __slots__ = ()


class Pattern(Node):
    __slots__ = ()

    def validate_unsequenced(self, parser):
        pass


class BinaryOperator(StrEnum):
    ADD = "+"
    SUBTRACT = "-"
    MULTIPLY = "*"
    DIVIDE = "/"
    FLOOR_DIVIDE = "//"
    MODULO = "%"
    MATRIX_MULTIPLY = "@"
    POWER = "**"
    LEFT_SHIFT = "<<"
    RIGHT_SHIFT = ">>"
    BIT_AND = "&"
    BIT_XOR = "^"
    BIT_OR = "|"


class UnaryOperator(StrEnum):
    POSITIVE = "+"
    NEGATIVE = "-"
    INVERT = "~"
    NOT = "not"


class BooleanOperator(StrEnum):
    AND = "and"
    OR = "or"


class ComparisonOperator(StrEnum):
    EQUAL = "=="
    NOT_EQUAL = "!="
    LESS = "<"
    LESS_EQUAL = "<="
    GREATER = ">"
    GREATER_EQUAL = ">="
    IS = "is"
    IS_NOT = "is not"
    IN = "in"
    NOT_IN = "not in"


class ArgumentKind(StrEnum):
    POSITIONAL = "positional"
    KEYWORD = "keyword"
    STARRED = "starred"
    DOUBLE_STARRED = "double_starred"


class ParameterKind(StrEnum):
    POSITIONAL_ONLY = "positional_only"
    POSITIONAL_OR_KEYWORD = "positional_or_keyword"
    VAR_POSITIONAL = "var_positional"
    KEYWORD_ONLY = "keyword_only"
    VAR_KEYWORD = "var_keyword"


class ComprehensionKind(StrEnum):
    LIST = "list"
    SET = "set"
    DICT = "dict"
    GENERATOR = "generator"


class TypeParameterKind(StrEnum):
    TYPE_VAR = "type_var"
    TYPE_VAR_TUPLE = "type_var_tuple"
    PARAM_SPEC = "param_spec"


@dataclass(slots=True)
class TypeIgnore(Node):
    line: int
    tag: str = ""

    def children(self) -> Iterator[Node]:
        return iter(())

    def accept(self, visitor):
        return visitor.visit_TypeIgnore(self)


@dataclass(slots=True)
class Module(Node):
    statements: list[Statement] = field(default_factory=list)
    type_ignores: list[TypeIgnore] = field(default_factory=list)

    def children(self) -> Iterator[Node]:
        yield from self.statements
        yield from self.type_ignores

    def accept(self, visitor):
        return visitor.visit_Module(self)


@dataclass(slots=True)
class ExpressionInput(Node):
    expression: Expression

    def children(self) -> Iterator[Node]:
        yield self.expression

    def accept(self, visitor):
        return visitor.visit_ExpressionInput(self)


@dataclass(slots=True)
class InteractiveInput(Node):
    statements: list[Statement] = field(default_factory=list)

    def children(self) -> Iterator[Node]:
        yield from self.statements

    def accept(self, visitor):
        return visitor.visit_InteractiveInput(self)


@dataclass(slots=True)
class Name(Expression):
    identifier: str

    def children(self) -> Iterator[Node]:
        return iter(())

    def accept(self, visitor):
        return visitor.visit_Name(self)

    def binding_name(self):
        return self.identifier

    def validate_target(self, parser, *, delete=False):
        pass

    def validate_single_target(self, parser, message, start):
        pass

    def validate_named_target(self, parser) -> Name:
        return self

    def pattern_reference(self):
        return CapturePattern(
            None if self.identifier == "_" else self.identifier, span=self.span
        )


@dataclass(slots=True)
class Literal(Expression):
    value: Any

    def children(self) -> Iterator[Node]:
        return iter(())

    def accept(self, visitor):
        return visitor.visit_Literal(self)


@dataclass(slots=True)
class IntegerLiteral(Literal):
    value: int

    def children(self) -> Iterator[Node]:
        return iter(())

    def accept(self, visitor):
        return visitor.visit_IntegerLiteral(self)

    def validate_numeric_pattern(self, parser):
        pass

    def validate_real_pattern(self, parser):
        pass


@dataclass(slots=True)
class FloatLiteral(Literal):
    value: float

    def children(self) -> Iterator[Node]:
        return iter(())

    def accept(self, visitor):
        return visitor.visit_FloatLiteral(self)

    def validate_numeric_pattern(self, parser):
        pass

    def validate_real_pattern(self, parser):
        pass


@dataclass(slots=True)
class ComplexLiteral(Literal):
    value: complex

    def children(self) -> Iterator[Node]:
        return iter(())

    def accept(self, visitor):
        return visitor.visit_ComplexLiteral(self)

    def validate_numeric_pattern(self, parser):
        pass

    def validate_imaginary_pattern(self, parser):
        pass


@dataclass(slots=True)
class BooleanLiteral(Literal):
    value: bool

    def children(self) -> Iterator[Node]:
        return iter(())

    def accept(self, visitor):
        return visitor.visit_BooleanLiteral(self)

    def literal_pattern_operator(self) -> ComparisonOperator:
        # The singletons match by identity (PEP 634).
        return ComparisonOperator.IS


@dataclass(slots=True)
class NoneLiteral(Literal):
    value: None = None

    def children(self) -> Iterator[Node]:
        return iter(())

    def accept(self, visitor):
        return visitor.visit_NoneLiteral(self)

    def literal_pattern_operator(self) -> ComparisonOperator:
        # The singletons match by identity (PEP 634).
        return ComparisonOperator.IS


@dataclass(slots=True)
class TextLiteral(Literal):
    value: str

    def children(self) -> Iterator[Node]:
        return iter(())

    def accept(self, visitor):
        return visitor.visit_TextLiteral(self)

    def validate_pattern_literal(self, parser):
        pass

    def append_string_part(self, parts):
        if not self.value:
            return
        if parts:
            parts[-1].merge_string_fragment(parts, self)
        else:
            parts.append(self)

    def merge_string_fragment(self, parts, fragment):
        span = (
            Span(self.span.start, fragment.span.end)
            if self.span and fragment.span
            else None
        )
        parts[-1] = TextLiteral(self.value + fragment.value, span=span)


@dataclass(slots=True)
class BytesLiteral(Literal):
    value: bytes

    def children(self) -> Iterator[Node]:
        return iter(())

    def accept(self, visitor):
        return visitor.visit_BytesLiteral(self)

    def validate_pattern_literal(self, parser):
        pass

    def append_string_part(self, parts):
        if not self.value:
            return
        if parts:
            parts[-1].merge_string_fragment(parts, self)
        else:
            parts.append(self)

    def merge_string_fragment(self, parts, fragment):
        span = (
            Span(self.span.start, fragment.span.end)
            if self.span and fragment.span
            else None
        )
        parts[-1] = BytesLiteral(self.value + fragment.value, span=span)


@dataclass(slots=True)
class EllipsisLiteral(Literal):
    value: EllipsisType = Ellipsis

    def children(self) -> Iterator[Node]:
        return iter(())

    def accept(self, visitor):
        return visitor.visit_EllipsisLiteral(self)


@dataclass(slots=True)
class Binary(Expression):
    operator: BinaryOperator
    left: Expression
    right: Expression

    def children(self) -> Iterator[Node]:
        yield self.left
        yield self.right

    def accept(self, visitor):
        return visitor.visit_Binary(self)


@dataclass(slots=True)
class Unary(Expression):
    operator: UnaryOperator
    operand: Expression

    def children(self) -> Iterator[Node]:
        yield self.operand

    def accept(self, visitor):
        return visitor.visit_Unary(self)


@dataclass(slots=True)
class Boolean(Expression):
    operator: BooleanOperator
    operands: list[Expression]

    def children(self) -> Iterator[Node]:
        yield from self.operands

    def accept(self, visitor):
        return visitor.visit_Boolean(self)

    def append_boolean(self, operator, right, *, grouped):
        if operator != self.operator or grouped:
            return Boolean(
                operator,
                [self, right],
                span=Span(self.source_start(), right.source_end()),
            )
        self.operands.append(right)
        self.span = Span(self.source_start(), right.source_end())
        return self


@dataclass(slots=True)
class Comparison(Expression):
    left: Expression
    operators: list[ComparisonOperator]
    comparators: list[Expression]

    def children(self) -> Iterator[Node]:
        yield self.left
        yield from self.comparators

    def accept(self, visitor):
        return visitor.visit_Comparison(self)

    def append_comparison(self, operator, right, *, grouped):
        if grouped:
            return Comparison(
                self,
                [operator],
                [right],
                span=Span(self.source_start(), right.source_end()),
            )
        self.operators.append(operator)
        self.comparators.append(right)
        self.span = Span(self.source_start(), right.source_end())
        return self


@dataclass(slots=True)
class Conditional(Expression):
    condition: Expression
    then: Expression
    otherwise: Expression

    def children(self) -> Iterator[Node]:
        yield self.condition
        yield self.then
        yield self.otherwise

    def accept(self, visitor):
        return visitor.visit_Conditional(self)


@dataclass(slots=True)
class Parameter(Node):
    name: str
    kind: ParameterKind = ParameterKind.POSITIONAL_OR_KEYWORD
    annotation: Expression | None = None
    default: Expression | None = None
    type_comment: str | None = None

    def children(self) -> Iterator[Node]:
        if self.annotation is not None:
            yield self.annotation
        if self.default is not None:
            yield self.default

    def accept(self, visitor):
        return visitor.visit_Parameter(self)


@dataclass(slots=True)
class Lambda(Expression):
    parameters: list[Parameter]
    body: Expression

    def children(self) -> Iterator[Node]:
        yield from self.parameters
        yield self.body

    def accept(self, visitor):
        return visitor.visit_Lambda(self)


@dataclass(slots=True)
class NamedExpression(Expression):
    target: Name
    value: Expression

    def children(self) -> Iterator[Node]:
        yield self.target
        yield self.value

    def accept(self, visitor):
        return visitor.visit_NamedExpression(self)

    def assigned_targets(self):
        return (self.target,)


@dataclass(slots=True)
class Yield(Expression):
    value: Expression | None = None
    from_iterable: bool = False

    def children(self) -> Iterator[Node]:
        if self.value is not None:
            yield self.value

    def accept(self, visitor):
        return visitor.visit_Yield(self)


@dataclass(slots=True)
class Await(Expression):
    value: Expression

    def children(self) -> Iterator[Node]:
        yield self.value

    def accept(self, visitor):
        return visitor.visit_Await(self)


@dataclass(slots=True)
class Attribute(Expression):
    value: Expression
    attribute: str

    def children(self) -> Iterator[Node]:
        yield self.value

    def accept(self, visitor):
        return visitor.visit_Attribute(self)

    def validate_target(self, parser, *, delete=False):
        pass

    def validate_single_target(self, parser, message, start):
        pass

    def mapping_pattern_key(self, parser):
        return self


@dataclass(slots=True)
class CallArgument(Node):
    value: Expression
    kind: ArgumentKind = ArgumentKind.POSITIONAL
    keyword: str | None = None

    def children(self) -> Iterator[Node]:
        yield self.value

    def accept(self, visitor):
        return visitor.visit_CallArgument(self)


@dataclass(slots=True)
class Call(Expression):
    function: Expression
    arguments: list[CallArgument] = field(default_factory=list)

    def children(self) -> Iterator[Node]:
        yield self.function
        yield from self.arguments

    def accept(self, visitor):
        return visitor.visit_Call(self)


@dataclass(slots=True)
class Subscript(Expression):
    value: Expression
    index: Expression

    def children(self) -> Iterator[Node]:
        yield self.value
        yield self.index

    def accept(self, visitor):
        return visitor.visit_Subscript(self)

    def validate_target(self, parser, *, delete=False):
        pass

    def validate_single_target(self, parser, message, start):
        pass


@dataclass(slots=True)
class Slice(Expression):
    lower: Expression | None = None
    upper: Expression | None = None
    step: Expression | None = None

    def children(self) -> Iterator[Node]:
        if self.lower is not None:
            yield self.lower
        if self.upper is not None:
            yield self.upper
        if self.step is not None:
            yield self.step

    def accept(self, visitor):
        return visitor.visit_Slice(self)


@dataclass(slots=True)
class Unpack(Expression):
    value: Expression

    def children(self) -> Iterator[Node]:
        yield self.value

    def accept(self, visitor):
        return visitor.visit_Unpack(self)

    def require_collection(self, parser, message, start=None):
        parser.error(message, start)

    def subscript_item(self, parser, start):
        return parser.finish(Tuple([self]), start)

    def validate_target(self, parser, *, delete=False):
        if delete:
            parser.error("Invalid deletion target")
        self.value.validate_target(parser)


@dataclass(slots=True)
class Tuple(Expression):
    elements: list[Expression] = field(default_factory=list)

    def children(self) -> Iterator[Node]:
        yield from self.elements

    def accept(self, visitor):
        return visitor.visit_Tuple(self)

    def validate_target(self, parser, *, delete=False):
        for element in self.elements:
            element.validate_target(parser, delete=delete)

    def delete_operands(self, *, grouped):
        return [self] if grouped else self.elements

    def type_parameter_bound(self):
        return None, self.elements

    def validate_exception_binding(self, parser, *, grouped):
        if not grouped:
            parser.error("exception types must be parenthesized with an as binding")


@dataclass(slots=True)
class List(Expression):
    elements: list[Expression] = field(default_factory=list)

    def children(self) -> Iterator[Node]:
        yield from self.elements

    def accept(self, visitor):
        return visitor.visit_List(self)

    def validate_target(self, parser, *, delete=False):
        for element in self.elements:
            element.validate_target(parser, delete=delete)


@dataclass(slots=True)
class Set(Expression):
    elements: list[Expression] = field(default_factory=list)

    def children(self) -> Iterator[Node]:
        yield from self.elements

    def accept(self, visitor):
        return visitor.visit_Set(self)


@dataclass(slots=True)
class DictEntry(Node):
    key: Expression | None
    value: Expression

    def children(self) -> Iterator[Node]:
        if self.key is not None:
            yield self.key
        yield self.value

    def accept(self, visitor):
        return visitor.visit_DictEntry(self)


@dataclass(slots=True)
class Dict(Expression):
    entries: list[DictEntry] = field(default_factory=list)

    def children(self) -> Iterator[Node]:
        yield from self.entries

    def accept(self, visitor):
        return visitor.visit_Dict(self)


@dataclass(slots=True)
class ComprehensionClause(Node):
    target: Expression
    iterable: Expression
    conditions: list[Expression] = field(default_factory=list)
    is_async: bool = False

    def children(self) -> Iterator[Node]:
        yield self.target
        yield self.iterable
        yield from self.conditions

    def accept(self, visitor):
        return visitor.visit_ComprehensionClause(self)

    def assigned_targets(self):
        return (self.target,)


@dataclass(slots=True)
class Comprehension(Expression):
    kind: ComprehensionKind
    element: Expression
    clauses: list[ComprehensionClause]
    key: Expression | None = None

    def children(self) -> Iterator[Node]:
        yield self.element
        yield from self.clauses
        if self.key is not None:
            yield self.key

    def accept(self, visitor):
        return visitor.visit_Comprehension(self)


@dataclass(slots=True)
class Interpolation(Node):
    expression: Expression
    conversion: str | None = None
    format_spec: FormattedString | None = None
    expression_text: str | None = None

    def children(self) -> Iterator[Node]:
        yield self.expression
        if self.format_spec is not None:
            yield self.format_spec

    def accept(self, visitor):
        return visitor.visit_Interpolation(self)


@dataclass(slots=True)
class FormattedString(Expression):
    parts: list[Literal | Interpolation] = field(default_factory=list)

    def children(self) -> Iterator[Node]:
        yield from self.parts

    def accept(self, visitor):
        return visitor.visit_FormattedString(self)


@dataclass(slots=True)
class TemplateString(Expression):
    parts: list[Literal | Interpolation] = field(default_factory=list)

    def children(self) -> Iterator[Node]:
        yield from self.parts

    def accept(self, visitor):
        return visitor.visit_TemplateString(self)


@dataclass(slots=True)
class ExpressionStatement(Statement):
    expression: Expression

    def children(self) -> Iterator[Node]:
        yield self.expression

    def accept(self, visitor):
        return visitor.visit_ExpressionStatement(self)


@dataclass(slots=True)
class Assignment(Statement):
    targets: list[Expression]
    value: Expression
    type_comment: str | None = None

    def children(self) -> Iterator[Node]:
        yield from self.targets
        yield self.value

    def accept(self, visitor):
        return visitor.visit_Assignment(self)

    def assigned_targets(self):
        return tuple(self.targets)


@dataclass(slots=True)
class AnnotatedAssignment(Statement):
    target: Expression
    annotation: Expression
    value: Expression | None = None
    parenthesized: bool = False

    def children(self) -> Iterator[Node]:
        yield self.target
        yield self.annotation
        if self.value is not None:
            yield self.value

    def accept(self, visitor):
        return visitor.visit_AnnotatedAssignment(self)

    def assigned_targets(self):
        return (self.target,)


@dataclass(slots=True)
class AugmentedAssignment(Statement):
    target: Expression
    operator: BinaryOperator
    value: Expression

    def children(self) -> Iterator[Node]:
        yield self.target
        yield self.value

    def accept(self, visitor):
        return visitor.visit_AugmentedAssignment(self)

    def assigned_targets(self):
        return (self.target,)


@dataclass(slots=True)
class Delete(Statement):
    targets: list[Expression]

    def children(self) -> Iterator[Node]:
        yield from self.targets

    def accept(self, visitor):
        return visitor.visit_Delete(self)

    def assigned_targets(self):
        return tuple(self.targets)


@dataclass(slots=True)
class ImportItem(Node):
    name: str
    alias: str | None = None

    def children(self) -> Iterator[Node]:
        return iter(())

    def accept(self, visitor):
        return visitor.visit_ImportItem(self)


@dataclass(slots=True)
class Import(Statement):
    items: list[ImportItem]

    def children(self) -> Iterator[Node]:
        yield from self.items

    def accept(self, visitor):
        return visitor.visit_Import(self)


@dataclass(slots=True)
class ImportFrom(Statement):
    module: str | None
    items: list[ImportItem]
    level: int = 0

    def children(self) -> Iterator[Node]:
        yield from self.items

    def accept(self, visitor):
        return visitor.visit_ImportFrom(self)


@dataclass(slots=True)
class If(Statement):
    condition: Expression
    body: list[Statement]
    else_body: list[Statement] = field(default_factory=list)

    def children(self) -> Iterator[Node]:
        yield self.condition
        yield from self.body
        yield from self.else_body

    def accept(self, visitor):
        return visitor.visit_If(self)


@dataclass(slots=True)
class While(Statement):
    condition: Expression
    body: list[Statement]
    else_body: list[Statement] = field(default_factory=list)

    def children(self) -> Iterator[Node]:
        yield self.condition
        yield from self.body
        yield from self.else_body

    def accept(self, visitor):
        return visitor.visit_While(self)


@dataclass(slots=True)
class For(Statement):
    target: Expression
    iterable: Expression
    body: list[Statement]
    else_body: list[Statement] = field(default_factory=list)
    is_async: bool = False
    type_comment: str | None = None

    def children(self) -> Iterator[Node]:
        yield self.target
        yield self.iterable
        yield from self.body
        yield from self.else_body

    def accept(self, visitor):
        return visitor.visit_For(self)

    def assigned_targets(self):
        return (self.target,)


@dataclass(slots=True)
class WithItem(Node):
    context: Expression
    target: Expression | None = None

    def children(self) -> Iterator[Node]:
        yield self.context
        if self.target is not None:
            yield self.target

    def accept(self, visitor):
        return visitor.visit_WithItem(self)

    def assigned_targets(self):
        return () if self.target is None else (self.target,)


@dataclass(slots=True)
class With(Statement):
    items: list[WithItem]
    body: list[Statement]
    is_async: bool = False
    type_comment: str | None = None

    def children(self) -> Iterator[Node]:
        yield from self.items
        yield from self.body

    def accept(self, visitor):
        return visitor.visit_With(self)


@dataclass(slots=True)
class ExceptionHandler(Node):
    exception: Expression | None
    binding: str | None
    body: list[Statement]

    def children(self) -> Iterator[Node]:
        if self.exception is not None:
            yield self.exception
        yield from self.body

    def accept(self, visitor):
        return visitor.visit_ExceptionHandler(self)


@dataclass(slots=True)
class Try(Statement):
    body: list[Statement]
    handlers: list[ExceptionHandler] = field(default_factory=list)
    else_body: list[Statement] = field(default_factory=list)
    finally_body: list[Statement] = field(default_factory=list)
    exception_groups: bool = False

    def children(self) -> Iterator[Node]:
        yield from self.body
        yield from self.handlers
        yield from self.else_body
        yield from self.finally_body

    def accept(self, visitor):
        return visitor.visit_Try(self)


@dataclass(slots=True)
class TypeParameter(Node):
    name: str
    kind: TypeParameterKind = TypeParameterKind.TYPE_VAR
    bound: Expression | None = None
    constraints: list[Expression] = field(default_factory=list)
    default: Expression | None = None

    def children(self) -> Iterator[Node]:
        if self.bound is not None:
            yield self.bound
        yield from self.constraints
        if self.default is not None:
            yield self.default

    def accept(self, visitor):
        return visitor.visit_TypeParameter(self)


@dataclass(slots=True)
class FunctionDefinition(Statement):
    name: str
    parameters: list[Parameter]
    body: list[Statement]
    decorators: list[Expression] = field(default_factory=list)
    return_annotation: Expression | None = None
    type_parameters: list[TypeParameter] = field(default_factory=list)
    is_async: bool = False
    type_comment: str | None = None

    def children(self) -> Iterator[Node]:
        yield from self.parameters
        yield from self.body
        yield from self.decorators
        if self.return_annotation is not None:
            yield self.return_annotation
        yield from self.type_parameters

    def accept(self, visitor):
        return visitor.visit_FunctionDefinition(self)

    def consume_signature_comment(self, parser):
        if self.type_comment is not None:
            parser.error("duplicate function type comment")
        self.type_comment = parser._type_comment()
        if parser.current.kind == "NEWLINE":
            parser.advance()


@dataclass(slots=True)
class ClassDefinition(Statement):
    name: str
    body: list[Statement]
    arguments: list[CallArgument] = field(default_factory=list)
    decorators: list[Expression] = field(default_factory=list)
    type_parameters: list[TypeParameter] = field(default_factory=list)

    def children(self) -> Iterator[Node]:
        yield from self.body
        yield from self.arguments
        yield from self.decorators
        yield from self.type_parameters

    def accept(self, visitor):
        return visitor.visit_ClassDefinition(self)


@dataclass(slots=True)
class TypeAlias(Statement):
    name: str
    value: Expression
    type_parameters: list[TypeParameter] = field(default_factory=list)

    def children(self) -> Iterator[Node]:
        yield self.value
        yield from self.type_parameters

    def accept(self, visitor):
        return visitor.visit_TypeAlias(self)


@dataclass(slots=True)
class Pass(Statement):
    def children(self) -> Iterator[Node]:
        return iter(())

    def accept(self, visitor):
        return visitor.visit_Pass(self)


@dataclass(slots=True)
class Break(Statement):
    def children(self) -> Iterator[Node]:
        return iter(())

    def accept(self, visitor):
        return visitor.visit_Break(self)


@dataclass(slots=True)
class Continue(Statement):
    def children(self) -> Iterator[Node]:
        return iter(())

    def accept(self, visitor):
        return visitor.visit_Continue(self)


@dataclass(slots=True)
class Return(Statement):
    value: Expression | None = None

    def children(self) -> Iterator[Node]:
        if self.value is not None:
            yield self.value

    def accept(self, visitor):
        return visitor.visit_Return(self)


@dataclass(slots=True)
class Raise(Statement):
    exception: Expression | None = None
    cause: Expression | None = None

    def children(self) -> Iterator[Node]:
        if self.exception is not None:
            yield self.exception
        if self.cause is not None:
            yield self.cause

    def accept(self, visitor):
        return visitor.visit_Raise(self)


@dataclass(slots=True)
class Assert(Statement):
    condition: Expression
    message: Expression | None = None

    def children(self) -> Iterator[Node]:
        yield self.condition
        if self.message is not None:
            yield self.message

    def accept(self, visitor):
        return visitor.visit_Assert(self)


@dataclass(slots=True)
class Global(Statement):
    identifiers: list[str]

    def children(self) -> Iterator[Node]:
        return iter(())

    def accept(self, visitor):
        return visitor.visit_Global(self)


@dataclass(slots=True)
class Nonlocal(Statement):
    identifiers: list[str]

    def children(self) -> Iterator[Node]:
        return iter(())

    def accept(self, visitor):
        return visitor.visit_Nonlocal(self)


@dataclass(slots=True)
class CapturePattern(Pattern):
    name: str | None

    def children(self) -> Iterator[Node]:
        return iter(())

    def accept(self, visitor):
        return visitor.visit_CapturePattern(self)


@dataclass(slots=True)
class LiteralPattern(Pattern):
    value: Expression

    def children(self) -> Iterator[Node]:
        yield self.value

    def accept(self, visitor):
        return visitor.visit_LiteralPattern(self)


@dataclass(slots=True)
class ValuePattern(Pattern):
    value: Expression

    def children(self) -> Iterator[Node]:
        yield self.value

    def accept(self, visitor):
        return visitor.visit_ValuePattern(self)


@dataclass(slots=True)
class SequencePattern(Pattern):
    elements: list[Pattern]

    def children(self) -> Iterator[Node]:
        yield from self.elements

    def accept(self, visitor):
        return visitor.visit_SequencePattern(self)


@dataclass(slots=True)
class StarPattern(Pattern):
    name: str | None

    def children(self) -> Iterator[Node]:
        return iter(())

    def accept(self, visitor):
        return visitor.visit_StarPattern(self)

    def validate_unsequenced(self, parser):
        parser.error("star pattern requires a sequence")


@dataclass(slots=True)
class PatternEntry(Node):
    key: Expression
    pattern: Pattern

    def children(self) -> Iterator[Node]:
        yield self.key
        yield self.pattern

    def accept(self, visitor):
        return visitor.visit_PatternEntry(self)


@dataclass(slots=True)
class MappingPattern(Pattern):
    entries: list[PatternEntry]
    rest: str | None = None

    def children(self) -> Iterator[Node]:
        yield from self.entries

    def accept(self, visitor):
        return visitor.visit_MappingPattern(self)


@dataclass(slots=True)
class KeywordPattern(Node):
    name: str
    pattern: Pattern

    def children(self) -> Iterator[Node]:
        yield self.pattern

    def accept(self, visitor):
        return visitor.visit_KeywordPattern(self)


@dataclass(slots=True)
class ClassPattern(Pattern):
    class_name: Expression
    positional: list[Pattern] = field(default_factory=list)
    keywords: list[KeywordPattern] = field(default_factory=list)

    def children(self) -> Iterator[Node]:
        yield self.class_name
        yield from self.positional
        yield from self.keywords

    def accept(self, visitor):
        return visitor.visit_ClassPattern(self)


@dataclass(slots=True)
class OrPattern(Pattern):
    alternatives: list[Pattern]

    def children(self) -> Iterator[Node]:
        yield from self.alternatives

    def accept(self, visitor):
        return visitor.visit_OrPattern(self)


@dataclass(slots=True)
class AsPattern(Pattern):
    pattern: Pattern
    name: str

    def children(self) -> Iterator[Node]:
        yield self.pattern

    def accept(self, visitor):
        return visitor.visit_AsPattern(self)


@dataclass(slots=True)
class MatchCase(Node):
    pattern: Pattern
    body: list[Statement]
    guard: Expression | None = None

    def children(self) -> Iterator[Node]:
        yield self.pattern
        yield from self.body
        if self.guard is not None:
            yield self.guard

    def accept(self, visitor):
        return visitor.visit_MatchCase(self)


@dataclass(slots=True)
class Match(Statement):
    subject: Expression
    cases: list[MatchCase]

    def children(self) -> Iterator[Node]:
        yield self.subject
        yield from self.cases

    def accept(self, visitor):
        return visitor.visit_Match(self)


def children(node: Node) -> Iterator[Node]:
    """Yield immediate syntax children using their explicit node behavior."""
    return node.children()


def walk(node: Node) -> Iterator[Node]:
    """Visit a syntax tree in preorder, including its root."""
    yield node
    for child in node.children():
        yield from walk(child)


_AST_STATEMENT_VISITS = (
    "visit_ExpressionStatement",
    "visit_Assignment",
    "visit_AnnotatedAssignment",
    "visit_AugmentedAssignment",
    "visit_Delete",
    "visit_Import",
    "visit_ImportFrom",
    "visit_If",
    "visit_While",
    "visit_For",
    "visit_With",
    "visit_Try",
    "visit_FunctionDefinition",
    "visit_ClassDefinition",
    "visit_TypeAlias",
    "visit_Pass",
    "visit_Break",
    "visit_Continue",
    "visit_Return",
    "visit_Raise",
    "visit_Assert",
    "visit_Global",
    "visit_Nonlocal",
    "visit_Match",
)

_AST_NODE_VISITS = (
    "visit_Statement",
    "visit_TypeIgnore",
    "visit_Module",
    "visit_ExpressionInput",
    "visit_InteractiveInput",
    "visit_Name",
    "visit_Literal",
    "visit_IntegerLiteral",
    "visit_FloatLiteral",
    "visit_ComplexLiteral",
    "visit_BooleanLiteral",
    "visit_NoneLiteral",
    "visit_TextLiteral",
    "visit_BytesLiteral",
    "visit_EllipsisLiteral",
    "visit_Binary",
    "visit_Unary",
    "visit_Boolean",
    "visit_Comparison",
    "visit_Conditional",
    "visit_Parameter",
    "visit_Lambda",
    "visit_NamedExpression",
    "visit_Yield",
    "visit_Await",
    "visit_Attribute",
    "visit_CallArgument",
    "visit_Call",
    "visit_Subscript",
    "visit_Slice",
    "visit_Unpack",
    "visit_Tuple",
    "visit_List",
    "visit_Set",
    "visit_DictEntry",
    "visit_Dict",
    "visit_ComprehensionClause",
    "visit_Comprehension",
    "visit_Interpolation",
    "visit_FormattedString",
    "visit_TemplateString",
    "visit_ImportItem",
    "visit_WithItem",
    "visit_ExceptionHandler",
    "visit_TypeParameter",
    "visit_CapturePattern",
    "visit_LiteralPattern",
    "visit_ValuePattern",
    "visit_SequencePattern",
    "visit_StarPattern",
    "visit_PatternEntry",
    "visit_MappingPattern",
    "visit_KeywordPattern",
    "visit_ClassPattern",
    "visit_OrPattern",
    "visit_AsPattern",
    "visit_MatchCase",
)

_AST_VISITOR_FALLBACKS = (
    *((name, "visit_Node") for name in _AST_NODE_VISITS),
    *((name, "visit_Statement") for name in _AST_STATEMENT_VISITS),
)


class Visitor:
    """Explicit node dispatch; subclasses override the syntax they handle."""

    _explicit_visits: frozenset[str] = frozenset()

    def __init_subclass__(cls, **kwargs):
        super().__init_subclass__(**kwargs)
        cls._explicit_visits = frozenset(
            name for name in cls.__dict__ if name.startswith("visit_")
        )
        mro = [c for c in cls.__mro__ if "_explicit_visits" in c.__dict__]
        for child, parent in _AST_VISITOR_FALLBACKS:
            for c in mro:
                if child in c.__dict__["_explicit_visits"]:
                    setattr(cls, child, c.__dict__[child])
                    break
            else:
                setattr(cls, child, getattr(cls, parent))

    def visit_Node(self, node: Node):
        return None

    def visit_Statement(self, node: Statement):
        """Every statement kind falls back here, then to ``visit_Node``."""
        return self.visit_Node(node)

    def visit_TypeIgnore(self, node: TypeIgnore):
        return self.visit_Node(node)

    def visit_Module(self, node: Module):
        return self.visit_Node(node)

    def visit_ExpressionInput(self, node: ExpressionInput):
        return self.visit_Node(node)

    def visit_InteractiveInput(self, node: InteractiveInput):
        return self.visit_Node(node)

    def visit_Name(self, node: Name):
        return self.visit_Node(node)

    def visit_Literal(self, node: Literal):
        return self.visit_Node(node)

    def visit_IntegerLiteral(self, node: IntegerLiteral):
        return self.visit_Node(node)

    def visit_FloatLiteral(self, node: FloatLiteral):
        return self.visit_Node(node)

    def visit_ComplexLiteral(self, node: ComplexLiteral):
        return self.visit_Node(node)

    def visit_BooleanLiteral(self, node: BooleanLiteral):
        return self.visit_Node(node)

    def visit_NoneLiteral(self, node: NoneLiteral):
        return self.visit_Node(node)

    def visit_TextLiteral(self, node: TextLiteral):
        return self.visit_Node(node)

    def visit_BytesLiteral(self, node: BytesLiteral):
        return self.visit_Node(node)

    def visit_EllipsisLiteral(self, node: EllipsisLiteral):
        return self.visit_Node(node)

    def visit_Binary(self, node: Binary):
        return self.visit_Node(node)

    def visit_Unary(self, node: Unary):
        return self.visit_Node(node)

    def visit_Boolean(self, node: Boolean):
        return self.visit_Node(node)

    def visit_Comparison(self, node: Comparison):
        return self.visit_Node(node)

    def visit_Conditional(self, node: Conditional):
        return self.visit_Node(node)

    def visit_Parameter(self, node: Parameter):
        return self.visit_Node(node)

    def visit_Lambda(self, node: Lambda):
        return self.visit_Node(node)

    def visit_NamedExpression(self, node: NamedExpression):
        return self.visit_Node(node)

    def visit_Yield(self, node: Yield):
        return self.visit_Node(node)

    def visit_Await(self, node: Await):
        return self.visit_Node(node)

    def visit_Attribute(self, node: Attribute):
        return self.visit_Node(node)

    def visit_CallArgument(self, node: CallArgument):
        return self.visit_Node(node)

    def visit_Call(self, node: Call):
        return self.visit_Node(node)

    def visit_Subscript(self, node: Subscript):
        return self.visit_Node(node)

    def visit_Slice(self, node: Slice):
        return self.visit_Node(node)

    def visit_Unpack(self, node: Unpack):
        return self.visit_Node(node)

    def visit_Tuple(self, node: Tuple):
        return self.visit_Node(node)

    def visit_List(self, node: List):
        return self.visit_Node(node)

    def visit_Set(self, node: Set):
        return self.visit_Node(node)

    def visit_DictEntry(self, node: DictEntry):
        return self.visit_Node(node)

    def visit_Dict(self, node: Dict):
        return self.visit_Node(node)

    def visit_ComprehensionClause(self, node: ComprehensionClause):
        return self.visit_Node(node)

    def visit_Comprehension(self, node: Comprehension):
        return self.visit_Node(node)

    def visit_Interpolation(self, node: Interpolation):
        return self.visit_Node(node)

    def visit_FormattedString(self, node: FormattedString):
        return self.visit_Node(node)

    def visit_TemplateString(self, node: TemplateString):
        return self.visit_Node(node)

    def visit_ExpressionStatement(self, node: ExpressionStatement):
        return self.visit_Statement(node)

    def visit_Assignment(self, node: Assignment):
        return self.visit_Statement(node)

    def visit_AnnotatedAssignment(self, node: AnnotatedAssignment):
        return self.visit_Statement(node)

    def visit_AugmentedAssignment(self, node: AugmentedAssignment):
        return self.visit_Statement(node)

    def visit_Delete(self, node: Delete):
        return self.visit_Statement(node)

    def visit_ImportItem(self, node: ImportItem):
        return self.visit_Node(node)

    def visit_Import(self, node: Import):
        return self.visit_Statement(node)

    def visit_ImportFrom(self, node: ImportFrom):
        return self.visit_Statement(node)

    def visit_If(self, node: If):
        return self.visit_Statement(node)

    def visit_While(self, node: While):
        return self.visit_Statement(node)

    def visit_For(self, node: For):
        return self.visit_Statement(node)

    def visit_WithItem(self, node: WithItem):
        return self.visit_Node(node)

    def visit_With(self, node: With):
        return self.visit_Statement(node)

    def visit_ExceptionHandler(self, node: ExceptionHandler):
        return self.visit_Node(node)

    def visit_Try(self, node: Try):
        return self.visit_Statement(node)

    def visit_TypeParameter(self, node: TypeParameter):
        return self.visit_Node(node)

    def visit_FunctionDefinition(self, node: FunctionDefinition):
        return self.visit_Statement(node)

    def visit_ClassDefinition(self, node: ClassDefinition):
        return self.visit_Statement(node)

    def visit_TypeAlias(self, node: TypeAlias):
        return self.visit_Statement(node)

    def visit_Pass(self, node: Pass):
        return self.visit_Statement(node)

    def visit_Break(self, node: Break):
        return self.visit_Statement(node)

    def visit_Continue(self, node: Continue):
        return self.visit_Statement(node)

    def visit_Return(self, node: Return):
        return self.visit_Statement(node)

    def visit_Raise(self, node: Raise):
        return self.visit_Statement(node)

    def visit_Assert(self, node: Assert):
        return self.visit_Statement(node)

    def visit_Global(self, node: Global):
        return self.visit_Statement(node)

    def visit_Nonlocal(self, node: Nonlocal):
        return self.visit_Statement(node)

    def visit_CapturePattern(self, node: CapturePattern):
        return self.visit_Node(node)

    def visit_LiteralPattern(self, node: LiteralPattern):
        return self.visit_Node(node)

    def visit_ValuePattern(self, node: ValuePattern):
        return self.visit_Node(node)

    def visit_SequencePattern(self, node: SequencePattern):
        return self.visit_Node(node)

    def visit_StarPattern(self, node: StarPattern):
        return self.visit_Node(node)

    def visit_PatternEntry(self, node: PatternEntry):
        return self.visit_Node(node)

    def visit_MappingPattern(self, node: MappingPattern):
        return self.visit_Node(node)

    def visit_KeywordPattern(self, node: KeywordPattern):
        return self.visit_Node(node)

    def visit_ClassPattern(self, node: ClassPattern):
        return self.visit_Node(node)

    def visit_OrPattern(self, node: OrPattern):
        return self.visit_Node(node)

    def visit_AsPattern(self, node: AsPattern):
        return self.visit_Node(node)

    def visit_MatchCase(self, node: MatchCase):
        return self.visit_Node(node)

    def visit_Match(self, node: Match):
        return self.visit_Statement(node)
