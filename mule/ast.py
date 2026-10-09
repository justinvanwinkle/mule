"""Mule's syntax tree, independent of the host interpreter's parser.

Targets use the same expression nodes as values; their role follows from the
owning statement or comprehension clause. Scope validation belongs to the
compiler. Source columns count Unicode characters, starting at zero. Lines
start at one, and spans exclude their end position.
"""

from collections.abc import Iterator
from dataclasses import dataclass, field, fields
from enum import StrEnum
from types import EllipsisType


@dataclass(frozen=True, slots=True)
class Position:
    line: int
    column: int


@dataclass(frozen=True, slots=True)
class Span:
    start: Position
    end: Position


@dataclass(slots=True, kw_only=True)
class Node:
    span: Span | None = None


class Expression(Node):
    __slots__ = ()


class Statement(Node):
    __slots__ = ()


class Pattern(Node):
    __slots__ = ()


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


@dataclass(slots=True)
class Module(Node):
    statements: list[Statement] = field(default_factory=list)
    type_ignores: list[TypeIgnore] = field(default_factory=list)


@dataclass(slots=True)
class ExpressionInput(Node):
    expression: Expression


@dataclass(slots=True)
class InteractiveInput(Node):
    statements: list[Statement] = field(default_factory=list)


@dataclass(slots=True)
class Name(Expression):
    identifier: str


@dataclass(slots=True)
class Literal(Expression):
    value: str | bytes | int | float | complex | bool | None | EllipsisType


@dataclass(slots=True)
class Binary(Expression):
    operator: BinaryOperator
    left: Expression
    right: Expression


@dataclass(slots=True)
class Unary(Expression):
    operator: UnaryOperator
    operand: Expression


@dataclass(slots=True)
class Boolean(Expression):
    operator: BooleanOperator
    operands: list[Expression]


@dataclass(slots=True)
class Comparison(Expression):
    left: Expression
    operators: list[ComparisonOperator]
    comparators: list[Expression]


@dataclass(slots=True)
class Conditional(Expression):
    condition: Expression
    then: Expression
    otherwise: Expression


@dataclass(slots=True)
class Parameter(Node):
    name: str
    kind: ParameterKind = ParameterKind.POSITIONAL_OR_KEYWORD
    annotation: Expression | None = None
    default: Expression | None = None
    type_comment: str | None = None


@dataclass(slots=True)
class Lambda(Expression):
    parameters: list[Parameter]
    body: Expression


@dataclass(slots=True)
class NamedExpression(Expression):
    target: Name
    value: Expression


@dataclass(slots=True)
class Yield(Expression):
    value: Expression | None = None
    from_iterable: bool = False


@dataclass(slots=True)
class Await(Expression):
    value: Expression


@dataclass(slots=True)
class Attribute(Expression):
    value: Expression
    attribute: str


@dataclass(slots=True)
class CallArgument(Node):
    value: Expression
    kind: ArgumentKind = ArgumentKind.POSITIONAL
    keyword: str | None = None


@dataclass(slots=True)
class Call(Expression):
    function: Expression
    arguments: list[CallArgument] = field(default_factory=list)


@dataclass(slots=True)
class Subscript(Expression):
    value: Expression
    index: Expression


@dataclass(slots=True)
class Slice(Expression):
    lower: Expression | None = None
    upper: Expression | None = None
    step: Expression | None = None


@dataclass(slots=True)
class Unpack(Expression):
    value: Expression


@dataclass(slots=True)
class Tuple(Expression):
    elements: list[Expression] = field(default_factory=list)


@dataclass(slots=True)
class List(Expression):
    elements: list[Expression] = field(default_factory=list)


@dataclass(slots=True)
class Set(Expression):
    elements: list[Expression] = field(default_factory=list)


@dataclass(slots=True)
class DictEntry(Node):
    key: Expression | None
    value: Expression


@dataclass(slots=True)
class Dict(Expression):
    entries: list[DictEntry] = field(default_factory=list)


@dataclass(slots=True)
class ComprehensionClause(Node):
    target: Expression
    iterable: Expression
    conditions: list[Expression] = field(default_factory=list)
    is_async: bool = False


@dataclass(slots=True)
class Comprehension(Expression):
    kind: ComprehensionKind
    element: Expression
    clauses: list[ComprehensionClause]
    key: Expression | None = None


@dataclass(slots=True)
class Interpolation(Node):
    expression: Expression
    conversion: str | None = None
    format_spec: FormattedString | None = None
    expression_text: str | None = None


@dataclass(slots=True)
class FormattedString(Expression):
    parts: list[Literal | Interpolation] = field(default_factory=list)


@dataclass(slots=True)
class TemplateString(Expression):
    parts: list[Literal | Interpolation] = field(default_factory=list)


@dataclass(slots=True)
class ExpressionStatement(Statement):
    expression: Expression


@dataclass(slots=True)
class Assignment(Statement):
    targets: list[Expression]
    value: Expression
    type_comment: str | None = None


@dataclass(slots=True)
class AnnotatedAssignment(Statement):
    target: Expression
    annotation: Expression
    value: Expression | None = None
    parenthesized: bool = False


@dataclass(slots=True)
class AugmentedAssignment(Statement):
    target: Expression
    operator: BinaryOperator
    value: Expression


@dataclass(slots=True)
class Delete(Statement):
    targets: list[Expression]


@dataclass(slots=True)
class ImportItem(Node):
    name: str
    alias: str | None = None


@dataclass(slots=True)
class Import(Statement):
    items: list[ImportItem]


@dataclass(slots=True)
class ImportFrom(Statement):
    module: str | None
    items: list[ImportItem]
    level: int = 0


@dataclass(slots=True)
class If(Statement):
    condition: Expression
    body: list[Statement]
    else_body: list[Statement] = field(default_factory=list)


@dataclass(slots=True)
class While(Statement):
    condition: Expression
    body: list[Statement]
    else_body: list[Statement] = field(default_factory=list)


@dataclass(slots=True)
class For(Statement):
    target: Expression
    iterable: Expression
    body: list[Statement]
    else_body: list[Statement] = field(default_factory=list)
    is_async: bool = False
    type_comment: str | None = None


@dataclass(slots=True)
class WithItem(Node):
    context: Expression
    target: Expression | None = None


@dataclass(slots=True)
class With(Statement):
    items: list[WithItem]
    body: list[Statement]
    is_async: bool = False
    type_comment: str | None = None


@dataclass(slots=True)
class ExceptionHandler(Node):
    exception: Expression | None
    binding: str | None
    body: list[Statement]


@dataclass(slots=True)
class Try(Statement):
    body: list[Statement]
    handlers: list[ExceptionHandler] = field(default_factory=list)
    else_body: list[Statement] = field(default_factory=list)
    finally_body: list[Statement] = field(default_factory=list)
    exception_groups: bool = False


@dataclass(slots=True)
class TypeParameter(Node):
    name: str
    kind: TypeParameterKind = TypeParameterKind.TYPE_VAR
    bound: Expression | None = None
    constraints: list[Expression] = field(default_factory=list)
    default: Expression | None = None


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


@dataclass(slots=True)
class ClassDefinition(Statement):
    name: str
    body: list[Statement]
    arguments: list[CallArgument] = field(default_factory=list)
    decorators: list[Expression] = field(default_factory=list)
    type_parameters: list[TypeParameter] = field(default_factory=list)


@dataclass(slots=True)
class TypeAlias(Statement):
    name: str
    value: Expression
    type_parameters: list[TypeParameter] = field(default_factory=list)


@dataclass(slots=True)
class Pass(Statement):
    pass


@dataclass(slots=True)
class Break(Statement):
    pass


@dataclass(slots=True)
class Continue(Statement):
    pass


@dataclass(slots=True)
class Return(Statement):
    value: Expression | None = None


@dataclass(slots=True)
class Raise(Statement):
    exception: Expression | None = None
    cause: Expression | None = None


@dataclass(slots=True)
class Assert(Statement):
    condition: Expression
    message: Expression | None = None


@dataclass(slots=True)
class Global(Statement):
    identifiers: list[str]


@dataclass(slots=True)
class Nonlocal(Statement):
    identifiers: list[str]


@dataclass(slots=True)
class CapturePattern(Pattern):
    name: str | None


@dataclass(slots=True)
class LiteralPattern(Pattern):
    value: Expression


@dataclass(slots=True)
class ValuePattern(Pattern):
    value: Expression


@dataclass(slots=True)
class SequencePattern(Pattern):
    elements: list[Pattern]


@dataclass(slots=True)
class StarPattern(Pattern):
    name: str | None


@dataclass(slots=True)
class PatternEntry(Node):
    key: Expression
    pattern: Pattern


@dataclass(slots=True)
class MappingPattern(Pattern):
    entries: list[PatternEntry]
    rest: str | None = None


@dataclass(slots=True)
class KeywordPattern(Node):
    name: str
    pattern: Pattern


@dataclass(slots=True)
class ClassPattern(Pattern):
    class_name: Expression
    positional: list[Pattern] = field(default_factory=list)
    keywords: list[KeywordPattern] = field(default_factory=list)


@dataclass(slots=True)
class OrPattern(Pattern):
    alternatives: list[Pattern]


@dataclass(slots=True)
class AsPattern(Pattern):
    pattern: Pattern
    name: str


@dataclass(slots=True)
class MatchCase(Node):
    pattern: Pattern
    body: list[Statement]
    guard: Expression | None = None


@dataclass(slots=True)
class Match(Statement):
    subject: Expression
    cases: list[MatchCase]


def children(node: Node) -> Iterator[Node]:
    """Yield immediate syntax children in field and source-list order."""
    for item in fields(node):
        if item.name == "span":
            continue
        value = getattr(node, item.name)
        if isinstance(value, Node):
            yield value
        elif isinstance(value, list):
            yield from (child for child in value if isinstance(child, Node))


def walk(node: Node) -> Iterator[Node]:
    """Visit a syntax tree in preorder, including its root."""
    yield node
    for child in children(node):
        yield from walk(child)
