"""Language-level value types, following SBCL's CTYPE (``ctype.lisp``).

Polymorphic semantic facts used by control-flow and local-call analysis.

Intervals describe values, not representations of class names. Operations use
ordinary virtual methods and double dispatch; representation properties expose
only proofs that LLVM may use after a successful language operation.
"""

from dataclasses import dataclass, field, replace

from mule import ast

FIXNUM_MIN = -(2**62)
FIXNUM_MAX = 2**62 - 1


class ValueTypeVisitor:
    """A value type selects its visitor method directly; unhandled kinds fall
    back to their parent class's method, ending at ``visit_ValueType``."""

    def visit_ValueType(self, fact):
        return None

    def visit_UnknownType(self, fact):
        return self.visit_ValueType(fact)

    def visit_PrimitiveType(self, fact):
        return self.visit_ValueType(fact)

    def visit_IntegerType(self, fact):
        return self.visit_PrimitiveType(fact)

    def visit_PythonIntegerType(self, fact):
        return self.visit_PrimitiveType(fact)

    def visit_BooleanType(self, fact):
        return self.visit_IntegerType(fact)

    def visit_FloatType(self, fact):
        return self.visit_PrimitiveType(fact)

    def visit_RealType(self, fact):
        return self.visit_PrimitiveType(fact)

    def visit_ImmortalType(self, fact):
        return self.visit_ValueType(fact)

    def visit_NoneType(self, fact):
        return self.visit_PrimitiveType(fact)

    def visit_TextType(self, fact):
        return self.visit_PrimitiveType(fact)

    def visit_BytesType(self, fact):
        return self.visit_PrimitiveType(fact)

    def visit_FunctionType(self, fact):
        return self.visit_ValueType(fact)

    def visit_TupleType(self, fact):
        return self.visit_ValueType(fact)

    def visit_ListType(self, fact):
        return self.visit_ValueType(fact)

    def visit_DictType(self, fact):
        return self.visit_ValueType(fact)

    def visit_IteratorType(self, fact):
        return self.visit_ValueType(fact)

    def visit_BottomType(self, fact):
        return self.visit_ValueType(fact)

    def visit_UnboundType(self, fact):
        return self.visit_ValueType(fact)

    def visit_OptionalIntegerType(self, fact):
        return self.visit_ValueType(fact)


@dataclass(frozen=True, slots=True)
class ValueType:
    @property
    def may_be_unbound(self) -> bool:
        return False

    @property
    def minimum(self):
        return None

    @property
    def maximum(self):
        return None

    @property
    def is_fixnum(self):
        return False

    @property
    def is_immortal(self):
        return False

    @property
    def is_heap_object(self):
        return False

    @property
    def is_exact_list(self):
        return False

    @property
    def needs_release(self):
        return True

    @property
    def may_run_user_code(self):
        return True

    def accept(self, visitor):
        return visitor.visit_ValueType(self)

    def to_dict(self) -> dict:
        return self.accept(_SERIALIZE_VALUE_TYPE)

    def as_bound(self):
        if not self.may_be_unbound:
            return self
        return replace(self, may_be_unbound=False)

    def with_unbound(self):
        if self.may_be_unbound:
            return self
        return replace(self, may_be_unbound=True)

    def as_maybe_unbound(self):
        return self.with_unbound()

    def truth_value(self) -> bool | None:
        return None

    def normalized_bottom(self):
        return self

    def when_defined(self, continuation):
        return continuation()

    def merge_information(self, other, left, right, combine):
        return other.merge_into_information(left, right, combine)

    def merge_into_information(self, left, right, combine):
        return combine(left, right)

    def join(self, other):
        return other.join_unknown(self)

    def join_unknown(self, left):
        return (
            UNKNOWN_UNBOUND if self.may_be_unbound or left.may_be_unbound else UNKNOWN
        )

    def join_integer(self, left):
        return self.join_unknown(left)

    def join_boolean(self, left):
        return self.join_unknown(left)

    def join_python_integer(self, left):
        return self.join_unknown(left)

    def join_float(self, left):
        return self.join_unknown(left)

    def join_real(self, left):
        return self.join_unknown(left)

    def join_none(self, left):
        return self.join_unknown(left)

    def join_immortal(self, left):
        return self.join_unknown(left)

    def join_text(self, left):
        return self.join_unknown(left)

    def join_bytes(self, left):
        return self.join_unknown(left)

    def join_function(self, left):
        return self.join_unknown(left)

    def join_tuple(self, left):
        return self.join_unknown(left)

    def join_list(self, left):
        return self.join_unknown(left)

    def join_dict(self, left):
        return self.join_unknown(left)

    def join_iterator(self, left):
        return self.join_unknown(left)

    def join_unbound(self, left):
        return self.with_unbound()

    def widen(self, new):
        return new

    def widen_integer(self, old):
        return self

    def widen_tuple(self, old):
        return self

    def widen_list(self, old):
        return self

    def widen_dict(self, old):
        return self

    def widen_iterator(self, old):
        return self

    def widen_constant(self):
        return self

    def binary(self, operator, right):
        return right.binary_unknown(operator, self)

    def binary_unknown(self, operator, left):
        return UNKNOWN

    def binary_integer(self, operator, left):
        return UNKNOWN

    def binary_boolean(self, operator, left):
        return UNKNOWN

    def binary_float(self, operator, left):
        return UNKNOWN

    def binary_real(self, operator, left):
        return UNKNOWN

    def binary_text(self, operator, left):
        return UNKNOWN

    def binary_bytes(self, operator, left):
        return UNKNOWN

    def binary_tuple(self, operator, left):
        return UNKNOWN

    def binary_list(self, operator, left):
        return UNKNOWN

    def unary(self, operator):
        return BOOLEAN if operator == ast.UnaryOperator.NOT else UNKNOWN

    def comparison(self, operator, right):
        if operator in {
            ast.ComparisonOperator.IS,
            ast.ComparisonOperator.IS_NOT,
            ast.ComparisonOperator.IN,
            ast.ComparisonOperator.NOT_IN,
        }:
            return BOOLEAN
        return right.compare_unknown(self)

    def compare_unknown(self, left):
        return UNKNOWN

    def compare_primitive(self, left):
        return UNKNOWN

    def narrow(self, operator, bound, truth):
        return self

    def narrow_none(self, is_none: bool):
        return self

    def exact_integer(self):
        return None

    def interned_text(self) -> str | None:
        return None

    def fixnum_domain(self):
        return None

    def dispatch_domain(self):
        return self.fixnum_domain()

    def dispatch_rank(self) -> int:
        return 2**64

    def getitem_type(self, index):
        return UNKNOWN

    def iterate_type(self):
        return UNKNOWN

    def next_item_type(self):
        return UNKNOWN

    def unpacked_tuple_type(self, arity: int):
        return TupleType(arity)

    def with_element(self, element):
        return self

    def with_entry(self, key, value):
        return self

    def dict_get_type(self, default=None):
        return UNKNOWN

    def dict_items_iterator_type(self):
        return UNKNOWN

    def dict_keys_iterator_type(self):
        return UNKNOWN

    def dict_values_iterator_type(self):
        return UNKNOWN

    def method_return_type(
        self, name: str, arity: int, has_keywords: bool, storage_methods=None
    ):
        return UNKNOWN

    def fits(self, expected):
        return expected.accepts(self)

    def dispatch_fits(self, expected):
        return expected.accepts_dispatch(self)

    def may_dispatch_fit(self, expected):
        return expected.accepts_possible_dispatch(self)

    def accepts_dispatch(self, actual):
        return False

    def accepts_possible_dispatch(self, actual):
        return self.accepts_dispatch(actual)

    def accepts(self, actual):
        return actual.fits_unknown(self)

    def annotation_conflict(self, contract):
        return False

    def annotated_integer(self, expected):
        return expected

    def annotated_list(self, expected):
        return expected

    def fits_unknown(self, expected):
        return not self.may_be_unbound

    def fits_integer(self, expected):
        return False

    def fits_possible_integer(self, expected):
        return False

    def fits_python_integer(self, expected):
        return False

    def fits_boolean(self, expected):
        return False

    def fits_possible_boolean(self, expected):
        return False

    def fits_float(self, expected):
        return False

    def fits_real(self, expected):
        return False

    def fits_none(self, expected):
        return False

    def fits_possible_none(self, expected):
        return False

    def fits_immortal(self, expected):
        return False

    def fits_possible_immortal(self, expected):
        return False

    def fits_text(self, expected):
        return False

    def fits_possible_text(self, expected):
        return False

    def fits_bytes(self, expected):
        return False

    def fits_function(self, expected):
        return False

    def fits_tuple(self, expected):
        return False

    def fits_possible_tuple(self, expected):
        return False

    def fits_list(self, expected=None):
        return False

    def fits_possible_list(self, expected=None):
        return False

    def fits_dict(self, expected=None):
        return False

    def fits_iterator(self, expected=None):
        return False

    def proves_tuple_index(self, index):
        """Whether every value of this type is a bound tuple with an element
        at ``index``."""
        return False


@dataclass(frozen=True, slots=True)
class UnknownType(ValueType):
    may_be_unbound: bool = False

    def accept(self, visitor):
        return visitor.visit_UnknownType(self)

    def as_bound(self):
        return UNKNOWN

    def with_unbound(self):
        return UNKNOWN_UNBOUND

    def join(self, other):
        if other is self:
            return self
        return other.join_unknown(self)

    def accepts_dispatch(self, actual):
        return True

    def fits_possible_integer(self, expected):
        return not self.may_be_unbound

    def fits_possible_boolean(self, expected):
        return not self.may_be_unbound

    def fits_possible_none(self, expected):
        return not self.may_be_unbound

    def fits_possible_immortal(self, expected):
        return not self.may_be_unbound

    def fits_possible_tuple(self, expected):
        return not self.may_be_unbound

    def fits_possible_text(self, expected):
        return not self.may_be_unbound and expected.literal is None

    def fits_possible_list(self, expected=None):
        return not self.may_be_unbound and (
            expected is None or expected.element is UNKNOWN
        )


UNKNOWN = UnknownType()
UNKNOWN_UNBOUND = UnknownType(may_be_unbound=True)


@dataclass(frozen=True, slots=True)
class PrimitiveType(ValueType):
    def annotation_conflict(self, contract):
        return not self.fits(contract.fact)

    @property
    def may_run_user_code(self):
        return False

    def comparison(self, operator, right):
        if operator in {
            ast.ComparisonOperator.IS,
            ast.ComparisonOperator.IS_NOT,
            ast.ComparisonOperator.IN,
            ast.ComparisonOperator.NOT_IN,
        }:
            return BOOLEAN
        return right.compare_primitive(self)

    def compare_primitive(self, left):
        return BOOLEAN


@dataclass(frozen=True, slots=True)
class IntegerType(PrimitiveType):
    minimum: int | None = None
    maximum: int | None = None
    may_be_unbound: bool = False

    def fits_real(self, expected):
        return not self.may_be_unbound

    def join_real(self, left):
        return left.join_numeric(self)

    def annotation_conflict(self, contract):
        return contract.integer_conflict(self)

    def annotated_integer(self, expected):
        return IntegerType(
            max(self.minimum, expected.minimum)
            if self.minimum is not None
            else expected.minimum,
            min(self.maximum, expected.maximum)
            if self.maximum is not None
            else expected.maximum,
        )

    @property
    def is_fixnum(self):
        return (
            not self.may_be_unbound
            and self.minimum is not None
            and self.maximum is not None
            and FIXNUM_MIN <= self.minimum <= self.maximum <= FIXNUM_MAX
        )

    @property
    def needs_release(self):
        return not (
            self.minimum is not None
            and self.maximum is not None
            and FIXNUM_MIN <= self.minimum <= self.maximum <= FIXNUM_MAX
        )

    def accept(self, visitor):
        return visitor.visit_IntegerType(self)

    def join(self, other):
        return other.join_integer(self)

    def join_integer(self, left):
        if left is self:
            return self
        lo = (
            min(left.minimum, self.minimum)
            if left.minimum is not None and self.minimum is not None
            else None
        )
        hi = (
            max(left.maximum, self.maximum)
            if left.maximum is not None and self.maximum is not None
            else None
        )
        unbound = left.may_be_unbound or self.may_be_unbound
        if lo is None and hi is None and not unbound:
            return INTEGER
        if lo == self.minimum and hi == self.maximum and unbound == self.may_be_unbound:
            return self
        if lo == left.minimum and hi == left.maximum and unbound == left.may_be_unbound:
            return left
        return IntegerType(lo, hi, may_be_unbound=unbound)

    def join_boolean(self, left):
        return _join_python_integer(left, self)

    def join_python_integer(self, left):
        return _join_python_integer(left, self)

    def widen(self, new):
        return new.widen_integer(self)

    def widen_integer(self, old):
        minimum, maximum = self.minimum, self.maximum
        if old.minimum is None or (minimum is not None and minimum < old.minimum):
            minimum = None
        if old.maximum is None or (maximum is not None and maximum > old.maximum):
            maximum = None
        if minimum == self.minimum and maximum == self.maximum:
            return self
        return replace(self, minimum=minimum, maximum=maximum)

    def widen_constant(self):
        if (
            self.minimum is not None
            and self.maximum is not None
            and FIXNUM_MIN <= self.minimum <= self.maximum <= FIXNUM_MAX
        ):
            return IntegerType(FIXNUM_MIN, FIXNUM_MAX, self.may_be_unbound)
        return IntegerType(may_be_unbound=True) if self.may_be_unbound else INTEGER

    def binary(self, operator, right):
        if self.may_be_unbound:
            return right.binary_unknown(operator, self)
        return right.binary_integer(operator, self)

    def binary_integer(self, operator, left):
        return _integer_binary(operator, left, self)

    def binary_boolean(self, operator, left):
        return _integer_binary(operator, left, self)

    def binary_float(self, operator, left):
        return _float_binary(operator, left, self)

    def binary_real(self, operator, left):
        return _real_binary(operator, left, self)

    def binary_text(self, operator, left):
        if not left.may_be_unbound and operator in {
            ast.BinaryOperator.MULTIPLY,
            ast.BinaryOperator.MODULO,
        }:
            return TEXT_TYPE
        return UNKNOWN

    def binary_bytes(self, operator, left):
        if not left.may_be_unbound and operator in {
            ast.BinaryOperator.MULTIPLY,
            ast.BinaryOperator.MODULO,
        }:
            return BYTES_TYPE
        return UNKNOWN

    def binary_list(self, operator, left):
        if (
            not left.may_be_unbound
            and not self.may_be_unbound
            and operator == ast.BinaryOperator.MULTIPLY
        ):
            return ListType(left.element)
        return UNKNOWN

    def unary(self, operator):
        if operator == ast.UnaryOperator.NOT:
            return BOOLEAN
        if self.may_be_unbound:
            return UNKNOWN
        if operator == ast.UnaryOperator.POSITIVE:
            return IntegerType(self.minimum, self.maximum)
        adjustment = 1 if operator == ast.UnaryOperator.INVERT else 0
        return IntegerType(
            -self.maximum - adjustment if self.maximum is not None else None,
            -self.minimum - adjustment if self.minimum is not None else None,
        )

    def narrow(self, operator, bound, truth):
        if self.may_be_unbound:
            return self
        if not truth:
            operator = {
                ast.ComparisonOperator.LESS: ast.ComparisonOperator.GREATER_EQUAL,
                ast.ComparisonOperator.LESS_EQUAL: ast.ComparisonOperator.GREATER,
                ast.ComparisonOperator.GREATER: ast.ComparisonOperator.LESS_EQUAL,
                ast.ComparisonOperator.GREATER_EQUAL: ast.ComparisonOperator.LESS,
                ast.ComparisonOperator.EQUAL: ast.ComparisonOperator.NOT_EQUAL,
                ast.ComparisonOperator.NOT_EQUAL: ast.ComparisonOperator.EQUAL,
            }.get(operator, operator)
        minimum, maximum = self.minimum, self.maximum
        if operator in {ast.ComparisonOperator.LESS, ast.ComparisonOperator.LESS_EQUAL}:
            upper = bound - (operator == ast.ComparisonOperator.LESS)
            maximum = min(maximum, upper) if maximum is not None else upper
        elif operator in {
            ast.ComparisonOperator.GREATER,
            ast.ComparisonOperator.GREATER_EQUAL,
        }:
            lower = bound + (operator == ast.ComparisonOperator.GREATER)
            minimum = max(minimum, lower) if minimum is not None else lower
        elif operator == ast.ComparisonOperator.EQUAL:
            minimum = max(minimum, bound) if minimum is not None else bound
            maximum = min(maximum, bound) if maximum is not None else bound
        if minimum is not None and maximum is not None and minimum > maximum:
            return self
        if minimum == self.minimum and maximum == self.maximum:
            return self
        return replace(self, minimum=minimum, maximum=maximum)

    def exact_integer(self):
        if (
            not self.may_be_unbound
            and self.minimum is not None
            and self.minimum == self.maximum
        ):
            return self.minimum
        return None

    def fixnum_domain(self):
        if self.may_be_unbound:
            return None
        minimum = (
            max(FIXNUM_MIN, self.minimum) if self.minimum is not None else FIXNUM_MIN
        )
        maximum = (
            min(FIXNUM_MAX, self.maximum) if self.maximum is not None else FIXNUM_MAX
        )
        return IntegerType(minimum, maximum) if minimum <= maximum else None

    def dispatch_rank(self) -> int:
        return self.maximum - self.minimum if self.is_fixnum else 2**64

    def accepts(self, actual):
        return actual.fits_integer(self)

    def accepts_dispatch(self, actual):
        return self.is_fixnum and actual.fits_integer(self)

    def accepts_possible_dispatch(self, actual):
        return self.is_fixnum and actual.fits_possible_integer(self)

    def fits_integer(self, expected):
        return _bounds_fit(self, expected)

    def fits_possible_integer(self, expected):
        return _bounds_overlap(self, expected)

    def fits_python_integer(self, expected):
        return _bounds_fit(self, expected)


@dataclass(frozen=True, slots=True)
class PythonIntegerType(IntegerType):
    """An unbounded Python int may be a boolean singleton or an integer value."""

    @property
    def is_fixnum(self):
        return False

    def accept(self, visitor):
        return visitor.visit_PythonIntegerType(self)

    def accepts(self, actual):
        return actual.fits_python_integer(self)

    def accepts_dispatch(self, actual):
        return False

    def accepts_possible_dispatch(self, actual):
        return False

    def fits_integer(self, expected):
        return False

    def fits_possible_boolean(self, expected):
        return _bounds_overlap(self, expected)

    def fits_possible_immortal(self, expected):
        return _bounds_overlap(self, BOOLEAN)

    def join(self, other):
        return other.join_python_integer(self)

    def join_python_integer(self, left):
        return _join_python_integer(left, self)

    join_integer = join_python_integer
    join_boolean = join_python_integer

    def widen_constant(self):
        return (
            PythonIntegerType(may_be_unbound=True)
            if self.may_be_unbound
            else PYTHON_INTEGER
        )

    def narrow(self, operator, bound, truth):
        narrowed = super().narrow(operator, bound, truth)
        if (narrowed.minimum is not None and narrowed.minimum > 1) or (
            narrowed.maximum is not None and narrowed.maximum < 0
        ):
            return IntegerType(
                narrowed.minimum,
                narrowed.maximum,
                may_be_unbound=narrowed.may_be_unbound,
            )
        return narrowed

    def exact_integer(self):
        return None


@dataclass(frozen=True, slots=True)
class BooleanType(IntegerType):
    minimum: int | None = 0
    maximum: int | None = 1

    def annotation_conflict(self, contract):
        return contract.boolean_conflict(self)

    @property
    def is_fixnum(self):
        return False

    @property
    def is_immortal(self):
        return not self.may_be_unbound

    @property
    def needs_release(self):
        return False

    def accept(self, visitor):
        return visitor.visit_BooleanType(self)

    def join(self, other):
        return other.join_boolean(self)

    def join_integer(self, left):
        return _join_python_integer(left, self)

    def join_python_integer(self, left):
        return _join_python_integer(left, self)

    def join_boolean(self, left):
        if left is self:
            return self
        lo = (
            min(left.minimum, self.minimum)
            if left.minimum is not None and self.minimum is not None
            else None
        )
        hi = (
            max(left.maximum, self.maximum)
            if left.maximum is not None and self.maximum is not None
            else None
        )
        unbound = left.may_be_unbound or self.may_be_unbound
        if lo == 0 and hi == 1 and not unbound:
            return BOOLEAN
        return BooleanType(lo, hi, may_be_unbound=unbound)

    def join_none(self, left):
        return _join_immortal(self, left)

    def join_immortal(self, left):
        return _join_immortal(self, left)

    def widen(self, new):
        return new

    def widen_integer(self, old):
        return self

    def widen_constant(self):
        return BooleanType(may_be_unbound=True) if self.may_be_unbound else BOOLEAN

    def binary(self, operator, right):
        if self.may_be_unbound:
            return right.binary_unknown(operator, self)
        return right.binary_boolean(operator, self)

    def binary_boolean(self, operator, left):
        factory = (
            BooleanType
            if operator
            in {
                ast.BinaryOperator.BIT_AND,
                ast.BinaryOperator.BIT_OR,
                ast.BinaryOperator.BIT_XOR,
            }
            else IntegerType
        )
        return _integer_binary(operator, left, self, factory)

    def narrow(self, operator, bound, truth):
        return self

    def exact_integer(self):
        return None

    def fixnum_domain(self):
        return None

    def dispatch_domain(self):
        return None if self.may_be_unbound else self

    def dispatch_rank(self) -> int:
        if self.may_be_unbound:
            return 2**64
        return (
            self.maximum - self.minimum
            if self.minimum is not None and self.maximum is not None
            else 2
        )

    def accepts(self, actual):
        return actual.fits_boolean(self)

    def fits_integer(self, expected):
        return False

    def fits_possible_integer(self, expected):
        return False

    def accepts_dispatch(self, actual):
        return actual.fits_boolean(self)

    def accepts_possible_dispatch(self, actual):
        return actual.fits_possible_boolean(self)

    def fits_boolean(self, expected):
        return _bounds_fit(self, expected)

    def fits_possible_boolean(self, expected):
        return _bounds_overlap(self, expected)

    def fits_immortal(self, expected):
        return self.is_immortal

    def fits_possible_immortal(self, expected):
        return True


@dataclass(frozen=True, slots=True)
class FloatType(PrimitiveType):
    may_be_unbound: bool = False

    def fits_real(self, expected):
        return not self.may_be_unbound

    def join_real(self, left):
        return left.join_numeric(self)

    @property
    def is_heap_object(self):
        return not self.may_be_unbound

    def accept(self, visitor):
        return visitor.visit_FloatType(self)

    def join(self, other):
        return other.join_float(self)

    def join_float(self, left):
        return (
            FloatType(may_be_unbound=True)
            if left.may_be_unbound or self.may_be_unbound
            else FLOAT
        )

    def binary(self, operator, right):
        if self.may_be_unbound:
            return right.binary_unknown(operator, self)
        return right.binary_float(operator, self)

    def binary_integer(self, operator, left):
        return _float_binary(operator, left, self)

    def binary_boolean(self, operator, left):
        return _float_binary(operator, left, self)

    def binary_float(self, operator, left):
        return _float_binary(operator, left, self)

    def binary_real(self, operator, left):
        return _float_binary(operator, left, self)

    def unary(self, operator):
        if operator == ast.UnaryOperator.NOT:
            return BOOLEAN
        return FLOAT if operator != ast.UnaryOperator.INVERT else UNKNOWN

    def accepts(self, actual):
        return actual.fits_float(self)

    def fits_float(self, expected):
        return not self.may_be_unbound


@dataclass(frozen=True, slots=True)
class RealType(PrimitiveType):
    """Python float annotations admit integers without changing their objects."""

    may_be_unbound: bool = False

    def accept(self, visitor):
        return visitor.visit_RealType(self)

    def accepts(self, actual):
        return actual.fits_real(self)

    def fits_real(self, expected):
        return not self.may_be_unbound

    def annotation_conflict(self, contract):
        # Either integer or floating members may satisfy a numeric contract.
        return False

    def join(self, other):
        return other.join_real(self)

    def join_numeric(self, left):
        return (
            RealType(may_be_unbound=True)
            if self.may_be_unbound or left.may_be_unbound
            else REAL
        )

    join_real = join_numeric
    join_integer = join_numeric
    join_boolean = join_numeric
    join_python_integer = join_numeric
    join_float = join_numeric

    def binary(self, operator, right):
        if self.may_be_unbound:
            return right.binary_unknown(operator, self)
        return right.binary_real(operator, self)

    def binary_integer(self, operator, left):
        return _real_binary(operator, left, self)

    def binary_boolean(self, operator, left):
        return _real_binary(operator, left, self)

    def binary_float(self, operator, left):
        return _float_binary(operator, left, self)

    def binary_real(self, operator, left):
        return _real_binary(operator, left, self)

    def unary(self, operator):
        if operator == ast.UnaryOperator.NOT:
            return BOOLEAN
        if self.may_be_unbound or operator == ast.UnaryOperator.INVERT:
            return UNKNOWN
        return REAL


@dataclass(frozen=True, slots=True)
class ImmortalType(ValueType):
    may_be_unbound: bool = False

    def annotation_conflict(self, contract):
        return True

    @property
    def is_immortal(self):
        return not self.may_be_unbound

    @property
    def needs_release(self):
        return False

    @property
    def may_run_user_code(self):
        return False

    def accept(self, visitor):
        return visitor.visit_ImmortalType(self)

    def join(self, other):
        return other.join_immortal(self)

    def join_boolean(self, left):
        return _join_immortal(self, left)

    def join_none(self, left):
        return _join_immortal(self, left)

    def join_immortal(self, left):
        return _join_immortal(self, left)

    def accepts(self, actual):
        return actual.fits_immortal(self)

    def accepts_dispatch(self, actual):
        return actual.fits_immortal(self)

    def accepts_possible_dispatch(self, actual):
        return actual.fits_possible_immortal(self)

    def fits_immortal(self, expected):
        return self.is_immortal

    def fits_possible_boolean(self, expected):
        return True

    def fits_possible_none(self, expected):
        return True

    def fits_possible_immortal(self, expected):
        return True


@dataclass(frozen=True, slots=True)
class NoneType(PrimitiveType):
    may_be_unbound: bool = False

    @property
    def is_immortal(self):
        return not self.may_be_unbound

    @property
    def needs_release(self):
        return False

    def accept(self, visitor):
        return visitor.visit_NoneType(self)

    def join(self, other):
        return other.join_none(self)

    def join_none(self, left):
        return (
            NoneType(may_be_unbound=True)
            if self.may_be_unbound or left.may_be_unbound
            else NONE_TYPE
        )

    def join_boolean(self, left):
        return _join_immortal(self, left)

    def join_immortal(self, left):
        return _join_immortal(self, left)

    def accepts(self, actual):
        return actual.fits_none(self)

    def accepts_dispatch(self, actual):
        return actual.fits_none(self)

    def accepts_possible_dispatch(self, actual):
        return actual.fits_possible_none(self)

    def fits_none(self, expected):
        return not self.may_be_unbound

    def fits_possible_none(self, expected):
        return True

    def fits_immortal(self, expected):
        return self.is_immortal

    def fits_possible_immortal(self, expected):
        return True


@dataclass(frozen=True, slots=True)
class TextType(PrimitiveType):
    literal: str | None = None
    may_be_unbound: bool = False

    @property
    def is_heap_object(self):
        return not self.may_be_unbound

    @property
    def is_immortal(self):
        return not self.may_be_unbound and self.interned_text() is not None

    @property
    def needs_release(self):
        return self.may_be_unbound or self.interned_text() is None

    def interned_text(self) -> str | None:
        if self.may_be_unbound or self.literal is None:
            return None
        if self.literal == "" or (len(self.literal) == 1 and ord(self.literal) <= 0xFF):
            return self.literal
        return None

    def dispatch_domain(self):
        return None if self.may_be_unbound else TEXT_TYPE

    def dispatch_rank(self) -> int:
        return 2**64 if self.may_be_unbound else 0

    def accept(self, visitor):
        return visitor.visit_TextType(self)

    def join(self, other):
        return other.join_text(self)

    def join_text(self, left):
        if left is self:
            return self
        unbound = self.may_be_unbound or left.may_be_unbound
        literal = self.literal if self.literal == left.literal else None
        if literal is None and not unbound:
            return TEXT_TYPE
        return TextType(literal, may_be_unbound=unbound)

    def widen(self, new):
        if new.fits_text(TEXT_TYPE) and self.literal != new.interned_text():
            return (
                TEXT_TYPE if not new.may_be_unbound else TextType(may_be_unbound=True)
            )
        return new

    def widen_constant(self):
        return TextType(may_be_unbound=True) if self.may_be_unbound else TEXT_TYPE

    def binary(self, operator, right):
        if self.may_be_unbound:
            return right.binary_unknown(operator, self)
        if operator == ast.BinaryOperator.MODULO and not right.may_be_unbound:
            return TEXT_TYPE
        return right.binary_text(operator, self)

    def binary_text(self, operator, left):
        if not left.may_be_unbound and operator in {
            ast.BinaryOperator.ADD,
            ast.BinaryOperator.MODULO,
        }:
            return TEXT_TYPE
        return UNKNOWN

    def binary_integer(self, operator, left):
        if not left.may_be_unbound and operator == ast.BinaryOperator.MULTIPLY:
            return TEXT_TYPE
        return UNKNOWN

    def binary_boolean(self, operator, left):
        if not left.may_be_unbound and operator == ast.BinaryOperator.MULTIPLY:
            return TEXT_TYPE
        return UNKNOWN

    def accepts(self, actual):
        return actual.fits_text(self)

    def accepts_dispatch(self, actual):
        return actual.fits_text(self)

    def accepts_possible_dispatch(self, actual):
        return actual.fits_possible_text(self)

    def fits_text(self, expected):
        return not self.may_be_unbound and (
            expected.literal is None or self.literal == expected.literal
        )

    def fits_possible_text(self, expected):
        return not self.may_be_unbound and (
            expected.literal is None
            or self.literal is None
            or self.literal == expected.literal
        )

    def getitem_type(self, index):
        return UNKNOWN if self.may_be_unbound else TEXT_TYPE

    def iterate_type(self):
        if self.may_be_unbound:
            return UNKNOWN
        return IteratorType(TEXT_TYPE)

    def annotation_conflict(self, contract):
        return False if self.may_be_unbound else contract.text_conflict(self)

    def method_return_type(
        self, name: str, arity: int, has_keywords: bool, storage_methods=None
    ):
        if self.may_be_unbound or storage_methods is None:
            return UNKNOWN
        return storage_methods.get(("TextStorage", name), UNKNOWN)


@dataclass(frozen=True, slots=True)
class BytesType(PrimitiveType):
    may_be_unbound: bool = False

    @property
    def is_heap_object(self):
        return not self.may_be_unbound

    def accept(self, visitor):
        return visitor.visit_BytesType(self)

    def join(self, other):
        return other.join_bytes(self)

    def join_bytes(self, left):
        return (
            BytesType(may_be_unbound=True)
            if self.may_be_unbound or left.may_be_unbound
            else BYTES_TYPE
        )

    def binary(self, operator, right):
        if self.may_be_unbound:
            return right.binary_unknown(operator, self)
        if operator == ast.BinaryOperator.MODULO and not right.may_be_unbound:
            return BYTES_TYPE
        return right.binary_bytes(operator, self)

    def binary_bytes(self, operator, left):
        if not left.may_be_unbound and operator in {
            ast.BinaryOperator.ADD,
            ast.BinaryOperator.MODULO,
        }:
            return BYTES_TYPE
        return UNKNOWN

    def binary_integer(self, operator, left):
        if not left.may_be_unbound and operator == ast.BinaryOperator.MULTIPLY:
            return BYTES_TYPE
        return UNKNOWN

    def binary_boolean(self, operator, left):
        if not left.may_be_unbound and operator == ast.BinaryOperator.MULTIPLY:
            return BYTES_TYPE
        return UNKNOWN

    def accepts(self, actual):
        return actual.fits_bytes(self)

    def fits_bytes(self, expected):
        return not self.may_be_unbound

    def getitem_type(self, index):
        if self.may_be_unbound or index.may_be_unbound:
            return UNKNOWN
        if index.fits(PYTHON_INTEGER):
            return IntegerType(0, 255)
        return UNKNOWN

    def iterate_type(self):
        if self.may_be_unbound:
            return UNKNOWN
        return IteratorType(IntegerType(0, 255))

    def annotation_conflict(self, contract):
        return False if self.may_be_unbound else contract.bytes_conflict(self)

    def method_return_type(
        self, name: str, arity: int, has_keywords: bool, storage_methods=None
    ):
        if self.may_be_unbound or storage_methods is None:
            return UNKNOWN
        return storage_methods.get(("BytesStorage", name), UNKNOWN)


@dataclass(frozen=True, slots=True)
class FunctionType(ValueType):
    may_be_unbound: bool = False

    def annotation_conflict(self, contract):
        return True

    @property
    def is_heap_object(self):
        return not self.may_be_unbound

    def accept(self, visitor):
        return visitor.visit_FunctionType(self)

    def join(self, other):
        return other.join_function(self)

    def join_function(self, left):
        return (
            FunctionType(may_be_unbound=True)
            if self.may_be_unbound or left.may_be_unbound
            else FUNCTION_TYPE
        )

    def accepts(self, actual):
        return actual.fits_function(self)

    def fits_function(self, expected):
        return not self.may_be_unbound


@dataclass(frozen=True, slots=True)
class TupleType(ValueType):
    """Exact builtin tuple storage, with bounded arity information only.

    Elements remain unknown: even an immutable tuple may contain objects whose
    comparison invokes user code. This fact does not imply immortal ownership.
    """

    arity: int | None = None
    elements: tuple[ValueType, ...] | None = None
    may_be_unbound: bool = False

    def annotation_conflict(self, contract):
        return False if self.may_be_unbound else contract.tuple_conflict(self)

    @property
    def is_heap_object(self):
        return not self.may_be_unbound

    def accept(self, visitor):
        return visitor.visit_TupleType(self)

    def join(self, other):
        return other.join_tuple(self)

    def join_tuple(self, left):
        if left is self:
            return self
        if self.arity is not None and self.arity == left.arity:
            arity = self.arity
            if (
                self.elements is not None
                and left.elements is not None
                and len(self.elements) == len(left.elements)
            ):
                joined_elems = tuple(
                    l_el.join(r_el) for l_el, r_el in zip(left.elements, self.elements)
                )
                elements = (
                    None if all(el is UNKNOWN for el in joined_elems) else joined_elems
                )
            else:
                elements = None
        else:
            arity = None
            elements = None
        unbound = self.may_be_unbound or left.may_be_unbound
        if (
            arity == self.arity
            and elements == self.elements
            and unbound == self.may_be_unbound
        ):
            return self
        if (
            arity == left.arity
            and elements == left.elements
            and unbound == left.may_be_unbound
        ):
            return left
        return TupleType(arity, elements, may_be_unbound=unbound)

    def widen(self, new):
        return new.widen_tuple(self)

    def widen_tuple(self, old):
        if (
            self.arity is not None
            and self.arity == old.arity
            and self.elements is not None
            and old.elements is not None
            and len(self.elements) == len(old.elements)
        ):
            widened_elems = tuple(
                widen_fact(o_el, n_el)
                for o_el, n_el in zip(old.elements, self.elements)
            )
            elements = (
                None if all(el is UNKNOWN for el in widened_elems) else widened_elems
            )
            unbound = old.may_be_unbound or self.may_be_unbound
            if elements == self.elements and unbound == self.may_be_unbound:
                return self
            return TupleType(self.arity, elements, may_be_unbound=unbound)
        return self

    def widen_constant(self):
        return TupleType(
            self.arity,
            None
            if self.elements is None
            else tuple(el.widen_constant() for el in self.elements),
            self.may_be_unbound,
        )

    def binary(self, operator, right):
        if self.may_be_unbound:
            return right.binary_unknown(operator, self)
        return right.binary_tuple(operator, self)

    def binary_tuple(self, operator, left):
        if not left.may_be_unbound and operator == ast.BinaryOperator.ADD:
            if left.arity is not None and self.arity is not None:
                elements = (
                    left.elements + self.elements
                    if left.elements is not None and self.elements is not None
                    else None
                )
                return TupleType(left.arity + self.arity, elements)
            return TupleType()
        return UNKNOWN

    def getitem_type(self, index):
        if self.may_be_unbound or index.may_be_unbound:
            return UNKNOWN
        if self.elements is not None:
            idx = index.exact_integer()
            if idx is not None:
                if idx < 0:
                    idx += len(self.elements)
                if 0 <= idx < len(self.elements):
                    return self.elements[idx]
            if len(self.elements) > 0 and index.fits(PYTHON_INTEGER):
                joined = self.elements[0]
                for element in self.elements[1:]:
                    joined = joined.join(element)
                return joined
        return UNKNOWN

    def iterate_type(self):
        if self.may_be_unbound:
            return UNKNOWN
        if self.elements:
            elem = self.elements[0]
            for item in self.elements[1:]:
                elem = elem.join(item)
            return IteratorType(elem)
        if self.arity == 0:
            return IteratorType(BOTTOM)
        return ITERATOR_TYPE

    def unpacked_tuple_type(self, arity: int):
        if not self.may_be_unbound and self.arity == arity:
            return self
        return TupleType(arity)

    def accepts(self, actual):
        return actual.fits_tuple(self)

    def accepts_dispatch(self, actual):
        return actual.fits_tuple(self)

    def accepts_possible_dispatch(self, actual):
        return actual.fits_possible_tuple(self)

    def fits_tuple(self, expected):
        return not self.may_be_unbound and (
            expected.arity is None or self.arity == expected.arity
        )

    def fits_possible_tuple(self, expected):
        return (
            expected.arity is None or self.arity is None or self.arity == expected.arity
        )

    def proves_tuple_index(self, index):
        return not self.may_be_unbound and self.arity is not None and index < self.arity


@dataclass(frozen=True, slots=True)
class ListType(ValueType):
    """Exact builtin list storage."""

    element: ValueType = UNKNOWN
    may_be_unbound: bool = False

    def annotation_conflict(self, contract):
        return False if self.may_be_unbound else contract.list_conflict(self)

    def annotated_list(self, expected):
        if self.element is UNKNOWN:
            return ListType(expected.element, may_be_unbound=self.may_be_unbound)
        return self

    @property
    def is_heap_object(self):
        return not self.may_be_unbound

    @property
    def is_exact_list(self):
        return not self.may_be_unbound

    def accept(self, visitor):
        return visitor.visit_ListType(self)

    def as_bound(self):
        if not self.may_be_unbound:
            return self
        return LIST_TYPE if self.element is UNKNOWN else ListType(self.element, False)

    def with_unbound(self):
        if self.may_be_unbound:
            return self
        return ListType(self.element, True)

    def join(self, other):
        return other.join_list(self)

    def join_list(self, left):
        if left is self:
            return self
        may_be_unbound = left.may_be_unbound or self.may_be_unbound
        if left.element is BOTTOM:
            element = self.element
        elif self.element is BOTTOM:
            element = left.element
        else:
            element = left.element.join(self.element)
        if element is UNKNOWN and not may_be_unbound:
            return LIST_TYPE
        if element == self.element and may_be_unbound == self.may_be_unbound:
            return self
        if element == left.element and may_be_unbound == left.may_be_unbound:
            return left
        return ListType(element, may_be_unbound)

    def widen(self, new):
        return new.widen_list(self)

    def widen_list(self, old):
        element = widen_fact(old.element, self.element)
        may_be_unbound = old.may_be_unbound or self.may_be_unbound
        if element == self.element and may_be_unbound == self.may_be_unbound:
            return self
        if element is UNKNOWN and not may_be_unbound:
            return LIST_TYPE
        return ListType(element, may_be_unbound)

    def dispatch_domain(self):
        return None if self.may_be_unbound else LIST_TYPE

    def dispatch_rank(self) -> int:
        return 2**64 if self.may_be_unbound else 0

    def accepts(self, actual):
        return actual.fits_list(self)

    def accepts_dispatch(self, actual):
        return actual.fits_list(self)

    def accepts_possible_dispatch(self, actual):
        return actual.fits_possible_list(self)

    def fits_list(self, expected=None):
        if expected is None:
            return not self.may_be_unbound
        if self.may_be_unbound and not expected.may_be_unbound:
            return False
        if expected.element is UNKNOWN or self.element is BOTTOM:
            return True
        return self.element.fits(expected.element)

    def fits_possible_list(self, expected=None):
        if expected is None:
            return not self.may_be_unbound
        if self.may_be_unbound and not expected.may_be_unbound:
            return False
        if (
            expected.element is UNKNOWN
            or self.element is UNKNOWN
            or self.element is BOTTOM
        ):
            return True
        return self.element.fits(expected.element)

    def getitem_type(self, index):
        if self.may_be_unbound or index.may_be_unbound:
            return UNKNOWN
        if index.fits(PYTHON_INTEGER) and self.element is not BOTTOM:
            return self.element
        return UNKNOWN

    def iterate_type(self):
        if self.may_be_unbound:
            return UNKNOWN
        return IteratorType(UNKNOWN if self.element is BOTTOM else self.element)

    def unpacked_tuple_type(self, arity: int):
        elem = (
            UNKNOWN if (self.may_be_unbound or self.element is BOTTOM) else self.element
        )
        if elem is not UNKNOWN and 0 < arity <= 16:
            return TupleType(arity, (elem,) * arity)
        return TupleType(arity)

    def with_element(self, element):
        elem = element.as_bound()
        new_elem = elem if self.element is BOTTOM else self.element.join(elem)
        return ListType(new_elem, self.may_be_unbound)

    def binary(self, operator, right):
        if self.may_be_unbound:
            return right.binary_unknown(operator, self)
        if operator == ast.BinaryOperator.MULTIPLY and right.fits(PYTHON_INTEGER):
            return ListType(self.element)
        return right.binary_list(operator, self)

    def binary_list(self, operator, left):
        if (
            not left.may_be_unbound
            and not self.may_be_unbound
            and operator == ast.BinaryOperator.ADD
        ):
            if left.element is BOTTOM:
                joined_elem = self.element
            elif self.element is BOTTOM:
                joined_elem = left.element
            else:
                joined_elem = left.element.join(self.element)
            return ListType(joined_elem)
        return UNKNOWN

    def binary_integer(self, operator, left):
        if (
            not left.may_be_unbound
            and not self.may_be_unbound
            and operator == ast.BinaryOperator.MULTIPLY
        ):
            return ListType(self.element)
        return UNKNOWN

    def binary_boolean(self, operator, left):
        if (
            not left.may_be_unbound
            and not self.may_be_unbound
            and operator == ast.BinaryOperator.MULTIPLY
        ):
            return ListType(self.element)
        return UNKNOWN

    def method_return_type(self, name: str, arity: int, has_keywords: bool):
        if self.may_be_unbound:
            return UNKNOWN
        if name == "append" and arity == 1 and not has_keywords:
            return NONE_TYPE
        if name in {"clear", "reverse", "insert", "extend", "remove", "sort"}:
            return NONE_TYPE
        if name == "copy":
            return LIST_TYPE if self.element is UNKNOWN else ListType(self.element)
        if name == "pop" and arity <= 1 and not has_keywords:
            return UNKNOWN if self.element is BOTTOM else self.element
        if name in {"index", "count"}:
            return IntegerType(0, (1 << 60) - 1)
        return UNKNOWN


@dataclass(frozen=True, slots=True)
class DictType(ValueType):
    """Exact builtin dict storage."""

    key: ValueType = UNKNOWN
    value: ValueType = UNKNOWN
    may_be_unbound: bool = False

    def annotation_conflict(self, contract):
        return True

    @property
    def is_heap_object(self):
        return not self.may_be_unbound

    def accept(self, visitor):
        return visitor.visit_DictType(self)

    def as_bound(self):
        if not self.may_be_unbound:
            return self
        if self.key is UNKNOWN and self.value is UNKNOWN:
            return DICT_TYPE
        return DictType(self.key, self.value, False)

    def with_unbound(self):
        if self.may_be_unbound:
            return self
        return DictType(self.key, self.value, True)

    def join(self, other):
        return other.join_dict(self)

    def join_dict(self, left):
        if left is self:
            return self
        may_be_unbound = left.may_be_unbound or self.may_be_unbound
        if left.key is BOTTOM:
            key = self.key
        elif self.key is BOTTOM:
            key = left.key
        else:
            key = left.key.join(self.key)
        if left.value is BOTTOM:
            value = self.value
        elif self.value is BOTTOM:
            value = left.value
        else:
            value = left.value.join(self.value)
        if key is UNKNOWN and value is UNKNOWN and not may_be_unbound:
            return DICT_TYPE
        if (
            key == self.key
            and value == self.value
            and may_be_unbound == self.may_be_unbound
        ):
            return self
        if (
            key == left.key
            and value == left.value
            and may_be_unbound == left.may_be_unbound
        ):
            return left
        return DictType(key, value, may_be_unbound)

    def widen(self, new):
        return new.widen_dict(self)

    def widen_dict(self, old):
        key = widen_fact(old.key, self.key)
        value = widen_fact(old.value, self.value)
        may_be_unbound = old.may_be_unbound or self.may_be_unbound
        if (
            key == self.key
            and value == self.value
            and may_be_unbound == self.may_be_unbound
        ):
            return self
        if key is UNKNOWN and value is UNKNOWN and not may_be_unbound:
            return DICT_TYPE
        return DictType(key, value, may_be_unbound)

    def accepts(self, actual):
        return actual.fits_dict(self)

    def fits_dict(self, expected=None):
        if expected is None:
            return not self.may_be_unbound
        if self.may_be_unbound and not expected.may_be_unbound:
            return False
        key_ok = (
            expected.key is UNKNOWN or self.key is BOTTOM or self.key.fits(expected.key)
        )
        val_ok = (
            expected.value is UNKNOWN
            or self.value is BOTTOM
            or self.value.fits(expected.value)
        )
        return key_ok and val_ok

    def with_entry(self, key, value):
        k = key.as_bound()
        v = value.as_bound()
        new_key = k if self.key is BOTTOM else self.key.join(k)
        new_val = v if self.value is BOTTOM else self.value.join(v)
        return DictType(new_key, new_val, self.may_be_unbound)

    def getitem_type(self, index):
        if self.may_be_unbound or index.may_be_unbound or self.value is BOTTOM:
            return UNKNOWN
        return self.value

    def dict_get_type(self, default=None):
        if default is None:
            default = NONE_TYPE
        if self.may_be_unbound or default.may_be_unbound:
            return UNKNOWN
        if self.value is BOTTOM:
            return default
        return self.value.join(default)

    def iterate_type(self):
        if self.may_be_unbound:
            return UNKNOWN
        return IteratorType(UNKNOWN if self.key is BOTTOM else self.key)

    def dict_keys_iterator_type(self):
        return self.iterate_type()

    def dict_values_iterator_type(self):
        if self.may_be_unbound:
            return UNKNOWN
        return IteratorType(UNKNOWN if self.value is BOTTOM else self.value)

    def dict_items_iterator_type(self):
        if self.may_be_unbound:
            return UNKNOWN
        k = UNKNOWN if self.key is BOTTOM else self.key
        v = UNKNOWN if self.value is BOTTOM else self.value
        return IteratorType(TupleType(2, (k, v)))

    def method_return_type(self, name: str, arity: int, has_keywords: bool):
        if self.may_be_unbound:
            return UNKNOWN
        if name == "get" and arity == 1 and not has_keywords:
            return self.dict_get_type(NONE_TYPE)
        if name in {"clear", "update"} and not has_keywords:
            return NONE_TYPE
        if name == "copy" and arity == 0 and not has_keywords:
            return (
                DICT_TYPE
                if (self.key is UNKNOWN and self.value is UNKNOWN)
                else DictType(self.key, self.value)
            )
        if name == "pop" and arity == 1 and not has_keywords:
            return UNKNOWN if self.value is BOTTOM else self.value
        return UNKNOWN


@dataclass(frozen=True, slots=True)
class IteratorType(ValueType):
    """Iterator with element type information."""

    element: ValueType = UNKNOWN
    may_be_unbound: bool = False

    def annotation_conflict(self, contract):
        return True

    @property
    def is_heap_object(self):
        return not self.may_be_unbound

    @property
    def may_run_user_code(self):
        return self.may_be_unbound or self.element is UNKNOWN

    def accept(self, visitor):
        return visitor.visit_IteratorType(self)

    def as_bound(self):
        if not self.may_be_unbound:
            return self
        return (
            ITERATOR_TYPE
            if self.element is UNKNOWN
            else IteratorType(self.element, False)
        )

    def with_unbound(self):
        if self.may_be_unbound:
            return self
        return IteratorType(self.element, True)

    def join(self, other):
        return other.join_iterator(self)

    def join_iterator(self, left):
        if left is self:
            return self
        may_be_unbound = left.may_be_unbound or self.may_be_unbound
        if left.element is BOTTOM:
            element = self.element
        elif self.element is BOTTOM:
            element = left.element
        else:
            element = left.element.join(self.element)
        if element is UNKNOWN and not may_be_unbound:
            return ITERATOR_TYPE
        if element == self.element and may_be_unbound == self.may_be_unbound:
            return self
        if element == left.element and may_be_unbound == left.may_be_unbound:
            return left
        return IteratorType(element, may_be_unbound)

    def widen(self, new):
        return new.widen_iterator(self)

    def widen_iterator(self, old):
        element = widen_fact(old.element, self.element)
        may_be_unbound = old.may_be_unbound or self.may_be_unbound
        if element == self.element and may_be_unbound == self.may_be_unbound:
            return self
        if element is UNKNOWN and not may_be_unbound:
            return ITERATOR_TYPE
        return IteratorType(element, may_be_unbound)

    def accepts(self, actual):
        return actual.fits_iterator(self)

    def fits_iterator(self, expected=None):
        if expected is None:
            return not self.may_be_unbound
        if self.may_be_unbound and not expected.may_be_unbound:
            return False
        if expected.element is UNKNOWN or self.element is BOTTOM:
            return True
        return self.element.fits(expected.element)

    def iterate_type(self):
        if self.may_be_unbound:
            return UNKNOWN
        return self

    def next_item_type(self):
        if self.may_be_unbound or self.element is BOTTOM:
            return UNKNOWN
        return self.element


@dataclass(frozen=True, slots=True)
class BottomType(ValueType):
    may_be_unbound: bool = False

    @property
    def needs_release(self):
        return False

    def accepts(self, actual):
        return False

    def accepts_dispatch(self, actual):
        return False

    def join(self, other):
        return other

    def join_unknown(self, left):
        return left

    join_integer = join_unknown
    join_boolean = join_unknown
    join_python_integer = join_unknown
    join_float = join_unknown
    join_real = join_unknown
    join_none = join_unknown
    join_immortal = join_unknown
    join_text = join_unknown
    join_bytes = join_unknown
    join_function = join_unknown
    join_tuple = join_unknown
    join_list = join_unknown
    join_dict = join_unknown
    join_iterator = join_unknown
    join_unbound = join_unknown

    def normalized_bottom(self):
        return UNKNOWN

    def when_defined(self, continuation):
        return self

    def merge_information(self, other, left, right, combine):
        return right

    def merge_into_information(self, left, right, combine):
        return left

    def binary(self, operator, right):
        return self

    def binary_unknown(self, operator, left):
        return self

    binary_integer = binary_unknown
    binary_boolean = binary_unknown
    binary_float = binary_unknown
    binary_real = binary_unknown
    binary_text = binary_unknown
    binary_bytes = binary_unknown
    binary_tuple = binary_unknown
    binary_list = binary_unknown

    def unary(self, operator):
        return BOOLEAN if operator == ast.UnaryOperator.NOT else self

    def fits_unknown(self, expected):
        return False


@dataclass(frozen=True, slots=True)
class UnboundType(ValueType):
    may_be_unbound: bool = True

    @property
    def needs_release(self):
        return False

    def as_bound(self):
        return BOTTOM

    def accepts(self, actual):
        return False

    def accepts_dispatch(self, actual):
        return False

    def join(self, other):
        return other.join_unbound(self)

    def join_unknown(self, left):
        return left.with_unbound()

    join_integer = join_unknown
    join_boolean = join_unknown
    join_python_integer = join_unknown
    join_float = join_unknown
    join_real = join_unknown
    join_none = join_unknown
    join_immortal = join_unknown
    join_text = join_unknown
    join_bytes = join_unknown
    join_function = join_unknown
    join_tuple = join_unknown
    join_list = join_unknown
    join_dict = join_unknown
    join_iterator = join_unknown
    join_unbound = join_unknown


@dataclass(frozen=True, slots=True)
class OptionalIntegerType(ValueType):
    integer: IntegerType = field(default_factory=IntegerType)
    may_be_unbound: bool = False

    @property
    def is_immortal(self):
        return not self.may_be_unbound and self.integer.is_fixnum

    @property
    def needs_release(self):
        return self.may_be_unbound or self.integer.needs_release

    @property
    def may_run_user_code(self):
        return False

    def narrow_none(self, is_none: bool):
        if self.may_be_unbound:
            return self
        return NONE_TYPE if is_none else self.integer


def widen_fact(previous: ValueType, current: ValueType) -> ValueType:
    return previous.widen(previous.join(current))


INTEGER = IntegerType()
PYTHON_INTEGER = PythonIntegerType()
BOOLEAN = BooleanType()
BOOLEAN_FALSE = BooleanType(0, 0)
BOOLEAN_TRUE = BooleanType(1, 1)
FLOAT = FloatType()
REAL = RealType()
IMMORTAL = ImmortalType()
IMMORTAL_UNBOUND = ImmortalType(may_be_unbound=True)
NONE_TYPE = NoneType()
TEXT_TYPE = TextType()
BYTES_TYPE = BytesType()
FUNCTION_TYPE = FunctionType()
BOTTOM = BottomType()
UNBOUND = UnboundType()
LIST_TYPE = ListType()
EMPTY_LIST_TYPE = ListType(BOTTOM)
DICT_TYPE = DictType()
EMPTY_DICT_TYPE = DictType(BOTTOM, BOTTOM)
ITERATOR_TYPE = IteratorType()
OPTIONAL_FIXNUM = OptionalIntegerType(IntegerType(FIXNUM_MIN, FIXNUM_MAX))


def _bounds_fit(actual, expected):
    return (
        not actual.may_be_unbound
        and (
            expected.minimum is None
            or actual.minimum is not None
            and actual.minimum >= expected.minimum
        )
        and (
            expected.maximum is None
            or actual.maximum is not None
            and actual.maximum <= expected.maximum
        )
    )


def _bounds_overlap(actual, expected):
    return (
        expected.minimum is None
        or actual.maximum is None
        or actual.maximum >= expected.minimum
    ) and (
        expected.maximum is None
        or actual.minimum is None
        or actual.minimum <= expected.maximum
    )


def _join_immortal(left, right):
    return IMMORTAL_UNBOUND if left.may_be_unbound or right.may_be_unbound else IMMORTAL


def _join_python_integer(left, right):
    lo = (
        min(left.minimum, right.minimum)
        if left.minimum is not None and right.minimum is not None
        else None
    )
    hi = (
        max(left.maximum, right.maximum)
        if left.maximum is not None and right.maximum is not None
        else None
    )
    unbound = left.may_be_unbound or right.may_be_unbound
    if lo is None and hi is None and not unbound:
        return PYTHON_INTEGER
    return PythonIntegerType(lo, hi, may_be_unbound=unbound)


def _float_binary(operator, left, right):
    if left.may_be_unbound or right.may_be_unbound:
        return UNKNOWN
    if operator in {
        ast.BinaryOperator.ADD,
        ast.BinaryOperator.SUBTRACT,
        ast.BinaryOperator.MULTIPLY,
        ast.BinaryOperator.DIVIDE,
        ast.BinaryOperator.FLOOR_DIVIDE,
        ast.BinaryOperator.MODULO,
        ast.BinaryOperator.POWER,
    }:
        return FLOAT
    return UNKNOWN


def _real_binary(operator, left, right):
    if left.may_be_unbound or right.may_be_unbound:
        return UNKNOWN
    if operator == ast.BinaryOperator.DIVIDE:
        return FLOAT
    if operator in {
        ast.BinaryOperator.ADD,
        ast.BinaryOperator.SUBTRACT,
        ast.BinaryOperator.MULTIPLY,
        ast.BinaryOperator.FLOOR_DIVIDE,
        ast.BinaryOperator.MODULO,
    }:
        return REAL
    return UNKNOWN


def _integer_binary(operator, left, right, factory=IntegerType):
    if left.may_be_unbound or right.may_be_unbound:
        return UNKNOWN
    lo, hi, rlo, rhi = left.minimum, left.maximum, right.minimum, right.maximum
    if operator == ast.BinaryOperator.DIVIDE:
        return FLOAT
    if operator == ast.BinaryOperator.MATRIX_MULTIPLY:
        return UNKNOWN
    if operator == ast.BinaryOperator.POWER:
        if rhi is not None and rhi < 0:
            return FLOAT
        if rlo is None or rlo < 0:
            return UNKNOWN
    if operator == ast.BinaryOperator.ADD:
        return IntegerType(
            lo + rlo if lo is not None and rlo is not None else None,
            hi + rhi if hi is not None and rhi is not None else None,
        )
    if operator == ast.BinaryOperator.SUBTRACT:
        return IntegerType(
            lo - rhi if lo is not None and rhi is not None else None,
            hi - rlo if hi is not None and rlo is not None else None,
        )
    if None in (lo, hi, rlo, rhi):
        if operator == ast.BinaryOperator.BIT_AND:
            for minimum, maximum in ((lo, hi), (rlo, rhi)):
                if minimum is not None and minimum >= 0 and maximum is not None:
                    return IntegerType(0, maximum)
        if (
            operator == ast.BinaryOperator.MULTIPLY
            and lo is not None
            and lo >= 0
            and rlo is not None
            and rlo >= 0
        ):
            return IntegerType(lo * rlo, None)
        if (
            operator == ast.BinaryOperator.RIGHT_SHIFT
            and lo is not None
            and lo >= 0
            and rlo is not None
            and rlo >= 0
        ):
            return IntegerType(
                lo >> rhi if rhi is not None and rhi <= 256 else 0,
                hi >> rlo if hi is not None and rlo <= 256 else None,
            )
        if (
            operator == ast.BinaryOperator.LEFT_SHIFT
            and lo is not None
            and lo >= 0
            and rlo is not None
            and 0 <= rlo <= 256
        ):
            return IntegerType(lo << rlo, None)
        if operator == ast.BinaryOperator.MODULO:
            if rlo is not None and rlo > 0 and rhi is not None:
                return factory(0, rhi - 1)
            if rlo is not None and rhi is not None and rhi < 0:
                return factory(rlo + 1, 0)
        if (
            operator == ast.BinaryOperator.FLOOR_DIVIDE
            and lo is not None
            and lo >= 0
            and rlo is not None
            and rlo > 0
        ):
            return factory(
                lo // rhi if rhi is not None else 0,
                hi // rlo if hi is not None else None,
            )
        return INTEGER
    minimum = maximum = None
    if operator == ast.BinaryOperator.MULTIPLY:
        values = [lo * rlo, lo * rhi, hi * rlo, hi * rhi]
        minimum, maximum = min(values), max(values)
    elif operator in {
        ast.BinaryOperator.BIT_AND,
        ast.BinaryOperator.BIT_OR,
        ast.BinaryOperator.BIT_XOR,
    }:
        if lo == hi and rlo == rhi:
            if operator == ast.BinaryOperator.BIT_AND:
                minimum = maximum = lo & rlo
            elif operator == ast.BinaryOperator.BIT_OR:
                minimum = maximum = lo | rlo
            else:
                minimum = maximum = lo ^ rlo
        elif operator == ast.BinaryOperator.BIT_AND and (lo >= 0 or rlo >= 0):
            minimum = 0
            maximum = min(hi, rhi) if lo >= 0 and rlo >= 0 else hi if lo >= 0 else rhi
        elif lo >= 0 and rlo >= 0:
            minimum, maximum = 0, (1 << max(hi.bit_length(), rhi.bit_length())) - 1
        elif left.is_fixnum and right.is_fixnum:
            minimum, maximum = FIXNUM_MIN, FIXNUM_MAX
    elif operator in {ast.BinaryOperator.LEFT_SHIFT, ast.BinaryOperator.RIGHT_SHIFT}:
        if 0 <= rlo <= rhi <= 256:
            if operator == ast.BinaryOperator.LEFT_SHIFT:
                values = [lo << rlo, lo << rhi, hi << rlo, hi << rhi]
            else:
                values = [lo >> rlo, lo >> rhi, hi >> rlo, hi >> rhi]
            minimum, maximum = min(values), max(values)
    elif operator == ast.BinaryOperator.FLOOR_DIVIDE:
        if rlo > 0 or rhi < 0:
            values = [lo // rlo, lo // rhi, hi // rlo, hi // rhi]
            minimum, maximum = min(values), max(values)
    elif operator == ast.BinaryOperator.MODULO:
        if lo == hi and rlo == rhi and rlo != 0:
            minimum = maximum = lo % rlo
        elif rlo > 0:
            if lo >= 0 and hi < rlo:
                minimum, maximum = lo, hi
            else:
                minimum, maximum = 0, rhi - 1
        elif rhi < 0:
            if hi <= 0 and lo > rhi:
                minimum, maximum = lo, hi
            else:
                minimum, maximum = rlo + 1, 0
    elif operator == ast.BinaryOperator.POWER:
        if rhi <= 32 and max(abs(lo), abs(hi)).bit_length() <= 128:
            bases = {lo, hi} | {value for value in (-1, 0, 1) if lo <= value <= hi}
            values = [
                base**exponent for base in bases for exponent in range(rlo, rhi + 1)
            ]
            minimum, maximum = min(values), max(values)
    return factory(minimum, maximum)
