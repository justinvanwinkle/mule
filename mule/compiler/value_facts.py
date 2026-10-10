"""Polymorphic semantic facts used by control-flow and local-call analysis.

Intervals describe values, not representations of class names. Operations use
ordinary virtual methods and double dispatch; representation properties expose
only proofs that LLVM may use after a successful language operation.
"""

from dataclasses import dataclass, field, replace

from mule import ast

FIXNUM_MIN = -(2**62)
FIXNUM_MAX = 2**62 - 1


@dataclass(frozen=True, slots=True)
class ValueType:
    may_be_unbound: bool = field(default=False, kw_only=True)

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
    def may_run_user_code(self):
        return True

    def accept(self, visitor):
        return visitor.visit_ValueType(self)

    def with_unbound(self):
        return replace(self, may_be_unbound=True)

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
        return UnknownType(may_be_unbound=self.may_be_unbound or left.may_be_unbound)

    def join_integer(self, left):
        return self.join_unknown(left)

    def join_boolean(self, left):
        return self.join_unknown(left)

    def join_float(self, left):
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

    def join_unbound(self, left):
        return self.with_unbound()

    def widen(self, new):
        return new

    def widen_integer(self, old):
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

    def exact_integer(self):
        return None

    def fixnum_domain(self):
        return None

    def fits(self, expected):
        return expected.accepts(self)

    def dispatch_fits(self, expected):
        return expected.accepts_dispatch(self)

    def accepts_dispatch(self, actual):
        return False

    def accepts(self, actual):
        return actual.fits_unknown(self)

    def annotation_conflict(self, contract):
        return False

    def annotated_integer(self, expected):
        return expected

    def fits_unknown(self, expected):
        return not self.may_be_unbound

    def fits_integer(self, expected):
        return False

    def fits_python_integer(self, expected):
        return False

    def fits_boolean(self, expected):
        return False

    def fits_float(self, expected):
        return False

    def fits_none(self, expected):
        return False

    def fits_immortal(self, expected):
        return False

    def fits_text(self, expected):
        return False

    def fits_bytes(self, expected):
        return False

    def fits_function(self, expected):
        return False


@dataclass(frozen=True, slots=True)
class UnknownType(ValueType):
    def accepts_dispatch(self, actual):
        return True


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

    def accept(self, visitor):
        return visitor.visit_IntegerType(self)

    def join(self, other):
        return other.join_integer(self)

    def join_integer(self, left):
        return IntegerType(
            min(left.minimum, self.minimum)
            if left.minimum is not None and self.minimum is not None
            else None,
            max(left.maximum, self.maximum)
            if left.maximum is not None and self.maximum is not None
            else None,
            may_be_unbound=left.may_be_unbound or self.may_be_unbound,
        )

    def widen(self, new):
        return new.widen_integer(self)

    def widen_integer(self, old):
        minimum, maximum = self.minimum, self.maximum
        if old.minimum is None or (minimum is not None and minimum < old.minimum):
            minimum = None
        if old.maximum is None or (maximum is not None and maximum > old.maximum):
            maximum = None
        return replace(self, minimum=minimum, maximum=maximum)

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

    def accepts(self, actual):
        return actual.fits_integer(self)

    def accepts_dispatch(self, actual):
        return self.is_fixnum and actual.fits_integer(self)

    def fits_integer(self, expected):
        return _bounds_fit(self, expected)

    def fits_python_integer(self, expected):
        return not self.may_be_unbound


@dataclass(frozen=True, slots=True)
class PythonIntegerType(PrimitiveType):
    """An unbounded Python int may be a boolean singleton or an integer value."""

    def accepts(self, actual):
        return actual.fits_python_integer(self)

    def fits_python_integer(self, expected):
        return not self.may_be_unbound

    def annotation_conflict(self, contract):
        return contract.integer_conflict(self)


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

    def accept(self, visitor):
        return visitor.visit_BooleanType(self)

    def join(self, other):
        return other.join_boolean(self)

    def join_integer(self, left):
        return ValueType.join_integer(self, left)

    def join_boolean(self, left):
        return BooleanType(
            min(left.minimum, self.minimum)
            if left.minimum is not None and self.minimum is not None
            else None,
            max(left.maximum, self.maximum)
            if left.maximum is not None and self.maximum is not None
            else None,
            may_be_unbound=left.may_be_unbound or self.may_be_unbound,
        )

    def join_none(self, left):
        return _join_immortal(self, left)

    def join_immortal(self, left):
        return _join_immortal(self, left)

    def widen(self, new):
        return new

    def widen_integer(self, old):
        return self

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

    def accepts(self, actual):
        return actual.fits_boolean(self)

    def fits_integer(self, expected):
        return False

    def accepts_dispatch(self, actual):
        return actual.fits_boolean(self)

    def fits_boolean(self, expected):
        return _bounds_fit(self, expected)

    def fits_immortal(self, expected):
        return self.is_immortal


@dataclass(frozen=True, slots=True)
class FloatType(PrimitiveType):
    def join(self, other):
        return other.join_float(self)

    def join_float(self, left):
        return FloatType(may_be_unbound=left.may_be_unbound or self.may_be_unbound)

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

    def unary(self, operator):
        if operator == ast.UnaryOperator.NOT:
            return BOOLEAN
        return FLOAT if operator != ast.UnaryOperator.INVERT else UNKNOWN

    def accepts(self, actual):
        return actual.fits_float(self)

    def fits_float(self, expected):
        return not self.may_be_unbound


@dataclass(frozen=True, slots=True)
class ImmortalType(ValueType):
    def annotation_conflict(self, contract):
        return True

    @property
    def is_immortal(self):
        return not self.may_be_unbound

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

    def fits_immortal(self, expected):
        return self.is_immortal


@dataclass(frozen=True, slots=True)
class NoneType(PrimitiveType):
    @property
    def is_immortal(self):
        return not self.may_be_unbound

    def accept(self, visitor):
        return visitor.visit_NoneType(self)

    def join(self, other):
        return other.join_none(self)

    def join_none(self, left):
        return NoneType(may_be_unbound=self.may_be_unbound or left.may_be_unbound)

    def join_boolean(self, left):
        return _join_immortal(self, left)

    def join_immortal(self, left):
        return _join_immortal(self, left)

    def accepts(self, actual):
        return actual.fits_none(self)

    def accepts_dispatch(self, actual):
        return actual.fits_none(self)

    def fits_none(self, expected):
        return not self.may_be_unbound

    def fits_immortal(self, expected):
        return self.is_immortal


@dataclass(frozen=True, slots=True)
class TextType(PrimitiveType):
    def join(self, other):
        return other.join_text(self)

    def join_text(self, left):
        return TextType(may_be_unbound=self.may_be_unbound or left.may_be_unbound)

    def accepts(self, actual):
        return actual.fits_text(self)

    def fits_text(self, expected):
        return not self.may_be_unbound


@dataclass(frozen=True, slots=True)
class BytesType(PrimitiveType):
    def join(self, other):
        return other.join_bytes(self)

    def join_bytes(self, left):
        return BytesType(may_be_unbound=self.may_be_unbound or left.may_be_unbound)

    def accepts(self, actual):
        return actual.fits_bytes(self)

    def fits_bytes(self, expected):
        return not self.may_be_unbound


@dataclass(frozen=True, slots=True)
class FunctionType(ValueType):
    def annotation_conflict(self, contract):
        return True

    def join(self, other):
        return other.join_function(self)

    def join_function(self, left):
        return FunctionType(may_be_unbound=self.may_be_unbound or left.may_be_unbound)

    def accepts(self, actual):
        return actual.fits_function(self)

    def fits_function(self, expected):
        return not self.may_be_unbound


@dataclass(frozen=True, slots=True)
class BottomType(ValueType):
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
    join_float = join_unknown
    join_none = join_unknown
    join_immortal = join_unknown
    join_text = join_unknown
    join_bytes = join_unknown
    join_function = join_unknown
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

    def unary(self, operator):
        return BOOLEAN if operator == ast.UnaryOperator.NOT else self

    def fits_unknown(self, expected):
        return False


@dataclass(frozen=True, slots=True)
class UnboundType(ValueType):
    may_be_unbound: bool = field(default=True, kw_only=True)

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
    join_float = join_unknown
    join_none = join_unknown
    join_immortal = join_unknown
    join_text = join_unknown
    join_bytes = join_unknown
    join_function = join_unknown
    join_unbound = join_unknown


UNKNOWN = UnknownType()
INTEGER = IntegerType()
PYTHON_INTEGER = PythonIntegerType()
BOOLEAN = BooleanType()
FLOAT = FloatType()
BOTTOM = BottomType()
UNBOUND = UnboundType()


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


def _join_immortal(left, right):
    return ImmortalType(may_be_unbound=left.may_be_unbound or right.may_be_unbound)


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
    elif operator in {ast.BinaryOperator.FLOOR_DIVIDE, ast.BinaryOperator.MODULO}:
        if lo == hi and rlo == rhi and rlo != 0:
            minimum = maximum = (
                lo // rlo if operator == ast.BinaryOperator.FLOOR_DIVIDE else lo % rlo
            )
    elif operator == ast.BinaryOperator.POWER:
        if rhi <= 32 and max(abs(lo), abs(hi)).bit_length() <= 128:
            bases = {lo, hi} | {value for value in (-1, 0, 1) if lo <= value <= hi}
            values = [
                base**exponent for base in bases for exponent in range(rlo, rhi + 1)
            ]
            minimum, maximum = min(values), max(values)
    return factory(minimum, maximum)
