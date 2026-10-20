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

    def visit_OptionalValueType(self, fact):
        return self.visit_ValueType(fact)

    def visit_ClassType(self, fact):
        return self.visit_ValueType(fact)

    def visit_UniqueObjectType(self, fact):
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

    def as_fact(self, wrap):
        return wrap(self)

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

    def join_optional_integer(self, left):
        return self.join_unknown(left)

    def join_optional_value(self, left):
        joined = left.inner.join(self)
        unbound = left.may_be_unbound or self.may_be_unbound
        if joined is UNKNOWN:
            return UNKNOWN_UNBOUND if unbound else UNKNOWN
        return OptionalValueType(joined, unbound)

    def join_class(self, left):
        return self.join_unknown(left)

    def join_unique_object(self, left):
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

    def widen_optional_integer(self, old):
        return self

    def widen_optional_value(self, old):
        return self

    def widen_class(self, old):
        return self

    def widen_constant(self):
        return self

    def as_optional(self, may_be_unbound: bool = False):
        return UNKNOWN_UNBOUND if (self.may_be_unbound or may_be_unbound) else UNKNOWN

    def update_nested_lists(self, elem_type):
        return self

    def _update_as_nested_list(self, elem_type):
        return self.update_nested_lists(elem_type)

    def update_nested_dicts(self, key_type, val_type):
        return self

    def _update_as_nested_dict(self, key_type, val_type):
        return self.update_nested_dicts(key_type, val_type)

    def propagate_to_parent_container(self, parent):
        return parent

    def widen_mutable_containers(self):
        return self

    def limit_container_depth(self, max_depth: int = 3):
        return self

    def runtime_class_type(self):
        return UNKNOWN

    def runtime_parent_class_type(self):
        return None

    def same_runtime_class(self, other) -> bool:
        return False

    def is_runtime_subclass_of(self, target) -> bool:
        return target is UNKNOWN or self.same_runtime_class(target)

    def is_class_object(self) -> bool | None:
        return None if (self.may_be_unbound or self is UNKNOWN) else False

    def allocate_instance_type(self):
        return UNKNOWN

    def general_instance_type(self):
        return UNKNOWN

    def parent_class_type(self):
        return UNKNOWN

    def is_subclass_of(self, target) -> bool | None:
        return None

    def check_subclass_from(self, source_instances) -> bool | None:
        return None

    def is_instance_of(self, target) -> bool | None:
        if self.may_be_unbound:
            return None
        return self.runtime_class_type().is_subclass_of(target)

    def is_same_unique_object(self, identity: object) -> bool:
        return False

    def compare_identity_with(self, left, operator):
        return BOOLEAN

    def compare_class_identity(self, left, is_same: bool):
        return BOOLEAN

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
        }:
            return right.compare_identity_with(self, operator)
        if operator in {
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

    def narrow_identity(self, target, is_same: bool):
        if self.may_be_unbound:
            return self
        if target.fits_none(NONE_TYPE):
            return self.narrow_none(is_same)
        cmp = self.comparison(
            ast.ComparisonOperator.IS if is_same else ast.ComparisonOperator.IS_NOT,
            target,
        )
        if cmp is BOOLEAN_FALSE:
            return BOTTOM
        if is_same and target is not UNKNOWN and not target.may_be_unbound:
            if self.fits(target):
                return self
            if target.fits(self):
                return target
        return self

    def narrow_by_runtime_class(self, target_cls, is_same: bool):
        if self.may_be_unbound:
            return self
        my_cls = self.runtime_class_type()
        if my_cls is UNKNOWN:
            return self
        narrowed_cls = my_cls.narrow_identity(target_cls, is_same)
        if narrowed_cls is BOTTOM:
            return BOTTOM
        if narrowed_cls == my_cls:
            return self
        return narrowed_cls.allocate_instance_type()

    def exact_integer(self):
        return None

    def exact_length(self) -> int | None:
        return None

    def codepoint_at(self, index: int):
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

    def getslice_type(self):
        return UNKNOWN

    def iterate_type(self):
        return UNKNOWN

    def flow_iterate_type(self):
        return self.iterate_type()

    def is_empty_iterator(self) -> bool:
        return False

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

    def method_call(
        self, name: str, arg_types: tuple, has_keywords: bool, storage_methods=None
    ):
        return (
            self.method_return_type(
                name, len(arg_types), has_keywords, storage_methods
            ),
            None,
            None,
            False,
            True,
        )

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

    def fits_class(self, expected=None):
        return False

    def fits_unique_object(self, expected=None):
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

    def narrow_by_runtime_class(self, target_cls, is_same: bool):
        if not self.may_be_unbound and is_same:
            narrowed = target_cls.general_instance_type()
            if narrowed is not UNKNOWN:
                return narrowed
        return self

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

    def runtime_parent_class_type(self):
        return NONE_TYPE

    def same_runtime_class(self, other) -> bool:
        return other is UNKNOWN


UNKNOWN = UnknownType()
UNKNOWN_UNBOUND = UnknownType(may_be_unbound=True)


@dataclass(frozen=True, slots=True)
class PrimitiveType(ValueType):
    def annotation_conflict(self, contract):
        return not self.fits(contract.fact)

    @property
    def may_run_user_code(self):
        return False

    def runtime_parent_class_type(self):
        return CLASS_OBJECT_TYPE

    def comparison(self, operator, right):
        if operator in {
            ast.ComparisonOperator.IS,
            ast.ComparisonOperator.IS_NOT,
        }:
            return right.compare_identity_with(self, operator)
        if operator in {
            ast.ComparisonOperator.IN,
            ast.ComparisonOperator.NOT_IN,
        }:
            return BOOLEAN
        return right.compare_primitive(self)

    def compare_primitive(self, left):
        return BOOLEAN

    def narrow_none(self, is_none: bool):
        if self.may_be_unbound:
            return self
        return BOTTOM if is_none else self


@dataclass(frozen=True, slots=True)
class IntegerType(PrimitiveType):
    minimum: int | None = None
    maximum: int | None = None
    may_be_unbound: bool = False

    def runtime_class_type(self):
        return UNKNOWN if self.may_be_unbound else ClassType((self,))

    def same_runtime_class(self, other) -> bool:
        return other.fits_integer(INTEGER)

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

    def join_optional_integer(self, left):
        joined = left.integer.join_integer(self.as_bound())
        return OptionalIntegerType(
            joined, may_be_unbound=left.may_be_unbound or self.may_be_unbound
        )

    def as_optional(self, may_be_unbound: bool = False):
        return OptionalIntegerType(
            self.as_bound(), may_be_unbound=self.may_be_unbound or may_be_unbound
        )

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
        elif operator == ast.ComparisonOperator.NOT_EQUAL:
            if minimum == bound and maximum == bound:
                return BOTTOM
            if minimum == bound:
                minimum = bound + 1
            elif maximum == bound:
                maximum = bound - 1
        if minimum is not None and maximum is not None and minimum > maximum:
            return BOTTOM
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
        if self.is_fixnum and self.minimum is not None and self.maximum is not None:
            return self.maximum - self.minimum
        return 2**64

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

    def runtime_class_type(self):
        if self.may_be_unbound:
            return UNKNOWN
        return ClassType((IntegerType(self.minimum, self.maximum), BOOLEAN))

    def same_runtime_class(self, other) -> bool:
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

    def join_optional_integer(self, left):
        return self.join_unknown(left)

    def as_optional(self, may_be_unbound: bool = False):
        return OptionalValueType(
            self.as_bound(), may_be_unbound=self.may_be_unbound or may_be_unbound
        )

    def widen_constant(self):
        return (
            PythonIntegerType(may_be_unbound=True)
            if self.may_be_unbound
            else PYTHON_INTEGER
        )

    def narrow(self, operator, bound, truth):
        narrowed = super().narrow(operator, bound, truth)
        if narrowed is BOTTOM:
            return BOTTOM
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

    def runtime_class_type(self):
        return UNKNOWN if self.may_be_unbound else CLASS_BOOL_TYPE

    def runtime_parent_class_type(self):
        return CLASS_INT_TYPE

    def same_runtime_class(self, other) -> bool:
        return other.fits_boolean(BOOLEAN)

    def is_runtime_subclass_of(self, target) -> bool:
        return (
            target is UNKNOWN
            or target.fits_boolean(BOOLEAN)
            or target.fits_integer(INTEGER)
        )

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

    def join_optional_integer(self, left):
        return self.join_unknown(left)

    def as_optional(self, may_be_unbound: bool = False):
        return IMMORTAL_UNBOUND if (self.may_be_unbound or may_be_unbound) else IMMORTAL

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
        narrowed = super().narrow(operator, bound, truth)
        if narrowed is BOTTOM:
            return BOTTOM
        if (
            narrowed.minimum == 0
            and narrowed.maximum == 0
            and not narrowed.may_be_unbound
        ):
            return BOOLEAN_FALSE
        if (
            narrowed.minimum == 1
            and narrowed.maximum == 1
            and not narrowed.may_be_unbound
        ):
            return BOOLEAN_TRUE
        return narrowed

    def exact_integer(self):
        return None

    def fixnum_domain(self):
        return None

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

    def runtime_class_type(self):
        return UNKNOWN if self.may_be_unbound else CLASS_FLOAT_TYPE

    def same_runtime_class(self, other) -> bool:
        return other.fits_float(FLOAT)

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

    def as_optional(self, may_be_unbound: bool = False):
        return OptionalValueType(
            FLOAT, may_be_unbound=self.may_be_unbound or may_be_unbound
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

    def runtime_class_type(self):
        return UNKNOWN if self.may_be_unbound else ClassType((INTEGER, BOOLEAN, FLOAT))

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

    def runtime_class_type(self):
        return UNKNOWN if self.may_be_unbound else CLASS_NONE_TYPE

    def same_runtime_class(self, other) -> bool:
        return other.fits_none(NONE_TYPE)

    @property
    def is_immortal(self):
        return not self.may_be_unbound

    @property
    def needs_release(self):
        return False

    def accept(self, visitor):
        return visitor.visit_NoneType(self)

    def truth_value(self) -> bool | None:
        return None if self.may_be_unbound else False

    def narrow_none(self, is_none: bool):
        if self.may_be_unbound:
            return self
        return NONE_TYPE if is_none else BOTTOM

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

    def join_optional_integer(self, left):
        unbound = left.may_be_unbound or self.may_be_unbound
        if unbound == left.may_be_unbound:
            return left
        return OptionalIntegerType(left.integer, unbound)

    def join_optional_value(self, left):
        unbound = left.may_be_unbound or self.may_be_unbound
        if unbound == left.may_be_unbound:
            return left
        return OptionalValueType(left.inner, unbound)

    def join_class(self, left):
        return ClassType(
            left.instance_types,
            may_be_none=True,
            may_be_unbound=left.may_be_unbound or self.may_be_unbound,
        )

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

    def runtime_class_type(self):
        return UNKNOWN if self.may_be_unbound else CLASS_STR_TYPE

    def same_runtime_class(self, other) -> bool:
        return other.fits_text(TEXT_TYPE)

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

    def exact_length(self) -> int | None:
        return (
            len(self.literal)
            if (not self.may_be_unbound and self.literal is not None)
            else None
        )

    def codepoint_at(self, index: int):
        if self.may_be_unbound or self.literal is None:
            return None
        idx = index + len(self.literal) if index < 0 else index
        if 0 <= idx < len(self.literal):
            cp = ord(self.literal[idx])
            return IntegerType(cp, cp)
        return None

    def truth_value(self) -> bool | None:
        if self.may_be_unbound or self.literal is None:
            return None
        return bool(self.literal)

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

    def as_optional(self, may_be_unbound: bool = False):
        return OptionalValueType(
            self.as_bound(), may_be_unbound=self.may_be_unbound or may_be_unbound
        )

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

    def getslice_type(self):
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

    def runtime_class_type(self):
        return UNKNOWN if self.may_be_unbound else CLASS_BYTES_TYPE

    def same_runtime_class(self, other) -> bool:
        return other.fits_bytes(BYTES_TYPE)

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

    def as_optional(self, may_be_unbound: bool = False):
        return OptionalValueType(
            BYTES_TYPE, may_be_unbound=self.may_be_unbound or may_be_unbound
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

    def getslice_type(self):
        return UNKNOWN if self.may_be_unbound else BYTES_TYPE

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

    def runtime_class_type(self):
        return UNKNOWN if self.may_be_unbound else CLASS_FUNCTION_TYPE

    def runtime_parent_class_type(self):
        return CLASS_OBJECT_TYPE

    def same_runtime_class(self, other) -> bool:
        return other.fits_function(FUNCTION_TYPE)

    def annotation_conflict(self, contract):
        return True

    @property
    def is_heap_object(self):
        return not self.may_be_unbound

    def truth_value(self) -> bool | None:
        return None if self.may_be_unbound else True

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

    def runtime_class_type(self):
        return UNKNOWN if self.may_be_unbound else CLASS_TUPLE_TYPE

    def runtime_parent_class_type(self):
        return CLASS_OBJECT_TYPE

    def same_runtime_class(self, other) -> bool:
        return other.fits_tuple(TupleType())

    def exact_length(self) -> int | None:
        return None if self.may_be_unbound else self.arity

    def annotation_conflict(self, contract):
        return False if self.may_be_unbound else contract.tuple_conflict(self)

    @property
    def is_heap_object(self):
        return not self.may_be_unbound

    def truth_value(self) -> bool | None:
        if self.may_be_unbound or self.arity is None:
            return None
        return self.arity > 0

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
                    l_el.join(r_el).limit_container_depth(2)
                    for l_el, r_el in zip(left.elements, self.elements)
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
                widen_fact(o_el, n_el).limit_container_depth(2)
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

    def as_optional(self, may_be_unbound: bool = False):
        return OptionalValueType(
            self.as_bound(), may_be_unbound=self.may_be_unbound or may_be_unbound
        )

    def limit_container_depth(self, max_depth: int = 3):
        if self.elements is None:
            return self
        if max_depth <= 0:
            return TupleType(self.arity, None, self.may_be_unbound)
        limited = tuple(el.limit_container_depth(max_depth - 1) for el in self.elements)
        if all(el is UNKNOWN for el in limited):
            return TupleType(self.arity, None, self.may_be_unbound)
        if limited == self.elements:
            return self
        return TupleType(self.arity, limited, self.may_be_unbound)

    def update_nested_lists(self, elem_type):
        if self.elements is None:
            return self
        new_elems = tuple(
            el._update_as_nested_list(elem_type).limit_container_depth(2)
            for el in self.elements
        )
        if new_elems == self.elements:
            return self
        return TupleType(self.arity, new_elems, self.may_be_unbound)

    def update_nested_dicts(self, key_type, val_type):
        if self.elements is None:
            return self
        new_elems = tuple(
            el._update_as_nested_dict(key_type, val_type).limit_container_depth(2)
            for el in self.elements
        )
        if new_elems == self.elements:
            return self
        return TupleType(self.arity, new_elems, self.may_be_unbound)

    def widen_mutable_containers(self):
        if self.elements is None:
            return self
        new_elems = tuple(el.widen_mutable_containers() for el in self.elements)
        if new_elems == self.elements:
            return self
        return TupleType(self.arity, new_elems, self.may_be_unbound)

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
            if self.arity is None and len(self.elements) == 1:
                if index.fits(PYTHON_INTEGER):
                    return self.elements[0]
                return UNKNOWN
            idx = index.exact_integer()
            if idx is not None:
                if idx < 0:
                    idx += len(self.elements)
                if 0 <= idx < len(self.elements):
                    return self.elements[idx]
            if len(self.elements) > 0 and index.fits(PYTHON_INTEGER):
                lo, hi = index.minimum, index.maximum
                if (
                    lo is not None
                    and hi is not None
                    and 0 <= lo <= hi < len(self.elements)
                ):
                    sub = self.elements[lo : hi + 1]
                    joined = sub[0]
                    for element in sub[1:]:
                        joined = joined.join(element)
                    return joined
                joined = self.elements[0]
                for element in self.elements[1:]:
                    joined = joined.join(element)
                return joined
        return UNKNOWN

    def getslice_type(self):
        if self.may_be_unbound:
            return UNKNOWN
        if self.elements:
            joined = self.elements[0]
            for element in self.elements[1:]:
                joined = joined.join(element)
            return TupleType(None, None if joined is UNKNOWN else (joined,))
        return TupleType()

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
        if (
            not self.may_be_unbound
            and self.arity is None
            and self.elements is not None
            and len(self.elements) == 1
            and 0 < arity <= 16
        ):
            return TupleType(arity, self.elements * arity)
        return TupleType(arity)

    def check_subclass_from(self, source_instances) -> bool | None:
        if self.may_be_unbound or self.elements is None:
            return None
        if not self.elements:
            return False
        outcomes = set()
        for src in source_instances:
            any_true = False
            any_unknown = False
            for elem in self.elements:
                sub = elem.check_subclass_from((src,))
                if sub is True:
                    any_true = True
                    break
                if sub is None:
                    any_unknown = True
            if any_true:
                outcomes.add(True)
            elif any_unknown:
                return None
            else:
                outcomes.add(False)
        if len(outcomes) == 1:
            return next(iter(outcomes))
        return None

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

    def method_return_type(
        self, name: str, arity: int, has_keywords: bool, storage_methods=None
    ):
        if self.may_be_unbound or storage_methods is None:
            return UNKNOWN
        return storage_methods.get(("TupleStorage", name), UNKNOWN)


@dataclass(frozen=True, slots=True)
class ListType(ValueType):
    """Exact builtin list storage."""

    element: ValueType = UNKNOWN
    may_be_unbound: bool = False

    def runtime_class_type(self):
        return UNKNOWN if self.may_be_unbound else CLASS_LIST_TYPE

    def runtime_parent_class_type(self):
        return CLASS_OBJECT_TYPE

    def same_runtime_class(self, other) -> bool:
        return other.fits_list(LIST_TYPE)

    def exact_length(self) -> int | None:
        return 0 if (not self.may_be_unbound and self.element is BOTTOM) else None

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

    def truth_value(self) -> bool | None:
        if not self.may_be_unbound and self.element is BOTTOM:
            return False
        return None

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

    def as_optional(self, may_be_unbound: bool = False):
        return OptionalValueType(
            self.as_bound(), may_be_unbound=self.may_be_unbound or may_be_unbound
        )

    def limit_container_depth(self, max_depth: int = 3):
        if self.element is BOTTOM or self.element is UNKNOWN:
            return self
        if max_depth <= 0:
            return LIST_TYPE if not self.may_be_unbound else ListType(UNKNOWN, True)
        limited = self.element.limit_container_depth(max_depth - 1)
        if limited is UNKNOWN and not self.may_be_unbound:
            return LIST_TYPE
        if limited == self.element:
            return self
        return ListType(limited, self.may_be_unbound)

    def update_nested_lists(self, elem_type):
        if self.element is BOTTOM or self.element is UNKNOWN:
            return self
        new_elem = self.element._update_as_nested_list(elem_type).limit_container_depth(
            2
        )
        if new_elem == self.element:
            return self
        return ListType(new_elem, self.may_be_unbound)

    def _update_as_nested_list(self, elem_type):
        return self.update_nested_lists(elem_type).with_element(elem_type)

    def update_nested_dicts(self, key_type, val_type):
        if self.element is BOTTOM or self.element is UNKNOWN:
            return self
        new_elem = self.element._update_as_nested_dict(
            key_type, val_type
        ).limit_container_depth(2)
        if new_elem == self.element:
            return self
        return ListType(new_elem, self.may_be_unbound)

    def propagate_to_parent_container(self, parent):
        return parent.update_nested_lists(self.element)

    def widen_constant(self):
        return LIST_TYPE if not self.may_be_unbound else ListType(UNKNOWN, True)

    def widen_mutable_containers(self):
        if self.element is BOTTOM or self.element is UNKNOWN:
            return self
        new_elem = self.element.widen_constant().widen_mutable_containers()
        if new_elem == self.element:
            return self
        return ListType(new_elem, self.may_be_unbound)

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
            element = left.element.join(self.element).limit_container_depth(2)
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
        element = widen_fact(old.element, self.element).limit_container_depth(2)
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

    def getslice_type(self):
        return UNKNOWN if self.may_be_unbound else self

    def iterate_type(self):
        if self.may_be_unbound:
            return UNKNOWN
        return IteratorType(UNKNOWN if self.element is BOTTOM else self.element)

    def flow_iterate_type(self):
        if self.may_be_unbound:
            return UNKNOWN
        return IteratorType(self.element)

    def unpacked_tuple_type(self, arity: int):
        elem = (
            UNKNOWN if (self.may_be_unbound or self.element is BOTTOM) else self.element
        )
        if elem is not UNKNOWN and 0 < arity <= 16:
            return TupleType(arity, (elem,) * arity)
        return TupleType(arity)

    def with_element(self, element):
        elem = element.as_bound().limit_container_depth(2)
        new_elem = (
            elem
            if self.element is BOTTOM
            else self.element.join(elem).limit_container_depth(2)
        )
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

    def method_return_type(
        self, name: str, arity: int, has_keywords: bool, storage_methods=None
    ):
        if self.may_be_unbound:
            return UNKNOWN
        if name == "copy" and self.element is not UNKNOWN:
            return ListType(self.element)
        if (
            name == "pop"
            and arity <= 1
            and not has_keywords
            and self.element is not BOTTOM
        ):
            return self.element
        if storage_methods is not None:
            return storage_methods.get(("ListStorage", name), UNKNOWN)
        return UNKNOWN

    def method_call(
        self, name: str, arg_types: tuple, has_keywords: bool, storage_methods=None
    ):
        if self.may_be_unbound:
            return UNKNOWN, None, None, False, True
        arity = len(arg_types)
        if not has_keywords:
            if name == "append" and arity == 1:
                may_call = arg_types[0].may_be_unbound
                return (
                    NONE_TYPE if not may_call else UNKNOWN,
                    self.with_element(arg_types[0]),
                    0,
                    False,
                    may_call,
                )
            if name == "insert" and arity == 2:
                return (
                    self.method_return_type(name, arity, has_keywords, storage_methods),
                    self.with_element(arg_types[1]),
                    1,
                    False,
                    True,
                )
            if name == "extend" and arity == 1:
                elem_type = arg_types[0].iterate_type().next_item_type()
                return (
                    self.method_return_type(name, arity, has_keywords, storage_methods),
                    self.with_element(elem_type),
                    None,
                    False,
                    True,
                )
        return (
            self.method_return_type(name, arity, has_keywords, storage_methods),
            None,
            None,
            False,
            True,
        )


@dataclass(frozen=True, slots=True)
class DictType(ValueType):
    """Exact builtin dict storage."""

    key: ValueType = UNKNOWN
    value: ValueType = UNKNOWN
    may_be_unbound: bool = False

    def runtime_class_type(self):
        return UNKNOWN if self.may_be_unbound else CLASS_DICT_TYPE

    def runtime_parent_class_type(self):
        return CLASS_OBJECT_TYPE

    def same_runtime_class(self, other) -> bool:
        return other.fits_dict(DICT_TYPE)

    def exact_length(self) -> int | None:
        return (
            0
            if (not self.may_be_unbound and self.key is BOTTOM and self.value is BOTTOM)
            else None
        )

    def annotation_conflict(self, contract):
        return True

    @property
    def is_heap_object(self):
        return not self.may_be_unbound

    def truth_value(self) -> bool | None:
        if not self.may_be_unbound and self.key is BOTTOM and self.value is BOTTOM:
            return False
        return None

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

    def as_optional(self, may_be_unbound: bool = False):
        return OptionalValueType(
            self.as_bound(), may_be_unbound=self.may_be_unbound or may_be_unbound
        )

    def limit_container_depth(self, max_depth: int = 3):
        if self.key is UNKNOWN and self.value is UNKNOWN:
            return self
        if max_depth <= 0:
            return (
                DICT_TYPE
                if not self.may_be_unbound
                else DictType(UNKNOWN, UNKNOWN, True)
            )
        lk = (
            self.key
            if self.key is BOTTOM
            else self.key.limit_container_depth(max_depth - 1)
        )
        lv = (
            self.value
            if self.value is BOTTOM
            else self.value.limit_container_depth(max_depth - 1)
        )
        if lk is UNKNOWN and lv is UNKNOWN and not self.may_be_unbound:
            return DICT_TYPE
        if lk == self.key and lv == self.value:
            return self
        return DictType(lk, lv, self.may_be_unbound)

    def update_nested_lists(self, elem_type):
        if self.value is BOTTOM or self.value is UNKNOWN:
            return self
        new_val = self.value._update_as_nested_list(elem_type).limit_container_depth(2)
        if new_val == self.value:
            return self
        return DictType(self.key, new_val, self.may_be_unbound)

    def update_nested_dicts(self, key_type, val_type):
        if self.value is BOTTOM or self.value is UNKNOWN:
            return self
        new_val = self.value._update_as_nested_dict(
            key_type, val_type
        ).limit_container_depth(2)
        if new_val == self.value:
            return self
        return DictType(self.key, new_val, self.may_be_unbound)

    def _update_as_nested_dict(self, key_type, val_type):
        return self.update_nested_dicts(key_type, val_type).with_entry(
            key_type, val_type
        )

    def propagate_to_parent_container(self, parent):
        return parent.update_nested_dicts(self.key, self.value)

    def widen_constant(self):
        return (
            DICT_TYPE if not self.may_be_unbound else DictType(UNKNOWN, UNKNOWN, True)
        )

    def widen_mutable_containers(self):
        new_key = (
            self.key
            if self.key is BOTTOM or self.key is UNKNOWN
            else self.key.widen_constant().widen_mutable_containers()
        )
        new_val = (
            self.value
            if self.value is BOTTOM or self.value is UNKNOWN
            else self.value.widen_constant().widen_mutable_containers()
        )
        if new_key == self.key and new_val == self.value:
            return self
        return DictType(new_key, new_val, self.may_be_unbound)

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
            key = left.key.join(self.key).limit_container_depth(2)
        if left.value is BOTTOM:
            value = self.value
        elif self.value is BOTTOM:
            value = left.value
        else:
            value = left.value.join(self.value).limit_container_depth(2)
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
        key = widen_fact(old.key, self.key).limit_container_depth(2)
        value = widen_fact(old.value, self.value).limit_container_depth(2)
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
        k = key.as_bound().limit_container_depth(2)
        v = value.as_bound().limit_container_depth(2)
        new_key = k if self.key is BOTTOM else self.key.join(k).limit_container_depth(2)
        new_val = (
            v if self.value is BOTTOM else self.value.join(v).limit_container_depth(2)
        )
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
        joined = self.value.join(default)
        if (
            joined is UNKNOWN
            and default.fits_none(NONE_TYPE)
            and self.value is not UNKNOWN
        ):
            return self.value.as_optional()
        return joined

    def iterate_type(self):
        if self.may_be_unbound:
            return UNKNOWN
        return IteratorType(UNKNOWN if self.key is BOTTOM else self.key)

    def flow_iterate_type(self):
        if self.may_be_unbound:
            return UNKNOWN
        return IteratorType(self.key)

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

    def method_return_type(
        self, name: str, arity: int, has_keywords: bool, storage_methods=None
    ):
        if self.may_be_unbound:
            return UNKNOWN
        if name == "get" and arity == 1 and not has_keywords:
            return self.dict_get_type(NONE_TYPE)
        if name == "copy" and arity == 0 and not has_keywords:
            return (
                DICT_TYPE
                if (self.key is UNKNOWN and self.value is UNKNOWN)
                else DictType(self.key, self.value)
            )
        if (
            name == "pop"
            and arity == 1
            and not has_keywords
            and self.value is not BOTTOM
        ):
            return self.value
        if storage_methods is not None:
            return storage_methods.get(("dict", name), UNKNOWN)
        return UNKNOWN

    def method_call(
        self, name: str, arg_types: tuple, has_keywords: bool, storage_methods=None
    ):
        if self.may_be_unbound:
            return UNKNOWN, None, None, False, True
        arity = len(arg_types)
        if not has_keywords:
            if name == "setdefault" and arity in (1, 2):
                default_type = arg_types[1] if arity == 2 else NONE_TYPE
                updated = self.with_entry(arg_types[0], default_type)
                return (
                    self.method_return_type(name, arity, has_keywords, storage_methods),
                    updated,
                    1 if arity == 2 else None,
                    True,
                    True,
                )
            if name == "get" and arity in (1, 2):
                default_type = arg_types[1] if arity == 2 else NONE_TYPE
                res_type = self.dict_get_type(default_type)
                may_call = arg_types[0].may_run_user_code or res_type is UNKNOWN
                return res_type, None, None, True, may_call
            if name == "items" and arity == 0:
                iter_type = (
                    IteratorType(TupleType(2, (BOTTOM, BOTTOM)))
                    if (self.key is BOTTOM and self.value is BOTTOM)
                    else self.dict_items_iterator_type()
                )
                return iter_type, None, None, False, iter_type is UNKNOWN
            if name == "keys" and arity == 0:
                iter_type = (
                    IteratorType(BOTTOM)
                    if (self.key is BOTTOM and self.value is BOTTOM)
                    else self.dict_keys_iterator_type()
                )
                return iter_type, None, None, False, iter_type is UNKNOWN
            if name == "values" and arity == 0:
                iter_type = (
                    IteratorType(BOTTOM)
                    if (self.key is BOTTOM and self.value is BOTTOM)
                    else self.dict_values_iterator_type()
                )
                return iter_type, None, None, False, iter_type is UNKNOWN
        return (
            self.method_return_type(name, arity, has_keywords, storage_methods),
            None,
            None,
            False,
            True,
        )


@dataclass(frozen=True, slots=True)
class IteratorType(ValueType):
    """Iterator with element type information."""

    element: ValueType = UNKNOWN
    may_be_unbound: bool = False

    def runtime_class_type(self):
        return UNKNOWN if self.may_be_unbound else CLASS_ITERATOR_TYPE

    def runtime_parent_class_type(self):
        return CLASS_OBJECT_TYPE

    def is_empty_iterator(self) -> bool:
        return not self.may_be_unbound and (
            self.element is BOTTOM or self.element == TupleType(2, (BOTTOM, BOTTOM))
        )

    def same_runtime_class(self, other) -> bool:
        return other.fits_iterator(ITERATOR_TYPE)

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

    def limit_container_depth(self, max_depth: int = 3):
        if self.element is BOTTOM or self.element is UNKNOWN:
            return self
        if max_depth <= 0:
            return (
                ITERATOR_TYPE
                if not self.may_be_unbound
                else IteratorType(UNKNOWN, True)
            )
        limited = self.element.limit_container_depth(max_depth - 1)
        if limited is UNKNOWN and not self.may_be_unbound:
            return ITERATOR_TYPE
        if limited == self.element:
            return self
        return IteratorType(limited, self.may_be_unbound)

    def update_nested_lists(self, elem_type):
        if self.element is BOTTOM or self.element is UNKNOWN:
            return self
        new_elem = self.element._update_as_nested_list(elem_type).limit_container_depth(
            2
        )
        if new_elem == self.element:
            return self
        return IteratorType(new_elem, self.may_be_unbound)

    def update_nested_dicts(self, key_type, val_type):
        if self.element is BOTTOM or self.element is UNKNOWN:
            return self
        new_elem = self.element._update_as_nested_dict(
            key_type, val_type
        ).limit_container_depth(2)
        if new_elem == self.element:
            return self
        return IteratorType(new_elem, self.may_be_unbound)

    def widen_mutable_containers(self):
        if self.element is BOTTOM or self.element is UNKNOWN:
            return self
        new_elem = self.element.widen_mutable_containers()
        if new_elem == self.element:
            return self
        return IteratorType(new_elem, self.may_be_unbound)

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
            element = left.element.join(self.element).limit_container_depth(2)
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
        element = widen_fact(old.element, self.element).limit_container_depth(2)
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

    def accept(self, visitor):
        return visitor.visit_BottomType(self)

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
    join_optional_integer = join_unknown
    join_optional_value = join_unknown
    join_class = join_unknown
    join_unique_object = join_unknown
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

    def accept(self, visitor):
        return visitor.visit_UnboundType(self)

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
    join_optional_integer = join_unknown
    join_optional_value = join_unknown
    join_class = join_unknown
    join_unique_object = join_unknown
    join_unbound = join_unknown


@dataclass(frozen=True, slots=True)
class OptionalIntegerType(ValueType):
    integer: IntegerType = field(default_factory=IntegerType)
    may_be_unbound: bool = False

    def runtime_class_type(self):
        if self.may_be_unbound:
            return UNKNOWN
        return ClassType((self.integer, NONE_TYPE))

    def accept(self, visitor):
        return visitor.visit_OptionalIntegerType(self)

    @property
    def is_immortal(self):
        return not self.may_be_unbound and self.integer.is_fixnum

    @property
    def needs_release(self):
        return self.may_be_unbound or self.integer.needs_release

    @property
    def may_run_user_code(self):
        return False

    def as_bound(self):
        if not self.may_be_unbound:
            return self
        return OptionalIntegerType(self.integer, False)

    def with_unbound(self):
        if self.may_be_unbound:
            return self
        return OptionalIntegerType(self.integer, True)

    def join(self, other):
        return other.join_optional_integer(self)

    def join_optional_integer(self, left):
        if left is self:
            return self
        joined = left.integer.join_integer(self.integer)
        unbound = left.may_be_unbound or self.may_be_unbound
        return OptionalIntegerType(joined, unbound)

    def join_integer(self, left):
        joined = left.as_bound().join_integer(self.integer)
        unbound = left.may_be_unbound or self.may_be_unbound
        return OptionalIntegerType(joined, unbound)

    def join_none(self, left):
        unbound = left.may_be_unbound or self.may_be_unbound
        if unbound == self.may_be_unbound:
            return self
        return OptionalIntegerType(self.integer, unbound)

    def widen(self, new):
        return new.widen_optional_integer(self)

    def widen_optional_integer(self, old):
        integer = widen_fact(old.integer, self.integer)
        unbound = old.may_be_unbound or self.may_be_unbound
        if integer == self.integer and unbound == self.may_be_unbound:
            return self
        return OptionalIntegerType(
            IntegerType(integer.minimum, integer.maximum), unbound
        )

    def narrow_none(self, is_none: bool):
        if self.may_be_unbound:
            return self
        return NONE_TYPE if is_none else self.integer


@dataclass(frozen=True, slots=True)
class OptionalValueType(ValueType):
    inner: ValueType = UNKNOWN
    may_be_unbound: bool = False

    def runtime_class_type(self):
        if self.may_be_unbound:
            return UNKNOWN
        return self.inner.runtime_class_type().join(CLASS_NONE_TYPE)

    def accept(self, visitor):
        return visitor.visit_OptionalValueType(self)

    @property
    def may_run_user_code(self):
        return self.may_be_unbound or self.inner.may_run_user_code

    def as_bound(self):
        if not self.may_be_unbound:
            return self
        return OptionalValueType(self.inner, False)

    def with_unbound(self):
        if self.may_be_unbound:
            return self
        return OptionalValueType(self.inner, True)

    def narrow_none(self, is_none: bool):
        if self.may_be_unbound:
            return self
        return NONE_TYPE if is_none else self.inner

    def narrow_identity(self, target, is_same: bool):
        if self.may_be_unbound:
            return self
        if target.fits_none(NONE_TYPE):
            return self.narrow_none(is_same)
        if (
            is_same
            and target.comparison(ast.ComparisonOperator.IS, NONE_TYPE) is BOOLEAN_FALSE
        ):
            return self.inner.narrow_identity(target, True)
        if not is_same:
            narrowed_inner = self.inner.narrow_identity(target, False)
            if narrowed_inner is BOTTOM:
                return NONE_TYPE
            if narrowed_inner != self.inner:
                return OptionalValueType(narrowed_inner, False)
        return super().narrow_identity(target, is_same)

    def join(self, other):
        return other.join_optional_value(self)

    def join_optional_value(self, left):
        if left is self:
            return self
        joined = left.inner.join(self.inner)
        unbound = left.may_be_unbound or self.may_be_unbound
        if joined is UNKNOWN:
            return UNKNOWN_UNBOUND if unbound else UNKNOWN
        return OptionalValueType(joined, unbound)

    def join_none(self, left):
        unbound = left.may_be_unbound or self.may_be_unbound
        if unbound == self.may_be_unbound:
            return self
        return OptionalValueType(self.inner, unbound)

    def _join_with_inner(self, left):
        joined = left.as_bound().join(self.inner)
        unbound = left.may_be_unbound or self.may_be_unbound
        if joined is UNKNOWN:
            return UNKNOWN_UNBOUND if unbound else UNKNOWN
        return OptionalValueType(joined, unbound)

    join_float = _join_with_inner
    join_text = _join_with_inner
    join_bytes = _join_with_inner
    join_tuple = _join_with_inner
    join_list = _join_with_inner
    join_dict = _join_with_inner
    join_iterator = _join_with_inner
    join_python_integer = _join_with_inner

    def widen(self, new):
        return new.widen_optional_value(self)

    def widen_optional_value(self, old):
        inner = widen_fact(old.inner, self.inner)
        unbound = old.may_be_unbound or self.may_be_unbound
        if inner == self.inner and unbound == self.may_be_unbound:
            return self
        if inner is UNKNOWN:
            return UNKNOWN_UNBOUND if unbound else UNKNOWN
        return OptionalValueType(inner, unbound)

    def limit_container_depth(self, max_depth: int = 3):
        limited = self.inner.limit_container_depth(max_depth)
        if limited == self.inner:
            return self
        return OptionalValueType(limited, self.may_be_unbound)

    def update_nested_lists(self, elem_type):
        new_inner = self.inner._update_as_nested_list(elem_type).limit_container_depth(
            3
        )
        if new_inner == self.inner:
            return self
        return OptionalValueType(new_inner, self.may_be_unbound)

    def update_nested_dicts(self, key_type, val_type):
        new_inner = self.inner._update_as_nested_dict(
            key_type, val_type
        ).limit_container_depth(3)
        if new_inner == self.inner:
            return self
        return OptionalValueType(new_inner, self.may_be_unbound)

    def widen_mutable_containers(self):
        new_inner = self.inner.widen_mutable_containers()
        if new_inner == self.inner:
            return self
        return OptionalValueType(new_inner, self.may_be_unbound)


def _merge_class_instances(
    left_instances: tuple[ValueType, ...], right_instances: tuple[ValueType, ...]
) -> tuple[ValueType, ...]:
    merged: list[ValueType] = list(left_instances)
    for r_inst in right_instances:
        found = False
        for i, l_inst in enumerate(merged):
            if l_inst.same_runtime_class(r_inst):
                merged[i] = UNKNOWN if l_inst is UNKNOWN else l_inst.join(r_inst)
                found = True
                break
        if not found:
            merged.append(r_inst)
    return tuple(merged)


@dataclass(frozen=True, slots=True)
class ClassType(ValueType):
    """Compile-time type of a class object, parameterized by the ``ValueType``s
    of instances of the classes it may represent."""

    instance_types: tuple[ValueType, ...] = ()
    may_be_none: bool = False
    may_be_unbound: bool = False

    def accept(self, visitor):
        return visitor.visit_ClassType(self)

    @property
    def is_heap_object(self):
        return not self.may_be_unbound and not self.may_be_none

    @property
    def may_run_user_code(self):
        return False

    def truth_value(self) -> bool | None:
        return None if (self.may_be_unbound or self.may_be_none) else True

    def is_class_object(self) -> bool | None:
        return None if (self.may_be_unbound or self.may_be_none) else True

    def fits_class(self, expected=None):
        return not self.may_be_unbound

    def runtime_class_type(self):
        return UNKNOWN if (self.may_be_unbound or self.may_be_none) else CLASS_TYPE_TYPE

    def runtime_parent_class_type(self):
        return None if (self.may_be_unbound or self.may_be_none) else CLASS_OBJECT_TYPE

    def same_runtime_class(self, other) -> bool:
        return (
            not self.may_be_unbound
            and not self.may_be_none
            and other.fits_class()
            and not other.may_be_none
        )

    def general_instance_type(self):
        if (
            not self.may_be_unbound
            and not self.may_be_none
            and len(self.instance_types) == 1
        ):
            inst = self.instance_types[0]
            if inst.fits_class():
                return ANY_CLASS_TYPE
            return inst.widen_mutable_containers().widen_constant()
        return UNKNOWN

    def allocate_instance_type(self):
        if self.may_be_unbound or self.may_be_none or not self.instance_types:
            return UNKNOWN
        result = self.instance_types[0]
        for inst in self.instance_types[1:]:
            result = result.join(inst)
        return result

    def parent_class_type(self):
        if self.may_be_unbound or self.may_be_none or not self.instance_types:
            return UNKNOWN
        result: ValueType = BOTTOM
        for inst in self.instance_types:
            parent = inst.runtime_parent_class_type()
            if parent is None:
                return UNKNOWN
            result = result.join(parent)
        return result

    def is_subclass_of(self, target) -> bool | None:
        if self.may_be_unbound or self.may_be_none or not self.instance_types:
            return None
        return target.check_subclass_from(self.instance_types)

    def check_subclass_from(self, source_instances) -> bool | None:
        if self.may_be_unbound or self.may_be_none or not self.instance_types:
            return None
        outcomes = set()
        for src in source_instances:
            if len(self.instance_types) == 1:
                outcomes.add(src.is_runtime_subclass_of(self.instance_types[0]))
            else:
                sub_outcomes = {
                    src.is_runtime_subclass_of(t) for t in self.instance_types
                }
                if len(sub_outcomes) == 1:
                    outcomes.add(next(iter(sub_outcomes)))
                else:
                    return None
        if len(outcomes) == 1:
            return next(iter(outcomes))
        return None

    def join(self, other):
        return other.join_class(self)

    def join_class(self, left):
        if left is self:
            return self
        unbound = left.may_be_unbound or self.may_be_unbound
        may_be_none = left.may_be_none or self.may_be_none
        if not left.instance_types or not self.instance_types:
            return UNKNOWN_UNBOUND if unbound else UNKNOWN
        merged = _merge_class_instances(left.instance_types, self.instance_types)
        if len(merged) > 8:
            return UNKNOWN_UNBOUND if unbound else UNKNOWN
        return ClassType(merged, may_be_none=may_be_none, may_be_unbound=unbound)

    def join_none(self, left):
        return ClassType(
            self.instance_types,
            may_be_none=True,
            may_be_unbound=left.may_be_unbound or self.may_be_unbound,
        )

    def widen(self, new):
        return new.widen_class(self)

    def widen_class(self, old):
        if len(self.instance_types) > 6:
            return UNKNOWN_UNBOUND if self.may_be_unbound else UNKNOWN
        return self

    def comparison(self, operator, right):
        if operator in {
            ast.ComparisonOperator.IS,
            ast.ComparisonOperator.IS_NOT,
        }:
            is_same = operator is ast.ComparisonOperator.IS
            if right.fits_none(NONE_TYPE) and not self.may_be_unbound:
                if not self.may_be_none:
                    return BOOLEAN_FALSE if is_same else BOOLEAN_TRUE
                if not self.instance_types:
                    return BOOLEAN_TRUE if is_same else BOOLEAN_FALSE
                return BOOLEAN
            return right.compare_class_identity(self, is_same)
        return super().comparison(operator, right)

    def compare_identity_with(self, left, operator):
        is_same = operator is ast.ComparisonOperator.IS
        if left.fits_none(NONE_TYPE) and not self.may_be_unbound:
            if not self.may_be_none:
                return BOOLEAN_FALSE if is_same else BOOLEAN_TRUE
            if not self.instance_types:
                return BOOLEAN_TRUE if is_same else BOOLEAN_FALSE
            return BOOLEAN
        return left.compare_class_identity(self, is_same)

    def compare_class_identity(self, left, is_same: bool):
        if (
            left.may_be_unbound
            or self.may_be_unbound
            or not left.instance_types
            or not self.instance_types
        ):
            return BOOLEAN
        if (
            not left.may_be_none
            and not self.may_be_none
            and len(left.instance_types) == 1
            and len(self.instance_types) == 1
        ):
            same = left.instance_types[0].same_runtime_class(self.instance_types[0])
            res = same if is_same else not same
            return BOOLEAN_TRUE if res else BOOLEAN_FALSE
        overlap = any(
            l_inst.same_runtime_class(r_inst)
            for l_inst in left.instance_types
            for r_inst in self.instance_types
        ) or (left.may_be_none and self.may_be_none)
        if not overlap:
            return BOOLEAN_FALSE if is_same else BOOLEAN_TRUE
        return BOOLEAN

    def narrow_none(self, is_none: bool):
        if self.may_be_unbound:
            return self
        if is_none:
            return NONE_TYPE if self.may_be_none else BOTTOM
        if not self.may_be_none:
            return self
        if not self.instance_types:
            return BOTTOM
        return ClassType(self.instance_types, may_be_none=False, may_be_unbound=False)

    def narrow_identity(self, target, is_same: bool):
        if self.may_be_unbound:
            return self
        if target.fits_none(NONE_TYPE):
            return self.narrow_none(is_same)
        if (
            target.fits_class()
            and not target.may_be_none
            and len(target.instance_types) == 1
        ):
            if not self.instance_types and not self.may_be_none:
                return (
                    ClassType(
                        target.instance_types,
                        may_be_none=False,
                        may_be_unbound=False,
                    )
                    if is_same
                    else self
                )
            t_inst = target.instance_types[0]
            if is_same:
                matching = tuple(
                    inst
                    for inst in self.instance_types
                    if inst.same_runtime_class(t_inst)
                )
                return (
                    BOTTOM
                    if not matching
                    else ClassType(matching, may_be_none=False, may_be_unbound=False)
                )
            remaining = tuple(
                inst
                for inst in self.instance_types
                if not inst.same_runtime_class(t_inst)
            )
            if not remaining and not self.may_be_none:
                return BOTTOM
            if not remaining and self.may_be_none:
                return NONE_TYPE
            return ClassType(
                remaining, may_be_none=self.may_be_none, may_be_unbound=False
            )
        return super().narrow_identity(target, is_same)


@dataclass(frozen=True, slots=True)
class UniqueObjectType(ValueType):
    """Compile-time type of a unique singleton object allocated at module
    initialization (such as ``_binding_missing()`` or ``object()``)."""

    identity: object = None
    may_be_unbound: bool = False

    def accept(self, visitor):
        return visitor.visit_UniqueObjectType(self)

    @property
    def is_heap_object(self):
        return not self.may_be_unbound

    @property
    def may_run_user_code(self):
        return False

    def truth_value(self) -> bool | None:
        return None if self.may_be_unbound else True

    def fits_unique_object(self, expected=None):
        return not self.may_be_unbound

    def is_same_unique_object(self, identity: object) -> bool:
        return (
            not self.may_be_unbound
            and self.identity is not None
            and self.identity == identity
        )

    def join(self, other):
        return other.join_unique_object(self)

    def join_unique_object(self, left):
        if left.identity == self.identity and self.identity is not None:
            unbound = left.may_be_unbound or self.may_be_unbound
            return UniqueObjectType(self.identity, may_be_unbound=unbound)
        return self.join_unknown(left)

    def comparison(self, operator, right):
        if operator in {
            ast.ComparisonOperator.IS,
            ast.ComparisonOperator.IS_NOT,
        }:
            return self.compare_identity_with(right, operator)
        return super().comparison(operator, right)

    def compare_identity_with(self, left, operator):
        if self.may_be_unbound or left.may_be_unbound or self.identity is None:
            return BOOLEAN
        is_same = operator is ast.ComparisonOperator.IS
        if left.is_same_unique_object(self.identity):
            return BOOLEAN_TRUE if is_same else BOOLEAN_FALSE
        if left is not UNKNOWN:
            return BOOLEAN_FALSE if is_same else BOOLEAN_TRUE
        return BOOLEAN

    def narrow_none(self, is_none: bool):
        if self.may_be_unbound:
            return self
        return BOTTOM if is_none else self


def widen_fact(previous: ValueType, current: ValueType) -> ValueType:
    return previous.widen(previous.join(current)).limit_container_depth(3)


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
ANY_CLASS_TYPE = ClassType()
CLASS_TYPE_TYPE = ClassType((ANY_CLASS_TYPE,))
CLASS_INT_TYPE = ClassType((INTEGER,))
CLASS_BOOL_TYPE = ClassType((BOOLEAN,))
CLASS_FLOAT_TYPE = ClassType((FLOAT,))
CLASS_STR_TYPE = ClassType((TEXT_TYPE,))
CLASS_BYTES_TYPE = ClassType((BYTES_TYPE,))
CLASS_NONE_TYPE = ClassType((NONE_TYPE,))
CLASS_FUNCTION_TYPE = ClassType((FUNCTION_TYPE,))
CLASS_TUPLE_TYPE = ClassType((TupleType(),))
CLASS_LIST_TYPE = ClassType((EMPTY_LIST_TYPE,))
CLASS_DICT_TYPE = ClassType((EMPTY_DICT_TYPE,))
CLASS_ITERATOR_TYPE = ClassType((ITERATOR_TYPE,))
CLASS_OBJECT_TYPE = ClassType((UNKNOWN,))

BUILTIN_CLASS_TYPES: dict[str, ClassType] = {
    "int": CLASS_INT_TYPE,
    "bool": CLASS_BOOL_TYPE,
    "float": CLASS_FLOAT_TYPE,
    "str": CLASS_STR_TYPE,
    "bytes": CLASS_BYTES_TYPE,
    "tuple": CLASS_TUPLE_TYPE,
    "list": CLASS_LIST_TYPE,
    "dict": CLASS_DICT_TYPE,
    "object": CLASS_OBJECT_TYPE,
    "type": CLASS_TYPE_TYPE,
}
STORAGE_CLASS_TYPES: dict[str, ValueType] = {
    "TextStorage": TEXT_TYPE,
    "BytesStorage": BYTES_TYPE,
    "TupleStorage": TupleType(),
    "ListStorage": LIST_TYPE,
    "dict": DICT_TYPE,
    "_Table": DICT_TYPE,
}


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


class _SerializeValueType(ValueTypeVisitor):
    def visit_ValueType(self, fact):
        return {"kind": "unknown", "unbound": fact.may_be_unbound}

    def visit_UnknownType(self, fact):
        return {"kind": "unknown", "unbound": fact.may_be_unbound}

    def visit_IntegerType(self, fact):
        return {
            "kind": "int",
            "min": fact.minimum,
            "max": fact.maximum,
            "unbound": fact.may_be_unbound,
        }

    def visit_PythonIntegerType(self, fact):
        return {
            "kind": "py_int",
            "min": fact.minimum,
            "max": fact.maximum,
            "unbound": fact.may_be_unbound,
        }

    def visit_BooleanType(self, fact):
        return {
            "kind": "bool",
            "min": fact.minimum,
            "max": fact.maximum,
            "unbound": fact.may_be_unbound,
        }

    def visit_FloatType(self, fact):
        return {"kind": "float", "unbound": fact.may_be_unbound}

    def visit_RealType(self, fact):
        return {"kind": "real", "unbound": fact.may_be_unbound}

    def visit_ImmortalType(self, fact):
        return {"kind": "immortal", "unbound": fact.may_be_unbound}

    def visit_NoneType(self, fact):
        return {"kind": "none", "unbound": fact.may_be_unbound}

    def visit_TextType(self, fact):
        return {
            "kind": "text",
            "literal": fact.literal,
            "unbound": fact.may_be_unbound,
        }

    def visit_BytesType(self, fact):
        return {"kind": "bytes", "unbound": fact.may_be_unbound}

    def visit_FunctionType(self, fact):
        return {"kind": "function", "unbound": fact.may_be_unbound}

    def visit_TupleType(self, fact):
        return {
            "kind": "tuple",
            "arity": fact.arity,
            "elements": (
                None
                if fact.elements is None
                else [el.to_dict() for el in fact.elements]
            ),
            "unbound": fact.may_be_unbound,
        }

    def visit_ListType(self, fact):
        return {
            "kind": "list",
            "element": fact.element.to_dict(),
            "unbound": fact.may_be_unbound,
        }

    def visit_DictType(self, fact):
        return {
            "kind": "dict",
            "key": fact.key.to_dict(),
            "value": fact.value.to_dict(),
            "unbound": fact.may_be_unbound,
        }

    def visit_IteratorType(self, fact):
        return {
            "kind": "iterator",
            "element": fact.element.to_dict(),
            "unbound": fact.may_be_unbound,
        }

    def visit_BottomType(self, fact):
        return {"kind": "bottom", "unbound": fact.may_be_unbound}

    def visit_UnboundType(self, fact):
        return {"kind": "unbound"}

    def visit_OptionalIntegerType(self, fact):
        return {
            "kind": "opt_int",
            "integer": fact.integer.to_dict(),
            "unbound": fact.may_be_unbound,
        }

    def visit_OptionalValueType(self, fact):
        return {
            "kind": "opt_val",
            "inner": fact.inner.to_dict(),
            "unbound": fact.may_be_unbound,
        }

    def visit_ClassType(self, fact):
        return {
            "kind": "class",
            "instances": [inst.to_dict() for inst in fact.instance_types],
            "none": fact.may_be_none,
            "unbound": fact.may_be_unbound,
        }

    def visit_UniqueObjectType(self, fact):
        return {
            "kind": "unique_obj",
            "id": str(fact.identity),
            "unbound": fact.may_be_unbound,
        }


_SERIALIZE_VALUE_TYPE = _SerializeValueType()


def value_type_from_dict(data: dict | None) -> ValueType:
    if not data:
        return UNKNOWN
    kind = data.get("kind", "unknown")
    unbound = bool(data.get("unbound", False))
    if kind == "unknown":
        return UNKNOWN_UNBOUND if unbound else UNKNOWN
    if kind == "int":
        lo = data.get("min")
        hi = data.get("max")
        if lo is None and hi is None and not unbound:
            return INTEGER
        return IntegerType(lo, hi, may_be_unbound=unbound)
    if kind == "py_int":
        lo = data.get("min")
        hi = data.get("max")
        if lo is None and hi is None and not unbound:
            return PYTHON_INTEGER
        return PythonIntegerType(lo, hi, may_be_unbound=unbound)
    if kind == "bool":
        lo = data.get("min", 0)
        hi = data.get("max", 1)
        if lo == 0 and hi == 1 and not unbound:
            return BOOLEAN
        if lo == 0 and hi == 0 and not unbound:
            return BOOLEAN_FALSE
        if lo == 1 and hi == 1 and not unbound:
            return BOOLEAN_TRUE
        return BooleanType(lo, hi, may_be_unbound=unbound)
    if kind == "float":
        return FloatType(may_be_unbound=unbound) if unbound else FLOAT
    if kind == "real":
        return RealType(may_be_unbound=unbound) if unbound else REAL
    if kind == "immortal":
        return IMMORTAL_UNBOUND if unbound else IMMORTAL
    if kind == "none":
        return NoneType(may_be_unbound=unbound) if unbound else NONE_TYPE
    if kind == "text":
        lit = data.get("literal")
        if lit is None and not unbound:
            return TEXT_TYPE
        return TextType(lit, may_be_unbound=unbound)
    if kind == "bytes":
        return BytesType(may_be_unbound=unbound) if unbound else BYTES_TYPE
    if kind == "function":
        return FunctionType(may_be_unbound=unbound) if unbound else FUNCTION_TYPE
    if kind == "tuple":
        arity = data.get("arity")
        raw_elements = data.get("elements")
        elements = (
            None
            if raw_elements is None
            else tuple(value_type_from_dict(el) for el in raw_elements)
        )
        return TupleType(arity, elements, may_be_unbound=unbound)
    if kind == "list":
        elem = value_type_from_dict(data.get("element"))
        if elem is UNKNOWN and not unbound:
            return LIST_TYPE
        if elem is BOTTOM and not unbound:
            return EMPTY_LIST_TYPE
        return ListType(elem, may_be_unbound=unbound)
    if kind == "dict":
        key = value_type_from_dict(data.get("key"))
        value = value_type_from_dict(data.get("value"))
        if key is UNKNOWN and value is UNKNOWN and not unbound:
            return DICT_TYPE
        if key is BOTTOM and value is BOTTOM and not unbound:
            return EMPTY_DICT_TYPE
        return DictType(key, value, may_be_unbound=unbound)
    if kind == "iterator":
        elem = value_type_from_dict(data.get("element"))
        if elem is UNKNOWN and not unbound:
            return ITERATOR_TYPE
        return IteratorType(elem, may_be_unbound=unbound)
    if kind == "bottom":
        return BOTTOM
    if kind == "unbound":
        return UNBOUND
    if kind == "opt_int":
        inner = value_type_from_dict(data.get("integer"))
        return OptionalIntegerType(
            IntegerType(inner.minimum, inner.maximum), may_be_unbound=unbound
        )
    if kind == "opt_val":
        inner = value_type_from_dict(data.get("inner"))
        if inner is UNKNOWN:
            return UNKNOWN_UNBOUND if unbound else UNKNOWN
        return OptionalValueType(inner, may_be_unbound=unbound)
    if kind == "class":
        instances = tuple(
            value_type_from_dict(inst) for inst in data.get("instances", ())
        )
        return ClassType(
            instances,
            may_be_none=bool(data.get("none", False)),
            may_be_unbound=unbound,
        )
    if kind == "unique_obj":
        return UniqueObjectType(data.get("id"), may_be_unbound=unbound)
    return UNKNOWN
