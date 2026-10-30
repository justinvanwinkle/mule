"""Core ValueType lattice, scalar/class/function/union types, and ValueType serialization visitor."""

import json
from bisect import bisect_left, bisect_right
from dataclasses import dataclass, replace

from mule import ast

FIXNUM_MIN = -(2**62)
FIXNUM_MAX = 2**62 - 1


def _widen_lower_threshold(value: int, thresholds: tuple[int, ...]) -> int | None:
    if not thresholds:
        return None
    idx = bisect_right(thresholds, value) - 1
    return thresholds[idx] if idx >= 0 else None


def _widen_upper_threshold(value: int, thresholds: tuple[int, ...]) -> int | None:
    if not thresholds:
        return None
    idx = bisect_left(thresholds, value)
    return thresholds[idx] if idx < len(thresholds) else None


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

    def visit_ByteStorageType(self, fact):
        return self.visit_PrimitiveType(fact)

    def visit_ByteArrayType(self, fact):
        return self.visit_PrimitiveType(fact)

    def visit_WordVectorType(self, fact):
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

    def visit_UnionType(self, fact):
        return self.visit_ValueType(fact)

    def visit_ClassType(self, fact):
        return self.visit_ValueType(fact)

    def visit_UniqueObjectType(self, fact):
        return self.visit_ValueType(fact)

    def visit_InstanceType(self, fact):
        return self.visit_ValueType(fact)

    def visit_SetType(self, fact):
        return self.visit_InstanceType(fact)


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
    def is_exact_tuple(self):
        return False

    @property
    def is_byte_storage(self):
        return False

    @property
    def is_interned_text(self) -> bool:
        return False

    @property
    def needs_release(self):
        return True

    @property
    def may_run_user_code(self):
        return True

    def has_reflexive_equality(self) -> bool:
        return False

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

    def callable_target(self):
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
        return other.join_alternatives(self)

    def union_members(self):
        return (self.as_bound(),)

    def join_alternatives(self, left):
        return _union_result((left, self))

    def join_unknown(self, left):
        return (
            UNKNOWN_UNBOUND if self.may_be_unbound or left.may_be_unbound else UNKNOWN
        )

    def join_integer(self, left):
        return self.join_alternatives(left)

    def join_boolean(self, left):
        return self.join_alternatives(left)

    def join_python_integer(self, left):
        return self.join_alternatives(left)

    def join_float(self, left):
        return self.join_alternatives(left)

    def join_real(self, left):
        return self.join_alternatives(left)

    def join_none(self, left):
        return self.join_alternatives(left)

    def join_immortal(self, left):
        return self.join_alternatives(left)

    def join_text(self, left):
        return self.join_alternatives(left)

    def join_bytes(self, left):
        return self.join_alternatives(left)

    def join_bytearray(self, left):
        return self.join_alternatives(left)

    def join_function(self, left):
        return self.join_alternatives(left)

    def join_tuple(self, left):
        return self.join_alternatives(left)

    def join_list(self, left):
        return self.join_alternatives(left)

    def join_dict(self, left):
        return self.join_alternatives(left)

    def join_iterator(self, left):
        return self.join_alternatives(left)

    def join_class(self, left):
        return self.join_alternatives(left)

    def join_unique_object(self, left):
        return self.join_alternatives(left)

    def join_instance(self, left):
        return self.join_alternatives(left)

    def join_set(self, left):
        return self.join_alternatives(left)

    def join_unbound(self, left):
        return self.with_unbound()

    def widen(self, new, thresholds: tuple[int, ...] = ()):
        return new

    def widen_integer(self, old, thresholds: tuple[int, ...] = ()):
        return self

    def widen_tuple(self, old, thresholds: tuple[int, ...] = ()):
        return self

    def widen_list(self, old, thresholds: tuple[int, ...] = ()):
        return self

    def widen_dict(self, old, thresholds: tuple[int, ...] = ()):
        return self

    def widen_set(self, old, thresholds: tuple[int, ...] = ()):
        return self

    def widen_iterator(self, old, thresholds: tuple[int, ...] = ()):
        return self

    def widen_class(self, old, thresholds: tuple[int, ...] = ()):
        return self

    def widen_constant(self):
        return self

    def widen_inferred_contract(self):
        return self.widen_constant()

    def as_optional(self, may_be_unbound: bool = False):
        result = union_type(self, NONE_TYPE)
        return result.with_unbound() if may_be_unbound else result

    def conservative_type(self):
        return self

    def limit_container_depth(self, max_depth: int = 4):
        return self

    def has_float_specialization(self) -> bool:
        return False

    def has_unspecialized_part(self) -> bool:
        return False

    def has_bottom_part(self) -> bool:
        return False

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

    def binary_bytearray(self, operator, left):
        return UNKNOWN

    def binary_tuple(self, operator, left):
        return UNKNOWN

    def binary_list(self, operator, left):
        return UNKNOWN

    def binary_set(self, operator, left):
        return UNKNOWN

    def construct_type(self, arg_types: tuple, keywords: dict):
        return self

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

    def compare_text(self, operator, left):
        return self.compare_primitive(left)

    def narrow(self, operator, bound, truth):
        return self

    def narrow_equality(self, other, is_equal: bool):
        return self

    def narrow_none(self, is_none: bool):
        if not self.may_be_unbound:
            classes = self.runtime_class_type()
            if (
                classes.comparison(ast.ComparisonOperator.IS, CLASS_NONE_TYPE)
                is BOOLEAN_FALSE
            ):
                return BOTTOM if is_none else self
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

    def exact_boolean(self) -> bool | None:
        return None

    def exact_length(self) -> int | None:
        return None

    def valid_index_bounds(self) -> tuple[int, int] | None:
        return None

    def codepoint_at(self, index: int):
        return None

    def singleton_text(self) -> str | None:
        return None

    def interned_text(self) -> str | None:
        return None

    def exact_text(self) -> str | None:
        """The one text value this type admits, when it is a known literal."""
        return None

    def fixnum_domain(self):
        return None

    def dispatch_domain(self):
        return self.fixnum_domain()

    def dispatch_rank(self) -> int:
        return 2**64

    def getitem_type(self, index):
        return UNKNOWN

    def flow_getitem_type(self, index):
        return self.getitem_type(index)

    def getslice_type(self, start=None, stop=None, step=None):
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
        return self._tuple_type(arity)

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

    def fits_union(self, expected):
        return any(self.fits(member) for member in expected.members)

    def may_fit_union(self, expected):
        return any(self.may_dispatch_fit(member) for member in expected.members)

    def dispatch_fits(self, expected):
        return expected.accepts_dispatch(self)

    def may_dispatch_fit(self, expected):
        return expected.accepts_possible_dispatch(self)

    def accepts_dispatch(self, actual):
        return False

    def accepts_possible_dispatch(self, actual):
        return self.accepts_dispatch(actual)

    def accepts(self, actual):
        return actual == self

    def annotation_conflict(self, contract):
        return False

    def annotated_integer(self, expected):
        return expected

    def annotated_list(self, expected):
        return expected.unspecialized_container()

    def annotated_dict(self, expected):
        return expected.unspecialized_container()

    def annotated_set(self, expected):
        return expected.unspecialized_container()

    def annotated_tuple(self, expected):
        return self._tuple_type(expected.arity)

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

    def fits_possible_float(self, expected):
        return self.fits_float(expected)

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

    def fits_possible_bytes(self, expected):
        return self.fits_bytes(expected)

    def fits_bytearray(self, expected=None):
        return False

    def fits_possible_bytearray(self, expected=None):
        return self.fits_bytearray(expected)

    def fits_function(self, expected):
        return False

    def fits_possible_function(self, expected):
        return False

    def fits_tuple(self, expected):
        return False

    def fits_tuple_dispatch(self, expected):
        return False

    def fits_possible_tuple(self, expected):
        return False

    def fits_list(self, expected=None):
        return False

    def fits_list_dispatch(self, expected):
        return False

    def fits_possible_list(self, expected=None):
        return False

    def fits_dict(self, expected=None):
        return False

    def fits_possible_dict(self, expected=None):
        return self.fits_dict(expected)

    def fits_set(self, expected=None):
        return False

    def fits_mutable_set(self) -> bool:
        return False

    def fits_possible_set(self, expected=None):
        return self.fits_set(expected)

    def fits_iterator(self, expected=None):
        return False

    def fits_class(self, expected=None):
        return False

    def fits_unique_object(self, expected=None):
        return False

    def fits_instance(self, expected):
        return False

    def fits_possible_instance(self, expected):
        return self.fits_instance(expected)

    def instance_class(self) -> str | None:
        """The identity of the user class every value of this type is an
        exact instance of, or None."""
        return None

    def proves_tuple_index(self, index):
        """Whether every value of this type is a bound tuple with an element
        at ``index``."""
        return False

    def fixed_slot_index(self, name: str) -> int | None:
        """A physical field position, requiring a runtime layout guard."""
        return None


@dataclass(frozen=True, slots=True)
class UnknownType(ValueType):
    may_be_unbound: bool = False

    def has_unspecialized_part(self) -> bool:
        return True

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

    def accepts(self, actual):
        return actual.fits_unknown(self)

    def fits_possible_integer(self, expected):
        return not self.may_be_unbound

    def fits_possible_boolean(self, expected):
        return not self.may_be_unbound

    def fits_possible_float(self, expected):
        return not self.may_be_unbound

    def fits_possible_none(self, expected):
        return not self.may_be_unbound

    def fits_possible_immortal(self, expected):
        return not self.may_be_unbound

    def fits_possible_tuple(self, expected):
        return not self.may_be_unbound

    def fits_possible_text(self, expected):
        return not self.may_be_unbound and expected.literal is None

    def fits_possible_bytes(self, expected):
        return not self.may_be_unbound

    def fits_possible_bytearray(self, expected):
        return not self.may_be_unbound

    def fits_possible_list(self, expected=None):
        return not self.may_be_unbound

    def fits_possible_dict(self, expected=None):
        return not self.may_be_unbound and (
            expected is None or (expected.key is UNKNOWN and expected.value is UNKNOWN)
        )

    def fits_possible_set(self, expected=None):
        return not self.may_be_unbound and (
            expected is None or expected.element is UNKNOWN
        )

    def fits_possible_instance(self, expected):
        return not self.may_be_unbound

    def fits_possible_function(self, expected):
        return not self.may_be_unbound

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

    def has_reflexive_equality(self) -> bool:
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
        return self.join_alternatives(left)

    def join_python_integer(self, left):
        return _join_python_integer(left, self)

    def widen(self, new, thresholds: tuple[int, ...] = ()):
        return new.widen_integer(self, thresholds)

    def widen_integer(self, old, thresholds: tuple[int, ...] = ()):
        minimum, maximum = self.minimum, self.maximum
        if old.minimum is None:
            minimum = None
        elif minimum is not None and minimum < old.minimum:
            minimum = _widen_lower_threshold(minimum, thresholds)
        if old.maximum is None:
            maximum = None
        elif maximum is not None and maximum > old.maximum:
            maximum = _widen_upper_threshold(maximum, thresholds)
        if minimum == self.minimum and maximum == self.maximum:
            return self
        if minimum is None and maximum is None and not self.may_be_unbound:
            return INTEGER
        return replace(self, minimum=minimum, maximum=maximum)

    def widen_constant(self):
        if (
            self.minimum is not None
            and self.maximum is not None
            and FIXNUM_MIN <= self.minimum <= self.maximum <= FIXNUM_MAX
        ):
            return (
                IntegerType(FIXNUM_MIN, FIXNUM_MAX, True)
                if self.may_be_unbound
                else _FIXNUM_INTEGER
            )
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
            return left.dispatch_domain()
        return UNKNOWN

    def binary_bytes(self, operator, left):
        if not left.may_be_unbound and operator in {
            ast.BinaryOperator.MULTIPLY,
            ast.BinaryOperator.MODULO,
        }:
            return left
        return UNKNOWN

    def binary_bytearray(self, operator, left):
        if not left.may_be_unbound and operator in {
            ast.BinaryOperator.MULTIPLY,
            ast.BinaryOperator.MODULO,
        }:
            return left
        return UNKNOWN

    def binary_list(self, operator, left):
        if (
            not left.may_be_unbound
            and not self.may_be_unbound
            and operator == ast.BinaryOperator.MULTIPLY
        ):
            return left
        return UNKNOWN

    def binary_tuple(self, operator, left):
        if (
            not left.may_be_unbound
            and not self.may_be_unbound
            and operator == ast.BinaryOperator.MULTIPLY
        ):
            return left.getslice_type()
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

    def narrow_equality(self, other, is_equal: bool):
        if (
            self.may_be_unbound
            or other.may_be_unbound
            or not other.fits(PYTHON_INTEGER)
        ):
            return self
        exact = other.exact_integer()
        if exact is not None:
            return self.narrow(
                ast.ComparisonOperator.EQUAL
                if is_equal
                else ast.ComparisonOperator.NOT_EQUAL,
                exact,
                True,
            )
        if is_equal:
            narrowed = self
            if other.minimum is not None:
                narrowed = narrowed.narrow(
                    ast.ComparisonOperator.GREATER_EQUAL, other.minimum, True
                )
            if other.maximum is not None and narrowed is not BOTTOM:
                narrowed = narrowed.narrow(
                    ast.ComparisonOperator.LESS_EQUAL, other.maximum, True
                )
            return narrowed
        return self

    def narrow_identity(self, other, is_same: bool):
        if (
            not self.may_be_unbound
            and not other.may_be_unbound
            and self.is_fixnum
            and other.fits(PYTHON_INTEGER)
            and other.is_fixnum
        ):
            return self.narrow_equality(other, is_same)
        return super().narrow_identity(other, is_same)

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

    def truth_value(self) -> bool | None:
        if self.may_be_unbound:
            return None
        if self.minimum == 0 and self.maximum == 0:
            return False
        if self.minimum == 1 and self.maximum == 1:
            return True
        return None

    def join(self, other):
        return other.join_boolean(self)

    def join_integer(self, left):
        return self.join_alternatives(left)

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

    def join_immortal(self, left):
        return _join_immortal(self, left)

    def widen(self, new, thresholds: tuple[int, ...] = ()):
        return new

    def widen_integer(self, old, thresholds: tuple[int, ...] = ()):
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

    def exact_boolean(self) -> bool | None:
        if self.may_be_unbound:
            return None
        if self.minimum == 1 and self.maximum == 1:
            return True
        if self.minimum == 0 and self.maximum == 0:
            return False
        return None

    def fixnum_domain(self):
        return None

    def dispatch_rank(self) -> int:
        return 2**64 if self.may_be_unbound else 0

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

    def annotation_conflict(self, contract):
        return False if self.may_be_unbound else contract.float_conflict(self)

    @property
    def is_heap_object(self):
        return not self.may_be_unbound

    def dispatch_domain(self):
        return None if self.may_be_unbound else FLOAT

    def dispatch_rank(self) -> int:
        return 2**64 if self.may_be_unbound else 0

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

    def accepts_dispatch(self, actual):
        return actual.fits_float(self)

    def accepts_possible_dispatch(self, actual):
        return actual.fits_possible_float(self)

    def fits_float(self, expected):
        return not self.may_be_unbound

    def has_float_specialization(self) -> bool:
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

    def fits_possible_integer(self, expected):
        return not self.may_be_unbound

    def fits_possible_boolean(self, expected):
        return not self.may_be_unbound

    def fits_possible_float(self, expected):
        return not self.may_be_unbound

    def annotation_conflict(self, contract):
        # Either integer or floating members may satisfy a numeric contract.
        return (
            False
            if self.may_be_unbound
            else (
                contract.integer_conflict(INTEGER)
                and contract.boolean_conflict(BOOLEAN)
                and contract.float_conflict(FLOAT)
            )
        )

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
        return (
            False
            if self.may_be_unbound
            else (
                contract.boolean_conflict(BOOLEAN) and contract.none_conflict(NONE_TYPE)
            )
        )

    @property
    def is_immortal(self):
        return not self.may_be_unbound

    @property
    def needs_release(self):
        return False

    @property
    def may_run_user_code(self):
        return False

    def has_reflexive_equality(self) -> bool:
        return not self.may_be_unbound

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

    def fits_possible_text(self, expected):
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

    def annotation_conflict(self, contract):
        return False if self.may_be_unbound else contract.none_conflict(self)

    @property
    def is_immortal(self):
        return not self.may_be_unbound

    @property
    def needs_release(self):
        return False

    def has_reflexive_equality(self) -> bool:
        return not self.may_be_unbound

    def dispatch_rank(self) -> int:
        return 2**64 if self.may_be_unbound else 0

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

    def join_immortal(self, left):
        return _join_immortal(self, left)

    def join_class(self, left):
        return self.join_alternatives(left)

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
class FunctionType(ValueType):
    """A Python function, optionally restricted to one compiled code body.

    Code identity says nothing about the function's globals, defaults, or
    closure values. Calls must continue to use the actual function object.
    """

    may_be_unbound: bool = False
    target: object | None = None

    def callable_target(self):
        return None if self.may_be_unbound else self.target

    def dispatch_domain(self):
        return self if self.target is not None and not self.may_be_unbound else None

    def dispatch_rank(self) -> int:
        return 0 if self.target is not None else 2**64

    def runtime_class_type(self):
        return UNKNOWN if self.may_be_unbound else CLASS_FUNCTION_TYPE

    def runtime_parent_class_type(self):
        return CLASS_OBJECT_TYPE

    def same_runtime_class(self, other) -> bool:
        return other.fits_function(FUNCTION_TYPE)

    def annotation_conflict(self, contract):
        return not self.may_be_unbound

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
        target = self.target if self.target is left.target else None
        unbound = self.may_be_unbound or left.may_be_unbound
        return (
            FunctionType(unbound, target)
            if unbound or target is not None
            else FUNCTION_TYPE
        )

    def accepts(self, actual):
        return actual.fits_function(self)

    def fits_function(self, expected):
        return (not self.may_be_unbound or expected.may_be_unbound) and (
            expected.target is None or self.target is expected.target
        )

    def fits_possible_function(self, expected):
        return not self.may_be_unbound and (
            self.target is None
            or expected.target is None
            or self.target is expected.target
        )

    def accepts_dispatch(self, actual):
        return actual.fits_function(self)

    def accepts_possible_dispatch(self, actual):
        return actual.fits_possible_function(self)


@dataclass(frozen=True, slots=True)
class BottomType(ValueType):
    may_be_unbound: bool = False

    def has_bottom_part(self) -> bool:
        return True

    def as_bound(self):
        return BOTTOM

    def with_unbound(self):
        return UNBOUND

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
    join_bytearray = join_unknown
    join_function = join_unknown
    join_tuple = join_unknown
    join_list = join_unknown
    join_dict = join_unknown
    join_set = join_unknown
    join_iterator = join_unknown
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
    binary_bytearray = binary_unknown
    binary_tuple = binary_unknown
    binary_list = binary_unknown
    binary_set = binary_unknown

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
    join_bytearray = join_unknown
    join_function = join_unknown
    join_tuple = join_unknown
    join_list = join_unknown
    join_dict = join_unknown
    join_set = join_unknown
    join_iterator = join_unknown
    join_class = join_unknown
    join_unique_object = join_unknown
    join_unbound = join_unknown


@dataclass(frozen=True, slots=True)
class UnionType(ValueType):
    """A bounded disjunction. Construct through ``union_type`` to normalize it."""

    members: frozenset[ValueType]
    may_be_unbound: bool = False

    def accept(self, visitor):
        return visitor.visit_UnionType(self)

    def union_members(self):
        return self.members

    def join(self, other):
        return union_type(self, other)

    def join_alternatives(self, left):
        return union_type(left, self)

    def map(self, operation):
        result = union_type(*(operation(member) for member in self.members))
        return result.join(UNBOUND) if self.may_be_unbound else result

    @property
    def is_immortal(self):
        return not self.may_be_unbound and all(
            m.is_immortal or m.is_fixnum for m in self.members
        )

    @property
    def is_heap_object(self):
        return not self.may_be_unbound and all(m.is_heap_object for m in self.members)

    @property
    def is_interned_text(self) -> bool:
        return not self.may_be_unbound and all(m.is_interned_text for m in self.members)

    @property
    def needs_release(self):
        return self.may_be_unbound or any(m.needs_release for m in self.members)

    @property
    def may_run_user_code(self):
        return self.may_be_unbound or any(m.may_run_user_code for m in self.members)

    def has_reflexive_equality(self) -> bool:
        return not self.may_be_unbound and all(
            m.has_reflexive_equality() for m in self.members
        )

    def fits(self, expected):
        return (not self.may_be_unbound or expected.may_be_unbound) and all(
            m.fits(expected.as_bound()) for m in self.members
        )

    def accepts(self, actual):
        return actual.fits_union(self)

    def fits_union(self, expected):
        return (not self.may_be_unbound or expected.may_be_unbound) and all(
            m.fits_union(expected) for m in self.members
        )

    def accepts_dispatch(self, actual):
        return actual.fits_union(self)

    def accepts_possible_dispatch(self, actual):
        return actual.may_fit_union(self)

    def dispatch_rank(self) -> int:
        return (
            2**64
            if self.may_be_unbound
            else max((m.dispatch_rank() for m in self.members), default=2**64)
        )

    def annotation_conflict(self, contract):
        return not self.may_be_unbound and all(
            m.annotation_conflict(contract) for m in self.members
        )

    def dispatch_fits(self, expected):
        return not self.may_be_unbound and all(
            m.dispatch_fits(expected) for m in self.members
        )

    def may_dispatch_fit(self, expected):
        return not self.may_be_unbound and any(
            m.may_dispatch_fit(expected) for m in self.members
        )

    def may_fit_union(self, expected):
        return not self.may_be_unbound and any(
            m.may_fit_union(expected) for m in self.members
        )

    def fits_integer(self, expected):
        return self._fits_family(expected, lambda member: member.fits_integer(member))

    def fits_python_integer(self, expected):
        return self._fits_family(
            expected, lambda member: member.fits_python_integer(member)
        )

    def fits_boolean(self, expected):
        return self._fits_family(expected, lambda member: member.fits_boolean(member))

    def fits_float(self, expected):
        return self._fits_family(expected, lambda member: member.fits_float(member))

    def fits_real(self, expected):
        return self._fits_family(expected, lambda member: member.fits_real(member))

    def fits_none(self, expected):
        return self._fits_family(expected, lambda member: member.fits_none(member))

    def fits_immortal(self, expected):
        return self.is_immortal

    def fits_text(self, expected):
        return self._fits_family(expected, lambda member: member.fits_text(member))

    def fits_bytes(self, expected):
        return self._fits_family(expected, lambda member: member.fits_bytes(member))

    def fits_bytearray(self, expected=None):
        return self._fits_family(expected, lambda member: member.fits_bytearray(member))

    def fits_function(self, expected):
        return self._fits_family(expected, lambda member: member.fits_function(member))

    def fits_possible_function(self, expected):
        return not self.may_be_unbound and any(
            member.fits_possible_function(expected) for member in self.members
        )

    def fits_tuple(self, expected):
        return self._fits_family(expected, lambda member: member.fits_tuple(member))

    def fits_tuple_dispatch(self, expected):
        return not self.may_be_unbound and all(
            member.fits_tuple_dispatch(expected) for member in self.members
        )

    def fits_list(self, expected=None):
        return self._fits_family(expected, lambda member: member.fits_list(member))

    def fits_list_dispatch(self, expected):
        return not self.may_be_unbound and all(
            member.fits_list_dispatch(expected) for member in self.members
        )

    def fits_dict(self, expected=None):
        return self._fits_family(expected, lambda member: member.fits_dict(member))

    def fits_set(self, expected=None):
        return self._fits_family(expected, lambda member: member.fits_set())

    def fits_mutable_set(self) -> bool:
        return not self.may_be_unbound and all(
            member.fits_mutable_set() for member in self.members
        )

    def fits_iterator(self, expected=None):
        return self._fits_family(expected, lambda member: member.fits_iterator(member))

    def fits_class(self, expected=None):
        return self._fits_family(expected, lambda member: member.fits_class(member))

    def fits_instance(self, expected):
        return self._fits_family(expected, lambda member: member.fits_instance(member))

    def _fits_family(self, expected, proof):
        # A family contract belongs to each concrete member, not to the union.
        # Only after proving the family may generic inclusion check the caller's
        # expected fact, which can itself be a heterogeneous union.
        return (
            not self.may_be_unbound
            and all(proof(member) for member in self.members)
            and (expected is None or self.fits(expected))
        )

    def fits_possible_integer(self, expected):
        return self._may_fit_family(
            expected, lambda member: member.fits_possible_integer(member)
        )

    def fits_possible_boolean(self, expected):
        return self._may_fit_family(
            expected, lambda member: member.fits_possible_boolean(member)
        )

    def fits_possible_float(self, expected):
        return self._may_fit_family(
            expected, lambda member: member.fits_possible_float(member)
        )

    def fits_possible_none(self, expected):
        return self._may_fit_family(
            expected, lambda member: member.fits_possible_none(member)
        )

    def fits_possible_immortal(self, expected):
        return self._may_fit_family(
            expected, lambda member: member.fits_possible_immortal(member)
        )

    def fits_possible_text(self, expected):
        return self._may_fit_family(
            expected, lambda member: member.fits_possible_text(member)
        )

    def fits_possible_bytes(self, expected):
        return self._may_fit_family(
            expected, lambda member: member.fits_possible_bytes(member)
        )

    def fits_possible_bytearray(self, expected):
        return self._may_fit_family(
            expected, lambda member: member.fits_possible_bytearray(member)
        )

    def fits_possible_tuple(self, expected):
        return self._may_fit_family(
            expected, lambda member: member.fits_possible_tuple(member)
        )

    def fits_possible_list(self, expected=None):
        return self._may_fit_family(
            expected, lambda member: member.fits_possible_list(member)
        )

    def fits_possible_dict(self, expected=None):
        return self._may_fit_family(
            expected, lambda member: member.fits_possible_dict(member)
        )

    def fits_possible_set(self, expected=None):
        return self._may_fit_family(
            expected, lambda member: member.fits_possible_set(expected)
        )

    def fits_possible_instance(self, expected):
        return self._may_fit_family(
            expected, lambda member: member.fits_possible_instance(member)
        )

    def _may_fit_family(self, expected, proof):
        return not self.may_be_unbound and any(
            proof(member) and (expected is None or member.may_dispatch_fit(expected))
            for member in self.members
        )

    def truth_value(self):
        values = {member.truth_value() for member in self.members}
        return values.pop() if not self.may_be_unbound and len(values) == 1 else None

    def narrow_none(self, is_none: bool):
        return (
            self if self.may_be_unbound else self.map(lambda m: m.narrow_none(is_none))
        )

    def narrow_identity(self, target, is_same: bool):
        return (
            self
            if self.may_be_unbound
            else self.map(lambda m: m.narrow_identity(target, is_same))
        )

    def narrow_by_runtime_class(self, target_cls, is_same: bool):
        return (
            self
            if self.may_be_unbound
            else self.map(lambda m: m.narrow_by_runtime_class(target_cls, is_same))
        )

    def narrow(self, operator, bound, truth):
        return (
            self
            if self.may_be_unbound
            else self.map(lambda m: m.narrow(operator, bound, truth))
        )

    def narrow_equality(self, other, is_equal: bool):
        return (
            self
            if self.may_be_unbound
            else self.map(lambda m: m.narrow_equality(other, is_equal))
        )

    def binary(self, operator, right):
        return (
            UNKNOWN
            if self.may_be_unbound
            else self.map(lambda m: m.binary(operator, right))
        )

    def binary_unknown(self, operator, left):
        return (
            UNKNOWN
            if self.may_be_unbound
            else self.map(lambda m: left.binary(operator, m))
        )

    binary_integer = binary_unknown
    binary_boolean = binary_unknown
    binary_float = binary_unknown
    binary_real = binary_unknown
    binary_text = binary_unknown
    binary_bytes = binary_unknown
    binary_bytearray = binary_unknown
    binary_tuple = binary_unknown
    binary_list = binary_unknown
    binary_set = binary_unknown

    def unary(self, operator):
        return UNKNOWN if self.may_be_unbound else self.map(lambda m: m.unary(operator))

    def comparison(self, operator, right):
        return (
            BOOLEAN
            if self.may_be_unbound
            else self.map(lambda m: m.comparison(operator, right))
        )

    def compare_identity_with(self, left, operator):
        return (
            BOOLEAN
            if self.may_be_unbound
            else self.map(lambda m: left.comparison(operator, m))
        )

    def compare_unknown(self, left):
        return self.map(lambda m: m.compare_unknown(left))

    def compare_primitive(self, left):
        return self.map(lambda m: m.compare_primitive(left))

    def compare_text(self, operator, left):
        return self.map(lambda m: m.compare_text(operator, left))

    def runtime_class_type(self):
        return (
            UNKNOWN
            if self.may_be_unbound
            else self.map(lambda m: m.runtime_class_type())
        )

    def instance_class(self):
        classes = {m.instance_class() for m in self.members if m is not NONE_TYPE}
        return classes.pop() if not self.may_be_unbound and len(classes) == 1 else None

    def fixed_slot_index(self, name: str) -> int | None:
        if self.may_be_unbound:
            return None
        positions = {member.fixed_slot_index(name) for member in self.members}
        return positions.pop() if len(positions) == 1 else None

    def dispatch_domain(self):
        if self.may_be_unbound:
            return None
        domains = []
        for member in self.members:
            if member is NONE_TYPE:
                domains.append(NONE_TYPE)
                continue
            domain = member.dispatch_domain()
            if domain is None:
                return None
            domains.append(domain)
        return union_type(*domains) if domains else None

    def getitem_type(self, index):
        return (
            UNKNOWN
            if self.may_be_unbound
            else self.map(lambda m: m.getitem_type(index))
        )

    def flow_getitem_type(self, index):
        return (
            UNKNOWN
            if self.may_be_unbound
            else self.map(lambda m: m.flow_getitem_type(index))
        )

    def getslice_type(self, start=None, stop=None, step=None):
        return (
            UNKNOWN
            if self.may_be_unbound
            else self.map(lambda m: m.getslice_type(start, stop, step))
        )

    def iterate_type(self):
        return UNKNOWN if self.may_be_unbound else self.map(lambda m: m.iterate_type())

    def flow_iterate_type(self):
        return (
            UNKNOWN
            if self.may_be_unbound
            else self.map(lambda m: m.flow_iterate_type())
        )

    def next_item_type(self):
        return (
            UNKNOWN if self.may_be_unbound else self.map(lambda m: m.next_item_type())
        )

    def valid_index_bounds(self) -> tuple[int, int] | None:
        if self.may_be_unbound:
            return None
        lo = None
        hi = None
        for member in self.members:
            bounds = member.valid_index_bounds()
            if bounds is None:
                return None
            lo = bounds[0] if lo is None else min(lo, bounds[0])
            hi = bounds[1] if hi is None else max(hi, bounds[1])
        return (lo, hi) if lo is not None and hi is not None else None

    def widen(self, new, thresholds: tuple[int, ...] = ()):
        members = []
        for member in new.union_members():
            previous = next(
                (m for m in self.members if m.same_runtime_class(member)), None
            )
            members.append(
                member if previous is None else widen_fact(previous, member, thresholds)
            )
        result = union_type(*members)
        return result.join(UNBOUND) if new.may_be_unbound else result

    def widen_constant(self):
        return self.map(lambda m: m.widen_constant())

    def widen_inferred_contract(self):
        return self.map(lambda m: m.widen_inferred_contract())

    def limit_container_depth(self, max_depth: int = 4):
        return self.map(lambda m: m.limit_container_depth(max_depth))

    def has_float_specialization(self) -> bool:
        return not self.may_be_unbound and any(
            m.has_float_specialization() for m in self.members
        )

    def has_unspecialized_part(self) -> bool:
        return any(m.has_unspecialized_part() for m in self.members)

    def has_bottom_part(self) -> bool:
        return any(m.has_bottom_part() for m in self.members)

    def conservative_type(self):
        return self.map(lambda m: m.conservative_type())


MAX_UNION_MEMBERS = 8


def union_type(*facts: ValueType) -> ValueType:
    """Normalize alternatives as SBCL's ``type-union`` does before retaining a set.

    The member budget bounds analysis work, not supported Python values. A
    wider disjunction safely loses precision to the existing unknown domain.
    """
    unbound = any(fact.may_be_unbound for fact in facts)
    members = []
    for fact in facts:
        for member in fact.union_members():
            if member is UNKNOWN:
                return UNKNOWN_UNBOUND if unbound else UNKNOWN
            if member is BOTTOM:
                continue
            if member not in members:
                members.append(member)
    while True:
        merged = False
        for index, left in enumerate(members):
            for following in range(index + 1, len(members)):
                right = members[following]
                combined = left.join(right)
                alternatives = tuple(combined.union_members())
                if len(alternatives) == 1:
                    members[index] = alternatives[0]
                    del members[following]
                    merged = True
                    break
            if merged:
                break
        if not merged:
            return _union_result(members, may_be_unbound=unbound)


def _union_result(facts, *, may_be_unbound=False):
    """Retain an unsimplified pair, or finish the common union simplifier.

    Pair methods use this builder when they cannot simplify. Calling the full
    simplifier there would recursively ask the same unmatched pair to join.
    """
    unbound = may_be_unbound or any(fact.may_be_unbound for fact in facts)
    members = frozenset(
        member
        for fact in facts
        for member in fact.union_members()
        if member is not BOTTOM
    )
    if UNKNOWN in members:
        return UNKNOWN_UNBOUND if unbound else UNKNOWN
    if not members:
        return UNBOUND if unbound else BOTTOM
    if len(members) > MAX_UNION_MEMBERS:
        return UNKNOWN_UNBOUND if unbound else UNKNOWN
    if len(members) == 1:
        member = next(iter(members))
        return member.with_unbound() if unbound else member
    return UnionType(members, unbound)


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
    may_be_unbound: bool = False

    def accept(self, visitor):
        return visitor.visit_ClassType(self)

    @property
    def is_heap_object(self):
        return not self.may_be_unbound

    @property
    def may_run_user_code(self):
        return False

    def truth_value(self) -> bool | None:
        return None if self.may_be_unbound else True

    def is_class_object(self) -> bool | None:
        return None if self.may_be_unbound else True

    def method_return_type(
        self, name: str, arity: int, has_keywords: bool, storage_methods=None
    ):
        if self.may_be_unbound or not self.instance_types or has_keywords:
            return UNKNOWN
        result = BOTTOM
        for instance in self.instance_types:
            result = result.join(
                instance.method_return_type(
                    "$class:" + name, arity, has_keywords, storage_methods
                )
            )
        return result

    def fits_class(self, expected=None):
        return not self.may_be_unbound

    def annotation_conflict(self, contract):
        return not self.may_be_unbound

    def runtime_class_type(self):
        return UNKNOWN if self.may_be_unbound else CLASS_TYPE_TYPE

    def runtime_parent_class_type(self):
        return None if self.may_be_unbound else CLASS_OBJECT_TYPE

    def same_runtime_class(self, other) -> bool:
        return not self.may_be_unbound and other.fits_class()

    def general_instance_type(self):
        if not self.may_be_unbound and len(self.instance_types) == 1:
            inst = self.instance_types[0]
            if inst.fits_class():
                return ANY_CLASS_TYPE
            return inst.conservative_type().widen_constant()
        return UNKNOWN

    def allocate_instance_type(self):
        if self.may_be_unbound or not self.instance_types:
            return UNKNOWN
        result = self.instance_types[0]
        for inst in self.instance_types[1:]:
            result = result.join(inst)
        return result

    def parent_class_type(self):
        if self.may_be_unbound or not self.instance_types:
            return UNKNOWN
        result: ValueType = BOTTOM
        for inst in self.instance_types:
            parent = inst.runtime_parent_class_type()
            if parent is None:
                return UNKNOWN
            result = result.join(parent)
        return result

    def is_subclass_of(self, target) -> bool | None:
        if self.may_be_unbound or not self.instance_types:
            return None
        return target.check_subclass_from(self.instance_types)

    def check_subclass_from(self, source_instances) -> bool | None:
        if self.may_be_unbound or not self.instance_types:
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
        if not left.instance_types or not self.instance_types:
            return UNKNOWN_UNBOUND if unbound else UNKNOWN
        merged = _merge_class_instances(left.instance_types, self.instance_types)
        if len(merged) > 8:
            return UNKNOWN_UNBOUND if unbound else UNKNOWN
        return ClassType(merged, may_be_unbound=unbound)

    def widen(self, new, thresholds: tuple[int, ...] = ()):
        return new.widen_class(self, thresholds)

    def widen_class(self, old, thresholds: tuple[int, ...] = ()):
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
                return BOOLEAN_FALSE if is_same else BOOLEAN_TRUE
            return right.compare_class_identity(self, is_same)
        return super().comparison(operator, right)

    def compare_identity_with(self, left, operator):
        is_same = operator is ast.ComparisonOperator.IS
        if left.fits_none(NONE_TYPE) and not self.may_be_unbound:
            return BOOLEAN_FALSE if is_same else BOOLEAN_TRUE
        return left.compare_class_identity(self, is_same)

    def compare_class_identity(self, left, is_same: bool):
        if (
            left.may_be_unbound
            or self.may_be_unbound
            or not left.instance_types
            or not self.instance_types
        ):
            return BOOLEAN
        if len(left.instance_types) == 1 and len(self.instance_types) == 1:
            same = left.instance_types[0].same_runtime_class(self.instance_types[0])
            res = same if is_same else not same
            return BOOLEAN_TRUE if res else BOOLEAN_FALSE
        overlap = any(
            l_inst.same_runtime_class(r_inst)
            for l_inst in left.instance_types
            for r_inst in self.instance_types
        )
        if not overlap:
            return BOOLEAN_FALSE if is_same else BOOLEAN_TRUE
        return BOOLEAN

    def narrow_none(self, is_none: bool):
        if self.may_be_unbound:
            return self
        return BOTTOM if is_none else self

    def narrow_identity(self, target, is_same: bool):
        if self.may_be_unbound:
            return self
        if target.fits_none(NONE_TYPE):
            return self.narrow_none(is_same)
        if target.fits_class() and len(target.instance_types) == 1:
            if not self.instance_types:
                return (
                    ClassType(
                        target.instance_types,
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
                    else ClassType(matching, may_be_unbound=False)
                )
            remaining = tuple(
                inst
                for inst in self.instance_types
                if not inst.same_runtime_class(t_inst)
            )
            if not remaining:
                return BOTTOM
            return ClassType(remaining, may_be_unbound=False)
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

    def annotation_conflict(self, contract):
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


def widen_fact(
    previous: ValueType, current: ValueType, thresholds: tuple[int, ...] = ()
) -> ValueType:
    return previous.widen(previous.join(current), thresholds).limit_container_depth(4)


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
FUNCTION_TYPE = FunctionType()
BOTTOM = BottomType()
UNBOUND = UnboundType()
_FIXNUM_INTEGER = IntegerType(FIXNUM_MIN, FIXNUM_MAX)
OPTIONAL_FIXNUM = union_type(_FIXNUM_INTEGER, NONE_TYPE)
ANY_CLASS_TYPE = ClassType()
CLASS_TYPE_TYPE = ClassType((ANY_CLASS_TYPE,))
CLASS_INT_TYPE = ClassType((INTEGER,))
CLASS_BOOL_TYPE = ClassType((BOOLEAN,))
CLASS_FLOAT_TYPE = ClassType((FLOAT,))
CLASS_NONE_TYPE = ClassType((NONE_TYPE,))
CLASS_FUNCTION_TYPE = ClassType((FUNCTION_TYPE,))
CLASS_OBJECT_TYPE = ClassType((UNKNOWN,))


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
    if operator == ast.BinaryOperator.POWER:
        if (
            left.maximum is not None
            and left.maximum < 0
            and right.exact_integer() is None
        ):
            return UNKNOWN
        return FLOAT
    if operator in {
        ast.BinaryOperator.ADD,
        ast.BinaryOperator.SUBTRACT,
        ast.BinaryOperator.MULTIPLY,
        ast.BinaryOperator.DIVIDE,
        ast.BinaryOperator.FLOOR_DIVIDE,
        ast.BinaryOperator.MODULO,
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
            if lo is not None and lo >= 0 and rlo is not None and rlo >= 0:
                return IntegerType(0, None)
        if (
            operator == ast.BinaryOperator.BIT_OR
            and lo is not None
            and lo >= 0
            and rlo is not None
            and rlo >= 0
        ):
            return IntegerType(max(lo, rlo), None)
        if (
            operator == ast.BinaryOperator.BIT_XOR
            and lo is not None
            and lo >= 0
            and rlo is not None
            and rlo >= 0
        ):
            return IntegerType(0, None)
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
        elif lo >= 0 and 0 <= rlo <= 256:
            if operator == ast.BinaryOperator.LEFT_SHIFT:
                return IntegerType(lo << rlo, None)
            return IntegerType(0, hi >> rlo)
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
            "interned": fact.interned,
            "unbound": fact.may_be_unbound,
        }

    def visit_BytesType(self, fact):
        return {"kind": "bytes", "unbound": fact.may_be_unbound}

    def visit_ByteStorageType(self, fact):
        return {"kind": "byte_storage", "unbound": fact.may_be_unbound}

    def visit_WordVectorType(self, fact):
        return {"kind": "word_vector", "unbound": fact.may_be_unbound}

    def visit_ByteArrayType(self, fact):
        return {"kind": "bytearray", "unbound": fact.may_be_unbound}

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

    def visit_UnionType(self, fact):
        return {
            "kind": "union",
            "members": sorted(
                (member.to_dict() for member in fact.members),
                key=lambda member: json.dumps(member, sort_keys=True),
            ),
            "unbound": fact.may_be_unbound,
        }

    def visit_ClassType(self, fact):
        return {
            "kind": "class",
            "instances": [inst.to_dict() for inst in fact.instance_types],
            "unbound": fact.may_be_unbound,
        }

    def visit_UniqueObjectType(self, fact):
        return {
            "kind": "unique_obj",
            "id": str(fact.identity),
            "unbound": fact.may_be_unbound,
        }

    def visit_InstanceType(self, fact):
        return {
            "kind": "instance",
            "class": fact.identity,
            "unbound": fact.may_be_unbound,
            "fixed_slots": list(fact.fixed_slots),
        }

    def visit_SetType(self, fact):
        return {
            "kind": "set",
            "class": fact.identity,
            "element": fact.element.to_dict(),
            "unbound": fact.may_be_unbound,
            "fixed_slots": list(fact.fixed_slots),
        }


_SERIALIZE_VALUE_TYPE = _SerializeValueType()
