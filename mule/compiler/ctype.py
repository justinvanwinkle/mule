"""Language-level value types, following SBCL's CTYPE (``ctype.lisp``).

Polymorphic semantic facts used by control-flow and local-call analysis.

Intervals describe values, not representations of class names. Operations use
ordinary virtual methods and double dispatch; representation properties expose
only proofs that LLVM may use after a successful language operation.
"""


from dataclasses import dataclass

from mule import ast

from .ctype_core import (
    ANY_CLASS_TYPE,
    BOOLEAN,
    BOOLEAN_FALSE,
    BOOLEAN_TRUE,
    BOTTOM,
    CLASS_BOOL_TYPE,
    CLASS_FLOAT_TYPE,
    CLASS_FUNCTION_TYPE,
    CLASS_INT_TYPE,
    CLASS_NONE_TYPE,
    CLASS_OBJECT_TYPE,
    CLASS_TYPE_TYPE,
    FIXNUM_MAX,
    FIXNUM_MIN,
    FLOAT,
    FUNCTION_TYPE,
    IMMORTAL,
    IMMORTAL_UNBOUND,
    INTEGER,
    MAX_UNION_MEMBERS,
    NONE_TYPE,
    OPTIONAL_FIXNUM,
    PYTHON_INTEGER,
    REAL,
    UNBOUND,
    UNKNOWN,
    UNKNOWN_UNBOUND,
    BooleanType,
    BottomType,
    ClassType,
    FloatType,
    FunctionType,
    ImmortalType,
    IntegerType,
    NoneType,
    PrimitiveType,
    PythonIntegerType,
    RealType,
    UnboundType,
    UnionType,
    UniqueObjectType,
    UnknownType,
    ValueType,
    ValueTypeVisitor,
    union_type,
    widen_fact,
)


def _is_singleton_text(literal: str | None) -> bool:
    return literal is not None and (
        literal == "" or (len(literal) == 1 and ord(literal) <= 0xFF)
    )


@dataclass(frozen=True, slots=True)
class TextType(PrimitiveType):
    literal: str | None = None
    may_be_unbound: bool = False
    interned: bool | None = None
    length: int | None = None

    def runtime_class_type(self):
        return UNKNOWN if self.may_be_unbound else CLASS_STR_TYPE

    def same_runtime_class(self, other) -> bool:
        return other.fits_text(TEXT_TYPE)

    @property
    def is_heap_object(self):
        return not self.may_be_unbound

    def has_reflexive_equality(self) -> bool:
        return not self.may_be_unbound

    @property
    def is_immortal(self):
        return not self.may_be_unbound and _is_singleton_text(self.literal)

    @property
    def needs_release(self):
        return self.may_be_unbound or not _is_singleton_text(self.literal)

    @property
    def is_interned_text(self) -> bool:
        if self.may_be_unbound:
            return False
        if self.interned is not None:
            return self.interned or _is_singleton_text(self.literal)
        return self.literal is not None

    def singleton_text(self) -> str | None:
        if not self.may_be_unbound and _is_singleton_text(self.literal):
            return self.literal
        return None

    def interned_text(self) -> str | None:
        if not self.may_be_unbound and self.is_interned_text:
            return self.literal
        return None

    def exact_text(self) -> str | None:
        return None if self.may_be_unbound else self.literal

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
        return right.compare_text(operator, self)

    def compare_text(self, operator, left):
        if self.may_be_unbound or left.may_be_unbound:
            return BOOLEAN
        left_text = left.exact_text()
        right_text = self.exact_text()
        if left_text is not None and right_text is not None:
            if operator is ast.ComparisonOperator.EQUAL:
                res = left_text == right_text
            elif operator is ast.ComparisonOperator.NOT_EQUAL:
                res = left_text != right_text
            elif operator is ast.ComparisonOperator.LESS:
                res = left_text < right_text
            elif operator is ast.ComparisonOperator.LESS_EQUAL:
                res = left_text <= right_text
            elif operator is ast.ComparisonOperator.GREATER:
                res = left_text > right_text
            elif operator is ast.ComparisonOperator.GREATER_EQUAL:
                res = left_text >= right_text
            else:
                return BOOLEAN
            return BOOLEAN_TRUE if res else BOOLEAN_FALSE
        left_len = left.exact_length()
        right_len = self.exact_length()
        if left_len is not None and right_len is not None and left_len != right_len:
            if operator is ast.ComparisonOperator.EQUAL:
                return BOOLEAN_FALSE
            if operator is ast.ComparisonOperator.NOT_EQUAL:
                return BOOLEAN_TRUE
        return BOOLEAN

    def compare_identity_with(self, left, operator):
        if self.may_be_unbound or left.may_be_unbound:
            return BOOLEAN
        is_same = operator is ast.ComparisonOperator.IS
        if left.fits_none(NONE_TYPE):
            return BOOLEAN_FALSE if is_same else BOOLEAN_TRUE
        left_text = left.exact_text()
        if self.literal is not None and left_text is not None:
            if self.literal != left_text:
                return BOOLEAN_FALSE if is_same else BOOLEAN_TRUE
            if (
                (self.is_interned_text and left.is_interned_text)
                or self.singleton_text() is not None
                or left.singleton_text() is not None
            ):
                return BOOLEAN_TRUE if is_same else BOOLEAN_FALSE
        return BOOLEAN

    def exact_length(self) -> int | None:
        if self.may_be_unbound:
            return None
        if self.literal is not None:
            return len(self.literal)
        return self.length

    def codepoint_at(self, index: int):
        if self.may_be_unbound or self.literal is None:
            return None
        idx = index + len(self.literal) if index < 0 else index
        if 0 <= idx < len(self.literal):
            cp = ord(self.literal[idx])
            return IntegerType(cp, cp)
        return None

    def truth_value(self) -> bool | None:
        if self.may_be_unbound:
            return None
        if self.literal is not None:
            return bool(self.literal)
        if self.length is not None:
            return self.length > 0
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
        if left is self or left == self:
            return self
        unbound = self.may_be_unbound or left.may_be_unbound
        literal = self.literal if self.literal == left.literal else None
        both_interned = (
            (self.interned is True or left.interned is True)
            and self.as_bound().is_interned_text
            and left.as_bound().is_interned_text
        )
        if literal is not None:
            lit_interned = (
                self.as_bound().is_interned_text and left.as_bound().is_interned_text
            )
            return TextType(
                literal,
                may_be_unbound=unbound,
                interned=None if lit_interned else False,
            )
        length = (
            self.length
            if (self.length is not None and self.length == left.length)
            else None
        )
        if not unbound and length is None:
            return INTERNED_TEXT_TYPE if both_interned else TEXT_TYPE
        return TextType(
            None,
            may_be_unbound=unbound,
            interned=True if both_interned else None,
            length=length,
        )

    def widen(self, new, thresholds: tuple[int, ...] = ()):
        if new.fits_text(TEXT_TYPE) and (
            self.literal != new.exact_text() or self.length != new.length
        ):
            interned = bool(self.interned) and new.as_bound().is_interned_text
            length = (
                self.length
                if (self.length is not None and self.length == new.length)
                else None
            )
            if not new.may_be_unbound and length is None:
                return INTERNED_TEXT_TYPE if interned else TEXT_TYPE
            return TextType(
                may_be_unbound=new.may_be_unbound,
                interned=True if interned else None,
                length=length,
            )
        return new

    def narrow_equality(self, other, is_equal: bool):
        if self.may_be_unbound or other.may_be_unbound:
            return self
        my_len = self.exact_length()
        other_len = other.exact_length()
        if (
            is_equal
            and my_len is not None
            and other_len is not None
            and my_len != other_len
        ):
            return BOTTOM
        other_text = other.exact_text()
        if other_text is not None:
            if is_equal:
                if self.literal is not None and self.literal != other_text:
                    return BOTTOM
                is_interned = self.is_interned_text or _is_singleton_text(other_text)
                return TextType(
                    other_text,
                    interned=None if is_interned else False,
                )
            if self.literal is not None and self.literal == other_text:
                return BOTTOM
        if is_equal and my_len is None and other_len is not None:
            return TextType(
                None,
                interned=self.interned,
                length=other_len,
            )
        return self

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
        return (
            not self.may_be_unbound
            and (expected.literal is None or self.literal == expected.literal)
            and (expected.length is None or self.exact_length() == expected.length)
            and (not expected.interned or self.is_interned_text)
        )

    def fits_possible_text(self, expected):
        return (
            not self.may_be_unbound
            and (
                expected.literal is None
                or self.literal is None
                or self.literal == expected.literal
            )
            and (
                expected.length is None
                or self.exact_length() is None
                or self.exact_length() == expected.length
            )
        )

    def fits_immortal(self, expected):
        return self.is_immortal

    def fits_possible_immortal(self, expected):
        return True

    def valid_index_bounds(self) -> tuple[int, int] | None:
        if self.may_be_unbound:
            return None
        exact = self.exact_length()
        if exact is not None:
            return (-exact, exact - 1) if exact > 0 else None
        max_len = (1 << 58) - 1
        return (-max_len, max_len - 1)

    def getitem_type(self, index):
        if self.may_be_unbound:
            return UNKNOWN
        if self.literal is not None and not index.may_be_unbound:
            idx = index.exact_integer()
            if idx is not None:
                if idx < 0:
                    idx += len(self.literal)
                if 0 <= idx < len(self.literal):
                    return TextType(self.literal[idx])
        if not index.may_be_unbound and index.fits(PYTHON_INTEGER):
            return TextType(length=1)
        return TEXT_TYPE

    def getslice_type(self, start=None, stop=None, step=None):
        if self.may_be_unbound:
            return UNKNOWN
        if (
            start is not None
            and stop is not None
            and (step is None or step.fits_none(NONE_TYPE) or step.exact_integer() == 1)
        ):
            s_idx = start.exact_integer()
            e_idx = stop.exact_integer()
            if s_idx is not None and e_idx is not None:
                if self.literal is not None:
                    sliced = self.literal[s_idx:e_idx]
                    if len(sliced.encode("utf-8", "surrogatepass")) <= 64:
                        return TextType(sliced, interned=False)
                src_len = self.exact_length()
                if src_len is not None and 0 <= s_idx <= e_idx <= src_len:
                    return TextType(length=e_idx - s_idx, interned=False)
        return TEXT_TYPE

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
class ByteStorageType(PrimitiveType):
    """Private mutable byte payload, distinct from Python bytes and lists."""

    may_be_unbound: bool = False

    @property
    def is_byte_storage(self):
        return not self.may_be_unbound

    @property
    def is_heap_object(self):
        return not self.may_be_unbound

    def accept(self, visitor):
        return visitor.visit_ByteStorageType(self)

    def getitem_type(self, index):
        return IntegerType(0, 255) if index.fits(PYTHON_INTEGER) else UNKNOWN

    def iterate_type(self):
        return IteratorType(IntegerType(0, 255))


BYTE_STORAGE_TYPE = ByteStorageType()


@dataclass(frozen=True, slots=True)
class WordVectorType(PrimitiveType):
    """Private fixed-length vector of fixnum words (SBCL's
    ``(simple-array fixnum (*))``): its representation, not flow facts,
    gives every element the fixnum type, and its length never changes."""

    may_be_unbound: bool = False

    @property
    def is_heap_object(self):
        return not self.may_be_unbound

    def accept(self, visitor):
        return visitor.visit_WordVectorType(self)

    def getitem_type(self, index):
        if index.fits(PYTHON_INTEGER):
            return IntegerType(FIXNUM_MIN, FIXNUM_MAX)
        return UNKNOWN


WORD_VECTOR_TYPE = WordVectorType()


@dataclass(frozen=True, slots=True)
class BytesType(PrimitiveType):
    may_be_unbound: bool = False

    def runtime_class_type(self):
        return UNKNOWN if self.may_be_unbound else CLASS_BYTES_TYPE

    def same_runtime_class(self, other) -> bool:
        return other.fits_bytes(BYTES_TYPE)

    def has_reflexive_equality(self) -> bool:
        return not self.may_be_unbound

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

    def binary_bytearray(self, operator, left):
        if not left.may_be_unbound and operator in {
            ast.BinaryOperator.ADD,
            ast.BinaryOperator.MODULO,
        }:
            return BYTEARRAY_TYPE
        return UNKNOWN

    def binary_integer(self, operator, left):
        if not left.may_be_unbound and operator == ast.BinaryOperator.MULTIPLY:
            return BYTES_TYPE
        return UNKNOWN

    def binary_boolean(self, operator, left):
        if not left.may_be_unbound and operator == ast.BinaryOperator.MULTIPLY:
            return BYTES_TYPE
        return UNKNOWN

    def dispatch_domain(self):
        return None if self.may_be_unbound else BYTES_TYPE

    def dispatch_rank(self) -> int:
        return 2**64 if self.may_be_unbound else 0

    def accepts(self, actual):
        return actual.fits_bytes(self)

    def accepts_dispatch(self, actual):
        return actual.fits_bytes(self)

    def accepts_possible_dispatch(self, actual):
        return actual.fits_possible_bytes(self)

    def fits_bytes(self, expected):
        return not self.may_be_unbound

    def valid_index_bounds(self) -> tuple[int, int] | None:
        if self.may_be_unbound:
            return None
        max_len = (1 << 48) - 1
        return (-max_len, max_len - 1)

    def getitem_type(self, index):
        if self.may_be_unbound or index.may_be_unbound:
            return UNKNOWN
        if index.fits(PYTHON_INTEGER):
            return IntegerType(0, 255)
        return UNKNOWN

    def getslice_type(self, start=None, stop=None, step=None):
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
class ByteArrayType(PrimitiveType):
    may_be_unbound: bool = False

    def runtime_class_type(self):
        return UNKNOWN if self.may_be_unbound else CLASS_BYTEARRAY_TYPE

    def runtime_parent_class_type(self):
        return CLASS_OBJECT_TYPE

    def same_runtime_class(self, other) -> bool:
        return other.fits_bytearray(BYTEARRAY_TYPE)

    def has_reflexive_equality(self) -> bool:
        return not self.may_be_unbound

    @property
    def is_heap_object(self):
        return not self.may_be_unbound

    def dispatch_domain(self):
        return None if self.may_be_unbound else BYTEARRAY_TYPE

    def dispatch_rank(self) -> int:
        return 2**64 if self.may_be_unbound else 0

    def accept(self, visitor):
        return visitor.visit_ByteArrayType(self)

    def join(self, other):
        return other.join_bytearray(self)

    def join_bytearray(self, left):
        return (
            ByteArrayType(may_be_unbound=True)
            if self.may_be_unbound or left.may_be_unbound
            else BYTEARRAY_TYPE
        )

    def binary(self, operator, right):
        if self.may_be_unbound:
            return right.binary_unknown(operator, self)
        if operator == ast.BinaryOperator.MODULO and not right.may_be_unbound:
            return BYTEARRAY_TYPE
        return right.binary_bytearray(operator, self)

    def binary_bytearray(self, operator, left):
        if not left.may_be_unbound and operator in {
            ast.BinaryOperator.ADD,
            ast.BinaryOperator.MODULO,
        }:
            return BYTEARRAY_TYPE
        return UNKNOWN

    def binary_bytes(self, operator, left):
        if not left.may_be_unbound and operator == ast.BinaryOperator.ADD:
            return BYTEARRAY_TYPE
        return UNKNOWN

    def binary_integer(self, operator, left):
        if not left.may_be_unbound and operator == ast.BinaryOperator.MULTIPLY:
            return BYTEARRAY_TYPE
        return UNKNOWN

    def binary_boolean(self, operator, left):
        if not left.may_be_unbound and operator == ast.BinaryOperator.MULTIPLY:
            return BYTEARRAY_TYPE
        return UNKNOWN

    def accepts(self, actual):
        return actual.fits_bytearray(self)

    def accepts_dispatch(self, actual):
        return actual.fits_bytearray(self)

    def accepts_possible_dispatch(self, actual):
        return actual.fits_possible_bytearray(self)

    def fits_bytearray(self, expected=None):
        return not self.may_be_unbound

    def valid_index_bounds(self) -> tuple[int, int] | None:
        if self.may_be_unbound:
            return None
        max_len = (1 << 48) - 1
        return (-max_len, max_len - 1)

    def getitem_type(self, index):
        if self.may_be_unbound or index.may_be_unbound:
            return UNKNOWN
        if index.fits(PYTHON_INTEGER):
            return IntegerType(0, 255)
        return UNKNOWN

    def getslice_type(self, start=None, stop=None, step=None):
        return UNKNOWN if self.may_be_unbound else BYTEARRAY_TYPE

    def iterate_type(self):
        if self.may_be_unbound:
            return UNKNOWN
        return IteratorType(IntegerType(0, 255))

    def annotation_conflict(self, contract):
        return False if self.may_be_unbound else contract.bytearray_conflict(self)

    def method_return_type(
        self, name: str, arity: int, has_keywords: bool, storage_methods=None
    ):
        if self.may_be_unbound or storage_methods is None:
            return UNKNOWN
        return storage_methods.get(("bytearray", name), UNKNOWN)


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

    def valid_index_bounds(self) -> tuple[int, int] | None:
        if self.may_be_unbound:
            return None
        if self.arity is not None:
            return (-self.arity, self.arity - 1) if self.arity > 0 else None
        max_len = (1 << 60) - 1
        return (-max_len, max_len - 1)

    def annotation_conflict(self, contract):
        return False if self.may_be_unbound else contract.tuple_conflict(self)

    def annotated_tuple(self, expected):
        if self.arity is None and expected.arity is not None:
            return TupleType(expected.arity, None, may_be_unbound=self.may_be_unbound)
        return self

    def dispatch_rank(self) -> int:
        return 2**64 if self.may_be_unbound else 0

    @property
    def is_heap_object(self):
        return not self.may_be_unbound

    @property
    def is_exact_tuple(self):
        return not self.may_be_unbound

    @property
    def may_run_user_code(self):
        return (
            self.may_be_unbound
            or self.elements is None
            or any(elem.may_run_user_code for elem in self.elements)
        )

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

    def widen(self, new, thresholds: tuple[int, ...] = ()):
        return new.widen_tuple(self, thresholds)

    def widen_tuple(self, old, thresholds: tuple[int, ...] = ()):
        if (
            self.arity is not None
            and self.arity == old.arity
            and self.elements is not None
            and old.elements is not None
            and len(self.elements) == len(old.elements)
        ):
            widened_elems = tuple(
                widen_fact(o_el, n_el, thresholds).limit_container_depth(2)
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

    def widen_inferred_contract(self):
        return TupleType(
            self.arity,
            None
            if self.elements is None
            else tuple(el.widen_inferred_contract() for el in self.elements),
            self.may_be_unbound,
        )

    def limit_container_depth(self, max_depth: int = 4):
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

    def has_float_specialization(self) -> bool:
        return (
            not self.may_be_unbound
            and self.elements is not None
            and any(el.has_float_specialization() for el in self.elements)
        )

    def has_unspecialized_part(self) -> bool:
        return self.elements is None or any(
            el.has_unspecialized_part() for el in self.elements
        )

    def has_bottom_part(self) -> bool:
        return self.elements is not None and any(
            el.has_bottom_part() for el in self.elements
        )

    def dispatch_domain(self):
        if (
            self.may_be_unbound
            or self.arity is None
            or self.elements is None
            or not (
                self.has_float_specialization()
                or any(el.fits_list(LIST_TYPE) for el in self.elements)
            )
        ):
            return None
        elem_domains = tuple(
            el.dispatch_domain() or (FLOAT if el.fits_float(FLOAT) else UNKNOWN)
            for el in self.elements
        )
        if any(el is not UNKNOWN for el in elem_domains):
            return TupleType(self.arity, elem_domains)
        return None

    def conservative_type(self):
        if self.elements is None:
            return self
        new_elems = tuple(el.conservative_type() for el in self.elements)
        if new_elems == self.elements:
            return self
        return TupleType(self.arity, new_elems, self.may_be_unbound)

    def binary(self, operator, right):
        if self.may_be_unbound:
            return right.binary_unknown(operator, self)
        if operator == ast.BinaryOperator.MULTIPLY and right.fits(PYTHON_INTEGER):
            return self.getslice_type()
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

    def binary_integer(self, operator, left):
        if (
            not left.may_be_unbound
            and not self.may_be_unbound
            and operator == ast.BinaryOperator.MULTIPLY
        ):
            return self.getslice_type()
        return UNKNOWN

    def binary_boolean(self, operator, left):
        if (
            not left.may_be_unbound
            and not self.may_be_unbound
            and operator == ast.BinaryOperator.MULTIPLY
        ):
            return self.getslice_type()
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

    def getslice_type(self, start=None, stop=None, step=None):
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
        return actual.fits_tuple_dispatch(self)

    def accepts_possible_dispatch(self, actual):
        return actual.fits_possible_tuple(self)

    def fits_tuple(self, expected):
        if self.may_be_unbound:
            return False
        if expected.arity is not None and self.arity != expected.arity:
            return False
        if expected.elements is not None:
            if self.arity == 0 and (
                len(expected.elements) == 0
                or (expected.arity is None and len(expected.elements) == 1)
            ):
                return True
            if self.elements is None:
                return all(el is UNKNOWN for el in expected.elements)
            if expected.arity is None and len(expected.elements) == 1:
                return all(el.fits(expected.elements[0]) for el in self.elements)
            if self.arity is None or len(self.elements) != len(expected.elements):
                return False
            return all(
                s_el.fits(e_el) for s_el, e_el in zip(self.elements, expected.elements)
            )
        return True

    def fits_tuple_dispatch(self, expected):
        if self.may_be_unbound:
            return False
        if expected.arity is not None and self.arity != expected.arity:
            return False
        if expected.elements is not None:
            if self.arity == 0 and len(expected.elements) == 0:
                return True
            if self.elements is None:
                return all(el is UNKNOWN for el in expected.elements)
            if self.arity is None or len(self.elements) != len(expected.elements):
                return False
            return all(
                s_el is not UNKNOWN and (e_el is UNKNOWN or s_el.dispatch_fits(e_el))
                for s_el, e_el in zip(self.elements, expected.elements)
            )
        return True

    def fits_possible_tuple(self, expected):
        if self.may_be_unbound:
            return False
        if (
            expected.arity is not None
            and self.arity is not None
            and self.arity != expected.arity
        ):
            return False
        if (
            expected.arity is not None
            and self.arity == expected.arity
            and expected.elements is not None
            and self.elements is not None
            and len(self.elements) == len(expected.elements)
        ):
            return all(
                s_el.may_dispatch_fit(e_el)
                for s_el, e_el in zip(self.elements, expected.elements)
            )
        return True

    def proves_tuple_index(self, index):
        return not self.may_be_unbound and self.arity is not None and index < self.arity

    def method_return_type(
        self, name: str, arity: int, has_keywords: bool, storage_methods=None
    ):
        if self.may_be_unbound or storage_methods is None:
            return UNKNOWN
        return storage_methods.get(("TupleStorage", name), UNKNOWN)


ValueType._tuple_type = TupleType


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
        return self

    def unspecialized_container(self):
        return LIST_TYPE

    @property
    def is_heap_object(self):
        return not self.may_be_unbound

    @property
    def is_exact_list(self):
        return not self.may_be_unbound

    @property
    def may_run_user_code(self):
        return self.may_be_unbound or self.element.may_run_user_code

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

    def limit_container_depth(self, max_depth: int = 4):
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

    def has_float_specialization(self) -> bool:
        return not self.may_be_unbound and self.element.has_float_specialization()

    def has_unspecialized_part(self) -> bool:
        return self.element.has_unspecialized_part()

    def has_bottom_part(self) -> bool:
        return self.element.has_bottom_part()

    def widen_constant(self):
        return LIST_TYPE if not self.may_be_unbound else ListType(UNKNOWN, True)

    def widen_inferred_contract(self):
        if self.element is BOTTOM or self.element is UNKNOWN:
            return LIST_TYPE if not self.may_be_unbound else ListType(UNKNOWN, True)
        return ListType(self.element.widen_inferred_contract(), self.may_be_unbound)

    def conservative_type(self):
        # Code the analysis cannot see may store any value or remove any
        # element, so neither the length nor the element type survives.
        return self.widen_constant()

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
            element = left.element.join(self.element).limit_container_depth(3)
        if element is UNKNOWN and not may_be_unbound:
            return LIST_TYPE
        if element == self.element and may_be_unbound == self.may_be_unbound:
            return self
        if element == left.element and may_be_unbound == left.may_be_unbound:
            return left
        return ListType(element, may_be_unbound)

    def widen(self, new, thresholds: tuple[int, ...] = ()):
        return new.widen_list(self, thresholds)

    def widen_list(self, old, thresholds: tuple[int, ...] = ()):
        element = widen_fact(
            old.element, self.element, thresholds
        ).limit_container_depth(3)
        may_be_unbound = old.may_be_unbound or self.may_be_unbound
        if element == self.element and may_be_unbound == self.may_be_unbound:
            return self
        if element is UNKNOWN and not may_be_unbound:
            return LIST_TYPE
        return ListType(element, may_be_unbound)

    def valid_index_bounds(self) -> tuple[int, int] | None:
        if self.may_be_unbound or self.element is BOTTOM:
            return None
        max_len = (1 << 60) - 1
        return (-max_len, max_len - 1)

    def dispatch_domain(self):
        if self.may_be_unbound:
            return None
        if (
            self.element is not BOTTOM
            and self.element is not UNKNOWN
            and (self.has_float_specialization() or self.element.fits_real(REAL))
        ):
            elem_dom = self.element.dispatch_domain() or (
                FLOAT if self.element.fits_float(FLOAT) else None
            )
            if elem_dom is not None:
                return ListType(elem_dom)
        return LIST_TYPE

    def dispatch_rank(self) -> int:
        return 2**64 if self.may_be_unbound else 0

    def accepts(self, actual):
        return actual.fits_list(self)

    def accepts_dispatch(self, actual):
        return actual.fits_list_dispatch(self)

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

    def fits_list_dispatch(self, expected):
        if self.may_be_unbound and not expected.may_be_unbound:
            return False
        if expected.element is UNKNOWN or self.element is BOTTOM:
            return True
        return self.element.dispatch_fits(expected.element)

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
        return self.element.may_dispatch_fit(expected.element)

    def getitem_type(self, index):
        if self.may_be_unbound or index.may_be_unbound:
            return UNKNOWN
        if index.fits(PYTHON_INTEGER) and self.element is not BOTTOM:
            return self.element
        return UNKNOWN

    def flow_getitem_type(self, index):
        if self.may_be_unbound or index.may_be_unbound:
            return UNKNOWN
        if index.fits(PYTHON_INTEGER):
            return self.element
        return UNKNOWN

    def getslice_type(self, start=None, stop=None, step=None):
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
        elem = element.as_bound().limit_container_depth(3)
        new_elem = (
            elem
            if self.element is BOTTOM
            else self.element.join(elem).limit_container_depth(3)
        )
        return ListType(new_elem, self.may_be_unbound)

    def construct_type(self, arg_types: tuple, keywords: dict):
        if keywords or len(arg_types) > 1:
            return LIST_TYPE
        if not arg_types:
            return EMPTY_LIST_TYPE
        iter_type = arg_types[0].flow_iterate_type()
        if iter_type.fits_iterator():
            return EMPTY_LIST_TYPE.with_element(iter_type.element)
        return LIST_TYPE

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
    """Exact builtin dict storage (or optimistic field dict when not proven)."""

    key: ValueType = UNKNOWN
    value: ValueType = UNKNOWN
    may_be_unbound: bool = False
    proven: bool = True

    def runtime_class_type(self):
        return UNKNOWN if (self.may_be_unbound or not self.proven) else CLASS_DICT_TYPE

    def runtime_parent_class_type(self):
        return UNKNOWN if not self.proven else CLASS_OBJECT_TYPE

    def same_runtime_class(self, other) -> bool:
        return self.proven and other.fits_dict(DICT_TYPE)

    def exact_length(self) -> int | None:
        return (
            0
            if (
                self.proven
                and not self.may_be_unbound
                and self.key is BOTTOM
                and self.value is BOTTOM
            )
            else None
        )

    def annotation_conflict(self, contract):
        return (
            False
            if (self.may_be_unbound or not self.proven)
            else contract.dict_conflict(self)
        )

    def annotated_dict(self, expected):
        if not self.proven:
            return DictType(
                self.key, self.value, may_be_unbound=self.may_be_unbound, proven=True
            )
        return self

    def unspecialized_container(self):
        return DICT_TYPE

    def construct_type(self, arg_types: tuple, keywords: tuple):
        pos_count = len(arg_types) - len(keywords)
        if pos_count > 1 or any(kw is None for kw in keywords):
            return DICT_TYPE
        result = EMPTY_DICT_TYPE
        if pos_count == 1:
            other = arg_types[0]
            if other.fits_dict(DICT_TYPE) and not other.may_be_unbound:
                result = result.with_entry(other.key, other.value)
            else:
                iter_type = other.flow_iterate_type()
                if iter_type.fits_iterator() and iter_type.element.fits_tuple(
                    TupleType(2)
                ):
                    result = result.with_entry(
                        iter_type.element.getitem_type(IntegerType(0, 0)),
                        iter_type.element.getitem_type(IntegerType(1, 1)),
                    )
                else:
                    return DICT_TYPE
        if keywords:
            for val_type in arg_types[pos_count:]:
                result = result.with_entry(TEXT_TYPE, val_type)
        return result

    def dispatch_rank(self) -> int:
        return 2**64 if (self.may_be_unbound or not self.proven) else 0

    @property
    def is_heap_object(self):
        return self.proven and not self.may_be_unbound

    def truth_value(self) -> bool | None:
        if (
            self.proven
            and not self.may_be_unbound
            and self.key is BOTTOM
            and self.value is BOTTOM
        ):
            return False
        return None

    def accept(self, visitor):
        return visitor.visit_DictType(self)

    def as_bound(self):
        if not self.may_be_unbound:
            return self
        if self.key is UNKNOWN and self.value is UNKNOWN and self.proven:
            return DICT_TYPE
        return DictType(self.key, self.value, False, self.proven)

    def with_unbound(self):
        if self.may_be_unbound:
            return self
        return DictType(self.key, self.value, True, self.proven)

    def limit_container_depth(self, max_depth: int = 4):
        if self.key is UNKNOWN and self.value is UNKNOWN:
            return self
        if max_depth <= 0:
            if not self.proven:
                return DictType(UNKNOWN, UNKNOWN, self.may_be_unbound, False)
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
        if lk is UNKNOWN and lv is UNKNOWN and not self.may_be_unbound and self.proven:
            return DICT_TYPE
        if lk == self.key and lv == self.value:
            return self
        return DictType(lk, lv, self.may_be_unbound, self.proven)

    def has_unspecialized_part(self) -> bool:
        return self.key.has_unspecialized_part() or self.value.has_unspecialized_part()

    def has_bottom_part(self) -> bool:
        return self.key.has_bottom_part() or self.value.has_bottom_part()

    def widen_constant(self):
        if not self.proven:
            return DictType(UNKNOWN, UNKNOWN, self.may_be_unbound, False)
        return (
            DICT_TYPE if not self.may_be_unbound else DictType(UNKNOWN, UNKNOWN, True)
        )

    def conservative_type(self):
        # As for lists: unseen code may insert or delete any entry.
        return self.widen_constant()

    def join(self, other):
        return other.join_dict(self)

    def join_dict(self, left):
        if left is self:
            return self
        may_be_unbound = left.may_be_unbound or self.may_be_unbound
        proven = left.proven and self.proven
        if left.key is BOTTOM:
            key = self.key
        elif self.key is BOTTOM:
            key = left.key
        else:
            key = left.key.join(self.key).limit_container_depth(3)
        if left.value is BOTTOM:
            value = self.value
        elif self.value is BOTTOM:
            value = left.value
        else:
            value = left.value.join(self.value).limit_container_depth(3)
        if key is UNKNOWN and value is UNKNOWN and not may_be_unbound and proven:
            return DICT_TYPE
        if (
            key == self.key
            and value == self.value
            and may_be_unbound == self.may_be_unbound
            and proven == self.proven
        ):
            return self
        if (
            key == left.key
            and value == left.value
            and may_be_unbound == left.may_be_unbound
            and proven == left.proven
        ):
            return left
        return DictType(key, value, may_be_unbound, proven)

    def widen(self, new, thresholds: tuple[int, ...] = ()):
        return new.widen_dict(self, thresholds)

    def widen_dict(self, old, thresholds: tuple[int, ...] = ()):
        key = widen_fact(old.key, self.key, thresholds).limit_container_depth(3)
        value = widen_fact(old.value, self.value, thresholds).limit_container_depth(3)
        may_be_unbound = old.may_be_unbound or self.may_be_unbound
        proven = old.proven and self.proven
        if (
            key == self.key
            and value == self.value
            and may_be_unbound == self.may_be_unbound
            and proven == self.proven
        ):
            return self
        if key is UNKNOWN and value is UNKNOWN and not may_be_unbound and proven:
            return DICT_TYPE
        return DictType(key, value, may_be_unbound, proven)

    def accepts(self, actual):
        return actual.fits_dict(self)

    def accepts_dispatch(self, actual):
        return (
            not self.may_be_unbound
            and self.proven
            and self.key is UNKNOWN
            and self.value is UNKNOWN
            and actual.fits_dict(self)
        )

    def accepts_possible_dispatch(self, actual):
        return (
            not self.may_be_unbound
            and self.proven
            and self.key is UNKNOWN
            and self.value is UNKNOWN
            and actual.fits_possible_dict(self)
        )

    def fits_dict(self, expected=None):
        if not self.proven:
            return False
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

    def fits_possible_dict(self, expected=None):
        if expected is None:
            return not self.may_be_unbound
        return not (self.may_be_unbound and not expected.may_be_unbound)

    def with_entry(self, key, value):
        k = key.as_bound().limit_container_depth(3)
        v = value.as_bound().limit_container_depth(3)
        new_key = k if self.key is BOTTOM else self.key.join(k).limit_container_depth(3)
        new_val = (
            v if self.value is BOTTOM else self.value.join(v).limit_container_depth(3)
        )
        return DictType(new_key, new_val, self.may_be_unbound, self.proven)

    def getitem_type(self, index):
        if self.may_be_unbound or index.may_be_unbound or self.value is BOTTOM:
            return UNKNOWN
        return self.value

    def flow_getitem_type(self, index):
        if self.may_be_unbound or index.may_be_unbound or not self.proven:
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
    is_stream: bool = False

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
        return not self.may_be_unbound

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
            if self.element is UNKNOWN and not self.is_stream
            else IteratorType(self.element, False, self.is_stream)
        )

    def with_unbound(self):
        if self.may_be_unbound:
            return self
        return IteratorType(self.element, True, self.is_stream)

    def limit_container_depth(self, max_depth: int = 4):
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

    def has_unspecialized_part(self) -> bool:
        return self.element.has_unspecialized_part()

    def has_bottom_part(self) -> bool:
        return self.element.has_bottom_part()

    def conservative_type(self):
        if self.element is BOTTOM or self.element is UNKNOWN:
            return self
        new_elem = self.element.conservative_type()
        if new_elem == self.element:
            return self
        return IteratorType(new_elem, self.may_be_unbound)

    def join(self, other):
        return other.join_iterator(self)

    def join_iterator(self, left):
        if left is self:
            return self
        may_be_unbound = left.may_be_unbound or self.may_be_unbound
        is_stream = left.is_stream and self.is_stream
        if left.element is BOTTOM:
            element = self.element
        elif self.element is BOTTOM:
            element = left.element
        else:
            element = left.element.join(self.element).limit_container_depth(3)
        if element is UNKNOWN and not may_be_unbound and not is_stream:
            return ITERATOR_TYPE
        if (
            element == self.element
            and may_be_unbound == self.may_be_unbound
            and is_stream == self.is_stream
        ):
            return self
        if (
            element == left.element
            and may_be_unbound == left.may_be_unbound
            and is_stream == left.is_stream
        ):
            return left
        return IteratorType(element, may_be_unbound, is_stream)

    def widen(self, new, thresholds: tuple[int, ...] = ()):
        return new.widen_iterator(self, thresholds)

    def widen_iterator(self, old, thresholds: tuple[int, ...] = ()):
        element = widen_fact(
            old.element, self.element, thresholds
        ).limit_container_depth(3)
        may_be_unbound = old.may_be_unbound or self.may_be_unbound
        is_stream = old.is_stream and self.is_stream
        if (
            element == self.element
            and may_be_unbound == self.may_be_unbound
            and is_stream == self.is_stream
        ):
            return self
        if element is UNKNOWN and not may_be_unbound and not is_stream:
            return ITERATOR_TYPE
        return IteratorType(element, may_be_unbound, is_stream)

    def accepts(self, actual):
        return actual.fits_iterator(self)

    def fits_iterator(self, expected=None):
        if expected is None:
            return not self.may_be_unbound
        if self.may_be_unbound and not expected.may_be_unbound:
            return False
        if expected.is_stream and not self.is_stream:
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

    def method_return_type(
        self, name: str, arity: int, has_keywords: bool, storage_methods=None
    ):
        if (
            self.may_be_unbound
            or not self.is_stream
            or has_keywords
            or self.element is UNKNOWN
            or self.element is BOTTOM
        ):
            return UNKNOWN
        if name in ("read", "readline") and arity <= 1:
            return self.element
        if name == "readlines" and arity <= 1:
            return ListType(self.element)
        if name == "__enter__" and arity == 0:
            return self
        return UNKNOWN


@dataclass(frozen=True, slots=True)
class InstanceType(ValueType):
    """An instance of exactly one user-defined class, identified by the
    class definition's compile-time identity (SBCL's structure classoid
    type, ``(typep x 'node)``, adapted to Python classes).

    The fact is evidence for specialization, not a layout proof: classes
    and instances stay mutable, so code generated from it guards on the
    runtime class before using a field index or a resolved method.
    Subclass instances are a different class and do not fit."""

    identity: str = ""
    may_be_unbound: bool = False
    fixed_slots: tuple[str, ...] = ()

    def accept(self, visitor):
        return visitor.visit_InstanceType(self)

    @property
    def is_heap_object(self):
        return not self.may_be_unbound

    def instance_class(self) -> str | None:
        return None if self.may_be_unbound else self.identity

    def fixed_slot_index(self, name: str) -> int | None:
        if self.may_be_unbound or name not in self.fixed_slots:
            return None
        return self.fixed_slots.index(name)

    def runtime_class_type(self):
        return UNKNOWN if self.may_be_unbound else ClassType((self,))

    def runtime_parent_class_type(self):
        return None if self.may_be_unbound else CLASS_OBJECT_TYPE

    def same_runtime_class(self, other) -> bool:
        return other.fits_instance(self)

    def annotation_conflict(self, contract):
        return False if self.may_be_unbound else contract.instance_conflict(self)

    def join(self, other):
        return other.join_instance(self)

    def join_instance(self, left):
        if left.identity != self.identity:
            return self.join_alternatives(left)
        if left.may_be_unbound or not self.may_be_unbound:
            return left
        return self

    def join_set(self, left):
        if left.identity != self.identity:
            return self.join_alternatives(left)
        may_be_unbound = left.may_be_unbound or self.may_be_unbound
        slots = self.fixed_slots or left.fixed_slots
        if not may_be_unbound and left.proven and slots == _TABLE_FIXED_SLOTS:
            return (
                FROZENSET_TYPE if self.identity == "mule_dict_frozenset" else SET_TYPE
            )
        return SetType(self.identity, may_be_unbound, slots, UNKNOWN, left.proven)

    def fits_set(self, expected=None):
        if self.identity not in ("mule_dict_set", "mule_dict_frozenset"):
            return False
        if expected is None:
            return not self.may_be_unbound
        if self.may_be_unbound and not expected.may_be_unbound:
            return False
        if self.identity != expected.identity:
            return False
        return expected.element is UNKNOWN

    def fits_mutable_set(self) -> bool:
        return not self.may_be_unbound and self.identity == "mule_dict_set"

    def compare_identity_with(self, left, operator):
        # Two values of one class may still be distinct objects; only None
        # can be ruled out.
        if not self.may_be_unbound and left.fits_none(NONE_TYPE):
            is_same = operator is ast.ComparisonOperator.IS
            return BOOLEAN_FALSE if is_same else BOOLEAN_TRUE
        return BOOLEAN

    def narrow_none(self, is_none: bool):
        if self.may_be_unbound:
            return self
        return BOTTOM if is_none else self

    def truth_value(self) -> bool | None:
        # ``__bool__`` and ``__len__`` may be defined by the class.
        return None

    def dispatch_domain(self):
        return None if self.may_be_unbound else self

    def dispatch_rank(self) -> int:
        return 2**64 if self.may_be_unbound else 0

    def accepts(self, actual):
        return actual.fits_instance(self)

    def accepts_dispatch(self, actual):
        return actual.fits_instance(self)

    def accepts_possible_dispatch(self, actual):
        return actual.fits_possible_instance(self)

    def fits_instance(self, expected):
        return self.identity == expected.identity and (
            expected.may_be_unbound or not self.may_be_unbound
        )

    def method_return_type(
        self, name: str, arity: int, has_keywords: bool, storage_methods=None
    ):
        if self.may_be_unbound or storage_methods is None or not self.identity:
            return UNKNOWN
        return storage_methods.get((self.identity, name), UNKNOWN)


_TABLE_FIXED_SLOTS: tuple[str, ...] = (
    "_indices",
    "_hashes",
    "_keys",
    "_values",
    "_used",
)


@dataclass(frozen=True, slots=True)
class SetType(InstanceType):
    """Exact builtin set or frozenset instance with element type information."""

    identity: str = "mule_dict_set"
    may_be_unbound: bool = False
    fixed_slots: tuple[str, ...] = _TABLE_FIXED_SLOTS
    element: ValueType = UNKNOWN
    proven: bool = True

    def accept(self, visitor):
        return visitor.visit_SetType(self)

    @property
    def is_heap_object(self):
        return self.proven and not self.may_be_unbound

    @property
    def may_run_user_code(self):
        return self.may_be_unbound or not self.proven or self.element.may_run_user_code

    def instance_class(self) -> str | None:
        return None if (self.may_be_unbound or not self.proven) else self.identity

    def fixed_slot_index(self, name: str) -> int | None:
        if self.may_be_unbound or not self.proven or name not in self.fixed_slots:
            return None
        return self.fixed_slots.index(name)

    def runtime_class_type(self):
        if self.may_be_unbound or not self.proven:
            return UNKNOWN
        return (
            CLASS_FROZENSET_TYPE
            if self.identity == "mule_dict_frozenset"
            else CLASS_SET_TYPE
        )

    def runtime_parent_class_type(self):
        return (
            UNKNOWN if (self.may_be_unbound or not self.proven) else CLASS_OBJECT_TYPE
        )

    def same_runtime_class(self, other) -> bool:
        return self.proven and other.fits_set(SetType(self.identity))

    def exact_length(self) -> int | None:
        return (
            0
            if (self.proven and not self.may_be_unbound and self.element is BOTTOM)
            else None
        )

    def truth_value(self) -> bool | None:
        if self.proven and not self.may_be_unbound and self.element is BOTTOM:
            return False
        return None

    def annotation_conflict(self, contract):
        return (
            False
            if (self.may_be_unbound or not self.proven)
            else contract.set_conflict(self)
        )

    def annotated_set(self, expected):
        if self.identity == expected.identity:
            if not self.proven:
                return SetType(
                    self.identity,
                    self.may_be_unbound,
                    self.fixed_slots,
                    self.element,
                    True,
                )
            return self
        return SetType(expected.identity, fixed_slots=expected.fixed_slots)

    def unspecialized_container(self):
        return FROZENSET_TYPE if self.identity == "mule_dict_frozenset" else SET_TYPE

    def dispatch_domain(self):
        if self.may_be_unbound or not self.proven:
            return None
        return FROZENSET_TYPE if self.identity == "mule_dict_frozenset" else SET_TYPE

    def dispatch_rank(self) -> int:
        return 2**64 if (self.may_be_unbound or not self.proven) else 0

    def as_bound(self):
        if not self.may_be_unbound:
            if (
                self.element is UNKNOWN
                and self.proven
                and self.fixed_slots == _TABLE_FIXED_SLOTS
            ):
                return (
                    FROZENSET_TYPE
                    if self.identity == "mule_dict_frozenset"
                    else SET_TYPE
                )
            return self
        if (
            self.element is UNKNOWN
            and self.proven
            and self.fixed_slots == _TABLE_FIXED_SLOTS
        ):
            return (
                FROZENSET_TYPE if self.identity == "mule_dict_frozenset" else SET_TYPE
            )
        return SetType(
            self.identity, False, self.fixed_slots, self.element, self.proven
        )

    def with_unbound(self):
        if self.may_be_unbound:
            return self
        return SetType(self.identity, True, self.fixed_slots, self.element, self.proven)

    def limit_container_depth(self, max_depth: int = 3):
        if self.element is BOTTOM or self.element is UNKNOWN:
            return self
        if max_depth <= 0:
            if not self.proven:
                return SetType(
                    self.identity,
                    self.may_be_unbound,
                    self.fixed_slots,
                    UNKNOWN,
                    False,
                )
            if not self.may_be_unbound and self.fixed_slots == _TABLE_FIXED_SLOTS:
                return (
                    FROZENSET_TYPE
                    if self.identity == "mule_dict_frozenset"
                    else SET_TYPE
                )
            return SetType(
                self.identity, self.may_be_unbound, self.fixed_slots, UNKNOWN, True
            )
        limited = self.element.limit_container_depth(max_depth - 1)
        if (
            limited is UNKNOWN
            and not self.may_be_unbound
            and self.proven
            and self.fixed_slots == _TABLE_FIXED_SLOTS
        ):
            return (
                FROZENSET_TYPE if self.identity == "mule_dict_frozenset" else SET_TYPE
            )
        if limited == self.element:
            return self
        return SetType(
            self.identity, self.may_be_unbound, self.fixed_slots, limited, self.proven
        )

    def has_unspecialized_part(self) -> bool:
        return self.element.has_unspecialized_part()

    def has_bottom_part(self) -> bool:
        return self.element.has_bottom_part()

    def widen_constant(self):
        if not self.proven:
            return SetType(
                self.identity, self.may_be_unbound, self.fixed_slots, UNKNOWN, False
            )
        if not self.may_be_unbound and self.fixed_slots == _TABLE_FIXED_SLOTS:
            return (
                FROZENSET_TYPE if self.identity == "mule_dict_frozenset" else SET_TYPE
            )
        return SetType(
            self.identity, self.may_be_unbound, self.fixed_slots, UNKNOWN, True
        )

    def conservative_type(self):
        if self.identity == "mule_dict_frozenset":
            if self.element is BOTTOM or self.element is UNKNOWN:
                return self
            new_elem = self.element.conservative_type()
            if new_elem == self.element:
                return self
            return SetType(
                self.identity,
                self.may_be_unbound,
                self.fixed_slots,
                new_elem,
                self.proven,
            )
        return self.widen_constant()

    def join(self, other):
        return other.join_set(self)

    def join_instance(self, left):
        if left.identity != self.identity:
            return self.join_alternatives(left)
        may_be_unbound = left.may_be_unbound or self.may_be_unbound
        slots = self.fixed_slots or left.fixed_slots
        if not may_be_unbound and self.proven and slots == _TABLE_FIXED_SLOTS:
            return (
                FROZENSET_TYPE if self.identity == "mule_dict_frozenset" else SET_TYPE
            )
        return SetType(self.identity, may_be_unbound, slots, UNKNOWN, self.proven)

    def join_set(self, left):
        if left is self:
            return self
        if left.identity != self.identity:
            return self.join_alternatives(left)
        may_be_unbound = left.may_be_unbound or self.may_be_unbound
        proven = left.proven and self.proven
        slots = self.fixed_slots or left.fixed_slots
        if left.element is BOTTOM:
            element = self.element
        elif self.element is BOTTOM:
            element = left.element
        else:
            element = left.element.join(self.element).limit_container_depth(3)
        if (
            element is UNKNOWN
            and not may_be_unbound
            and proven
            and slots == _TABLE_FIXED_SLOTS
        ):
            return (
                FROZENSET_TYPE if self.identity == "mule_dict_frozenset" else SET_TYPE
            )
        if (
            element == self.element
            and may_be_unbound == self.may_be_unbound
            and proven == self.proven
            and slots == self.fixed_slots
        ):
            return self
        if (
            element == left.element
            and may_be_unbound == left.may_be_unbound
            and proven == left.proven
            and slots == left.fixed_slots
        ):
            return left
        return SetType(self.identity, may_be_unbound, slots, element, proven)

    def widen(self, new, thresholds: tuple[int, ...] = ()):
        return new.widen_set(self, thresholds)

    def widen_set(self, old, thresholds: tuple[int, ...] = ()):
        if old.identity != self.identity:
            return self
        element = widen_fact(
            old.element, self.element, thresholds
        ).limit_container_depth(3)
        may_be_unbound = old.may_be_unbound or self.may_be_unbound
        proven = old.proven and self.proven
        slots = self.fixed_slots or old.fixed_slots
        if (
            element == self.element
            and may_be_unbound == self.may_be_unbound
            and proven == self.proven
            and slots == self.fixed_slots
        ):
            return self
        if (
            element is UNKNOWN
            and not may_be_unbound
            and proven
            and slots == _TABLE_FIXED_SLOTS
        ):
            return (
                FROZENSET_TYPE if self.identity == "mule_dict_frozenset" else SET_TYPE
            )
        return SetType(self.identity, may_be_unbound, slots, element, proven)

    def accepts(self, actual):
        return actual.fits_set(self)

    def accepts_dispatch(self, actual):
        return (
            not self.may_be_unbound
            and self.proven
            and self.element is UNKNOWN
            and actual.fits_set(self)
        )

    def accepts_possible_dispatch(self, actual):
        return (
            not self.may_be_unbound
            and self.proven
            and self.element is UNKNOWN
            and actual.fits_possible_set(self)
        )

    def fits_instance(self, expected):
        if not self.proven:
            return False
        return self.identity == expected.identity and (
            expected.may_be_unbound or not self.may_be_unbound
        )

    def fits_set(self, expected=None):
        if not self.proven:
            return False
        if expected is None:
            return not self.may_be_unbound
        if self.may_be_unbound and not expected.may_be_unbound:
            return False
        if self.identity != expected.identity:
            return False
        return (
            expected.element is UNKNOWN
            or self.element is BOTTOM
            or self.element.fits(expected.element)
        )

    def fits_mutable_set(self) -> bool:
        return (
            self.proven and not self.may_be_unbound and self.identity == "mule_dict_set"
        )

    def fits_possible_set(self, expected=None):
        if expected is None:
            return not self.may_be_unbound
        if self.may_be_unbound and not expected.may_be_unbound:
            return False
        return self.identity == expected.identity

    def iterate_type(self):
        if self.may_be_unbound:
            return UNKNOWN
        return IteratorType(UNKNOWN if self.element is BOTTOM else self.element)

    def flow_iterate_type(self):
        if self.may_be_unbound or not self.proven:
            return UNKNOWN
        return IteratorType(self.element)

    def with_element(self, element):
        elem = element.as_bound().limit_container_depth(2)
        new_elem = (
            elem
            if self.element is BOTTOM
            else self.element.join(elem).limit_container_depth(2)
        )
        return (
            SetType(
                self.identity,
                self.may_be_unbound,
                self.fixed_slots,
                new_elem,
                self.proven,
            ).as_bound()
            if not self.may_be_unbound
            else SetType(self.identity, True, self.fixed_slots, new_elem, self.proven)
        )

    def construct_type(self, arg_types: tuple, keywords: dict):
        if keywords or len(arg_types) > 1:
            return (
                FROZENSET_TYPE if self.identity == "mule_dict_frozenset" else SET_TYPE
            )
        empty = (
            EMPTY_FROZENSET_TYPE
            if self.identity == "mule_dict_frozenset"
            else EMPTY_SET_TYPE
        )
        if not arg_types:
            return empty
        iter_type = arg_types[0].flow_iterate_type()
        if iter_type.fits_iterator():
            return empty.with_element(iter_type.element)
        return FROZENSET_TYPE if self.identity == "mule_dict_frozenset" else SET_TYPE

    def binary(self, operator, right):
        if self.may_be_unbound:
            return right.binary_unknown(operator, self)
        return right.binary_set(operator, self)

    def binary_set(self, operator, left):
        if left.may_be_unbound or self.may_be_unbound:
            return UNKNOWN
        proven = left.proven and self.proven
        if operator == ast.BinaryOperator.SUBTRACT:
            return SetType(
                left.identity, False, left.fixed_slots, left.element, proven
            ).as_bound()
        if operator in {
            ast.BinaryOperator.BIT_AND,
            ast.BinaryOperator.BIT_OR,
            ast.BinaryOperator.BIT_XOR,
        }:
            if operator == ast.BinaryOperator.BIT_AND and (
                left.element is BOTTOM or self.element is BOTTOM
            ):
                joined_elem = BOTTOM
            elif left.element is BOTTOM:
                joined_elem = self.element
            elif self.element is BOTTOM:
                joined_elem = left.element
            else:
                joined_elem = left.element.join(self.element).limit_container_depth(2)
            return SetType(
                left.identity, False, left.fixed_slots, joined_elem, proven
            ).as_bound()
        return UNKNOWN

    def method_return_type(
        self, name: str, arity: int, has_keywords: bool, storage_methods=None
    ):
        if self.may_be_unbound:
            return UNKNOWN
        if not has_keywords:
            if self.identity == "mule_dict_set":
                if name in ("add", "discard", "remove") and arity == 1:
                    return NONE_TYPE
                if name == "clear" and arity == 0:
                    return NONE_TYPE
                if (
                    name
                    in (
                        "update",
                        "intersection_update",
                        "difference_update",
                        "symmetric_difference_update",
                    )
                    and arity == 1
                ):
                    return NONE_TYPE
                if name == "pop" and arity == 0 and self.element is not BOTTOM:
                    return self.element
            if name == "copy" and arity == 0:
                return SetType(
                    self.identity, False, self.fixed_slots, self.element
                ).as_bound()
            if name == "difference" and arity == 1:
                return SetType(
                    self.identity, False, self.fixed_slots, self.element
                ).as_bound()
            if name in ("issubset", "issuperset", "isdisjoint") and arity == 1:
                return BOOLEAN
        if storage_methods is not None:
            res = storage_methods.get((self.identity, name), UNKNOWN)
            if res is not UNKNOWN:
                return res
            short_name = (
                "frozenset" if self.identity == "mule_dict_frozenset" else "set"
            )
            return storage_methods.get((short_name, name), UNKNOWN)
        return UNKNOWN

    def method_call(
        self, name: str, arg_types: tuple, has_keywords: bool, storage_methods=None
    ):
        if self.may_be_unbound:
            return UNKNOWN, None, None, False, True
        arity = len(arg_types)
        if not has_keywords:
            if self.identity == "mule_dict_set":
                if name == "add" and arity == 1:
                    may_call = arg_types[0].may_run_user_code
                    return (
                        NONE_TYPE if not arg_types[0].may_be_unbound else UNKNOWN,
                        self.with_element(arg_types[0]),
                        0,
                        False,
                        may_call,
                    )
                if name in ("discard", "remove") and arity == 1:
                    may_call = arg_types[0].may_run_user_code
                    return (
                        NONE_TYPE if not arg_types[0].may_be_unbound else UNKNOWN,
                        None,
                        None,
                        False,
                        may_call,
                    )
                if name == "pop" and arity == 0:
                    res = self.element if self.element is not BOTTOM else UNKNOWN
                    return res, None, None, True, True
                if name == "clear" and arity == 0:
                    return NONE_TYPE, None, None, False, False
                if name == "update" and arity == 1:
                    elem_type = arg_types[0].iterate_type().next_item_type()
                    return (
                        NONE_TYPE,
                        self.with_element(elem_type),
                        None,
                        False,
                        True,
                    )
            if name == "copy" and arity == 0:
                res = SetType(
                    self.identity, False, self.fixed_slots, self.element
                ).as_bound()
                return res, None, None, False, False
            if name == "difference" and arity == 1:
                res = SetType(
                    self.identity, False, self.fixed_slots, self.element
                ).as_bound()
                return res, None, None, False, True
            if name in ("union", "intersection", "symmetric_difference") and arity == 1:
                other_elem = arg_types[0].iterate_type().next_item_type()
                base = SetType(
                    self.identity, False, self.fixed_slots, self.element
                ).as_bound()
                return base.with_element(other_elem), None, None, False, True
            if name in ("issubset", "issuperset", "isdisjoint") and arity == 1:
                return BOOLEAN, None, None, False, True
        return (
            self.method_return_type(name, arity, has_keywords, storage_methods),
            None,
            None,
            False,
            True,
        )
TEXT_TYPE = TextType()
INTERNED_TEXT_TYPE = TextType(interned=True)
BYTES_TYPE = BytesType()
BYTEARRAY_TYPE = ByteArrayType()
LIST_TYPE = ListType()
EMPTY_LIST_TYPE = ListType(BOTTOM)
DICT_TYPE = DictType()
EMPTY_DICT_TYPE = DictType(BOTTOM, BOTTOM)
SET_TYPE = SetType("mule_dict_set")
EMPTY_SET_TYPE = SetType("mule_dict_set", element=BOTTOM)
FROZENSET_TYPE = SetType("mule_dict_frozenset")
EMPTY_FROZENSET_TYPE = SetType("mule_dict_frozenset", element=BOTTOM)
ITERATOR_TYPE = IteratorType()
CLASS_STR_TYPE = ClassType((TEXT_TYPE,))
CLASS_BYTES_TYPE = ClassType((BYTES_TYPE,))
CLASS_BYTEARRAY_TYPE = ClassType((BYTEARRAY_TYPE,))
CLASS_TUPLE_TYPE = ClassType((TupleType(),))
CLASS_LIST_TYPE = ClassType((EMPTY_LIST_TYPE,))
CLASS_DICT_TYPE = ClassType((EMPTY_DICT_TYPE,))
CLASS_SET_TYPE = ClassType((SET_TYPE,))
CLASS_FROZENSET_TYPE = ClassType((FROZENSET_TYPE,))
CLASS_ITERATOR_TYPE = ClassType((ITERATOR_TYPE,))

BUILTIN_CLASS_TYPES: dict[str, ClassType] = {
    "int": CLASS_INT_TYPE,
    "bool": CLASS_BOOL_TYPE,
    "float": CLASS_FLOAT_TYPE,
    "str": CLASS_STR_TYPE,
    "bytes": CLASS_BYTES_TYPE,
    "bytearray": CLASS_BYTEARRAY_TYPE,
    "tuple": CLASS_TUPLE_TYPE,
    "list": CLASS_LIST_TYPE,
    "dict": CLASS_DICT_TYPE,
    "set": CLASS_SET_TYPE,
    "frozenset": CLASS_FROZENSET_TYPE,
    "object": CLASS_OBJECT_TYPE,
    "type": CLASS_TYPE_TYPE,
}
STORAGE_CLASS_TYPES: dict[str, ValueType] = {
    "TextStorage": TEXT_TYPE,
    "BytesStorage": BYTES_TYPE,
    "bytearray": BYTEARRAY_TYPE,
    "TupleStorage": TupleType(),
    "ListStorage": LIST_TYPE,
    "dict": DICT_TYPE,
    "set": SET_TYPE,
    "frozenset": FROZENSET_TYPE,
}


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
        interned = data.get("interned")
        if lit is None and not unbound:
            return INTERNED_TEXT_TYPE if interned else TEXT_TYPE
        return TextType(lit, may_be_unbound=unbound, interned=interned)
    if kind == "bytes":
        return BytesType(may_be_unbound=unbound) if unbound else BYTES_TYPE
    if kind == "byte_storage":
        return ByteStorageType(may_be_unbound=unbound) if unbound else BYTE_STORAGE_TYPE
    if kind == "word_vector":
        return WordVectorType(may_be_unbound=unbound) if unbound else WORD_VECTOR_TYPE
    if kind == "bytearray":
        return ByteArrayType(may_be_unbound=unbound) if unbound else BYTEARRAY_TYPE
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
    if kind == "union":
        result = union_type(
            *(value_type_from_dict(member) for member in data["members"])
        )
        return result.with_unbound() if unbound else result
    if kind == "class":
        instances = tuple(
            value_type_from_dict(inst) for inst in data.get("instances", ())
        )
        return ClassType(
            instances,
            may_be_unbound=unbound,
        )
    if kind == "unique_obj":
        return UniqueObjectType(data.get("id"), may_be_unbound=unbound)
    if kind == "instance":
        cls_id = data["class"]
        slots = tuple(data.get("fixed_slots", ()))
        if cls_id in ("mule_dict_set", "mule_dict_frozenset"):
            slots = slots or _TABLE_FIXED_SLOTS
            if not unbound and slots == _TABLE_FIXED_SLOTS:
                return FROZENSET_TYPE if cls_id == "mule_dict_frozenset" else SET_TYPE
            return SetType(cls_id, may_be_unbound=unbound, fixed_slots=slots)
        return InstanceType(
            cls_id,
            may_be_unbound=unbound,
            fixed_slots=slots,
        )
    if kind == "set":
        cls_id = data.get("class", "mule_dict_set")
        elem = value_type_from_dict(data.get("element"))
        slots = tuple(data.get("fixed_slots", ())) or _TABLE_FIXED_SLOTS
        if elem is UNKNOWN and not unbound and slots == _TABLE_FIXED_SLOTS:
            return FROZENSET_TYPE if cls_id == "mule_dict_frozenset" else SET_TYPE
        if elem is BOTTOM and not unbound and slots == _TABLE_FIXED_SLOTS:
            return (
                EMPTY_FROZENSET_TYPE
                if cls_id == "mule_dict_frozenset"
                else EMPTY_SET_TYPE
            )
        return SetType(cls_id, may_be_unbound=unbound, fixed_slots=slots, element=elem)
    return UNKNOWN


__all__ = [
    "ANY_CLASS_TYPE",
    "BOOLEAN",
    "BOOLEAN_FALSE",
    "BOOLEAN_TRUE",
    "BOTTOM",
    "BUILTIN_CLASS_TYPES",
    "BYTEARRAY_TYPE",
    "BYTES_TYPE",
    "BYTE_STORAGE_TYPE",
    "CLASS_BOOL_TYPE",
    "CLASS_BYTEARRAY_TYPE",
    "CLASS_BYTES_TYPE",
    "CLASS_DICT_TYPE",
    "CLASS_FLOAT_TYPE",
    "CLASS_FROZENSET_TYPE",
    "CLASS_FUNCTION_TYPE",
    "CLASS_INT_TYPE",
    "CLASS_ITERATOR_TYPE",
    "CLASS_LIST_TYPE",
    "CLASS_NONE_TYPE",
    "CLASS_OBJECT_TYPE",
    "CLASS_SET_TYPE",
    "CLASS_STR_TYPE",
    "CLASS_TUPLE_TYPE",
    "CLASS_TYPE_TYPE",
    "DICT_TYPE",
    "EMPTY_DICT_TYPE",
    "EMPTY_FROZENSET_TYPE",
    "EMPTY_LIST_TYPE",
    "EMPTY_SET_TYPE",
    "FIXNUM_MAX",
    "FIXNUM_MIN",
    "FLOAT",
    "FROZENSET_TYPE",
    "FUNCTION_TYPE",
    "IMMORTAL",
    "IMMORTAL_UNBOUND",
    "INTEGER",
    "INTERNED_TEXT_TYPE",
    "ITERATOR_TYPE",
    "LIST_TYPE",
    "MAX_UNION_MEMBERS",
    "NONE_TYPE",
    "OPTIONAL_FIXNUM",
    "PYTHON_INTEGER",
    "REAL",
    "SET_TYPE",
    "STORAGE_CLASS_TYPES",
    "TEXT_TYPE",
    "UNBOUND",
    "UNKNOWN",
    "UNKNOWN_UNBOUND",
    "WORD_VECTOR_TYPE",
    "BooleanType",
    "BottomType",
    "ByteArrayType",
    "ByteStorageType",
    "BytesType",
    "ClassType",
    "DictType",
    "FloatType",
    "FunctionType",
    "ImmortalType",
    "InstanceType",
    "IntegerType",
    "IteratorType",
    "ListType",
    "NoneType",
    "PrimitiveType",
    "PythonIntegerType",
    "RealType",
    "SetType",
    "TextType",
    "TupleType",
    "UnboundType",
    "UnionType",
    "UniqueObjectType",
    "UnknownType",
    "ValueType",
    "ValueTypeVisitor",
    "WordVectorType",
    "union_type",
    "value_type_from_dict",
    "widen_fact",
]
