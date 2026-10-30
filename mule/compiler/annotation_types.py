"""Compiler-owned numeric contracts exported by the Mule standard library.

These are lexical annotation bindings and semantic ranges, separate from the
runtime representation of values. Contracts select their backend behavior
through visitors.
"""

from dataclasses import dataclass

from .ctype import (
    BOOLEAN,
    BOTTOM,
    BYTEARRAY_TYPE,
    BYTES_TYPE,
    DICT_TYPE,
    FLOAT,
    FROZENSET_TYPE,
    LIST_TYPE,
    NONE_TYPE,
    PYTHON_INTEGER,
    REAL,
    SET_TYPE,
    TEXT_TYPE,
    UNKNOWN,
    ClassType,
    DictType,
    InstanceType,
    IntegerType,
    ListType,
    SetType,
    TupleType,
    ValueType,
    ValueTypeVisitor,
    union_type,
)


class ContractVisitor:
    """Dispatch recognized contracts to a phase's general contract policy."""

    def visit_Contract(self, contract):
        raise NotImplementedError(f"No visitor policy for {contract.display_name}")

    def visit_IntegerContract(self, contract):
        return self.visit_Contract(contract)

    def visit_FloatContract(self, contract):
        return self.visit_Contract(contract)

    def visit_PythonIntegerContract(self, contract):
        return self.visit_Contract(contract)

    def visit_PythonFloatContract(self, contract):
        return self.visit_FloatContract(contract)

    def visit_BooleanContract(self, contract):
        return self.visit_Contract(contract)

    def visit_NoneContract(self, contract):
        return self.visit_Contract(contract)

    def visit_TextContract(self, contract):
        return self.visit_Contract(contract)

    def visit_BytesContract(self, contract):
        return self.visit_Contract(contract)

    def visit_ByteArrayContract(self, contract):
        return self.visit_Contract(contract)

    def visit_ListContract(self, contract):
        return self.visit_Contract(contract)

    def visit_TupleContract(self, contract):
        return self.visit_Contract(contract)

    def visit_DictContract(self, contract):
        return self.visit_Contract(contract)

    def visit_SetContract(self, contract):
        return self.visit_Contract(contract)

    def visit_ClassContract(self, contract):
        return self.visit_Contract(contract)

    def visit_UnionContract(self, contract):
        return self.visit_Contract(contract)

    def visit_InferredReturnContract(self, contract):
        return self.visit_Contract(contract)

    def visit_DeclaredFieldContract(self, contract):
        return self.visit_Contract(contract)


class AnnotationLedger:
    """The annotation declarations one module's compilation proved wrong.

    A contradicted annotation is reported and then ignored rather than
    failing compilation. Sources are ``("local", lambda, name)``,
    ``("return", lambda)`` or ``("global", namespace, name)``; ``None`` marks
    an assertion without a declaration to ignore."""

    def __init__(self):
        # Contradictions found since the IR1 phases last applied them, as
        # (source, location, annotation name).
        self.contradictions: list = []
        self.ignored: set = set()

    def check(self, contract, actual, location, source=None) -> bool:
        """Whether ``actual`` provably contradicts ``contract``. A
        contradiction of a declaration not yet ignored is recorded."""
        if source in self.ignored or not contract.contradicts(actual):
            return False
        self.contradictions.append((source, location, contract.display_name))
        return True


@dataclass(frozen=True, slots=True)
class Contract:
    name: str

    @property
    def fact(self) -> ValueType:
        raise NotImplementedError

    @property
    def checked_type(self) -> ValueType:
        return self.fact

    @property
    def display_name(self):
        return f"mule_types.{self.name}"

    def accept(self, visitor):
        return visitor.visit_Contract(self)

    def contradicts(self, actual):
        return actual.annotation_conflict(self)

    def refine(self, actual):
        checked = self.checked_type
        return actual if actual.fits(checked) else checked

    def general_parameter_type(self) -> ValueType:
        return UNKNOWN

    def speculative_parameter_type(self, fixnum_int: ValueType) -> ValueType:
        return UNKNOWN

    def union_members(self) -> tuple[Contract, ...]:
        return (self,)

    def has_element_contracts(self) -> bool:
        return False

    def getitem_contract(self, index_type: ValueType) -> Contract | None:
        return None

    def iter_element_contract(self) -> Contract | None:
        return None

    def unpack_element_contract(self, arity: int, index: int) -> Contract | None:
        return None

    def method_result_contract(
        self, name: str, arg_types: tuple[ValueType, ...]
    ) -> Contract | None:
        return None

    def binary_result_contract(
        self, operator, other: Contract | None, other_type: ValueType = UNKNOWN
    ) -> Contract | None:
        return None

    def requires_check(self, policy_checks: bool) -> bool:
        return policy_checks

    # Annotation hints do not establish a checked callable interface.
    def definition_contract(self):
        pass

    def integer_conflict(self, actual):
        return True

    def boolean_conflict(self, actual):
        return True

    def float_conflict(self, actual):
        return True

    def none_conflict(self, actual):
        return True

    def text_conflict(self, actual):
        return True

    def bytes_conflict(self, actual):
        return True

    def bytearray_conflict(self, actual):
        return True

    def list_conflict(self, actual):
        return True

    def tuple_conflict(self, actual):
        return True

    def dict_conflict(self, actual):
        return True

    def set_conflict(self, actual):
        return True

    def instance_conflict(self, actual):
        return True


@dataclass(frozen=True, slots=True)
class IntegerContract(Contract):
    name: str
    bits: int
    signed: bool

    @property
    def fact(self):
        return IntegerType(
            -(1 << (self.bits - 1)) if self.signed else 0,
            (1 << (self.bits - int(self.signed))) - 1,
        )

    def speculative_parameter_type(self, fixnum_int: ValueType) -> ValueType:
        return self.fact.annotated_integer(fixnum_int)

    def accept(self, visitor):
        return visitor.visit_IntegerContract(self)

    def definition_contract(self):
        return self

    def integer_conflict(self, actual):
        expected = self.fact
        return (actual.maximum is not None and actual.maximum < expected.minimum) or (
            actual.minimum is not None and actual.minimum > expected.maximum
        )

    def refine(self, actual):
        return actual.annotated_integer(self.fact)


@dataclass(frozen=True, slots=True)
class FloatContract(Contract):
    name: str
    bits: int

    @property
    def fact(self):
        return FLOAT

    def speculative_parameter_type(self, fixnum_int: ValueType) -> ValueType:
        return FLOAT

    def float_conflict(self, actual):
        return False

    def accept(self, visitor):
        return visitor.visit_FloatContract(self)

    def definition_contract(self):
        return self


@dataclass(frozen=True, slots=True)
class PythonIntegerContract(Contract):
    name: str = "int"

    @property
    def display_name(self):
        return self.name

    @property
    def fact(self):
        return PYTHON_INTEGER

    def speculative_parameter_type(self, fixnum_int: ValueType) -> ValueType:
        return fixnum_int

    def accept(self, visitor):
        return visitor.visit_PythonIntegerContract(self)

    def integer_conflict(self, actual):
        return False

    def boolean_conflict(self, actual):
        return False


class PythonFloatContract(FloatContract):
    def definition_contract(self):
        pass

    def accept(self, visitor):
        return visitor.visit_PythonFloatContract(self)

    @property
    def display_name(self):
        return self.name


@dataclass(frozen=True, slots=True)
class BooleanContract(Contract):
    name: str = "bool"

    @property
    def display_name(self):
        return self.name

    @property
    def fact(self):
        return BOOLEAN

    def speculative_parameter_type(self, fixnum_int: ValueType) -> ValueType:
        return BOOLEAN

    def accept(self, visitor):
        return visitor.visit_BooleanContract(self)

    def integer_conflict(self, actual):
        return not actual.fits_possible_boolean(BOOLEAN)

    def boolean_conflict(self, actual):
        return False


@dataclass(frozen=True, slots=True)
class NoneContract(Contract):
    name: str = "None"

    @property
    def display_name(self):
        return self.name

    @property
    def fact(self):
        return NONE_TYPE

    def speculative_parameter_type(self, fixnum_int: ValueType) -> ValueType:
        return NONE_TYPE

    def accept(self, visitor):
        return visitor.visit_NoneContract(self)

    def none_conflict(self, actual):
        return False


@dataclass(frozen=True, slots=True)
class TextContract(Contract):
    name: str = "str"

    @property
    def display_name(self):
        return self.name

    @property
    def fact(self):
        return TEXT_TYPE

    def speculative_parameter_type(self, fixnum_int: ValueType) -> ValueType:
        return TEXT_TYPE

    def accept(self, visitor):
        return visitor.visit_TextContract(self)

    def text_conflict(self, actual):
        return False


@dataclass(frozen=True, slots=True)
class BytesContract(Contract):
    name: str = "bytes"

    @property
    def display_name(self):
        return self.name

    @property
    def fact(self):
        return BYTES_TYPE

    def speculative_parameter_type(self, fixnum_int: ValueType) -> ValueType:
        return BYTES_TYPE

    def accept(self, visitor):
        return visitor.visit_BytesContract(self)

    def bytes_conflict(self, actual):
        return False

    def getitem_contract(self, index_type: ValueType) -> Contract | None:
        return (
            IntegerContract("uint8", 8, False)
            if index_type.fits(PYTHON_INTEGER)
            else None
        )

    def iter_element_contract(self) -> Contract | None:
        return IntegerContract("uint8", 8, False)


@dataclass(frozen=True, slots=True)
class ByteArrayContract(Contract):
    name: str = "bytearray"

    @property
    def display_name(self):
        return self.name

    @property
    def fact(self):
        return BYTEARRAY_TYPE

    def speculative_parameter_type(self, fixnum_int: ValueType) -> ValueType:
        return BYTEARRAY_TYPE

    def accept(self, visitor):
        return visitor.visit_ByteArrayContract(self)

    def bytearray_conflict(self, actual):
        return False

    def getitem_contract(self, index_type: ValueType) -> Contract | None:
        return (
            IntegerContract("uint8", 8, False)
            if index_type.fits(PYTHON_INTEGER)
            else None
        )

    def iter_element_contract(self) -> Contract | None:
        return IntegerContract("uint8", 8, False)

    def method_result_contract(
        self, name: str, arg_types: tuple[ValueType, ...]
    ) -> Contract | None:
        if name == "pop" and len(arg_types) <= 1:
            return IntegerContract("uint8", 8, False)
        if name == "copy" and not arg_types:
            return self
        return None


@dataclass(frozen=True, slots=True)
class ListContract(Contract):
    name: str = "list"
    element: Contract | None = None

    @property
    def display_name(self):
        if self.element is None:
            return self.name
        return f"{self.name}[{self.element.display_name}]"

    @property
    def fact(self):
        if self.element is None:
            return LIST_TYPE
        return ListType(self.element.fact)

    @property
    def checked_type(self) -> ValueType:
        return LIST_TYPE

    def speculative_parameter_type(self, fixnum_int: ValueType) -> ValueType:
        if self.element is not None:
            spec_elem = self.element.speculative_parameter_type(fixnum_int)
            if spec_elem != self.element.fact and spec_elem.has_float_specialization():
                return ListType(spec_elem)
        return LIST_TYPE

    def accept(self, visitor):
        return visitor.visit_ListContract(self)

    def list_conflict(self, actual):
        return (
            self.element is not None
            and actual.element is not UNKNOWN
            and self.element.contradicts(actual.element)
        )

    def refine(self, actual):
        return actual.annotated_list(self.fact)

    def has_element_contracts(self) -> bool:
        return self.element is not None

    def getitem_contract(self, index_type: ValueType) -> Contract | None:
        return self.element if index_type.fits(PYTHON_INTEGER) else None

    def iter_element_contract(self) -> Contract | None:
        return self.element

    def unpack_element_contract(self, arity: int, index: int) -> Contract | None:
        return self.element

    def method_result_contract(
        self, name: str, arg_types: tuple[ValueType, ...]
    ) -> Contract | None:
        if name == "pop" and len(arg_types) <= 1:
            return self.element
        if name == "copy" and not arg_types:
            return self
        return None


@dataclass(frozen=True, slots=True)
class TupleContract(Contract):
    name: str = "tuple"
    arity: int | None = None
    elements: tuple[Contract | None, ...] | None = None

    @property
    def display_name(self):
        if self.arity is None and self.elements is None:
            return self.name
        if self.arity == 0:
            return f"{self.name}[()]"
        if self.arity is None and self.elements is not None and len(self.elements) == 1:
            elem_name = (
                self.elements[0].display_name
                if self.elements[0] is not None
                else "object"
            )
            return f"{self.name}[{elem_name}, ...]"
        if self.elements is not None:
            inner = ", ".join(
                el.display_name if el is not None else "object" for el in self.elements
            )
            return f"{self.name}[{inner}]"
        return self.name

    @property
    def fact(self):
        if self.elements is None:
            return TupleType(self.arity)
        elem_facts = tuple(
            el.fact if el is not None else UNKNOWN for el in self.elements
        )
        if all(el is UNKNOWN for el in elem_facts):
            return TupleType(self.arity)
        return TupleType(self.arity, elem_facts)

    @property
    def checked_type(self) -> ValueType:
        return TupleType(self.arity)

    def speculative_parameter_type(self, fixnum_int: ValueType) -> ValueType:
        if self.arity is None or self.arity > 8:
            return TupleType()
        if self.elements is None:
            return TupleType(self.arity)
        spec_elems = tuple(
            el.speculative_parameter_type(fixnum_int) if el is not None else UNKNOWN
            for el in self.elements
        )
        if all(el is UNKNOWN for el in spec_elems):
            return TupleType(self.arity)
        return TupleType(self.arity, spec_elems)

    def accept(self, visitor):
        return visitor.visit_TupleContract(self)

    def tuple_conflict(self, actual):
        if (
            self.arity is not None
            and actual.arity is not None
            and self.arity != actual.arity
        ):
            return True
        if self.elements is not None and actual.elements is not None:
            if self.arity is None and len(self.elements) == 1:
                elem_contract = self.elements[0]
                return elem_contract is not None and any(
                    el is not UNKNOWN and elem_contract.contradicts(el)
                    for el in actual.elements
                )
            if (
                self.arity is not None
                and actual.arity == self.arity
                and len(self.elements) == len(actual.elements)
            ):
                return any(
                    c_el is not None and a_el is not UNKNOWN and c_el.contradicts(a_el)
                    for c_el, a_el in zip(self.elements, actual.elements)
                )
        return False

    def refine(self, actual):
        return actual.annotated_tuple(self.fact)

    def has_element_contracts(self) -> bool:
        return self.elements is not None and any(el is not None for el in self.elements)

    def getitem_contract(self, index_type: ValueType) -> Contract | None:
        if self.elements is None or not index_type.fits(PYTHON_INTEGER):
            return None
        if self.arity is None and len(self.elements) == 1:
            return self.elements[0]
        idx = index_type.exact_integer()
        if idx is not None and self.arity is not None:
            if idx < 0:
                idx += self.arity
            if 0 <= idx < len(self.elements):
                return self.elements[idx]
        if self.elements and all(el == self.elements[0] for el in self.elements):
            return self.elements[0]
        return None

    def iter_element_contract(self) -> Contract | None:
        if self.elements is None or not self.elements:
            return None
        if self.arity is None and len(self.elements) == 1:
            return self.elements[0]
        if all(el == self.elements[0] for el in self.elements):
            return self.elements[0]
        if all(el is not None for el in self.elements):
            return union_contract(*self.elements)
        return None

    def unpack_element_contract(self, arity: int, index: int) -> Contract | None:
        if self.elements is None:
            return None
        if self.arity is None and len(self.elements) == 1:
            return self.elements[0]
        if self.arity == arity and 0 <= index < len(self.elements):
            return self.elements[index]
        return None


@dataclass(frozen=True, slots=True)
class DictContract(Contract):
    name: str = "dict"
    key: Contract | None = None
    value: Contract | None = None

    @property
    def display_name(self):
        if self.key is None and self.value is None:
            return self.name
        k_name = self.key.display_name if self.key is not None else "object"
        v_name = self.value.display_name if self.value is not None else "object"
        return f"{self.name}[{k_name}, {v_name}]"

    @property
    def fact(self):
        k_fact = self.key.fact if self.key is not None else UNKNOWN
        v_fact = self.value.fact if self.value is not None else UNKNOWN
        if k_fact is UNKNOWN and v_fact is UNKNOWN:
            return DICT_TYPE
        return DictType(k_fact, v_fact)

    @property
    def checked_type(self) -> ValueType:
        return DICT_TYPE

    def speculative_parameter_type(self, fixnum_int: ValueType) -> ValueType:
        return DICT_TYPE

    def accept(self, visitor):
        return visitor.visit_DictContract(self)

    def dict_conflict(self, actual):
        key_bad = (
            self.key is not None
            and actual.key is not UNKNOWN
            and self.key.contradicts(actual.key)
        )
        val_bad = (
            self.value is not None
            and actual.value is not UNKNOWN
            and self.value.contradicts(actual.value)
        )
        return key_bad or val_bad

    def refine(self, actual):
        return actual.annotated_dict(self.fact)

    def has_element_contracts(self) -> bool:
        return self.key is not None or self.value is not None

    def getitem_contract(self, index_type: ValueType) -> Contract | None:
        return self.value

    def iter_element_contract(self) -> Contract | None:
        return self.key

    def method_result_contract(
        self, name: str, arg_types: tuple[ValueType, ...]
    ) -> Contract | None:
        if name == "copy" and not arg_types:
            return self
        if name == "pop" and len(arg_types) == 1:
            return self.value
        if name == "get" and self.value is not None:
            if len(arg_types) == 1:
                return union_contract(self.value, NoneContract())
            if len(arg_types) == 2:
                if arg_types[1].fits_none(NONE_TYPE):
                    return union_contract(self.value, NoneContract())
                if arg_types[1].fits(self.value.fact):
                    return self.value
        return None


@dataclass(frozen=True, slots=True)
class SetContract(Contract):
    name: str = "set"
    identity: str = "mule_dict_set"
    element: Contract | None = None

    @property
    def display_name(self):
        if self.element is None:
            return self.name
        return f"{self.name}[{self.element.display_name}]"

    @property
    def fact(self):
        base = FROZENSET_TYPE if self.identity == "mule_dict_frozenset" else SET_TYPE
        if self.element is None:
            return base
        return SetType(self.identity, element=self.element.fact)

    @property
    def checked_type(self) -> ValueType:
        return FROZENSET_TYPE if self.identity == "mule_dict_frozenset" else SET_TYPE

    def speculative_parameter_type(self, fixnum_int: ValueType) -> ValueType:
        return FROZENSET_TYPE if self.identity == "mule_dict_frozenset" else SET_TYPE

    def accept(self, visitor):
        return visitor.visit_SetContract(self)

    def set_conflict(self, actual):
        if actual.identity != self.identity:
            return True
        return (
            self.element is not None
            and actual.element is not UNKNOWN
            and self.element.contradicts(actual.element)
        )

    def instance_conflict(self, actual):
        return actual.identity != self.identity

    def refine(self, actual):
        return actual.annotated_set(self.fact)

    def has_element_contracts(self) -> bool:
        return self.element is not None

    def iter_element_contract(self) -> Contract | None:
        return self.element

    def method_result_contract(
        self, name: str, arg_types: tuple[ValueType, ...]
    ) -> Contract | None:
        if name == "pop" and not arg_types and self.identity == "mule_dict_set":
            return self.element
        if name == "copy" and not arg_types:
            return self
        if name == "difference" and len(arg_types) == 1:
            return self
        return None

    def binary_result_contract(
        self, operator, other: Contract | None, other_type: ValueType = UNKNOWN
    ) -> Contract | None:
        from mule import ast

        if operator == ast.BinaryOperator.SUBTRACT:
            return self
        if operator in {
            ast.BinaryOperator.BIT_AND,
            ast.BinaryOperator.BIT_OR,
            ast.BinaryOperator.BIT_XOR,
        }:
            if self.element is None:
                return self
            if other is not None:
                other_elem = other.iter_element_contract()
                if other_elem is not None:
                    return SetContract(
                        self.name,
                        self.identity,
                        union_contract(self.element, other_elem),
                    )
            else:
                iter_t = other_type.flow_iterate_type()
                if (
                    iter_t.fits_iterator()
                    and iter_t.element is not UNKNOWN
                    and (
                        iter_t.element is BOTTOM
                        or iter_t.element.fits(self.element.fact)
                    )
                ):
                    return self
        return None


@dataclass(frozen=True, slots=True)
class ClassContract(Contract):
    name: str
    identity: str

    @property
    def display_name(self):
        return self.name

    @property
    def fact(self):
        return InstanceType(self.identity)

    def speculative_parameter_type(self, fixnum_int: ValueType) -> ValueType:
        return self.fact

    def accept(self, visitor):
        return visitor.visit_ClassContract(self)

    def instance_conflict(self, actual):
        return actual.identity != self.identity


@dataclass(frozen=True, slots=True)
class UnionContract(Contract):
    name: str
    members: tuple[Contract, ...]

    @property
    def display_name(self):
        return " | ".join(m.display_name for m in self.members)

    @property
    def fact(self):
        return union_type(*(m.fact for m in self.members))

    @property
    def checked_type(self) -> ValueType:
        return union_type(*(m.checked_type for m in self.members))

    def union_members(self) -> tuple[Contract, ...]:
        return self.members

    def speculative_parameter_type(self, fixnum_int: ValueType) -> ValueType:
        spec_members = tuple(
            m.speculative_parameter_type(fixnum_int) for m in self.members
        )
        if any(m is UNKNOWN for m in spec_members):
            return UNKNOWN
        return union_type(*spec_members)

    def accept(self, visitor):
        return visitor.visit_UnionContract(self)

    def integer_conflict(self, actual):
        return all(m.integer_conflict(actual) for m in self.members)

    def boolean_conflict(self, actual):
        return all(m.boolean_conflict(actual) for m in self.members)

    def float_conflict(self, actual):
        return all(m.float_conflict(actual) for m in self.members)

    def none_conflict(self, actual):
        return all(m.none_conflict(actual) for m in self.members)

    def text_conflict(self, actual):
        return all(m.text_conflict(actual) for m in self.members)

    def bytes_conflict(self, actual):
        return all(m.bytes_conflict(actual) for m in self.members)

    def bytearray_conflict(self, actual):
        return all(m.bytearray_conflict(actual) for m in self.members)

    def list_conflict(self, actual):
        return all(m.list_conflict(actual) for m in self.members)

    def tuple_conflict(self, actual):
        return all(m.tuple_conflict(actual) for m in self.members)

    def dict_conflict(self, actual):
        return all(m.dict_conflict(actual) for m in self.members)

    def set_conflict(self, actual):
        return all(m.set_conflict(actual) for m in self.members)

    def instance_conflict(self, actual):
        return all(m.instance_conflict(actual) for m in self.members)


def union_contract(*contracts: Contract) -> Contract:
    unique: list[Contract] = []
    for contract in contracts:
        for member in contract.union_members():
            if member not in unique:
                unique.append(member)
    if len(unique) == 1:
        return unique[0]
    return UnionContract(" | ".join(m.display_name for m in unique), tuple(unique))


@dataclass(frozen=True, slots=True)
class InferredReturnContract(Contract):
    """A checked definition-derived result type for a mutable call binding.

    This is separate from declaration annotations: a contradiction raises
    TypeError at the call boundary instead of invalidating an annotation.
    """

    name: str
    inferred_type: ValueType

    @property
    def display_name(self) -> str:
        return f"return value of {self.name}"

    @property
    def fact(self) -> ValueType:
        return self.inferred_type.accept(_RUNTIME_CHECKED_RETURN_FACT)

    def accept(self, visitor):
        return visitor.visit_InferredReturnContract(self)

    def contradicts(self, actual) -> bool:
        return False

    def refine(self, actual):
        expected = self.fact
        return actual if actual.fits(expected) else expected

    def requires_check(self, policy_checks: bool) -> bool:
        # Inferred result contracts must reject an incompatible rebound value,
        # including when safety=0 suppresses optional annotation checks.
        return True


@dataclass(frozen=True, slots=True)
class DeclaredReturnContract(Contract):
    """Retain an explicit return declaration at a mutable call boundary."""

    annotation: Contract
    inferred_type: ValueType = UNKNOWN

    @property
    def fact(self):
        inferred = self.inferred_type.accept(_RUNTIME_CHECKED_RETURN_FACT)
        declared = self.annotation.fact
        if inferred is not UNKNOWN and inferred.fits(declared):
            return inferred
        return declared

    @property
    def checked_type(self) -> ValueType:
        inferred = self.inferred_type.accept(_RUNTIME_CHECKED_RETURN_FACT)
        declared = self.annotation.checked_type
        if inferred is not UNKNOWN and inferred.fits(declared):
            return inferred
        return declared

    def accept(self, visitor):
        if self.checked_type != self.annotation.checked_type:
            return InferredReturnContract(self.name, self.checked_type).accept(visitor)
        return self.annotation.accept(visitor)

    def contradicts(self, actual):
        return False

    def refine(self, actual):
        refined = self.annotation.refine(actual)
        expected = self.fact
        return refined if refined.fits(expected) else expected

    def requires_check(self, policy_checks):
        return True


@dataclass(frozen=True, slots=True)
class DeclaredFieldContract(Contract):
    """Enforce a declared class field's type on resolved stores according to policy."""

    class_name: str
    annotation: Contract

    @property
    def display_name(self):
        return self.annotation.display_name

    @property
    def fact(self):
        return self.annotation.fact

    @property
    def checked_type(self) -> ValueType:
        return self.annotation.checked_type

    def accept(self, visitor):
        return self.annotation.accept(visitor)

    def contradicts(self, actual) -> bool:
        return False

    def refine(self, actual):
        return self.annotation.refine(actual)

    def requires_check(self, policy_checks: bool) -> bool:
        return policy_checks


class _RuntimeCheckedReturnFact(ValueTypeVisitor):
    """Weaken definition facts to types the call boundary can check safely."""

    def visit_ValueType(self, fact):
        return UNKNOWN

    def visit_UnknownType(self, fact):
        return UNKNOWN

    def visit_BottomType(self, fact):
        return UNKNOWN

    def visit_UnboundType(self, fact):
        return UNKNOWN

    def visit_IntegerType(self, fact):
        return UNKNOWN if fact.may_be_unbound else fact

    def visit_PythonIntegerType(self, fact):
        return UNKNOWN if fact.may_be_unbound else fact

    def visit_BooleanType(self, fact):
        return UNKNOWN if fact.may_be_unbound else fact

    def visit_FloatType(self, fact):
        return UNKNOWN if fact.may_be_unbound else FLOAT

    def visit_RealType(self, fact):
        if fact.may_be_unbound:
            return UNKNOWN
        return PYTHON_INTEGER.join(FLOAT)

    def visit_NoneType(self, fact):
        return UNKNOWN if fact.may_be_unbound else NONE_TYPE

    def visit_TextType(self, fact):
        return UNKNOWN if fact.may_be_unbound else TEXT_TYPE

    def visit_BytesType(self, fact):
        return UNKNOWN if fact.may_be_unbound else BYTES_TYPE

    def visit_ByteArrayType(self, fact):
        return UNKNOWN if fact.may_be_unbound else BYTEARRAY_TYPE

    def visit_TupleType(self, fact):
        if fact.may_be_unbound:
            return UNKNOWN
        elements = (
            None
            if fact.elements is None
            else tuple(element.accept(self) for element in fact.elements)
        )
        return TupleType(fact.arity, elements)

    def visit_ListType(self, fact):
        if fact.may_be_unbound:
            return UNKNOWN
        if (
            fact.element is not BOTTOM
            and fact.element is not UNKNOWN
            and (fact.has_float_specialization() or fact.element.fits_real(REAL))
        ):
            elem = fact.element.accept(self)
            if elem is not UNKNOWN:
                return ListType(elem)
        return LIST_TYPE

    def visit_DictType(self, fact):
        if fact.may_be_unbound or not fact.proven:
            return UNKNOWN
        return DictType(UNKNOWN, UNKNOWN)

    def visit_UnionType(self, fact):
        if fact.may_be_unbound:
            return UNKNOWN
        checked_members = tuple(member.accept(self) for member in fact.members)
        if any(member is UNKNOWN for member in checked_members):
            return UNKNOWN
        return union_type(*checked_members)

    def visit_ClassType(self, fact):
        # Class identity can change independently of the definition site.
        return UNKNOWN if fact.may_be_unbound else ClassType(())

    def visit_InstanceType(self, fact):
        return UNKNOWN if fact.may_be_unbound else fact.as_optional()

    def visit_SetType(self, fact):
        if fact.may_be_unbound or not fact.proven:
            return UNKNOWN
        return FROZENSET_TYPE if fact.identity == "mule_dict_frozenset" else SET_TYPE

    def visit_ImmortalType(self, fact):
        # Immortality is a physical ownership fact, not a Python value category.
        return UNKNOWN

    def visit_FunctionType(self, fact):
        # A callable's predicted target is not its mutable runtime identity.
        return UNKNOWN

    def visit_IteratorType(self, fact):
        # User-defined iterator implementations have no single physical layout.
        return UNKNOWN

    def visit_UniqueObjectType(self, fact):
        return UNKNOWN


_RUNTIME_CHECKED_RETURN_FACT = _RuntimeCheckedReturnFact()


BUILTINS = {"int": PythonIntegerContract(), "float": PythonFloatContract("float", 64)}

DECLARATIONS = {
    **BUILTINS,
    "bool": BooleanContract(),
    "str": TextContract(),
    "bytes": BytesContract(),
    "bytearray": ByteArrayContract(),
    "set": SetContract("set", "mule_dict_set"),
    "frozenset": SetContract("frozenset", "mule_dict_frozenset"),
}


EXPORTS = {
    **{
        f"int{bits}": IntegerContract(f"int{bits}", bits, True)
        for bits in (8, 16, 32, 64, 128)
    },
    **{
        f"uint{bits}": IntegerContract(f"uint{bits}", bits, False)
        for bits in (8, 16, 32, 64, 128)
    },
    **{
        f"float{bits}": FloatContract(f"float{bits}", bits)
        for bits in (16, 32, 64, 128)
    },
}
