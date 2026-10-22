"""Compiler-owned numeric contracts exported by the Mule standard library.

These are lexical annotation bindings and semantic ranges, separate from the
runtime representation of values. Contracts select their backend behavior
through visitors.
"""

from dataclasses import dataclass

from .ctype import (
    BYTES_TYPE,
    FLOAT,
    LIST_TYPE,
    NONE_TYPE,
    PYTHON_INTEGER,
    REAL,
    TEXT_TYPE,
    UNKNOWN,
    ClassType,
    DictType,
    IntegerType,
    ListType,
    TupleType,
    ValueType,
    ValueTypeVisitor,
    union_type,
)


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
    def display_name(self):
        return f"mule.{self.name}"

    def accept(self, visitor):
        return visitor.visit_Contract(self)

    def contradicts(self, actual):
        return actual.annotation_conflict(self)

    def refine(self, actual):
        return actual if actual.fits(self.fact) else self.fact

    def general_parameter_type(self) -> ValueType:
        return UNKNOWN

    def requires_check(self, policy_checks: bool) -> bool:
        return policy_checks

    # Annotation hints do not establish a checked callable interface.
    def definition_contract(self):
        pass

    def integer_conflict(self, actual):
        return True

    def boolean_conflict(self, actual):
        return True

    def text_conflict(self, actual):
        return True

    def bytes_conflict(self, actual):
        return True

    def list_conflict(self, actual):
        return True

    def tuple_conflict(self, actual):
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

    def accept(self, visitor):
        return visitor.visit_PythonIntegerContract(self)

    def integer_conflict(self, actual):
        return False

    def boolean_conflict(self, actual):
        return False


class PythonFloatContract(FloatContract):
    def definition_contract(self):
        pass

    @property
    def fact(self):
        return REAL

    def accept(self, visitor):
        return visitor.visit_PythonFloatContract(self)

    def integer_conflict(self, actual):
        return False

    def boolean_conflict(self, actual):
        return False

    @property
    def display_name(self):
        return self.name


@dataclass(frozen=True, slots=True)
class TextContract(Contract):
    name: str = "str"

    @property
    def display_name(self):
        return self.name

    @property
    def fact(self):
        return TEXT_TYPE

    def accept(self, visitor):
        return visitor.visit_PythonIntegerContract(self)

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

    def accept(self, visitor):
        return visitor.visit_PythonIntegerContract(self)

    def bytes_conflict(self, actual):
        return False


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

    def general_parameter_type(self) -> ValueType:
        if self.element is None:
            return UNKNOWN
        return self.fact

    def accept(self, visitor):
        return visitor.visit_PythonIntegerContract(self)

    def list_conflict(self, actual):
        return (
            self.element is not None
            and actual.element is not UNKNOWN
            and self.element.contradicts(actual.element)
        )

    def refine(self, actual):
        return actual.annotated_list(self.fact)


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

    @property
    def fact(self):
        return self.annotation.fact

    def accept(self, visitor):
        return self.annotation.accept(visitor)

    def contradicts(self, actual):
        return False

    def refine(self, actual):
        return self.annotation.refine(actual)

    def requires_check(self, policy_checks):
        return True


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
        return UNKNOWN if fact.may_be_unbound else LIST_TYPE

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
        # Rebinding a class can change its runtime instance identity/layout.
        return UNKNOWN

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
    "str": TextContract(),
    "bytes": BytesContract(),
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
