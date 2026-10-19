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
    PYTHON_INTEGER,
    REAL,
    TEXT_TYPE,
    UNKNOWN,
    IntegerType,
    ListType,
    ValueType,
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
