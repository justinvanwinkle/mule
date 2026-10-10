"""Compiler-owned numeric contracts exported by the Mule standard library.

These are lexical annotation bindings and semantic ranges, separate from the
runtime representation of values. Contracts select their backend behavior
through visitors.
"""

from dataclasses import dataclass

from .value_facts import FLOAT, IntegerType


@dataclass(frozen=True, slots=True)
class OptimizationPolicy:
    speed: int = 3
    safety: int = 0
    debug: int = 1


class Contract:
    def accept(self, visitor):
        return visitor.visit_Contract(self)

    def contradicts(self, actual):
        return actual.annotation_conflict(self)

    def refine(self, actual):
        return actual if actual.fits(self.fact) else self.fact

    def integer_conflict(self, actual):
        return True

    def boolean_conflict(self, actual):
        return True

    def validate(self, actual, location):
        if self.contradicts(actual):
            raise SyntaxError(
                f"value contradicts annotation mule.{self.name}", location
            )


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
