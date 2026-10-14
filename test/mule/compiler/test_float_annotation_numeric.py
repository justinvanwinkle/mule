"""Python float annotations retain integer objects and never imply float storage."""

from mule.compiler import emit_llvm
from mule.compiler.annotation_types import BUILTINS
from mule.compiler.ctype import (
    BOOLEAN,
    FLOAT,
    PYTHON_INTEGER,
    REAL,
    UNKNOWN,
    IntegerType,
)
from test.mule.compiler.test_exception_groups import compare


def test_real_fact_membership_joins_and_concrete_refinement():
    contract = BUILTINS["float"]
    for fact in (IntegerType(0, 10), PYTHON_INTEGER, BOOLEAN, FLOAT, REAL):
        assert fact.fits(REAL)
        assert not contract.contradicts(fact)
        assert contract.refine(fact) is fact
        assert fact.join(REAL) == REAL.join(fact) == REAL
    assert not UNKNOWN.fits(REAL)
    assert not REAL.fits(FLOAT)
    assert contract.refine(UNKNOWN) is REAL


def test_float_annotations_preserve_numeric_values_through_unknown_calls(tmp_path):
    compare(
        tmp_path,
        """
def identity(value: float) -> float:
    local: float = value
    return local
def invoke(function, value):
    return function(value)
for original in (False, True, 42, -(2 ** 100), 2 ** 4000, 1.25):
    result = invoke(identity, original)
    print(type(result).__name__, result is original, result == original)
    print(type(identity(result) + 1).__name__)
def subtract(value: float):
    return 2.5 - value
print(invoke(subtract, 2), invoke(subtract, True), invoke(subtract, 1.25))
""",
    )


def test_real_annotation_does_not_assume_float_unboxing():
    llvm = emit_llvm("def identity(value: float) -> float:\n return value\n")
    assert "call i32 @mule_assert_real" in llvm
    assert "call i32 @mule_assert_float" not in llvm
