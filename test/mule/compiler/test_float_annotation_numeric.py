"""Python float annotations enforce exact float values and enable unboxed double storage."""

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


def test_float_contract_and_real_fact_membership():
    contract = BUILTINS["float"]
    assert contract.fact is FLOAT
    assert not contract.contradicts(FLOAT)
    assert contract.refine(FLOAT) is FLOAT
    assert contract.refine(UNKNOWN) is FLOAT
    for int_fact in (IntegerType(0, 10), PYTHON_INTEGER, BOOLEAN):
        assert contract.contradicts(int_fact)
        assert int_fact.fits(REAL)
        assert int_fact.join(REAL) == REAL.join(int_fact) == REAL
    assert FLOAT.fits(REAL)
    assert not UNKNOWN.fits(REAL)
    assert not REAL.fits(FLOAT)


def test_float_annotations_enforce_exact_float_at_runtime(tmp_path):
    compare(
        tmp_path,
        """
def identity(value: float) -> float:
    local: float = value
    return local
def invoke(function, value):
    return function(value)
result = invoke(identity, 1.25)
print(type(result).__name__, result == 1.25)
print(type(identity(result) + 1.0).__name__)
def subtract(value: float) -> float:
    return 2.5 - value
print(invoke(subtract, 1.25))
""",
    )


def test_float_annotation_asserts_exact_float_and_unboxes():
    llvm = emit_llvm(
        "def add_one(value: float) -> float:\n"
        "    return value + 1.0\n"
        "def use_add_one(x: float) -> float:\n"
        "    return add_one(x) * 2.0\n"
    )
    assert "call i32 @mule_assert_float" in llvm
    assert "call i32 @mule_assert_real" not in llvm
    assert 'define internal double @"mule_fn_add_one.numeric.fixnum"(' in llvm
