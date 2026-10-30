import re

from mule.compiler import emit_llvm

CHECK_POSITIVE_SOURCE = """
def check_positive(x: int) -> int:
    if x < 0:
        raise ValueError("negative")
    return x + 1
"""

DIVIDE_SOURCE = """
def divide(a: float, b: float) -> float:
    return a / b
"""


def test_raise_branch_is_marked_cold_and_calls_cold_noinline() -> None:
    llvm = emit_llvm(CHECK_POSITIVE_SOURCE)
    assert re.search(
        r'br i1 %[^,]+, label %block\.2, label %block\.1, !prof !\{!"branch_weights", i32 1, i32 2000\}',
        llvm,
    )
    assert re.search(
        r'call ptr @"mule\.protocol\.raise_value"\(ptr %[^)]+\) cold noinline',
        llvm,
    )


def test_guard_and_zero_division_branches_are_marked_cold() -> None:
    llvm = emit_llvm(DIVIDE_SOURCE)
    assert re.search(
        r'br i1 %[^,]+, label %[^,]+, label %[^,]+, !prof !\{!"branch_weights", i32 1, i32 2000\}',
        llvm,
    )
    assert "call i32 @mule_native_zero_division() cold noinline" in llvm
