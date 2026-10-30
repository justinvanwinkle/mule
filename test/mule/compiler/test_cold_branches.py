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

HOT_CALLS_AFTER_ATTR_SOURCE = """
class Base:
    def __init__(self, x: int) -> None:
        self.x = x

    def score(self, bias: int) -> int:
        return self.x + bias


class Sub(Base):
    def score(self, bias: int) -> int:
        return super().score(bias) + self.x


def compute(item: Sub, extra: object) -> int:
    val = getattr(extra, "value")
    return item.score(int(val))
"""

DEBUG0_RAISE_AFTER_ATTR_SOURCE = """
from mule_types import optimize

class Holder:
    def __init__(self, x: int) -> None:
        self.x = x


@optimize(debug=0)
def boom() -> int:
    raise RuntimeError("boom")


@optimize(debug=0)
def call_after_attr(h: Holder) -> int:
    v = h.x
    res = boom()
    return v + res


@optimize(debug=0)
def raise_wrapping_failing_call() -> int:
    raise ValueError(boom())
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


def test_attribute_and_super_reads_do_not_leak_cold_block_to_hot_calls() -> None:
    llvm = emit_llvm(HOT_CALLS_AFTER_ATTR_SOURCE)
    assert re.search(
        r"call ptr @mule_get_attr_cached\([^)]+\) cold noinline",
        llvm,
    )
    assert not re.search(
        r'call ptr @"mule\.method\.score\.[^"]+"\([^)]+\) cold noinline',
        llvm,
    )


def test_debug0_full_call_checks_null_after_attr_and_inside_raise() -> None:
    llvm = emit_llvm(DEBUG0_RAISE_AFTER_ATTR_SOURCE)
    call_after_attr_match = re.search(
        r'define internal ptr @"mule_fn_call_after_attr\.spec\.0"\([^)]*\)\s*\{(.*?)\n\}',
        llvm,
        re.DOTALL,
    )
    assert call_after_attr_match is not None
    call_after_attr_ir = call_after_attr_match.group(1)
    assert re.search(
        r'(%t\.\d+) = call ptr @"mule\.direct\.mule_fn_boom\.[^"]+"\([^)]*\)\n'
        r"  call void @mule\.fast\.release\(ptr %t\.\d+\)\n"
        r"  %t\.\d+ = icmp eq ptr \1, null",
        call_after_attr_ir,
    )

    raise_wrapping_match = re.search(
        r'define internal ptr @"mule_fn_raise_wrapping_failing_call\.body"\([^)]*\)\s*\{(.*?)\n\}',
        llvm,
        re.DOTALL,
    )
    assert raise_wrapping_match is not None
    raise_wrapping_ir = raise_wrapping_match.group(1)
    assert re.search(
        r'(%t\.\d+) = call ptr @"mule\.direct\.mule_fn_boom\.[^"]+"\([^)]*\) cold noinline\n'
        r"  call void @mule\.fast\.release\(ptr %t\.\d+\) cold noinline\n"
        r"  %t\.\d+ = icmp eq ptr \1, null",
        raise_wrapping_ir,
    )
