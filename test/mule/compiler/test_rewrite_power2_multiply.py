"""Power-of-two multiplication through ordinary inference and native packing."""

import re

import pytest

from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1opt import constant_lvar_value
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case


@pytest.mark.parametrize("factor", [2, 8, 256, 1024, 1 << 80])
@pytest.mark.parametrize("constant_first", [False, True])
@pytest.mark.parametrize("local_factor", [False, True])
def test_power2_multiply_normal_inference_retargets_to_shift(
    factor, constant_first, local_factor
):
    operand = "stride" if local_factor else str(factor)
    expression = f"{operand} * value" if constant_first else f"value * {operand}"
    assignment = f" stride = {factor}\n" if local_factor else ""
    compilation = compile_source(
        "import mule\n"
        "def scale(value: mule.int64):\n"
        f"{assignment} return {expression}\n"
    )
    function = next(fn for fn in compilation.lambdas if fn.source_name == "scale")
    assert not list(known_calls(function, "%multiply"))
    shift = list(known_calls(function, "%left_shift"))
    assert len(shift) == 1
    assert constant_lvar_value(shift[0].args[1]).value == factor.bit_length() - 1
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize("factor", [-8, 3, 6, 255, 257, True, 8.0])
@pytest.mark.parametrize("constant_first", [False, True])
def test_non_power2_or_non_integer_factor_keeps_multiplication(factor, constant_first):
    expression = f"{factor!r} * value" if constant_first else f"value * {factor!r}"
    compilation = compile_source(
        f"import mule\ndef scale(value: mule.int64):\n return {expression}\n"
    )
    function = next(fn for fn in compilation.lambdas if fn.source_name == "scale")
    assert list(known_calls(function, "%multiply"))
    assert not list(known_calls(function, "%left_shift"))


def test_runtime_factor_and_nonexact_operand_keep_multiplication():
    compilation = compile_source(
        "import mule\n"
        "def dynamic(value: mule.int64, stride: mule.int64):\n"
        " return value * stride\n"
        "def generic(value):\n"
        " return 8 * value, value * 8\n"
    )
    for name in ("dynamic", "generic"):
        function = next(fn for fn in compilation.lambdas if fn.source_name == name)
        assert list(known_calls(function, "%multiply"))
        assert not list(known_calls(function, "%left_shift"))


PACKING_SOURCE = """\
def pack_record(raw, tail):
    value = int(raw)
    byte = int(tail) & 255
    stride = 256
    packed = value * stride + byte
    scaled = 8 * value
    # The constant local must remain 256 after its use becomes a shift count.
    return packed, scaled, stride, value

raw = input()
tail = input()
print(pack_record(raw, tail))
"""


@pytest.mark.integration
def test_power2_record_packing_uses_real_shift_path_and_preserves_bigints(tmp_path):
    case = build_case(tmp_path, PACKING_SOURCE)
    for value in (
        0,
        7,
        -7,
        (1 << 54) - 1,
        1 << 54,
        -(1 << 54) - 1,
        (1 << 62) - 1,
        -(1 << 62),
        (1 << 63) - 1,
        1 << 90,
        -(1 << 90),
    ):
        case.compare(f"{value}\n319\n")

    body = case.function_ir(r"^mule_fn_pack_record\.")
    # Clang can inline these LLVM helpers. Follow any that remain instead of
    # accepting helper selection as evidence that shifting actually occurs.
    shift_helpers = set(
        re.findall(r'@"?(mule\.(?:fast|fixnum)\.binary\.8[\w.]*)', body)
    )
    path = body
    for helper in shift_helpers:
        path += "\n" + case.function_ir(rf"^{re.escape(helper)}$")
    assert re.search(r"\bshl i64 [^,\n]+, (?:3|8|%[^\s,]+)", path), path
    assert not re.search(r"mule\.(?:fast|fixnum)\.binary\.2(?:\D|$)", path), path
    assert "llvm.smul.with.overflow" not in path
    assert not re.search(r"\bmul(?: nsw| nuw)* i64\b", path), path
    # Overflow and bigint shift fallbacks are required. Multiplication dispatch
    # is not: every remaining binary runtime call on this path must use another opcode.
    assert not re.search(r"@mule_binary(?:_reuse)?\(i32(?: [a-z]+)* 2,", path), path


FALLBACK_SOURCE = """\
def multiply(value):
    return value * 8, 8 * value

class Numeric:
    def __mul__(self, other):
        print('mul', other)
        return 31
    def __rmul__(self, other):
        print('rmul', other)
        return 47
    def __lshift__(self, other):
        raise RuntimeError('multiplication became shift')

class Child(int):
    def __mul__(self, other):
        print('child-mul', other)
        return 53
    def __rmul__(self, other):
        print('child-rmul', other)
        return 59

value = int(input())
print(multiply(Numeric()))
print(multiply(Child(value)))
print(multiply(True))
print(multiply(float(value)))
print(multiply('ab'))
print(multiply(b'xy'))
print(value * -8, 3 * value, value * 8.0)
def observed(label, raw):
    print(label)
    return int(raw)

print(observed('left', value) * 8)
print(8 * observed('right', value))
try:
    print(8 * observed('conversion', 'invalid'))
except ValueError:
    print('conversion-error')
try:
    print(multiply(None))
except TypeError:
    print('type-error')
"""


@pytest.mark.integration
def test_power2_multiply_rejected_operand_protocols_match_python(tmp_path):
    case = build_case(tmp_path, FALLBACK_SOURCE)
    case.compare("-11\n")
