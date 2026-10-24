"""Scalar coercions expose representation while retaining checked boundaries."""

import subprocess

import pytest

from mule.compiler import emit_llvm
from mule.compiler.codegen import DECLARATIONS
from mule.compiler.driver import Toolchain, link_llvm
from mule.compiler.vop import INT64_BOX_LAYOUT
from test.mule.compiler.test_llvm_boxing import function_body
from test.mule.compiler.test_native import run_native


def integer_roundtrip(*, ordinary_release=False, assumptions=""):
    layout = INT64_BOX_LAYOUT
    release = "mule_release" if ordinary_release else layout.symbol("private.release")
    return (
        DECLARATIONS
        + "\n"
        + layout.codec.helpers(layout)
        + f"""
declare i64 @strtoll(ptr, ptr, i32)
define i64 @roundtrip(i64 %input) noinline {{
entry:
{assumptions}
  %box = call ptr @{layout.symbol()}(i64 %input)
  %value = call i64 @{layout.symbol("load")}(ptr %box)
  call void @{release}(ptr %box)
  ret i64 %value
}}
define i32 @main(i32 %argc, ptr %argv) {{
entry:
  %slot = getelementptr ptr, ptr %argv, i64 1
  %text = load ptr, ptr %slot
  %input = call i64 @strtoll(ptr %text, ptr null, i32 10)
  %actual = call i64 @roundtrip(i64 %input)
  %wrong = icmp ne i64 %actual, %input
  %status = zext i1 %wrong to i32
  ret i32 %status
}}
"""
    )


@pytest.mark.parametrize("ordinary_release", [False, True])
def test_full_width_integer_boxes_use_normal_gmp_and_private_cleanup(
    tmp_path, ordinary_release
):
    executable = link_llvm(
        integer_roundtrip(ordinary_release=ordinary_release),
        tmp_path / "integer-roundtrip",
        optimize=False,
        lto=False,
    )
    for value in (
        -(2**63),
        -(2**62) - 1,
        -(2**62),
        -17,
        0,
        2**62 - 1,
        2**62,
        2**63 - 1,
    ):
        result = subprocess.run(
            [executable, str(value)],
            check=False,
            capture_output=True,
            text=True,
            timeout=10,
        )
        assert result.returncode == 0, (value, result.stderr)


@pytest.mark.integration
@pytest.mark.parametrize("partition", ["fixnum", "positive-wide", "negative-wide"])
def test_range_proven_integer_roundtrip_eliminates_box_and_limb_storage(
    tmp_path, partition
):
    predicates = {
        "fixnum": """
  %lower = icmp sge i64 %input, -4611686018427387904
  %upper = icmp sle i64 %input, 4611686018427387903
  %range = and i1 %lower, %upper
""",
        "positive-wide": "  %range = icmp sgt i64 %input, 4611686018427387903\n",
        "negative-wide": "  %range = icmp slt i64 %input, -4611686018427387904\n",
    }
    assumptions = (
        predicates[partition]
        + """
  call void @llvm.assume(i1 %range)
  %limb.bits = load i32, ptr @__gmp_bits_per_limb, !invariant.load !{}
  %abi = icmp eq i32 %limb.bits, 64
  call void @llvm.assume(i1 %abi)
"""
    )
    source = tmp_path / "integer.ll"
    source.write_text(integer_roundtrip(assumptions=assumptions))
    optimized = tmp_path / "integer.opt.ll"
    result = subprocess.run(
        [
            Toolchain().llvm_tool("opt"),
            "-passes=default<O3>",
            "-S",
            source,
            "-o",
            optimized,
        ],
        check=False,
        capture_output=True,
        text=True,
        timeout=30,
    )
    assert result.returncode == 0, result.stderr
    body = function_body(optimized.read_text(), "roundtrip")
    assert "ret i64 %input" in body
    assert "@mule_allocate_" not in body
    assert "@mule_deallocate_" not in body


def test_wide_integer_forwarding_keeps_original_object_identity(tmp_path):
    result = run_native(
        tmp_path,
        "from mule_types import int64, optimize\n"
        "@optimize(speed=3, safety=3)\n"
        "def identity(value: int64) -> int64:\n return value\n"
        "for text in ('9223372036854775807', '-9223372036854775808'):\n"
        " value = int(text)\n print(identity(value) is value, identity(value))\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "True 9223372036854775807\nTrue -9223372036854775808\n"


def test_fresh_integer_factory_results_use_managed_private_lifetimes(tmp_path):
    source = """
from mule_types import int64, optimize
def check(text):
    @optimize(speed=3, safety=3)
    def make(value: int64) -> int64:
        return value + 1
    value = int(text)
    first = make(value)
    second = make(value)
    return first is second
for text in ('4611686018427387904', '-4611686018427387906',
             '-9223372036854775808', '9223372036854775806'):
    print(check(text))
"""
    llvm = emit_llvm(source)
    body = function_body(llvm, '"mule_fn_check.body"')
    assert f"@{INT64_BOX_LAYOUT.symbol('private.release')}(" in body
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "False\n" * 4


def test_checked_native_integer_store_rejects_non_integer_and_overflow(tmp_path):
    result = run_native(
        tmp_path,
        "from mule_types import int64, optimize\n"
        "@optimize(speed=3, safety=3)\n"
        "class Box:\n value: int64\n def __init__(self):\n  self.value = 42\n"
        "box = Box()\n"
        "for value in (bool(1), float('1.5'), int('1267650600228229401496703205376')):\n"
        " try:\n  box.value = value\n except TypeError:\n  print('rejected', box.value)\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "rejected 42\n" * 3


def test_integer_subclass_identity_and_native_storage_value_are_preserved(tmp_path):
    result = run_native(
        tmp_path,
        "from mule_types import int64, optimize\n"
        "class Number(int):\n pass\n"
        "@optimize(speed=3, safety=3)\n"
        "def identity(value: int64) -> int64:\n return value\n"
        "@optimize(speed=3, safety=3)\n"
        "class Box:\n value: int64\n def __init__(self, value: int64):\n  self.value = value\n"
        "for text in ('7', '9223372036854775807'):\n"
        " value = Number(int(text))\n"
        " print(identity(value) is value, Box(value).value == value)\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "True True\nTrue True\n"


def test_boolean_none_and_object_reference_coercions_preserve_identity(tmp_path):
    source = """
from mule_types import int64, optimize
@optimize(speed=3, safety=3)
def less(value: int64):
    return value < 3
@optimize(speed=3, safety=3)
def empty():
    return None
def identity(value):
    return value
print(less(2) is True, less(4) is False, empty() is None)
for value in ('a string', [1, 2], {'a': 1}):
    print(identity(value) is value)
"""
    llvm = emit_llvm(source)
    assert "select i1" in function_body(llvm, '"mule_fn_less.body"')
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "True True True\nTrue\nTrue\nTrue\n"
