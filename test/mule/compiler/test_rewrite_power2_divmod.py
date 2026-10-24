"""Signed bucket calculations must reduce division without truncating negatives."""

import re

import pytest

from mule.compiler import emit_llvm
from mule.compiler.codegen import ModuleGenerator, Program
from mule.compiler.ctype import BYTES_TYPE, IntegerType
from mule.compiler.ir1opt import constant_lvar_value
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case


@pytest.mark.parametrize("divisor", [2, 8, 1024, 1 << 70])
def test_bucket_divmod_positive_power_rewrites_runtime_integer(divisor):
    compilation = compile_source(
        "def bucket(raw):\n"
        " value = int(raw)\n"
        f" return value // {divisor}, value % {divisor}\n"
    )
    bucket = next(fn for fn in compilation.lambdas if fn.source_name == "bucket")
    assert not list(known_calls(bucket, "%floor_divide"))
    assert not list(known_calls(bucket, "%modulo"))
    shifts = list(known_calls(bucket, "%right_shift"))
    masks = list(known_calls(bucket, "%bit_and"))
    assert shifts and masks
    assert constant_lvar_value(shifts[0].args[0]) is None
    assert constant_lvar_value(masks[0].args[0]) is None
    assert constant_lvar_value(shifts[0].args[1]).value == divisor.bit_length() - 1
    assert constant_lvar_value(masks[0].args[1]).value == divisor - 1


@pytest.mark.parametrize("divisor", [0, 3, 6, -8])
def test_bucket_divmod_nonpositive_or_nonpower_keeps_language_operations(divisor):
    compilation = compile_source(
        "def bucket(raw):\n"
        " value = int(raw)\n"
        f" return value // {divisor}, value % {divisor}\n"
    )
    bucket = next(fn for fn in compilation.lambdas if fn.source_name == "bucket")
    assert list(known_calls(bucket, "%floor_divide"))
    assert list(known_calls(bucket, "%modulo"))
    assert not list(known_calls(bucket, "%right_shift"))
    assert not list(known_calls(bucket, "%bit_and"))


@pytest.mark.integration
def test_signed_bucket_native_shift_and_mask_match_python(tmp_path):
    case = build_case(
        tmp_path,
        "import mule\n"
        "def signed_bucket(value: mule.int64) -> mule.int64:\n"
        " return value // 8\n"
        "def bucket_offset(value: mule.int64) -> mule.int64:\n"
        " return value % 8\n"
        "value = int(input())\n"
        "print(signed_bucket(value), bucket_offset(value))\n",
    )
    for value in [-9223372036854775808, -65, -64, -9, -8, -1, 0, 1, 7, 8, 65]:
        case.compare(stdin=f"{value}\n")

    # Inspect the actual callable bodies, including their inlined arithmetic.
    # A selected helper alone must not satisfy this efficiency assertion.
    quotient = case.function_ir(r"mule_fn_signed_bucket\.")
    remainder = case.function_ir(r"mule_fn_bucket_offset\.")
    assert re.search(r"\bashr i64 [^\n]+, (?:3|4)\b", quotient), quotient
    assert re.search(r"\band i64 [^\n]+, (?:7|14)\b", remainder), remainder
    for body in (quotient, remainder):
        assert not re.search(r"\b(?:sdiv|srem|udiv|urem)\b", body), body
        assert not re.search(r"mule\.(?:fast|fixnum)\.binary\.[45]\b", body), body
        assert not re.search(r"@mule_binary(?:_reuse)?\(i32 [45]\b", body), body


@pytest.mark.integration
def test_bucket_divmod_bigints_and_divisor_boundaries_match_python(tmp_path):
    case = build_case(
        tmp_path,
        "def buckets(value):\n"
        " return (value // 8, value % 8, value // 1, value % 1,\n"
        "         value // 6, value % 6, value // -8, value % -8,\n"
        "         value // 1180591620717411303424,\n"
        "         value % 1180591620717411303424)\n"
        "value = int(input())\n"
        "print(buckets(value))\n"
        "try:\n"
        " print(value // 0)\n"
        "except ZeroDivisionError:\n"
        " print('floor zero')\n"
        "try:\n"
        " print(value % 0)\n"
        "except ZeroDivisionError:\n"
        " print('modulo zero')\n",
    )
    for value in [-((1 << 140) + 9), -(1 << 70), -9, -8, -1, 0, 1, 8, (1 << 140) + 9]:
        case.compare(stdin=f"{value}\n")


@pytest.mark.integration
def test_bucket_divmod_evaluation_and_overridden_operators_match_python(tmp_path):
    case = build_case(
        tmp_path,
        "class Coordinate:\n"
        " def __init__(self, value):\n"
        "  self.value = value\n"
        " def __floordiv__(self, divisor):\n"
        "  print('floor', divisor)\n"
        "  return self.value + divisor\n"
        " def __mod__(self, divisor):\n"
        "  print('modulo', divisor)\n"
        "  return self.value - divisor\n"
        "def source(value):\n"
        " print('source', value)\n"
        " return value\n"
        "def bucket(value):\n"
        " return value // 8, value % 8\n"
        "value = int(input())\n"
        "print(bucket(Coordinate(value)))\n"
        "print(source(value) // 8, source(value) % 8)\n"
        "print(source(value) // 1, source(value) % 1)\n"
        "try:\n"
        " print(source(value) // source(0))\n"
        "except ZeroDivisionError:\n"
        " print('zero after operands')\n"
        "print(bucket(True))\n"
        "print(bucket(-9.5))\n",
    )
    case.compare(stdin="-9\n")


def test_inline_fixnum_divmod_unsigned_signed_and_bytes_dispatch():
    compilation = compile_source(
        "def unsigned_divmod(x: int) -> tuple[int, int]:\n"
        " if x >= 0:\n"
        "  if x <= 100000:\n"
        "   return (x // 7, x % 65521)\n"
        " return (0, 0)\n"
        "def signed_divmod(x: int) -> tuple[int, int, int, int]:\n"
        " return (x // 7, x % 7, x // -7, x % -7)\n"
        "def byte_mod(buf, idx: int) -> int:\n"
        " return buf[idx] % 25\n"
        "byte_mod(b'hello', 0)\n"
    )
    assert compilation.facts is not None
    module = ModuleGenerator(
        Program(compilation.lambdas, compilation.entry, compilation.facts)
    )
    by_name = {
        fn.source_name: fn for fn in compilation.lambdas if fn.source_name is not None
    }

    _, unsigned_variants = module.contexts(by_name["unsigned_divmod"])
    assert unsigned_variants
    unsigned_ir = "\n".join(
        module.emit_body(by_name["unsigned_divmod"], ctx) for ctx in unsigned_variants
    )
    assert "udiv i64" in unsigned_ir
    assert "urem i64" in unsigned_ir
    assert "@mule.fixnum.binary.4" not in unsigned_ir
    assert "@mule.fixnum.binary.5" not in unsigned_ir
    assert "mule_format_modulo" not in unsigned_ir

    _, signed_variants = module.contexts(by_name["signed_divmod"])
    assert signed_variants
    signed_ir = "\n".join(
        module.emit_body(by_name["signed_divmod"], ctx) for ctx in signed_variants
    )
    assert "sdiv i64" in signed_ir
    assert "srem i64" in signed_ir
    assert "@mule.fixnum.binary.4" not in signed_ir
    assert "@mule.fixnum.binary.5" not in signed_ir
    assert "mule_format_modulo" not in signed_ir

    _, byte_variants = module.contexts(by_name["byte_mod"])
    assert byte_variants
    assert any(
        ctx.parameters[0] == BYTES_TYPE and ctx.return_type == IntegerType(0, 24)
        for ctx in byte_variants
    )
    module.protocol_helpers["getitem"] = "@mule.protocol.getitem"
    byte_ir = "\n".join(
        module.emit_body(by_name["byte_mod"], ctx) for ctx in byte_variants
    )
    assert "urem i64" in byte_ir
    assert "@mule.fixnum.binary.5" not in byte_ir
    assert "mule_format_modulo" not in byte_ir


@pytest.mark.integration
def test_inline_fixnum_divmod_nonpower2_matches_python(tmp_path):
    case = build_case(
        tmp_path,
        "def bounded_divmod(x: int) -> tuple[int, int, int, int, int, int]:\n"
        " if x >= -1000:\n"
        "  if x <= 1000:\n"
        "   u = x + 1000\n"
        "   return (u // 7, u % 7, x // 7, x % 7, x // -7, x % -7)\n"
        " return (0, 0, 0, 0, 0, 0)\n"
        "for sample in (-1000, -15, -14, -13, -7, -6, -1, 0, 1, 6, 7, 13, 14, 15, 1000):\n"
        " print(bounded_divmod(sample))\n",
    )
    case.compare()
    spec_ir = case.function_ir(r"mule_fn_bounded_divmod\.spec\.")
    assert not re.search(r"mule\.(?:fast|fixnum)\.binary\.[45]\b", spec_ir), spec_ir
    assert not re.search(r"@mule_binary(?:_reuse)?\(i32 [45]\b", spec_ir), spec_ir
    spec_asm = case.function_assembly(r"mule_fn_bounded_divmod\.spec\.")
    assert not re.search(r"\bi?div[qlwb]\b", spec_asm), spec_asm
    assert not re.search(r"mule_binary", spec_asm), spec_asm


def test_bytearray_varint_adler32_chain_emits_inline_urem_without_releases():
    source = (
        "def decode_varint(buf, pos):\n"
        " value = 0\n"
        " shift = 0\n"
        " while True:\n"
        "  b = buf[pos]\n"
        "  pos += 1\n"
        "  value |= (b & 0x7F) << shift\n"
        "  if not (b & 0x80):\n"
        "   return value, pos\n"
        "  shift += 7\n"
        "def run_packet(packet):\n"
        " payload_len, pos = decode_varint(packet, 0)\n"
        " decoded = bytearray()\n"
        " for j in range(payload_len):\n"
        "  decoded.append((packet[pos + j] ^ j) & 0xFF)\n"
        " a = 1\n"
        " b_acc = 0\n"
        " for b in decoded:\n"
        "  a = (a + b) % 65521\n"
        "  b_acc = (b_acc + a) % 65521\n"
        " return (b_acc << 16) | a\n"
        "run_packet(b'\\x03abc')\n"
    )
    ir = emit_llvm(source)
    start = ir.find('define internal i64 @"mule_fn_run_packet.numeric.observed"')
    assert start >= 0
    end = ir.find("\ndefine ", start + 1)
    fn_ir = ir[start:end]
    assert fn_ir.count("urem i64") >= 2
    assert "@mule_format_modulo" not in fn_ir
    assert "@mule.fixnum.binary.5" not in fn_ir
