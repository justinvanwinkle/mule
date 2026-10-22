"""Native GMP constants preserve numeric values and literal ownership."""

import subprocess

from mule.compiler.driver import build_runtime
from test.mule.compiler.test_native import run_native


def test_compiled_integer_constants_preserve_values_identity_and_immutable_reuse(
    tmp_path,
):
    values = [
        -(1 << 127),
        (1 << 127) - 1,
        (1 << 128) - 1,
        -(1 << 64) - 1,
        1 << 63,
        (1 << 1024) + (1 << 512) + 17,
        -((1 << 4096) + 37),
    ]
    literals = ", ".join(hex(value) for value in values)
    source = f"""def values():
    return [{literals}]
first = values()
second = values()
for left, right in zip(first, second):
    print(left, left == right)
    changed = left
    changed += 1
    print(changed - left, left == right)
def positive():
    return {hex(1 << 4096)}
print(positive() is positive())
"""
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "".join(f"{value} True\n1 True\n" for value in values) + "True\n"


def test_native_readonly_mpz_constants_match_gmp_layout_and_clone_ownership(tmp_path):
    runtime = build_runtime(tmp_path, crate_type="rlib")
    source = tmp_path / "integer_constant.rs"
    source.write_text(
        """use mule_runtime::{gmp::BigInt, numeric, object};
use std::cmp::Ordering;
use std::ptr::null_mut;

#[repr(C)]
struct NativeInteger { allocated: i32, size: i32, limbs: *const u64 }

fn main() {
    let limbs = [17u64, 0, 1];
    let mut expected = BigInt::from_u64(1).shifted_left(128);
    expected = expected.add(&BigInt::from_u64(17));
    for sign in [-1, 1] {
        let native = NativeInteger { allocated: 0, size: sign * 3, limbs: limbs.as_ptr() };
        let borrowed = &native as *const NativeInteger as *const BigInt;
        unsafe {
            let boxed = numeric::mule_literal_integer(borrowed);
            assert!(!boxed.is_null());
            let actual = numeric::integer_export(boxed).unwrap();
            let signed = if sign < 0 { expected.neg() } else { expected.clone() };
            assert_eq!(actual.compare(&signed), Ordering::Equal);
            assert_eq!(actual.compare_i64(i64::MIN), if sign < 0 { Ordering::Less } else { Ordering::Greater });
            assert_eq!(actual.compare_i64(i64::MAX), if sign < 0 { Ordering::Less } else { Ordering::Greater });
            object::release(boxed);
            assert_eq!(native.allocated, 0);
            assert_eq!(native.size, sign * 3);
            assert_eq!(limbs, [17, 0, 1]);
        }
    }
    for number in [i64::MIN, -1, 0, 1, i64::MAX] {
        let integer = BigInt::from_i64(number);
        assert_eq!(integer.compare_i64(number), Ordering::Equal);
        unsafe {
            let boxed = numeric::mule_literal_integer(&integer);
            assert!(!boxed.is_null());
            object::release(boxed);
        }
    }
    unsafe {
        assert!(numeric::mule_literal_integer(null_mut()).is_null());
        assert_eq!(object::error_kind(), "RuntimeError");
        object::clear_error();
    }
}
"""
    )
    executable = tmp_path / "integer-constant"
    compiled = subprocess.run(
        [
            "rustc",
            "--edition=2024",
            "-Copt-level=3",
            "-Cpanic=abort",
            "--extern",
            f"mule_runtime={runtime}",
            source,
            "-o",
            executable,
        ],
        capture_output=True,
        text=True,
        check=False,
        timeout=60,
    )
    assert compiled.returncode == 0, compiled.stderr
    result = subprocess.run(
        [executable], capture_output=True, text=True, check=False, timeout=10
    )
    assert result.returncode == 0, result.stderr
