"""Rejected contracts release the unrefined value, not its successful type."""

import re
import subprocess

from mule.compiler import emit_llvm
from mule.compiler.driver import build_runtime
from mule.compiler.vops import _ownership


def test_rejected_integer_contract_releases_owned_heap_argument(tmp_path):
    llvm = emit_llvm(
        "from mule import int32\n"
        "def checked(left: int32, right: int32): return left + right\n"
        "def checked_return(value) -> int32: return value\n"
    )
    bodies = []
    for name in ("checked", "checked_return"):
        body = re.search(
            rf'^define internal ptr @"mule_fn_{name}\.body"\([^\n]*\) \{{\n.*?^\}}',
            llvm,
            re.MULTILINE | re.DOTALL,
        )
        assert body is not None
        # Export the real generated body for a caller that can observe physical
        # reference counts. No compiler-generated instructions are replaced.
        bodies.append(
            body[0].replace(
                f'define internal ptr @"mule_fn_{name}.body"',
                f"define ptr @probe_{name}",
                1,
            )
        )
    declarations = [line for line in llvm.splitlines() if line.startswith("declare ")]
    constants = [
        line
        for line in llvm.splitlines()
        if line.startswith(
            (
                "@mule.data.",
                '@"mule_fn_checked.code" =',
                '@"mule_fn_checked_return.code" =',
            )
        )
        or (line.startswith("@") and " = external " in line)
    ]
    ownership = re.findall(
        r"^define internal (?:ptr|void) @mule\.(?:fast|heap)\.(?:retain|release)\([^\n]*\)[^\n]*\{\n.*?^\}",
        _ownership(),
        re.MULTILINE | re.DOTALL,
    )
    source = tmp_path / "ownership.ll"
    source.write_text("\n".join([*declarations, *constants, *ownership, *bodies]))
    object_file = tmp_path / "ownership.o"
    subprocess.run(
        ["clang", "-O1", "-Wno-override-module", "-c", source, "-o", object_file],
        check=True,
        capture_output=True,
        text=True,
        timeout=30,
    )
    runtime = build_runtime(tmp_path, crate_type="rlib")
    caller = tmp_path / "ownership.rs"
    caller.write_text(RUST_CALLER)
    executable = tmp_path / "ownership"
    subprocess.run(
        [
            "rustc",
            "--edition=2024",
            "-Copt-level=3",
            "-Cpanic=abort",
            "--extern",
            f"mule_runtime={runtime}",
            f"-Clink-arg={object_file}",
            caller,
            "-o",
            executable,
        ],
        check=True,
        capture_output=True,
        text=True,
        timeout=60,
    )
    result = subprocess.run(
        [executable], capture_output=True, text=True, check=False, timeout=10
    )
    assert result.returncode == 0, result.stderr


RUST_CALLER = """
use mule_runtime::object::{self, Data, Value};
use std::ptr::null_mut;
use std::sync::atomic::Ordering;

unsafe extern "C" {
    fn probe_checked(globals: Value, left: Value, right: Value) -> Value;
    fn probe_checked_return(globals: Value, value: Value) -> Value;
}

fn main() {
    unsafe {
        let value = object::new_value(Data::Float(1.5, null_mut(), null_mut()));
        for position in 0..3 {
            let argument = object::retain(value);
            let result = match position {
                0 => probe_checked(object::none_value(), argument, object::small(1)),
                1 => probe_checked(object::none_value(), object::small(1), argument),
                _ => probe_checked_return(object::none_value(), argument),
            };
            assert!(result.is_null());
            assert!(object::has_error());
            object::clear_error();
            assert_eq!((*value).references.load(Ordering::Relaxed), 1,
                       "rejected contract leaked its owned argument at position {position}");
        }
        object::release(value);
    }
}
"""
