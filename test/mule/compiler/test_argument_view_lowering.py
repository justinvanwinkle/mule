"""Generated argument loads preserve the real runtime's ownership contract."""

import re
import subprocess

from mule.compiler.argument_layout import helpers
from mule.compiler.codegen import DECLARATIONS
from mule.compiler.driver import build_runtime
from mule.compiler.vops import INTRINSICS, _ownership


def test_argument_view_helpers_match_runtime_layout_counts_and_transfers(tmp_path):
    llvm = tmp_path / "argument-view.ll"
    llvm.write_text(
        DECLARATIONS
        + INTRINSICS
        + _ownership()
        + helpers()
        + """
define i1 @view_matches(ptr %args) {
  %result = call i1 @mule.argument_view.is_view(ptr %args)
  ret i1 %result
}
define i32 @view_count(ptr %args, i32 %required, i32 %positional, i32 %rest) {
  %result = call i32 @mule.argument_view.positional(ptr %args, i32 %required, i32 %positional, i32 %rest)
  ret i32 %result
}
define ptr @view_take(ptr %args, i32 %index) {
  %result = call ptr @mule.argument_view.take(ptr %args, i32 %index)
  ret ptr %result
}
define ptr @view_default(ptr %args, i32 %index, i32 %default) {
  %result = call ptr @mule.argument_view.arg_or_default(ptr %args, i32 %index, i32 %default)
  ret ptr %result
}
"""
    )
    object_file = tmp_path / "argument-view.o"
    subprocess.run(
        ["clang", "-O1", "-Wno-override-module", "-c", llvm, "-o", object_file],
        check=True,
        capture_output=True,
        text=True,
        timeout=30,
    )
    optimized = subprocess.run(
        ["clang", "-O1", "-Wno-override-module", "-S", "-emit-llvm", llvm, "-o", "-"],
        check=True,
        capture_output=True,
        text=True,
        timeout=30,
    ).stdout
    count = re.search(
        r"define[^\n]*@view_count\([^\n]*\)[^\n]*\{(.*?)^}",
        optimized,
        re.MULTILINE | re.DOTALL,
    )
    assert count is not None
    assert "load i64" in count[1]
    assert not re.search(r"\bcall\b", count[1])
    take = re.search(
        r"define[^\n]*@view_take\([^\n]*\)[^\n]*\{(.*?)^}",
        optimized,
        re.MULTILINE | re.DOTALL,
    )
    assert take is not None
    assert "@mule_arg(" not in take[1]
    assert re.search(r"\bshl (?:nuw )?i64\b", take[1])

    runtime = build_runtime(tmp_path, crate_type="rlib")
    source = tmp_path / "argument_view_probe.rs"
    source.write_text(RUST_PROBE)
    executable = tmp_path / "argument-view-probe"
    subprocess.run(
        [
            "rustc",
            "--edition=2024",
            "-Copt-level=3",
            "-Cpanic=abort",
            "--extern",
            f"mule_runtime={runtime}",
            "-L",
            f"native={tmp_path}",
            "-Clink-arg=" + str(object_file),
            source,
            "-o",
            executable,
        ],
        check=True,
        capture_output=True,
        text=True,
        timeout=60,
    )
    result = subprocess.run(
        [executable], check=False, capture_output=True, text=True, timeout=10
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "argument view: layout, counts, ownership verified\n"


RUST_PROBE = """
use mule_runtime::object::{self, Data, Object, Value};
use std::cell::Cell;
use std::ptr::null_mut;
use std::sync::atomic::{AtomicUsize, Ordering};

unsafe extern "C" {
    fn view_matches(args: Value) -> bool;
    fn view_count(args: Value, required: i32, positional: i32, rest: i32) -> i32;
    fn view_take(args: Value, index: i32) -> Value;
    fn view_default(args: Value, index: i32, offset: i32) -> Value;
}

fn argument_view(values: &[Value], mask: u64) -> Object {
    Object {
        references: AtomicUsize::new(usize::MAX), tracked: false, gc_slot: 0,
        data: Data::ArgumentView {
            values: values.as_ptr(), count: values.len(), defaults: null_mut(),
            closure: null_mut(), owned_mask: Cell::new(mask),
        },
    }
}

fn main() {
    unsafe {
        assert!(!view_matches(null_mut()));
        assert!(!view_matches(object::small(17)));
        assert!(!view_matches(object::none_value()));
        let immediate = [object::small(3); 65];
        let mut receiver = Object {
            references: AtomicUsize::new(usize::MAX), tracked: false, gc_slot: 0,
            data: Data::ReceiverArgs {
                receiver: object::small(5), values: immediate.as_ptr(), count: 65,
                defaults: null_mut(), closure: null_mut(), owned_mask: Cell::new(0),
            },
        };
        assert!(!view_matches(&mut receiver));
        let captured = [object::small(19)];
        let mut defaults = argument_view(&captured, 0);
        let mut with_default = argument_view(&immediate[..1], 0);
        let Data::ArgumentView { defaults: slot, .. } = &mut with_default.data else { unreachable!() };
        *slot = &mut defaults;
        assert_eq!(view_default(&mut with_default, 0, 0), immediate[0]);
        assert_eq!(view_default(&mut with_default, 1, 0), captured[0]);
        for count in [0, 1, 2, 63, 64, 65] {
            let mut view = argument_view(&immediate[..count], 0);
            assert!(view_matches(&mut view));
            for required in [0, 1, 2, 64] {
                for positional in [required, required + 1, 65] {
                    for rest in [0, 1] {
                        assert_eq!(
                            view_count(&mut view, required, positional, rest),
                            mule_runtime::mule_binding_positional(&mut view, required, positional, rest),
                        );
                    }
                }
            }
        }
        for index in [0, 1, 63, 64] {
            for owned in [false, true] {
                // An index beyond the mask always borrows. Bit zero remains
                // owned to detect accidental masked shifts at index 64.
                let bit = if index < 64 { 1u64 << index } else { 1 };
                for generated in [false, true] {
                    let value = object::new_value(Data::Float(3.5, null_mut(), null_mut()));
                    let values = [value; 65];
                    let mask = if owned { object::retain(value); bit } else { 0 };
                    let mut view = argument_view(&values, mask);
                    let first = if generated { view_take(&mut view, index) }
                                else { object::mule_arg(&mut view, index) };
                    assert_eq!(first, value);
                    let transfers = owned && index < 64;
                    let expected_refs = 1 + usize::from(owned) + usize::from(!transfers);
                    assert_eq!((*value).references.load(Ordering::Relaxed), expected_refs);
                    let Data::ArgumentView { owned_mask, .. } = &view.data else { unreachable!() };
                    assert_eq!(owned_mask.get(), if transfers { 0 } else { mask });
                    let second = if generated { view_take(&mut view, index) }
                                 else { object::mule_arg(&mut view, index) };
                    assert_eq!(second, value);
                    assert_eq!((*value).references.load(Ordering::Relaxed), expected_refs + 1);
                    object::release(first);
                    object::release(second);
                    if owned && !transfers { object::release(value); }
                    assert_eq!((*value).references.load(Ordering::Relaxed), 1);
                    object::release(value);
                }
            }
        }
    }
    println!("argument view: layout, counts, ownership verified");
}
"""
