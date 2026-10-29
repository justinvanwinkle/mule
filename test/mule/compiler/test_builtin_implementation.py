"""Builtin implementation policies suppress only Python traceback additions."""

import os
import subprocess

import pytest

from mule.compiler import build, emit_llvm
from test.python_host import python_host


@pytest.mark.parametrize("marker", (True, False))
def test_builtin_marker_controls_traceback_entries_preserving_native_frames(marker):
    llvm = emit_llvm(
        "__mule_builtin_implementation__ = "
        + str(marker)
        + "\ndef fail():\n raise ValueError('problem')\nfail()\n",
        "implementation.py",
    )
    assert ("call void @mule_traceback_here" in llvm) is (not marker)
    assert "store atomic ptr %frame, ptr %frame.slot release" in llvm


@pytest.mark.parametrize("value", ("1", "None", "'yes'", "bool(1)"))
def test_builtin_marker_requires_a_literal_boolean(value):
    with pytest.raises(SyntaxError, match="markers must be boolean literals") as caught:
        emit_llvm("__mule_builtin_implementation__ = " + value + "\n", "marker.py")
    assert caught.value.filename == "marker.py"
    assert caught.value.lineno == 1


def test_builtin_marker_obeys_module_prologue_and_duplicate_validation():
    with pytest.raises(SyntaxError, match="precede the module's code"):
        emit_llvm("value = 1\n__mule_builtin_implementation__ = True\n")
    with pytest.raises(SyntaxError, match="duplicate module policy"):
        emit_llvm(
            "__mule_builtin_implementation__ = True\n__mule_builtin_implementation__ = False\n"
        )


def test_builtin_implementation_keeps_user_callback_tracebacks(tmp_path):
    implementation = tmp_path / "implementation.py"
    implementation.write_text("from operator import call as invoke\n")
    source = tmp_path / "program.py"
    source.write_text("""from implementation import invoke

def callback():
    raise ValueError('callback failed')

invoke(callback)
""")
    environment = {**os.environ, "PYTHON_COLORS": "0"}
    oracle = subprocess.run(
        [python_host(), source],
        env=environment,
        capture_output=True,
        check=False,
        timeout=10,
    )
    implementation.write_text("""__mule_builtin_implementation__ = True

def invoke(function):
    return function()
""")
    executable = build(source, tmp_path / "program", optimize=False, workers=1)
    native = subprocess.run(
        [executable], env=environment, capture_output=True, check=False, timeout=10
    )
    assert native.returncode == oracle.returncode == 1
    assert native.stdout == oracle.stdout
    assert native.stderr == oracle.stderr


def test_debug_levels_control_tracebacks_and_frames():
    llvm_debug0 = emit_llvm(
        "__debug_level__ = 0\n"
        "def leaf(x: int) -> int:\n"
        "    return x + 1\n"
        "def caller(n: int) -> int:\n"
        "    return leaf(n)\n"
        "raise ValueError('problem')\n",
        "ordinary0.py",
    )
    assert "call void @mule_traceback_here" not in llvm_debug0
    assert "call ptr @mule_frame_head_slot()" not in llvm_debug0
    assert 'define internal ptr @"mule.direct.mule_fn_leaf' in llvm_debug0
    leaf_helper = llvm_debug0.split(
        'define internal ptr @"mule.direct.mule_fn_leaf', 1
    )[1].split("}", 1)[0]
    assert "call i32 @mule_enter_frame()" not in leaf_helper

    llvm_debug1 = emit_llvm(
        "__debug_level__ = 1\n"
        "def leaf(x: int) -> int:\n"
        "    return x + 1\n"
        "def caller(n: int) -> int:\n"
        "    return leaf(n)\n"
        "raise ValueError('problem')\n",
        "ordinary1.py",
    )
    assert "call void @mule_traceback_here" in llvm_debug1
    leaf_helper1 = llvm_debug1.split(
        'define internal ptr @"mule.direct.mule_fn_leaf', 1
    )[1].split("}", 1)[0]
    assert "call i32 @mule_enter_frame()" not in leaf_helper1

    llvm_debug2 = emit_llvm(
        "__debug_level__ = 2\n"
        "def leaf(x: int) -> int:\n"
        "    return x + 1\n"
        "def caller(n: int) -> int:\n"
        "    return leaf(n)\n"
        "raise ValueError('problem')\n",
        "ordinary2.py",
    )
    assert "call void @mule_traceback_here" in llvm_debug2
    assert "call ptr @mule_frame_head_slot()" in llvm_debug2
    assert "call i32 @mule_enter_frame()" in llvm_debug2

    llvm_rec_default = emit_llvm(
        "def fib(n: int) -> int:\n"
        "    if n < 2:\n"
        "        return n\n"
        "    return fib(n - 1) + fib(n - 2)\n",
        "rec_default.py",
    )
    fib_helper = llvm_rec_default.split(
        'define internal ptr @"mule.direct.mule_fn_fib', 1
    )[1].split("}", 1)[0]
    assert "call i32 @mule_enter_frame()" in fib_helper
    assert "call ptr @mule_frame_head_slot()" not in llvm_rec_default

    llvm_rec_safety2 = emit_llvm(
        "__safety__ = 2\n"
        "def fib(n: int) -> int:\n"
        "    if n < 2:\n"
        "        return n\n"
        "    return fib(n - 1) + fib(n - 2)\n",
        "rec_safety2.py",
    )
    fib_helper2 = llvm_rec_safety2.split(
        'define internal ptr @"mule.direct.mule_fn_fib', 1
    )[1].split("}", 1)[0]
    assert "call i32 @mule_enter_frame()" in fib_helper2
    assert "call ptr @mule_frame_head_slot()" not in llvm_rec_safety2


def test_policy_qualities_self_calls_and_safepoints():
    llvm_self_call_default = emit_llvm(
        "def countdown(n: int) -> int:\n"
        "    if n <= 0:\n"
        "        return 0\n"
        "    return countdown(n - 1)\n",
        "self_default.py",
    )
    default_direct = llvm_self_call_default.split(
        'define internal i64 @"mule.numeric.mule_fn_countdown', 1
    )[1].split("}", 1)[0]
    assert "@mule_method_entry" in default_direct

    llvm_self_call_speed2 = emit_llvm(
        "from mule_types import optimize\n"
        "@optimize(speed=2)\n"
        "def countdown(n: int) -> int:\n"
        "    if n <= 0:\n"
        "        return 0\n"
        "    return countdown(n - 1)\n",
        "self_speed2.py",
    )
    assert (
        'define internal i64 @"mule.numeric.mule_fn_countdown'
        not in llvm_self_call_speed2
    )
    assert (
        'define internal ptr @"mule.direct.mule_fn_countdown'
        not in llvm_self_call_speed2
    )
    speed2_fix = llvm_self_call_speed2.split(
        'define internal i64 @"mule_fn_countdown.numeric.fixnum"', 1
    )[1].split("\n}", 1)[0]
    assert "@mule_method_entry" not in speed2_fix
    assert 'call i64 @"mule_fn_countdown.numeric.fixnum"' in speed2_fix

    llvm_safety0 = emit_llvm(
        "__safety__ = 0\n"
        "def spin(n: int) -> int:\n"
        "    s = 0\n"
        "    while s < n:\n"
        "        s += 1\n"
        "    return s\n",
        "safety0.py",
    )
    spin_body0 = llvm_safety0.split('define internal ptr @"mule_fn_spin.body"', 1)[
        1
    ].split("}", 1)[0]
    assert "@mule_safepoint_requested" not in spin_body0

    llvm_safety1 = emit_llvm(
        "__safety__ = 1\n"
        "def spin(n: int) -> int:\n"
        "    s = 0\n"
        "    while s < n:\n"
        "        s += 1\n"
        "    return s\n",
        "safety1.py",
    )
    spin_body1 = llvm_safety1.split('define internal ptr @"mule_fn_spin.body"', 1)[
        1
    ].split("}", 1)[0]
    assert "@mule_safepoint_requested" in spin_body1
