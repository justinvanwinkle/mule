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


def test_debug_zero_retains_ordinary_source_tracebacks():
    llvm = emit_llvm(
        "__debug_level__ = 0\nraise ValueError('problem')\n", "ordinary.py"
    )
    assert "call void @mule_traceback_here" in llvm
