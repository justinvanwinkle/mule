"""Exercise Mule's Rust runtime through a separately compiled Rust probe."""

import shutil
import subprocess
from pathlib import Path

import pytest

from mule.compiler.driver import build_runtime


def test_runtime_internal_invariants(tmp_path):
    rustc = shutil.which("rustc")
    assert rustc is not None, "native runtime tests require rustc"
    source = Path(__file__).resolve().parents[3] / "mule/runtime/lib.rs"
    executable = tmp_path / "runtime-invariants"
    compiled = subprocess.run(
        [rustc, "--edition=2024", "--test", "-Copt-level=3", source, "-o", executable],
        check=False,
        capture_output=True,
        text=True,
        timeout=90,
    )
    assert compiled.returncode == 0, compiled.stderr
    result = subprocess.run(
        [executable], check=False, capture_output=True, text=True, timeout=30
    )
    assert result.returncode == 0, result.stderr + result.stdout
    assert "test result: ok." in result.stdout
    assert "; 0 failed;" in result.stdout


@pytest.fixture(scope="module")
def runtime_probe(tmp_path_factory):
    rustc = shutil.which("rustc")
    assert rustc is not None, "native runtime tests require rustc"
    directory = tmp_path_factory.mktemp("runtime-probe")
    runtime = build_runtime(directory, rustc=rustc, crate_type="rlib")
    source = Path(__file__).with_name("runtime_probe.rs")
    executable = directory / "probe"
    command = [
        rustc,
        "--edition=2024",
        "-Copt-level=3",
        "-Cpanic=abort",
        "--extern",
        f"mule_runtime={runtime}",
        "-L",
        f"native={directory}",
        str(source),
        "-o",
        str(executable),
    ]
    result = subprocess.run(command, check=False, capture_output=True, text=True)
    assert result.returncode == 0, result.stderr
    return executable


@pytest.mark.parametrize(
    ("scenario", "output"),
    [
        (
            "literals",
            "-123456789012345678901234567890\n1.25\n'snow ☃\\x00end'\nb'a\\x00b'\nNone\nFalse\nTrue\n",
        ),
        ("binary", "9\n5\n14\n3.5\n3\n1\n49\n28\n1\n2\n5\n7\n"),
        ("huge_int", "True\nTrue\n"),
        ("unary", "7\n-7\n-8\nFalse\n"),
        (
            "compare",
            "True\nFalse\nFalse\nTrue\nFalse\nTrue\nFalse\nTrue\nTrue\nFalse\n",
        ),
        ("inplace", "same=1\n[5]\n"),
        ("globals", "42\ncallable=1\nNone\n'__main__'\n"),
        ("function", "42\n"),
        ("function_metadata", "definitions=10000\n'first'\n'second'\n42\n42\n4\n"),
        ("builtin_call", "15\n"),
        ("truth", "0 1\n"),
        ("native_safepoint_handshake", "native safepoint=ok\n"),
        ("shared_heap_registry", "shared heap=ok\n"),
        ("class_ownership", "classes=ok\n"),
        ("dynamic_tagged_attributes", "dynamic=ok\n"),
        ("tuple_slots", "'owned child'\n"),
        ("owned_tuple", "'transferred child'\n"),
    ],
)
def test_runtime_values_and_direct_native_calls(runtime_probe, scenario, output):
    result = subprocess.run(
        [runtime_probe, scenario], check=False, capture_output=True, text=True
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == output
    assert result.stderr == ""


@pytest.mark.parametrize(
    ("scenario", "error"),
    [
        ("zero_division", "ZeroDivisionError"),
        ("name_error", "NameError: name 'absent_name' is not defined"),
        ("local_error", "UnboundLocalError"),
        ("not_callable", "TypeError"),
        ("arity", "TypeError"),
        ("release_error", "ValueError: original failure"),
        ("null_without_error", "SystemError"),
        ("recursion", "RecursionError"),
    ],
)
def test_runtime_errors_exit_nonzero(runtime_probe, scenario, error):
    result = subprocess.run(
        [runtime_probe, scenario], check=False, capture_output=True, text=True
    )
    assert result.returncode != 0
    assert error in result.stderr
    assert "cleanup failure" not in result.stderr
