import subprocess
from pathlib import Path

import pytest

from mule.compiler import ToolchainError, build
from mule.compiler.driver import link_llvm


def test_failed_compilation_preserves_existing_executable(tmp_path):
    source = tmp_path / "bad.py"
    source.write_text("def broken(:\n")
    output = tmp_path / "program"
    output.write_bytes(b"previous artifact")
    with pytest.raises(SyntaxError):
        build(source, output, optimize=False, workers=1)
    assert output.read_bytes() == b"previous artifact"


def test_missing_toolchain_reports_actionable_error(tmp_path):
    source = tmp_path / "program.py"
    source.write_text("print(42)\n")
    with pytest.raises(ToolchainError, match="was not found"):
        build(
            source,
            tmp_path / "program",
            clang="mule-nonexistent-clang",
            optimize=False,
            workers=1,
        )


def test_missing_rust_compiler_preserves_existing_executable(tmp_path):
    source = tmp_path / "program.py"
    source.write_text("print(42)\n")
    output = tmp_path / "program"
    output.write_bytes(b"previous artifact")
    with pytest.raises(ToolchainError, match="Rust compiler.*was not found"):
        build(source, output, rustc="mule-nonexistent-rustc", optimize=False, workers=1)
    assert output.read_bytes() == b"previous artifact"


def test_rust_runtime_build_failure_preserves_existing_executable(tmp_path):
    source = tmp_path / "program.py"
    source.write_text("print(42)\n")
    output = tmp_path / "program"
    output.write_bytes(b"previous artifact")
    with pytest.raises(ToolchainError, match="Rust runtime compilation failed"):
        build(source, output, rustc="/usr/bin/false", optimize=False, workers=1)
    assert output.read_bytes() == b"previous artifact"


@pytest.mark.parametrize("artifact", ["executable", "llvm"])
def test_build_rejects_overwriting_source(tmp_path, artifact):
    source = tmp_path / "program.py"
    source.write_text("print(42)\n")
    options = {"llvm_output": source} if artifact == "llvm" else {}
    output = tmp_path / "program" if artifact == "llvm" else source
    with pytest.raises(ValueError, match="source"):
        build(source, output, **options, optimize=False, workers=1)
    assert source.read_text() == "print(42)\n"


def test_link_failure_reports_error_and_preserves_previous_artifact(tmp_path):
    source = tmp_path / "program.py"
    source.write_text("print(42)\n")
    output = tmp_path / "program"
    output.write_bytes(b"previous artifact")
    # A real failing executable tests subprocess failure handling without a mock.
    with pytest.raises(ToolchainError, match="compilation/linking failed"):
        build(
            source, output, clang=str(Path("/usr/bin/false")), optimize=False, workers=1
        )
    assert output.read_bytes() == b"previous artifact"


@pytest.mark.integration
def test_lto_eliminates_a_call_into_the_real_rust_runtime(tmp_path):
    # Without the Rust body available to LLVM, this call and comparison cannot
    # disappear: a separately compiled declaration could return NULL.
    llvm = """declare ptr @mule_none()
define i32 @main() {
entry:
  %value = call ptr @mule_none()
  %null = icmp eq ptr %value, null
  %status = zext i1 %null to i32
  ret i32 %status
}
"""
    executable = link_llvm(llvm, tmp_path / "lto-probe")
    result = subprocess.run([executable], check=False, capture_output=True, timeout=10)
    assert result.returncode == 0
    assembly = subprocess.run(
        ["objdump", "-d", "--disassemble=main", executable],
        check=True,
        capture_output=True,
        text=True,
    ).stdout
    assert "<main>:" in assembly
    assert "\tcall" not in assembly
    assert "ret" in assembly
