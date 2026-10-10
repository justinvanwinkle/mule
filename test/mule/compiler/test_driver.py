from pathlib import Path

import pytest

from mule.compiler import ToolchainError, UnsupportedSyntax, build


def test_failed_compilation_preserves_existing_executable(tmp_path):
    source = tmp_path / "bad.py"
    source.write_text("import missing\n")
    output = tmp_path / "program"
    output.write_bytes(b"previous artifact")
    with pytest.raises(UnsupportedSyntax):
        build(source, output)
    assert output.read_bytes() == b"previous artifact"


def test_missing_toolchain_reports_actionable_error(tmp_path):
    source = tmp_path / "program.py"
    source.write_text("print(42)\n")
    with pytest.raises(ToolchainError, match="was not found"):
        build(source, tmp_path / "program", clang="mule-nonexistent-clang")


def test_missing_rust_compiler_preserves_existing_executable(tmp_path):
    source = tmp_path / "program.py"
    source.write_text("print(42)\n")
    output = tmp_path / "program"
    output.write_bytes(b"previous artifact")
    with pytest.raises(ToolchainError, match="Rust compiler.*was not found"):
        build(source, output, rustc="mule-nonexistent-rustc")
    assert output.read_bytes() == b"previous artifact"


def test_rust_runtime_build_failure_preserves_existing_executable(tmp_path):
    source = tmp_path / "program.py"
    source.write_text("print(42)\n")
    output = tmp_path / "program"
    output.write_bytes(b"previous artifact")
    with pytest.raises(ToolchainError, match="Rust runtime compilation failed"):
        build(source, output, rustc="/usr/bin/false")
    assert output.read_bytes() == b"previous artifact"


@pytest.mark.parametrize("artifact", ["executable", "llvm"])
def test_build_rejects_overwriting_source(tmp_path, artifact):
    source = tmp_path / "program.py"
    source.write_text("print(42)\n")
    options = {"llvm_output": source} if artifact == "llvm" else {}
    output = tmp_path / "program" if artifact == "llvm" else source
    with pytest.raises(ValueError, match="source"):
        build(source, output, **options)
    assert source.read_text() == "print(42)\n"


def test_link_failure_reports_error_and_preserves_previous_artifact(tmp_path):
    source = tmp_path / "program.py"
    source.write_text("print(42)\n")
    output = tmp_path / "program"
    output.write_bytes(b"previous artifact")
    # A real failing executable tests subprocess failure handling without a mock.
    with pytest.raises(ToolchainError, match="compilation/linking failed"):
        build(source, output, clang=str(Path("/usr/bin/false")))
    assert output.read_bytes() == b"previous artifact"
