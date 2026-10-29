import subprocess
from pathlib import Path

import pytest

from mule.compiler import ToolchainError, build
from mule.compiler.driver import Toolchain, link_llvm


@pytest.mark.parametrize("arch", [None, "native", "x86-64-v3"])
@pytest.mark.parametrize("lto", [False, True])
def test_architecture_is_forwarded_to_clang_compile_and_link(arch, lto):
    toolchain = Toolchain(optimize=3, lto=lto, arch=arch)
    for flags in (toolchain.compile_flags(), toolchain.link_flags()):
        architecture_flags = [flag for flag in flags if flag.startswith("-march=")]
        assert architecture_flags == ([] if arch is None else [f"-march={arch}"])


def test_architecture_changes_clang_artifact_cache_identity():
    from mule.compiler.artifacts import artifact_key

    keys = {
        artifact_key(Toolchain(arch=arch).clang_settings(), [])
        for arch in (None, "native", "x86-64-v3")
    }
    assert len(keys) == 3


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
    executable = link_llvm(llvm, tmp_path / "lto-probe", optimize=3, lto=True)
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


@pytest.mark.integration
@pytest.mark.parametrize("optimize", (0, 1, 2, 3))
@pytest.mark.parametrize("lto", (False, True))
def test_lto_exposes_imported_unit_bodies_to_the_native_optimizer(
    tmp_path, optimize, lto
):
    from mule.compiler.artifacts import Graph, Step
    from mule.compiler.driver import Build, Toolchain, lto_linker

    library = """define i64 @mule_test_increment(i64 %value) {
entry:
  %answer = add i64 %value, 2
  ret i64 %answer
}
"""
    main = """declare i64 @mule_test_increment(i64)
define i32 @main() {
entry:
  %value = call i64 @mule_test_increment(i64 40)
  %wrong = icmp ne i64 %value, 42
  %status = zext i1 %wrong to i32
  ret i32 %status
}
"""
    toolchain = Toolchain(optimize=optimize, lto=lto)
    with Graph(1) as graph:
        builder = Build(graph, toolchain)
        objects = []
        for name, llvm in (("library", library), ("__main__", main)):
            source = graph.text(name + " source", llvm)

            def write_unit(inputs, directory):
                (directory / "unit.ll").write_text(inputs[0].text)

            unit = graph.step(
                name,
                lambda name=name, source=source: Step(
                    graph,
                    name,
                    inputs=[source],
                    settings={"step": "lto probe"},
                    outputs=("unit.ll",),
                    action=write_unit,
                ),
            )
            object_file = builder.object(unit)
            graph.run(object_file)
            assert (
                object_file.path("unit.o")
                .read_bytes()
                .startswith(b"BC" if lto else b"\x7fELF")
            )
            objects.append(str(object_file.path("unit.o")))
    executable = tmp_path / "cross-unit-probe"
    subprocess.run(
        [
            toolchain.clang,
            *toolchain.link_flags(),
            f"--ld-path={lto_linker(toolchain.rustc)}",
            *objects,
            "-o",
            str(executable),
        ],
        check=True,
        capture_output=True,
        timeout=30,
    )
    result = subprocess.run([executable], check=False, capture_output=True, timeout=10)
    assert result.returncode == 0, result.stderr
    assembly = subprocess.run(
        ["objdump", "-d", "--disassemble=main", executable],
        check=True,
        capture_output=True,
        text=True,
    ).stdout
    assert "<main>:" in assembly
    if optimize >= 2 and lto:
        assert "mule_test_increment" not in assembly
        assert "\tcall" not in assembly
    elif not lto or optimize == 0:
        assert "mule_test_increment" in assembly
