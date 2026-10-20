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


def test_partition_count_bounds_partitions_by_size_and_cores():
    from mule.compiler.driver import PARTITION_BYTES, partition_count

    assert partition_count(PARTITION_BYTES - 1, 16) == 1
    assert partition_count(3 * PARTITION_BYTES, 16) == 3
    assert partition_count(40 * PARTITION_BYTES, 16) == 16
    assert partition_count(40 * PARTITION_BYTES, 1) == 1


@pytest.mark.integration
def test_partitioned_objects_keep_linkonce_and_local_definitions(tmp_path):
    """Units large enough to compile as several objects keep a linkonce
    definition for the partitions that reference it after the partition
    holding it has inlined its own uses, and a local helper that several
    partitions use stays local to its unit, so two units may define it."""
    from mule.compiler.artifacts import Graph, Step
    from mule.compiler.driver import PARTITION_BYTES, Build, Toolchain, lto_linker

    def unit_text(prefix: str, entry: str) -> str:
        padding = []
        index = 0
        while sum(map(len, padding)) < 3 * PARTITION_BYTES:
            padding.append(
                f"define i64 @{prefix}_pad{index}(i64 %value) {{\n"
                f'entry:\n  %shared = call i64 @"mule.test.shared"(i64 %value)\n'
                f'  %local = call i64 @"mule.test.local"(i64 %shared)\n'
                f"  %answer = add i64 %local, {index}\n  ret i64 %answer\n}}\n"
            )
            index += 1
        return (
            'define linkonce_odr i64 @"mule.test.shared"(i64 %value) {\n'
            "entry:\n  %answer = add i64 %value, 1\n  ret i64 %answer\n}\n"
            'define internal i64 @"mule.test.local"(i64 %value) {\n'
            "entry:\n  %answer = add i64 %value, 1\n  ret i64 %answer\n}\n"
            + entry
            + "".join(padding)
        )

    units = {
        "library": unit_text("mule_test_library", ""),
        "__main__": unit_text(
            "mule_test_main",
            "declare i64 @mule_test_library_pad0(i64)\n"
            "define i32 @main() {\n"
            "entry:\n  %value = call i64 @mule_test_library_pad0(i64 40)\n"
            "  %wrong = icmp ne i64 %value, 42\n  %status = zext i1 %wrong to i32\n"
            "  ret i32 %status\n}\n",
        ),
    }
    toolchain = Toolchain(optimize=1)
    objects = []
    with Graph(1) as graph:
        builder = Build(graph, toolchain)
        for name, text in units.items():
            source = graph.text(name + " source", text)

            def write_unit(inputs, directory):
                (directory / "unit.ll").write_text(inputs[0].text)

            unit = graph.step(
                name,
                lambda name=name, source=source: Step(
                    graph,
                    name,
                    inputs=[source],
                    settings={"step": "partition probe"},
                    outputs=("unit.ll",),
                    action=write_unit,
                ),
            )
            object_file = builder.object(unit)
            graph.run(object_file)
            objects.append(str(object_file.path("unit.o")))
    executable = tmp_path / "partitioned-probe"
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
        timeout=60,
    )
    result = subprocess.run([executable], check=False, capture_output=True, timeout=10)
    assert result.returncode == 0, result.stderr
