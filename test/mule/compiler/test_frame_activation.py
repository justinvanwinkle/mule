"""Physical activation records survive optimized native-stack migration."""

import re
import subprocess
from pathlib import Path

import pytest


@pytest.mark.integration
def test_native_frame_migration_preserves_tls_under_lto(tmp_path):
    source = Path(__file__).resolve().parents[3] / "mule/runtime/lib.rs"
    executable = tmp_path / "frame-migration-lto"
    compiled = subprocess.run(
        [
            "rustc",
            "--edition=2024",
            "--test",
            "-Copt-level=3",
            "-Clto=fat",
            "-Ccodegen-units=1",
            source,
            "-o",
            executable,
        ],
        capture_output=True,
        text=True,
        check=False,
        timeout=90,
    )
    assert compiled.returncode == 0, compiled.stderr
    migrated = subprocess.run(
        [
            executable,
            "coroutine::tests::suspended_native_stack_resumes_with_new_thread_head_and_parent",
            "--exact",
        ],
        capture_output=True,
        text=True,
        check=False,
        timeout=30,
    )
    assert migrated.returncode == 0, migrated.stdout + migrated.stderr
    assert "1 passed; 0 failed" in migrated.stdout
    # Inspect the linked executable: intermediate Rust object code can still
    # contain a TLS resolver call that the executable linker must eliminate.
    assembly = subprocess.run(
        ["objdump", "-d", "--disassemble=mule_frame_head_slot", executable],
        capture_output=True,
        text=True,
        check=True,
        timeout=10,
    ).stdout
    assert "<mule_frame_head_slot>:" in assembly
    assert "%fs:" in assembly
    assert not re.search(r"\bcallq?\b", assembly)
