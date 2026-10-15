"""Pytest's real compiled entry discovers linked native test functions."""

import subprocess
from pathlib import Path

import pytest

from mule.compiler import build


@pytest.mark.integration
def test_native_pytest_collects_runs_and_finishes_a_small_project(tmp_path):
    (tmp_path / "pyproject.toml").write_text(
        '[tool.mule]\ninclude_modules = ["test_smoke", "test_failure", '
        '"xdist.plugin", "xdist.looponfail"]\n'
    )
    (tmp_path / "test_failure.py").write_text(
        "def test_failure():\n    assert 1 == 2\n"
    )
    (tmp_path / "test_smoke.py").write_text(
        """
def test_arithmetic():
    print("native arithmetic ran")
    assert 6 * 7 == 42

def test_containers(tmp_path):
    print("native containers ran")
    (tmp_path / "sample.txt").write_text("ok")
    assert (tmp_path / "sample.txt").read_text() == "ok"
    assert {"key": [1, 2]}["key"] == [1, 2]
"""
    )
    source = tmp_path / "main.py"
    source.write_text((Path(pytest.__file__).parent / "__main__.py").read_text())
    executable = build(source, tmp_path / "program", optimize=False, workers=1)
    result = subprocess.run(
        [executable, "-n0", "-q", "-s", "test_smoke.py"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        timeout=30,
        check=False,
    )
    assert result.returncode == 0, result.stderr + result.stdout
    assert "native arithmetic ran" in result.stdout
    assert "native containers ran" in result.stdout
    assert "2 passed" in result.stdout
    assert "INTERNALERROR" not in result.stdout
    assert result.stderr == ""
    selected = subprocess.run(
        [
            executable,
            "-n0",
            "-q",
            "-s",
            "-k",
            "arithmetic and not containers",
            "test_smoke.py",
        ],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        timeout=30,
        check=False,
    )
    assert selected.returncode == 0, selected.stderr + selected.stdout
    assert "native arithmetic ran" in selected.stdout
    assert "native containers ran" not in selected.stdout
    assert "1 passed, 1 deselected" in selected.stdout
    assert selected.stderr == ""
    failure = subprocess.run(
        [executable, "-n0", "-q", "--tb=line", "test_failure.py"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        timeout=30,
        check=False,
    )
    assert failure.returncode == 1, failure.stderr + failure.stdout
    assert "test_failure.py:2: AssertionError" in failure.stdout
    assert "1 failed" in failure.stdout
    assert failure.stderr == ""
    (tmp_path / "empty").mkdir()
    empty = subprocess.run(
        [executable, "-n0", "-q", "empty"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        timeout=30,
        check=False,
    )
    assert empty.returncode == 5, empty.stderr + empty.stdout
    assert "no tests ran" in empty.stdout
    assert empty.stderr == ""
