"""Pytest's real compiled entry discovers linked native test functions."""

import subprocess
from pathlib import Path

import pytest

from mule.compiler import build


@pytest.mark.integration
def test_native_pytest_collects_runs_and_finishes_a_small_project(tmp_path):
    (tmp_path / "pyproject.toml").write_text(
        '[tool.mule]\ninclude_modules = ["test_smoke", "xdist.plugin", "xdist.looponfail"]\n'
    )
    (tmp_path / "test_smoke.py").write_text(
        """
def test_arithmetic():
    print("native arithmetic ran")
    assert 6 * 7 == 42

def test_containers():
    print("native containers ran")
    assert {"key": [1, 2]}["key"] == [1, 2]
"""
    )
    source = tmp_path / "main.py"
    source.write_text((Path(pytest.__file__).parent / "__main__.py").read_text())
    executable = build(source, tmp_path / "program", optimize=False, workers=1)
    result = subprocess.run(
        [executable, "-n0", "-q", "-s"],
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
