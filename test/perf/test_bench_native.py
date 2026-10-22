from pathlib import Path

import pytest

from test.python_host import python_host
from tools.bench_native import benchmark


def test_reused_executable_reports_mule_time_divided_by_comparison_time(tmp_path):
    # Reused executables need no compiler invocation. Execute the same workload
    # through Python to check reporting independently of native compilation.
    source = (
        Path(__file__).resolve().parents[2] / "examples/performance/integer_loop.py"
    )
    program = (
        f"#!{python_host()}\n"
        "import runpy\n"
        f"runpy.run_path({str(source)!r}, run_name='__main__')\n"
    )
    for directory in (tmp_path / "current", tmp_path / "baseline"):
        directory.mkdir()
        executable = directory / "integer_loop"
        executable.write_text(program)
        executable.chmod(0o755)
    result = benchmark(
        "integer_loop",
        10,
        tmp_path / "current",
        3,
        10,
        False,
        baseline_directory=tmp_path / "baseline",
    )
    medians = result["medians_seconds"]
    assert result["time_ratio"] == pytest.approx(medians["mule"] / medians["python"])
    assert result["time_ratio_to_baseline"] == pytest.approx(
        medians["mule"] / medians["baseline"]
    )
