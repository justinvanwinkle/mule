"""Tests for the vendored pyperformance benchmark suite and harness."""

import hashlib
import json
import subprocess
from pathlib import Path

import pytest

from perf.pyperformance import pyperf
from perf.pyperformance.__main__ import (
    BENCHMARKS_DIR,
    HERE,
    discover_benchmarks,
    format_table,
    load_runtimes,
    parse_bench_output,
    resolve_case_name,
)
from test.python_host import python_host

ROOT = Path(__file__).resolve().parents[2]


def test_comparison_table_uses_candidate_time_over_baseline_time():
    cases = ["bm_fast", "bm_slow"]
    rows = {
        case: [
            {
                "runtime": runtime,
                "status": "ok",
                "summary": {"kernel_seconds": {"median": seconds}},
            }
            for runtime, seconds in (("cpython", 0.010), ("mule", candidate))
        ]
        for case, candidate in zip(cases, (0.005, 0.020), strict=True)
    }
    table = format_table(
        cases, "cpython", [{"name": "cpython"}, {"name": "mule"}], rows
    )
    lines = table.splitlines()
    assert "mule / cpython" in lines[0]
    assert next(line for line in lines if line.startswith("fast")).split()[-1] == "0.50"
    assert next(line for line in lines if line.startswith("slow")).split()[-1] == "2.00"
    assert (
        next(line for line in lines if line.startswith("GEOMEAN")).split()[-1] == "1.00"
    )


@pytest.mark.parametrize(
    "baseline_seconds,candidate_seconds,expected_ratio",
    [(0.010, 0.005, 0.5), (0.010, 0.020, 2.0), (0.010, 0.0, 0.0), (0.0, 0.010, None)],
)
def test_comparison_records_candidate_time_over_baseline_time(
    tmp_path, baseline_seconds, candidate_seconds, expected_ratio
):
    host = python_host()
    config = tmp_path / "runtimes.toml"
    config.write_text(
        'baseline = "cpython"\n'
        + "".join(
            "[[runtime]]\nname = "
            + json.dumps(runtime)
            + "\ncommand = "
            + json.dumps([host, "-c", f"print('BENCH\\tsample\\t1\\t{seconds}')"])
            + "\n"
            for runtime, seconds in (
                ("cpython", baseline_seconds),
                ("mule", candidate_seconds),
            )
        )
    )
    output = tmp_path / "out"
    subprocess.run(
        [
            host,
            "-m",
            "perf.pyperformance",
            "run",
            "unpack_sequence",
            "--config",
            str(config),
            "--samples",
            "1",
            "--warmups",
            "0",
            "--output",
            str(output),
        ],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=True,
    )
    report = json.loads((output / "results.json").read_text())
    reference, candidate = report["rows"]
    assert candidate["time_ratio"]["kernel_seconds"] == expected_ratio
    assert candidate["time_ratio"]["process_seconds"] == (
        candidate["summary"]["process_seconds"]["median"]
        / reference["summary"]["process_seconds"]["median"]
    )
    table = (output / "summary.txt").read_text()
    candidate_line = next(
        line for line in table.splitlines() if line.startswith("unpack_sequence")
    )
    assert candidate_line.split()[-1] == (
        "-" if expected_ratio is None else f"{expected_ratio:.2f}"
    )


def test_upstream_manifest_matches_vendored_files():
    upstream = json.loads((HERE / "UPSTREAM.json").read_text())
    assert upstream["source_url"] == "https://github.com/python/pyperformance"
    recorded = {entry["path"]: entry["sha256"] for entry in upstream["files"]}
    assert "COPYING" in recorded
    assert "benchmarks/MANIFEST" in recorded
    available = discover_benchmarks()
    assert len(available) == 52
    for sample in ("bm_nbody", "bm_richards", "bm_deltablue", "bm_fannkuch"):
        rel = f"benchmarks/{sample}/run_benchmark.py"
        assert rel in recorded
        actual = hashlib.sha256((HERE / rel).read_bytes()).hexdigest()
        assert actual == recorded[rel]


def test_pyperf_runner_parses_options_and_emits_bench_lines(capsys):
    runner = pyperf.Runner()
    runner.argparser.add_argument("--iterations", type=int, default=10)
    runner.argparser.add_argument("--flag", action="store_true")
    runner.argparser.add_argument("mode", nargs="?", default="fast")
    args = runner.parse_args(
        ["--loops", "2", "--warmups", "1", "--iterations", "25", "--flag", "custom"]
    )
    assert args.loops == 2
    assert args.warmups == 1
    assert args.iterations == 25
    assert args.flag is True
    assert args.mode == "custom"

    calls = []
    runner.bench_func("sample_func", lambda x: calls.append(x), 42, inner_loops=3)
    assert calls == [42, 42, 42]
    captured = capsys.readouterr()
    obs = parse_bench_output(subprocess.CompletedProcess([], 0, captured.out, ""), 0.05)
    assert obs["total_loops"] == 6
    sub_benchmarks = obs["sub_benchmarks"]
    assert isinstance(sub_benchmarks, dict)
    assert "sample_func" in sub_benchmarks
    kernel_seconds = obs["kernel_seconds"]
    assert isinstance(kernel_seconds, float)
    assert kernel_seconds >= 0.0


def test_pyperformance_cli_list_and_run_baseline(tmp_path):
    host = python_host()
    listed = subprocess.run(
        [host, "-m", "perf.pyperformance", "list"],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=True,
    )
    names = listed.stdout.splitlines()
    assert "nbody" in names
    assert "richards" in names

    available = discover_benchmarks()
    assert resolve_case_name("nbody", available) == "bm_nbody"
    assert resolve_case_name("bm_nbody", available) == "bm_nbody"

    baseline, runtimes = load_runtimes(HERE / "runtimes.toml", ["mule-o2"])
    assert baseline == "cpython-3.15t"
    assert [r["name"] for r in runtimes] == ["cpython-3.15t", "mule-o2"]

    config = tmp_path / "runtimes.toml"
    config.write_text(
        'baseline = "cpython"\n'
        '[[runtime]]\nname = "cpython"\ncommand = '
        + json.dumps([host, "{source}"])
        + "\n"
    )
    output = tmp_path / "out"
    ran = subprocess.run(
        [
            host,
            "-m",
            "perf.pyperformance",
            "run",
            "unpack_sequence",
            "--config",
            str(config),
            "--samples",
            "1",
            "--warmups",
            "0",
            "--loops",
            "1",
            "--output",
            str(output),
        ],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=True,
    )
    assert "unpack_sequence" in ran.stdout
    report = json.loads((output / "results.json").read_text())
    assert len(report["rows"]) == 1
    assert report["rows"][0]["status"] == "ok"
    assert BENCHMARKS_DIR.is_dir()
