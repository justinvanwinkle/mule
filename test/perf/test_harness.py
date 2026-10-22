import json
import subprocess
from argparse import Namespace
from pathlib import Path

import pytest

from perf.suite.perfutils import history
from perf.suite.perfutils.__main__ import (
    RunFailure,
    load_runtimes,
    measure_group,
    read_sample,
)
from test.python_host import python_host

ROOT = Path(__file__).resolve().parents[2]


def protocol(**overrides):
    fields = {
        "CASE": "integer_accumulation",
        "SIZE": "10",
        "SEED": "42",
        "LOOPS": "1",
        "RESULT": "(12, 3)",
        "PREPARE": "0.001",
        "KERNEL": "0.002",
    }
    fields.update(overrides)
    return "\n".join(key + "\t" + value for key, value in fields.items()) + "\n"


@pytest.mark.parametrize(
    "changes", [{"KERNEL": "nan"}, {"PREPARE": "-1"}, {"LOOPS": "0"}, {"SIZE": "wrong"}]
)
def test_protocol_rejects_invalid_measurements(changes):
    process = subprocess.CompletedProcess([], 0, protocol(**changes), "")
    with pytest.raises(RunFailure) as error:
        read_sample(process, 0.1, "integer_accumulation")
    assert error.value.status == "invalid_output"


def test_protocol_rejects_duplicate_fields():
    process = subprocess.CompletedProcess([], 0, protocol() + "KERNEL\t0.1\n", "")
    with pytest.raises(RunFailure, match="duplicate"):
        read_sample(process, 0.1, "integer_accumulation")


def test_config_rejects_duplicate_names(tmp_path):
    config = tmp_path / "runtimes.toml"
    config.write_text(
        'baseline = "x"\n' + '[[runtime]]\nname="x"\ncommand=["python"]\n' * 2
    )
    with pytest.raises(ValueError, match="unique"):
        load_runtimes(config)


def test_harness_records_bad_results_and_continues(tmp_path):
    # Execute the real case, then corrupt just the candidate's result. The
    # candidate must never contribute a time ratio or a timing sample.
    host = python_host()
    wrapper = tmp_path / "corrupt.py"
    wrapper.write_text(
        "import subprocess, sys\n"
        "result = subprocess.run([sys.executable, *sys.argv[1:]], "
        "capture_output=True, text=True, check=True)\n"
        "for line in result.stdout.splitlines():\n"
        "    print('RESULT\\t(0, 0)' if line.startswith('RESULT\\t') else line)\n"
    )
    config = tmp_path / "runtimes.toml"
    config.write_text(
        'baseline = "reference"\n'
        '[[runtime]]\nname="reference"\ncommand='
        + json.dumps([host, "{source}"])
        + '\n[[runtime]]\nname="corrupt"\ncommand='
        + json.dumps([host, str(wrapper), "{source}"])
        + '\n[[runtime]]\nname="another-python"\ncommand='
        + json.dumps([host, "{source}"])
        + "\n"
    )
    output = tmp_path / "report"
    process = subprocess.run(
        [
            host,
            "-m",
            "perf.suite.perfutils",
            "run",
            "integer_accumulation",
            "--config",
            str(config),
            "--size",
            "10",
            "--samples",
            "2",
            "--output",
            str(output),
        ],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
    )
    assert process.returncode == 1, process.stderr
    report = json.loads((output / "results.json").read_text())
    assert len(report["rows"]) == 3
    for row in report["rows"]:
        if row["runtime"] == "corrupt":
            assert row["status"] == "incorrect_result"
            assert row["samples"] == []
            assert "time_ratio" not in row
        else:
            assert row["status"] == "ok"
            assert len(row["samples"]) == 2
            assert row["time_ratio"]["kernel_seconds"] > 0
    assert "incorrect_result" in (output / "index.html").read_text()
    for name in ("kernel.svg", "process.svg"):
        svg = (output / name).read_text()
        assert svg.startswith('<svg xmlns="http://www.w3.org/2000/svg"')
        assert svg.endswith("</svg>")


def test_timeout_is_visible(tmp_path):
    script = tmp_path / "slow.py"
    script.write_text("import time\ntime.sleep(10)\n")
    from perf.suite.perfutils.__main__ import invoke

    with pytest.raises(RunFailure) as error:
        invoke([python_host(), str(script)], tmp_path, 0.02)
    assert error.value.status == "timeout"


@pytest.mark.parametrize(
    ("baseline_seconds", "runtime_seconds", "expected"),
    [(2, 1, 0.5), (2, 6, 3), (2, 2, 1), (2, 0, 0), (0, 2, None)],
)
def test_measure_group_reports_runtime_time_over_baseline(
    tmp_path, baseline_seconds, runtime_seconds, expected
):
    # Static reported kernel times distinguish the ratio direction without
    # depending on process scheduling or clock resolution.
    entries = []
    for runtime, seconds in (
        ("reference", baseline_seconds),
        ("candidate", runtime_seconds),
    ):
        script = tmp_path / f"{runtime}.py"
        script.write_text(f"print({protocol(KERNEL=str(seconds))!r}, end='')\n")
        entries.append(
            {
                "runtime": runtime,
                "command": [python_host(), str(script)],
                "cwd": tmp_path,
            }
        )
    args = Namespace(seed=42, loops=1, warmups=0, size=10, samples=1, timeout=10)
    reference = read_sample(
        subprocess.CompletedProcess([], 0, protocol(KERNEL=str(baseline_seconds)), ""),
        0.1,
        "integer_accumulation",
    )
    rows = measure_group("integer_accumulation", entries, "reference", args, reference)
    assert rows[1]["time_ratio"]["kernel_seconds"] == expected
    assert rows[1]["time_ratio"]["process_seconds"] == (
        rows[1]["summary"]["process_seconds"]["median"]
        / rows[0]["summary"]["process_seconds"]["median"]
    )


def _report(
    kernel_seconds, status="ok", commit="0123456789abcdef", diff=history._CLEAN_DIFF
):
    def row(runtime, seconds, time_ratio):
        summary = {
            field: {"median": seconds, "min": seconds, "max": seconds, "stdev": 0.0}
            for field in ("kernel_seconds", "process_seconds", "preparation_seconds")
        }
        return {
            "case": "integer_accumulation",
            "runtime": runtime,
            "status": status if runtime == "mule" else "ok",
            "samples": [],
            "summary": summary,
            "time_ratio": {"kernel_seconds": time_ratio, "process_seconds": time_ratio},
        }

    return {
        "created_utc": "20261015T120000.000000Z",
        "platform": "test",
        "baseline": "cpython",
        "options": {"samples": 3, "size": None, "seed": 42, "loops": 1, "warmups": 1},
        "runtimes": [
            {"name": "cpython", "revision": None, "version": "Python 3.14.7\n"},
            {
                "name": "mule",
                "revision": {"commit": commit, "tracked_diff_sha256": diff},
            },
        ],
        "rows": [
            row("cpython", 0.010, 1.0),
            row("mule", kernel_seconds, kernel_seconds / 0.010),
        ],
    }


def test_history_records_runs_and_tabulates_the_candidate_trend(tmp_path):
    path = tmp_path / "history.jsonl"
    history.record(_report(0.050), path)
    history.record(_report(0.020, diff="not clean"), path)
    runs = history.load(path)
    assert [run["results"][1]["kernel_ms"] for run in runs] == [50.0, 20.0]
    assert [run["results"][1]["kernel_time_ratio"] for run in runs] == [5.0, 2.0]
    assert [run["results"][1]["process_time_ratio"] for run in runs] == [5.0, 2.0]
    assert runs[0]["runtimes"][1]["commit"] == "0123456789abcdef"
    lines = history.table(runs)
    assert "mule" in lines[2] and "mule" in lines[3]
    assert "integer_accumulation" in lines[1]
    assert "012345678 " in lines[2] and "50 ms 5" in lines[2]
    assert "012345678+" in lines[3] and "20 ms 2" in lines[3]


def test_history_shows_failures_and_limits_runs(tmp_path):
    path = tmp_path / "history.jsonl"
    history.record(_report(0.050), path)
    history.record(_report(0.050, status="build_failed"), path)
    lines = history.table(history.load(path), limit=1)
    assert len(lines) == 3
    assert "build_failed" in lines[2]
    assert history.table([]) == ["no history"]


def test_history_lists_all_candidate_runtimes_including_sbcl(tmp_path):
    path = tmp_path / "history.jsonl"
    report = _report(0.020)
    report["runtimes"].append(
        {
            "name": "sbcl",
            "revision": {
                "commit": "0123456789abcdef",
                "tracked_diff_sha256": history._CLEAN_DIFF,
            },
            "version": "SBCL 2.6.4\n",
        }
    )
    report["rows"].append(
        {
            "case": "integer_accumulation",
            "runtime": "sbcl",
            "status": "ok",
            "samples": [],
            "summary": {
                field: {"median": 0.002, "min": 0.002, "max": 0.002, "stdev": 0.0}
                for field in (
                    "kernel_seconds",
                    "process_seconds",
                    "preparation_seconds",
                )
            },
            "time_ratio": {"kernel_seconds": 0.2, "process_seconds": 0.2},
        }
    )
    history.record(report, path)
    runs = history.load(path)
    lines = history.table(runs)
    assert len(lines) == 4
    assert "mule" in lines[2] and "20 ms 2" in lines[2]
    assert "sbcl" in lines[3] and "2 ms 0.2" in lines[3]
    sbcl_only = history.table(runs, runtime_name="sbcl")
    assert len(sbcl_only) == 3
    assert "sbcl" in sbcl_only[2]
