import json
import subprocess
import sys
from pathlib import Path
from xml.etree import ElementTree

import pytest

from perf.suite.perfutils.__main__ import RunFailure, load_runtimes, read_sample

ROOT = Path(__file__).resolve().parents[2]


def protocol(**overrides):
    fields = {
        "CASE": "integer_accumulation",
        "SIZE": "10",
        "SEED": "42",
        "VARIANT": "untyped",
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
        read_sample(process, 0.1, "integer_accumulation", "untyped")
    assert error.value.status == "invalid_output"


def test_protocol_rejects_duplicate_fields():
    process = subprocess.CompletedProcess([], 0, protocol() + "KERNEL\t0.1\n", "")
    with pytest.raises(RunFailure, match="duplicate"):
        read_sample(process, 0.1, "integer_accumulation", "untyped")


def test_config_rejects_duplicate_names(tmp_path):
    config = tmp_path / "runtimes.toml"
    config.write_text(
        'baseline = "x"\n' + '[[runtime]]\nname="x"\ncommand=["python"]\n' * 2
    )
    with pytest.raises(ValueError, match="unique"):
        load_runtimes(config)


def test_harness_records_bad_results_and_continues(tmp_path):
    # Execute the real case, then corrupt just the candidate's result. The
    # candidate must never contribute a speedup or a timing sample.
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
        + json.dumps([sys.executable, "{source}"])
        + '\n[[runtime]]\nname="corrupt"\ncommand='
        + json.dumps([sys.executable, str(wrapper), "{source}"])
        + '\n[[runtime]]\nname="another-python"\ncommand='
        + json.dumps([sys.executable, "{source}"])
        + "\n"
    )
    output = tmp_path / "report"
    process = subprocess.run(
        [
            sys.executable,
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
    assert len(report["rows"]) == 9
    for row in report["rows"]:
        if row["runtime"] == "corrupt":
            assert row["status"] == "incorrect_result"
            assert row["samples"] == []
            assert "speedup" not in row
        else:
            assert row["status"] == "ok"
            assert len(row["samples"]) == 2
            assert row["speedup"]["kernel_seconds"] > 0
    assert "incorrect_result" in (output / "index.html").read_text()
    for name in ("kernel.svg", "process.svg"):
        ElementTree.parse(output / name)


def test_timeout_is_visible(tmp_path):
    script = tmp_path / "slow.py"
    script.write_text("import time\ntime.sleep(10)\n")
    from perf.suite.perfutils.__main__ import invoke

    with pytest.raises(RunFailure) as error:
        invoke([sys.executable, str(script)], tmp_path, 0.02)
    assert error.value.status == "timeout"
