"""Run standalone scenarios against configured interpreters and native builds."""

import argparse
import hashlib
import math
import os
import platform
import signal
import statistics
import subprocess
import sys
import time
import tomllib
from datetime import UTC, datetime
from pathlib import Path

from .report import write_report

SUITE = Path(__file__).resolve().parents[1]
ROOT = SUITE.parents[1]


class RunFailure(Exception):
    def __init__(self, status, message):
        super().__init__(message)
        self.status = status


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def invoke(command, cwd, timeout):
    started = time.perf_counter()
    try:
        process = subprocess.Popen(
            command,
            cwd=cwd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            start_new_session=True,
        )
    except OSError as error:
        raise RunFailure("launch_failed", str(error)) from error
    try:
        stdout, stderr = process.communicate(timeout=timeout)
    except subprocess.TimeoutExpired as error:
        # Compiler drivers and scenarios can spawn children. Kill the whole
        # process group so a timed-out job cannot overlap subsequent samples.
        try:
            os.killpg(process.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        process.communicate()
        raise RunFailure("timeout", f"exceeded {timeout}s: {command}") from error
    elapsed = time.perf_counter() - started
    if process.returncode:
        raise RunFailure(
            "execution_failed",
            f"exit {process.returncode}: {stderr[-16000:]}\nstdout: {stdout[-16000:]}",
        )
    return subprocess.CompletedProcess(
        command, process.returncode, stdout, stderr
    ), elapsed


def read_sample(process, wall, case):
    fields = {}
    for line in process.stdout.splitlines():
        key, separator, value = line.partition("\t")
        if not separator or key in fields:
            raise RunFailure("invalid_output", "malformed or duplicate protocol field")
        fields[key] = value
    required = {
        "CASE",
        "SIZE",
        "SEED",
        "LOOPS",
        "RESULT",
        "PREPARE",
        "KERNEL",
    }
    if set(fields) != required or fields["CASE"] != case:
        raise RunFailure("invalid_output", "missing fields or wrong scenario identity")
    if not fields["RESULT"]:
        raise RunFailure("invalid_output", "empty result")
    try:
        kernel = float(fields.pop("KERNEL"))
        preparation = float(fields.pop("PREPARE"))
        size = int(fields["SIZE"])
        int(fields["SEED"])
        loops = int(fields["LOOPS"])
    except ValueError as error:
        raise RunFailure("invalid_output", "invalid timing or input field") from error
    if not all(math.isfinite(value) and value >= 0 for value in (kernel, preparation)):
        raise RunFailure("invalid_output", "nonfinite or negative timing")
    if size < 1 or loops < 1:
        raise RunFailure("invalid_output", "nonpositive size or loop count")
    return {
        "kernel_seconds": kernel,
        "preparation_seconds": preparation,
        "process_seconds": wall,
        "fields": fields,
        "stderr": process.stderr,
    }


def load_runtimes(path):
    if path is None:
        return "cpython", [
            {
                "name": "cpython",
                "command": [sys.executable, "{source}"],
                "version_command": [sys.executable, "--version"],
            }
        ]
    config = tomllib.loads(path.read_text())
    baseline = config["baseline"]
    runtimes = config["runtime"]
    names = [runtime["name"] for runtime in runtimes]
    if len(set(names)) != len(names) or baseline not in names:
        raise ValueError("runtime names must be unique and include the baseline")
    for runtime in runtimes:
        if not runtime["name"] or not runtime["command"]:
            raise ValueError("each runtime needs a name and command")
        cwd = Path(runtime.get("cwd", "."))
        runtime["cwd"] = str((path.parent / cwd).resolve())
    return baseline, runtimes


def expand(tokens, source, binary):
    substitutions = {
        "python": sys.executable,
        "source": str(source),
        "binary": str(binary),
    }
    return [token.format_map(substitutions) for token in tokens]


def summarize(samples):
    summary = {}
    for field in ("kernel_seconds", "process_seconds", "preparation_seconds"):
        values = [sample[field] for sample in samples]
        summary[field] = {
            "median": statistics.median(values),
            "min": min(values),
            "max": max(values),
            "stdev": statistics.pstdev(values),
        }
    return summary


def scenario_command(entry, args):
    options = [
        "--seed",
        str(args.seed),
        "--loops",
        str(args.loops),
        "--warmups",
        str(args.warmups),
    ]
    if args.size is not None:
        options += ["--size", str(args.size)]
    return entry["command"] + options


def scenario_sample(entry, case, args):
    process, wall = invoke(scenario_command(entry, args), entry["cwd"], args.timeout)
    observation = read_sample(process, wall, case)
    fields = observation["fields"]
    if int(fields["SEED"]) != args.seed or int(fields["LOOPS"]) != args.loops:
        raise RunFailure("invalid_output", "scenario ignored seed or loops")
    if args.size is not None and int(fields["SIZE"]) != args.size:
        raise RunFailure("invalid_output", "scenario ignored requested size")
    return observation


def revision(cwd):
    result = subprocess.run(
        ["git", "rev-parse", "HEAD"],
        cwd=cwd,
        capture_output=True,
        text=True,
        check=False,
    )
    if result.returncode:
        return None
    diff = subprocess.run(
        ["git", "diff", "HEAD"], cwd=cwd, capture_output=True, check=False
    )
    return {
        "commit": result.stdout.strip(),
        "tracked_diff_sha256": hashlib.sha256(diff.stdout).hexdigest(),
    }


def measure_group(case, entries, baseline, args, reference):
    """Validate all candidates before rotating serial timed process samples."""
    rows = []
    active = []
    for entry in entries:
        row = {
            "case": case,
            "runtime": entry["runtime"],
            "samples": [],
        }
        rows.append(row)
        if entry.get("error"):
            row.update(entry["error"])
            continue
        if reference is None:
            row.update(status="reference_failed", error="baseline run failed")
            continue
        row["command"] = scenario_command(entry, args)

        def sample(entry=entry):
            observation = scenario_sample(entry, case, args)
            fields = observation["fields"]
            for field in ("SIZE", "SEED", "LOOPS", "RESULT"):
                if fields[field] != reference["fields"][field]:
                    raise RunFailure(
                        "incorrect_result", f"{field} differs from baseline"
                    )
            return observation

        try:
            validation = sample()
            row["validation"] = validation
            row["status"] = "ok"
            active.append((row, sample))
        except RunFailure as error:
            row.update(status=error.status, error=str(error))
    for index in range(args.samples):
        offset = index % max(1, len(active))
        order = active[offset:] + active[:offset]
        if index % 2:
            order = list(reversed(order))
        for row, sample in order:
            if row["status"] != "ok":
                continue
            try:
                row["samples"].append(sample())
            except RunFailure as error:
                row.update(status=error.status, error=str(error))
    for row in rows:
        if row["status"] == "ok":
            row["summary"] = summarize(row["samples"])
    baseline_row = next((row for row in rows if row["runtime"] == baseline), None)
    if baseline_row and baseline_row["status"] == "ok":
        for row in rows:
            if row["status"] == "ok":
                row["speedup"] = {}
                for field in ("kernel_seconds", "process_seconds"):
                    seconds = row["summary"][field]["median"]
                    row["speedup"][field] = (
                        baseline_row["summary"][field]["median"] / seconds
                        if seconds
                        else None
                    )
    return rows


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=("list", "run"))
    parser.add_argument("cases", nargs="*")
    parser.add_argument("--config", type=Path, help="TOML runtime/build commands")
    parser.add_argument("--output", type=Path)
    parser.add_argument("--samples", type=int, default=5)
    parser.add_argument("--size", type=int, help="override each case's workload size")
    parser.add_argument("--seed", type=int, default=42)
    parser.add_argument("--loops", type=int, default=1)
    parser.add_argument("--warmups", type=int, default=1)
    parser.add_argument("--timeout", type=float, default=30)
    parser.add_argument("--build-timeout", type=float, default=600)
    args = parser.parse_args()
    available = {path.stem: path for path in sorted(SUITE.glob("*.py"))}
    if args.action == "list":
        print("\n".join(available))
        return 0
    cases = args.cases or list(available)
    if len(set(cases)) != len(cases) or any(case not in available for case in cases):
        parser.error("select unique scenario names from the list command")
    if args.samples < 1 or args.loops < 1 or args.warmups < 0:
        parser.error("samples/loops must be positive; warmups must be nonnegative")
    if args.size is not None and args.size < 1:
        parser.error("size must be positive")
    if not all(
        math.isfinite(value) and value > 0
        for value in (args.timeout, args.build_timeout)
    ):
        parser.error("timeouts must be finite and positive")
    baseline, runtimes = load_runtimes(args.config.resolve() if args.config else None)
    stamp = datetime.now(UTC).strftime("%Y%m%dT%H%M%S.%fZ")
    output = (args.output or ROOT / "perf/results" / stamp).resolve()
    output.mkdir(parents=True, exist_ok=False)
    artifacts = output / "build"
    artifacts.mkdir()
    options = vars(args).copy()
    options["config"] = str(args.config) if args.config else None
    options["output"] = str(output)
    report = {
        "schema_version": 2,
        "created_utc": stamp,
        "baseline": baseline,
        "platform": platform.platform(),
        "harness_python": sys.version,
        "affinity": sorted(os.sched_getaffinity(0)),
        "options": options,
        "sources": {case: digest(available[case]) for case in cases},
        "helpers": {
            path.name: digest(path)
            for path in sorted(Path(__file__).parent.glob("*.py"))
        },
        "runtimes": runtimes,
        "builds": [],
        "rows": [],
        "timing": "kernel: prepare/validation excluded; process: startup, prepare, warmups and loops included",
    }
    for runtime in runtimes:
        runtime.setdefault("cwd", str(ROOT))
        runtime["revision"] = revision(runtime["cwd"])
        if runtime.get("version_command"):
            command = expand(runtime["version_command"], ROOT, ROOT)
            try:
                process, _ = invoke(command, runtime["cwd"], args.timeout)
                runtime["version"] = process.stdout + process.stderr
            except RunFailure as error:
                runtime["version_error"] = str(error)
    # All builds finish before any measurements, so compiler jobs never overlap timing.
    entries_by_case = {}
    for case_index, case in enumerate(cases):
        entries = []
        for runtime_index, runtime in enumerate(runtimes):
            binary = artifacts / f"{case_index}-{runtime_index}"
            entry = {
                "runtime": runtime["name"],
                "cwd": runtime["cwd"],
                "command": expand(runtime["command"], available[case], binary),
            }
            entries.append(entry)
            if runtime.get("build"):
                command = expand(runtime["build"], available[case], binary)
                build = {"case": case, "runtime": runtime["name"], "command": command}
                report["builds"].append(build)
                print(f"Building {case}: {runtime['name']}", flush=True)
                try:
                    process, seconds = invoke(
                        command, runtime["cwd"], args.build_timeout
                    )
                    if not binary.is_file():
                        raise RunFailure(
                            "build_failed", "build did not produce {binary}"
                        )
                    build.update(
                        status="ok",
                        seconds=seconds,
                        binary_sha256=digest(binary),
                        stdout=process.stdout,
                        stderr=process.stderr,
                    )
                except RunFailure as error:
                    entry["error"] = {"status": "build_failed", "error": str(error)}
                    build.update(entry["error"])
        entries_by_case[case] = entries
    write_report(report, output)
    for case in cases:
        entries = entries_by_case[case]
        reference_entry = next(
            entry for entry in entries if entry["runtime"] == baseline
        )
        reference = None
        if not reference_entry.get("error"):
            try:
                reference = scenario_sample(reference_entry, case, args)
            except RunFailure as error:
                reference_entry["error"] = {"status": error.status, "error": str(error)}
        print(f"Measuring {case}", flush=True)
        rows = measure_group(case, entries, baseline, args, reference)
        report["rows"].extend(rows)
        write_report(report, output)
        for row in rows:
            print(f"  {row['runtime']}: {row['status']}", flush=True)
    print(output / "index.html")
    return int(any(row["status"] != "ok" for row in report["rows"]))


if __name__ == "__main__":
    raise SystemExit(main())
