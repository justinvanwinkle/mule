"""Build once, validate output, then compare native runtime with CPython 3.14."""

import argparse
import hashlib
import json
import os
import platform
import statistics
import subprocess
import sys
import time
from pathlib import Path
from typing import Any

from mule.compiler import build

ROOT = Path(__file__).resolve().parents[1]
CASES = {
    "integer_loop": 1_000_000,
    "float_loop": 1_000_000,
    "function_calls": 500_000,
    "list_index": 1_000_000,
    "attributes": 250_000,
    "sha3": 262_144,
    "nbody": 1_000,
}
SOURCES = {
    "nbody": ROOT / "examples/benchmarks-game/nbody/nbody.py",
}


def run(command, timeout):
    started = time.perf_counter()
    result = subprocess.run(command, capture_output=True, check=False, timeout=timeout)
    elapsed = time.perf_counter() - started
    if result.returncode:
        raise RuntimeError(
            f"{command}: exit {result.returncode}: {result.stderr.decode(errors='replace')}"
        )
    if result.stderr:
        raise RuntimeError(
            f"unexpected stderr: {result.stderr.decode(errors='replace')}"
        )
    return elapsed, result.stdout


def benchmark(
    name,
    count,
    directory,
    samples,
    timeout,
    rebuild,
    baseline_directory=None,
    lto=False,
) -> dict[str, Any]:
    source = SOURCES.get(name, ROOT / "examples/performance" / (name + ".py"))
    executable = directory / name
    built_seconds = None
    if rebuild or not executable.exists():
        started = time.perf_counter()
        build(source, executable, optimize=3, lto=lto, workers=1)
        built_seconds = time.perf_counter() - started
    python_command = [sys.executable, str(source), str(count)]
    native_command = [str(executable), str(count)]
    _, expected = run(python_command, timeout)
    _, actual = run(native_command, timeout)
    if actual != expected:
        raise AssertionError(f"{name}: native output differs from CPython")
    if name == "nbody" and count == 1_000:
        upstream = source.with_name("1000_out").read_bytes()
        if actual != upstream:
            raise AssertionError("nbody: output differs from upstream reference")
    measurements = {"python": [], "mule": []}
    commands = {"python": python_command, "mule": native_command}
    baseline_executable = None
    if baseline_directory is not None:
        baseline_executable = baseline_directory / name
        baseline_command = [str(baseline_executable), str(count)]
        _, baseline_output = run(baseline_command, timeout)
        if baseline_output != expected:
            raise AssertionError(f"{name}: baseline output differs from CPython")
        commands["baseline"] = baseline_command
        measurements["baseline"] = []
    for sample in range(samples):
        order = list(commands)
        offset = sample % len(order)
        order = order[offset:] + order[:offset]
        if sample % 2:
            order.reverse()
        for runtime in order:
            elapsed, output = run(commands[runtime], timeout)
            if output != expected:
                raise AssertionError(f"{name}: unstable {runtime} output")
            measurements[runtime].append(elapsed)
    medians = {key: statistics.median(values) for key, values in measurements.items()}
    report: dict[str, Any] = {
        "name": name,
        "input": count,
        "source_sha3_256": hashlib.sha3_256(source.read_bytes()).hexdigest(),
        "executable_sha3_256": hashlib.sha3_256(executable.read_bytes()).hexdigest(),
        "output": expected.decode(),
        "build_seconds": built_seconds,
        "reused_executable": built_seconds is None,
        "samples_seconds": measurements,
        "medians_seconds": medians,
        "speedup": medians["python"] / medians["mule"],
    }
    if baseline_executable is not None:
        report["baseline_executable_sha3_256"] = hashlib.sha3_256(
            baseline_executable.read_bytes()
        ).hexdigest()
        report["speedup_over_baseline"] = medians["baseline"] / medians["mule"]
    return report


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("cases", nargs="*", choices=tuple(CASES))
    parser.add_argument("--directory", type=Path, default=Path("/tmp/mule-performance"))
    parser.add_argument("--samples", type=int, default=3)
    parser.add_argument("--timeout", type=float, default=30)
    parser.add_argument("--count", type=int)
    parser.add_argument("--reuse", action="store_true")
    parser.add_argument("--report", type=Path)
    parser.add_argument("--baseline-directory", type=Path)
    parser.add_argument(
        "--lto", action="store_true", help="enable full LTO for new builds"
    )
    args = parser.parse_args()
    if args.samples < 1:
        parser.error("--samples must be positive")
    args.directory.mkdir(parents=True, exist_ok=True)
    revision = subprocess.run(
        ["git", "rev-parse", "HEAD"],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=True,
    ).stdout.strip()
    diff = subprocess.run(
        ["git", "diff", "HEAD", "--", "mule", "stdlib"],
        cwd=ROOT,
        capture_output=True,
        check=True,
    ).stdout
    report = {
        "python": sys.version,
        "python_executable": sys.executable,
        "platform": platform.platform(),
        "affinity": sorted(os.sched_getaffinity(0)),
        "optimization": "LLVM O3 and Rust O3",
        "lto": args.lto,
        "compiler_revision": revision,
        "compiler_diff_sha3_256": hashlib.sha3_256(diff).hexdigest(),
        "compiler_state_applies_to_reused_executables": False,
        "timing": "process wall clock, including startup, excluding compilation",
        "cases": [],
    }
    timed_out = False
    for name in args.cases or CASES:
        print(f"Building/checking {name}...", flush=True)
        count = CASES[name] if args.count is None else args.count
        try:
            result = benchmark(
                name,
                count,
                args.directory,
                args.samples,
                args.timeout,
                not args.reuse,
                args.baseline_directory,
                args.lto,
            )
        except subprocess.TimeoutExpired as error:
            timed_out = True
            result: dict[str, Any] = {
                "name": name,
                "input": count,
                "status": "timeout",
                "timeout_seconds": error.timeout,
                "command": error.cmd,
            }
        report["cases"].append(result)
        (args.report or args.directory / "results.json").write_text(
            json.dumps(report, indent=2) + "\n"
        )
        if result.get("status") == "timeout":
            print(
                f"{name}: exceeded {result['timeout_seconds']}s; no validated speedup",
                flush=True,
            )
            continue
        print(
            f"{name}: Python {result['medians_seconds']['python']:.6f}s, "
            f"Mule {result['medians_seconds']['mule']:.6f}s, "
            f"{result['speedup']:.3f}x speedup",
            flush=True,
        )
        if "speedup_over_baseline" in result:
            print(
                f"{name}: {result['speedup_over_baseline']:.3f}x faster than baseline",
                flush=True,
            )
    if timed_out:
        raise SystemExit(1)


if __name__ == "__main__":
    main()
