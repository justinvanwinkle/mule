"""Build and compare vendored pyperformance benchmarks across CPython 3.15t and Mule."""

import argparse
import json
import math
import os
import platform
import statistics
import subprocess
import sys
import time
import tomllib
from concurrent.futures import ThreadPoolExecutor
from datetime import UTC, datetime
from pathlib import Path
from typing import Any

HERE = Path(__file__).resolve().parent
BENCHMARKS_DIR = HERE / "benchmarks"
ROOT = HERE.parents[1]
_SIGKILL = 9

# Benchmarks that require positional/mode arguments when invoked directly.
DEFAULT_BENCHMARK_ARGS: dict[str, tuple[str, ...]] = {
    "bm_argparse": ("subparsers",),
    "bm_async_tree": ("none",),
    "bm_pickle": ("--pure-python", "pickle"),
    "bm_xml_etree": ("--no-accelerator",),
}

# Subprocess benchmarks that spawn sys.executable as a Python interpreter.
SUBPROCESS_BENCHMARKS = frozenset(
    {
        "bm_2to3",
        "bm_python_startup",
        "bm_stdlib_startup",
    }
)


class RunFailure(Exception):
    def __init__(self, status: str, message: str):
        super().__init__(message)
        self.status = status


def discover_benchmarks() -> dict[str, Path]:
    found: dict[str, Path] = {}
    for directory in sorted(BENCHMARKS_DIR.glob("bm_*")):
        entry = directory / "run_benchmark.py"
        if entry.is_file():
            found[directory.name] = entry
    return found


def resolve_case_name(raw: str, available: dict[str, Path]) -> str:
    if raw in available:
        return raw
    prefixed = f"bm_{raw}"
    if prefixed in available:
        return prefixed
    raise KeyError(raw)


def benchmark_env(case_dir: Path) -> dict[str, str]:
    env = os.environ.copy()
    existing = [entry for entry in env.get("PYTHONPATH", "").split(os.pathsep) if entry]
    paths = [str(HERE), str(case_dir), *existing]
    env["PYTHONPATH"] = os.pathsep.join(dict.fromkeys(paths))
    return env


def invoke(
    command: list[str], cwd: str | Path, timeout: float, env: dict[str, str]
) -> tuple[subprocess.CompletedProcess[str], float]:
    started = time.perf_counter()
    try:
        process = subprocess.Popen(
            command,
            cwd=cwd,
            env=env,
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
        killpg = getattr(os, "killpg", None)
        try:
            if killpg is not None:
                killpg(process.pid, _SIGKILL)
            else:
                process.kill()
        except ProcessLookupError:
            pass
        process.communicate()
        raise RunFailure("timeout", f"exceeded {timeout}s: {command}") from error
    elapsed = time.perf_counter() - started
    if process.returncode:
        raise RunFailure(
            "execution_failed",
            f"exit {process.returncode}: {stderr[-4000:]}\nstdout: {stdout[-4000:]}",
        )
    return subprocess.CompletedProcess(
        command, process.returncode, stdout, stderr
    ), elapsed


def parse_bench_output(
    process: subprocess.CompletedProcess[str], wall: float
) -> dict[str, Any]:
    sub_benchmarks: dict[str, float] = {}
    total_loops = 0
    for line in process.stdout.splitlines():
        if not line.startswith("BENCH\t"):
            continue
        parts = line.split("\t")
        if len(parts) != 4:
            raise RunFailure("invalid_output", f"malformed BENCH line: {line!r}")
        _, name, loops_str, seconds_str = parts
        try:
            loops = int(loops_str)
            seconds = float(seconds_str)
        except ValueError as error:
            raise RunFailure(
                "invalid_output", f"invalid BENCH numbers: {line!r}"
            ) from error
        if loops < 1 or not math.isfinite(seconds) or seconds < 0:
            raise RunFailure(
                "invalid_output", f"nonfinite or negative timing: {line!r}"
            )
        sub_benchmarks[name] = seconds
        total_loops += loops
    if not sub_benchmarks:
        raise RunFailure("invalid_output", "benchmark produced no BENCH timing lines")
    # Sum per-loop times across sub-benchmarks in the script (single value for 1-sub-benchmark scripts).
    kernel_seconds = sum(sub_benchmarks.values())
    return {
        "kernel_seconds": kernel_seconds,
        "process_seconds": wall,
        "sub_benchmarks": sub_benchmarks,
        "total_loops": total_loops,
    }


def load_runtimes(
    path: Path | None, selected_runtimes: list[str] | None = None
) -> tuple[str, list[dict[str, Any]]]:
    if path is None:
        path = HERE / "runtimes.toml"
    config = tomllib.loads(path.read_text())
    baseline = config["baseline"]
    runtimes = config["runtime"]
    names = [runtime["name"] for runtime in runtimes]
    if len(set(names)) != len(names) or baseline not in names:
        raise ValueError("runtime names must be unique and include the baseline")
    for runtime in runtimes:
        cwd = Path(runtime.get("cwd", "."))
        runtime["cwd"] = str((path.parent / cwd).resolve())
    if selected_runtimes:
        unknown = [r for r in selected_runtimes if r not in names]
        if unknown:
            raise ValueError(f"unknown runtime(s): {', '.join(unknown)}")
        keep = {baseline, *selected_runtimes}
        runtimes = [r for r in runtimes if r["name"] in keep]
    return baseline, runtimes


def expand(tokens: list[str], source: Path, binary: Path) -> list[str]:
    substitutions = {
        "python": sys.executable,
        "source": str(source),
        "binary": str(binary),
    }
    return [token.format_map(substitutions) for token in tokens]


def summarize(samples: list[dict[str, Any]]) -> dict[str, dict[str, float]]:
    summary: dict[str, dict[str, float]] = {}
    for field in ("kernel_seconds", "process_seconds"):
        values = [float(sample[field]) for sample in samples]
        summary[field] = {
            "median": statistics.median(values),
            "min": min(values),
            "max": max(values),
            "stdev": statistics.pstdev(values),
        }
    return summary


def format_table(
    cases: list[str],
    baseline: str,
    runtimes: list[dict[str, Any]],
    rows_by_case: dict[str, list[dict[str, Any]]],
) -> str:
    runtime_names = [str(r["name"]) for r in runtimes]
    candidates = [name for name in runtime_names if name != baseline]
    header = ["Benchmark", f"{baseline} (ms)"]
    for cand in candidates:
        header.extend([f"{cand} (ms)", f"vs {baseline}"])
    table_rows = [header]
    speedups_by_candidate: dict[str, list[float]] = {c: [] for c in candidates}
    for case in cases:
        display = case.removeprefix("bm_")
        case_rows = {str(r["runtime"]): r for r in rows_by_case.get(case, [])}
        base_row = case_rows.get(baseline)
        if base_row and base_row.get("status") == "ok":
            base_sec = base_row["summary"]["kernel_seconds"]["median"]
            base_cell = f"{base_sec * 1000:.2f}"
        else:
            base_sec = None
            base_cell = base_row.get("status", "missing") if base_row else "missing"
        line = [display, base_cell]
        for cand in candidates:
            cand_row = case_rows.get(cand)
            if cand_row and cand_row.get("status") == "ok":
                cand_sec = cand_row["summary"]["kernel_seconds"]["median"]
                cand_cell = f"{cand_sec * 1000:.2f}"
                if base_sec and cand_sec > 0:
                    sp = base_sec / cand_sec
                    speedups_by_candidate[cand].append(sp)
                    sp_cell = f"{sp:.2f}x"
                else:
                    sp_cell = "-"
            else:
                cand_cell = cand_row.get("status", "missing") if cand_row else "missing"
                sp_cell = "-"
            line.extend([cand_cell, sp_cell])
        table_rows.append(line)
    if any(speedups_by_candidate.values()):
        footer = ["GEOMEAN", "1.00x"]
        for cand in candidates:
            vals = speedups_by_candidate[cand]
            if vals:
                gmean = math.exp(sum(math.log(v) for v in vals) / len(vals))
                footer.extend([f"({len(vals)}/{len(cases)} ok)", f"{gmean:.2f}x"])
            else:
                footer.extend(["-", "-"])
        table_rows.append(footer)
    widths = [max(len(row[i]) for row in table_rows) for i in range(len(header))]
    lines = []
    for idx, row in enumerate(table_rows):
        formatted = "  ".join(
            cell.ljust(widths[i]) if i == 0 else cell.rjust(widths[i])
            for i, cell in enumerate(row)
        )
        lines.append(formatted)
        if idx == 0 or (
            idx == len(table_rows) - 2
            and len(table_rows) > 2
            and table_rows[-1][0] == "GEOMEAN"
        ):
            lines.append("  ".join("-" * w for w in widths))
    return "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=("list", "run"))
    parser.add_argument("cases", nargs="*")
    parser.add_argument(
        "--config",
        type=Path,
        default=HERE / "runtimes.toml",
        help="TOML runtime configuration (default: perf/pyperformance/runtimes.toml)",
    )
    parser.add_argument(
        "--runtime",
        action="append",
        dest="runtimes",
        help="candidate runtime(s) to compare against baseline (repeatable)",
    )
    parser.add_argument(
        "--include-subprocess",
        action="store_true",
        help="include subprocess startup benchmarks when running all benchmarks",
    )
    parser.add_argument("--output", type=Path)
    parser.add_argument("--samples", type=int, default=3)
    parser.add_argument("--loops", type=int, default=1)
    parser.add_argument("--warmups", type=int, default=1)
    parser.add_argument("--timeout", type=float, default=60.0)
    parser.add_argument("--build-timeout", type=float, default=120.0)
    parser.add_argument("--jobs", type=int, default=4)
    args = parser.parse_args()

    available = discover_benchmarks()
    if args.action == "list":
        for name in available:
            suffix = " (subprocess)" if name in SUBPROCESS_BENCHMARKS else ""
            print(f"{name.removeprefix('bm_')}{suffix}")
        return 0

    if args.cases:
        try:
            cases = [resolve_case_name(c, available) for c in args.cases]
        except KeyError as error:
            parser.error(
                f"unknown benchmark {error.args[0]!r}; use 'list' to see available benchmarks"
            )
    else:
        cases = [
            name
            for name in available
            if args.include_subprocess or name not in SUBPROCESS_BENCHMARKS
        ]

    baseline, runtimes = load_runtimes(args.config.resolve(), args.runtimes)
    stamp = datetime.now(UTC).strftime("%Y%m%dT%H%M%S.%fZ")
    output = (args.output or ROOT / "perf/results" / f"pyperformance-{stamp}").resolve()
    output.mkdir(parents=True, exist_ok=True)
    artifacts = output / "build"
    artifacts.mkdir(exist_ok=True)

    entries_by_case: dict[str, list[dict[str, Any]]] = {}
    build_tasks = []
    for case_idx, case in enumerate(cases):
        source = available[case]
        env = benchmark_env(source.parent)
        entries = []
        for rt_idx, runtime in enumerate(runtimes):
            binary = artifacts / f"{case_idx}-{rt_idx}-{case}"
            entry: dict[str, Any] = {
                "runtime": runtime["name"],
                "cwd": runtime["cwd"],
                "env": env,
                "command": expand(runtime["command"], source, binary),
            }
            entries.append(entry)
            if runtime.get("build"):
                cmd = expand(runtime["build"], source, binary)
                build_tasks.append((case, runtime, binary, entry, cmd, env))
        entries_by_case[case] = entries

    def _run_build(task):
        case, runtime, binary, entry, cmd, env = task
        print(f"Building {case.removeprefix('bm_')}: {runtime['name']}", flush=True)
        try:
            _, seconds = invoke(cmd, runtime["cwd"], args.build_timeout, env)
            if not binary.is_file():
                raise RunFailure("build_failed", f"build did not produce {binary}")
            entry["build_seconds"] = seconds
        except RunFailure as error:
            entry["error"] = {"status": "build_failed", "error": str(error)}

    if build_tasks:
        with ThreadPoolExecutor(max_workers=args.jobs) as pool:
            list(pool.map(_run_build, build_tasks))

    def _case_command(entry: dict[str, Any], case: str) -> list[str]:
        extra = list(DEFAULT_BENCHMARK_ARGS.get(case, ()))
        return [
            *entry["command"],
            "--warmups",
            str(args.warmups),
            "--loops",
            str(args.loops),
            *extra,
        ]

    def _measure_case(case: str) -> tuple[str, list[dict[str, Any]]]:
        entries = entries_by_case[case]
        rows: list[dict[str, Any]] = []
        active: list[tuple[dict[str, Any], dict[str, Any]]] = []
        for entry in entries:
            row: dict[str, Any] = {
                "case": case,
                "runtime": entry["runtime"],
                "samples": [],
            }
            if "build_seconds" in entry:
                row["build_seconds"] = entry["build_seconds"]
            rows.append(row)
            if entry.get("error"):
                row.update(entry["error"])
                continue
            row["status"] = "ok"
            active.append((row, entry))

        for sample_idx in range(args.samples):
            offset = sample_idx % max(1, len(active))
            order = active[offset:] + active[:offset]
            if sample_idx % 2:
                order = list(reversed(order))
            for row, entry in order:
                if row["status"] != "ok":
                    continue
                cmd = _case_command(entry, case)
                try:
                    proc, wall = invoke(cmd, entry["cwd"], args.timeout, entry["env"])
                    obs = parse_bench_output(proc, wall)
                    row["samples"].append(obs)
                except RunFailure as error:
                    row.update(status=error.status, error=str(error))

        for row in rows:
            if row["status"] == "ok" and row["samples"]:
                row["summary"] = summarize(row["samples"])
        base_row = next((r for r in rows if r["runtime"] == baseline), None)
        if base_row and base_row.get("status") == "ok":
            for row in rows:
                if row.get("status") == "ok":
                    row["speedup"] = {}
                    for field in ("kernel_seconds", "process_seconds"):
                        sec = row["summary"][field]["median"]
                        row["speedup"][field] = (
                            base_row["summary"][field]["median"] / sec
                            if sec > 0
                            else None
                        )
        statuses = ", ".join(f"{r['runtime']}={r['status']}" for r in rows)
        print(f"Measured {case.removeprefix('bm_')}: {statuses}", flush=True)
        return case, rows

    rows_by_case: dict[str, list[dict[str, Any]]] = {}
    all_rows: list[dict[str, Any]] = []
    with ThreadPoolExecutor(max_workers=args.jobs) as pool:
        for case, rows in pool.map(_measure_case, cases):
            rows_by_case[case] = rows
            all_rows.extend(rows)

    report = {
        "created_utc": stamp,
        "platform": platform.platform(),
        "baseline": baseline,
        "runtimes": [
            {"name": r["name"], "command": r["command"], "build": r.get("build")}
            for r in runtimes
        ],
        "options": {
            "samples": args.samples,
            "loops": args.loops,
            "warmups": args.warmups,
            "timeout": args.timeout,
        },
        "rows": all_rows,
    }
    results_path = output / "results.json"
    results_path.write_text(json.dumps(report, indent=2) + "\n")
    table = format_table(cases, baseline, runtimes, rows_by_case)
    (output / "summary.txt").write_text(table + "\n")
    print("\n" + table)
    print(f"\nResults written to {results_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
