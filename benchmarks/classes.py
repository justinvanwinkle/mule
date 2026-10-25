"""Compare compiled raw fields, boxed fields, CPython, and Rust structs."""

import argparse
import hashlib
import json
import os
import platform
import random
import re
import statistics
import subprocess
import sys
import time
from datetime import UTC, datetime
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
OUTPUT = ROOT / "build/benchmarks/native-classes"


def execute(command, limit, environment):
    start = time.perf_counter_ns()
    process = subprocess.run(
        command,
        input=f"{limit}\n",
        text=True,
        capture_output=True,
        check=True,
        env=environment,
        timeout=120,
    )
    seconds = (time.perf_counter_ns() - start) / 1e9
    if process.stderr:
        raise RuntimeError(process.stderr)
    return process.stdout.strip(), seconds


def main():
    from mule.compiler import build

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repeats", type=int, default=7)
    parser.add_argument("--warmups", type=int, default=1)
    parser.add_argument("--limit", type=int, default=200_000_000)
    parser.add_argument("--cpu", type=int)
    parser.add_argument("--output", type=Path, default=OUTPUT / "results.json")
    args = parser.parse_args()
    if args.repeats < 1 or args.warmups < 0 or not 0 <= args.limit < 2**61:
        parser.error(
            "positive repeats, nonnegative warmups and limit below 2**61 required"
        )
    allowed = os.sched_getaffinity(0)
    cpu = min(allowed) if args.cpu is None else args.cpu
    if cpu not in allowed:
        parser.error("CPU is outside allowed affinity")
    os.sched_setaffinity(0, {cpu})
    OUTPUT.mkdir(parents=True, exist_ok=True)
    source = (ROOT / "examples/class_counter.py").read_text()
    prefix, separator, _ = source.rpartition("\nprint(count(")
    if not separator:
        raise ValueError("class counter example must end with print(count(...))")
    source = prefix + "\nprint(count(int(input())))\n"
    source = re.sub(
        r"(?m)^@mule_types\.optimize\([^\n]*\)\n(?=(?:class Counter|def count)\b)",
        "@mule_types.optimize(speed=3, safety=0)\n",
        source,
    )
    nonlinear = source.replace(
        "self.value += amount", "self.value = (self.value ^ amount) + 3"
    )
    boxed = source.replace(
        "@mule_types.optimize(speed=3, safety=0)\nclass Counter:", "class Counter:"
    ).replace("    value: mule_types.int64\n", "")
    sources = {"linear": source, "nonlinear": nonlinear, "boxed": boxed}
    for name, text in sources.items():
        path = OUTPUT / f"{name}.py"
        path.write_text(text)
        build(path, OUTPUT / f"{name}-mule", llvm_output=OUTPUT / f"{name}.ll")
        subprocess.run(
            [
                "clang",
                "-O3",
                "-S",
                "-emit-llvm",
                "-Wno-override-module",
                str(OUTPUT / f"{name}.ll"),
                "-o",
                str(OUTPUT / f"{name}.opt.ll"),
            ],
            check=True,
        )
    rust = OUTPUT / "classes-rust"
    subprocess.run(
        [
            "rustc",
            "--edition=2024",
            "-C",
            "opt-level=3",
            str(ROOT / "benchmarks/classes.rs"),
            "-o",
            str(rust),
        ],
        check=True,
    )
    subprocess.run(
        [
            "rustc",
            "--edition=2024",
            "-C",
            "opt-level=3",
            "--emit=llvm-ir",
            str(ROOT / "benchmarks/classes.rs"),
            "-o",
            str(OUTPUT / "classes-rust.ll"),
        ],
        check=True,
    )
    environment = dict(os.environ, PYTHONPATH=str(ROOT / "stdlib"))
    report = {
        "timestamp_utc": datetime.now(UTC).isoformat(),
        "platform": platform.platform(),
        "cpu_affinity": cpu,
        "cpu_model": next(
            line.split(":", 1)[1].strip()
            for line in Path("/proc/cpuinfo").read_text().splitlines()
            if line.startswith("model name")
        ),
        "python": sys.version,
        "clang": subprocess.check_output(
            ["clang", "--version"], text=True
        ).splitlines()[0],
        "rustc": subprocess.check_output(["rustc", "--version"], text=True).strip(),
        "settings": {
            "mule": "LLVM -O3, Rust primitives opt-level=3, speed3 safety0 signed i64, generic class binding guards",
            "rust": "opt-level=3, generic target, signed i64, no black_box",
            "cpython": "-S; same generated source; PYTHONPATH=stdlib annotation markers",
        },
        "measurement": "fresh process wall time, including startup/I/O/shutdown; build and uv startup excluded",
        "repeats": args.repeats,
        "warmups": args.warmups,
        "limit": args.limit,
        "source_sha256": {
            name: hashlib.sha256(text.encode()).hexdigest()
            for name, text in sources.items()
        },
        "rust_source_sha256": hashlib.sha256(
            (ROOT / "benchmarks/classes.rs").read_bytes()
        ).hexdigest(),
        "compiler_sha256": {
            str(path.relative_to(ROOT)): hashlib.sha256(path.read_bytes()).hexdigest()
            for path in sorted(
                [
                    *(ROOT / "mule").rglob("*.py"),
                    *(ROOT / "mule").rglob("*.rs"),
                    *(ROOT / "stdlib").rglob("*.py"),
                ]
            )
        },
        "profiles": {},
    }
    rng = random.Random(20261009)
    for profile in ("linear", "nonlinear"):
        commands = {
            "mule_native": [str(OUTPUT / f"{profile}-mule")],
            "rust": [str(rust), profile],
            "cpython": [sys.executable, "-S", str(OUTPUT / f"{profile}.py")],
        }
        if profile == "linear":
            commands["mule_boxed"] = [str(OUTPUT / "boxed-mule")]

        def expected(limit, profile=profile):
            return str(
                3 * max(limit, 0)
                if profile == "linear"
                else 8 * (max(limit, 0) // 2) + 6 * (max(limit, 0) % 2)
            )

        for limit in (-1, 0, 1, 2, 3, 10, 101):
            for command in commands.values():
                actual, _ = execute(command, limit, environment)
                if actual != expected(limit):
                    raise RuntimeError(f"{command}: {actual} != {expected(limit)}")
        samples = {name: [] for name in commands}
        print(
            f"{profile}: {args.limit} iterations, expected {expected(args.limit)}",
            flush=True,
        )
        for trial in range(args.warmups + args.repeats):
            order = list(commands)
            rng.shuffle(order)
            for name in order:
                actual, seconds = execute(commands[name], args.limit, environment)
                if actual != expected(args.limit):
                    raise RuntimeError(f"{name}: {actual} != {expected(args.limit)}")
                if trial >= args.warmups:
                    samples[name].append(seconds)
            print(
                f"  completed trial {trial + 1}/{args.warmups + args.repeats}",
                flush=True,
            )
        report["profiles"][profile] = {
            "expected": expected(args.limit),
            "commands": commands,
            "measurements": {
                name: {"seconds": values, "median_seconds": statistics.median(values)}
                for name, values in samples.items()
            },
        }
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(json.dumps(report, indent=2) + "\n")
        for name, values in samples.items():
            print(f"  {name}: {statistics.median(values):.6f}s median", flush=True)
    print(f"Results: {args.output}", flush=True)


if __name__ == "__main__":
    sys.path.insert(0, str(ROOT))
    main()
