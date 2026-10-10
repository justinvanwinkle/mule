"""Build, verify, and time the four implementations without external packages."""

import argparse
import hashlib
import json
import math
import os
import platform
import random
import statistics
import subprocess
import sys
import sysconfig
import time
from datetime import UTC, datetime
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
OUTPUT = ROOT / "build/benchmarks"
PROFILES = {
    "startup": (1, 0, 0),
    "search": (12, 8, 0),
    "integers": (1, 0, 80000),
    "combined": (12, 8, 80000),
}
QUEENS = {1: 1, 2: 0, 3: 0, 4: 2, 8: 92, 10: 724, 12: 14200}


def reference_checksum(iterations):
    """Verify primality via a sieve and GCD via the host's math implementation."""
    primes = bytearray(b"\1") * 100000
    primes[0] = primes[1] = 0
    for divisor in range(2, math.isqrt(99999) + 1):
        if primes[divisor]:
            for multiple in range(divisor * divisor, 100000, divisor):
                primes[multiple] = 0
    state = 123456789
    checksum = 0
    for _ in range(iterations):
        state = (state * 1664525 + 1013904223) & 2147483647
        candidate = 10000 + state % 90000
        common = math.gcd(candidate, (state >> 8) % 10000 + 1)
        contribution = candidate + common if primes[candidate] else candidate ^ common
        checksum = (checksum * 33 + contribution) % 1000000007
    return checksum


def version(command):
    result = subprocess.run(command, check=True, capture_output=True, text=True)
    return result.stdout.strip().splitlines()[0]


def build_programs():
    from mule.compiler import build

    OUTPUT.mkdir(parents=True, exist_ok=True)
    build(
        ROOT / "benchmarks/workload.py",
        OUTPUT / "workload-mule",
        llvm_output=OUTPUT / "workload-mule.ll",
    )
    subprocess.run(
        [
            "rustc",
            "--edition=2024",
            "-C",
            "opt-level=3",
            "-C",
            "target-cpu=native",
            str(ROOT / "benchmarks/workload.rs"),
            "-o",
            str(OUTPUT / "workload-rust"),
        ],
        check=True,
        cwd=ROOT,
    )
    subprocess.run(
        [
            "sbcl",
            "--noinform",
            "--non-interactive",
            "--no-sysinit",
            "--no-userinit",
            "--load",
            "benchmarks/build-lisp.lisp",
        ],
        check=True,
        cwd=ROOT,
    )


def execute(command, parameters):
    payload = "".join(f"{value}\n" for value in parameters)
    start = time.perf_counter_ns()
    result = subprocess.run(
        command,
        input=payload,
        text=True,
        capture_output=True,
        check=True,
        timeout=180,
        cwd=ROOT,
    )
    elapsed = (time.perf_counter_ns() - start) / 1e9
    if result.stderr:
        raise RuntimeError(f"{command}: unexpected stderr: {result.stderr}")
    return result.stdout.strip(), elapsed


def expected_output(parameters, checksums):
    n, repetitions, iterations = parameters
    return f"{QUEENS[n] * repetitions} {checksums[iterations]}"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repeats", type=int, default=7)
    parser.add_argument("--warmups", type=int, default=1)
    parser.add_argument("--cpu", type=int)
    parser.add_argument("--skip-build", action="store_true")
    args = parser.parse_args()
    if args.repeats < 1 or args.warmups < 0:
        parser.error("repeats must be positive and warmups nonnegative")
    allowed = os.sched_getaffinity(0)
    cpu = min(allowed) if args.cpu is None else args.cpu
    if cpu not in allowed:
        parser.error(f"CPU {cpu} is outside the allowed affinity: {sorted(allowed)}")
    os.sched_setaffinity(0, {cpu})
    if not args.skip_build:
        build_programs()
    commands = {
        "mule": [str(OUTPUT / "workload-mule")],
        "cpython": [sys.executable, "-I", "-S", str(ROOT / "benchmarks/workload.py")],
        "sbcl": [str(OUTPUT / "workload-sbcl")],
        "rust": [str(OUTPUT / "workload-rust")],
    }
    cases = [(n, 1, 1000) for n in QUEENS] + [(4, 3, 37), (1, 0, 0)]
    sizes = {case[2] for case in [*cases, *PROFILES.values()]}
    checksums = {size: reference_checksum(size) for size in sizes}
    for parameters in cases:
        expected = expected_output(parameters, checksums)
        for name, command in commands.items():
            actual, _ = execute(command, parameters)
            if actual != expected:
                raise RuntimeError(f"{name} {parameters}: {actual!r} != {expected!r}")
    print(f"Verified {len(cases)} cases across all four implementations.", flush=True)

    report = {
        "timestamp_utc": datetime.now(UTC).isoformat(),
        "platform": platform.platform(),
        "cpu_model": next(
            line.split(":", 1)[1].strip()
            for line in Path("/proc/cpuinfo").read_text().splitlines()
            if line.startswith("model name")
        ),
        "cpu_affinity": cpu,
        "versions": {
            "cpython": sys.version,
            "clang": version(["clang", "--version"]),
            "sbcl": version(["sbcl", "--version"]),
            "rustc": version(["rustc", "--version"]),
        },
        "python_jit": {
            "available": getattr(
                getattr(sys, "_jit", None), "is_available", lambda: False
            )(),
            "enabled": getattr(
                getattr(sys, "_jit", None), "is_enabled", lambda: False
            )(),
        },
        "gil_disabled": bool(sysconfig.get_config_var("Py_GIL_DISABLED")),
        "compiler_settings": {
            "mule": "current driver: clang -O3, boxed CPython objects",
            "cpython": "-I -S, same workload.py, normal bytecode interpreter",
            "sbcl": "compiled executable; speed 3, safety 1; fixnum declarations",
            "rust": "edition 2024, opt-level=3, target-cpu=native; u64 integers",
        },
        "source_sha256": {
            filename: hashlib.sha256(
                (ROOT / "benchmarks" / filename).read_bytes()
            ).hexdigest()
            for filename in (
                "workload.py",
                "workload.lisp",
                "workload.rs",
                "build-lisp.lisp",
                "run.py",
            )
        },
        "commands": commands,
        "repeats": args.repeats,
        "warmups": args.warmups,
        "verification_cases": len(cases),
        "profiles": {},
    }
    rng = random.Random(20261009)
    for profile, parameters in PROFILES.items():
        expected = expected_output(parameters, checksums)
        samples = {name: [] for name in commands}
        print(f"Timing {profile}: {parameters}, expected {expected}", flush=True)
        for trial in range(args.warmups + args.repeats):
            order = list(commands)
            rng.shuffle(order)
            for name in order:
                actual, seconds = execute(commands[name], parameters)
                if actual != expected:
                    raise RuntimeError(f"{name}/{profile}: {actual!r} != {expected!r}")
                if trial >= args.warmups:
                    samples[name].append(seconds)
            if trial >= args.warmups:
                print(f"  sample {trial - args.warmups + 1}/{args.repeats}", flush=True)
        measurements = {
            name: {
                "seconds": values,
                "median_seconds": statistics.median(values),
                "min_seconds": min(values),
                "max_seconds": max(values),
            }
            for name, values in samples.items()
        }
        report["profiles"][profile] = {
            "parameters": parameters,
            "expected_output": expected,
            "measurements": measurements,
        }
        for name, measurement in measurements.items():
            print(
                f"  {name:8}: {measurement['median_seconds']:.6f}s median", flush=True
            )
        (OUTPUT / "results.json").write_text(json.dumps(report, indent=2) + "\n")
    print(f"Raw results: {OUTPUT / 'results.json'}", flush=True)


if __name__ == "__main__":
    # The project is intentionally run from its checkout rather than installed.
    sys.path.insert(0, str(ROOT))
    main()
