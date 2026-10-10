"""Verify and benchmark LLVM i128/i256 against GMP mpz and mpn arithmetic."""

import argparse
import hashlib
import json
import os
import platform
import random
import statistics
import subprocess
from datetime import UTC, datetime
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
BACKENDS = ("llvm", "mpz", "mpn")


def llvm_source(bits):
    functions = []
    for name, instruction in (
        ("add", "add"),
        ("xor", "xor"),
        ("mul", "mul"),
        ("div", "udiv"),
    ):
        functions.append(f"""define void @llvm_{name}(ptr %out, ptr %lhs, ptr %rhs, i64 %count) {{
entry:
  %empty = icmp eq i64 %count, 0
  br i1 %empty, label %exit, label %loop
loop:
  %index = phi i64 [0, %entry], [%next, %loop]
  %ap = getelementptr i{bits}, ptr %lhs, i64 %index
  %bp = getelementptr i{bits}, ptr %rhs, i64 %index
  %rp = getelementptr i{bits}, ptr %out, i64 %index
  %a = load i{bits}, ptr %ap, align 8
  %b = load i{bits}, ptr %bp, align 8
  %r = {instruction} i{bits} %a, %b
  store i{bits} %r, ptr %rp, align 8
  %next = add i64 %index, 1
  %done = icmp eq i64 %next, %count
  br i1 %done, label %exit, label %loop
exit:
  ret void
}}
""")
    return "\n".join(functions)


def build(output, bits):
    source = output / "kernels.ll"
    source.write_text(llvm_source(bits))
    clang_flags = ["clang", "-O3", "-march=native", "-x", "ir", str(source)]
    subprocess.run([*clang_flags, "-c", "-o", str(output / "kernels.o")], check=True)
    subprocess.run([*clang_flags, "-S", "-o", str(output / "kernels.s")], check=True)
    subprocess.run(
        [
            "rustc",
            "--edition=2024",
            "-C",
            "opt-level=3",
            "-C",
            "target-cpu=native",
            "-C",
            "panic=abort",
            *(["--cfg", "width128"] if bits == 128 else []),
            "-C",
            f"link-arg={output / 'kernels.o'}",
            str(ROOT / "benchmarks/i256.rs"),
            "-o",
            str(output / "benchmark"),
        ],
        check=True,
    )


def inputs(case, count, bits):
    mask = (1 << bits) - 1
    rng = random.Random(256)
    pairs = []
    for _ in range(count):
        a = rng.getrandbits(bits) | (1 << (bits - 1))
        divisor_bits = bits // 2 if case == f"div{bits // 2}" else bits
        b = rng.getrandbits(divisor_bits) | (1 << (divisor_bits - 1))
        pairs.append((a, b))
    # Carry propagation, discarded high product bits, and zero XOR results.
    pairs[:3] = [
        (mask, pairs[0][1]),
        (1 << (bits - 1), pairs[1][1]),
        (pairs[2][1], pairs[2][1]),
    ]
    if case == f"div{bits // 2}":
        pairs[2] = (mask, (1 << (bits // 2)) - 1)
    return pairs


def expected(operation, a, b, bits):
    mask = (1 << bits) - 1
    if operation == "add":
        return (a + b) & mask
    if operation == "xor":
        return a ^ b
    if operation == "mul":
        return (a * b) & mask
    return a // b


def execute(output, backend, case, operation, rounds, destination=None):
    command = [
        str(output / "benchmark"),
        backend,
        operation,
        str(output / f"{case}.bin"),
        str(rounds),
    ]
    if destination:
        command.append(str(destination))
    result = subprocess.run(
        command, check=True, capture_output=True, text=True, timeout=120
    )
    elapsed, operations, checksum = map(int, result.stdout.split())
    return {
        "elapsed_ns": elapsed,
        "operations": operations,
        "checksum": checksum,
        "ns_per_operation": elapsed / operations,
    }


def version(command):
    return subprocess.run(
        command, check=True, capture_output=True, text=True
    ).stdout.splitlines()[0]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--bits", type=int, choices=(128, 256), default=256)
    parser.add_argument("--repeats", type=int, default=7)
    parser.add_argument("--warmups", type=int, default=1)
    parser.add_argument("--batch", type=int, default=1024)
    parser.add_argument("--sample-ms", type=float, default=150)
    parser.add_argument("--cpu", type=int)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    if args.repeats < 1 or args.warmups < 0 or args.batch < 3 or args.sample_ms <= 0:
        parser.error(
            "positive repeats/sample-ms, batch >= 3, and nonnegative warmups required"
        )
    allowed = os.sched_getaffinity(0)
    cpu = min(allowed) if args.cpu is None else args.cpu
    if cpu not in allowed:
        parser.error(f"CPU {cpu} is outside allowed affinity")
    bits = args.bits
    byte_count = bits // 8
    output = ROOT / f"build/benchmarks/i{bits}"
    cases = {
        "add": "add",
        "xor": "xor",
        "mul": "mul",
        f"div{bits // 2}": "div",
        f"div{bits}": "div",
    }
    output.mkdir(parents=True, exist_ok=True)
    build(output, bits)
    os.sched_setaffinity(0, {cpu})
    checksums = {}
    for case, operation in cases.items():
        pairs = inputs(case, args.batch, bits)
        (output / f"{case}.bin").write_bytes(
            b"".join(
                a.to_bytes(byte_count, "little") + b.to_bytes(byte_count, "little")
                for a, b in pairs
            )
        )
        reference = b"".join(
            expected(operation, a, b, bits).to_bytes(byte_count, "little")
            for a, b in pairs
        )
        for backend in BACKENDS:
            destination = output / "validation.bin"
            result = execute(output, backend, case, operation, 1, destination)
            actual = destination.read_bytes()
            if actual != reference:
                mismatch = next(
                    i
                    for i in range(args.batch)
                    if actual[i * byte_count : (i + 1) * byte_count]
                    != reference[i * byte_count : (i + 1) * byte_count]
                )
                raise RuntimeError(
                    f"Incorrect {backend} {case} result at pair {mismatch}: {pairs[mismatch]}"
                )
            checksums[backend, case] = result["checksum"]
    print(
        f"Verified {args.batch * len(cases) * len(BACKENDS):,} results against Python integers.",
        flush=True,
    )
    target_ns = args.sample_ms * 1_000_000
    rounds = {}
    samples = {(backend, case): [] for backend in BACKENDS for case in cases}
    for backend, case in samples:
        pilot = execute(output, backend, case, cases[case], 64)
        rounds[backend, case] = max(1, round(64 * target_ns / pilot["elapsed_ns"]))
        for _ in range(args.warmups):
            execute(output, backend, case, cases[case], rounds[backend, case])
    rng = random.Random(20261009)
    for repeat in range(args.repeats):
        order = list(samples)
        rng.shuffle(order)
        for backend, case in order:
            result = execute(output, backend, case, cases[case], rounds[backend, case])
            if result["checksum"] != checksums[backend, case]:
                raise RuntimeError(f"Timed checksum mismatch: {backend} {case}")
            samples[backend, case].append(result)
        print(f"Measured round {repeat + 1}/{args.repeats}.", flush=True)
    medians = {
        case: {
            backend: statistics.median(
                sample["ns_per_operation"] for sample in samples[backend, case]
            )
            for backend in BACKENDS
        }
        for case in cases
    }
    paths = [
        ROOT / "benchmarks/i256.py",
        ROOT / "benchmarks/i256.rs",
        output / "kernels.ll",
    ]
    report = {
        "recorded_at": datetime.now(UTC).isoformat(),
        "cpu": cpu,
        "cpu_info": version(["lscpu", "--extended=CPU,MODELNAME"]),
        "lscpu": subprocess.run(
            ["lscpu"], check=True, capture_output=True, text=True
        ).stdout,
        "platform": platform.platform(),
        "clang": version(["clang", "--version"]),
        "rust": version(["rustc", "--version"]),
        "gmp_version": version([str(output / "benchmark"), "version"]),
        "settings": {
            "clang": "-O3 -march=native",
            "bits": bits,
            "rust": "opt-level=3 target-cpu=native panic=abort",
            "batch": args.batch,
            "sample_target_ms": args.sample_ms,
            "repeats": args.repeats,
            "warmups": args.warmups,
        },
        "semantics": f"Unsigned {bits}-bit operands; add/mul modulo 2^{bits}; XOR; unsigned quotient.",
        "scope": "Hot-array batch throughput, native arithmetic only; not Mule object/runtime overhead or dependent-operation latency.",
        "gmp": f"mpz outputs preallocated for {bits * 2 + 64} bits and reused; mpn uses {bits // 64} limbs. mpn_mul_n computes {bits * 2} product bits; mpn_tdiv_qr also computes remainder.",
        "division": f"div{bits // 2}: full-width {bits}/{bits // 2}; div{bits}: full-width {bits}/{bits} (quotient 0 or 1).",
        "excluded": "Build, process startup, input setup, mpz initialization, validation, checksum, output conversion, teardown.",
        "source_sha256": {
            str(path.relative_to(ROOT)): hashlib.sha256(path.read_bytes()).hexdigest()
            for path in paths
        },
        "input_sha256": {
            case: hashlib.sha256((output / f"{case}.bin").read_bytes()).hexdigest()
            for case in cases
        },
        "medians_ns_per_operation": medians,
        "samples": {
            case: {backend: samples[backend, case] for backend in BACKENDS}
            for case in cases
        },
    }
    report_path = args.output or output / "results.json"
    report_path.parent.mkdir(parents=True, exist_ok=True)
    report_path.write_text(json.dumps(report, indent=2) + "\n")
    print("Operation      LLVM ns/op   GMP mpz ns/op   GMP mpn ns/op")
    for case, times in medians.items():
        print(
            f"{case:12} {times['llvm']:11.2f} {times['mpz']:15.2f} {times['mpn']:15.2f}"
        )
    print(f"Results: {report_path}")


if __name__ == "__main__":
    main()
