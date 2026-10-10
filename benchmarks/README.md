# Native compiler benchmark

The same deterministic scalar workload is implemented in
[Python](workload.py), [Common Lisp](workload.lisp), and [Rust](workload.rs).
The Python source runs both through Mule's native compiler and directly through
CPython. The native Mule build uses the approved GMP library; no third-party
Python packages are required.

## Workload

The recursive kernel counts every N-Queens solution using bit masks, recursive
backtracking, diagonal constraints, and extraction of the lowest available bit.
It does not use memoization, symmetry reduction, or parallel search.

The integer kernel updates a 31-bit linear congruential generator, derives a
candidate between 10,000 and 99,999, computes a Euclidean GCD, tests primality
using trial divisors of the form `6k ± 1`, and folds the result into a modular
checksum. State and checksum dependencies prevent iteration elimination.

Each program reads three lines from stdin: board size, search repetitions, and
integer iterations. It prints the total number of solutions and integer checksum.
Inputs are supplied at runtime rather than compiled into the binaries. Rust
additionally uses `black_box` at each repeated search root to prevent hoisting
identical searches out of the loop. All benchmark intermediates fit machine
integers; Rust uses `u64`, and SBCL has explicit bounded integer declarations.
The benchmark does not compare arbitrary-precision overflow behavior.

The combined profile uses a 12×12 board searched eight times and 80,000 integer
iterations. Its result is:

```text
113600 974617263
```

## Run the comparison

From the repository root, with Clang, SBCL, Rust, and GMP installed:

```sh
uv run python benchmarks/run.py
```

This builds Mule and Rust executables and an SBCL executable image under
`build/benchmarks/`. It verifies nine small/reference cases across all four
implementations, checks every timed output, and measures these profiles:

| Profile | Board | Search repetitions | Integer iterations |
| --- | ---: | ---: | ---: |
| Startup | 1 | 0 | 0 |
| Search | 12 | 8 | 0 |
| Integers | 1 | 0 | 80,000 |
| Combined | 12 | 8 | 80,000 |

Known N-Queens solution counts verify search output. An independent sieve and
`math.gcd` implementation verify the integer checksum. The default is one warmup
and seven measured fresh processes per implementation and profile, with shuffled
execution order. The harness pins all child processes to one allowed logical CPU
(the lowest CPU number by default); use `--cpu N` to select another.

Timings are process wall time, including startup, stdin, output, and shutdown.
Compilation and reference verification are excluded. The startup profile exposes
runtime initialization cost separately; no startup estimate is subtracted from
workload timings. CPython runs the exact same source with `-I -S` to disable user
environment and site initialization, matching Mule's lack of site initialization.
Warmups warm filesystem/runtime pages; each measured CPython process has its own
interpreter specialization state.

Mule uses Clang `-O3`, its Rust runtime at `opt-level=3`, interprocedural type/range
analysis, specialized internal entries, inline integer operations, and guarded
direct calls. Its linker discards unused sections and strips debug information.
Integer overflow
uses GMP. Global lookups remain dynamic, and unknown types retain runtime checks.
SBCL uses compiled native functions with `(speed 3) (safety 1)` and integer type
declarations. Rust uses `-C opt-level=3 -C target-cpu=native`. These settings
compare the current implementations, not equivalent optimization pipelines or
the eventual performance ceiling of Mule.

Raw samples, versions, CPU information, settings, source hashes, and expected
outputs are saved in `build/benchmarks/results.json`. Use `--skip-build` to reuse
the existing executables, `--repeats N` to change sample count, and `--warmups N`
to change warmup count. [Recorded results](results.md) describe this run.

## Run an individual implementation

After building through the comparison harness:

```sh
printf '12\n8\n80000\n' | ./build/benchmarks/workload-mule
printf '12\n8\n80000\n' | uv run python -I -S benchmarks/workload.py
printf '12\n8\n80000\n' | ./build/benchmarks/workload-sbcl
printf '12\n8\n80000\n' | ./build/benchmarks/workload-rust
```

Manual builds:

```sh
uv run python -m mule benchmarks/workload.py -o build/benchmarks/workload-mule
sbcl --noinform --no-sysinit --no-userinit --script benchmarks/build-lisp.lisp
rustc --edition=2024 -C opt-level=3 -C target-cpu=native benchmarks/workload.rs -o build/benchmarks/workload-rust
```

SBCL's executable includes its Lisp image. Mule dynamically links GMP and the
platform C/math services and has no Python runtime dependency. Binary size does
not directly compare total deployment size.

## Class counter

[`examples/class_counter.py`](../examples/class_counter.py) performs 200 million
native method calls that update a fixed instance field. Numeric parameters, the
loop index, and the return value use `mule.int64` annotations; the instance field
still uses the current generic object storage. Build and run it with:

```sh
uv run python -m mule examples/class_counter.py -o build/class_counter
time build/class_counter
time env PYTHONPATH=stdlib uv run python -S examples/class_counter.py
```

The expected output is `600000000`. The host run uses `stdlib/mule.py` for the
annotation markers; its numeric annotations do not change Python's execution.
[Current samples](results-classes-int64.json) compare Mule's `int32` and `int64`
annotations with the same `int64` source run by standard Python. They use one
warmup, seven fresh processes per implementation, shuffled order, and one pinned
CPU. Process wall time includes startup and shutdown but excludes compilation
and `uv` startup. This measures the current boxed class backend, not the proposed
native struct representation.

The current medians are 5.616 seconds for Mule `int32`, 5.907 seconds for Mule
`int64`, and 6.122 seconds for Python 3.14. The `int64` annotations make this
version 5.2% slower than `int32`; native typed fields remain implementation work.

[Earlier samples](results-classes.json) measured the unannotated example with
two million iterations and output `6000000`. Mule's median was 60.03 ms and
Python 3.14's was 67.67 ms, a 1.13x speedup; those durations do not describe the
current 200-million-iteration program.

## LLVM i128 and i256 versus GMP

Run the native arithmetic comparison with:

```sh
uv run python benchmarks/i256.py
uv run python benchmarks/i256.py --bits 128
```

[`i256.py`](i256.py) generates LLVM batch kernels, links them to the
[`Rust harness`](i256.rs), and compares LLVM `i256` against GMP's high-level
`mpz` and low-level `mpn` APIs. `--bits 128` selects LLVM `i128` and two-limb
GMP operands using the same harness. Clang uses `-O3 -march=native`; Rust uses
`opt-level=3 target-cpu=native`. This measures the numeric implementations
directly, without Mule's object or runtime overhead.

Each batch contains 1,024 deterministic unsigned input pairs. Addition and
multiplication return their low 256 bits; XOR returns 256 bits; division returns
the unsigned quotient. Division has separate 256/128 and 256/256 datasets.
Both datasets use full-width operands, so the 256/256 quotient is only zero or
one. The latter is a cheap division case, not representative of every division.

Every result is checked against independent Python integer arithmetic before
timing. Timings cover repeated batch arithmetic, array access, loop overhead,
and GMP calls. They exclude builds, process startup, input setup, GMP object
initialization, checksums, result conversion, and teardown. Each process is
pinned to one CPU; the default is one warmup and seven shuffled measured
samples, targeting 150 ms per sample. Results are median nanoseconds per result,
measuring throughput rather than dependent-operation latency.

GMP `mpz` results reuse preallocated 576-bit storage. Addition and multiplication
include reduction modulo 2²⁵⁶. `mpn` operates on four 64-bit limbs with reusable
outputs. Its `mpn_mul_n` computes the complete 512-bit product before truncation,
and `mpn_tdiv_qr` also computes the remainder; LLVM can omit this extra work.
These are comparisons of the selected public APIs with equivalent final results.

Generated LLVM, assembly, inputs, and default results are retained under
`build/benchmarks/i256/`. [Recorded results](results-i256.json) include raw
samples, versions, CPU information, settings, and source/input hashes.
Use `--cpu`, `--repeats`, `--warmups`, `--batch`, or `--sample-ms` to change the
run, and `--output PATH` to save its report elsewhere.

The 128-bit run uses addition/multiplication modulo 2¹²⁸, 128-bit XOR, and
separate 128/64 and 128/128 division datasets. Full-width 128/128 division also
has quotients restricted to zero or one. GMP `mpz` storage is preallocated for
320 bits, and `mpn_mul_n` computes a 256-bit product before truncation. Generated
artifacts are under `build/benchmarks/i128/`;
[recorded 128-bit results](results-i128.json) include the same verification and
timing metadata as the 256-bit run.
