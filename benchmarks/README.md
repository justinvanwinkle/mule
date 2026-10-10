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

From the repository root, with Clang, SBCL, and Rust installed:

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

Mule uses Clang `-O3`, its own runtime, tagged native integers, and GMP for bigint
overflow. It does not yet have language-level optimizations such as type inference
or direct-call specialization.
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
