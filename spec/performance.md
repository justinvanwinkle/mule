# Compiled program performance

Status: measurement and optimization work in progress. The target is at least
10 times CPython's speed for compiled programs; current measurements do not meet
that target.

## Reproducible comparisons

`tools/bench_native.py` builds programs at LLVM and Rust optimization level 3
with full LTO, checks their stdout against CPython 3.14, then alternates direct
native and Python executions. Compilation is measured separately and excluded
from program timings. Timings include process startup. Inputs come from command
line arguments rather than compile-time constants. JSON reports contain input,
output, source and executable hashes, individual samples, medians, Python
version, CPU affinity, and compiler revision/diff identity.

```sh
XDG_CACHE_HOME=/tmp/mule-progress-cache uv run python -m tools.bench_native \
  integer_loop float_loop function_calls list_index attributes \
  --directory /tmp/mule-performance/baseline --samples 7
```

Use separate directories for successive builds. `--reuse` times an existing
executable; the current compiler revision does not establish the provenance of
that executable. `--report` chooses a separate JSON path when remeasuring it.
`--baseline-directory` adds an earlier native executable to the same alternating
sample sequence, validates its output, and records its hash and relative speed.
Increase `--count` until process startup is a small fraction of the measured
time. Do not run simultaneous timing jobs or include builds in elapsed program
runtime. A timeout is recorded as a timeout, without a validated speedup.

The `nbody` case uses the unmodified base Python program from Benchmarks Game;
input 1000 must also match `1000_out` byte for byte. `sha3` measures the compiled
Mule implementation against CPython's hashing implementation. These broader
cases supplement the small diagnostic kernels; results from a numeric loop
alone do not establish application performance.

## Initial observations, 2026-10-14

Host: AMD Ryzen 9 9955HX, Linux x86-64, CPython 3.14.7 free-threading build,
Clang 21.1.8.
The initial three-sample diagnostic run measured the following whole-process
medians. Inputs and startup costs differ, so compare implementations within a
row rather than comparing absolute times between rows.
These historical builds used LTO for the main unit and thin-LTO Rust bitcode;
library units were already machine code. The build corrections below enable
full LTO across all generated units and the Rust primitives.

| Program | Input | CPython seconds | Mule seconds | CPython / Mule |
| --- | ---: | ---: | ---: | ---: |
| Integer loop | 1,000,000 | 0.0290 | 0.00948 | 3.06 |
| Float loop | 1,000,000 | 0.0341 | 0.111 | 0.307 |
| Function calls | 500,000 | 0.0242 | 0.0206 | 1.18 |
| List indexing | 1,000,000 | 0.0315 | 0.0197 | 1.60 |
| Instance attributes | 250,000 | 0.0216 | 0.170 | 0.127 |

Ratios greater than one mean Mule is faster. These are exploratory measurements,
not performance guarantees or optimized application benchmarks.

Callgrind on the attribute case shows substantial cost in string hashing,
text conversion/allocation, class lookup, and ownership operations. The initial
float loop selected generic numeric object paths and repeatedly allocated
literal and result boxes. Proven float templates, pooled literals, and guarded
result box reuse are documented in [native lowering](native-lowering.md).
They retain safe arithmetic and observable aliases; larger representation and
dispatch improvements remain necessary.

After float specialization, constant pooling, guarded result-box reuse,
ownership transfer, unboxed literals and intermediates, and complete
cross-language LTO, a nine-sample comparison
pinned to CPU 6 at input 10,000,000 measured:

| Runtime | Median seconds |
| --- | ---: |
| CPython | 0.2182 |
| Mule before this change | 1.0170 |
| Mule after this change | 0.1444 |

This is a 7.04-times improvement over Mule's earlier executable and 1.51 times
CPython's speed. Exact output was verified for every sample.

Inlining the inactive safepoint poll further removes native call overhead from
entries and loops, while retaining the acquire flag read and active collection
handshake. A nine-sample comparison pinned to CPU 6 with 10,000,000 integer-loop
iterations measured CPython at 0.18429 seconds, the original Mule executable at
0.02215 seconds, and the new Mule executable at 0.01787 seconds. This case reaches
10.32 times CPython's speed and improves 1.24 times over the original Mule build
before enabling complete cross-language LTO. With the final build settings,
the same input measured CPython at 0.18593 seconds and Mule at 0.01903 seconds,
or 9.77 times CPython's speed. Full LTO modestly changed this case's startup and
code layout; it remains enabled for the whole compiled implementation.
At 1,000,000 iterations, startup has a greater effect on whole-process timings;
the larger-input result does not establish a 10-times guarantee for every input
or other programs.

The unmodified nbody program at input 1000 matched the upstream reference, but
took 0.303 seconds against CPython's 0.0166 seconds in a three-sample exploratory
run. Guarded list unpacking reduced the same nbody workload to 0.0511 seconds,
5.84 times its earlier Mule speed, while remaining 3.21 times slower than
CPython in a five-sample comparison. The optimization snapshots list elements
before target setters execute and preserves subclass iteration overrides.

SHA3-256 over 4 MiB matched CPython's digest but took 19.29 seconds against
0.0222 seconds in a three-sample run: approximately 868 times slower. The SHA3
program imports `_sha3` directly in both implementations to measure one algorithm
without building all algorithms exposed by `hashlib`. Its profile includes
generic numeric calls, GMP integer allocation/operations, name lookup, and
ownership work. Native unsigned-word representations and general dispatch
specialization remain unresolved compiler work; replacing the algorithm with a
Rust implementation is outside the language/runtime boundary.

The build now passes `-flto=full` for every optimized generated unit and
`-C linker-plugin-lto -C lto=fat` for the Rust runtime. Previously, library
machine-code objects blocked optimization into compiled protocols, while thin
Rust bitcode blocked the full-LTO main unit from eliminating even `mule_none`.
Integration probes now verify that calls disappear in native machine code
across both boundaries. These settings expose optimization opportunities;
they do not establish a 10-times speedup for every program.

```sh
valgrind --tool=callgrind --callgrind-out-file=/tmp/attributes.callgrind \
  /tmp/mule-performance/baseline/attributes 10000
callgrind_annotate --inclusive=no /tmp/attributes.callgrind
```

Keep profiling separate from timing: instrumentation substantially changes
runtime. Check generated LLVM and native code alongside measurements when
claiming that a compiler transformation removed dispatch or allocation.
