# Recorded benchmark results

Measured 2026-10-10T00:26:31.515757+00:00 on AMD Ryzen 9 9955HX 16-Core Processor, Linux x86-64.
All programs were pinned to logical CPU 0. Each profile
had one warmup and seven measured fresh processes, in shuffled order.
Timings include process startup and shutdown; compilation is excluded.
See [methodology and run commands](README.md).

The workload combines recursive bitboard N-Queens search with a deterministic
LCG, Euclidean GCD, trial-division primality testing, and modular checksum.
Combined input is `12`, `8`, `80000`; all four implementations produce
`113600 974617263`. All nine reference cases and every timed output matched.

## Median wall time

All times below are milliseconds; lower is faster.

| Implementation | Startup | Search | Integers | Combined |
| --- | ---: | ---: | ---: | ---: |
| Mule (Clang -O3) | 6.546 | 3418.320 | 170.289 | 3580.038 |
| CPython 3.14 | 5.999 | 973.256 | 50.797 | 1017.086 |
| Common Lisp / SBCL | 2.842 | 46.869 | 7.829 | 50.558 |
| Rust (-O3) | 1.487 | 38.666 | 5.568 | 42.224 |

## Effect of -O3 on Mule

The completed earlier run used Clang `-O0`. Switching the compiler driver to
`-O3` reduced the combined median from 5.233950 s to 3.580038 s:
**1.46× faster**, or 31.6% less elapsed time.
This is a comparison of separate repeated runs, not paired simultaneous samples.

Mule at `-O3` is still **3.52× slower
than CPython**, 70.8× slower than SBCL,
and 84.8× slower than Rust on this
combined workload.

Mule continues to use boxed objects, dynamic lookup/calls, and reference
management through CPython C APIs. Clang optimization does not provide Python
semantic type inference, unboxing, or direct-call specialization. SBCL's bounded
integer declarations and Rust's `u64` operations let those implementations use
machine arithmetic. The timing comparison establishes the current gap; it does
not isolate the cost of each of these mechanisms.

## Combined sample range

Milliseconds across the seven measured processes:

| Implementation | Minimum | Median | Maximum |
| --- | ---: | ---: | ---: |
| mule | 3555.970 | 3580.038 | 3640.464 |
| cpython | 1013.281 | 1017.086 | 1033.989 |
| sbcl | 50.463 | 50.558 | 50.910 |
| rust | 41.958 | 42.224 | 42.434 |

## Builds and raw data

- Mule: current native driver, Clang `-O3`, CPython object runtime.
- CPython: 3.14.7, same `workload.py`, `-I -S`, JIT available but disabled.
- SBCL: SBCL 2.6.4.debian, compiled executable image, speed 3 / safety 1,
  bounded integer declarations in the arithmetic/search kernels.
- Rust: rustc 1.97.1 (8bab26f4f 2026-07-14), edition 2024, `opt-level=3`, `target-cpu=native`.
- Clang: Debian clang version 21.1.8 (13).

[Optimized raw samples and metadata](results.json) and
[original -O0 raw samples](results-o0.json) include every sample, parameters,
expected output, source hashes, commands, and runtime settings. Compiler tests
also passed after the `-O3` change: 838 tests.
