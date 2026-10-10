# Recorded benchmark results

Measured 2026-10-10T01:27:02.210470+00:00 on AMD Ryzen 9 9955HX 16-Core Processor, Linux x86-64.
All programs were pinned to logical CPU 0. Each profile used
one warmup and seven measured fresh processes in shuffled order. Timings include
startup, stdin, output, and shutdown; compilation is excluded.
See [methodology and run commands](README.md).

The combined workload runs recursive bitboard N-Queens search eight times on a
12x12 board and 80,000 iterations of LCG, GCD, primality testing, and checksum
arithmetic. All four implementations produced `113600 974617263`.
All nine reference cases and every timed output matched.

## Median wall time

Milliseconds; lower is faster.

| Implementation | Startup | Search | Integers | Combined |
| --- | ---: | ---: | ---: | ---: |
| Mule | 1.930 | 224.642 | 14.153 | 235.975 |
| CPython 3.14 | 6.277 | 977.052 | 51.373 | 1017.498 |
| Common Lisp / SBCL | 3.053 | 46.631 | 7.590 | 50.969 |
| Rust | 1.898 | 38.463 | 5.469 | 42.091 |

Mule is **4.31x faster than CPython** on the combined
workload, and remains 4.63x slower than SBCL and
5.61x slower than Rust.

## Change from the initial Rust runtime

The [pre-optimization Rust runtime baseline](results-rust-runtime-baseline.json)
measured 1.897354 seconds for the combined workload. The current run measured
0.235975 seconds: **8.04x faster**, or 87.6% less elapsed time.
These are separate repeated runs on the same workload and machine, not paired
simultaneous samples. The source hashes and raw samples are retained for both.

Mule now performs fixed-point type/range analysis across call sites, emits
specialized internal function entries, and generates native integer operations.
Unknown call targets and types retain guarded general paths. GMP handles bigint
overflow; the runtime borrows integer inputs, moves results into objects, and
reuses bigint storage when ownership permits. These timings do not isolate the
contribution of each change. SBCL's explicit bounded integer declarations and
Rust's `u64` operations still provide stronger static information than Mule's
current inferred/guarded contexts.

## Combined sample range

Milliseconds across seven measured processes:

| Implementation | Minimum | Median | Maximum |
| --- | ---: | ---: | ---: |
| Mule | 235.377 | 235.975 | 236.926 |
| CPython 3.14 | 1014.210 | 1017.498 | 1027.034 |
| Common Lisp / SBCL | 50.554 | 50.969 | 51.105 |
| Rust | 41.815 | 42.091 | 42.716 |

## Factorial and bigint reuse

The current `examples/factorial.py` computes 1500! 1,001 times. Seven shuffled
fresh-process samples, after one warmup, pinned to CPU 0 measured:

| Runtime | Median seconds |
| --- | ---: |
| Before GMP changes | 0.271663 |
| Current | 0.108070 |

This is **2.51x faster**. Both executables use the same current LLVM lowering
and optimized link settings. The baseline uses the prior committed Rust numeric
runtime with the new reuse ABI delegated to its original allocating operation;
the current runtime borrows inputs, moves results, and reuses bigint storage.
The source is unchanged between the two builds. The program prints nothing, so
native value/alias tests separately establish arithmetic correctness.
[Raw factorial samples and source/runtime hashes](results-factorial-gmp.json)
record this comparison. The rebuilt current binary is 547,456 bytes (535 KiB),
with GMP dynamically linked; dead section removal and debug stripping reduced
it from approximately 5.1 MiB.

## Builds and raw data

- Mule: Clang `-O3`, Rust runtime `opt-level=3`, section garbage collection,
  stripped debug sections, independent runtime, dynamically linked GMP.
- CPython: 3.14.7, same source with `-I -S`, JIT available but disabled.
- SBCL: SBCL 2.6.4.debian, compiled image, speed 3 / safety 1.
- Rust: rustc 1.97.1 (8bab26f4f 2026-07-14), edition 2024, `opt-level=3`, `target-cpu=native`.
- Clang: Debian clang version 21.1.8 (13).

[Current raw samples and metadata](results.json) contain settings, all samples,
expected outputs, and hashes of benchmark and compiler/runtime sources.
Historical [CPython-linked -O3](results-o3-cpython-runtime.json) and
[-O0](results-o0.json) data describe the replaced implementation; they are not
results for the current independent runtime. The full suite passed: **1,039 tests**.
