# Compiled program performance

Status: measurement and optimization work in progress. The target is at least
10 times CPython's speed for compiled programs; current measurements do not meet
that target.

## Design goal

Every operation is to be as efficient as it theoretically could be with the
available technology: given all the information derivable from the program,
the compiler emits the most specialized, most efficient machine code. The
reference for each operation is the cost its information permits. A tuple
comparison whose elements are known fixnums is an inline integer compare; a
call to a module function whose binding is never reassigned is a direct call;
a value that provably does not escape carries no reference-count traffic.
Measured instruction counts above that reference are defects in the compiler's
analyses or lowering, not properties of the workload.

Specialization is derived from the program through the compiler's analyses,
following SBCL: known functions and direct calls, type inference and
declarations, constant bindings, ownership and escape facts, and
representation selection. Per-type fast paths inside generic runtime primitives
are not the mechanism: they hide missing compiler work and do not transfer to
other programs or types. They may be added only once an operation is already as
specialized as its information allows and a measured remainder justifies them.

## Reproducible comparisons

The new [standalone performance suite](../perf/README.md) lives in `perf/suite/`.
Its standard-library-only harness runs as `python -m perf.suite.perfutils`, with
configurable interpreter/build commands, one implementation per case,
separate kernel/preparation/process timings, correctness validation, and offline
HTML/SVG reports. The initial cases cover integer traversal, graph shortest
paths, access-log aggregation, and in-process HTTP parsing/routing. Socket/server
load tests and memory measurement are not implemented in this first set.

`tools/bench_native.py` builds programs at LLVM and Rust optimization level 3,
with full LTO enabled separately by `--lto`, checks their stdout against CPython 3.14, then alternates direct
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

## Parser construction staging, 2026-10-19

The Python-hosted parser and the native parser remain separate measurements.
This experiment compares two native executables built at LLVM `-O3`, with the
Rust runtime at level 3 and no LTO, on the Intel Core i7-1370P host. Both use
the same preloaded source strings and run on CPU 0. Imports, source-file reads
and compilation are outside the parser timer. Five samples per executable and
input group produce these median times for one complete pass over the group:

| Input group | Files | Before seconds | After seconds | Before / after |
| --- | ---: | ---: | ---: | ---: |
| Small-program corpus | 44 | 0.257052 | 0.200906 | 1.279 |
| Parser modules | 7 | 1.024453 | 0.744841 | 1.375 |
| Compiler modules | 47 | 12.952870 | 9.677561 | 1.338 |

The baseline source is revision `da5316f2cd4ebe1028e2bdcc2bebc972316349a1`.
Both isolated source snapshots include the same globals-wrapper cache
initialization-order repair described in `compiled-modules.md`. The changed
snapshot adds only positional binding for keyword-free `**kwargs` calls,
binding-derived collecting-parameter types, and dataclass initialization
metadata staging. Unrelated concurrent compiler changes were excluded. Runtime
order reverses for the middle input group; samples within each executable are
consecutive. These are one-host results, not an SBCL parser comparison.

The installed dataclass initializer still iterates a per-class initialization
plan and invokes descriptor-aware setters. This is not full known-constructor
specialization: ordinary class/layout facts, direct initializer target proofs,
and ownership analysis remain needed to approach SBCL's specialized construction
paths. Source identifiers, raw samples and executable hashes for this local
experiment are recorded in `/tmp/mule-constructor-measurement.json`; the
temporary before/after source trees and benchmark harness are under `/tmp/`.

Native traversal of all 98 input ASTs matched Python-hosted Mule for node kinds,
preorder traversal and source spans. This comparison checks those properties,
not complete AST payload equivalence or complete Python parser conformance.

Callgrind collection around one warmed parse of `mule/parser/core.py` recorded
905,726,362 instructions before and 644,946,477 after, a 28.8% reduction. Imports
and the caller's final destruction of the returned tree are outside that
collection. Both parses entered the dataclass initializer 6,845 times; calls
from that entry into the general binder fell from 6,845 to 123. The initializer
and its callees still account for 57.4% of the changed parse's instructions.
`object.__setattr__` and its callees account for 24.2%, with the same 14,824 calls
before and after. Inclusive regions overlap and cannot be added. These counts
support continuing with ordinary-class constructor and descriptor-aware field
specialization rather than attributing the remaining cost to cursor arithmetic.

## Guarded unbound method calls, 2026-10-19

Resolved class-qualified calls such as `property.__set__(descriptor, instance,
value)` now use guarded internal entries as bound method calls already did.
The helper chooses the receiver shape once and checks only candidates whose
signature accepts that shape. Attribute resolution remains observable; the
guard checks the actual resulting callable and passes its actual namespace,
closure and defaults. This is invocation specialization, not direct slot access.

Both controlled builds include the preceding constructor staging and the
worker-local type-derivation mode repair described in `compiler-gaps.md`.
Only guarded unbound-call lowering and its shape-partitioned dispatch differ.
The input sets, five-sample methodology, CPU affinity, `-O3` and no-LTO settings
match the experiment above. Median seconds per complete input pass are:

| Input group | Before seconds | After seconds | Before / after |
| --- | ---: | ---: | ---: |
| Small-program corpus | 0.200366 | 0.198532 | 1.009 |
| Parser modules | 0.764915 | 0.751739 | 1.018 |
| Compiler modules | 9.732363 | 9.370380 | 1.039 |

A separate one-million-call `Base.step(instance, value)` probe produced the
validated result 1,000,000 in every run. Its five alternating process samples
had kernel medians of 0.171629 seconds before and 0.160023 seconds after
(1.073 times as fast). This isolates one invocation shape, not a whole parser
or SBCL comparison. GDB also confirmed internal-body entry through a real
member-descriptor assignment chain, and native AST traversal still matched the
hosted parser across all 98 inputs for node kinds, order and source spans.

An earlier mixed-candidate dispatch improved that isolated call probe but did
not improve parser timings. It was replaced by separate eligible bound and
unbound candidate chains before these final measurements. Raw final samples,
input hashes and executable hashes are in
`/tmp/mule-method-flow-fixed-measurement.json`. The small corpus difference is
not a broad performance claim; class/descriptor lookup remains a larger gap.

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
