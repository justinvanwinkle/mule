# Compiled program performance

Status: measurement and optimization work in progress. The target is a
Mule / CPython elapsed-time ratio of at most 0.10 for compiled programs;
current measurements do not meet that target.

Performance comparisons use elapsed-time fractions: the measured implementation's
time divided by its comparator's time. Lower is better. A ratio below 1 means
the measured implementation takes less time. Before/after comparisons use
after / before, with the changed executable in the numerator.

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

## Bounded runtime aggregate projections, 2026-10-25

The fixed-aggregate scalar-replacement pass now accepts proved in-bounds runtime
indices. Previously, a local two-element list indexed by `(selector + index) & 1`
still called `mule_sequence_list` and the list getter inside its loop. The new
consumer selects the existing scalar child references without constructing a
container. Runtime values and the original source remain unchanged.

The allocation-focused diagnostic is the source in
`test/mule/compiler/test_scalar_aggregates.py`'s bounded runtime projection test,
using its list and positive-index variant. Both native executables used O2 without
LTO. Input was `100000000 37`, supplied on stdin. Builds preceded measurement.
Each runtime received one separate warmup and three measured runs, with native
before/after order alternating and CPU affinity fixed to CPUs 0–31. The CPython
oracle used Python 3.14.7. Subprocess wall-clock measurements include process
startup and exclude builds and `uv` startup. The comparison isolates the
allocation change on base `442f52a`.

| Runtime | Elapsed samples (s) | Median (s) |
| --- | --- | ---: |
| Before, master `442f52a` | 3.173024, 3.280310, 3.381729 | 3.280310 |
| After bounded projection elimination | 0.111982, 0.109493, 0.110664 | 0.110664 |
| CPython 3.14.7 | 5.012698, 5.001364, 5.074642 | 5.012698 |

The candidate takes approximately 0.034 of the baseline elapsed time, about a
30-fold speedup on this diagnostic. Every measured execution printed
`5000000000000000`, matching CPython. The existing suite CPU guard accepted the
phase at 0.812 external CPU cores against its unchanged 2.000-core limit.
Concurrent `vmstat` samples showed no swap-out or sustained I/O wait. No compiler
builds ran concurrently.
An initial one-million-iteration trial was too short for the timer's resolution
and was not used for this comparison. Another attempt was discarded after
unrelated CPU and disk activity interfered. The accepted phase ran only after
the other benchmark suite finished.

The retained source and executables are `/tmp/mule-allocation-tfeezvah.py`,
`/tmp/mule-allocation-tfeezvah-sharded-before`, and
`/tmp/mule-allocation-tfeezvah-sharded-after`. Their SHA-256 values are, respectively:

```text
6934f440f85fc3c69b86a92e701ce04f39819b707e79c0ad618d6fddc9f83b77
b24c27ce616b421c88bf01c18f9acf83ba9fc5fd649adf3f0b4e9bc6969c46c8
ea1ed931f9a2e97012c4baec8cc6a95a2099e852590826047971f9e27390f383
```

Raw samples and CPU evidence are retained in
`/tmp/mule-allocation-tfeezvah-guarded-1.json`. The temporary adapter reuses
`measure_with_cpu_guard`; it does not introduce another monitoring mechanism.
From the repository checkout, the accepted timing command was:

```sh
env PYTHONPATH="$PWD" taskset -c 0-31 uv run --script --python 3.15t \
  /tmp/mule-allocation-tfeezvah-measure.py \
  --output /tmp/mule-allocation-tfeezvah-guarded-1.json
```

This is a separate allocation-elimination diagnostic, not a rerun of the standard
suite or evidence that the earlier set-storage regression is resolved.

## Reproducible comparisons

The new [standalone performance suite](../perf/README.md) lives in `perf/suite/`.
Its standard-library-only harness runs as `python -m perf.suite.perfutils`, with
configurable interpreter/build commands, one implementation per case,
separate kernel/preparation/process timings, correctness validation, and offline
HTML/SVG reports. The initial cases cover integer traversal, graph shortest
paths, access-log aggregation, and in-process HTTP parsing/routing. Socket/server
load tests and memory measurement are not implemented in this first set.

Historical trend charts retain all scenario runs but scale each zero-based
vertical axis from its latest `min(10, ceil(run_count / 4))` runs, across all
displayed runtimes and typed variants, with 20% headroom. Values above the scale
use upward triangles and numbered value labels below the panel. Missing or
failed measurements break connecting lines. See the suite documentation for
empty-window handling and the distinction between kernel and process timing.

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
  --directory /tmp/mule-performance/baseline --samples 3
```

Use separate directories for successive builds. `--reuse` times an existing
executable. Use three measured runs per runtime and workload, with warmups
separate. Historical results below retain their actual sample counts.
The current compiler revision does not establish the provenance of that
executable. `--report` chooses a separate JSON path when remeasuring it.
`--baseline-directory` adds an earlier native executable to the same alternating
sample sequence, validates its output, and records its hash and Mule / baseline
elapsed-time ratio.
Increase `--count` until process startup is a small fraction of the measured
time. Do not run simultaneous timing jobs or include builds in elapsed program
runtime. A timeout is recorded as a timeout, without a validated time ratio.

The `nbody` case uses the unmodified base Python program from Benchmarks Game;
input 1000 must also match `1000_out` byte for byte. `sha3` measures the compiled
Mule implementation against CPython's hashing implementation. These broader
cases supplement the small diagnostic kernels; results from a numeric loop
alone do not establish application performance.

## Suite-wide benchmark audit and root-cause diagnosis, 2026-10-22

The recorded 14-workload suite snapshot in `docs/assets/results.json` (checkout
`45c936432`, five samples per runtime and workload, one warmup, seed 42, default
scenario sizes, four concurrent scenario jobs) compares CPython 3.15.0, Mule
`-O2` without LTO, and SBCL (running corresponding Common Lisp implementations).
Ratios are fractions (`Mule / CPython` and `Mule / SBCL`) where lower values
mean faster Mule execution:

| Workload | CPython median (s) | Mule `-O2` median (s) | SBCL median (s) | Mule / CPython | Mule / SBCL |
| --- | ---: | ---: | ---: | ---: | ---: |
| `word_frequency` | 4.0467 | 12.0207 | 3.2783 | 2.97 | 3.67 |
| `log_aggregation` | 0.0529 | 0.1530 | 0.0192 | 2.89 | 7.95 |
| `expression_tree` | 3.9955 | 9.6383 | 1.2752 | 2.41 | 7.56 |
| `lru_simulation` | 3.9788 | 7.4738 | 1.7312 | 1.88 | 4.32 |
| `http_routing` | 0.0423 | 0.0685 | 0.0485 | 1.62 | 1.41 |
| `graph_paths` | 0.0288 | 0.0411 | 0.0121 | 1.42 | 3.40 |
| `exception_validation` | 3.9169 | 2.6073 | 0.6139 | 0.67 | 4.25 |
| `account_ledger` | 4.2361 | 2.0007 | 0.6797 | 0.47 | 2.94 |
| `closure_pipeline` | 4.0818 | 1.6042 | 0.2663 | 0.39 | 6.02 |
| `matrix_multiply` | 4.5688 | 0.5461 | 0.0802 | 0.12 | 6.80 |
| `json_tokenizer` | 3.9440 | 0.9361 | 0.4435 | 0.24 | 2.11 |
| `interval_merging` | 4.0738 | 0.8330 | 0.4406 | 0.20 | 1.89 |
| `integer_accumulation` | 0.0297 | 0.0060 | 0.0034 | 0.20 | 1.75 |
| `kv_record_parse` | 4.0059 | 1.6923 | 2.4106 | 0.42 | 0.70 |

Inspection of the compiled IR1, `locall` summaries, and `stdlib/_dict.py` across
these 14 workloads identified **four fundamental architectural gaps** where Mule
departs from SBCL and falls back to untyped, boxed, generic runtime operations
inside inner loops:

1. **Declared parameter types are discarded at function boundaries instead of
   checked/guarded and asserted in the body (unlike SBCL `ir1-convert-lambda` +
   `checkgen.lisp`)**:
   - In SBCL, `(declare (type ...))` on function parameters emits an entry type
     check (governed by `safety`) and asserts the declared `CTYPE` across the
     entire function body.
   - In Mule, `_DECLARATION_BINDINGS` (`mule/compiler/ir1tran.py`),
     `Contract.general_parameter_type` (`mule/compiler/annotation_types.py`), and
     `_ParameterUse` (`mule/compiler/locall.py`) discard `tuple[...]`,
     `dict[...]`, nested container types, and user-defined class annotations
     (`node: _Node`, `left: Expr`, `acct: Account`), and return `UNKNOWN` from
     `general_parameter_type()` for all contracts except single-level
     `ListContract` with a scalar element.
   - Because each benchmark passes `prepare` and `run` as callbacks to
     `perfutils.run_case`, `locall.analyze` sees no direct intra-unit call site
     for `run` and creates no `observed` entry context.
   - Direct consequences in the suite:
     - `matrix_multiply.run(data: tuple[list[list[int]], list[list[int]]])`
       compiles with `parameters=(UnknownType,)` and `variants=()`. Inside the
       $490^3 = 117.6\text{M}$ inner loop, `row_a[k] * col_b[k]` executes
       generic `UnknownType` subscripting, boxed `mule_multiply`, and boxed
       `mule_add` 117 million times.
     - `word_frequency.run(data: list[tuple[str, int]])` strips `tuple[str, int]`
       to `ListType(element=UnknownType)`. Unpacking `for token, weight in data:`
       yields `token: UnknownType, weight: UnknownType`, degrading every
       arithmetic operation in the loop to generic `UnknownType` dispatch.
     - `lru_simulation`: `LRUCache._unlink(self, node: _Node)` and
       `LRUCache._push_front(self, node: _Node)` discard `node: _Node`;
       `LRUCache.get(self, key: int)` leaves `key` as `UnknownType` because
       `self._map.get(key)` is a method call rather than a primitive numeric op;
       and `run(data: list[tuple[int, int, int]])` unpacks `is_put, key, value`
       as `UnknownType`.
2. **`stdlib/_dict.py` compiles as untyped dynamic code doing 5+ generic
   attribute lookups and boxed comparisons per hash probe (unlike SBCL
   `src/code/target-hash-table.lisp`)**:
   - `_table_lookup` and `_table_insert` in `stdlib/_dict.py` are standalone
     functions taking untyped `(self, key)` shared by `dict`, `set`, and
     `frozenset`. They compile with `self: UnknownType` and `variants=()`.
   - Every dictionary lookup or insertion performs dynamic runtime attribute
     lookups (`mule_attribute_lookup_once`) for `self._indices`, `self._hashes`,
     `self._keys`, `self._values`, and `self._used`.
   - Because `self._indices` and `self._hashes` have `UnknownType`,
     `_storage_read(indices, slot)` returns `UnknownType`. Consequently,
     `_table_lookup`'s inferred return type is `UnknownType` (forcing
     `entry >= 0` in `dict_get` and `dict_getitem` to call generic
     `mule_compare_greater_equal`), and `_storage_read(hashes, entry) == code`
     calls generic `mule_compare_equal`. Furthermore, `hash(key)` returns a
     64-bit integer (`[-2^63, 2^63 - 1]`) exceeding Mule's 63-bit fixnum range
     (`[-2^62, 2^62 - 1]`), forcing heap/GMP integer paths when bit 62 is set.
   - This directly accounts for the `Mule / CPython` ratios of `1.62`–`2.97` and
     `Mule / SBCL` ratios of `1.41`–`7.95` across `word_frequency`,
     `log_aggregation`, `expression_tree` (`VarExpr` / `LetExpr` env dict
     lookups), `lru_simulation`, and `http_routing`.
3. **Class methods drop `self: InstanceType(cls)` entry variants in
   `locall._SpecializesBody` unless a primitive numeric operation changes type**:
   - `_SpecializesBody` (`mule/compiler/locall.py`) discards a method's `fixnum`
     entry context `(InstanceType(cls), ...)` unless narrowing `self` changes a
     node's derived type to `INTEGER`, `BOOLEAN`, or `FLOAT`, or specializes a
     `list`/`str`/`dict` container op.
   - Methods whose bodies read or write instance attributes (`self.value`,
     `self.name`, `self.left`, `self.right`, `self._head`, `node.prev`) or
     return `PYTHON_INTEGER` fail `_SpecializesBody` and discard their
     `InstanceType(cls)` variant.
   - In `expression_tree.py`, `LitExpr.eval_node`, `VarExpr.eval_node`,
     `AddExpr.eval_node`, `MulExpr.eval_node`, and `LetExpr.eval_node` all
     compile with `variants=()` and `self: UnknownType`, executing generic
     attribute lookups on every AST node evaluation.
4. **Boxed container element storage and per-iteration reference-count traffic
   (`mule_retain` / `mule_release`) in hot loops**:
   - Even when `list` or `dict` storage is proven, elements are stored as boxed
     `Value` pointers and every `_storage_read` in an inner loop retains and
     releases the loaded element rather than borrowing or reading unboxed
     scalars when no mutation or escape can occur across the read.

See [remaining compiler gaps](compiler-gaps.md#0-high-impact-architectural-parity-gaps-from-the-14-workload-suite-audit-proposed)
for the SBCL references and staged remediation plan for these four defects.

## Instance field inference, method entry dispatch, and method return contracts, 2026-10-25

With `0C` implemented ([`spec/compiler-gaps.md`](compiler-gaps.md#0c-preserving-instancetypecls-method-specialization-instance-field-inference-and-method-call-return-contracts-locallpy-constraintpy-return_checkspy-codegenpy-implemented)),
`locall` preserves `InstanceType(cls)` specialization across `GETATTR` / `SETATTR`,
infers scalar (`IntegerType`, `TextType`) and `DictType` instance fields from
`__init__` and method stores, asserts derived and declared return contracts
across monomorphic and polymorphic `CALL_RESOLVED` method calls, and dispatches
`method_call_helper` / `call_resolved_target` directly to specialized `.spec.0`
entries via `_entry_steps`.

A three-sample comparison (`uv run python -m perf.suite.perfutils run --config perf/runtimes.example.toml --no-history --samples 3 expression_tree lru_simulation`,
one warmup, seed 42, default sizes, `--jobs 4`, external CPU 0.908 cores)
measured:

| Workload | CPython median (s) | Mule `-O2` before (s) | Mule `-O2` after (s) | SBCL median (s) | After / before | Mule / CPython |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `expression_tree` | 4.0448 | 9.3300 | 6.9978 | 1.2816 | 0.750 | 1.73 |
| `lru_simulation` | 3.9814 | 7.4900 | 6.2344 | 1.6033 | 0.832 | 1.57 |

## Closure slot lowering, 2026-10-22

The unchanged `perf/suite/closure_pipeline.py` workload now uses constant-index
closure entry loads and explicit IR2 value-cell operations. The compiler borrows
the rooted shared cell instead of retaining it for each protocol call. Guarded
field operations preserve mutable Python attributes and avoid the cell-access
helpers on the bound path. The SBCL correspondence and Python adaptation are in
[native lowering](native-lowering.md#closure-and-value-cell-slot-lowering).

The baseline is the published suite executable from clean revision `a9f8dbc`.
The changed executable uses `7ba321f` plus this implementation. Both use LLVM
`-O2`, Rust level 3, and no LTO. Builds, compiler tests, and instruction profiling
finished before timing. Five samples per executable alternate execution order,
with one warmup per process, size 100,000, seed 42, and one kernel loop. CPU
affinity was not restricted. Every sample returns `(192382896, 8062672)`, matching
the recorded CPython result for the same input.

| Build | Kernel samples, seconds | Median seconds |
| --- | --- | ---: |
| Before slot lowering | 3.236301, 3.258334, 3.241263, 3.230092, 3.216703 | 3.236301 |
| After slot lowering | 1.570104, 1.559280, 1.566053, 1.567621, 1.561517 | 1.566053 |

The changed / baseline kernel-time ratio is **0.484** on this workload. This is
a focused kernel result, not a suite-wide performance claim.
At size 10,000, three kernel loops, and no warmup, whole-process Callgrind
instruction counts fall from 1,639,283,736 to 951,782,353, a 42% reduction.
Profiling is separate from timing. The changed machine code uses closure-slot
loads and guarded cell-field accesses; its unbound branch retains a compiled
Python error helper. Generic closure-call dispatch remains.

The source SHA-256 is
`9f159cb8dbba2f44970c2c8628d63ba6902159518536c35e5aaf8d2757168518`.
The baseline executable SHA-256 is
`7e6bd82565d3f376777ee20f9c8d90e4f9740e4bcdfc5304864ca39787ac56ae`.
The changed executable SHA-256 is
`c6892c0ec5caf22e745a9a851bd3ec589e6abf998246101476fdbb0abe8d809c`.
The final rebuild is byte-identical to the measured changed executable. Local
executables and emitted LLVM are under `/tmp/mule-closure-slot-*`; instruction
profiles are `/tmp/mule-closure-large.callgrind` and
`/tmp/mule-closure-slot-lowered.callgrind`. These are temporary local evidence.

## Mixed-boundary float coercions, 2026-10-21

The representation pass now separates numeric-only copies from a float whose
original object remains observable. This follows SBCL's representation selection
and subsequent move/coercion insertion. The implementation and identity constraints
are in [the representation contract](compiler-gaps.md#identity-aware-float-representation-coercions-implemented).

The diagnostic program was:

```python
def calculate(n, label):
    original = 1.25
    saved = [original]
    value = original
    for index in range(n):
        value = value * 1.00000001 + 0.00001
    print(label, saved[0] is original, value > 20.0)
calculate(2000000, 'result')
```

The general object entry keeps `original` boxed and selects double storage for
the numeric recurrence. The focused LLVM regression checks one extraction before
the recurrence and no float-result boxing in that body. LLVM `opt` with
`-passes=default<O3>` retains `fmul double` and `fadd double` in the numeric loop
blocks, without float boxing or extraction calls in those blocks. This is not
a claim that all loop bookkeeping is native or allocation-free.

Both executables used LLVM `-O3`, no LTO, and the same source. The baseline was
an isolated archive of `d33fd0d`; the changed build used that revision plus the
uncommitted coercion implementation. Builds preceded timing. One warmup preceded
five whole-process samples per executable, with execution order alternating.
Every run matched CPython's `result True True` output.

| Build | Samples, seconds | Median, milliseconds |
| --- | --- | --- |
| Baseline | 0.020677, 0.018598, 0.018819, 0.021795, 0.021554 | 20.677 |
| Float coercions | 0.019641, 0.020957, 0.020031, 0.021047, 0.019385 | 20.031 |

The changed / baseline median-time ratio is 0.969, within the observed variation.
This workload does not demonstrate a material timing improvement. The verified
change is the representation capability, not an application-performance claim.

The source SHA-256 was
`bb06f01641986c209c7bab29a7fcf9e5a48e16f32e723e0c60b7d6842c4306a5`.
The baseline executable SHA-256 was
`6d041b8454419a86f5335eb6337110e7d1bbe528afcd3888f96c52b6c37a6517`;
the changed executable SHA-256 was
`7525e297c26b693030e0668fb979e407d5d61d4d25b8a7a8844347c74c5d4967`.
The focused checks are in `test/mule/compiler/test_representation_coercions.py`.

## Scalar boxing kernels, 2026-10-20

Four native LLVM kernels compare the real opaque runtime conversions with the
compiler's LLVM-visible codecs and private lifetime operations. Every iteration
changes its input: float uses multiplication/addition, and the integer kernel
increments and masks into a positive wide-int64 range. The step functions remain
non-inlined, and the iteration count comes from the command line. This isolates
boxing overhead without allowing the arithmetic loop to disappear.

Both builds use LLVM `-O3`, Rust level 3, and no LTO. Five alternating samples
per pair run sequentially on CPU 0 after all builds and compiler tests finish.
Every timed checksum matches an independent Python 3.15 numeric loop. Inputs
0, 1, and 1,000 iterations also validate before timing.

| Payload | Iterations | Opaque seconds | Visible seconds | Visible / opaque |
| --- | ---: | ---: | ---: | ---: |
| float64 | 96,000,000 | 1.035982 | 0.127182 | 0.123 |
| positive wide int64 | 128,000,000 | 3.117412 | 0.119781 | 0.0384 |

The visible float step has only multiplication, addition, and return. The
integer step retains its genuine GMP ABI guard, but its supported 64-bit-limb
path has no object or limb allocation/deallocation calls. Other limb ABIs retain
the checked fallback. The integer preparation supplies a real range proof; it
does not assume the returned value. These are box/load/release microbenchmarks,
not application-wide speed measurements.

Source, input and optimized LLVM, machine code, executables, hashes, oracle
checksums, raw samples, and medians are under
`/tmp/mule-boxing-alltypes.ybS8ro/perf/`, with results in `results.json` and the
reproduction script in `bench.py`. These artifacts are temporary local evidence.

Range-proven fixnum, positive-wide, and negative-wide integer roundtrips reduce
to `ret i64 %input` at one `-O3` pipeline. A fully generic tagged/heap pointer
merge can still retain allocations in LLVM 21; the codec's correctness does not
depend on elimination. Fallback scratch storage stays in the helper entry block
so inlining does not introduce avoidable stack-save/restore barriers.

## Private leaf-box elimination, 2026-10-20

The compiler's generated float construction, payload-load, and private-release
helpers are tested together against the real pooled-storage runtime ABI. The
unoptimized probe allocates a box, initializes its active fields, reads its
payload, and releases its sole private reference. At LLVM `-O3`, without LTO,
the entire probe becomes `ret double %input`: no allocation, deallocation,
load, store, or helper call remains. Native machine code also contains no calls.
Executed probes compare exact bits for 3.25, negative zero, infinity, and NaN.

This relies on truthful allocation-family contracts for raw storage and on
private ownership, not on treating generic reference release as free. Native
regressions separately verify private aliases, fresh factory returns, container
and closure escapes, error cleanup, slot reuse, and the physical weak-handle API
for builtin floats. Shared and escaped objects retain ordinary destruction.

Reproduce the optimized-IR and executable check with:

```sh
uv run pytest -m integration test/mule/compiler/test_llvm_boxing.py
```

This is an instruction/allocation-elimination result, not a new application-wide
timing claim. The older runtime box/unbox probes under
`/tmp/mule-llvm-roundtrip.5sUn0A/` document why visibility and ownership contracts
were needed; their files are local temporary evidence.

## Numeric calls in mixed bodies, 2026-10-20

This diagnostic calls a nested accumulator function with ordinary object
parameters and a numeric weight. The helper runs
`total = total * 1.0000001 + step` from `0.5`, can print its object parameter,
and returns the accumulated float to a numeric comparison in its caller. The
iteration count comes from `sys.argv`. The new internal convention returns a
`double` and accepts the weight as a `double` without requiring a whole-function
native plan.

On the AMD Ryzen 9 9955HX, 20,000,000 iterations produced these whole-process
medians. Both builds use LLVM `-O3`, Rust level 3, and no LTO. Five alternating
samples per executable follow validation/warmup runs, pinned to CPU 0. Builds
and compiler tests finished before timing started.

| Build | Median seconds |
| --- | ---: |
| Before numeric calling conventions | 0.585985 |
| After numeric calling conventions | 0.030313 |

The changed / baseline time ratio is 0.0517 on this diagnostic. The baseline
uses a clean source snapshot of `a31c335`; the changed build includes this
numeric-convention implementation and its float-identity adapter repair. Both
executables match CPython 3.15 host output at counts 0, 1, 10,000, and 20,000,000.
Separate native regressions compare numeric values, aliases, closures, exception
paths, arbitrary-precision integers, and booleans against CPython 3.14.

The generated LLVM declares and calls a `double`-returning numeric entry with a
raw weight parameter. In optimized machine code LLVM inlines that entry into
the caller and keeps the recurrence in an XMM register with `mulsd` and `addsd`.
The caller has no float boxing/unboxing calls. Frame, generic iteration,
safepoint, and conditional print paths remain. Source, LLVM, executables,
hashes, samples, and the measurement script are under
`/tmp/mule-numeric-abi.Dy4sF8/`; `eligible.py` is the measured source and
`results.json` contains the samples. These are temporary local artifacts.

This is not an application-wide speedup claim. Eligibility still depends on
existing type and call-target proofs. A preliminary probe that instead calls a
small helper on each iteration's accumulator did not establish the float facts
needed to select a numeric entry; it remains in `probe.py` and was not timed.
This measurement does not establish that the type solver resolves such cyclic
argument/result dependencies.

## Closed numeric-use float local storage, 2026-10-20

An ordinary object-mode function repeatedly computes
`value = value * 1.0000001 + 0.125`, starting at `0.5`, and returns whether the
result exceeds `1000.0`. The iteration count comes from `sys.argv`, so it is
not a compile-time constant. This diagnostic specifically exercises a float
whose identity never escapes. It does not measure returned float accumulators
or application-wide performance.

On the AMD Ryzen 9 9955HX host, 20,000,000 iterations produced these median
whole-process times. Both builds use LLVM `-O3`, Rust optimization level 3, and
no LTO. Five alternating samples per executable follow validation/warmup runs,
with the measurement process and its children pinned to CPU 0. Compilation and
other compiler tests finished before timing started.

| Build | Median seconds |
| --- | ---: |
| Before closed-group representation selection | 0.260872 |
| After closed-group representation selection | 0.030628 |

The changed / baseline time ratio is 0.117 on this diagnostic. The source
snapshots differ only in `mule/compiler/represent.py`; both include the same
other working-tree compiler and runtime changes. Both executables match the
CPython 3.15 host's output at counts 0, 1, 10,000, and 20,000,000. Focused
regression tests separately compare branch, copy, exception, boundness, and
capture behavior with the CPython 3.14 conformance oracle.

The baseline LLVM boxes and unboxes the loop-carried value. The changed
executable's optimized machine code keeps that value in an XMM register and
uses `mulsd` and `addsd` without float boxing calls in the function. Generic
iteration and safepoint paths remain. Source, baseline snapshot, LLVM, native
executables, executable/source hashes, raw samples, and the measurement script
are under `/tmp/mule-float-storage.7PQvSc/`, with results in `results.json`.
These temporary artifacts are local evidence, not published benchmark assets.

## Parser construction staging, 2026-10-19

The Python-hosted parser and the native parser remain separate measurements.
This experiment compares two native executables built at LLVM `-O3`, with the
Rust runtime at level 3 and no LTO, on the Intel Core i7-1370P host. Both use
the same preloaded source strings and run on CPU 0. Imports, source-file reads
and compilation are outside the parser timer. Five samples per executable and
input group produce these median times for one complete pass over the group:

| Input group | Files | Before seconds | After seconds | After / before |
| --- | ---: | ---: | ---: | ---: |
| Small-program corpus | 44 | 0.257052 | 0.200906 | 0.782 |
| Parser modules | 7 | 1.024453 | 0.744841 | 0.727 |
| Compiler modules | 47 | 12.952870 | 9.677561 | 0.747 |

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

| Input group | Before seconds | After seconds | After / before |
| --- | ---: | ---: | ---: |
| Small-program corpus | 0.200366 | 0.198532 | 0.991 |
| Parser modules | 0.764915 | 0.751739 | 0.983 |
| Compiler modules | 9.732363 | 9.370380 | 0.963 |

A separate one-million-call `Base.step(instance, value)` probe produced the
validated result 1,000,000 in every run. Its five alternating process samples
had kernel medians of 0.171629 seconds before and 0.160023 seconds after
(after / before = 0.932). This isolates one invocation shape, not a whole parser
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

| Program | Input | CPython seconds | Mule seconds | Mule / CPython |
| --- | ---: | ---: | ---: | ---: |
| Integer loop | 1,000,000 | 0.0290 | 0.00948 | 0.327 |
| Float loop | 1,000,000 | 0.0341 | 0.111 | 3.26 |
| Function calls | 500,000 | 0.0242 | 0.0206 | 0.851 |
| List indexing | 1,000,000 | 0.0315 | 0.0197 | 0.625 |
| Instance attributes | 250,000 | 0.0216 | 0.170 | 7.87 |

Ratios below one mean Mule takes less time. These are exploratory measurements,
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

The after / before time ratio is 0.142, and Mule / CPython is 0.662.
Exact output was verified for every sample.

Inlining the inactive safepoint poll further removes native call overhead from
entries and loops, while retaining the acquire flag read and active collection
handshake. A nine-sample comparison pinned to CPU 6 with 10,000,000 integer-loop
iterations measured CPython at 0.18429 seconds, the original Mule executable at
0.02215 seconds, and the new Mule executable at 0.01787 seconds. The Mule / CPython
time ratio is 0.0970, and new / original Mule is 0.807 before enabling complete
cross-language LTO. With the final build settings,
the same input measured CPython at 0.18593 seconds and Mule at 0.01903 seconds,
for a Mule / CPython time ratio of 0.102. Full LTO modestly changed this case's
startup and code layout; it remains enabled for the whole compiled implementation.
At 1,000,000 iterations, startup has a greater effect on whole-process timings;
the larger-input result does not establish a Mule / CPython ratio of at most
0.10 for every input or other programs.

The unmodified nbody program at input 1000 matched the upstream reference, but
took 0.303 seconds against CPython's 0.0166 seconds in a three-sample exploratory
run. A five-sample comparison after guarded list unpacking measured the same
nbody workload at 0.0511 seconds, with after / before = 0.171 and
Mule / CPython = 3.21. The optimization snapshots list elements
before target setters execute and preserves subclass iteration overrides.

SHA3-256 over 4 MiB matched CPython's digest but took 19.29 seconds against
0.0222 seconds in a three-sample run: Mule / CPython was approximately 869.
The SHA3 program imports `_sha3` directly in both implementations to measure one
algorithm without building all algorithms exposed by `hashlib`. Its profile includes
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
they do not establish a Mule / CPython time ratio of at most 0.10 for every
program.

```sh
valgrind --tool=callgrind --callgrind-out-file=/tmp/attributes.callgrind \
  /tmp/mule-performance/baseline/attributes 10000
callgrind_annotate --inclusive=no /tmp/attributes.callgrind
```

Keep profiling separate from timing: instrumentation substantially changes
runtime. Check generated LLVM and native code alongside measurements when
claiming that a compiler transformation removed dispatch or allocation.

## General tuple delivery and per-store liveness (2026-10-27)

Section 6G in `compiler-gaps.md` moves tuple convention selection and unpack
delivery out of code-generation sequence recognition. Correctness checks cover
non-local targets, target effects and failures, weak-reference ownership
observers, alias returns, and dead assignments across control flow. Optimized
LLVM checks follow the actual direct-call fast path and tuple field loads,
allowing required cold fallbacks. They establish the general lowering mechanism,
not a speedup for every workload.

The comparison used `perf/runtimes.example.toml`: Mule `-O2`, no LTO, Rust `-O3`,
CPython 3.15.0, and SBCL 2.6.4.debian. Each workload used size 50,000, seed 42,
one kernel loop, one separate warmup, and three measured samples. The harness
used `--jobs 4` and the unchanged CPU affinity, logical CPUs 0 through 31.
Compilation and preparation were outside the kernel timer. Each runtime's
result matched CPython on every accepted sample.

Accepted Mule kernel medians, in milliseconds:

| Workload | Baseline | Candidate | Repeat baseline | Repeat candidate |
| --- | --- | --- | --- | --- |
| HTTP routing | 60.357 | 56.458 | 61.281 | 56.023 |
| Key-value parsing | 1426.272 | 1405.368 | 1455.359 | 1399.340 |
| Interval merging | 526.100 | 536.125 | 525.594 | 537.858 |

The candidate consistently measured lower for HTTP routing and key-value parsing.
Interval merging measured about 2% higher. Its binary-search and outer query
loops have the same logical machine instructions before and after the change,
apart from addresses, stack offsets, and alignment. In particular, the borrowed
tuple path still performs direct field reads without new output-slot copies or
ownership operations. The initial hypothesis that slot delivery replaced that
borrowed path was wrong: its existing borrowed-capture metadata excludes it.
The measured interval-merging slowdown remains unattributed. These results do
not establish a universal performance improvement.

The baseline was `0af7761ea462b3a9e1154fe819a2cf35f97d7984`. The candidate was
the 6G worktree based on that revision; the accepted report records tracked-diff
SHA-256 `fe6281711d2512b68c71ab43c5962c609597677c66f0a8f710b12505cb21c847`.
The compiler sources remained unchanged between the accepted candidate runs.
Documentation and test-observation updates are included in the final commit.

Run from the respective baseline or candidate checkout:

```sh
uv run python -m perf.suite.perfutils run \
  --config perf/runtimes.example.toml --jobs 4 --samples 3 --no-history \
  --output /tmp/mule-6g-before-clean \
  http_routing kv_record_parse interval_merging
```

The candidate command substituted `/tmp/mule-6g-after-clean` for the output
directory. Repeats used `/tmp/mule-6g-before-repeat` and
`/tmp/mule-6g-after-repeat`. Each directory contains raw samples, commands,
binary hashes, source identity, CPU observations, and `results.json`.
Accepted external CPU utilization was respectively 0.471, 1.690, 0.745, and
0.868 cores, below the unchanged 2.0-core limit. `vmstat` monitoring showed no
swap-out activity during those accepted comparisons and no I/O-wait saturation.
An earlier run in `/tmp/mule-6g-before` exceeded the CPU-noise limit and included
active swapping; none of its timings contributed to this table.

## General fixed-slot access and singleton fill (verified, 2026-10-27)

Section 6J replaces the compiler's `_Table` field maps with owner-derived class
layouts and guarded attribute access. Singleton repetition uses the compiled
fill algorithm. Correctness and optimized-code checks pass, including the actual
exact-dictionary get/set protocol entries at `-O2` and `-O3` without LTO.

The first layout implementation lost receiver facts at the separately compiled
protocol boundary. The dictionary wrappers called unknown-receiver bodies rather
than the specialized table helpers inspected by the original regression. A
repair publishes implementation layouts separately from constructor result types,
declares internal protocol argument domains, and exports the actual typed
descriptor body under a stable protocol symbol. Generic callable bodies remain
unchanged. The regression now follows the real wrapper-to-algorithm call.

The comparison used unchanged `perf/runtimes.example.toml` settings: Mule `-O2`,
no LTO, Rust `-O3`, `--jobs 4`, three measured samples, seed 42, one kernel loop,
one separate warmup, and affinity CPUs 0 through 31. Workload sizes were 80,000
for `word_frequency` and 100,000 for `account_ledger`. Every reported runtime
result matched the CPython oracle.

```sh
uv run python -m perf.suite.perfutils run \
  --config perf/runtimes.example.toml --jobs 4 --samples 3 --no-history \
  word_frequency account_ledger
```

The fixed baseline was `7d8bdd6`, in worktree `general-slot-fill-baseline`.
The initial candidate was `8ef8736`; the receiver-proof repair was measured at
`1c3b37b`. Reports are under the respective worktrees' ignored `perf/results/`
directories: `20261027T023054.647709Z`, `20261027T023311.704085Z`, and
`20261027T032301.613870Z`. Each contains raw samples, commands, hashes, revision
identity, and CPU observations.

The CPU-noise guard accepted these attempts, but it does not validate memory
or I/O contention. The repaired candidate's resource log shows active swapping
inside the measured interval, including approximately 270 MiB/s peak swap-out.
Its 5.307-second `word_frequency` median must not be treated as a clean
comparison with the baseline's recorded 3.205 seconds. A representative
before/after timing conclusion remains pending. No speedup is established.

Separate bounded Callgrind snapshots explain the remaining generated work.
After receiver-proof repair, actual callers reach `dict_get.protocol` and
`dict_setitem.protocol`; hot generic storage reads/writes and entry-index
comparisons disappear, and reader field retains disappear. Normalized exclusive
instructions per call to the unchanged storage hash routine are approximately
435 in the baseline and 725 in the repaired candidate. Attribute hit/layout
helpers contribute about 147 additional instructions per hash. Retain/release
helpers contribute about 60 additional instructions relative to the baseline.
These partial instrumented snapshots identify work; they are not timing samples.

At that checkpoint the branch was not merged into `master`. Removal of ordinary
attribute guards cannot assume that mutable Python descriptors or hooks are
absent. One proposed direction is general lowering of the existing physical
`instance_field` / `set_instance_field` primitives, with explicit use of those
operations in private dictionary storage algorithms. That source-level semantic
change received approval. Its implementation and updated before/after
verification proceeded through the clean comparisons below.

A clean physical-access comparison used baseline `a65df009` and the candidate
working tree with that baseline integrated. The reports are
`20261027T070215.226560Z` in the baseline worktree and
`20261027T082054.883451Z` in `general-slot-fill`. Settings remained unchanged.

| Workload | Baseline kernel seconds | Candidate kernel seconds |
| --- | --- | --- |
| word_frequency | 2.952241 | 3.525844 |
| account_ledger | 1.850329 | 1.840648 |

External CPU utilization was 0.536 and 0.402 cores. Neither resource capture
showed swap-out. The word-frequency regression is approximately 19%, so this
candidate does not establish performance parity.

Bounded Callgrind runs used the saved executables with size 2,000, seed 42,
one loop, and no warmup. Both produced `(500, 759926, 752216)`. Whole-process
instruction totals were 79,655,113 and 86,107,775. The extra work includes
reference-count traffic in the dictionary kernels. Inspection found that the
physical reader unconditionally retained values even when representation
selection chose non-owning immediate storage. It also used generic retain
logic for known heap values. The ownership correction uses those existing
representation and type facts. It preserves missing-field checks and the
callback restrictions on borrowed values.

The final comparison used `d714a6c` as the baseline and the verified candidate
with that revision integrated. Reports are `20261027T091511.164711Z` and
`20261027T100121.208363Z`. Raw reports and build artifacts are retained in the
main checkout's ignored `perf/results/` directory after worktree cleanup.

| Workload | Baseline kernel seconds | Final kernel seconds | Change |
| --- | --- | --- | --- |
| word_frequency | 3.003716 | 2.849528 | 5.1% less time |
| account_ledger | 1.814499 | 1.815919 | Effectively unchanged |

The three word-frequency samples ranged from 2.998284 to 3.016507 seconds in
the baseline and from 2.844797 to 2.882662 seconds in the candidate. Ledger
sample ranges overlapped. External CPU utilization was 0.474 and 0.392 cores.
Both captures showed no swap-out; the candidate's peak I/O wait was zero.
All runtime results matched the Python oracle. Settings remained Mule `-O2`,
no LTO, Rust `-O3`, four scenario jobs, three samples, seed 42, one loop,
and one separate warmup.

The final ownership correction also restores element borrowing through an
existing local. A physical-field lifetime marker had prevented the consumer
proof from finding the actual local assignment. Recognizing that marker and
its real assignment position removes redundant key retains and releases.
It does not remove null checks or permit callbacks inside the protected
region. Terminal physical storage mutations finish array access before
releasing an overwritten value. A native weak-reference callback that clears
the owning dictionary during release matches Python.

The final default suite passed with 4,252 tests. The targeted native integration
suite passed at `-O2` and `-O3`, including real dictionary protocol loops,
guarded layouts, singleton fill, ownership, and mutation behavior. Ruff passed.

## SBCL conservative container types, word vectors, and declared instance fields, 2026-10-30

Replacing static container-mutation, alias, parent-container, and unannotated
instance-field store tracking with SBCL's conservative container rule
(`lvar-conservative-type`), `Data::RawWords` word vectors for `_Table._indices`
and `_Table._hashes`, and declared class-body field contracts (`slot-access-transform`
and `dsd-safe-p`) resolves the `heterogeneous_slot_mutation` miscompilation in
`perf/topics/objects/cycles_and_lifetimes.py`:

| Scenario (`--quick`) | CPython 3.14t | Mule | Mule / CPython |
| --- | ---: | ---: | ---: |
| `objects/cycles_and_lifetimes:cyclic_graph_churn` | 11.47s | 10.56s | 0.92x |
| `objects/cycles_and_lifetimes:heterogeneous_slot_mutation` | 10.02s | 13.65s | 1.36x |

Unannotated mutable containers accessed through multiply referenced variables
or module globals carry only their container kind (`LIST_TYPE`, `DICT_TYPE`,
`SET_TYPE`) and retain bound checks on adjustable sequences (`list`,
`bytearray`), while declared container annotations (`list[T]`, `dict[K, V]`,
`set[T]`) and declared class-body instance fields preserve element and field
specialization.
