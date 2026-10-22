# Performance suite

Each scenario in `suite/` has a standalone, standard-library-only Python
workload (`<case>.py`) alongside its SBCL Common Lisp equivalent
(`<case>.lisp`). Shared measurement and orchestration code lives in
`suite/perfutils/`; there is no `perf/bin/` and no plotting dependency.

```sh
# From the repository root: list cases or run against the current CPython.
uv run python -m perf.suite.perfutils list
uv run python -m perf.suite.perfutils run --samples 3

# Compare CPython, an optimized native Mule build, and SBCL on every case.
uv run python -m perf.suite.perfutils run \
  --config perf/runtimes.example.toml --samples 3

# Run one scenario directly, from any working directory.
uv run python perf/suite/log_aggregation.py \
  --size 10000 --seed 17 --loops 3
sbcl --script perf/suite/log_aggregation.lisp \
  --size 10000 --seed 17 --loops 3

# The shorter module invocation also works from perf/suite/.
cd perf/suite
uv run python -m perfutils list
```

## Initial scenarios

| Script | SBCL equivalent | Size means | Work measured |
| --- | --- | --- | --- |
| `account_ledger.py` | `account_ledger.lisp` | Transactions | Polymorphic class hierarchy, `super()` calls, slot updates |
| `closure_pipeline.py` | `closure_pipeline.lisp` | Values | Higher-order function pipelines with mutable lexical closure state |
| `exception_validation.py` | `exception_validation.lisp` | Records | Nested validation raising and catching domain exceptions |
| `expression_tree.py` | `expression_tree.lisp` | Evaluations | Recursive AST evaluation with polymorphic nodes and `dict` environments |
| `graph_paths.py` | `graph_paths.lisp` | Vertices | Dijkstra traversal, heap operations, distance reduction |
| `http_routing.py` | `http_routing.lisp` | Requests | HTTP parsing, query extraction, handler dispatch, response accounting |
| `integer_accumulation.py` | `integer_accumulation.lisp` | Integers | Signed sum and data-dependent positive count |
| `interval_merging.py` | `interval_merging.lisp` | Intervals | Tuple sorting, interval coalescing, binary-search point queries |
| `json_tokenizer.py` | `json_tokenizer.lisp` | Payloads | Handwritten character scanning, slicing, and `dict` aggregation |
| `kv_record_parse.py` | `kv_record_parse.lisp` | Log lines | Delimited key-value string splitting, partitioning, and formatting |
| `log_aggregation.py` | `log_aggregation.lisp` | Log records | Parsing, numeric conversion, dictionary grouping |
| `lru_simulation.py` | `lru_simulation.lisp` | Operations | Doubly-linked list node mutation and `dict` LRU cache simulation |
| `matrix_multiply.py` | `matrix_multiply.lisp` | Matrix dimension | Dense 2D matrix multiplication, transpose, and norm reduction |
| `word_frequency.py` | `word_frequency.lisp` | Tokens | Dictionary frequency counting and bucketed list reduction |

The HTTP case executes handlers in process. It deliberately covers bounded
generated requests rather than a production parser; socket throughput,
concurrency, and latency distributions are future separate cases.

## Common scenario pattern

Keep `NAME`, `DESCRIPTION`, `DEFAULT_SIZE`, and `DEFAULT_SEED` at module scope.
`prepare(size, seed)` constructs repeatable runtime input. `run(data)` implements
the workload and returns a deterministic tuple of integers. Each scenario has
one Python implementation and one matching SBCL implementation. Annotations
follow that scenario's code naturally; setup is shared and unmeasured.
Annotations do not promise that Mule currently specializes every declared type.

`main()` passes those functions and metadata to `perfutils.run_case`
(`run-case` in `perfutils/case.lisp`). All cases accept `--size`, `--seed`,
`--loops`, `--warmups`, and `--help`.
Kernels must not mutate prepared input. Every warmup and measured invocation's
result is checked outside its timer, and the final result is printed. The small
tab-separated protocol keeps JSON, subprocess, and dashboard code out of the
compiled scenario's dependency tree.

Runtime-generated input prevents accidental constant folding of an entire
workload. Every input contributes to a result. Do not introduce opaque calls,
volatile operations, artificial dependencies, or other optimization barriers.
Inlining, vectorization, redundant-check removal, allocation elimination, and
valid algorithmic shortcuts are legitimate wins. Investigate dramatic speedups
using more seeds/sizes and optimized code inspection; replace a workload for
all runtimes if a shortcut defeats the operation it was intended to measure.

## Runtime configuration and reports

Use three measured samples per runtime and scenario, the harness default.
Warmups are separate. Recorded historical runs retain their actual sample counts.

Copy `runtimes.example.toml` and add named runtimes. `command` is an argument
array, optionally accompanied by a `build` command and `version_command`.
Tokens support `{source}`, `{lisp_source}`, `{binary}`, and `{python}` (the
harness interpreter).
`cwd` is resolved relative to the config. For other Mule checkouts, set both
their working directory and their own compiler-host Python executable.
The designated baseline must be a trusted CPython interpreter.
Mule builds use `-O1` per-unit optimization by default; the example runtime selects
`-O2` explicitly, which keeps build times reasonable for the whole suite. Append `--lto` to a build
command to measure full link-time optimization separately; this can increase
link time substantially. The example configuration also compiles each scenario's
`.lisp` implementation into a standalone SBCL executable for side-by-side comparison.

All native builds complete before validation or timing begins. Each case is
built once. The harness validates every runtime
against the baseline's result, then runs processes serially
with rotated runtime order. Each sample repeats validation. Build failures,
timeouts, and incorrect results appear in the report and produce a nonzero
exit status; other comparisons continue.

Generated files live in an ignored `perf/results/<timestamp>/`, or a fresh
directory selected with `--output`.

## History

A complete run, meaning every scenario with a `--config` of runtimes, appends
one line to the committed `perf/history.jsonl`: the UTC stamp, platform, run
options, each runtime's checkout commit and tracked-diff digest, and each
row's status, kernel/process/preparation medians, and elapsed-time ratios. Runs
of a subset of scenarios, baseline-only runs, and runs with `--no-history` are not
recorded, so every line is comparable with every other. Commit the file with
the compiler change it measures.

```sh
uv run python -m perf.suite.perfutils history            # every recorded run
uv run python -m perf.suite.perfutils history --last 10 --runtime mule-o3
uv run python -m perf.suite.perfutils history graph_paths
```

The table has one row per run and one column per scenario, each cell the
selected runtime's kernel median and its elapsed-time ratio against that run's
baseline.
Ratios divide the selected runtime's time by the baseline's time. Lower is
better: 0.50 means half the baseline's elapsed time, and 3.30 means 3.30 times
the baseline's elapsed time. Mule / CPython and Mule / SBCL use the same direction.
A `+` after the commit means the tree had uncommitted tracked changes. Reports include raw samples, medians,
variation, commands, source/helper/binary hashes, build diagnostics, platform,
affinity, runtime version when configured, and checkout revision/diff identity.
`index.html` is an offline dashboard with exportable `kernel.svg` and
`process.svg`; `results.json` contains the underlying measurements.

Kernel time excludes preparation, warmups, result validation, and printing.
Whole-process time includes startup, setup, warmups, measured calls, and output;
it is not a pure startup measurement. Compilation is separate. Kernel timing
includes call and clock overhead, so increase workload sizes until kernels run
long enough for meaningful measurements. Default sizes are starting points,
not a calibration guarantee. Use multiple seeds and sizes explicitly. These
first reports do not measure peak memory, confidence intervals, or machine
idleness. Do not interpret noisy or tiny runs as performance conclusions.

Focused correctness checks:

```sh
uv run pytest -n0 test/perf
uv run ruff check perf test/perf
uv run ruff format --check perf test/perf
```
