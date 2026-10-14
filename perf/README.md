# Performance suite

Each file in `suite/` is a standalone, standard-library-only workload. Shared
measurement and orchestration code lives in `suite/perfutils/`; there is no
`perf/bin/` and no plotting dependency.

```sh
# From the repository root: list cases or run against the current CPython.
uv run python -m perf.suite.perfutils list
uv run python -m perf.suite.perfutils run --samples 7

# Compare CPython with an optimized native build of every case.
XDG_CACHE_HOME=/tmp/mule-progress-cache uv run python -m perf.suite.perfutils run \
  --config perf/runtimes.example.toml --samples 7

# Run one scenario directly, from any working directory.
uv run python perf/suite/log_aggregation.py \
  --size 10000 --seed 17 --variant typed --loops 3

# The shorter module invocation also works from perf/suite/.
cd perf/suite
uv run python -m perfutils list
```

## Initial scenarios

| Script | Size means | Work measured |
| --- | --- | --- |
| `integer_accumulation.py` | Integers | Signed sum and data-dependent positive count |
| `graph_paths.py` | Vertices | Dijkstra traversal, heap operations, distance reduction |
| `log_aggregation.py` | Log records | Parsing, numeric conversion, dictionary grouping |
| `http_routing.py` | Requests | HTTP parsing, query extraction, handler dispatch, response accounting |

The HTTP case executes handlers in process. It deliberately covers bounded
generated requests rather than a production parser; socket throughput,
concurrency, and latency distributions are future separate cases.

## Common scenario pattern

Keep `NAME`, `DESCRIPTION`, `DEFAULT_SIZE`, and `DEFAULT_SEED` at module scope.
`prepare(size, seed)` constructs repeatable runtime input. `untyped(data)`,
`partial(data)`, and `typed(data)` implement the same algorithm and return a
deterministic tuple of integers. They differ in annotation coverage; setup is
shared and unmeasured. Typed means ordinary Python annotations, not an unsafe
policy or a promise that Mule currently specializes every annotation.

`main()` passes those functions and metadata to `perfutils.run_case`. All cases
accept `--size`, `--seed`, `--variant`, `--loops`, `--warmups`, and `--help`.
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

Copy `runtimes.example.toml` and add named runtimes. `command` is an argument
array, optionally accompanied by a `build` command and `version_command`.
Tokens support `{source}`, `{binary}`, and `{python}` (the harness interpreter).
`cwd` is resolved relative to the config. For other Mule checkouts, set both
their working directory and their own compiler-host Python executable.
The designated baseline must be a trusted CPython interpreter.

All native builds complete before validation or timing begins. Each case is
built once, containing all three kernels. The harness validates every runtime
and variant against the baseline's untyped result, then runs processes serially
with rotated runtime order. Each sample repeats validation. Build failures,
timeouts, and incorrect results appear in the report and produce a nonzero
exit status; other comparisons continue.

Generated files live in an ignored `perf/results/<timestamp>/`, or a fresh
directory selected with `--output`. Reports include raw samples, medians,
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
