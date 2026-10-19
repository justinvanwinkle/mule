# Vendored `pyperformance` benchmark suite

`perf/pyperformance/` contains the 52 standard-library benchmarks from the
upstream [`python/pyperformance`](https://github.com/python/pyperformance)
suite (those without third-party PyPI `requirements.txt`), alongside a
self-contained `pyperf.py` compatibility harness that compiles natively with
Mule and executes identically under CPython 3.15t.

Upstream source and data files under `benchmarks/` are byte-for-byte unchanged;
[`UPSTREAM.json`](file:///home/jvanwinkle/repos/mule/perf/pyperformance/UPSTREAM.json)
records the upstream repository URL, commit hash, retrieval date, and SHA-256
digest of every vendored file. [`COPYING`](file:///home/jvanwinkle/repos/mule/perf/pyperformance/COPYING)
preserves the upstream MIT license.

## Usage

```sh
# List all 52 vendored standard-library benchmarks
uv run python -m perf.pyperformance list

# Compare CPython 3.15t against Mule (-O2) on selected benchmarks
uv run python -m perf.pyperformance run --runtime mule-o2 \
  nbody richards richards_super deltablue fannkuch nqueens float \
  spectral_norm chaos go hexiom meteor_contest unpack_sequence \
  comprehensions generators coroutines json_dumps json_loads \
  pidigits telco pathlib deepcopy gc_collect gc_traversal

# Compare CPython 3.15t against both Mule (-O2) and Mule (-O3)
uv run python -m perf.pyperformance run \
  nbody richards deltablue fannkuch float spectral_norm

# Run the entire in-process suite against Mule (-O2)
uv run python -m perf.pyperformance run --runtime mule-o2
```

Reports and build artifacts are written to `perf/results/pyperformance-<timestamp>/`
(`results.json` and `summary.txt`).
