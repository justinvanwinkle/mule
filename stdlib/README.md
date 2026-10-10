# Python standard library sources

This directory contains the `.py` library files from the released
[Python 3.15.0](https://www.python.org/downloads/release/python-3150/), alongside
Mule's existing `mule.py` and `_native_class.py`.

The official source archive is
`https://www.python.org/ftp/python/3.15.0/Python-3.15.0.tar.xz`. Its verified SHA-256
is `ba4bed1ba346b916890b76d9e320451420aa69f6408997d33c66482eeae3d575`.
`LICENSE.cpython` preserves the release's PSF and historical license notices.
`UPSTREAM.json` records the archive identity and upstream/vendored hashes of every
copied Python file. No Python runtime binaries, extension modules, C sources,
bytecode, regression-test directories or non-Python resource files are included.

693 upstream Python files are included. 39 files are adapted to select existing
Python implementations rather than import optional native accelerators. These
include bisect, heapq, collections helpers, functools, operator, statistics, JSON,
decimal, datetime, pickle, queues, ABCs, warnings, asyncio, ElementTree, zoneinfo,
UUID helpers, type helpers, path normalization and exception suggestions. Frozen
importlib references are routed to the Python sources.

`io` exports the standalone `_pyio` implementation. The original circular import
and registration against C stream classes are removed; upstream `Reader`/`Writer`
protocols move into `_pyio`. The Python thread-local initialization runs after
threading has defined its functions and registered the main thread.

Native imports that have no Python implementation remain explicit Mule
implementation requirements. Examples include OS operations, clocks, threads,
weak references, codecs, sockets, math, regex matching, hashing, `struct`, and
`collections.deque`/`defaultdict`. `hmac.compare_digest` needs a secure primitive;
it is not replaced with a timing-sensitive Python comparison. The Python pickle
fallback lacks `PickleBuffer`; the Python `types` fallback lacks `CapsuleType`
and `LazyImportType`.

This is source setup, not a claim that Mule can compile or execute the full
library yet. Python 3.15 lazy imports and other new language/runtime facilities
still need compiler support. Import/bootstrap code retains upstream Python
semantics; it must be adapted to Mule's compiled-module loading before use by
generated executables. It must never call a CPython runtime or host evaluator.
Modules requiring excluded data resources will need those resources separately.

The imported Python code preserves upstream language idioms; this task does not
refactor the standard library's algorithms or type inspection into Mule's own
compiler implementation style.

## Reproduce the import

From the repository root, with the official archive downloaded:

```sh
uv run python -m tools.vendor_stdlib build/python-3.15.0/Python-3.15.0.tar.xz
uv run pytest test/stdlib
```

The tool verifies the release checksum before writing anything and selects exact,
audited fallback blocks. Mule's two existing library files are not overwritten.
The tests exercise selected compatible Python algorithms on the external test
host; they do not substitute that host for Mule execution.
