# Python standard library sources

This directory contains the `.py` library files from the released
[Python 3.14.7](https://www.python.org/downloads/release/python-3147/), alongside
Mule's `mule.py`, `_native_class.py`, `_format.py`, and `_sequences.py`. The
underscored modules contain compiler-private protocols compiled by Mule itself.

The official source archive is
`https://www.python.org/ftp/python/3.14.7/Python-3.14.7.tar.xz`. Its verified SHA-256
is `3b48dac8fb59f62eaa67ac83c1eb12bda1b7a08406dd286e252c11a66be27f81`.
3.14.7 is the host CPython used as the conformance oracle, so the vendored
library and the oracle's library are the same release. It replaced an earlier
Python 3.15.0 import, whose lazy imports Mule's parser does not accept.
`LICENSE.cpython` preserves the release's PSF and historical license notices.
No CPython interpreter or native extension implementation
is included. The test tree preserves binary and intentionally invalid source
fixtures as test data; these are not runtime dependencies.

655 upstream Python files are included. 39 files are adapted to select existing
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
library yet. Language and runtime facilities the library uses still need
compiler support. Import/bootstrap code retains upstream Python
semantics; it must be adapted to Mule's compiled-module loading before use by
generated executables. It must never call a CPython runtime or host evaluator.
The release's 46 non-Python library resources are included for the upstream tests,
including IDLE configuration and ensurepip's bundled wheel.

The imported Python code preserves upstream language idioms; this task does not
refactor the standard library's algorithms or type inspection into Mule's own
compiler implementation style.

## Reproduce the import

From the repository root, with the official archive downloaded:

```sh
uv run python -m tools.vendor_stdlib build/python-3.14.7/Python-3.14.7.tar.xz
uv run pytest test/stdlib/test_python_fallbacks.py
```

The tool verifies the release checksum before writing anything and selects exact,
audited fallback blocks. Mule's own library files are not overwritten.
The fallback smoke tests exercise selected compatible Python algorithms on the
external test host; they do not substitute that host for Mule execution.

## Complete upstream tests

All 1,660 files under the release's `Lib/test` and all 69 files under
`idlelib/idle_test` are preserved unchanged, including support code, tests requiring
native extensions, and 515 non-Python fixture files. Their accelerator imports,
decorators and assertions are retained. Tests for missing native extensions are
not excluded.

```sh
uv run python -m tools.run_stdlib_tests
uv run pytest test/stdlib/test_upstream.py
```

Both commands attempt all 492 entries discovered by Python's upstream rules.
Discovery reads `SPLITTESTDIRS` from the shipped `findtests.py` with Mule's parser.
It splits only the packages upstream splits; other packages remain whole so their
`load_tests` hooks can select nested tests and doctests. The pytest entries are also
part of the default `uv run pytest` suite. Current compilation gaps are marked
`xfail(strict=True)`: every entry is still attempted, and an unexpected pass fails
the run so its expected-failure mark can be removed. Runner errors other than the
failed-outcome assertion remain failures.

To report the gaps as ordinary failures, use pytest's `--runxfail` option:

```sh
uv run pytest test/stdlib/test_upstream.py --runxfail
```

Each entry goes through Mule's native compiler. Simple suites consisting of
ordinary undecorated `unittest.TestCase` classes can use the bootstrap driver:
Mule compiles the unchanged upstream module, a small assertion implementation
in `bootstrap/unittest.py`, and a driver that invokes every discovered test
method. Each method runs in a separate process using the same native executable.
Assertions and runtime errors are recorded individually, and later cases still
run. Bootstrap discovery rejects fixtures, decorators, package hooks and dynamic
discovery rather than silently dropping them. It uses Mule's AST and does not
evaluate test bodies on the host.

Other entries first require standalone source compilation, then a compiled
driver imports that suite and calls
`unittest.TestLoader.loadTestsFromModule`, respecting its hooks. Test execution
requires a native ELF executable; there is no host-Python fallback. All 15 resource
categories are enabled, including the four omitted by upstream's `-uall`.
Loader errors, zero executed tests, unavailable/skipped tests and upstream
expected failures all make the run unsuccessful. The full unittest loader is
still a compilation target, not an implemented native facility.

The unchanged `test_int_literal.py` is now executed: all six binary, octal and
hexadecimal literal tests pass under Mule. Its pytest entry has no xfail mark.

```sh
uv run python -m tools.run_stdlib_tests --suite test_int_literal
```

The standalone command continues after failed entries and returns nonzero if any
fail; it reports raw results independently of pytest's expected-failure marks.
`build/stdlib-tests/results.json` records every result and distinguishes
compilation failures from executed test results. Each entry also has its own
`outcome.json`. `--list` lists the complete selection; `--jobs` controls parallel
suite attempts and `--timeout` bounds each compilation/execution. Repeat `--suite`
to select entries; omitting it continues to attempt the full upstream selection.

The original Mule baseline, against the 3.15.0 import, was 557 compilation
failures and zero executed cases.
The current execution milestone is one passing suite with six passing cases.
`test_longexp` also reaches its individual test and fails because `eval` is not
implemented. The remaining selection is still expected to fail and does not
establish full Python conformance. See `spec/compiled-modules.md` for bootstrap
limitations.

`_arguments.py` is a compiler-private compiled constructor protocol. Signature-specific
argument binders are generated as Mule Python by `mule/compiler/argument_protocol.py`;
Rust supplies borrowed call buffers and captured-default storage.
