# Python conformance programs

Authored programs that exercise Python 3.14 feature by feature, including
features nested inside each other and surprising-but-specified semantics.
Unlike `corpus/`, these are not upstream programs and carry no metadata: CPython
is the oracle, and each program's expected behavior is whatever the host
interpreter prints.

| Directory | Covers |
| --- | --- |
| `numbers/` | Integer, bigint, float, complex and bool arithmetic, conversions, rounding, precedence, numeric descriptors and subclasses |
| `strings/` | `str`, `bytes` and `bytearray` methods, descriptors, subclasses, Unicode, slicing, `%`, `str.format`, f-strings, format specs, t-strings |
| `containers/` | Lists, tuples, dicts, dict views, sets, frozensets, descriptors, subclasses, slicing, unpacking, starred displays, comprehensions, copying |
| `builtins/` | `object` and `type` descriptors, iteration builtins, `range`, `slice`, numeric and attribute builtins, `print`, `repr`, hashing, `memoryview` |
| `control_flow/` | Branches, `for` loops over builtin iterables, loops with `else`, cleanup ordering, assignment evaluation order, walrus, every `match` pattern kind |
| `scoping/` | LEGB, `global`/`nonlocal`, class-body scope, `del`, exception-variable unbinding, `eval`/`exec`, PEP 695 scopes |
| `functions/` | Parameter kinds, binding errors, call unpacking, closures, decorators, introspection, annotations, generics |
| `generators/` | `send`/`throw`/`close`, `yield from`, PEP 479, generator expressions, cleanup |
| `async_/` | Coroutines, awaitables, `async with`/`for`, async generators and comprehensions, mostly on a hand-written loop |
| `classes/` | Inheritance and MRO, `super`, properties, descriptors, slots, metaclasses, dataclasses, enums, ABCs |
| `protocols/` | Operator overloading and reflection, comparisons, hashing, container, call, context-manager and conversion protocols |
| `exceptions/` | Hierarchy, handler ordering, `finally` overrides, chaining, exception groups and `except*`, abnormal exits |
| `stdlib/` | Everyday use of functools, itertools, collections, re, json, math, decimal, typing and others |
| `weird/` | Cross-feature stress programs and oddities: `t[0] += [1]`, self-referential containers, evaluation-order traces, a small interpreter |

## Rules for programs

- One facet per file, so one unsupported construct does not hide unrelated
  coverage. Start each file with a one-line docstring naming that facet.
- Print labeled values. Output must not depend on hash seeds, addresses, time,
  the environment or recursion limits; sort set contents before printing.
- Print `type(e).__name__` for exceptions raised by Python itself. Exit 0 unless
  the program tests abnormal termination (`exceptions/exit_*.py`).

## Run and record gaps

```sh
uv run pytest test/mule/compiler/test_conformance.py
uv run pytest test/mule/compiler/test_conformance.py --conformance-strict
uv run python -m tools.record_conformance_gaps
```

Each program has two tests. The reference test runs CPython with hash seeds 0
and 1 and requires identical results. The native test builds the program with
Mule and compares stdout and exit code with CPython; stderr is compared only
when CPython wrote none, because Mule has no tracebacks and CPython may print
compile-time warnings.

`gaps.toml` records each program Mule cannot yet compile or run identically: the
exact diagnostic, file and line for compile gaps, and the exact output for
runtime gaps. A recorded gap is an expected failure; any other failure fails the
test, and `--conformance-strict` makes recorded gaps fail too. Programs that
start matching CPython pass without editing `gaps.toml`. After compiler changes,
rerun the recording tool to regenerate the file and review its diff.
