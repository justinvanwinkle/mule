# Compiled modules and the first upstream test execution

Status: implemented initial static module linking and native test execution.
All six methods in the unchanged Python 3.15 `test.test_int_literal` module execute
and pass. General unittest support remains incomplete.

## SBCL correspondence and required adaptations

SBCL `src/compiler/main.lisp`, `sub-compile-file`, processes top-level forms
through IR1 and emits compiled functions and load-time initialization. Its
`src/code/load.lisp`, `load-as-fasl` and `fop-funcall*`, loads artifacts and invokes
compiled initialization; compiling a file does not execute its tests. Mule must
likewise compile module bodies and functions, then execute initialization at the
import site. Python requires distinct module globals, once-only initialization,
module names, and access to exported bindings. These are Python adaptations of
compiled initialization and namespace resolution, not an evaluator fallback.

The first implementation statically includes reachable Python modules in one
LLVM artifact. Each module has separately named compiler functions, its own
namespace and a cached initializer. Generated LLVM performs initialization;
Rust supplies existing namespace storage and reference ownership. Module caches
own their namespaces until executable shutdown, preserving captured globals.
Dynamic module discovery, relative imports, extension loading, and concurrent
imports remain future work. Unsupported source remains a compilation error.

SBCL `tests/test-util.lisp`, `run-test`, invokes a test function and records errors
without abandoning later tests. Mule currently has no general exception handlers.
The initial test harness therefore runs each selected test method in a separate
invocation of the same native executable. It reports assertion failures and
runtime errors and continues with the remaining cases. Process isolation costs
startup time and does not preserve cross-test module state; it is an explicit
bootstrap test-runner limitation, not general unittest compatibility.

Runtime imports of `typing` and `typing_extensions` are ordinary module imports;
they are no longer silently discarded by the compiler. Their imported sources
must compile before execution can succeed. Arbitrary deferred annotations remain
legal independently of whether the compiler understands their metadata.

The host harness discovers ordinary undecorated TestCase classes and test methods
using Mule AST visitors, then generates direct native calls. It does not evaluate
test bodies, change assertions, prune methods, or run them in host Python.
Hooks, parameterized tests, fixtures, inheritance-based discovery and the complete
upstream unittest loader remain unsupported by this initial harness and must not
be silently omitted. The full runner remains available for those entries.

`stdlib/bootstrap/unittest.py` is a small Mule Python implementation of the
assertion interface used by the selected module. It precedes the vendored stdlib
in native module resolution; the released unittest source remains unchanged.
Assertion predicates and diagnostic construction compile through the normal
frontend. An assertion statement branches to a primitive that records an
AssertionError and follows the compiler's existing owned-reference cleanup.
No Rust test framework, source interpreter or CPython linkage is introduced.

SBCL `src/code/macros.lisp`, `assert`, expands a predicate into control flow and
calls `assert-error` only on failure. Mule likewise lowers the predicate and lazy
diagnostic into ordinary IR branches. Python assertions signal AssertionError;
they do not offer Common Lisp's corrective places and restarts. This semantic
adaptation preserves message evaluation only when the predicate fails.
