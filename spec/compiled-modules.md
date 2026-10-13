# Compiled modules and the first upstream test execution

Status: implemented initial static module linking and native test execution.
All six methods in the unchanged Python 3.14 `test.test_int_literal` module execute
and pass. General unittest support remains incomplete.

## SBCL correspondence and required adaptations

### Initial sys module

`sys` is compiled from `stdlib/sys.py`, with shared decoding in
`stdlib/_mule_codecs.py`. Both resolve to repository implementation sources before
source-local files or packages. `_mule_native` is a built-in native module, not
a Python source file. Ordinary modules retain their existing resolution order.
Python source imports native exports explicitly; there are no undeclared process
intrinsics or module-specific identifier substitutions.

Generated native `main(argc, argv)` publishes borrowed, process-lifetime argument
pointers before any compiled module initialization. The native module exports the
raw startup count and an opaque read-only view of the pointer vector. Compiled
Python reads its elements, copies each C string to bytes, decodes those bytes with
the shared compiled UTF-8 decoder, and constructs the mutable `sys.argv` list.
Rust does not select Python arguments, decode their bytes, or construct the list.
The module cache owns it through normal module globals: repeated imports share
mutations and reassignment, and
each executable invocation constructs its own state. `argv[0]` is the native
executable invocation string, not a canonical path or the compiler's source path.
An empty native argument vector produces an empty list.

SBCL `src/runtime/runtime.c`, `main`, publishes `options.argv` as `posix_argv`
before entering Lisp. `src/code/common-os.lisp`, `os-cold-init-or-reinit`, uses
foreign pointer access to construct the language-level `*posix-argv*` list. Mule
keeps that boundary: the OS storage is native, while the list and module state
are compiled Python. Python requires a mutable list in a module rather than a
Lisp special variable. LLVM's executable entry receives the platform argc/argv
ABI directly because Mule has no separate native launcher processing options.

SBCL `src/code/target-alieneval.lisp`, `extern-alien` and
`define-alien-routine`, declares foreign access explicitly and constructs callable
interfaces. `src/code/c-call.lisp`, C-string naturalization, delegates encoding
conversion to the shared language-level external-format implementation;
`src/code/external-formats/enc-basic.lisp`, `read-from-c-string/utf-8/lf`, validates
and constructs decoded strings in Lisp. Mule's explicit native module exports
provide ordinary imported callable values using Mule's existing boxed-value ABI.
This is an initial fixed set of foreign-storage facilities, not arbitrary foreign
symbol loading or a general user-authored FFI declaration system. Python decoding
and argument traversal remain normal compiled language code.

Initial decoding remains strictly UTF-8. Compiled Python checks leading and
continuation bytes, length, overlong sequences, surrogates, and the maximum
codepoint, raising `UnicodeDecodeError` with encoding, data, offsets, and reason
in its argument tuple. No bytes are replaced or ignored. Unlike Python's POSIX
filesystem decoding, locale-derived encoding and surrogateescape remain gaps.
The current exception implementation also lacks the specialized properties and
formatting of Python's `UnicodeDecodeError`. Text storage already permits lone
surrogates; the missing surrogateescape behavior is codec policy, not a missing
storage representation. Decoding is linear in byte length, collects codepoints
in a Python list, then calls generic physical text construction once. This avoids
repeated immutable-string concatenation and its quadratic copying cost.

### Explicit native storage module

The existing generated import cache owns the `_mule_native` namespace and its
real function objects. Native initialization is an explicit import variant: it
populates the same namespace, returns owned success or null with an error, and
uses the existing failure cache reset and reference cleanup. It has `__name__`
but no fabricated `__file__`. Imports, aliases, passing callables, and module
attribute rebinding retain ordinary Python behavior; no call-site spelling is
rewritten. This does not introduce a separate runtime module registry or evaluator.

Explicit module bindings, including import aliases, take precedence over implicit
protocol primitive bindings, both for stores and for global reads inside functions.
Unshadowed protocol primitives still resolve to known functions. SBCL separates
these lookups in `src/compiler/ir1tran.lisp`, `find-free-var` and `find-free-fun`;
Mule must account for Python's shared variable/callable namespace while retaining
SBCL's invariant that assignment destinations are variable leaves, not function
leaves. Shadowed primitives use ordinary mutable globals rather than unconditional
primitive lowering.

Exports are positional-only physical operations:

| Export | Storage contract |
| --- | --- |
| argc | Native startup integer count. |
| argv | Read-only borrowed pointer vector; includes the NULL sentinel for nonempty native vectors. |
| read_pointer(vector, index) | Guard the vector kind and index; return an opaque borrowed C pointer, or None for NULL. |
| read_c_string(pointer) | Copy valid NUL-terminated foreign storage into owned bytes without decoding. |
| byte_length(data) | Guard byte storage and return its physical byte length. |
| read_byte(data, index) | Guard byte storage and bounds, then return an unsigned byte integer. |
| text_from_codepoints(values) | Guard list/tuple integer storage and codepoint bounds, encode the existing text representation, and allocate owned text. |
| math_number_kind(value) | Return physical kind 0 for other storage, 1 for integer/boolean storage, or 2 for binary64 storage. |
| math_to_double(value) | Convert builtin numeric storage to an owned binary64 value using the existing checked conversion. |
| math_integer_value(value) | Validate/retain integer storage and normalize boolean storage to an integer. |
| math_integer_bit_length(value) | Return the bit length of the stored integer magnitude. |
| math_special_method(value, attribute) | Return the existing special-method binding or None; validate text attribute storage. |
| math_sqrt(value) | Apply physical binary64 square root to float storage, returning the raw IEEE result; Python chooses domain errors. |
| math_inf, math_nan | Owned binary64 infinity and quiet-NaN constants. |

Only native-created pointer views are available; Python cannot construct one
from an arbitrary integer. Their storage is process-lifetime borrowed and is
never freed by the wrappers. The startup ABI guarantees valid NUL termination
and vector storage; generic C-string access relies on that explicit foreign-memory
contract. Vector bounds and pointer kinds are checked before dereferencing.
The view is read-only, but native-module attributes are ordinarily rebindable.
Compared with SBCL's raw alien pointers, these opaque checked views are a safety
adaptation with one small wrapper allocation per pointer read and a byte copy per
C-string read. Rust handles physical access, allocation, and storage checks; it
does not choose a decoding policy or expose argument-to-text operations.

The `chr` builtin added with this work also imports `text_from_codepoints`
explicitly. Older class/sequence/format private intrinsics remain bootstrap debt
outside this migration; new sys and codec code do not depend on that convention.

Only `argv` is available in this increment. Standard streams, descriptor access,
buffering, text codecs, and `detach` remain work to enable the existing compiled
Python `_pyio` implementation. This increment does not add a second stream
hierarchy or extend Rust with stream algorithms, and does not establish that
the unmodified Benchmarks Game programs compile or execute.

### Compiled initialization and imports

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
Dynamic module discovery, extension loading, and concurrent imports remain
future work. Unsupported source remains a compilation error.

Relative names are resolved against the importing module's package, following
the same static artifact resolution as absolute imports. SBCL's FASL loader
resolves compiled initialization and named bindings (`load-as-fasl` and
`fop-funcall*`); Python additionally requires package-relative module names and
`from package import child` fallback to a submodule. Mule checks the package's
binding at runtime first, so an existing binding (including None) wins. Only an
absent binding initializes the compiled submodule and attaches it to its parent.
Potential submodule source is statically compiled, never interpreted on demand.
Directly executed source without a package rejects relative imports. Python's
dynamic search-path mutation and runtime source compilation remain unsupported.

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
