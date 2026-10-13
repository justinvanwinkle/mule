# Compiled modules and the first upstream test execution

Status: implemented initial static module linking and native test execution.
All six methods in the unchanged Python 3.14 `test.test_int_literal` module execute
and pass. General unittest support remains incomplete.

## SBCL correspondence and required adaptations

### Process environment and its Python mapping

`posix.putenv` and `posix.unsetenv` change the process environment through native
OS primitives. They do not mutate the imported `posix.environ` dictionary.
The compiled `os._Environ` methods own updates to their backing dictionary;
deletion therefore removes a key once, and mapping `pop` can return its value
without a second deletion raising `KeyError`. Direct POSIX changes leave the
Python mapping unchanged, as in CPython.

SBCL `contrib/sb-posix/interface.lisp`, the `setenv` and `unsetenv` `define-call`
forms and `putenv`, changes native environment storage through foreign calls.
Mule keeps that OS boundary. Python additionally exposes a cached dictionary
and an `os.environ` wrapper over it; that mapping policy stays in compiled
Python. Removing dictionary mutation from the POSIX wrappers avoids duplicate
updates and does not add runtime dispatch or change the native call cost.

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

The executable's `__main__` namespace enters the same module registry before
its first source statement, after generated LLVM links its global cells. If
`sys` is imported later, its registration receives this namespace along with
the early imports. `import __main__`, `sys.modules['__main__']`, and functions'
captured globals therefore refer to the same mutable namespace. String
`ClassVar` and `InitVar` annotations resolve through that namespace's actual
imports and aliases; there is no fallback to an unrelated typing namespace.
Static resolution of `__main__` selects registry lookup instead of compiling
a same-named source file that would create a second executable namespace.

SBCL `src/code/load.lisp`, `aux-fop-intern`, establishes package symbol identity
before `fop-funcall*` executes compiled initialization. Python additionally
requires the executable namespace to be an importable module object, even when
no source file named `__main__.py` exists. Generated LLVM publishes the existing
namespace through the same registry primitive used for imported modules; this
adds one startup registration, without another registry or runtime evaluator.
Before `sys` creates its compiled dictionary, registry lookup borrows the
matching entry from the existing early-registration storage and returns a
retained reference. This keeps early `import __main__` on the same object;
after registration, lookup uses the compiled dictionary as usual.
Shutdown releases early registry references even if `sys` was never imported.

Failed initialization removes the module's registry entry before releasing its
namespace and resetting the initializer cache. This applies both before `sys`
registers `sys.modules` and afterward. A later import executes the body again;
the original exception is preserved across cleanup. Generated LLVM selects this
failure path; the existing native registry owner releases its retained entry,
using compiled dictionary `pop` when `sys.modules` is available. Successful
dependencies remain registered.

SBCL `src/code/load.lisp`, `aux-fop-intern`, resolves package symbols before
compiled references use them; `src/compiler/ir1tran.lisp`, `find-free-var`,
canonicalizes global leaves, and `src/compiler/x86-64/cell.lisp`,
`%set-symbol-global-value`, writes the symbol's value slot directly. Mule retains
fixed global cells and compiled initialization, but Python imports additionally
require removing a failed module registration so it can be retried. This cleanup
adds a compiled dictionary call only on initialization failure; it introduces no
source evaluation or Rust dictionary algorithm. Registry storage remains existing
native bootstrap infrastructure, not a new language-level import implementation.

This supports retries when the registry and initializer cache are the failed
namespace's only remaining owners. A failed namespace retained through a circular
import remains a gap: its compiled functions and a later retry share fixed static
global cells. Independent simultaneous namespaces for one compiled module require
additional compiler cell-ownership machinery; registry cleanup does not provide it.

Ordinary module attribute access routes through the compiled `module` class's
`__getattribute__`. Stored namespace values, including None, retain precedence
under the existing namespace lookup. After ordinary lookup raises AttributeError,
compiled Python looks up the namespace's `__getattr__` without invoking hooks and
calls it with the requested attribute string, without binding a module receiver.
An absent hook preserves the original AttributeError; a present non-callable hook
or any exception from the hook propagates normally. Hooks can cache results through
`globals()`. Global-variable reads and explicit `object.__getattribute__` bypass
this fallback. Direct access, `getattr`, `hasattr`, and ordinary from-import
attribute lookup use this path. Raw child-submodule presence checks and star
imports still bypass it and remain compatibility gaps.

SBCL `src/code/target-package.lisp`, `find-symbol`, implements namespace policy in
compiled Lisp over physical lookup; `src/compiler/ir1tran.lisp`, `find-free-var`,
keeps global-leaf resolution separate. Python's module fallback has no identical
SBCL hook. Mule implements that semantic adaptation in compiled Python, reusing
the runtime's existing class-hook invocation and hooks-disabled storage lookup.
It adds a compiled call to ordinary module attribute access, not a new Rust
namespace algorithm, evaluator, cache, or binding store.

The module dictionary property chooses the live view of its existing storage:
imported native namespaces use `_GlobalsDict`, while constructed module objects
and their subclasses use `_InstanceDict`. Dictionary writes, attribute writes,
and deletion therefore share the same owner; copying an imported module's
bindings into a constructed module does not create another attribute store.
SBCL `src/pcl/slots.lisp`, `standard-instance-access` and `slot-value-using-class`,
similarly read and write the instance's existing slot storage. Python's module
dictionary view is an additional compiled-language interface to that storage,
not a new namespace allocation or class-identity scheme.

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
