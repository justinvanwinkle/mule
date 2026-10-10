# CPython-compatible C extension interface

Status: required compatibility surface and compiler consequences. The
[initial native compiler](native-compiler.md) links CPython 3.14's object runtime
and exposes native functions through its C callable interface. A Mule extension
build SDK and full function/frame compatibility are not implemented yet; extension
loading and arbitrary extension compatibility have not been validated.

Mule must expose a CPython-compatible interface so existing C extension source
can be built and used with Mule-compiled Python. This constrains the runtime and
implementation lowering described in [Python changes](python-changes.md).
SBCL's Alien/foreign-call facilities provide a reference for foreign-call
lowering, but a generic C FFI does not provide CPython extension compatibility.

## Compatibility means more than exported functions

Separate three contracts:

| Contract | Meaning and work involved |
| --- | --- |
| Source API compatibility | Existing extension source includes `Python.h` and uses compatible declarations, macros, layouts, and behavior when rebuilt for Mule. This is required by the requested ability to build extensions. |
| Release-specific binary ABI compatibility | An extension built for a particular CPython version/build/platform loads unchanged. Requires compatible symbols, calling conventions, data layouts, and runtime behavior. The exact supported build profiles remain open. |
| Limited API / Stable ABI (`abi3`) | Support the documented subset and exported ABI, including its selected minimum API version. This is useful additional coverage, not a replacement for the full source interface. |

Source API compatibility is required. Unchanged CPython binaries and `abi3`
coverage are separate possible guarantees; their supported profiles remain open.

CPython distinguishes its public API, Limited API, Stable ABI, unstable API,
and private implementation interfaces. Ordinary ABI compatibility is tied to
a minor release and compatible build/platform settings. `Py_LIMITED_API`
selects a source subset; it is not a switch that makes an arbitrary runtime
compatible. See [C API stability][stability].

Use CPython 3.14 as the initial reference for this design, consistent with the
frontend. Before implementation, record the supported OS/architecture, header
revision, release/debug configuration, and GIL/free-threaded configuration.
The compiler host's Python version is not automatically the target extension ABI.
Older language syntax support does not imply support for every older C ABI.

Public 3.14 APIs are the baseline surface to provide. Compatibility inventory
must additionally identify extension dependencies on private `_Py*` or
`PyUnstable_*` interfaces and generated-code assumptions; those require explicit
version-specific treatment. Do not claim arbitrary wheel compatibility from
passing a small extension fixture, or advertise `abi3` before verifying it.

## Runtime implementation choices

| Approach | Consequence |
| --- | --- |
| Use CPython's runtime/object model beneath Mule's LLVM-generated code | Reuse its objects, API implementation, collection, thread state, and extension loading. Integrate Mule-native functions, frames, exceptions, and module execution with that runtime. |
| Supply an independently implemented compatible runtime | Supply the headers, symbols, object layouts, lifecycle, type protocols, thread/exception state, and loader behavior required by the selected compatibility profile. |

The initial executable implementation uses CPython's runtime; a separate runtime
is not implemented. This choice makes the first native compiler executable without
settling the long-term runtime distribution or compatibility profile. Reusing
runtime services would not require using CPython's parser, AST, bytecode compiler,
or bytecode execution for Mule-compiled functions. Conversely, wrapping a native
entry as a generic callable would not by itself establish function/frame introspection
compatibility. Those contracts require explicit implementation either way.

Both approaches need one coherent identity/lifetime model for objects shared
with extensions. An unrelated heap plus copies at each crossing cannot preserve
identity, shared mutation, and extension-retained pointers. If a hybrid heap is
chosen later, stable canonical objects and cross-heap ownership/collection must
be specified before relying on it.

## Object and type interface

The public object structures include `PyObject`, `PyVarObject`, `PyObject_HEAD`,
`PyVarObject_HEAD`, and scalar types such as `Py_ssize_t`. Full-API headers can
expose layout-dependent accessors/macros; extensions can embed object headers
inside their own structs. Declarations alone do not provide the corresponding
memory layout or behavior. See [common object structures][structures].

Implement compatible static/heap type construction, `PyTypeObject`, readiness,
allocation/deallocation, and the relevant slots: calls, number/sequence/mapping
operations, attribute/descriptor access, iteration, comparison, buffers, and
collection. Extension-defined instances participate in the same operations as
Mule-created objects. See [type object structures][types].

Consequences for lowering:

- A general boxed value crossing the interface is a valid compatible
  `PyObject *`, not an SBCL tagged word or opaque unrelated handle.
- Unboxed internal values are materialized before an object-requiring call;
  observable aliases share one object and do not become independent copies.
- References held by an extension can outlive the originating compiled call.
  Objects must retain a stable public address for that lifetime.
- Extension fields and callbacks remain valid under allocation, recursion,
  collection, and reentrant calls into Mule code.

These are design constraints. They do not require every internal temporary to
be boxed or every internal call to use the public C convention.

## Ownership, allocation, and collection

Honor each API's new/strong, borrowed, and stolen reference contract. Emit
retains/releases and exceptional-path cleanup accordingly. `Py_INCREF`,
`Py_DECREF`, null-tolerant variants, and immortality behavior must agree with the
selected headers/runtime. A borrowed reference is not automatically safe across
callbacks or mutations that can release its owner.

Decrementing a reference can invoke deallocation and arbitrary Python code.
Publish consistent container/cell state before releasing an old object, and
model that release as potentially reentrant. Treating every decrement as a pure
machine operation would invalidate optimizer assumptions.
See [reference counting][refcount] and [C API ownership conventions][intro].

Preserve allocator pairing and domains. Extension types can own native memory
and Python references. Their traversal/clear hooks participate in cyclic
collection; keep native-frame references, cells, and suspended state reachable
as required. Implement lifecycle behavior for weak references, finalization,
and resurrection rather than freeing an object solely because LLVM no longer
uses a pointer. See [cyclic collection support][gc] and [memory management][memory].

LLVM register liveness cannot establish Python reference ownership or collector
reachability. The selected runtime must define the required roots and barriers.

## Calls, callbacks, and failure paths

Support CPython callable protocols in both directions: compiled code calls
extension functions and extension code calls Mule-compiled functions.
`tp_call` uses positional tuples and keyword dictionaries; vectorcall uses its
documented argument array/count/keyword tuple convention. If vectorcall is
supported, its behavior agrees with `tp_call`. See [call protocols][calls].

Dispatch `PyMethodDef` flags with their corresponding C signatures, including
`METH_VARARGS`, `METH_KEYWORDS`, `METH_NOARGS`, `METH_O`, and fast-call variants.
Argument parsing and value construction APIs operate on the shared object model.
Native callback adapters must obey the target C ABI, independent of any optimized
internal convention. See [method structures and flags][structures].

Preserve the per-thread error indicator and each API's error sentinel: `NULL`,
`-1`, or another documented result. A Python exception from a callback must
translate back into that convention; a failing extension call must enter the
compiled function's handler/cleanup path. Do not interpret every `-1` as failure
without consulting the API's contract/error state.
Maintain raised versus handled exception state, traceback, normalization, and
chaining as required. See [exception handling][exceptions].

CPython-style errors do not require C++ exceptions or a particular LLVM unwind
strategy. Internal lowering may use explicit failure branches; the externally
observable convention remains the same.

## Threads, interpreters, and reentrancy

Implement attached thread state and the selected locking semantics.
`Py_BEGIN_ALLOW_THREADS` / `Py_END_ALLOW_THREADS` detach and restore state;
extensions can create native threads and attach before accessing Python objects.
A free-threaded runtime still requires attached thread state. The selected ABI
must reflect its build configuration. See [thread state and the GIL][threads].

Compiled code can be reentered from native callbacks while its previous activation
is live. Keep ownership, exception state, and exposed frame/module state valid
across that reentry. Do not treat an extension call as a leaf operation by default.

Module state and object references belong to the appropriate interpreter/module
instance. Subinterpreter and free-threading support need explicit compatibility
claims; unsupported cases must fail clearly rather than sharing incompatible
state. See [multiple interpreters][interpreters].

## Native memory protocols

Provide the buffer protocol, including `Py_buffer` shape/stride/format metadata,
readonly flags, export ownership, and matching release. A consumer can retain
a buffer across compiled calls; its backing memory cannot move or disappear
while the export is valid. See [buffer protocol][buffers].

Capsules carry native pointers, context, and destructor behavior used to share
interfaces between extensions. Preserve their pointer identity and lifecycle;
object conversion must not replace them with unrelated values.
See [capsules][capsules]. These protocols are part of extension interoperability,
not merely scalar argument conversion.

## Building, linking, and importing extensions

Provide a usable target SDK: `Python.h` and its dependent headers, consistent
configuration/version definitions, include/library locations, and required
exported functions/data. Existing source must compile against that SDK without
being rewritten to a Mule-only extension API. The exact SDK packaging is open.

Build configuration must report correct compiler/linker flags, library and
installation paths, and extension suffix/ABI settings. If standard build tools
use `sysconfig`, its values must describe the target runtime consistently.
See [sysconfig][sysconfig]. Copying the compiler host's configuration would be
incorrect for another target.

The loader must discover the platform shared-library suffix, resolve its exports,
and call the correct initialization symbol (`PyInit_<module>` for ASCII module
identifiers, the specified encoded form otherwise). Support both legacy
single-phase initialization and multi-phase definitions, creation, and execution.
See [defining extension modules][extension-modules].

Implement per-module state, method/type registration, traversal/clear/free hooks,
initialization errors, and applicable interpreter/GIL capability declarations.
Respect Python's module/import state during initialization and failure.
See [module objects][modules] and [import API][imports]. Keep dependency resolution
and symbol visibility consistent with the supported platform.

Building an extension also runs a build frontend/backend. Running those tools
under Mule depends on broader Python/standard-library support; C compilation
and extension loading are distinct guarantees. A build using another supported
Python host can still target Mule's SDK when the tools support that separation.

## Function, frame, and execution APIs

Extensions can inspect functions, code, frames, locals, traceback, and tracing
state. Native compilation must provide the selected public interface's
observable state; native debug metadata alone does not satisfy it.
Frame/tracing facilities are documented under [profiling and tracing][profiling].

APIs that compile/evaluate Python source also require an execution strategy.
Routing their inputs through Mule must use Mule's parser and LLVM pipeline.
If a selected runtime executes other CPython code objects, define that separate
interoperability boundary explicitly. This document does not choose a bytecode
fallback for Mule functions.

## Evidence required before a compatibility claim

Compatibility checks must build and import real C/C++ fixtures against the
target SDK, then exercise them from compiled Python. A future suite should cover:

| Fixture | Observable contract |
| --- | --- |
| Scalar/container API use | Values, identity, mutation, and documented argument/error behavior. |
| Extension-defined type | Layout/macros, slots, inheritance, allocation, deallocation. |
| Ownership and callback reentry | Retained/borrowed/stolen references, finalization, valid state on errors. |
| C-to-Mule callback | Parameter binding, return values, exceptions, recursive calls. |
| Buffer and capsule exchange | Shared native memory, export/release lifetime, pointer/destructor behavior. |
| Cyclic extension objects | Traversal, clearing, collection, weak references and finalization. |
| Single/multi-phase module | Import, module-instance state, initialization failure cleanup. |
| Native thread callback | Detach/attach and locking semantics for the supported runtime profile. |
| Limited API and binary fixtures | Actual supported ABI profiles, independently from source rebuild coverage. |

Track supported API families, header/profile settings, and observed extension
gaps. Successful LLVM emission or a library that merely links is insufficient
evidence for runtime compatibility.

[stability]: https://docs.python.org/3.14/c-api/stable.html
[intro]: https://docs.python.org/3.14/c-api/intro.html
[structures]: https://docs.python.org/3.14/c-api/structures.html
[types]: https://docs.python.org/3.14/c-api/typeobj.html
[refcount]: https://docs.python.org/3.14/c-api/refcounting.html
[gc]: https://docs.python.org/3.14/c-api/gcsupport.html
[memory]: https://docs.python.org/3.14/c-api/memory.html
[calls]: https://docs.python.org/3.14/c-api/call.html
[exceptions]: https://docs.python.org/3.14/c-api/exceptions.html
[threads]: https://docs.python.org/3.14/c-api/threads.html
[interpreters]: https://docs.python.org/3.14/c-api/subinterpreters.html
[buffers]: https://docs.python.org/3.14/c-api/buffer.html
[capsules]: https://docs.python.org/3.14/c-api/capsule.html
[sysconfig]: https://docs.python.org/3.14/library/sysconfig.html
[extension-modules]: https://docs.python.org/3.14/c-api/extension-modules.html
[modules]: https://docs.python.org/3.14/c-api/module.html
[imports]: https://docs.python.org/3.14/c-api/import.html
[profiling]: https://docs.python.org/3.14/c-api/profiling.html
