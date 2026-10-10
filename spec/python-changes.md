# Changes required for Python and LLVM

Status: adaptation requirements, organized against the [SBCL baseline](sbcl-compiler.md).
The [initial native compiler](native-compiler.md) implements a small subset of
conversion, binding, control-flow IR, LLVM lowering, and runtime support. The
remaining stages and broader semantics below are requirements. A requirement
here follows from the requested language/backend/interface; details marked open
require a later decision.

## What carries over

Preserve SBCL's separation of language reasoning from implementation lowering:
AST conversion into an IR1 graph; interacting local-call, type, constraint, and
optimization phases; environment/cleanup analysis; then implementation lowering.
Carry over graph consistency checks, source provenance, bounded optimization,
and precise invalidation when transformations change dependencies.

The IR1 result model can retain the distinction between control positions and
value destinations. Python operations normally produce one object, so LVAR-like
results do not need Lisp's variable-count multiple-value protocol. Mapping this
graph to LLVM SSA is a lowering responsibility; relabeling SBCL LVARs as SSA
values would not perform that conversion.

## Lisp facilities that do not apply

| SBCL facility | Treatment for Mule |
| --- | --- |
| Lisp reader forms, reader macros, macro expansion, compiler macros, `MACROLET`, `SYMBOL-MACROLET` | Replace source handling with Mule's parser/AST. Python has no corresponding macro language. |
| `EVAL-WHEN`, compiler-time definition execution, `LOAD-TIME-VALUE`, `MAKE-LOAD-FORM` | Omit the Lisp protocols. Python module initialization and runtime object construction have their own timing. |
| Separate lexical variable/function namespaces, symbol packages, symbol value/function cells | Use Python binding and namespace rules. A callable is an ordinary value. |
| Special-variable dynamic binding and Lisp binding-stack restoration | Omit as a language feature. Keep generic protected-region and cleanup analysis. Python context managers and context variables have their own semantics. |
| `BLOCK`/`RETURN-FROM`, `TAGBODY`/`GO`, `CATCH`/`THROW` | Omit Lisp operators and exit tags. Lower Python structured control and exceptions; retain cleanup across exits. |
| Multiple values, `MV-COMBINATION`, unknown-value stacks and return conventions | Python returns one object; a tuple is an ordinary object. Internal multi-result helpers do not change Python semantics. |
| Lisp lambda lists, supplied-p variables, `&KEY` rules | Replace argument dispatch with Python's parameter/default/keyword rules. |
| `THE`, `DECLARE`, `DECLAIM`, Lisp type specifiers and unsafe declaration policies | Retain internal inferred facts; do not give Python annotations declaration semantics. |
| CLOS generic functions, Lisp class layouts, cons-specific and Lisp library transforms | Replace operation metadata and object semantics with Python's model. Dynamic dispatch remains necessary. |
| SBCL lowtags/widetags, number stack, architecture VOPs, assembler, FASL/core image formats | Replace representations and output mechanisms as required by LLVM and the CPython interface. Their generic responsibilities remain. |
| SBCL's Lisp cross-compilation/bootstrap machinery | Not part of the language compiler to port. Mule's Python 3.14 host and eventual bootstrap are separate concerns. |

The omissions above remove Lisp semantics, not analysis capabilities. For
example, removing special bindings does not remove exception cleanup; removing
multiple-value stacks does not remove closure or suspension storage analysis.

## Conversion, binding, and semantic validation

Scope classification must precede conversion of binding accesses. Scan each
block for bindings and `global`/`nonlocal` declarations; distinguish local slots,
cells/free variables, module globals, and namespace lookup. An assignment later
in a function can make an earlier read local and raise `UnboundLocalError`.
Class bodies use a namespace; ordinary methods do not capture that namespace as
an enclosing function scope. Annotation scopes have distinct class visibility
and lazy evaluation rules. See the [Python execution model][execution].

These rules extend SBCL's conversion-time resolution. Resolving a Python global
identifies its lookup operation, not its permanent value. Explicitly represent
unbound slots and deleted bindings instead of using `None` as an absence marker.
Track binding identity through graph rewrites and closure conversion.

Semantic validation belongs before optimization can erase invalid constructs.
It must cover function/loop/async context, declarations and their binding
conflicts, parameter restrictions, comprehension/walrus scope restrictions,
and pattern capture consistency. A node in unreachable source still receives
the applicable compile-time validation. Preserve the original AST span when
reporting failure. Parsing success alone does not establish these guarantees.

Python 3.14 is the current frontend reference. Supporting earlier language
versions also requires their semantic rules; accepting their syntax is not a
complete version-compatibility policy.

## Calls and function construction

Retain SBCL's internal-entry/general-entry distinction. General Python calls
must bind positional-only, positional-or-keyword, keyword-only, variadic
positional, and variadic keyword parameters and diagnose invalid combinations.
Expand `*`/`**` arguments according to Python's rules. Defaults are stored
objects computed when the definition executes, not on each call. Function
construction also applies decorators and captures closure/module state.
See [compound statements][compound] and [call expressions][expressions].

The AST retains argument syntax order. Conversion must still implement Python's
specified call processing, including starred positional arguments that may
appear after keyword syntax; do not infer all runtime ordering from dataclass
traversal. Preserve argument-evaluation effects even if parameter binding fails.

A local body can be called directly only when analysis establishes the actual
callable and its applicable entry. Global binding, decorators, closure mutation,
and arbitrary callbacks can defeat that assumption. A guarded specialization
needs a general fallback on a failed guard. It cannot substitute a compile-time
function merely because an identifier matches a definition.

For example, in `def f(): return f()`, the call reads the current global `f`.
Saving the original function and rebinding `f` can change what that call does.
SBCL's optional self-call recognition policy cannot be copied as an unconditional
Python transformation.

## Types, known operations, and effects

Keep the CTYPE-style distinction between language facts and implementation
representation. Rewrite the type domain for Python objects: exact type versus
subclass possibilities, constants, integer ranges, callable identity, and
unknown values. Facts need justification from semantics, analysis, or runtime
guards; annotation syntax alone supplies none.

Python annotations are runtime metadata, not enforced declarations. Python 3.14
uses lazy evaluation by default; `from __future__ import annotations` retains
stringized annotations. Preserve the applicable introspection behavior;
evaluation can execute user code. Type aliases and type parameters also require
their runtime objects and lazy values. See [annotationlib][annotations]. A checked optimizer
guard must remain distinct from an annotation and from a user-visible type error.

Rebuild SBCL's known-operation database for proven Python operations and runtime
helpers. A binding spelled `len` is not sufficient evidence for the builtin.
An exact builtin object and suitable operands can justify specialized metadata;
subclasses and overridden methods can change behavior. Calls to extensions are
conservative unless the runtime contract supplies stronger guarantees.

Python operators, truth tests, attribute access, comparisons, iteration, hashing,
and descriptor access can invoke user code. Class construction invokes metaclass
and other hooks. Facts about mutable objects can expire after callbacks or
mutation. See the [data model][datamodel]. Effect information must include
possible exceptions, namespace/object mutation, allocation, callbacks, and
ownership changes that can run finalizers.

Unused values do not make operations removable. For example, `1 / 0` must still
raise when its result is ignored. Optimizer policy may control cost and debug
detail; it does not permit removing required errors or trusting annotations.
Recursive inference must retain SBCL's rule that optimistic intermediate facts
are not published as established types.

## Control flow, cleanup, and suspension

IR1 conversion must expose normal flow and the paths required by Python errors,
handlers, cleanup, and suspension. Preserve short-circuit operand results and
evaluate shared operands in chained comparisons once. Dynamic truth/comparison
operations are separate from a machine boolean branch. See [expressions][expressions].

Assignments preserve target ordering; augmented assignment evaluates its target
once and attempts in-place dispatch before normal binary fallback.
`return`, `raise`, `break`, and `continue` must cross the appropriate cleanup
regions; a `finally` body can replace a pending transfer. Exception state and
handler-bound variable cleanup survive these transfers.
See [simple statements][simple].

Adapt environment/cleanup analysis for context managers, `try`/`except`/`else`/
`finally`, and exception groups. A context manager may suppress an exception;
`except*` splits and recombines groups rather than acting as an ordinary handler.
Pattern matching lowers to protocol operations, tests, bindings, and guards,
including OR-pattern capture constraints. Do not give unsuccessful matches
stronger binding guarantees than Python specifies. See [compound statements][compound].

Generators, coroutines, and async generators introduce suspension, not just
non-local exits. Determine state that must outlive the native activation:
locals/cells, live values, resume position, exception state, and active cleanup.
Model iteration completion separately from failure. `send`, `throw`, `close`,
`yield from`, `await`, and async cleanup require their runtime protocols.
See the [generator and coroutine model][datamodel].

Tail-position analysis can remain useful internally. Removing Python-visible
frames requires a separate semantics decision because traceback, introspection,
and tracing behavior depend on them. LLVM tail-call support is not permission
to change that behavior.

## Environment and object representation

Retain closure analysis, captured-mutation cells, and escape analysis. Add
suspension storage and Python-visible function/frame state. Optimizations must
preserve shared cells, object identity, and all state observable across callbacks.
The [C extension requirement](cpython-interface.md) constrains boxed object
representation, ownership, and runtime-visible objects from the start.

Unboxing is an internal optimization. Python integers require arbitrary precision;
machine overflow needs a correct fallback. Python division/modulo and exception
behavior cannot be replaced blindly by native integer instructions. Boxing at
an observable boundary must preserve identity and ownership where relevant,
not merely equal values. An LLVM value has no implicit Python object lifetime.
LLVM's [GC support][llvm-gc] supplies integration mechanisms, not a collector
or Python ownership model.

## Fixed-layout classes and a Python runtime library

Mule's initial compiled classes use immutable class descriptors, indexed instance
fields, and native methods. The compiler discovers fields from assignments on
the first method parameter; single inheritance preserves inherited indices.
Attribute sites cache class identity and field or method index. Calls resolve
methods before argument evaluation and pass the receiver separately, avoiding a
temporary bound-method allocation. Saved bound methods remain ordinary owned
values. See the [implemented class contract](native-compiler.md#fixed-layout-classes).

This adopts SBCL's separation of layouts and indexed instance access from general
language-level slot lookup. In the local SBCL checkout,
`src/compiler/generic/vm-fndb.lisp` declares `%instance-ref` and `%instance-set`
as always translatable operations; `src/pcl/slots.lisp` and `slots-boot.lisp`
implement the language-level slot protocols and optimized access functions.
Mule does not copy CLOS's generic-function or class-redefinition semantics.

Mule restricts compatibility to keep instance layouts and dispatch stable.
Fixed-layout compiled classes reject undeclared fields and class
mutation; their first implementation excludes multiple inheritance, metaclasses,
arbitrary class-body execution, descriptors, and decorators. These are explicit
language restrictions, not optimizations justified by assuming ordinary Python
classes are immutable. Mutable module globals and object field values retain
their existing semantics. Additional guarded class inference and direct LLVM
slot access remain future optimizations; the current cache design alone does not
establish a timing result.

Container classes and their algorithms must be implemented in Mule Python and
compiled to native code. Rust supplies primitives for allocation, lifetime,
reference slot access, byte or machine-word storage, native numeric operations,
and host services. Python methods own container policy: logical length, growth,
index normalization, slicing, iteration, hashing protocol, collision handling,
resizing, and the public API. Class field storage is foundational object storage,
not an implementation of a Python list or dictionary. Existing Rust empty
container builtin helpers are bootstrap facilities and do not establish the
architecture for full containers.

Storage primitives and a compiled Python runtime-library bootstrap are still
required before those containers can be provided. Primitive bindings must remain
private and cannot be selected merely because user code calls a matching global
identifier. Ownership must distinguish an unused storage slot from a valid
`None` element, preserve aliases during moves or growth, and eventually support
collection of container cycles. A CPython-compatible extension interface will
call Mule's object and container implementation without linking CPython.

## LLVM replaces the machine backend

The adaptation boundary reaches into GTN/LTN and known-function backend hooks,
not just SBCL's final assembly stage.

| SBCL responsibility | Mule retains | LLVM handles |
| --- | --- | --- |
| GTN and entry analysis | Function interfaces, closure/cell layout, general/internal entry relationship. | Machine argument/result locations for the selected ABI. |
| LTN and IR2 conversion | Known-operation selection, guards, runtime fallbacks, checks, logical values. | Lowered SSA instructions, calls, and target legalization. |
| Representation selection | Python boxed/unboxed choices, materialization, overflow and protocol fallback. | Machine storage and instruction costs for the selected representations. |
| Control and stack analysis | Semantic CFG, cleanup, suspension, runtime/frame storage. | Machine block layout, stack layout, prologue/epilogue generation. |
| Lifetime and pack | Object ownership, roots, escaping state, observable frame values. | Register liveness, register allocation, spills. |
| VOP generation and assembler | Runtime-call signatures and module/artifact metadata. | Instruction selection, scheduling, assembly/object emission. |

LLVM documents its machine stages in [the code-generator design][llvm-codegen].
Mule need not reproduce SBCL's architecture VOP catalogs, storage classes,
register allocator, or assembler. The initial implementation lowers its
control-flow IR directly to LLVM; a separate implementation-level IR remains
an open choice for later representation selection and specialization.

Lowering must satisfy LLVM's SSA, control-flow, type, and target-layout rules.
Overflow flags, fast-math assumptions, and memory-effect attributes require
proof compatible with Python semantics. Runtime calls that execute callbacks
or modify exception state cannot be declared pure by default.
See [LLVM's language reference][llvm-ir].

## Output, loading, and observability

Replace the FASL protocol with native code plus runtime constants, module
initialization, compatibility metadata, and source information. Module code
executes at initialization in Python order; compilation is not module execution.
Imports and namespace mutation remain runtime operations. The initial compiler
produces AOT native executables. Module/library output, artifact caching, and any
JIT mode remain future decisions.

Carry AST provenance through IR1 and implementation lowering. Native debug
metadata and Python traceback/frame/tracing state are separate requirements.
Do not assume LLVM debug information creates Python frames or introspection;
LLVM expects frontend-provided [source debug metadata][llvm-debug].
The [C interface document](cpython-interface.md) records the corresponding
runtime compatibility surface.

[execution]: https://docs.python.org/3.14/reference/executionmodel.html
[compound]: https://docs.python.org/3.14/reference/compound_stmts.html
[expressions]: https://docs.python.org/3.14/reference/expressions.html
[simple]: https://docs.python.org/3.14/reference/simple_stmts.html
[annotations]: https://docs.python.org/3.14/library/annotationlib.html
[datamodel]: https://docs.python.org/3.14/reference/datamodel.html
[llvm-codegen]: https://llvm.org/docs/CodeGenerator.html
[llvm-ir]: https://llvm.org/docs/LangRef.html
[llvm-gc]: https://llvm.org/docs/GarbageCollection.html
[llvm-debug]: https://llvm.org/docs/SourceLevelDebugging.html
