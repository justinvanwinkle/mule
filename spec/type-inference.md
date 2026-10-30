# Type inference and flow propagation

Status: **implemented** for the value-type lattice (`ctype`, including `SetType` for `set` and `frozenset`), bounded text content domain (`text_domain`), checked machine-width contracts, declared scalar/container/class parameter and element contracts (Section 0A and Section 6M), intraprocedural forward constraint propagation, interprocedural local-call entry specialization (`propagate-to-args`), `InstanceType` method specialization and field-type inference (Section 0C), and cross-module return-type and constant summaries in the `builtins` interface and `ModuleCompiler`; **proposed** for wiring `TextContent` into `TextType` (Section 6D), `match`/`%isinstance` user-class branch narrowing (Section 6O), wheel-level `.so` interfaces, and demand-driven cross-package re-specialization.

See [SBCL compiler](sbcl-compiler.md), [Native implementation selection](native-lowering.md), [Optimization policies](optimization-policies.md), and [Remaining compiler and IR1 transform gaps](compiler-gaps.md).

---

## 1. Design charter

### 1.1 Single measure of value: optimization and code generation

Mule's type inference exists for **one purpose only**: to derive facts about runtime values and callable identities that enable **more specialized, more efficient machine code**. Every type representation, transfer function, and interprocedural propagation rule is evaluated exclusively by whether it unlocks (now or in a planned compiler stage):

- **Unboxed physical representations**: keeping integers in `i64` registers and floats in `double` registers across expressions, loops, and calls without heap allocation or tag manipulation (`mule/compiler/represent.py`).
- **Specialized instruction selection (VOPs)**: selecting direct 49-bit `fixnum` operations, direct `float64` arithmetic, or direct container storage reads/writes (`_storage_read`, `_text_length`, `byte_length`) instead of generic runtime dispatch (`mule/compiler/ltn.py`, `mule/compiler/vops.py`).
- **Direct and guarded calls**: replacing full Python call-protocol dispatch (`CallKind.FULL`) with guarded direct calls (`CallKind.GUARDED`) and routing calls directly to specialized entry contexts (`fixnum`, `observed`) that skip argument boxing and redundant checks (`mule/compiler/locall.py`, `mule/compiler/codegen.py`).
- **Compile-time IR1 transformations**: constant folding, dead-branch elimination, tuple element projection, `len()` strength reduction (`mule/compiler/seqtran.py`), and loop induction-variable optimization (`mule/compiler/loop.py`, `mule/compiler/ir1opt.py`).
- **Check elision**: proving that a value already satisfies a parameter, local, or return contract so `Cast` nodes emit zero runtime instructions (`mule/compiler/checkgen.py`).

A type distinction that does not pay rent in at least one current or planned optimization does not belong in the compiler's type lattice.

### 1.2 Non-goals: static linting and user-facing type policing

Mule's type system is an **internal compiler analysis**, not a user-facing static type checker (such as mypy, Pyright, or TypeScript):

- We do not design or evaluate type inference around documentation value, IDE diagnostics, or enforcing nominal type discipline on programmers.
- Valid dynamic Python programs where a variable holds different types across branches or reassignments must compile cleanly; when a precise type cannot be proven, the lattice widens to a union or `UNKNOWN` and code generation emits the general polymorphic path.
- Compile-time type errors are raised **only** when a program explicitly opts into a checked machine-width contract (`int32`, `uint64`, `float64`, etc. under a checking safety policy) or `optimize(target_class=...)` and the compiler proves a static contradiction.

### 1.3 Ergonomics for programmers and AI agents

- Unannotated Python code must be inferred as aggressively as the available program text permits from literals, operators, control-flow predicates, container construction, and call graphs.
- Standard Python type annotations (`x: int`, `s: str`, `items: list[int]`) and Mule numeric markers (`from mule_types import int64`) must remain idiomatic, low-friction Python. Using or omitting annotations must never make writing Python cumbersome for human programmers or AI coding agents.

---

## 2. Compilation world model: closed-world vs. open-world packages

Type inference must operate across two different visibility regimes:

1. **Closed-world knowledge**: The compiler sees all call sites of a function or all assignments to a binding (for example, non-escaping nested functions, private module globals, intra-module helpers, or a full-program build from source).
2. **Open-world / separate-compilation boundaries**: The compiler is building a library (such as a precompiled standard-library unit or a third-party Mule `.whl`) whose public functions and classes may be called by unknown external modules, or via dynamic `importlib.import_module()` at runtime.

An imported `typing.TYPE_CHECKING` binding is not an unconditional constant.
The import may not have executed yet, and the module attribute may have changed
before the import. A guarded import therefore remains a possible dependency
unless other analysis proves the guard unreachable.

### 2.1 Multi-entry contexts bridge open and closed worlds

Following SBCL's entry-point splitting in `src/compiler/locall.lisp`, Mule never sacrifices soundness at public boundaries to specialize internal calls, and never sacrifices internal specialization to preserve external generality. Every compiled function (`Lambda`) can publish multiple **entry contexts** (`EntryContext` in `mule/compiler/locall.py`):

- **`"general"` entry**: Sound for arbitrary external callers. Uncontracted parameters start at `UNKNOWN` (or `TupleType()` / `DICT_TYPE` for `*args` / `**kwargs`, or validated local hints under `safety < 3`). External callers, `getattr` dispatch, and unpredicted dynamic calls enter here.
- **`"fixnum"` entry**: Speculative specialized entry synthesized when a demand scan of the function body (`_ParameterUse`) proves that parameters are consumed by integer arithmetic, string methods (`TEXT_TYPE`), or list operations (`LIST_TYPE`).
- **`"observed"` entry**: Interprocedurally derived entry whose parameter domain is the join of the `dispatch_domain()` of all known call sites in the compilation unit.
- **Closed-lambda seeding**: If a nested function never escapes (`_collect_closed_lambdas`) and is only called via direct positional calls, its `"general"` entry parameters are seeded directly from the join of its actual call sites.

Observed calls with incompatible integer and float representations retain separate
entries through the existing bounded `observed.N` contexts, as calls with distinct
callable identities do. Integer intervals still join within the integer entry.
Within the observed-group limit, a float observation does not widen an integer
call's parameter and result facts to a numeric union. The general entry remains
valid for arbitrary values, including booleans, and every specialized entry
retains its dispatch guards. The existing limit on observed groups still bounds
code growth.

At each guarded call site, code generation compares the caller's derived argument `ValueType`s against the target's published entry contexts (`observed`, `fixnum`, `general`) and calls the most specific matching entry directly.

### Callable code identities and captured values

SBCL's `src/compiler/locall.lisp`, `substitute-let-funargs`, preserves functional
arguments by substituting their functional leaves and scheduling local-call
reanalysis. Mule retains the same knowledge in its value-type lattice because
Python function objects remain first-class values and can have distinct closure
environments despite sharing a compiled body. A callable type can identify its
compiled code without identifying the function object or its environment.
Entry dispatch must check that code identity before treating a callback target
as proven. Direct calls still obtain globals, defaults, and closure storage from
the actual callable supplied by the program. Predicted module bindings remain
untrusted until checked. Joins of different code identities widen to a general
function type. This representation preserves functional knowledge through the
existing parameter and return solver rather than replacing Python values with
compile-time function objects.

Captured parameter facts must include the defining function's incoming values
and every possible captured write. Facts from a guarded defining entry do not
become unconditional facts for all closure objects sharing that code. Unknown
external creation contexts therefore retain unknown captured values.

Captured parameter initialization participates in inference when every function
that captures the parameter is a nonescaping, direct-call-closed lambda. Shared
cells reachable through escaping functions remain unknown: Python exposes their
replacement and deletion through `cell_contents`. This applies to scalar and
container values, so nested callable facts cannot survive an unsupported cell
assumption. Inference for immutable retained function objects still preserves
their native entry identity. It does not equate distinct closure instances.

When closed capture facts narrow, local-call analysis invalidates affected return
summaries and their caller dependencies before propagating again. Definition-derived
return queries also discard cached bodies that used the previous capture assumptions.

The specialized callback path can therefore resolve calls and derive arithmetic
results while a general fallback remains less precise. For example, a native
`range` branch supplies integer elements even when a general iterator fallback
does not. Facts from the native branch do not establish an integer result for
the entire function. Likewise, returning a callable through a proven local
helper preserves its identity, but a mutable global helper binding does not
establish the returned code identity without an additional checked boundary.

### Final module function bindings

Mule gives `typing.final` on a module-level function a constant-binding contract.
The declaration follows an unshadowed import of `typing.final`, including an
import alias or a qualified `typing` import. A same-spelled user decorator is
not a declaration. This extends Python's typing-only meaning: after the function
definition initializes its module binding, reassignment and deletion are errors.
Methods, classes, and nested function definitions retain their ordinary runtime
decorator behavior.

The frontend preserves the declaration's `__final__ = True` marker at its place
in decorator order, while keeping the original function value visible to IR1.
Other decorators still execute and may return different objects. A known target
is recorded only when the resulting value actually identifies a compiled
function. Existing local-call analysis then propagates arguments and selects
specialized entries; it does not infer identity through an arbitrary wrapper.
Calls still retain guards needed for callable code and pre-initialization reads.

Other explicit stores and deletions of the declared module name are rejected
with source context. Runtime namespace writes and deletions also reject changes
to the initialized binding, including writes through module attributes and
`globals()`. Repeated execution of the defining statement cannot replace an
already initialized final binding. Bulk namespace operations retain their normal
incremental behavior: a failed change to a final binding leaves that binding
unchanged, but earlier changes to unrelated keys need not be rolled back.

This follows SBCL's `src/code/symbol.lisp`, `set-symbol-global-value` and
`makunbound`, which call `about-to-modify-symbol-value` before changing a symbol
value. Its `:constant` declaration rejects or signals a continuable error for
mutation. Mule makes that error noncontinuable, as required by its final-binding
contract, at every safety level. IR1 uses the existing local-call propagation
corresponding to `src/compiler/locall.lisp`, `propagate-to-args`.
Python's function object and its
module binding are separate, so Mule attaches this declaration to the module's
existing global cell rather than treating the function's `__final__` attribute
as evidence that every alias is constant. The low-level cell write-protection
primitive enforces the declared storage invariant without adding another
resolver, cache, or language-level evaluator.

### Checked inferred results within a compilation unit

Status: implemented for unambiguous same-unit named definitions and supported
runtime-checkable result types.

A named definition visible before the call in the same source unit can provide
an untrusted inferred result contract. Ambiguous definitions do not establish a
single inferred contract. Checked return declarations take precedence over
definition-derived value ranges. Argument constants are widened to their type
domains rather than fixing a contract to one observed argument value.
The call still uses the current global
binding. An IR1 `Cast` checks the actual returned object before its consumers
can use the contract's type facts. Compatible replacements execute normally.
An incompatible result raises `TypeError`; it does not discard the contract or
continue through an unchecked generic path.

This follows SBCL's `src/compiler/ir1opt.lisp`, `validate-call-type`, which applies
the defined type for `:defined-here` calls through `assert-call-type` with
`trusted` false. `src/compiler/ctype.lisp`, `assert-call-type`, inserts a result
assertion and reoptimizes its consumers. Mule uses its existing `Cast`,
propagation, and check-generation stages rather than freezing function bindings.
The initial scope excludes tail-position calls and result lvars with multiple
producers, matching the corresponding SBCL restrictions.

Result assertion insertion changes the IR1 def-use graph even when its block
identities stay unchanged. Subsequent definition-derived return queries rebuild
their cached graph when a block's node sequence changes and discard memoized
propagation from the stale snapshot. This preserves the current producer edges
that SBCL's `assert-call-type` and `reoptimize-lvar` operate on directly.
In particular, inserted checks on recursive calls participate in the next
return derivation instead of appearing as unavailable tuple elements. Leaf and
non-leaf tuple results therefore remain alternatives in the checked contract.

General method result checks use the receiver and callee facts of the general
body. Facts available only in specialized entries do not establish a general
result contract.

Only runtime-checkable facts become contracts. Immutable tuple element facts can
be checked recursively. Mutable collection contents must not become trusted from
a shallow object check; those facts are widened before the contract is formed.
The checked object remains owned through failure cleanup and transfers normally
after success. Inferred contracts are separate from ignored annotation hints.
Unpacked tuple-return calls retain their allocation-free convention. Proven
result types can eliminate redundant checks on a guarded direct arm; an unknown
replacement's original object is checked before unpack coercion. Tuple fields
delivered through the unpacked ABI are checked without constructing a tuple.

The existing `speed=0` and `debug=3` policies prevent establishing these inferred
contracts at the call site or for the predicted callee. `debug=3` is the existing
no-inlining policy and supplies the dynamic opt-out corresponding to SBCL's
`NOTINLINE` behavior. An established inferred contract remains checked even at
`safety=0`; incompatible replacement results must be errors. Explicit annotation
contracts retain their existing policy rules. This mandatory inferred check is
the user's requested boundary, not an assumption that rebinding cannot occur.

### Declared instance fields

Instance field types come from explicit class-body field annotations, not from
joining stores across methods. Following SBCL's `defstruct` slot descriptors
(`dsd-type` and `slot-access-transform` in `src/code/defstruct.lisp`), an
undeclared attribute has `UNKNOWN` (`*universal-type*`) so arbitrary mutations
(such as `None` defaults later replaced by integers, strings, or containers)
retain generic access without static store tracking. Declared class-body fields
are enforced at resolved store sites via policy-controlled `Cast`s (checked
above `safety == 0`, trusted at `safety == 0`) and trusted on read unless a
subclass narrows an inherited declared field's type (`not dsd-safe-p`), in
which case reads of the narrowed field emit a check (Section 3.6).

### 2.2 Hybrid package and wheel model (package `.so` + `interface.json` + `.py` source)

To support **instant runtime imports (including dynamic `importlib.import_module`)**, **fast separate compilation**, and **whole-program optimization**, compiled libraries and future Mule wheels use a three-part layout:

1. **One `.so` per package/wheel (compiled in parallel)**:
   - Each package compiles its modules in parallel within the build worker limit and links their per-module `.o` files into one shared library (`.so`). Each LLVM unit compiles whole, including the builtins unit.
   - At runtime, loading the package `.so` registers all of its compiled modules via `@mule_register_compiled_module`, so both static imports and dynamic `importlib.import_module(name)` calls resolve immediately without arbitrary library splitting or hundreds of individual `.so` files.

2. **Package type and symbol sidecar (`interface.json`, generalizing `BuiltinsInterface`)**:
   - An ELF `.so` or `.o` symbol table erases Python signatures, default parameter descriptors, class method tables, and inferred `ValueType` return types.
   - When a package is compiled, `ModuleCompiler` analyzes its modules in topological dependency order (grouping import cycles) and writes a package-level `interface.json` recording:
     - Every module's initialization symbol.
     - Every exported function's signature, symbol name, available specialized entry contexts (`general`, `fixnum`, `observed`), and inferred `ValueType` return type (`exported_types`, `storage_methods`, `builtin_return_types`).
   - **Separate compilation (default)**: When compiling downstream code that imports the package, Mule loads `interface.json` in milliseconds without re-parsing or re-analyzing the library's `.py` files. Callers immediately propagate the library's inferred return types and emit direct/guarded calls to its specialized entry symbols.

3. **Retained `.py` sources for whole-program and generic specialization**:
   - Because standard Python wheels also include `.py` source files, a build is not limited to the pre-baked entry contexts in the `.so`. When application call sites pass concrete argument types (such as a specific `TupleType` or homogeneous `ListType`) into an imported library function where specialization unlocks new IR1/IR2 transforms, the compiler can pull the function's `.py` source into the compilation unit's `locall` fixpoint and emit a caller-specialized clone.
   - (Note: shipping textual `.ll` in wheels is intentionally avoided—once IR1/IR2 has lowered an unspecialized function to boxed `MuleValue` calls and reference counting in `.ll`, LLVM LTO cannot recover unboxed representations or Python-level transforms, and `.ll` text is an order of magnitude larger than `.py` source.)

### 2.3 Caller-side monomorphization of specialized generics

A prebuilt `.so` cannot contain machine code specialized for type parameters that are only known when downstream caller code is compiled (for example, `class Box[T]`, `def bisect[T](a: list[T], x: T)`, or functions instantiated with a caller-defined `@optimize(target_class=True)` native struct).

To support specialized generics across `.so` boundaries, Mule follows the C++ template / Rust `.rlib`+`.so` split:

1. **What lives in the library's `.so`**:
   - Non-generic functions and classes.
   - The boxed `"general"` fallback implementations of generic functions and classes (used by dynamic or unspecialized callers).
   - The library module's global variable cells (`@mule.cell:<namespace>:<name>`), exported with default symbol visibility so code outside the `.so` binds to the same live module globals.
2. **Where specialized generics are emitted (the caller's binary)**:
   - When a caller compilation unit instantiates a generic class or calls a generic function from an imported package with concrete types (`int64`, `float64`, `list[int64]`, or a native struct layout), the compiler loads the generic definition from the package's `.py` source, specializes its IR1/IR2 under the concrete type arguments, and **emits the specialized native functions into the caller's compilation unit** with `linkonce_odr` / `weak_odr` linkage (so duplicate instantiations across multiple caller modules merge into a single definition at link time).
   - Any module globals or non-generic helpers referenced by the specialized generic body resolve as external symbol references against the library's `.so`.

---

## 3. Current type inference architecture

### 3.1 Algorithm overview: forward abstract interpretation with targeted demand scans

Mule's type derivation is **primarily a forward dataflow analysis** (abstract interpretation over the IR1 control-flow graph), paired with an **interprocedural optimistic fixpoint** over the call graph.

It is **not** a classical bidirectional type system (it does not alternate between top-down checking $\Gamma \vdash e \Leftarrow \tau$ and bottom-up synthesis $\Gamma \vdash e \Rightarrow \tau$ over AST expressions). Instead, information flows in two directions at specific phase boundaries:

- **Forward / bottom-up (primary)**: Constants, variable loads (`Ref`), assignments (`Set`), casts (`Cast`), and operations (`Combination`) compute derived `ValueType`s forward through basic blocks (`mule/compiler/constraint.py`).
- **Backward / demand-driven (targeted)**:
  1. **Use-site parameter classification (`_ParameterUse` in `mule/compiler/locall.py`)**: Walks parameter def-use chains forward to consumer operations to infer what demand the body places on unannotated parameters (`fixnum` arithmetic, `TEXT_TYPE` string methods, or `LIST_TYPE` list operations).
  2. **Branch condition origin narrowing (`_ConstrainComparison` in `mule/compiler/constraint.py`)**: Maps the `Lvar` of a branch condition back through `visitor.origins` to narrow the source `LambdaVar` along outgoing `If` edges.
  3. **Checked contract casts (`Cast` in `mule/compiler/node.py` and `mule/compiler/checkgen.py`)**: Intersects the forward-derived type of an expression with an asserted contract (`contract.narrow(actual)`) and elides the runtime check when `actual.fits(contract.fact)`.
  4. **IR2 representation selection (`mule/compiler/represent.py`)**: Combines producer `ValueType` proofs with consumer VOP operand storage classes to choose unboxed `int64` / `double` TNs vs. boxed `MuleValue` TNs.

### 3.2 The value-type lattice (`mule/compiler/ctype.py`)

Following SBCL's `CTYPE` hierarchy, all lattice operations (`join`, `fits`, arithmetic transfer functions, comparisons, container projections, and representation proofs) are implemented via **double dispatch** (`ValueType` methods and `ValueTypeVisitor`, with zero `isinstance` checks):

| Lattice element | Class / singleton | Meaning and optimization role |
| --- | --- | --- |
| $\bot$ | `BottomType` (`BOTTOM`) | No reachable values observed yet; identity element for `join` and initial state of optimistic return/closure summaries. |
| Unbound | `UnboundType` (`UNBOUND`) | Uninitialized local variable slot before any assignment. Any concrete `ValueType` also carries `may_be_unbound: bool` when joined with `UNBOUND` across branches. |
| $\top$ | `UnknownType` (`UNKNOWN`, `UNKNOWN_UNBOUND`) | Arbitrary Python object; requires boxed `MuleValue` representation and general runtime/protocol dispatch. |
| Booleans | `BooleanType` (`BOOLEAN_TRUE`, `BOOLEAN_FALSE`, `BOOLEAN`) | Singleton truth values enable compile-time dead-branch pruning in `_ConstrainEdge` and constant branch folding in `ir1opt`. |
| None | `NoneType` (`NONE_TYPE`) | Singleton `None`; participates in `is None` / `is not None` edge narrowing and immortal representation proofs (`proves_immortal`). |
| Integers | `IntegerType(minimum, maximum, may_be_unbound)` | Closed/open integer intervals. Tracks exact constants (`[k, k]`), 49-bit signed fixnums (`FIXNUM_MIN..FIXNUM_MAX`, `is_fixnum`), signed 64-bit native integers (`INT64_MIN..INT64_MAX`, `fits_int64`), and unsigned 64-bit integers (`0..UINT64_MAX`, `fits_uint64`). |
| Floats | `FloatType` (`FLOAT`) | IEEE-754 double-precision float (`fits_float`); enables unboxed `double` registers and raw float VOPs. Builtin `float` annotations (`PythonFloatContract`) also resolve to `FLOAT`. |
| Strings & bytes | `TextType(literal)` (`TEXT_TYPE`), `BytesType` (`BYTES_TYPE`) | Exact `str` (optionally with known constant literal) and exact `bytes`; enables direct `_text_length`, `_text_slice`, `byte_length`, and `TextStorage` / `BytesStorage` method resolution. |
| Tuples | `TupleType(arity, elements)` | Exact `tuple` with optional known length and per-element `ValueType` tuple; enables constant-bounds `len()` folding and per-index element type projection (`tuple_index_type`). |
| Lists | `ListType(element)` (`LIST_TYPE`, `EMPTY_LIST_TYPE`) | Exact `list`. A list is adjustable, so its element type and length describe its contents only at a reference to a variable with a single reference; every other reference has only `LIST_TYPE` (Section 3.6). Enables direct `_storage_length`, `_storage_read`, and iteration on proven lists. |
| Dicts | `DictType(key, value)` (`DICT_TYPE`, `EMPTY_DICT_TYPE`) | Exact `dict`. Key and value types follow the same single-reference rule as list elements (Section 3.6). Enables direct `dict` method resolution. |
| Sets | `SetType(element, frozen)` (`SET_TYPE`, `EMPTY_SET_TYPE`, `FROZENSET_TYPE`, `EMPTY_FROZENSET_TYPE`) | Exact `set` or `frozenset`. A mutable `set`'s element type follows the single-reference rule (Section 3.6); a `frozenset` keeps its element type. Enables direct `set` / `frozenset` method resolution and iteration. |
| Word vectors | `WordVectorType` (`WORD_VECTOR_TYPE`) | Private fixed-length vector of raw fixnum words, SBCL's `(simple-array fixnum (*))`. Every element is a fixnum because of the representation, and the length never changes (Section 3.6). |
| Iterators | `IteratorType(element)` (`ITERATOR_TYPE`) | Iterator object yielding `element` on each `%for_next` step; propagates loop-target variable types across `for` loops. |
| Callables | `FunctionType` (`FUNCTION_TYPE`) | Function object; paired in `Fact(type, target, proven)` with the predicted `Lambda` target and whether its identity is proven against rebinding. |
| Unions | `UnionType(members)` | Bounded disjunction of disjoint `ValueType` members (such as `T \| NoneType` or `IntegerType \| FloatType`). Supports `narrow_none` and distributes transfer operations across members before widening to `UNKNOWN`. |

Mutable container (`list`, `dict`, `set`) element summaries follow SBCL's
conservative rule: contents types apply only at a single-reference variable,
while declared element contracts come from annotations (Section 3.6).

`union_type` normalizes all alternative joins. It flattens nested unions, removes
bottom, absorbs unknown, and repeatedly applies the existing polymorphic pair
joins before retaining the remaining alternatives. As in SBCL's
`src/code/type.lisp`, `simplify-unions` and `type-union`, a new value type inherits
the general union fallback rather than requiring another table of type pairs.
`None` is an ordinary member; optional integers, optional values, and nullable
class objects use this same representation. Possible-unbound state remains
separate from `None`. Serialization uses a single union kind.

Transfers and narrowing operate memberwise. Proof queries require every member
to satisfy the property; possible-type queries require only one. The numeric
annotation summaries retain their existing domains, but a join of exact integer
and boolean facts preserves their alternatives rather than widening them into
an annotation summary. Existing same-family interval and container joins still
merge their facts. Loop widening reuses those domain-specific widening rules.
More than eight distinct union members widens to unknown, and the existing
container-depth limit remains three. These are analysis precision budgets, not
limits on supported Python values. Heterogeneous unions retain object storage
until narrowing proves a representation already supported by the backend.

### 3.3 Annotations and contracts (`mule/compiler/annotation_types.py`)

Mule distinguishes between two kinds of source annotations:

1. **Checked machine-width contracts** (`from mule_types import int8, int16, int32, int64, uint8, uint16, uint32, uint64, float64`):
   - Lower to explicit IR1 `Cast` nodes at parameter entry, annotated local stores, and `return` statements (`mule/compiler/ir1tran.py`, `mule/compiler/ir1tran_lambda.py`).
   - On `*args: T` and `**kwargs: T`, preserve the outer `TupleType()` / `DICT_TYPE` container fact while checking individual elements/values.
   - Statically contradictory values fail compilation (`AnnotationLedger.check`); unproven values emit borrowed runtime checks (`mule/compiler/checkgen.py`) unless `safety == 0`.
2. **Standard Python annotations** (`int`, `float`, `bool`, `None`, `str`, `bytes`, `list[T]`, `tuple[...]`, `dict[K, V]`, `T | None`, and user classes):
   - `mule/compiler/annotation_types.py` and `_DeclaredAnnotationReference` (`mule/compiler/ir1tran.py`) recognize scalar, `TupleContract`, `DictContract`, `ClassContract`, `UnionContract`, nested container subscript bindings, and `*args: T` / `**kwargs: T` annotations (see [compiler-gaps.md](compiler-gaps.md#0a-declared-parameter-contracts-and-nested-containerclass-annotations-ir1tranpy-annotation_typespy-locallpy-constraintpy-codegenpy-vopspy-implemented)).
   - Under `safety == 3` (`safety == 0` when trusted), `partial_params` establishes the entry-checked parameter `ValueType` in the `"general"` entry, while under `1 <= safety < 3` `locall.py` synthesizes a guarded entry (`.spec.0`) from `Contract.speculative_parameter_type()` alongside the hint-tolerant `"general"` entry and propagates container element contracts with runtime element guards (`_ContractAssertion`).
   - If any local store or interprocedural call site contradicts an unchecked Python annotation, `locall.py` records the variable in `annotations.ignored` and resets the parameter/local expectation to `UNKNOWN` so dynamic callers remain sound.

### 3.4 Intraprocedural constraint propagation (`mule/compiler/constraint.py`)

For a single `Lambda` and parameter tuple, `propagate()` runs a worklist over the precomputed `LambdaGraph` (`_Walk.run`):

1. **State representation**:
   - Each block entry holds a dictionary mapping every `LambdaVar`, captured closure variable, and referenced module global `(_GLOBAL, name)` to a `Fact(type, target, proven)`.
   - Unvisited blocks have no entry state; when multiple predecessor edges reach a block, their states are joined key-by-key via `join_fact`.
2. **Forward node evaluation (`_Propagation`)**:
   - `visit_Ref`: Reads the variable's current `Fact` from `self.state` (or `Constant` literal type via `constant_type`), records `self.origins[node.lvar] = leaf` for local variables, and sets `self.facts[node]`.
   - `visit_Set`: Updates `self.state[leaf]` with the RHS `Fact` (converting module-level `MAKE_FUNCTION` stores into proven `Fact(FUNCTION_TYPE, target=lambda_, proven=True)`).
   - `visit_Cast`: Validates the incoming `Fact` against `node.contract` and narrows the outgoing `Fact` via `node.contract.narrow(actual.type)`.
   - `visit_Combination`: Computes result types for:
     - Known arithmetic, bitwise, shift, unary, and comparison primitives (`_BINARY_OPERATORS`, `_UNARY_OPERATORS`, `_COMPARISON_OPERATORS`).
     - Container construction and mutation (`%build_list`, `%build_tuple`, `_build_dict`, `_storage_append`, `%getitem`, `%getslice`, `%setitem`, `%iterate`, `%for_next`, `%unpack`).
     - Known builtins (`len`, `ord`, `chr`, `bool`, `int`, `float`, `str`, `bytes`, `abs`, `min`, `max`, `sum`, `any`, `all`, `isinstance`, `issubclass`) and method calls (`%resolve_method`) against `storage_methods` (`TextStorage`, `BytesStorage`, `ListStorage`, `TupleStorage`, `dict`).
     - Direct/guarded local calls via the `resolve_call` callback supplied by `locall.analyze`.
3. **Side-effect invalidation**:
   - After any node where `_node_may_run_user_code` is true (an unknown call, or an operation on potentially non-primitive operands that could invoke user dunder methods), `invalidate_globals` clears proven module-global callable identities and resets indirect (closed-over mutable cell) variables to their interprocedural `optimistic_type`.
   - Container contents are not tracked through mutation. A reference to a mutable container through a variable with several references, or through a module global, carries only the container's conservative type (Section 3.6), so no later write can invalidate an element or length fact.
   - Method resolution marks its receiver as escaped when argument evaluation separates resolution and invocation across blocks. This discards stale size facts before block-local origins disappear.
   - Conversion publishes dynamic namespace mutation before unit preparation optimizes IR1. Such namespaces do not permit predictable builtin-call folding.
4. **Path-sensitive branch narrowing (`_ConstrainEdge` and `_ConstrainComparison`)**:
   - When a block ends with a two-way `If`, `constrain_edge(block, target)` inspects `node.test`:
     - If `test_type` is `BOOLEAN_FALSE` on the consequent edge or `BOOLEAN_TRUE` on the alternative edge, returns `None` (marking the control-flow edge dead).
     - Traces `node.test` through `_ConstrainComparison`:
       - Integer comparisons (`<, <=, >, >=, ==, !=`) against bounded integer expressions narrow the operand's `IntegerType` interval (`ValueType.narrow`).
       - `x is None` / `x is not None` narrows `x` to `NONE_TYPE` or strips `NoneType` (`narrow_none`).
       - `type(x) is T` / `type_of(x) is T`, `isinstance(x, T)` / `is_instance(x, T)`, and storage predicates (`_text_storage`, `_bytes_storage`, `_tuple_storage`) narrow `x` to the corresponding exact storage fact (`INTEGER_FACT`, `TEXT_FACT`, `BYTES_FACT`, `LIST_FACT`, etc.).
       - `%unary_not` flips the polarity `truth = not self.truth` and recurses.
     - Direct variable truthiness (`if x:`) applies `_NarrowTruthiness`, narrowing `BooleanType` to `BOOLEAN_TRUE`/`BOOLEAN_FALSE`, removing `0` from non-negative/non-positive integer intervals, and stripping `NoneType` on the truthy edge.
5. **Loop widening (`widen_fact`)**:
   - Back-edges `(block, target)` to loop headers are counted. After 2 visits, `widen_fact` widens growing integer lower/upper bounds to `None` (`±∞`, or `FIXNUM_MIN`/`FIXNUM_MAX` when the previous bound was a fixnum and the new bound remains within fixnum range), guaranteeing termination on loops with non-constant bounds.
6. **Exception handler edges**:
   - Inside a block covered by an exception handler (`handlers[block]`), `record_throw_point` joins the pre-operation state at every throwing node into `throw_state` and invalidates globals, then propagates `throw_state` along the exceptional edge to the handler entry block.

### 3.5 Interprocedural local-call analysis (`mule/compiler/locall.py`)

`locall.analyze` wraps `constraint.propagate` in a whole-unit fixpoint loop:

1. **Static discovery before the fixpoint**:
   - `_collect_closed_lambdas`: Finds nested functions whose function object never escapes and whose every reference is the callee of a direct positional call.
   - `collect_global_constants` and `_fold_protocol_constants`: Tracks module-level `GlobalVar.sets`, dominance/post-dominance (`_unconditional_toplevel_stores`), and `Final` annotations without identifier-spelling heuristics (Section 6A), seeding constant `derived_type`s and folding `Final` constants to `Constant` leaves under `safety < 3`.
   - `_GlobalFunctionStores` and `_ImportBindingStores`: Maps module-level function definitions and `from mod import fn` bindings so intra-module and cross-module calls can be predicted and typed.
   - `captured_vars`: Initializes `var.optimistic_type = BOTTOM` for closed-over variables assigned in the unit so their types grow optimistically from observed stores.
   - `_ParameterUse`: Seeds `"general"` and `"fixnum"` parameter tuples for every `Lambda`.
2. **Iterative propagation and context refinement**:
   - Propagates each `(lambda_, context)` body, memoizing `inputs[key] = (expected_parameters, consulted_calls, closure_types, consulted_targets)` so unchanged bodies are not re-walked.
   - Joins each body's `return_type` into `summaries[key]` (widening after 3 changes).
   - Updates `var.optimistic_type` for captured closure variables from the `"general"` bodies of all lambdas that store to `var`.
   - Updates `"general"` parameters for closed non-escaping lambdas from the joined argument types of their callers.
   - Collects `dispatch_domain()` argument tuples across all resolved intra-unit call sites to seed or widen `(target, "observed")` entry contexts. When an `"observed"` context narrows or is newly created, transitive callers in `inputs` have their provisional `BOTTOM` return summaries reset so cyclic call graphs converge cleanly.
3. **Publication and IR1 reoptimization**:
   - Once the fixpoint settles, `_publish_derived_types` writes the `"general"` node and lvar types onto IR1 (`node.derived_type`, `lvar.derived_type`).
   - Any known call (`CallKind.KNOWN`, `len()`, or predictable global known function) whose operand `Lvar` gained a narrower non-`UNKNOWN` type is marked `node.reoptimize = True`, triggering another pass of `ir1opt.ir1_optimize_until_done` (`mule/compiler/main.py`).
   - For `"fixnum"` and `"observed"` entry contexts whose narrower parameter types unlock additional IR1 transforms (`_would_reoptimize_for_context`), `specialized_ir1_context` clones the `Lambda`'s IR1 component during IR2 translation, runs `ir1opt` + `dfo` + `loop.analyze` under that entry's types, lowers the specialized IR2/LLVM body, and restores the canonical IR1 component.
   - `_SpecializesBody` retains `"fixnum"` / `"observed"` method entry contexts when `candidate` narrows a `GETATTR` or `SETATTR` receiver to `InstanceType` or narrows numeric/container operations, and drops `"observed"` when it adds no body, element-contract, or return-type specialization beyond `"fixnum"` (Section 0C in [compiler-gaps.md](compiler-gaps.md#0c-preserving-instancetypecls-method-specialization-instance-field-inference-and-method-call-return-contracts-locallpy-constraintpy-return_checkspy-codegenpy-implemented)).

#### Bounded text content domain

The bounded text content component is the first isolated increment toward
caller-site specialization. It does not replace or extend the active
`TextType` producers, propagation rules, or code generation in this increment.
Integration with those consumers is a separate change.

SBCL's `src/code/type.lisp` implements array/string shape subtyping in
`(array :simple-subtypep)`, unions in `(array :simple-union2)`, and
intersections in `array-intersection`. Dimensions are exact or wildcard
constraints, independent of array element representation. Mule follows that
separation between content constraints and physical representation.

The Python-specific extension represents a non-negative character-length
interval and guaranteed prefix and suffix, each limited to five Unicode
characters. Unlike SBCL's exact/wildcard array dimensions, intervals retain
useful information for strings of varying lengths. Guaranteed ends retain
partial content without retaining arbitrary-size literals. These constraints
do not establish interning, object identity, storage ownership, or subclass
behavior. No executable behavior or performance claim follows until later
increments integrate the component with compiler proofs and consumers.

The component provides union approximation, intersection, containment,
widening, concrete membership, and serialization. Union retains only common
guarantees. Intersection reports an empty domain when lengths or ends
conflict. Overlapping ends must agree at a permitted length. Canonicalization
reduces implied length and end constraints without enumerating strings.
Widening drops a decreasing lower bound or increasing upper bound and retains
only common ends, so ascending chains stabilize. Length bounds have no fixed
numeric limit. Operation cost depends on the five-character affix limit, not
the maximum represented string length.

The component is content metadata rather than a language-level `ValueType`.
Its intersection uses `None` for the empty content domain, not the Python
`None` value. Constructors reject inconsistent constraints and oversized ends.
Literal construction records exact character length and bounded ends only.
Serialization preserves these content facts and requires all fields.

#### Known analysis gaps

- **Inferred instance field types (partially unresolved):** Although
  `_update_class_field_types` in `locall.py` no longer infers instance field
  types from stores, `_InstanceFieldType` in `constraint_domain.py` and
  `_predicted_field_result` in `constraint.py` still infer mutable instance
  attribute types across methods and attach `element_contracts[node]` on
  `%getattr`. Only declared class-body field annotations (`ClassFacts.fields`)
  should supply slot types, following SBCL's `defstruct` `dsd-type` and
  `slot-access-transform` (Section 3.6).
- **Mutable-container flow facts and `escape.py` (active SBCL departure):**
  Despite Section 3.6's rule that element types come only from representation
  and declarations, `ListType(element)`, `DictType(key, value, proven)`, and
  `SetType(..., element, proven)` in `ctype.py` still carry flow-sensitive
  element/key/value types (`flow_getitem_type`, `flow_iterate_type`,
  `with_element`, `with_entry`) that `_propagate_append_argument`,
  `_propagate_extend_arguments`, and `_propagate_setitem` in `constraint.py`
  mutate in place, gated by the 1,565-line `escape.py` interprocedural solver.
- **Unsound `%tuple_from_list` arity-1 derivation (`constraint.py`):**
  `_propagate_tuple_from_list` returns `_tuple_fact(1, (elem_type,))`
  (`TupleType(arity=1, elements=(elem_type,))`) whenever a local list has a
  known `elem_type` and more than one `Set`, falsely proving exact length `1`
  for any tuple built from a non-empty list of unknown length.
- **Unsound single-`Ref` `LambdaVar` container flow facts
  (`constraint_propagate.py`):** `_Propagation.visit_LambdaVar` calls
  `fact.conservative()` only when `len(leaf.refs) > 1`. A variable with a
  single `Ref` (including inside a loop or after an aliasing call that mutates
  the container) retains stale mutable-container element facts.
- **Non-SBCL relational difference-bound matrix, midpoint solver, side tables,
  and component cloning:** `State.relations` in `constraint_propagate.py`
  maintains an all-pairs difference-bound matrix `u - v <= off` with synthetic
  keys `(_SEQ_LEN, var)` and `(_RANGE_STOP, range_state)`, paired with
  `_SequenceProducerLength` and `_LinearUpdate` (including binary-search
  midpoint extraction `(u + v) >> 1` and `(u + v) // 2`). `check_in_bounds_access`
  mutates `relations[idx_var, seq_key]` even when returning `False`. `Body` in
  `constraint_domain.py` stores out-of-band side tables (`lvar_contracts`,
  `var_contracts`, `iter_contracts`, `element_contracts`, `in_bounds_nodes`,
  `tuple_list_origins`) instead of IR1 `Cast` nodes, and
  `_derive_user_lambda_call` in `constraint_propagate.py` clones whole IR1
  components via `clone_lambda_component` in `ir1util.py`.
- **`TYPE_CHECKING` import folding (resolved):** `fold_type_checking_constants`
  has been removed from `main.py` and `locall.py` (`eebe93d`), so imported
  `typing.TYPE_CHECKING` bindings follow ordinary dominance, execution-order,
  and rebinding analysis rather than unconditional syntactic replacement.
- **Context derivation depth:** `locall.py` limits active context-dependent
  derivations to six, including acyclic chains. At the limit it falls back to a
  general return summary or abandons branch narrowing, which can discard useful
  specialization facts. A workload impact has not been measured. Preserve
  termination for recursive analysis while investigating how to retain facts in
  deeper acyclic chains. SBCL's local-call analysis and subsequent LET conversion
  are described in `src/compiler/locall.lisp`.
- **Standard-library summary cache investigation:** `stdlib_type_summary` and
  `stdlib_target_summary` in `sequence_protocol.py` use process-wide cached
  summaries without a source-version key at the lookup. `prepare` can refresh
  these caches. Determine whether a long-lived compiler process can reuse stale
  facts after standard-library sources change. Stale generated code has not been
  reproduced, and the relevant refresh paths have not been fully established.

### 3.6 Container contents, specialized storage, and declared fields

Element types come from a container's representation and from declarations.
They never come from inference about the values written into a mutable
container.

#### Conservative types for mutable containers

SBCL's `lvar-conservative-type`, `node-conservative-type`, and
`conservative-type` in `src/compiler/ir1opt.lisp` use a type that describes an
object's contents only at a reference to a variable with a single reference.
There, no operation through another reference can have changed the object.
Every other reference gets the conservative type. A cons becomes `cons`. An
adjustable array keeps its element type, which is its representation, but
loses its dimensions. A simple array is unchanged. `propagate-to-refs` does not
narrow such variables from their uses.

Mule follows the same rule. `ValueType.conservative_type()` and
`Fact.conservative()` in `ctype.py` compute the conservative type, and
`_Propagation.visit_LambdaVar` and `visit_GlobalVar` in `constraint.py` apply
it:

- A `list`, `dict`, or `set` reached through a variable with more than one
  reference, or through a module global, has only its kind: `LIST_TYPE`,
  `DICT_TYPE`, or `SET_TYPE`. Python's general containers have no specialized
  element representation, so their element types are unknown at such
  references.
- Tuples, frozensets, iterators, and unions keep their own shape and apply the
  rule to their members.
- Vector-length constraints, which remove bound checks, apply only to simple
  sequences, whose length is fixed when they are created: text, bytes, tuples,
  and word vectors. This follows `%check-bound`, `vector-constraint-length`,
  and `vector-length-var-p` in `src/compiler/equality-constraints.lisp`. List
  and bytearray indexing keeps its bound check.
- Declared types are not contents facts. A declaration is the programmer's
  assertion, and the conservative rule never widens it (Section 3.3 and
  declared instance fields below). Parameter and local container contracts live
  in `walk.local_contracts` and `walk.element_contracts`, separate from the
  container's `ValueType`, and propagate element contracts across reads and
  iteration.

#### Specialized storage: word vectors

An element type that matters for performance comes from a representation that
holds only that type, as with SBCL's specialized arrays. SBCL's own hash table
declares its index, next, and hash vectors as
`(simple-array hash-table-index (*))` (`src/code/hash-table.lisp:106-119`).

`WordVectorType` is Mule's fixed-length vector of raw fixnum words:

- Compiled code owns the layout. `mule.word_vector.new` in
  `mule/compiler/wordops.py` allocates the words with `malloc` and initializes
  the object header, tag 28, data pointer, and length. The runtime knows the
  object only as `Data::RawWords` in `mule/runtime/object.rs`: a block of words
  with no references, which `destroy` frees.
- `_word_vector_new(length, initial)` creates a vector. The generic
  `_storage_read`, `_storage_write`, and `_storage_length` primitives select
  word-vector templates when the receiver is a proven word vector, as SBCL
  selects `data-vector-ref` by the array's element type. Each operation is one
  LLVM procedure, and LLVM decides inlining.
- A generic storage access whose receiver is not proven dispatches on the
  receiver's tag, as SBCL's `hairy-data-vector-ref` dispatches on the array's
  widetag (`fast_storage_length.ll`, `fast_storage_read.ll`,
  `fast_storage_read_borrow.ll`, `fast_storage_write.ll`, and
  `mule/runtime/sequences.rs`).
- The templates check the receiver's tag and the fixnum tag of any operand not
  proven to be a fixnum. Bounds remain the responsibility of the compiled
  Python that indexes the vector.
- `_Table._indices` and `_Table._hashes` in `stdlib/_dict.py` are word vectors.
  `_hashes` holds two thirds of the index size, the limit at which inserts
  resize, and each new entry's hash is written at the entry's position.

#### Declared instance fields

Class-body field annotations (`ClassInfo.field_annotations` in `ir1tran.py`,
`ClassFacts.declared_fields` in `locall.py`) follow SBCL's typed `defstruct`
slots, `slot-access-transform` in `src/code/defstruct.lisp`:

- Every read of a safe declared field has the declared type and is never
  checked, at any safety (`the*` with `:derive-type-only`). The number of
  references to the object, and what other code could reach the field, do not
  matter.
- A store the compiler resolves to a declared field asserts the declared type
  of the stored value (`DeclaredFieldContract` in `return_checks.py`). It is a
  policy-controlled `Cast`: checked when safety is above 0, and trusted at
  safety 0 (`type-check` in `src/compiler/policies.lisp`). The class's own
  methods are not treated differently from other code.
- A store the compiler cannot resolve, such as dynamic `setattr`, corresponds
  to SBCL's raw `%instance-set`. It is not checked, and the field keeps its
  declared type. Code that writes a field this way is responsible for keeping
  the type correct. Accessing a private member from outside its class is at
  the accessor's own risk.
- SBCL checks reads only of an unsafe slot (`dsd-safe-p`), for example one
  whose type a subclass narrows (`ClassFacts.unsafe_declared_fields`). A
  subclass that redeclares an inherited field with a narrower type emits a
  read-site type check when reading that field on the subclass.
- A declared element type on a general container field, such as
  `list[int]`, is kept at every read of the field (`_InstanceDeclaredField` in
  `constraint.py`). Element reads through it have the declared element type,
  and element writes the compiler attributes to the field (`append`, `insert`,
  `add`, `%setitem`, `_storage_append`, `_storage_write`, and container
  displays assigned to the field) are checked under the same policy as field
  stores.
