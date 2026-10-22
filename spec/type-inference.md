# Type inference and flow propagation

Status: **implemented** for the value-type lattice (`ctype`), checked machine-width contracts, basic scalar/single-level-list local annotation hints, intraprocedural forward constraint propagation, interprocedural local-call entry specialization, and cross-module return-type summaries in the `builtins` interface and `ModuleCompiler`; **proposed** for nested container/tuple/dict/class parameter contracts, entry-checked/guarded parameter specialization on uncalled public/callback functions, `InstanceType` method variant retention on attribute access, wheel-level `.so` interfaces, and demand-driven cross-package re-specialization.

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
- Standard Python type annotations (`x: int`, `s: str`, `items: list[int]`) and Mule numeric markers (`from mule import int64`) must remain idiomatic, low-friction Python. Using or omitting annotations must never make writing Python cumbersome for human programmers or AI coding agents.

---

## 2. Compilation world model: closed-world vs. open-world packages

Type inference must operate across two different visibility regimes:

1. **Closed-world knowledge**: The compiler sees all call sites of a function or all assignments to a binding (for example, non-escaping nested functions, private module globals, intra-module helpers, or a full-program build from source).
2. **Open-world / separate-compilation boundaries**: The compiler is building a library (such as a precompiled standard-library unit or a third-party Mule `.whl`) whose public functions and classes may be called by unknown external modules, or via dynamic `importlib.import_module()` at runtime.

### 2.1 Multi-entry contexts bridge open and closed worlds

Following SBCL's entry-point splitting in `src/compiler/locall.lisp`, Mule never sacrifices soundness at public boundaries to specialize internal calls, and never sacrifices internal specialization to preserve external generality. Every compiled function (`Lambda`) can publish multiple **entry contexts** (`EntryContext` in `mule/compiler/locall.py`):

- **`"general"` entry**: Sound for arbitrary external callers. Uncontracted parameters start at `UNKNOWN` (or `TupleType()` / `DICT_TYPE` for `*args` / `**kwargs`, or validated local hints under `safety < 3`). External callers, `getattr` dispatch, and unpredicted dynamic calls enter here.
- **`"fixnum"` entry**: Speculative specialized entry synthesized when a demand scan of the function body (`_ParameterUse`) proves that parameters are consumed by integer arithmetic, string methods (`TEXT_TYPE`), or list operations (`LIST_TYPE`).
- **`"observed"` entry**: Interprocedurally derived entry whose parameter domain is the join of the `dispatch_domain()` of all known call sites in the compilation unit.
- **Closed-lambda seeding**: If a nested function never escapes (`_collect_closed_lambdas`) and is only called via direct positional calls, its `"general"` entry parameters are seeded directly from the join of its actual call sites.

At each guarded call site, code generation compares the caller's derived argument `ValueType`s against the target's published entry contexts (`observed`, `fixnum`, `general`) and calls the most specific matching entry directly.

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
| Floats | `FloatType` (`FLOAT`) | IEEE-754 double-precision float (`fits_float`); enables unboxed `double` registers and raw float VOPs. The annotation summary `REAL` admits integers, booleans, and floats without changing their objects. |
| Strings & bytes | `TextType(literal)` (`TEXT_TYPE`), `BytesType` (`BYTES_TYPE`) | Exact `str` (optionally with known constant literal) and exact `bytes`; enables direct `_text_length`, `_text_slice`, `byte_length`, and `TextStorage` / `BytesStorage` method resolution. |
| Tuples | `TupleType(arity, elements)` | Exact `tuple` with optional known length and per-element `ValueType` tuple; enables constant-bounds `len()` folding and per-index element type projection (`tuple_index_type`). |
| Lists | `ListType(element)` (`LIST_TYPE`, `EMPTY_LIST_TYPE`) | Exact `list` with homogeneous element-type summary (`element` starts at `BOTTOM` for `[]` and joins on `append`/`insert`/`extend`/`__setitem__`); enables direct `_storage_length`, `_storage_read`, and typed iteration. |
| Dicts | `DictType(key, value)` (`DICT_TYPE`, `EMPTY_DICT_TYPE`) | Exact `dict` with joined key and value `ValueType` summaries; enables direct `dict` method resolution and key/value type propagation on subscript and iteration. |
| Iterators | `IteratorType(element)` (`ITERATOR_TYPE`) | Iterator object yielding `element` on each `%for_next` step; propagates loop-target variable types across `for` loops. |
| Callables | `FunctionType` (`FUNCTION_TYPE`) | Function object; paired in `Fact(type, target, proven)` with the predicted `Lambda` target and whether its identity is proven against rebinding. |
| Unions | `UnionType(members)` | Bounded disjunction of disjoint `ValueType` members (such as `T \| NoneType` or `IntegerType \| FloatType`). Supports `narrow_none` and distributes transfer operations across members before widening to `UNKNOWN`. |

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

1. **Checked machine-width contracts** (`from mule import int8, int16, int32, int64, uint8, uint16, uint32, uint64, float64`):
   - Lower to explicit IR1 `Cast` nodes at parameter entry, annotated local stores, and `return` statements (`mule/compiler/ir1tran.py`, `mule/compiler/ir1tran_lambda.py`).
   - On `*args: T` and `**kwargs: T`, preserve the outer `TupleType()` / `DICT_TYPE` container fact while checking individual elements/values.
   - Statically contradictory values fail compilation (`AnnotationLedger.check`); unproven values emit borrowed runtime checks (`mule/compiler/checkgen.py`) unless `safety == 0`.
2. **Standard Python annotations** (`int`, `float`, `str`, `bytes`, `list[T]`; **proposed**: `bool`, `dict[K, V]`, `tuple[...]`, nested container contracts, `T | None`, and in-module user classes):
   - Currently, `_DECLARATION_BINDINGS` (`mule/compiler/ir1tran.py`) recognizes only `int`, `float`, `str`, `bytes`, and `list` (`ListContract`), where `ListContract`'s element must itself be one of those scalar contracts. Annotations using `bool`, `tuple[...]`, `dict[K, V]`, nested containers (such as `list[tuple[str, int]]` or `tuple[list[list[int]], list[list[int]]]`), unions (`T | None`), or user-defined classes (`_Node`, `Expr`, `Account`) currently resolve to `_OPAQUE_BINDING` (`None`) and are ignored.
   - Recognized contracts are recorded on `Lambda.local_annotations`. However, `Contract.general_parameter_type()` (`mule/compiler/annotation_types.py`) currently returns `UNKNOWN` for all contracts except `ListContract` with a non-`None` element. Scalar parameter annotations (`x: int`, `s: str`) only seed a `"fixnum"` variant when `_ParameterUse.analyze` (`mule/compiler/locall.py`) directly observes primitive numeric, text, or list operations on the parameter, and do not seed `"general"`.
   - Consequently, functions passed as callbacks without direct intra-module call sites (such as benchmark `run(data: ...)` functions) or methods whose annotated parameters are passed to other methods (`self._map.get(key)`) currently receive `UNKNOWN` parameter types unless extended as proposed in [compiler-gaps.md](compiler-gaps.md#0-high-impact-architectural-parity-gaps-from-the-14-workload-suite-audit-proposed).
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
   - `collect_global_constants`: Finds module-level private constants (`_NAME = <literal>`) that are assigned once at toplevel and never rebound, seeding their `Ref.derived_type` under `safety < 3`.
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
   - **Current limitation in `_SpecializesBody`**: `locall.analyze` retains a `"fixnum"` entry context (including `(InstanceType(cls), ...)` on class methods) only when `_SpecializesBody` observes that a node narrows to `INTEGER`, `BOOLEAN`, or `FLOAT`, or specializes a `list`/`str`/`dict` container operation. Methods whose specialization consists of `InstanceType` attribute reads/writes (`GETATTR` / `SETATTR`) or `PYTHON_INTEGER` results currently drop their `"fixnum"` variant (`variants=()`). Extending `_SpecializesBody` to preserve `InstanceType` slot specialization is tracked in [compiler-gaps.md](compiler-gaps.md#0-high-impact-architectural-parity-gaps-from-the-14-workload-suite-audit-proposed).
