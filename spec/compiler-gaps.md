# Remaining SBCL compiler and IR1 transform gaps

Status: **implemented** for declared parameter contracts and sound container
element propagation (Section 0A), general-context flow-type feedback into IR1
transforms, unified constant-lvar/dead-operand helpers, and monotonic
`derive_node_type` (Section 1A), entry-context IR1 specialization / closed-call
parameter promotion (Section 1B), identity-aware float storage and coercions,
numeric internal calling conventions, and LLVM-visible private leaf boxes
(parts of Section 5); **proposed** for Sections 0B–0D, Sections 1C–4,
and the rest of Section 5.

This document records the remaining architectural and transform gaps between
SBCL's compiler (`~/checkout/sbcl/src/compiler/`) and Mule (`mule/compiler/`),
together with the SBCL source references, Python semantic adaptations, and
staged implementation plan for each area.

## Builtin-type conformance coverage (known gap)

Status: incomplete coverage; the systematic coverage work below is not implemented.

The conformance suite contains useful builtin, container, and protocol programs,
but it does not establish complete conformance for builtin types. Coverage needs
an explicit inventory of builtin types and their exposed methods and protocols,
not only programs that happen to exercise common operations. For example,
`conformance/containers/list_methods.py` exercises ordinary list operations, while
`conformance/builtins/type_and_len.py` exercises custom-object length protocols.
Neither establishes the complete `list` descriptor and subclass contract.

The runtime failures found in `perf/suite/dataclass_ast.py` exposed this gap:

- Mule's compiled list implementation lacked `list.__len__`, although ordinary
  builtin `len(list_value)` worked. Sequence-pattern lowering called the missing
  method on its materialized list and raised `AttributeError`.
- Investigation also found that builtin `len()` bypasses an overriding
  `__len__` on a list subclass through the Rust `mule_fast_len` storage path.
  This is a separate unresolved defect. Adding the missing base descriptor does
  not fix it. The explicit base call `list.__len__(instance)` must return the
  storage length, while `len(instance)` must honor the subclass override.

Required coverage work:

1. Inventory builtin types and exposed operations against the Python 3.14 oracle.
   Record which contracts have executable conformance cases and which remain gaps.
2. Test syntax and builtin dispatch separately from bound method calls and explicit
   base-descriptor calls. Include builtin subclasses, inherited methods, overrides,
   invalid receivers, argument errors, empty values, and relevant mutation effects.
3. Compare observable results, exception classes, and evaluation effects with
   CPython. Do not treat the presence of a method or successful compilation as
   evidence that its semantics conform.
4. Keep cases focused so an unsupported feature cannot hide unrelated builtin
   coverage. Record remaining failures through the existing conformance-gap
   mechanism. Focused compiler regressions supplement this matrix; they do not
   establish that the rest of a builtin type conforms.

This is a verification gap as well as an implementation gap. Passing the existing
tests or a benchmark does not establish complete builtin-type compatibility.

## 0. High-impact architectural parity gaps from the 14-workload suite audit (proposed)

An audit of the compiled IR1, `locall` summaries, and `stdlib/_dict.py` across
the 14 benchmark workloads ([performance.md](performance.md#suite-wide-benchmark-audit-and-root-cause-diagnosis-2026-10-22))
showed that Mule's largest slowdowns relative to CPython and SBCL (`Mule / CPython`
up to `2.97` and `Mule / SBCL` up to `7.95`) come from four fundamental
departures from SBCL's compiler and runtime architecture:

### 0A. Declared parameter contracts and nested container/class annotations (`ir1tran.py`, `annotation_types.py`, `locall.py`, `constraint.py`, `codegen.py`, `vops.py`) (implemented)

- **SBCL reference**: `src/compiler/ir1tran-lambda.lisp` (`ir1-convert-lambda`,
  `ir1-convert-aux-bindings`) and `src/compiler/checkgen.lisp`
  (`generate-type-checks`), where `(declare (type ...))` on function parameters
  emits an entry type check (under `safety >= 1`) and asserts the declared
  `CTYPE` across the entire function body, regardless of whether callers are
  known at compile time.
- **Implemented design and soundness contract**:
  1. `BooleanContract`, `NoneContract`, `TupleContract`, `DictContract`,
     `ClassContract`, `UnionContract`, recursive nested container subscript
     bindings, and `*args: T` / `**kwargs: T` contracts are recognized by
     `mule/compiler/annotation_types.py` and `_DeclaredAnnotationReference` in
     `mule/compiler/ir1tran.py`.
  2. Under `safety == 3` (`safety == 0` when trusted), `partial_params`
     establishes the entry-checked parameter `ValueType` in the general entry,
     while under `1 <= safety < 3` `locall.py` synthesizes a guarded entry
     (`.spec.0`) from `Contract.speculative_parameter_type()` alongside the
     hint-tolerant general entry.
  3. Because Python containers are mutable and may be aliased, a shallow entry
     check on `list[T]` or `dict[K, V]` establishes only the outer container
     shape (`LIST_TYPE`, `DICT_TYPE`, `TupleType(arity)`). `constraint.py`
     propagates container element contracts across indexing, slicing, iteration,
     sequence unpacking, `dict.get`/`pop`/`setdefault`, and `list`/`sorted`,
     emitting per-element runtime guards via `_ContractAssertion` (`codegen.py`,
     `vops.py`) whenever the element type is not already statically proven.

### 0B. Typed `dict` / `set` storage layout and fixnum hash invariants (`stdlib/_dict.py`, `locall.py`, `vops.py`) (proposed)

- **SBCL reference**: `src/code/target-hash-table.lisp` (`define-ht-getter`,
  `define-gethash/flat`, `puthash`) and `src/code/target-sxhash.lisp` (`sxhash`),
  where hash-table slots (`index-vector`, `hash-vector`, `table`, `next-vector`)
  are typed `defstruct` slots accessed at fixed offsets and hashes are unboxed
  fixnums.
- **Current Mule defect**:
  - In `stdlib/_dict.py`, `_table_lookup` and `_table_insert` are standalone
    functions taking untyped `(self, key)` shared by `dict`, `set`, and
    `frozenset`. They compile with `self: UnknownType` and `variants=()`.
  - Every lookup or insertion executes 5+ dynamic attribute lookups
    (`mule_attribute_lookup_once`) for `self._indices`, `self._hashes`,
    `self._keys`, `self._values`, and `self._used`.
  - Because `self._indices` and `self._hashes` have `UnknownType`,
    `_storage_read(indices, slot)` returns `UnknownType`. Consequently,
    `_table_lookup` has inferred return type `UnknownType` (forcing `entry >= 0`
    in `dict_get` and `dict_getitem` to call generic `mule_compare_greater_equal`),
    and `_storage_read(hashes, entry) == code` calls generic `mule_compare_equal`.
  - `hash(key)` returns a 64-bit signed integer (`[-2^63, 2^63 - 1]`) that
    exceeds Mule's 63-bit fixnum range (`[-2^62, 2^62 - 1]`), forcing heap/GMP
    integer allocation and comparison when bit 62 is set.
- **Required implementation**:
  1. Access dictionary/set table storage (`_indices`, `_hashes`, `_keys`,
     `_values`, `_used`) through fixed native slot offsets or dedicated
     storage primitives so `_table_lookup`, `_table_insert`, `_table_get_value`,
     and `_table_resize` never perform dynamic attribute lookup.
  2. Prove `IntegerType` (fixnum) for index-table and hash-table reads so
     `_table_lookup` returns `IntegerType(-1, FIXNUM_MAX)`, `entry >= 0` and
     `entry is _EMPTY` compile to unboxed integer comparisons, and probe-loop
     hash comparisons execute in registers without boxed calls.

### 0C. Preserving `InstanceType(cls)` method specialization in `locall._SpecializesBody` (`mule/compiler/locall.py`) (proposed)

- **SBCL reference**: `src/code/defstruct.lisp` (`slot-access-transform`) and
  `src/compiler/locall.lisp`, where structure/instance accessors always
  specialize on the receiver's class layout and propagate slot types.
- **Current Mule defect**:
  - `_SpecializesBody` (`mule/compiler/locall.py`) discards a method's `"fixnum"`
    entry context `(InstanceType(cls), ...)` unless narrowing `self` changes a
    node's derived type to `INTEGER`, `BOOLEAN`, or `FLOAT`, or specializes a
    `list`/`str`/`dict` container op.
  - Methods that read or write instance attributes (`GETATTR` / `SETATTR`, such
    as `self.value`, `self.left`, `self.right`, `self._head`, `node.prev`) or
    return `PYTHON_INTEGER` fail `_SpecializesBody` and lose their
    `InstanceType(cls)` variant (`variants=()`), falling back to generic
    attribute lookup on `self`.
- **Required implementation**:
  - Teach `_SpecializesBody` in `mule/compiler/locall.py` to recognize when
    `candidate` narrows a `GETATTR` or `SETATTR` receiver to `InstanceType`
    (enabling direct slot access or narrowing the loaded field's `ValueType`) or
    narrows a node from `UNKNOWN` to `PYTHON_INTEGER` / `InstanceType` /
    container types.

### 0D. Borrowed / unboxed container element reads and loop reference-count elimination (`represent.py`, `vops.py`) (proposed)

- **SBCL reference**: `src/compiler/array-tran.lisp` (`data-vector-ref`) and
  `src/compiler/represent.lisp`, where vector reads load unboxed or register
  values without reference-count traffic.
- **Current Mule defect**:
  - Even when `list` or `dict` storage is proven, `_storage_read` in an inner
    loop retains (`mule_retain`) and releases (`mule_release`) every loaded
    element and boxes scalars in container buffers.
- **Required implementation**:
  - Elide `retain`/`release` on non-escaping borrowed reads from proven
    containers across non-mutating, non-calling basic-block regions, and
    unbox scalar loads when feeding fixnum/float consumers.

---

## 1. IR1 transform and source-to-source specialization gaps

### 1A. Flow-derived type feedback into IR1 transforms (implemented)

- **SBCL reference**: `ir1-phases` (`src/compiler/main.lisp`),
  `propagate-to-args` / `constraint-propagate` (`src/compiler/constraint.lisp`),
  and `ir1-optimize` (`src/compiler/ir1opt.lisp`), where flow-narrowed `LVAR`
  types trigger `deftransform` rules and `derive-type` recomputation in a
  fixpoint loop.
- **Mule implementation**:
  - `locall._publish_derived_types` (`mule/compiler/locall.py`) publishes
    `general`-context flow-derived `ValueType`s onto every `ValuedNode` and
    `Lvar` in the component and marks narrowed known calls, `If` nodes whose
    test gained a known truth value or constant literal, and predictable
    builtin global calls (`len`, `str`, and any future `KNOWN` entry) for
    reoptimization via `_ReoptimizeNarrowedKnownCalls`.
  - `_DeriveNodeType` and `_PrepareNodeOptimize` (`mule/compiler/ir1opt.py`)
    preserve flow-derived types on `Ref(LambdaVar)` and non-`KNOWN`
    `Combination`s across IR1 reoptimization rounds, and intersect/narrow with
    existing `node.derived_type` (`derive-node-type` monotonicity) when local
    syntactic derivation is coarser than flow propagation.
  - `ir1opt.constant_lvar_value`, `ir1opt.bound_constant_literal`,
    `ir1opt.is_pure_bound_ref`, and backward `_flush_dead_operands`
    (`flush-dead-code`) unify constant extraction and dead-operand flushing
    across `ir1opt.py`, `srctran.py`, `modarith.py`, and `seqtran.py`.
  - `srctran.py` and `modarith.py` consult flow-proven singleton types on pure
    bound local references (`is_pure_bound_ref`), enabling identity
    reductions, absorbing-zero folds, power-of-two strength reductions,
    redundant bitmask elimination, and commutative/associative constant folding
    on local variables proven constant by flow analysis, while unlinking dead
    `Ref(LambdaVar)` nodes.

### 1B. Entry-context IR1 specialization and closed-call parameter promotion (implemented)

- **SBCL reference**: `locall-analyze-component`, `propagate-to-args`,
  `convert-more-call` (`src/compiler/locall.lisp`, `src/compiler/ir1opt.lisp`).
- **Mule implementation**:
  1. **Closed-call promotion to `general`**: `_collect_closed_lambdas` and the
     fixpoint solver in `locall.analyze` (`mule/compiler/locall.py`) identify
     non-escaping nested `Lambda`s whose function object is bound once to a
     local variable and referenced only as the callee of direct positional
     calls (`CallSite.proven` in `mule/compiler/constraint.py`), including
     across recursive calls and local calls that cannot mutate uncaptured outer
     locals. Their `general` parameter types are seeded and widened from the
     join of their callers' `general`-context argument types, allowing IR1
     `@deftransform`s to fire directly on their canonical IR1 graph.
  2. **Per-`EntryContext` IR1 cloning and reoptimization**: `clone_lambda_component`
     (`mule/compiler/ir1util.py`) and `specialized_ir1_context`
     (`mule/compiler/locall.py`, wired into `compile_component` and `emit_body`
     in `mule/compiler/codegen.py`) clone a `Lambda`'s IR1 `Component` whenever
     a specialized `EntryContext` (`fixnum` or `observed`) narrows arguments to
     operations with IR1 transforms, run `_publish_derived_types` +
     `ir1opt.ir1_optimize_until_done` + `join_blocks_if_possible` + `find_dfo`
     + `loop.analyze` on the cloned component for that entry context's IR2/LLVM
     emission, and restore the canonical IR1 component afterward.

### Worker-local derivation mode

Status: implemented; focused isolation and derivation checks completed.

During `constraint.propagate`, operand-dependent `fndb` hooks defer result
derivation to the pass's current flow facts. Outside that dynamic scope, the
ordinary IR1 derivation hooks run normally. This mode belongs to the synchronous
analysis running on one compiler worker, not the whole process. Its owner is a
plain `threading.local` subclass with an unslotted `active` attribute initialized
to false for each thread. `flow_propagation()` saves that thread's previous value
and restores it in `finally`, including nested scopes and exceptions. Every
mode-dependent hook reads the same thread-local state object. This does not
serialize compilation or change a derivation rule.

SBCL `src/compiler/constraint.lisp`, `find-and-propagate-constraints` and
`constraint-propagate`, dynamically binds constraint-pass state with `LET`;
`src/code/target-thread.lisp`, `start-thread`, initializes thread-local storage
for new workers. Python module globals do not provide those dynamic thread-local
bindings, so the compiler uses the standard-library local-storage facility.
Compiler analyses are synchronous within each worker; this contract does not
claim isolation for interleaving asynchronous analyses on one thread.

A plain `ContextVar` would require an additional worker-boundary reset: on the
free-threaded Python 3.15 host, new threads inherit contexts by default, and the
graph scheduler creates workers without an explicit empty context. A worker
created within active propagation must instead start in ordinary derivation
mode. This state therefore must not use subclass slots, which are shared across
threads rather than stored in each thread's local dictionary. The compiler host
uses its own standard-library `threading.local`; Mule's vendored `threading`
also exposes the Python `_threading_local` implementation for eventual
self-hosting, without establishing native OS-thread creation support.

Event-synchronized checks observe actual known-operation derivation, including
overlapping scopes with out-of-order exits and restoration after both workers
finish. These checks establish state isolation, not the cause of a particular
suite failure or performance measurement.

### 1C. Sequence, string, and container transforms (`seqtran.py` vs `seqtran.lisp`) (proposed)

- **SBCL reference**: `src/compiler/seqtran.lisp` (3,371 lines in SBCL vs
  `mule/compiler/seqtran.py` and `mule/compiler/containtran.py`).
- **Remaining gaps and proposed transforms**:
  1. **IR1-level slice-comparison and slice-parse fusion**: Currently,
     `text[start:stop] == "lit"` (`_fuse_text_slice_compares`),
     `text[i] == "c"` (`_fuse_char_index_compares`), `text.find(...)`
     (`exact_text_find`), and `int(text[start:stop])` (`_fuse_int_slice_parse`)
     are recognized in `ir2tran.py` so they work in `EntryContext`s. Once
     Section 1B provides per-context IR1 specialization, express these
     rewrites as `@deftransform`s on `%compare_*`, `%resolve_method`, and `int`
     in `seqtran.py` so their result types (`BOOLEAN`, `FIXNUM`, `INTEGER`) feed
     directly back into `constraint.py` and downstream IR1 transforms.
  2. **Pure comprehension and generator fusion**: Lower `sum([...])`,
     `any([...])`, `all([...])`, `min([...])`, `max([...])`, and
     `list(x for x in seq)` directly into counted or iterator-driven
     accumulation loops in IR1 without allocating an intermediate list (as
     SBCL's `map` / `reduce` / `every` / `some` transforms do in
     `seqtran.lisp`).
  3. **Constant sequence operations**: Fold `%getitem` on constant `str`,
     `bytes`, and `tuple` with constant or bounded index; fold `%add` /
     `%multiply` on small constant sequences within size limits; specialize
     `"".join(...)` on constant or known-length tuple/list displays into a
     single pre-sized text build.
  4. **Known container methods**: Specialize `dict.get(k, default)`,
     `list.pop()`, `str.startswith(prefix)`, and `str.endswith(suffix)` on
     proven receiver types in `seqtran.py`.

### 1D. Numeric, division, and float transforms (`srctran.py` / `modarith.py` / `float-tran.lisp`) (proposed)

- **SBCL reference**: `src/compiler/srctran.lisp` (6,247 lines),
  `src/compiler/modarith.lisp`, and `src/compiler/float-tran.lisp`.
- **Remaining gaps and proposed transforms**:
  1. **Division and modulo by arbitrary constant integers**: SBCL transforms
     `(truncate x c)` and `(rem x c)` on machine integers into multiply-high +
     shift sequences (`Granlund-Montgomery`). In Mule, when the dividend is
     proven `int64` / `FIXNUM` and the divisor is a non-zero constant integer,
     emit Python floor-division / modulo adjustment over hardware signed
     division or magic-multiplier lowering in IR2/LLVM instead of the generic
     helper when overflow (`INT64_MIN // -1`) is excluded by interval bounds.
  2. **Commutative canonicalization and reassociation**: Normalize
     `const + x -> x + const` and `const & x -> x & const` on proven commutative
     domains (`INTEGER`, `BOOLEAN`) so associative transforms in `modarith.py`
     match regardless of operand order.
  3. **Modular arithmetic cutting (`modarith.lisp`)**: Propagate modular width
     demand backward through `(a + b) & mask`, `(a - b) & mask`, and
     `(a * b) & mask` when `mask = (1 << k) - 1` fits in `int64`, allowing
     intermediate operations to use wrapping 64-bit arithmetic without bigint
     overflow allocation.
  4. **Float and math transforms (`float-tran.lisp`)**: Add `fndb.py` and
     `srctran.py` transforms for builtin `abs(x)`, `min(a, b)`, `max(a, b)`,
     and `float(x)` on proven `INTEGER` and `FLOAT` operands, plus `math`
     module intrinsics (`math.sqrt`, `math.floor`, `math.ceil`, `math.fabs`,
     `math.isinf`, `math.isnan`) lowered to LLVM intrinsics.

---

## 2. Interprocedural and local-call optimizations (`locall.lisp`) (proposed)

- **SBCL reference**: `src/compiler/locall.lisp` (`convert-local-call`,
  `let-convert`, `merge-tail-sets`).
- **Remaining gaps and plan**:
  1. **LET-conversion and local inlining**: Convert single-call non-escaping
     local functions (`def helper(...): ...`) into inline IR1 blocks sharing the
     caller's frame, eliminating closure/frame setup and exposing the body to
     the caller's flow constraints.
  2. **Self-tail-call loop conversion**: Detect self-recursive tail calls in
     functions without active enclosing `try`/`with` cleanups (`CleanupKind` in
     `envanal.py`) and convert them into a loop header jumping to the start of
     the function body after parallel parameter assignment.
  3. **Proven intra-module global calls**: When a module-level function is never
     rebound in the module and no external code can mutate the module dict
     between definition and call (or under an explicit module-sealing / closed
     optimization policy), allow `locall.analyze` to publish its return type to
     callers in IR1 alongside the guarded direct call.

---

## 3. Escape analysis and stack allocation (`envanal.lisp` / `ltn.lisp`) (proposed)

- **SBCL reference**: `src/compiler/envanal.lisp` and `dynamic-extent` handling
  in `src/compiler/ltn.lisp` and `src/compiler/ir2tran.lisp`.
- **Remaining gaps and plan**:
  1. **Non-escaping closure environment elision**: When a nested `Lambda` is
     only called locally (never stored in a data structure, returned, or passed
     to an unknown callee), pass its captured `LambdaVar`s as direct parameters
     or stack-allocated cell pointers rather than heap-allocating closure cells.
  2. **Non-escaping tuple and container elision**: Extend the existing unpacked
     tuple-return fast path (`%tuple.out`) and `ir1opt.py` / `seqtran.py` tuple
     forwarding so temporary tuples (`a, b = b, a` when non-trivial, `for k, v
     in items`, or returning a tuple to a known local caller) never allocate on
     the heap.

---

## 4. Loop optimizations (`loop.lisp` + loop-invariant code motion) (proposed)

- **SBCL reference**: `src/compiler/loop.lisp` (natural loop detection,
  dominators, loop nesting tree).
- **Current Mule state**: `mule/compiler/loop.py` computes dominators, natural
  loops, and loop depth on `Block`s at the end of `ir1_phases`, and `ir1tran.py`
  specializes `for i in range(...)` into native counted loops.
- **Remaining gaps and plan**:
  1. **Loop-invariant code motion (LICM)**: Use the natural loop headers and
     dominator tree from `mule/compiler/loop.py` to hoist loop-invariant
     read-only global cell loads (`GlobalVar` with no `Set` inside the loop and
     no intervening unknown calls), invariant attribute/method resolutions on
     known immutable receivers, and pure `foldable` known calls whose operands
     are defined outside the loop into the loop preheader.
  2. **While-loop induction variable range inference**: Extend `constraint.py`
     to recognize monotonic `while i < n:` / `i += 1` induction variables on
     proven integers and bound `i` to `[init, n - 1]` inside the loop body,
     enabling bounds-check and negative-index elision on sequence indexing.

---

## 5. Representation selection and unboxed SSA values across blocks (`represent.lisp`) (partially implemented)

- **SBCL reference**: `src/compiler/represent.lisp` (`select-tn-representation`,
  automatic coercion VOPs at block boundaries).
- **Current Mule state**: `mule/compiler/represent.py` and `vops.py` keep raw
  `i64` / `double` values for `mule.int64` / `mule.float64` annotated locals and
  range-loop counters, and unbox fixnum arithmetic inside basic blocks.
  Ordinary float locals can also remain raw across blocks. Identity-aware
  selection and explicit float coercions separate numeric-only copies from
  object-observable sources, as described below.
- **Proposed plan**:
  1. **Flow-proven unboxed local TNs (`SC_SIGNED_REG` / `SC_DOUBLE_REG`)**:
     When `represent.py` sees that every write and read of a non-closed-over
     `LambdaVar` in an `IR2Component` (general or specialized `EntryContext`) has
     a proven `FIXNUM` (`IntegerType` fitting `[FIXNUM_MIN, FIXNUM_MAX]`, not
     `may_be_unbound`), `int64`, or `FLOAT` type, assign its local TN to
     `SC_SIGNED_REG` (`i64`) or `SC_DOUBLE_REG` (`double`) across the entire
     function body and insert tag/box coercions only at boundaries where a
     generic runtime helper requires a `ptr` `Value`.

### Identity-aware float representation coercions (implemented)

SBCL `src/compiler/represent.lisp`, `add-representation-costs` and
`select-tn-representation`, choose storage using operand and move costs.
`coerce-vop-operands` and `emit-moves-and-coercions` subsequently insert
conversion VOPs and replace generic moves. Mule retains these separate
selection and conversion stages, after internal call conventions settle.

Initial float selection ignores unresolved copy costs. A subsequent cost pass
charges representation changes across selected copy edges and VOP restrictions.
It changes a selection only when the total cost decreases, retaining ties to
terminate on cyclic copy graphs. This keeps profitable boxed arithmetic operands
and guarded box reuse while allowing independent raw numeric branches.

Python object identity requires an additional legality constraint. Float values
that reach descriptor-only consumers retain their original objects. Demand for
that identity propagates backward through moves, bound local reads, and unary
positive operations, including every writer of a mutable local. Deletion and
possibly unbound local reads also retain descriptor storage. This analysis does
not split a local into separate live ranges. Numeric-only copies may independently
use doubles, even when their source must retain its box. Fresh arithmetic can
construct the one authoritative box for a descriptor result; copying a raw value
must not reconstruct an identity demanded from its original object.

Unlike SBCL's register moves under tracing collection, Mule copies also transfer,
retain, borrow, and release references. A converted local read must retain the
original read metadata and first produce a temporary in the source representation.
A converted assignment must convert its input before the original move overwrites
the destination. Conversion temporaries carry the original IR1 node and enter the
ordinary cleanup analysis. The conversion stage rebuilds IR2 operand indices before
ownership and liveness analysis. These adaptations preserve object aliases and
exception cleanup while allowing numeric branches of a copy graph to use raw
storage. Numeric-call eligibility and native object-adapter freshness proofs remain
conservative independent legality checks.

### Float locals and remaining representation work

Representation selection preserves `BOXED_FLOAT` for a local when every value
assigned to it has that primitive type. Numeric-only float locals, branch joins,
and loop-carried values can use doubles in ordinary object-mode functions.
The explicit coercion stage supports `DESCRIPTOR` / `DOUBLE` changes on float
copies and VOP operands. It preserves the original local-read and assignment
operations instead of teaching those generators to perform implicit conversions.

Returns, unknown calls, identity comparisons, captured values, deletion, and
possibly unbound reads retain object boundaries. Backward identity demand includes
copies into mixed-type descriptor locals, not only float destinations. It does not
force independent numeric-only copies to remain boxed. Generic object entries
returning their float accumulator still conservatively use boxed accumulator
storage; the numeric internal conventions below remove that boundary for numeric
callers. Splitting one mutable local's live ranges is not implemented.

Generalizing explicit coercion insertion to integer and other storage classes,
extending identity-safe materialization, and using target-specific conversion
costs remain proposed work. Existing integer and whole-native entry conventions
retain their implemented generators. This increment does not establish complete
SBCL representation-selection parity.

### Numeric internal calling conventions (implemented)

SBCL's `src/compiler/ltn.lisp`, `ltn-analyze-return`, annotates return locations
independently of the representations used by other operations in the function.
`src/compiler/ir2tran.lisp`, `ir2-convert-local-known-call` and
`ir2-convert-return`, deliver known local calls and returns through those
locations. Mule's counterpart separates internal `CallConvention` metadata from
the all-or-nothing `NativePlan` used for fully native bodies.

The implementation selects same-unit proven positional calls with a statically
valid parameter mapping. It uses an existing semantic entry context whose
parameter facts the caller proves without guards. A float result qualifies only
when all assignment-connected caller uses permit doubles; a bounded integer
result requires a non-boolean fixnum proof. Demand-generated numeric entries
return `double` or `i64`. Float parameters use doubles only when their complete
callee copy group admits raw storage. Proven fixnum parameters can cross as
`i64` and retain the existing tagged representation for object operations.
Other parameters retain their owned-object convention.

Numeric entries preserve ordinary object operations, frame tracking, reference
cleanup, handlers, and traceback generation. They do not use `emit_raw` or its
whole-native-body ownership assumptions. A separate error flag reports failure,
following the existing native-entry convention: every numeric bit pattern remains
a valid result, so no numeric sentinel can signal an exception. The caller checks
that flag before delivering a result and retains the actual callable while its
borrowed closure is in use.

Unlike SBCL's tracing collector, Mule currently transfers owned object arguments
and releases owned temporaries explicitly. Numeric arguments carry no references.
This ownership adaptation requires separate handling at mixed call boundaries.
The generic wrapper never calls a numeric entry: forwarded object identities,
pooled literal identities, and escaped aliases keep their ordinary object path.
The numeric-use restriction allows a numeric entry to extract a still-boxed
return value without reconstructing object identities. Guarded unknown calls,
keyword/default adaptation, and calls to other compilation units remain on the
existing object convention in this increment.

Object-entry adaptation of existing fully native float bodies also requires
return provenance. SBCL's representation coercions do not establish Python's
observable float identity. Mule therefore permits scalar-result reboxing in an
object adapter only when every return copy group comes from fresh-result VOPs
and has no intervening identity-bearing use. Float arithmetic and unary negation
can establish freshness; pooled constants, forwarded parameters, captures, and
unknown calls cannot. Otherwise the generic object body remains authoritative,
while the raw native entry remains available to native callers.

### LLVM-visible private boxes and coercions (implemented)

Layouts describe allocation, active fields, payload access, owned-resource
cleanup, and private lifetime helpers. Float and signed-word construction and
proven extraction expose ordinary LLVM stores and loads. Integer words retain
the existing tagged/GMP representation, with guarded support for other limb
ABIs. Raw object and limb storage have matching allocation-family contracts,
without restricted
memory-effect claims. Generic construction, shared ownership, and destruction
retain their existing contracts.

The representation stage proves private ownership for complete copy groups
whose origins are fresh and whose uses explicitly cannot capture references.
It rejects pooled constants, arguments, unknown calls, stored or returned
references, and in-place forwarding. Proven factory results require fresh
summaries for every possible normal object entry. Private ownership is not
inferred from float type alone, and groups with different lifetime modes do not
share physical slots.

Private counts and terminal storage release let LLVM eliminate eligible
box/load/free sequences itself, including without LTO. No transform substitutes
an unboxed value for a box/unbox pattern. This is a limited private-box ownership proof,
not complete closure/container escape analysis. See the
[lowering contract](native-lowering.md#llvm-visible-boxes) and
[optimization evidence](performance.md#private-leaf-box-elimination-2026-10-20).
