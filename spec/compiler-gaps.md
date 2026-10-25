# Remaining SBCL compiler and IR1 transform gaps

Status: **implemented** for declared parameter contracts and sound container
element propagation (Section 0A), general-context flow-type feedback into IR1
transforms, unified constant-lvar/dead-operand helpers, and monotonic
`derive_node_type` (Section 1A), entry-context IR1 specialization / closed-call
parameter promotion (Section 1B), local closure extent / fixed aggregate scalar
replacement (Section 3), loop optimizations / loop-invariant code motion /
induction-variable range and relational constraint inference (Section 4), and
identity-aware float storage and coercions, numeric internal calling
conventions, LLVM-visible private leaf boxes (parts of Section 5); **proposed**
for Sections 0B–0D, Sections 1C–2, and the rest of Section 5.

This document records the remaining architectural and transform gaps between
SBCL's compiler (`~/checkout/sbcl/src/compiler/`) and Mule (`mule/compiler/`),
together with the SBCL source references, Python semantic adaptations, and
staged implementation plan for each area.

## Builtin-type conformance coverage (partially implemented)

Status: initial focused conformance programs for `object`, `type`, `bool`, `int`,
`float`, `complex`, `str`, `bytes`, `bytearray`, `memoryview`, `list`, `tuple`,
`dict` (including views), `set`, `frozenset`, `range`, `slice`, and `for`-loop
iteration are in place and recorded in `conformance/gaps.toml`; full method-by-method
CPython 3.14 parity and remaining runtime descriptor fixes remain open.

The conformance suite contains builtin, container, and protocol programs, and now
separates passing core type/descriptor/subclass cases from isolated runtime gaps:

- **Passing focused conformance cases**:
  - `conformance/control_flow/for_loops.py`: `for`-loop iteration over `list`,
    `tuple`, `str`, `bytes`, `bytearray`, `dict`, `dict.keys()`, `dict.values()`,
    `dict.items()`, `set`, `frozenset`, `range`, `enumerate`, `zip`, `reversed`,
    `map`, `filter`, custom `__iter__`/`__next__`, sequence `__getitem__`
    iteration, nested/starred unpacking targets, `break`/`continue`/`else`, and
    non-iterable `TypeError`.
  - `conformance/builtins/object_type_descriptors.py`: `object` and `type`
    construction, dynamic `type(name, bases, namespace)` class creation,
    `object.__eq__`/`__ne__` (including `NotImplemented` fallback),
    `object.__getattribute__`/`__setattr__`/`__delattr__`, `type.mro`,
    `type.__subclasses__`, `type.__getattribute__`/`__setattr__`/`__delattr__`,
    and receiver `TypeError`s.
  - `conformance/builtins/range_slice_basics.py`: `range` and `slice`
    constructors, `start`/`stop`/`step` attributes, `len(range)`, indexing,
    slicing, membership, sequence slicing with explicit `slice` objects, and
    error checks (`ValueError` on zero step, `IndexError` on out-of-range index,
    `TypeError` on `hash(slice)`).
  - `conformance/numbers/int_bool_descriptors.py`: `int` and `bool` constructors,
    base parsing, `bit_length`, `to_bytes`/`from_bytes`, numeric properties,
    explicit descriptor calls (`int.__int__`, `int.__index__`, `int.__float__`,
    `int.__round__`, `int.__divmod__`, `int.__repr__`, `bool.__repr__`),
    `int` subclass inheritance/overrides, and receiver `TypeError`s.
  - `conformance/numbers/float_descriptors.py`: `float` constructors, arithmetic,
    `is_integer`, `as_integer_ratio`, `hex`, numeric properties, explicit
    descriptor calls (`float.__int__`, `float.__float__`, `float.__round__`,
    `float.__divmod__`, `float.is_integer`, `float.as_integer_ratio`,
    `float.hex`), `float` subclassing, and receiver `TypeError`s.
  - `conformance/strings/str_descriptors.py`: `str` constructors, indexing,
    slicing, concatenation, repetition, comparisons, `in`, explicit descriptor
    calls (`str.__len__`, `str.__getitem__`, `str.__contains__`, `str.upper`,
    `str.lower`, `str.strip`, `str.split`, `str.join`, `str.replace`,
    `str.startswith`, `str.endswith`, `str.find`, `str.count`, `str.encode`),
    `str` subclass overrides vs base descriptor calls, and receiver `TypeError`s.
  - `conformance/strings/bytes_bytearray_memoryview.py`: `bytes`, `bytearray`,
    and `memoryview` constructors, indexing, slicing, methods, in-place
    `bytearray` and writable `memoryview` mutation, subclassing, and `ValueError`
    range checks.
  - `conformance/containers/list_tuple_descriptors.py`: `list` and `tuple`
    constructors, indexing, slicing, methods, explicit base-descriptor calls
    (`list.__len__`, `list.__getitem__`, `list.__setitem__`, `list.__delitem__`,
    `list.append`, `list.extend`, `list.insert`, `list.pop`, `list.remove`,
    `list.reverse`, `list.sort`, `list.count`, `list.index`, `list.copy`,
    `list.clear`, `tuple.__len__`, `tuple.__getitem__`, `tuple.count`,
    `tuple.index`), `list`/`tuple` subclass overrides vs base descriptor calls,
    and receiver `TypeError`s.
  - `conformance/containers/dict_set_descriptors.py`: `dict`, `dict_keys`,
    `dict_values`, `dict_items`, `set`, and `frozenset` constructors, views,
    set algebra, explicit descriptor calls, `dict` subclass `__missing__` and
    `__getitem__` overrides vs `dict.__getitem__`, `set` subclassing, and
    `KeyError`/unhashable `TypeError` checks.
- **Isolated builtin-type conformance gaps recorded in `conformance/gaps.toml`**:
  - `conformance/classes/builtin_subclass_len_override.py`: builtin `len(sub)` on
    `list` and `str` subclasses currently takes the `mule_fast_len` storage fast
    path instead of consulting a subclass `__len__` override (`sub.__len__()` and
    `Base.__len__(sub)` already distinguish override vs storage length).
  - `conformance/numbers/complex_descriptors.py`: `complex.__repr__` formats
    integral float components with `.0` (`(3.0-4.0j)` instead of `(3-4j)`), and
    `complex.conjugate(1.0)` raises `AttributeError` instead of `TypeError`.
  - `conformance/strings/bytes_bytearray_descriptors.py`: `BytesStorage` (`bytes`)
    does not yet expose `bytes.__len__` or ASCII case methods (`upper`, `lower`)
    as class descriptors, and `bytes`/`bytearray` descriptors do not validate
    receiver types with `TypeError`.
  - `conformance/containers/dict_set_receiver_errors.py`: explicit `dict`, `set`,
    and `frozenset` descriptor calls (`dict.__len__([])`, `set.add([], 1)`) raise
    `AttributeError` on internal table fields instead of `TypeError` when given a
    non-instance receiver.
  - Broader method-completeness files (`conformance/numbers/int_methods.py`,
    `conformance/numbers/float_methods.py`, `conformance/builtins/range_objects.py`,
    `conformance/classes/builtin_subclasses.py`) remain recorded gaps for
    `int.bit_count` / `int.as_integer_ratio` / `int.is_integer`, `float.fromhex`,
    `range.count` / `range.index` / `range.__eq__` / `range.__repr__`, and
    `str.format_map` / `tuple.__new__`.

Remaining coverage and implementation work:

1. Expand method-by-method and operator-dunder conformance cases across remaining
   builtin types against the Python 3.14 oracle.
2. Fix the isolated runtime gaps above (`len()` on `list`/`str` subclasses,
   `bytes.__len__` and ASCII case methods, receiver type validation on `complex` /
   `bytes` / `bytearray` / `dict` / `set` / `frozenset` descriptors, `range`
   methods/equality/repr, `int.bit_count`/`as_integer_ratio`/`is_integer`, and
   `float.fromhex`).
3. Keep cases focused so an unsupported feature cannot hide unrelated builtin
   coverage, and regenerate `conformance/gaps.toml` via
   `uv run python -m tools.record_conformance_gaps` whenever compiler or stdlib
   changes land.

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
  5. **`bytes` and `bytearray` type representation, element-type derivation,
     and physical indexing/mutation VOPs (`array-tran.lisp`)**:
     - **SBCL reference**: `src/compiler/array-tran.lisp` (`deftransform aref`
       and `data-vector-ref` / `data-vector-set` on
       `(simple-array (unsigned-byte 8) (*))`, which derive element type
       `(unsigned-byte 8)` = `[0, 255]` and lower to inline byte-load /
       byte-store VOPs `data-vector-ref-with-offset/simple-array-unsigned-byte-8`
       and `data-vector-set-with-offset/simple-array-unsigned-byte-8` in
       `src/compiler/x86-64/array.lisp`).
     - **Implemented**:
       - `BytesType` (`ctype.py`) implements `dispatch_domain()` returning
         `BYTES_TYPE` (unless `may_be_unbound`), and `locall.py` includes
         `BYTES_TYPE` alongside `LIST_TYPE` and `TEXT_TYPE` when seeding
         `observed` entry contexts. Consequently, `BYTES_TYPE` parameters
         preserve their `IntegerType(0, 255)` `getitem_type` and `iterate_type`
         across both `fixnum` and `observed` entry contexts.
     - **Remaining Mule defects (observed in `perf/suite/bytes_codec.py`)**:
       - `bytearray` in `stdlib/_sequences.py` is a compiled Python class
         (`InstanceType`), whose exported class type in `modules.py` is widened
         to `UNKNOWN` (`STORAGE_CLASS_TYPES.get(class_name, UNKNOWN)`), and
         `fndb.py` has no `KNOWN` entry for `bytes(...)` or `bytearray(...)`.
         Consequently, `decoded = bytearray()` in `bytes_codec.py` calls the
         global `bytearray` dynamically and derives `UNKNOWN`.
       - Because `decoded` has `derived_type = UNKNOWN`, `decoded.append(b)` in
         the inner packet-decoding loop resolves `.append` via inline cache and
         speculatively dispatches to `ListStorage.append` (which always misses
         on `ByteArrayStorage` and falls back to dynamic call on every byte),
         and indexing/iterating `decoded` (`decoded[k]`, `for b in decoded:`)
         derives `UNKNOWN` rather than `IntegerType(0, 255)`.
       - `vops.py` (`protocol-getitem` / `protocol-setitem`) has no inline
         physical byte-vector load/store fast path for proven `BYTES_TYPE` or
         `bytearray` with `FIXNUM` index, calling `@mule.fast.getitem` /
         `@mule.fast.setitem.move` out of line instead of inline bounds check +
         `load i8` / `store i8`.
     - **Required implementation**:
       1. Export `bytearray`'s exact `InstanceType` / storage type in
          `modules.py`, map `bytearray` in `annotation_types.py` and
          `ir1tran.py`, and register `bytes` and `bytearray` in `fndb.py`.
       2. Derive `IntegerType(0, 255)` for `getitem(bytearray, INTEGER)` and
          `iterate` / `next_item` on `bytearray` in `ctype.py`, `fndb.py`, and
          `constraint.py`.
       3. Emit inline byte-buffer load/store VOPs in `vops.py` for
          `getitem(BYTES_TYPE | bytearray, FIXNUM)` (`u8` zero-extended to
          tagged fixnum `(zext i8 << 1) | 1`) and
          `setitem(bytearray, FIXNUM, FIXNUM)` (bounds check + `[0, 255]`
          range check + `store i8`), and specialize `bytearray.append` on
          `bytearray` receivers with proven `[0, 255]` or `FIXNUM` operands.
  6. **Direct `for`-loop lowering over proven sequences (`seqtran.lisp`
     `dolist` / vector loop transforms)**:
     - **SBCL reference**: `src/compiler/seqtran.lisp` (`define-source-transform dolist`,
       `map` / `dosequence` transforms over vectors and lists), which lower
       sequence iteration directly into pointer/index loops without allocating
       iterator objects or signaling end-of-iteration conditions.
     - **Current Mule defect (observed in `perf/suite/bytes_codec.py`)**:
       - While `for j in range(...)` lowers to a counted loop in `ir1tran.py`,
         `for packet in data:` (with `data: list[bytes]`) and `for b in decoded:`
         (with `decoded: bytearray`) allocate a heap iterator via
         `@mule.fast.iterate.move`, call `@mule.fast.next_item` on each step,
         and terminate by raising and catching `StopIteration`
         (`@mule_catch_exception` + `@"mule.protocol.iteration_exhausted"`).
     - **Required implementation**:
       - When the iterable of a `for` loop is proven to be a non-subclassed
         `LIST_TYPE`, `TUPLE_TYPE`, `BYTES_TYPE`, or `BYTEARRAY_TYPE` (or under
         an entry/loop guard on exact storage kind), lower or transform the
         iterator protocol into a direct indexed loop (`idx = 0; while idx < len(seq): ...`)
         reading storage directly at `idx` without allocating an iterator or
         raising `StopIteration` on termination (re-reading current length on
         each step for mutable `list` / `bytearray` to preserve Python's
         mutation-during-iteration semantics).

### 1D. Numeric, division, and float transforms (`srctran.py` / `modarith.py` / `float-tran.lisp`) (partially implemented)

- **SBCL reference**: `src/compiler/srctran.lisp` (6,247 lines),
  `src/compiler/modarith.lisp`, and `src/compiler/float-tran.lisp`.
- **Implemented**:
  1. **Division and modulo by arbitrary constant and proven non-zero fixnum
     integers (`vops.py`, `ir2tran.py`, `locall.py`)**:
     - Following SBCL's `truncate-by-constant` and `rem` / `mod` lowering
       (`src/compiler/srctran.lisp`, `src/compiler/x86-64/arith.lisp`),
       `_binary_generic` (`mule/compiler/vops.py`) lowers fixnum `//`
       (`opcode == 4`) and `%` (`opcode == 5`) inline whenever
       `fixnums and result_type.is_fixnum`:
       - When `left_type.minimum >= 0` and `right_type.minimum > 0`, emits
         inline `udiv i64` (`//`) or `urem i64` (`%`) via `gen.native_result`,
         which LLVM lowers directly to Granlund-Montgomery multiply-high + shift
         register sequences for constant divisors without calls or branches.
       - When `right_type` is proven non-zero (`right_type.minimum > 0` or
         `right_type.maximum < 0`) with possibly negative `left_type`, emits
         inline branchless sign-adjusted `sdiv i64` / `srem i64` via
         `gen.native_result`.
     - `_binary_helper` (`mule/compiler/vops.py`) marks `@mule.fast.binary.*`
       and `@mule.fixnum.binary.*` `alwaysinline` and omits the unused
       `sdiv` quotient and redundant `_RANGE` check from `op == 5` (`%`).
     - `_borrows_operand` (`mule/compiler/ir2tran.py`) includes
       `binary-modulo` and `inplace-modulo` when `lhs` is proven `REAL`, and
       `_PURE_FIXNUM_VOPS` / `_PURE_BINARY_ARITH_VOPS` include
       `binary-floor_divide`, `binary-modulo`, `inplace-floor_divide`,
       `inplace-modulo`, `binary-left_shift`, and `binary-right_shift`.
     - `_SpecializesBody.visit_Combination` (`mule/compiler/locall.py`)
       preserves specialized entry contexts whenever a known numeric/comparison
       operation's operand narrows from non-fixnum (`PythonIntegerType` /
       `INTEGER`) to `is_fixnum` (or `FLOAT`), even when the operation's result
       type (`BOOLEAN` or bounded `IntegerType`) is already proven in `general`.
- **Remaining gaps and proposed transforms**:
  1. **Commutative canonicalization and reassociation**: Normalize
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

## 3. Escape analysis and allocation elimination (`envanal.lisp` / `ltn.lisp`)

- **SBCL reference**: `src/compiler/envanal.lisp` and `dynamic-extent` handling
  in `src/compiler/ltn.lisp` and `src/compiler/ir2tran.lisp`.
- **Implemented**:
  - `escape.py` extends the existing variable/parameter escape solver with
    allocation-result and alias facts and implicit environment containment.
  - Stable direct-only local functions use a compiler-owned environment ABI.
    Mutable captures reference home-frame slots when every retaining closure
    has local extent. Escaping siblings keep the authoritative heap cell.
    Function objects, closure tuples, and unobservable annotation companions
    disappear on eligible paths. Recursive calls, zero-argument functions,
    aliases, native scalar entries, and existing unpacked returns retain their
    respective semantics.
  - `scalar.py` consumes allocation escape proofs for fixed lists and tuples,
    including interprocedural non-escaping calls to local and proven guarded
    functions. Constant-index reads, fixed-index list writes, length,
    structural unpack, and aliases become scalar operations within a block or
    across multiple blocks for a single local with inert children, and scalar
    child slots participate in unboxed representation selection (`DOUBLE_REG`)
    and frame slot packing. Normal and error cleanup preserve child ownership
    and reverse destruction order.
- **Conservative allocation boundaries**: Observable functions and cells,
  defaults/decorators, suspended owning activations, unknown uses, resizing,
  overlapping aggregate lifetimes, pending element checks, and unrepresentable
  cleanup order retain ordinary allocations. This is not arbitrary heap-object
  scalar replacement or stack allocation of materialized Python containers.
- **Remaining extensions**: Broader inter-block aggregate lifetime proofs,
  escape summaries across additional call boundaries, and more container
  consumers can extend the existing analysis and lowering. Existing tuple
  projection and unpacked-return mechanisms remain in use. The precise
  [local allocation extent contract](native-lowering.md#local-allocation-extent)
  documents the SBCL adaptation and lifetime restrictions.

---

## 4. Loop optimizations (`loop.lisp` + loop-invariant code motion) (implemented)

- **SBCL reference**: `src/compiler/loop.lisp` (natural loop detection,
  dominators, loop nesting tree), `src/compiler/constraint.lisp`
  (`constraint-propagate-back` post-operation type narrowing), and
  `src/compiler/equality-constraints.lisp` (`add-eq-constraint`,
  `add-difference-constraint`, `constrain-integer-type`).
- **Implemented Mule architecture**:
  1. **Natural loop analysis and IR1 loop-invariant code motion (LICM)**
     (`mule/compiler/loop.py`): `loop_analyze` computes dominators, natural
     loops (`NaturalLoop`), and loop nesting depth on `Component`s.
     `hoist_loop_invariants` runs in `_ir1_optimize_phase_1` (`main.py`) and in
     specialized `EntryContext` optimization (`locall.py`) after constraint
     propagation and IR1 transforms. For each natural loop in inside-out
     postorder (skipping loops containing `%yield` / `%await` suspension points
     or non-entry `Bind`s), it identifies or splits an explicit preheader block
     outside the loop and hoists loop-invariant expressions from blocks that
     dominate all loop exits (`loop.exits`, or all back-edge sources when the
     loop has no normal exits):
     - read-only module/builtin global cell loads (`Ref(GlobalVar)`) when the
       loop has no impure calls and no writes to the global, and every reference
       is proven bound to a stable module/function/class/numeric/sequence value;
     - pure `foldable` known calls (`Combination`) on proven total domains:
       integer/float `+`, `-`, `*`, unary `+`/`-`, bitwise `&`/`|`/`^`/`~`,
       shifts `<<`/`>>` with proven shift count in `[0, 61]`, `//` and `%` with
       proven non-zero integer divisor (`>= 1` or `<= -1`), `_text_length` on
       proven `str`, and `_storage_length` on proven `bytes`, `tuple`, or
       unmutated `list`;
     - common invariant subexpressions across multiple loop blocks are
       deduplicated into a single preheader temporary `LambdaVar`.
  2. **Threshold widening, relational difference constraints, and post-indexing
     bounds narrowing** (`mule/compiler/ctype.py`,
     `mule/compiler/constraint.py`, `mule/compiler/protocols.py`):
     - `widen` / `widen_integer` in `ctype.py` accepts per-function integer
       comparison thresholds collected from AST comparison and `range(...)`
       literals (`_collect_comparison_thresholds` in `constraint.py`), so loop
       back-edge widening stops at literal loop bounds above `1024` (e.g.
       `while i < 2000:`) before falling back to `FIXNUM_MAX` / unbounded.
     - `IntegerType.narrow_equality` narrows both endpoints when an interval is
       compared with `!=` against a singleton endpoint (`[0, n]` with `!= n` ->
       `[0, n - 1]`).
     - `_Propagation` in `constraint.py` maintains a relational difference-bound
       domain (`x - y <= c`, following `equality-constraints.lisp`) across linear
       updates (`x = x + d`, `x = y + d`), initialization (`i = c` relative to
       bounded integer locals), and ordered comparisons (`<`, `<=`, `>`, `>=`,
       `==`, `!=` on `u + c0` vs `v + c1`), propagating relational bounds into
       variable intervals, `u - v` subtractions (`n - i >= 1` inside
       `while i < n:`), comparison folding, and loop-exit equality (`i == n`
       after `while i < n: i += 1`).
     - Sequence indexing and storage primitives (`%getitem`, `%setitem`,
       `_storage_read`, `_storage_write`, `_text_codepoint`, `_text_character`,
       `_text_slice`, `_native_read_byte`) call `visitor.prove_argument_type`
       with `receiver_type.valid_index_bounds()` so successful indexing narrows
       both direct index variables (`t[i]`) and linear index offsets (`t[i + 1]`)
       for subsequent operations in the block.

---

## 5. Representation selection and unboxed SSA values across blocks (`represent.lisp`) (partially implemented)

- **SBCL reference**: `src/compiler/represent.lisp` (`select-tn-representation`,
  automatic coercion VOPs at block boundaries).
- **Current Mule state**: `mule/compiler/represent.py` and `vops.py` keep raw
  `i64` / `double` values for `mule_types.int64` / `mule_types.float64` annotated locals and
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
