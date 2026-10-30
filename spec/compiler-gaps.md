# Remaining SBCL compiler and IR1 transform gaps

Status: **implemented** for builtin-type conformance coverage across all 254
conformance programs (`conformance/gaps.toml` has 0 recorded gaps), declared
parameter contracts and sound container element propagation (Section 0A), typed
`dict`/`set` storage and fixnum hash clipping (Section 0B), `InstanceType(cls)`
method specialization and return contracts (Section 0C), borrowed/unboxed
container element reads (Section 0D), general-context flow-type feedback into
IR1 transforms, unified constant-lvar/dead-operand helpers, and monotonic
`derive_node_type` (Section 1A), entry-context IR1 specialization / closed-call
parameter promotion / worker-local derivation mode (Section 1B), fixnum
division/modulo, shift intervals, 2-arg `min`/`max`, and `math.sqrt`
(parts of Section 1D), local closure extent / fixed aggregate scalar replacement
(Section 3), loop optimizations / loop-invariant code motion / induction-variable
range and relational constraint inference (Section 4), identity-aware float
storage and coercions, flow-proven fixnum locals, numeric internal calling
conventions, and LLVM-visible private leaf boxes (parts of Section 5),
module-level constant folding via write-set tracking (Section 6A), call-site
argument propagation (`propagate-to-args`) and structural method-receiver typing
(Section 6B), IR1 combination matching (`lvar_matches` / `splice_fun_args`) and
slice-comparison/int-parse transforms (Section 6C), symbolic sequence-length
constraints and dead-allocation flushing (Section 6E), general tuple delivery
with per-store liveness (Section 6G), general fixed-slot layout access and
singleton-list fill (Section 6J), pre-representation calling-convention
selection and inline Latin-1 string equality (Section 6L), and `SetType`
lattice, IR1 set/dict transforms, and pre-sized `_Table` set operations
(Section 6M); **proposed** for
Sections 1C, 2, the remaining items in Sections 1D and 5, Sections 6D, 6F, 6H, 6I, 6K, and the remaining SBCL parity gaps identified in the
24-workload and `pyperformance` suite reviews (Sections 6N–6R); **in progress**
for SBCL conservative container types, word vectors, and declared instance
fields (Section 6V).

This document records the remaining architectural and transform gaps between
SBCL's compiler (`~/checkout/sbcl/src/compiler/`) and Mule (`mule/compiler/`,
`stdlib/`, `mule/runtime/`), together with the SBCL source references, Python
semantic adaptations, and staged implementation plan for each area.

## Builtin-type conformance coverage (implemented)

Status: **implemented** across all 254 programs in `conformance/` (19 categories,
`0` recorded gaps in `conformance/gaps.toml` as of commit `1c0cbcc`).

The conformance suite covers `object`, `type`, `bool`, `int`, `float`,
`complex`, `str`, `bytes`, `bytearray`, `memoryview`, `list`, `tuple`, `dict`
(including `dict_keys`, `dict_values`, `dict_items`), `set`, `frozenset`,
`range`, `slice`, `for`-loop iteration, descriptors, and builtin subclassing:

- **Passing core type, descriptor, and protocol suites**:
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
  - `conformance/builtins/range_slice_basics.py` and
    `conformance/builtins/range_objects.py`: `range` and `slice` constructors,
    `start`/`stop`/`step` attributes, `len(range)`, indexing, slicing,
    membership, `range.count`, `range.index`, `range.__eq__`, `range.__repr__`,
    sequence slicing with explicit `slice` objects, and error checks
    (`ValueError` on zero step, `IndexError` on out-of-range index, `TypeError`
    on `hash(slice)`).
  - `conformance/numbers/int_bool_descriptors.py` and
    `conformance/numbers/int_methods.py`: `int` and `bool` constructors, base
    parsing, `bit_length`, `bit_count`, `as_integer_ratio`, `is_integer`,
    `to_bytes`/`from_bytes`, numeric properties, explicit descriptor calls
    (`int.__int__`, `int.__index__`, `int.__float__`, `int.__round__`,
    `int.__divmod__`, `int.__repr__`, `bool.__repr__`), `int` subclass
    inheritance/overrides, and receiver `TypeError`s.
  - `conformance/numbers/float_descriptors.py` and
    `conformance/numbers/float_methods.py`: `float` constructors, arithmetic,
    `is_integer`, `as_integer_ratio`, `hex`, `float.fromhex`, numeric properties,
    explicit descriptor calls (`float.__int__`, `float.__float__`,
    `float.__round__`, `float.__divmod__`, `float.is_integer`,
    `float.as_integer_ratio`, `float.hex`), `float` subclassing, and receiver
    `TypeError`s.
  - `conformance/numbers/complex_descriptors.py`: `complex` construction,
    `real`/`imag` descriptors, `conjugate`, CPython-matching `complex.__repr__`
    formatting of integral float components, and receiver `TypeError` validation.
  - `conformance/strings/str_descriptors.py`: `str` constructors, indexing,
    slicing, concatenation, repetition, comparisons, `in`, `str.format_map`,
    explicit descriptor calls (`str.__len__`, `str.__getitem__`,
    `str.__contains__`, `str.upper`, `str.lower`, `str.strip`, `str.split`,
    `str.join`, `str.replace`, `str.startswith`, `str.endswith`, `str.find`,
    `str.count`, `str.encode`), `str` subclass overrides vs base descriptor
    calls, and receiver `TypeError`s.
  - `conformance/strings/bytes_bytearray_memoryview.py` and
    `conformance/strings/bytes_bytearray_descriptors.py`: `bytes`, `bytearray`,
    and `memoryview` constructors, indexing, slicing, `bytes.__len__`, ASCII case
    methods (`upper`, `lower`), in-place `bytearray` and writable `memoryview`
    mutation, subclassing, receiver `TypeError` checks, and `ValueError` range
    checks.
  - `conformance/containers/list_tuple_descriptors.py`: `list` and `tuple`
    constructors (including `tuple.__new__`), indexing, slicing, methods,
    explicit base-descriptor calls (`list.__len__`, `list.__getitem__`,
    `list.__setitem__`, `list.__delitem__`, `list.append`, `list.extend`,
    `list.insert`, `list.pop`, `list.remove`, `list.reverse`, `list.sort`,
    `list.count`, `list.index`, `list.copy`, `list.clear`, `tuple.__len__`,
    `tuple.__getitem__`, `tuple.count`, `tuple.index`), `list`/`tuple` subclass
    overrides vs base descriptor calls, and receiver `TypeError`s.
  - `conformance/containers/dict_set_descriptors.py` and
    `conformance/containers/dict_set_receiver_errors.py`: `dict`, `dict_keys`,
    `dict_values`, `dict_items`, `set`, and `frozenset` constructors, views, set
    algebra, explicit descriptor calls with receiver `TypeError` validation,
    `dict` subclass `__missing__` and `__getitem__` overrides vs
    `dict.__getitem__`, `set` subclassing, and `KeyError`/unhashable `TypeError`
    checks.
  - `conformance/classes/builtin_subclasses.py` and
    `conformance/classes/builtin_subclass_len_override.py`: subclassing builtin
    types and consulting subclass `__len__` overrides on `len(sub)` while
    keeping exact-type `_storage_length` / `_text_length` fast paths on proven
    exact builtin instances.

Ongoing maintenance contract:

1. Expand method-by-method and operator-dunder conformance cases across builtin
   types and standard-library modules against the Python 3.14 oracle as new
   surfaces are added.
2. Keep cases focused so an unsupported feature cannot hide unrelated builtin
   coverage, and regenerate `conformance/gaps.toml` via
   `uv run python -m tools.record_conformance_gaps` whenever compiler or stdlib
   changes land.

## 0. High-impact architectural parity gaps from the benchmark suite audit (implemented; follow-up gaps in Section 6)

An initial audit of the compiled IR1, `locall` summaries, and `stdlib/_dict.py`
across the original 14 benchmark workloads
([performance.md](performance.md#suite-wide-benchmark-audit-and-root-cause-diagnosis-2026-10-22))
showed that Mule's largest slowdowns relative to CPython and SBCL came from four
departures from SBCL's compiler and runtime architecture (Sections 0A–0D below),
all of which are now **implemented**.

### Current 24-workload benchmark status (`docs/assets/results.json`, 2026-10-28, commit `b69344598`)

With Sections 0A–0D, 1A–1B, 3, 4, 5 (parts), 6A–6C, 6E, 6G, 6J, and 6L in place
(plus commit `9a8b4f8` eliminating `str.split()` list allocation in counted
whitespace token loops), the expanded 24-workload suite (`Mule -O2` without LTO
vs. CPython 3.15.0 and SBCL 2.6.4, median kernel seconds across 5 samples) breaks
down into four groups:

| Category | Workload | CPython 3.15.0 (s) | Mule `-O2` (s) | SBCL 2.6.4 (s) | Mule / CPython | Mule / SBCL | Primary status / remaining gap |
| --- | --- | ---: | ---: | ---: | ---: | ---: | --- |
| **1. Faster than both CPython & SBCL (7)** | `float_nbody_typed` | 0.7176 | 0.0258 | 0.1782 | **0.04x** | **0.14x** | Unboxed `double` fields & loops (Sections 0C, 0D, 5) |
| | `float_nbody` | 0.7330 | 0.0269 | 0.1787 | **0.04x** | **0.15x** | Inferred float fields & private leaf boxes (Sections 0C, 5) |
| | `Richards` | 0.1705 | 0.0091 | 0.0177 | **0.05x** | **0.51x** | Fixed-slot layout access & method specialization (Sections 0C, 6J) |
| | `Richards_typed` | 0.1711 | 0.0102 | 0.0178 | **0.06x** | **0.58x** | Declared class contracts & fixed-slot layout (Sections 0A, 6J) |
| | `kv_record_parse` | 4.3168 | 1.6010 | 2.6374 | **0.37x** | **0.61x** | IR1 slice-compare & `%parse_int_slice` fusion (Section 6C) |
| | `matrix_multiply` | 5.2116 | 0.1788 | 0.2709 | **0.03x** | **0.66x** | Nested list contracts, LICM, borrowed row locals (Sections 0A, 0D, 4) |
| | `word_frequency` | 4.1490 | 2.9011 | 3.2838 | **0.70x** | **0.88x** | Typed `_Table` slots & fixnum hashes (Sections 0B, 6J); further sped up in `9a8b4f8` |
| **2. Near SBCL parity & 4–25x faster than CPython (7)** | `numeric_sieve` | 0.0725 | 0.0029 | 0.0028 | **0.04x** | **1.03x** | Singleton `[1] * n` fill & unboxed fixnum loops (Sections 5, 6J) |
| | `numeric_sieve_typed` | 0.0724 | 0.0033 | 0.0028 | **0.05x** | **1.19x** | Checked `int64` loop & list indexing |
| | `interval_merging` | 0.0497 | 0.0058 | 0.0048 | **0.12x** | **1.21x** | Exact-tuple inline indexing & borrowed unpack (Section 0D) |
| | `json_tokenizer` | 0.0680 | 0.0088 | 0.0070 | **0.13x** | **1.27x** | Remaining gap: IR2 codepoint peephole vs IR1 transform (Section 6D) |
| | `topological_schedule` | 0.0258 | 0.0042 | 0.0022 | **0.16x** | **1.94x** | Remaining gap: `list.pop()` specialization & inner list iteration (Section 1C) |
| | `text_pipeline_typed` | 0.0726 | 0.0169 | 0.0076 | **0.23x** | **2.21x** | Remaining gap: text content domain wiring & string method transforms (Sections 6D, 6I, 6P) |
| | `text_pipeline` | 0.0736 | 0.0176 | 0.0076 | **0.24x** | **2.31x** | Same as `text_pipeline_typed` |
| **3. Faster than CPython, trailing SBCL 5–12x (5)** | `bytes_codec` | 0.1032 | 0.0216 | 0.0027 | **0.21x** | **7.89x** | `bytearray` generic iterator protocol & C-ABI indexing calls (Sections 1C.5–6, 6P) |
| | `closure_pipeline` | 4.1499 | 1.6020 | 0.2638 | **0.39x** | **6.07x** | Boxed heap `Cell` reads/writes for non-escaping mutable `nonlocal` fixnums (Section 6Q) |
| | `binary_trees` | 0.1029 | 0.0432 | 0.0074 | **0.42x** | **5.83x** | Recursive `Node` allocation & refcount traffic vs SBCL bump-pointer GC |
| | `dataclass_ast` | 0.0570 | 0.0257 | 0.0022 | **0.45x** | **11.67x** | `match` class-pattern `isinstance(x, (Cls,))` lacks branch narrowing in `constraint.py` (Section 6O) |
| | `exception_validation` | 4.3184 | 3.1574 | 0.6050 | **0.73x** | **5.22x** | Exception instance + `_args` tuple allocation & `super().__init__` dispatch on hot raise path (Section 6O) |
| **4. Slower than both CPython & SBCL (7)** | `set_operations` | 0.1184 | 1.4211 | 0.1975 | **12.00x** | **7.20x** | No `SetType` in `ctype.py`, 4-list `_Table` layout with unused `_values`, `try/except TypeError` in `_set_lookup`/`_set_remove`, `_table_live` list allocation (Section 6M) |
| | `generator_pipeline` | 0.1028 | 0.3134 | 0.0064 | **3.05x** | **49.23x** | Stackful coroutine switch + boxed locals per `yield`/`next` instead of IR1 generator fusion / state machine (Section 6N) |
| | `log_aggregation` | 0.0572 | 0.1389 | 0.0213 | **2.43x** | **6.53x** | Unspecialized `str.rsplit(..., 1)` + list allocation, `set.add`, and `dict` composite-key/nested updates (Sections 6M, 6P) |
| | `lru_simulation` | 4.0235 | 6.7929 | 1.7226 | **1.69x** | **3.94x** | Repeated `dict` probe loops (`in`, `[]`, `del`, `[]=`) & 4-list `_Table` indirection (Section 6M) |
| | `http_routing` | 0.0506 | 0.0707 | 0.0537 | **1.40x** | **1.32x** | Chained `str + str` allocations, `name.lower() == "..."` allocation, `int(value.strip())` (Section 6P) |
| | `expression_tree` | 4.3744 | 5.8504 | 1.5239 | **1.34x** | **3.84x** | Polymorphic `Expr.eval_node` dispatch & 3–4 `dict` probes per `LetExpr` node (Sections 6M, 6O) |
| | `graph_paths` | 0.0322 | 0.0427 | 0.0122 | **1.33x** | **3.51x** | `dict[int, list[tuple[int, int]]]` inner unpacking & heap rebuild loops (Sections 6K, 6M) |

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

### 0B. Typed `dict` / `set` storage layout and fixnum hash invariants (`stdlib/_dict.py`, `mule/runtime/classes.rs`, `locall.py`, `constraint.py`, `vops.py`) (implemented)

- **SBCL reference**: `src/code/target-hash-table.lisp` (`define-ht-getter`,
  `define-gethash/flat`, `puthash`, `clip-hash`) and `src/code/target-sxhash.lisp`
  (`sxhash`), where hash-table slots (`index-vector`, `hash-vector`, `table`,
  `next-vector`) are typed `defstruct` slots accessed at fixed offsets and table
  hashes are clipped to unboxed fixnums.
- **Previous Mule defect**:
  - In `stdlib/_dict.py`, `_table_lookup` and `_table_insert` are standalone
    functions taking untyped `(self, key)` shared by `dict`, `set`, and
    `frozenset`. They compiled with `self: UnknownType` and `variants=()`.
  - Every lookup or insertion executed 5+ dynamic attribute lookups
    for `self._indices`, `self._hashes`, `self._keys`, `self._values`, and
    `self._used`.
  - Because `self._indices` and `self._hashes` had `UnknownType`,
    `_storage_read(indices, slot)` returned `UnknownType`. Consequently,
    `_table_lookup` had inferred return type `UnknownType` (forcing `entry >= 0`
    in `dict_get` and `dict_getitem` to call generic comparison), and
    `_storage_read(hashes, entry) == code` called generic equality comparison.
  - `hash(key)` can return a 64-bit or arbitrary-precision integer outside
    Mule's 63-bit fixnum range (`[-2^62, 2^62 - 1]`), preventing `_hashes` from
    being proven a fixnum `ListType`.
- **Implemented architecture**:
  1. `_Table` defines `__mule_slots__ = ("_indices", "_hashes", "_keys", "_values", "_used")`
     at fixed `Instance.fields` slots `0..4`, and `create_class_with`
     (`mule/runtime/classes.rs`) preserves the solid base's slot prefix `0..4`
     across single and multiple inheritance. Section 6J replaces the `_Table`-
     specific compiler shortcuts with general layout facts and guarded fixed-index
     accesses at `INSTANCE_FIELDS_OFFSET` (`32`).
  2. `_table_hash_fallback` (`stdlib/_dict.py`) clips out-of-range fallback hashes
     into `[-2^62, 2^62 - 1]` (matching SBCL's `clip-hash`) while preserving
     `hash(key)`'s exact Python return value.
  3. `_Table._indices` and `_Table._hashes` are word vectors, fixed-length
     vectors of raw fixnum words, as SBCL's hash table declares its index and
     hash vectors `(simple-array hash-table-index (*))`
     (`src/code/hash-table.lisp:106-119`). Their element type comes from that
     representation, not from inference about the values written into them
     ([type-inference.md](type-inference.md) Section 3.6). The representation
     proves `_table_lookup -> IntegerType(-1, (1 << 60) - 1)`, unboxed
     `icmp eq i64` probe-loop hash comparisons, and unboxed `icmp sge i64`
     entry checks in `dict_get` and `dict_getitem`.

### 0C. Preserving `InstanceType(cls)` method specialization, instance field inference, and method call return contracts (`locall.py`, `constraint.py`, `return_checks.py`, `codegen.py`) (implemented)

- **SBCL reference**: `src/code/defstruct.lisp` (`slot-access-transform`),
  `src/compiler/locall.lisp`, and `src/compiler/ir1tran.lisp` (`assert-call-type`),
  where structure/instance accessors specialize on the receiver's class layout,
  propagate inferred slot types, and assert derived return types across known
  and local calls.
- **Mule implementation and Python adaptations**:
  1. `_SpecializesBody` (`mule/compiler/locall.py`) retains `"fixnum"` /
     `"observed"` method entry contexts when `candidate` narrows a `GETATTR` or
     `SETATTR` receiver to `InstanceType`, and drops `"observed"` when it adds
     no body, element-contract, or return-type specialization beyond `"fixnum"`.
  2. `ClassConstructorTarget.propagate_call` (`mule/compiler/node.py`) seeds
     `__init__` call sites into `method_calls` so `_SanitizeInstanceFieldType`
     and `_update_class_field_types` (`mule/compiler/locall.py`) infer scalar
     (`IntegerType`, `BooleanType`, `PythonIntegerType`, `TextType`),
     `InstanceType` / `UnionType`, and `DictType` instance field types from
     `__init__` and method stores, resetting stale return summaries when field
     types narrow.
  3. Because Python attributes can be rebound dynamically via `setattr`,
     `_propagate_getattr` (`mule/compiler/constraint.py`) guards scalar and
     `DictType` instance field reads with an `InferredReturnContract`.
  4. `assert_inferred_results` (`mule/compiler/return_checks.py`) asserts
     derived and declared return contracts across both monomorphic and
     polymorphic `CALL_RESOLVED` method calls on typed instance receivers, and
     `method_call_helper` / `call_resolved_target` (`mule/compiler/codegen.py`)
     dispatch directly to specialized `.spec.0` entries via `_entry_steps` and
     elide redundant return checks when the specialized entry's return type
     already satisfies the contract.

### 0D. Borrowed / unboxed container element reads and loop reference-count elimination (`represent.py`, `ir2tran.py`, `vops.py`) (implemented)

- **SBCL reference**: `src/compiler/array-tran.lisp` (`aref` ->
  `data-vector-ref` / `hairy-data-vector-ref/check-bounds`) and
  `src/compiler/represent.lisp`, where vector and sequence reads load element
  values into registers without reference-count traffic.
- **Mule implementation and SBCL adaptation**:
  - SBCL relies on tracing GC, so `data-vector-ref` loads tagged words directly
    from vector storage without per-read ownership operations. Mule uses
    reference-counted heap ownership with tagged 64-bit immediates (fixnums,
    booleans, `None`, immortal singletons).
  - `_specialize_list_getitems` (`mule/compiler/ir2tran.py`) and
    `protocol_getitem` / `protocol_setitem` / `_storage_helpers` / `unpack_ref`
    (`mule/compiler/vops.py`) adapt SBCL's direct vector access to Mule's
    ownership model in four complementary ways:
    1. **Fixnum and immortal element elision**: When the loaded element's
       derived `ValueType` is a proven fixnum or immortal (`is_fixnum` /
       `is_immortal`), `protocol-getitem` (`list_owned`, `tuple_owned`),
       `primitive-_storage_read`, and `unpack-ref` omit `@mule.fast.retain`
       and mark the result TN as borrowed (`borrowed_from = seq_tn`), because
       tagged immediate words never require reference-count updates.
    2. **Inline exact-tuple and exact-list physical reads**: `protocol-getitem`
       now lowers proven exact `tuple` indexing (`tuple_borrow`, `tuple_owned`)
       inline alongside exact `list` indexing (`list_borrow`, `list_owned`),
       using `TupleType.exact_length()` when statically known to check bounds
       without loading the header length field and falling back to
       `@mule_getitem_borrow` / `@mule_getitem` only on out-of-bounds indices.
    3. **Borrowed container-element locals and structural unpack across
       protected regions**: `_try_borrow_container_element_local` and
       `_try_borrow_unpacked_getitem` (`mule/compiler/ir2tran.py`) trace single-
       assignment local variables initialized from `protocol-getitem` or
       `primitive-_storage_read` (such as `row_a = a[i]` across an inner loop,
       or `m_start, m_end = merged[mid]` via `structural-unpack` /
       `unpack-ref`) across multi-block regions. When the backing container
       remains bound to the same local (`_same_local_origin`) and no intervening
       step along any CFG path to the reads can mutate that container or invoke
       arbitrary Python user code (`_safe_container_local_region_step`, using
       `ValueType.may_run_user_code` so stores of tuples/lists with inert
       elements do not act as false barriers), both the container read and the
       local variable borrow their reference from the container root without
       `@mule.fast.retain` or `@mule.fast.release`.
    4. **Unboxed float element reads and in-place list stores**: Exact-list
       indexing (`protocol-getitem`) with a `FIXNUM` index borrows the element
       (`list_borrow`) when feeding float coercions (`@mule.box.f64.load`) or
       pure float arithmetic, and `protocol-setitem` on exact lists accepts
       unboxed `DOUBLE` values directly (`represent.py`, `vops.py`) and updates
       an exclusively owned (`refs == 1`) `FloatStorage` slot in place before
       falling back to allocating a fresh box.

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

### 1C. Sequence, string, and container transforms (`seqtran.py` vs `seqtran.lisp`) (partially implemented)

- **SBCL reference**: `src/compiler/seqtran.lisp` (3,371 lines in SBCL vs
  `mule/compiler/seqtran.py` and `mule/compiler/containtran.py`).
- **Implemented**:
  1. **IR1 slice-comparison, integer-slice parsing, constant-slice folding,
     empty-container truthiness, and 0-arg `str.split()` loop fusion
     (`seqtran.py`, `srctran.py`)**:
     - `str[a:b] == target` (`%text_slice_compare` / `%text_slice2_compare`) and
       `int(str[a:b])` (`%parse_int_text` / `%parse_int_slice`) lower in IR1 via
       `@deftransform`s and `splice_fun_args` in `seqtran.py` and `srctran.py`,
       with multi-use local slice forwarding in `escape.py` and `scalar.py`.
     - Constant `%getslice` on constant `str`, `bytes`, and `tuple` folds in IR1,
       and truthiness tests on proven empty or non-empty containers fold in
       `seqtran.py`.
     - `for tok in s.split():` on proven `TEXT_TYPE` with 0 arguments lowers in
       IR1 (`seqtran.py`, commit `9a8b4f8`) to a counted index loop using
       `_text_split_whitespace_next` and `_text_slice` without allocating an
       intermediate `list[str]`.
  2. **`bytes` and `bytearray` type representation, element-type derivation,
     and physical indexing/mutation VOPs (`array-tran.lisp`)**:
     - `BytesType` and `ByteArrayType` (`ctype.py`) implement `dispatch_domain()`
       returning `BYTES_TYPE` and `BYTEARRAY_TYPE` (unless `may_be_unbound`), and
       `locall.py` includes `BYTES_TYPE` and `BYTEARRAY_TYPE` when seeding
       `observed` entry contexts, preserving `IntegerType(0, 255)` `getitem_type`
       and `iterate_type`.
     - `seqtran.py` lowers `len(bytearray)` to `_storage_length`
       (`visit_ByteArrayType`), and `vops.py` / `buffers.rs` / `sequences.rs`
       specialize `getitem(BYTES_TYPE | BYTEARRAY_TYPE, FIXNUM)`
       (`@mule_bytes_getitem`, `@mule_bytearray_getitem_fast`),
       `setitem(BYTEARRAY_TYPE, FIXNUM, FIXNUM)` (`@mule_bytearray_setitem_fast`),
       and `bytearray.append` (`@mule_bytearray_append_fast`).
  3. **IR1 sequence iteration specialization for `list`, `tuple`, `str`, `bytes`,
     and `dict.items()` (`seqtran.py`, `ir1tran_loops.py`)**:
     - `seqtran.py` transforms `%sequence_available` iteration over proven exact
       `list`, `tuple`, `str`, `bytes`, and `dict.items()` into counted/indexed
       loops re-reading mutable list length on each step.
- **Remaining gaps and proposed transforms**:
  1. **IR1 character-comparison and text-method transforms**: Remaining IR2
     pattern matchers for `text[i] == "c"` (`_fuse_text_codepoint_comparisons`)
     and `text.find(...)` (`convert_call_resolved`) should move to IR1
     `@deftransform`s (see Sections 6D and 6I).
  2. **Pure comprehension and generator fusion**: Lower `sum([...])`,
     `any([...])`, `all([...])`, `min([...])`, `max([...])`, and
     `list(x for x in seq)` / generator pipelines directly into counted or
     iterator-driven accumulation loops in IR1 without allocating an intermediate
     list or stackful coroutine (as SBCL's `map` / `reduce` / `every` / `some`
     transforms do in `seqtran.lisp`; see Section 6N).
  3. **Constant sequence operations and string joins**: Fold `%getitem` on
     constant `str`, `bytes`, and `tuple` with constant index; fold `%add` /
     `%multiply` on small constant sequences within size limits; specialize
     `"".join(...)` on constant or known-length tuple/list displays and chained
     `str + str + ...` into a single pre-sized `%format` text build (see Section 6P).
  4. **Known container methods**: Move `dict.get(k, default)`, `list.pop()`,
     `str.startswith(prefix)`, `str.endswith(suffix)`, `str.rsplit(sep, 1)`,
     `str.lower()`, `str.strip()`, `set.add()`, and `set.discard()` on proven
     receiver types to IR1 `@deftransform`s in `seqtran.py` (see Sections 6I, 6M,
     and 6P).
  5. **Counted iteration for `bytearray`, `set`, `frozenset`, and aliased
     `range` (`seqtran.lisp`)**:
     - **SBCL reference**: `build-sequence-iterator` in `src/compiler/seqtran.lisp`.
     - **Current Mule defect (observed in `perf/suite/bytes_codec.py` and
       `perf/suite/set_operations.py`)**:
       - `for b in decoded:` (with `decoded: bytearray`) and `for x in s:` (with
         `s: set`) still fall back to the generic iterator protocol
         (`@mule.fast.iterate.move` + `@mule.fast.next_item` + `StopIteration`),
         because `seqtran.py` does not yet include `ByteArrayType` or `SetType`
         in `%sequence_available` counted loop lowering.
     - **Required implementation**:
       - Extend `seqtran.py` counted-loop lowering to `ByteArrayType` (re-reading
         current length and byte buffer on each step to preserve mutation and
         reallocation semantics), `SetType` / `FrozenSetType` (scanning `_keys`
         while skipping `_TOMBSTONE`), and `RangeType` values (Section 6H), while
         preserving `break`, `continue`, cleanup, and `for`-`else` semantics.

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
  2. **Bitwise shift interval derivation, 2-argument `min`/`max`, `math.sqrt`,
     and commutative/associative constant folding (`ctype.py`, `fndb.py`,
     `ir1opt.py`, `srctran.py`, `modarith.py`, `constraint.py`, `represent.py`,
     `vops.py`)**:
     - `IntegerType.binary` (`ctype.py`) derives exact interval bounds for
       `<<` (with shift count in `[0, 61]`) and `>>` (with non-negative shift
       count), keeping `(lo + hi) >> 1` and `(x << k)` within `FIXNUM` bounds.
     - `_canonicalize_commutative_constant` (`ir1opt.py`) normalizes
       `const + x -> x + const`, `const & x -> x & const`, and
       `const == x -> x == const` on proven commutative domains, and
       `modarith.py` reassociates and folds `(x + c1) + c2` / `(x & m1) & m2`
       via `splice_fun_args` (Section 6C).
     - `srctran.py` lowers 2-argument `min(a, b)` and `max(a, b)` to `%min2` /
       `%max2` (`fndb.MIN2`, `fndb.MAX2`), with interval bound derivation in
       `fndb.py`/`constraint.py` and inline `icmp`/`fcmp` + `select` VOPs
       (`raw-min2`, `raw-max2`, `raw-float-min2`, `raw-float-max2`, `min2`,
       `max2` in `vops.py`) preserving Python's first-operand tie-breaking rule.
     - `srctran.py` lowers 1-argument `math.sqrt(x)` to `%math_sqrt`
       (`fndb.MATH_SQRT`), which lowers to inline `fcmp olt double %x, 0.0`
       domain guard + `call double @llvm.sqrt.f64(double %x)` (`raw-math-sqrt`,
       `boxed-float-math-sqrt`, `math-sqrt` in `vops.py`).
- **Remaining gaps and proposed transforms**:
  - **Large integer constant folding**: `ir1opt.py` rejects folded integer
    results larger than 128 bits, including unary negation. This is a fixed
    literal-size limit, not a measured workload regression. SBCL's folding
    dispatch in `src/compiler/ir1opt.lisp` uses function attributes and constant
    arguments. Revisit Mule's limit with an explicit compile-time resource policy
    and verify the generated code and compilation cost.
  1. **Modular arithmetic cutting (`modarith.lisp`)**: Propagate modular width
     demand backward through `(a + b) & mask`, `(a - b) & mask`, and
     `(a * b) & mask` when `mask = (1 << k) - 1` fits in `int64`, allowing
     intermediate operations to use wrapping 64-bit arithmetic without bigint
     overflow allocation.
  2. **Additional float and math transforms (`float-tran.lisp`)**: Add `fndb.py`
     and `srctran.py` transforms for builtin `abs(x)` and `float(x)` on proven
     `INTEGER` and `FLOAT` operands, plus remaining `math` module intrinsics
     (`math.floor`, `math.ceil`, `math.fabs`, `math.isinf`, `math.isnan`; see
     Section 6I).

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

## 3. Escape analysis and allocation elimination (`envanal.lisp` / `ltn.lisp`) (implemented)

- **SBCL reference**: `src/compiler/envanal.lisp` and `dynamic-extent` handling
  in `src/compiler/ltn.lisp` and `src/compiler/ir2tran.lisp`.
- **Implemented**:
  - `escape.py` extends the existing variable/parameter escape solver with
    allocation-result and alias facts, implicit environment containment,
    keyword and default-parameter call mapping (`call_order`), cross-unit
    `ForeignFunction.borrowed_parameters` summaries in `BuiltinsInterface` and
    `ModuleInterface`, and non-escaping consumer classifications for builtin
    container truthiness, disjoint identity comparisons, reflexive containment,
    non-user-code value comparisons and `math.sqrt`, exact builtin subscript
    keys, and exact builtin `str`/`list`/`bytearray`/`dict` methods.
  - Stable direct-only local functions use a compiler-owned environment ABI.
    Mutable captures reference home-frame slots when every retaining closure
    has local extent. Escaping siblings keep the authoritative heap cell.
    Function objects, closure tuples, and unobservable annotation companions
    disappear on eligible paths. Recursive calls, zero-argument functions,
    aliases, native scalar entries, and existing unpacked returns retain their
    respective semantics.
  - `scalar.py` consumes allocation escape proofs for fixed lists and tuples,
    including interprocedural non-escaping calls (positional, keyword, and
    defaulted) to local and proven guarded functions. Constant-index reads,
    bounded runtime-index reads, fixed-index list writes, length, truthiness
    (`unary-not` and `branch`), disjoint-type identity comparisons (`is` / `is
    not`), reflexive-equality containment (`in` / `not in` via
    `scalar-contains`), structural unpack, and aliases become scalar operations
    within a block or across multiple blocks (including multiple aliases,
    conditional expression temporaries, and non-inert child teardown across
    branches when forward dataflow proves consistent live owners and a single
    cleanup anchor), and scalar child slots participate in unboxed
    representation selection (`DOUBLE_REG`) and frame slot packing. Normal and
    error cleanup preserve child ownership and reverse destruction order.
- **Conservative allocation boundaries**: Observable functions and cells,
  defaults/decorators, suspended owning activations, unknown uses, resizing,
  overlapping aggregate lifetimes, pending element checks, and unrepresentable
  cleanup order retain ordinary allocations. This is not arbitrary heap-object
  scalar replacement or stack allocation of materialized Python containers.
  Existing tuple projection and unpacked-return mechanisms remain in use. The
  precise [local allocation extent contract](native-lowering.md#local-allocation-extent)
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
     - `_Propagation` in `constraint.py` maintains a sparse relational
       difference-bound domain (`x - y <= c`, following
       `equality-constraints.lisp`) across linear updates (`x = x + d`,
       `x = y + d`) and ordered comparisons (`<`, `<=`, `>`, `>=`, `==`, `!=`
       on `u + c0` vs `v + c1`), consulting incoming interval bounds on demand
       only for active relational pairs at control-flow joins (`_state_diff_bound`)
       and propagating relational bounds into variable intervals, `u - v`
       subtractions (`n - i >= 1` inside `while i < n:`), comparison folding,
       and loop-exit equality (`i == n` after `while i < n: i += 1`).
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
  Ordinary float and flow-proven fixnum locals can also remain raw across
  blocks. Explicit coercion VOPs connect independently selected representations.
  Float selection additionally preserves observable object identity.

### Flow-proven fixnum locals (implemented)

`represent.py` preserves the `FIXNUM` primitive type when the complete set of
local writers proves exact integers within `[FIXNUM_MIN, FIXNUM_MAX]`.
Booleans, possibly unbound values, and wider or heterogeneous integer joins
do not establish that proof. The compiler selects `SIGNED` (`i64`) or tagged
`IMMEDIATE` (`ptr`) storage per TN from operand and copy costs, independently
of a whole-function `NativePlan`. Local slots retain their selected LLVM type
across basic-block boundaries.

This follows SBCL `select-tn-representation`, `coerce-vop-operands`, and
`emit-moves-and-coercions`. Generic copies do not impose an initial descriptor
bias. After selection, explicit coercion VOPs perform arithmetic right shift
to decode a tagged fixnum or shift/tag to restore its object representation.
These conversions allocate no heap object and require no runtime dispatch.
Local reads and assignment moves retain their original binding and overwrite
metadata. The operand index is rebuilt before ownership and cleanup analysis.

Ordinary-entry arithmetic templates admit signed operands only with the
required exact integer proofs. Arithmetic whose result may exceed the fixnum
range retains the existing arbitrary-precision behavior. Generic calls,
container storage, returns, and other object consumers receive tagged pointers.
Locals requiring unbound or deletion checks and address-exposed closure slots
retain tagged storage. Generator and coroutine local slots also remain tagged:
frame inspection can observe a binding before its first assignment, even without
an explicit source read. Their arithmetic temporaries may still use signed
storage. Raw suspended locals need an independent binding-state proof or
representation before that restriction can be removed.

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

`select_tn_representation` charges one unit for each operand restriction
mismatch, and copy refinement charges one unit for differing endpoint LLVM
types. SBCL's `add-representation-costs` uses storage-class-specific operand
and move costs. Mule's uniform costs can select more expensive conversions, but
a resulting performance regression has not been measured.

The list-setitem identity exemption in `represent.py` permits indexing and
iteration of a local exact list without examining later uses of retrieved values.
Its identity-comparison scan covers only the current component. A raw float
stored through that path can require a new box at the descriptor boundary, so
identity observed through a retrieved value needs investigation. Probes so far
returned the expected result and did not establish that the exemption was
reached; this is an unverified correctness risk, not a confirmed miscompile.
Trace generated IR and test the actual exemption before changing the legality
rule. The general identity guarantee above remains the required contract.

Generalizing explicit coercion insertion beyond fixnums and floats, extending
identity-safe materialization, and using target-specific conversion costs remain
proposed work. Whole-native entry conventions retain their implemented
generators. These increments do not establish complete SBCL
representation-selection parity.

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

---

## 6. Replacing benchmark-specific special cases and closing remaining SBCL parity gaps (partially implemented)

An audit of `mule/compiler/` and `stdlib/` against `perf/suite/*.py` and
`~/checkout/sbcl/src/` identified both compiler/stdlib paths that relied on
syntactic heuristics (Sections 6A–6L, seven of which are now implemented: 6A,
6B, 6C, 6E, 6G, 6J, 6L) and five remaining architectural gaps explaining the
24-workload performance deltas against CPython and SBCL (Sections 6M–6Q). Each
item below states the current behavior, the corresponding SBCL compiler or
runtime mechanism in `~/checkout/sbcl/src/`, and the required implementation.

### 6A. Module-level constant folding via write-set tracking (`ir1tran.lisp` / `globaldb.lisp`) — **implemented**

- **Status**: **Implemented**.
  - Removed all `startswith("_")` and `.isupper()` identifier-spelling checks
    from `_GlobalConstantStores` and cross-module constant import propagation in
    `mule/compiler/locall.py`, and from `_GlobalConstantScan` in
    `mule/compiler/main.py`.
  - Tracked module-level `GlobalVar.sets` alongside dominance/post-dominance
    (`_unconditional_toplevel_stores` in `mule/compiler/locall.py`) and
    module-namespace dynamic mutation flags (`Component.mutates_module_namespace`
    in `mule/compiler/node.py` / `ir1tran.py` and `has_module_namespace_mutations`
    in `mule/compiler/locall.py` covering `from ... import *`, mutating uses of
    `globals()`, `exec`/`eval`, and `setattr`/`delattr`/`.attr = ...` on
    `sys.modules[...]`).
  - Recognized single unconditional top-level assignments to immutable constants
    (including through `Cast` nodes), frozen container initializers
    (`frozenset(...)`, `tuple(...)`, `bytes(...)`), and explicit `Final` /
    `Final[T]` / `typing.Final` annotations, and exported proven constant
    bindings in module summaries for cross-module imports without treating
    imported function bindings as value constants.
  - Preserved exact constant types (`IntegerType(c, c)`, `BooleanType(b, b)`,
    `TEXT_TYPE`, `TupleType` with exact element types) on `Final`-annotated
    top-level bindings (`locall.py`) and folded proven intra-module and
    cross-module `Final` global references directly to `Constant` leaves in IR1
    (`_GlobalConstantScan` / `_fold_protocol_constants` in `main.py`, matching
    SBCL's `defconstant` reference conversion in `src/compiler/ir1tran.lisp:355`),
    while widening unannotated module globals via `_widen_user_constant_type` so
    external `module.attr = ...` rebinding remains valid.
  - Implemented `fold-call-derived-to-constant` (`_fold_call_derived_to_constant`
    in `ir1opt.py`, matching `src/compiler/ir1opt.lisp:1550, 2450`) across all
    flushable known calls and exact container/string length derivation on
    `_storage_length` and `_text_length` (`_derive_storage_length`,
    `_derive_text_length`, and `"flushable"` protocol query attributes in
    `protocols.py`, matching `vector-length-type` in
    `src/compiler/array-tran.lisp:2233-2257`), folding `len` on constant/global
    sequences and flow-narrowed locals to `Constant` leaves in IR1 and flushing
    dead `Ref` operands.
  - Extended IR1 constant folding (`_constant_fold`, `_constant_fold_unary` in
    `ir1opt.py`, matching `%constant-fold-call` in
    `src/compiler/ir1opt.lisp:2289-2295`), integer interval comparison folding
    (`_integer_interval_comparison` in `typetran.py`), and integer power-of-two
    strength reduction (`_RewriteIntConstant.visit_GlobalVar` in `srctran.py`)
    to fold flushable pure bound references with singleton `derived_type`
    alongside syntactic `Constant` leaves, and widened non-integer `min`/`max`
    derivation (`derive_min2_types` / `derive_max2_types` in `fndb.py`) so
    unordered singleton types do not claim a foldable constant result.

### 6B. Call-site argument propagation (`propagate-to-args`) vs. unannotated parameter guessing (completed)

- **Implemented architecture**:
  - `CallSite` (`mule/compiler/constraint.py`) records call-site keyword names and
    maps positional, keyword, and default arguments onto callee parameter order via
    `target.call_order` and `target.default_types` (`CallSite.bind_argument_facts`),
    covering direct, guarded, and predicted method calls.
  - `locall.analyze` (`mule/compiler/locall.py`) propagates intra-module call-site
    argument unions (`propagate-to-args`) across multi-hop call chains and
    recursive/mutually recursive call cycles into `"general"` entries for closed
    local functions (`_collect_closed_lambdas`, including functions with keyword
    and default arguments) and `"observed"` specialized entries for module-level
    and escaping functions, using per-parameter monotonic seeding so unseeded
    recursive callers do not poison concrete caller argument unions to `UNKNOWN`.
  - Receiver typing uses the structural class ownership recorded on `Lambda` during
    IR1 conversion (`lambda_.method_class` in `mule/compiler/ir1tran_lambda.py` and
    `Lambda.protocol_self_type` in `mule/compiler/node.py`), typing method parameter
    0 to `InstanceType(method_class)` regardless of parameter spelling (`self`,
    `this`, `recv`, etc.) while keeping non-method functions whose first parameter
    is named `self` unconfused with methods.
  - `_ParameterUse` (`mule/compiler/locall.py`) no longer guesses `LIST_TYPE` from
    unannotated `len(x)` + `x[i]` body syntax or `TextStorage`/`ListStorage` from
    method names; parameter specialization is driven by declared annotations,
    call-site argument propagation (`propagate-to-args`), and numeric parameter
    speculation.

### 6C. IR1 combination matching (`cmatch` / `splice-fun-args`) to replace physical IR1 adjacency and IR2 slice peepholes (completed)

- **Implemented architecture**:
  - `ok_lvar_lambda_var`, `almost_immediately_used_p`, `lvar_matches`, and
    `splice_fun_args` in `mule/compiler/ir1opt.py` match producer
    `Combination`s through `Lvar.uses` and single-assignment local `LambdaVar`
    references across intervening flushable or effectless nodes, following
    SBCL's `lvar-matches` (`src/compiler/combination-match.lisp`) and
    `almost-immediately-used-p` / `splice-fun-args` (`src/compiler/ir1util.lisp`).
  - `_canonicalize_commutative_constant` in `mule/compiler/ir1opt.py`
    canonicalizes commutative constants (`const + x -> x + const`,
    `const & x -> x & const`, `const == x -> x == const`), and IR1 transforms
    in `modarith.py`, `containtran.py`, `srctran.py`, `typetran.py`, and
    `seqtran.py` match through `splice_fun_args` and `almost_immediately_used_p`
    rather than requiring physical `inner_node.next is fun_ref.prev` adjacency.
  - `str[a:b] == target` (`%text_slice_compare` / `%text_slice2_compare`) and
    `int(str[a:b])` (`%parse_int_text` / `%parse_int_slice`) are IR1
    `@deftransform`s in `mule/compiler/seqtran.py` and `mule/compiler/srctran.py`
    using `splice_fun_args` on `%getslice`; `escape.py` and `scalar.py` forward
    `(seq, start, stop)` across non-escaping multi-use local slice bindings;
    and `_fuse_text_slice_compares` and `note_int_parse` have been removed from
    `mule/compiler/ir2tran.py`.

### 6D. Single-character string comparison transforms, codepoint representations, and bounded `TextContent` wiring (`seqtran.lisp` / `represent.lisp` / `type.lisp`)

- **Current special case and partial progress**:
  - `_fuse_text_codepoint_comparisons` (`mule/compiler/ir2tran.py:556-697`)
    post-processes IR2 VOPs to rewrite `protocol-getitem` into
    `@mule_text_codepoint_getitem` and `TextLiteral` into `IntegerLiteral` only
    when 100% of uses of `ch = text[i]` are comparisons with 1-character string
    literals.
  - `mule/compiler/text_domain.py` (`TextContent`, implemented in commit
    `b693445`) defines the bounded text content domain (`min_length`,
    `max_length`, and 5-character `prefix` / `suffix` with union, intersection,
    containment, widening, and serialization), but `TextType` in
    `mule/compiler/ctype.py` still carries only `literal: str | None = None` and
    `interned: bool = False` and does not yet wire `TextContent` into
    `TextType`, `constraint.py`, `fndb.py`, or `seqtran.py`.
- **SBCL reference**: `select-tn-representation` and `emit-moves-and-coercions`
  in `src/compiler/represent.lisp` (`CHARACTER-REG` vs `CHARACTER-DESCRIPTOR`),
  `deftransform string=*` in `src/compiler/seqtran.lisp`, and array/string shape
  subtyping/intersection in `src/code/type.lisp`.
- **Required implementation**:
  1. Add IR1 `@deftransform`s in `mule/compiler/seqtran.py` on
     `%compare_equal`, `%compare_not_equal`, `<`, `<=`, `>`, and `>=` when one
     operand is a 1-character `str` constant and the other is `TEXT_TYPE`. When
     the `TEXT_TYPE` operand matches `text[i]` via `lvar_matches` (directly or
     through a local variable `ch = text[i]`), lower the comparison to an
     unboxed codepoint comparison (`_text_codepoint_at`).
  2. When all uses of `ch = text[i]` are rewritten to codepoint consumers,
     standard IR1 dead-code flushing (`flush-dead-code` in `ir1opt.py`) removes
     the 1-character string allocation; when `ch` also escapes on a cold path
     (such as `out.append(ch)`), hot comparisons still compare unboxed
     codepoints.
  3. Delete `_fuse_text_codepoint_comparisons` from `ir2tran.py`.
  4. Wire `TextContent` (`mule/compiler/text_domain.py`) into `TextType`
     (`mule/compiler/ctype.py`), `constraint.py` (`_text_length`, `%getslice`,
     `%add`, `startswith`, `endswith`), and `typetran.py` / `seqtran.py` so
     proven length bounds (`min_length == max_length == 1` on `text[i]`) and
     prefix/suffix guarantees drive IR1 specialization directly.

### 6E. Symbolic sequence-length constraints and dead-allocation flushing (`equality-constraints.lisp` / `ir1opt.lisp`) (completed)

- **Implemented architecture**:
  - `_transform_known_length_primitive` in `mule/compiler/seqtran.py` transforms
    `_text_length(x)` and `_storage_length(x)` when `x` matches `%add`
    (`str + str`, `bytes + bytes`, `bytearray + bytearray`, `list + list`,
    `tuple + tuple`), `_text_slice`, `%getslice`, `%_list_fill` (`[elem] * n`),
    or `%multiply` (`seq * n`) via `lvar_matches` (looking through satisfied
    `Cast`s and local `LambdaVar` aliases), rewriting `len(x)` to arithmetic on
    operand lengths regardless of whether `x` carries a local type annotation
    or has other uses.
  - `FunInfo.flushable_test` (`mule/compiler/knownfun.py` and
    `mule/compiler/fndb.py`), `flushable_combination_p`, and
    `_FlushDeadReference` (`mule/compiler/ir1util.py` and
    `mule/compiler/ir1opt.py`) mark pure sequence allocations and numeric
    operations on proven operand types as flushable and flush dead `Set` and
    satisfied `Cast` nodes when a local `LambdaVar` loses its last reference
    (`_ReleasedReference.visit_LambdaVar`), following SBCL's
    `flushable-combination-p`, `delete-lambda-var`, and `flush-dead-code`.
  - `mule/compiler/constraint.py` and `mule/compiler/protocols.py` track
    symbolic sequence-length variables (`(_SEQ_LEN, var)`) and propagate
    sequence-length intervals and relational difference bounds across
    concatenation, slicing, list fill, and repetition (`_SequenceProducerLength`
    and `_LinearUpdate`), while invalidating mutable container lengths on
    container mutation or escape.
  - `_fuse_concat_length` and `_match_concat_local_store` have been deleted from
    `mule/compiler/seqtran.py`.

### 6F. Per-value identity escape for float representation selection (`represent.lisp`)

- **Current special case**: `_select_float_representations`
  (`mule/compiler/represent.py:173-237`) forces any TN feeding a `DESCRIPTOR`
  operand to be boxed at its producer unless `_is_non_identity_list_setitem`
  (`represent.py:240-308`) holds, which requires zero `compare-is` /
  `compare-is_not` VOPs in the entire function (`has_identity_compare`) and
  restricts `seq_local` to a non-parameter local whose loads feed only 6
  whitelisted VOPs.
- **SBCL reference**: `select-tn-representation`, `add-representation-costs`,
  and `emit-moves-and-coercions` in `src/compiler/represent.lisp`.
- **Required implementation**:
  1. Track per-value identity escape in `escape.py`: fresh float arithmetic
     results have no pre-existing object identity prior to boxing, and loaded
     float objects only demand descriptor preservation when that value's copy
     group can reach an identity-observing operation (`is`, `is not`, `id`).
  2. Allow `protocol-setitem` on proven `exact_list` (whether local, parameter,
     or attribute) to accept `StorageClass.DOUBLE` directly, and delete
     `_is_non_identity_list_setitem` and the whole-function
     `has_identity_compare` kill switch from `represent.py`.

### 6G. IR1 tuple destructuring, multi-value returns, and per-store liveness (`ltn.lisp` / `ir2tran.lisp` / `life.lisp`) (implemented)

- **SBCL reference**: `ir1-convert-mv-bind` (`src/compiler/ir1tran.lisp`),
  `ltn-analyze-mv-bind` and `ltn-analyze-return` (`src/compiler/ltn.lisp`),
  `ir2-convert-mv-bind` and `ir2-convert-return` (`src/compiler/ir2tran.lisp`),
  and backward liveness in `src/compiler/life.lisp`.
- **Implemented stage ownership**:
  - `_transform_unpack` in `seqtran.py` follows the capture temporary's references
    and their `Lvar` destinations to find `%unpack_ref` projections. Display
    elements keep their existing value temporaries. The transform does not inspect
    assignment targets, so globals, attributes, subscripts, nested targets, and
    target-side effects keep their original assignment order.
  - `callconv.py` selects a `TupleReturnConvention` from the proven `TupleType`
    facts of every `Return.result` producer. Equal fixed arities from 1 through 8
    qualify, including returns through local aliases and branch joins. Adjacent
    tuple construction permits direct output without allocation; other eligible
    returns extract owned children at the return boundary. Ordinary object entries
    retain tuple identity and their object convention.
  - `ir2tran.select_tuple_delivery` follows definitions and uses to connect calls,
    unpack captures, projections, and result assertions. `IR2Component` owns this
    selection. Code generation consumes it rather than scanning fixed instruction
    offsets. `_find_unpacked_calls` and the partition separator special case are
    removed. Shared output slots require nonoverlapping, single-block delivery
    regions; unsupported or overlapping regions keep ordinary boxed unpacking.
  - `_mark_last_local_reads` computes backward liveness at each instruction,
    including exception successors. It marks dead local assignments independently
    of other writes. Captures, borrowed roots, and inspectable suspended locals
    retain their required bindings. A separate destruction-safety proof excludes
    classes, functions, iterators, unknown values, and mutable object-reference
    containers. Exact bytearrays remain inert because their elements are bytes. Dead
    inert assignments release the previous binding at the original assignment
    point. Their input may borrow from a stable owner instead of retaining a copy.
- **Python ownership adaptation**: SBCL uses tracing collection. Mule's output
  slots own children until projections transfer them. Failure edges release only
  unconsumed slots, including across intervening target effects. Independently
  live tuple aliases retain their own children. Type purity for operations does
  not establish destruction safety: class objects can expose weak-reference
  callbacks, and mutable containers can acquire observable children later.
  Focused native checks use weak-reference callbacks rather than `__del__`, whose
  dispatch remains an existing runtime gap. Exception traceback retention remains
  outside this change's implemented ownership contract.

### 6H. First-class `RangeType` and IR1 range-loop lowering (`seqtran.lisp`)

- **Current special case**: `ir1tran.py:4838-4888` only recognizes
  `for i in range(...)` when `range` is a syntactic call in the `for` header,
  and `ir2tran.py:note_range_prepare` (lines 3230-3311) matches a rigid 9-node
  IR2 sequence in a single block when bounds are untyped.
- **SBCL reference**: `build-sequence-iterator` and `dosequence` transforms in
  `src/compiler/seqtran.lisp`.
- **Required implementation**:
  1. Add `RangeType(start_type, stop_type, step_type)` to `ctype.py` and
     register `range` in `fndb.py`.
  2. Transform iteration over any `Lvar` of `RangeType` (including aliased
     `r = range(n); for i in r:` and untyped `range(n)`) in IR1 `seqtran.py`,
     emitting `@mule_range_loop_prepare` or direct integer setup in IR1 before
     the loop header so the loop induction variable has `INT_TYPE` during IR1
     constraint propagation, LICM, and representation selection.
  3. Delete `note_range_prepare` from `ir2tran.py`.

### 6I. Known-function database (`fndb.lisp`) and IR1 method resolution vs. hardcoded Rust/math bypasses

Section 6J adds declared argument domains for existing compiled protocol entries
and preserves their owner layouts across unit interfaces. It does not replace
the method and math selection mechanisms below.

- **Current special case**: `ir2tran.py:382-439, 3747-3922` hardcodes 6
  `exact_text_*` / `exact_bytearray_append` methods to Rust helpers in
  `mule/runtime/sequences.rs` (leaving `rsplit`, `strip`, `lower`, `set.add`,
  and `set.discard` on generic paths, although 0-arg `for tok in s.split():`
  now has an IR1 counted token-loop transform in `seqtran.py` from commit
  `9a8b4f8`), and `srctran.py:_try_replace_math_sqrt` (lines 721-813)
  pattern-matches `CALL_RESOLVED` for `math.sqrt` only.
- **SBCL reference**: `defknown` in `src/compiler/fndb.lisp`,
  `src/compiler/float-tran.lisp`, and `ir1-optimize-combination` in
  `src/compiler/ir1opt.lisp`.
- **Required implementation**:
  1. Register `math` module functions (`sqrt`, `sin`, `cos`, `tan`, `fabs`,
     `floor`, `ceil`, `trunc`, `exp`, `log`, `log2`, `log10`, `copysign`,
     `isqrt`) in `fndb.py` so both `math.op(x)` and `from math import op; op(x)`
     resolve to `KnownFunction` leaves in IR1 and lower via `@deftransform` in
     `srctran.py`.
  2. Transform `%resolve_method` + `%call_resolved` on proven builtin receiver
     types (`TEXT_TYPE`, `BYTES_TYPE`, `BYTEARRAY_TYPE`, `LIST_TYPE`,
     `DICT_TYPE`, `SET_TYPE`) in IR1 `seqtran.py` into direct calls to compiled
     Mule Python protocol functions in `sequence_protocol.py` and
     `stdlib/_dict.py` backed by low-level Rust byte-search/memory primitives.

### 6J. General fixed-slot layout access (`slot-access-transform`) and sequence fill (`vector-fill/t`)

Status: implemented, with conservative layout and borrowing boundaries below.

- **Replaced special cases**: `_TABLE_SLOTS` and `_TABLE_FIELD_NAMES` no longer
  select physical accesses, field types, or borrowing by attribute spelling.
  `_empty_indices` now uses `return [-1] * size`, without size-specific literals.
- **SBCL reference**: `slot-access-transform` in `src/code/defstruct.lisp`
  (`%instance-ref` / `%instance-set`) and `fill-transform` /
  `make-array :initial-element` -> `vector-fill/t` in
  `src/compiler/seqtran.lisp`.
- **Implemented layout access**:
  - `ClassFacts` and `InstanceType` carry ordered physical field metadata from
    `MAKE_CLASS`, including literal `__slots__` / `__mule_slots__` declarations
    and constructor-proven fields. Known single inheritance preserves the base
    prefix. Unknown or multiple-base layouts retain ordinary dynamic access.
  - Attribute VOPs use constant-index loads and stores at
    `INSTANCE_FIELDS_OFFSET` only after the existing cache validates the plain
    instance layout, mutation epoch, and complete FIELD/index encoding. This
    preserves descriptor replacement, custom attribute hooks, and unset-slot
    errors. A predicted index never authorizes an unguarded storage access.
  - Class constructor targets export an independent implementation layout,
    without changing their semantic result type. Imported field predictions use
    that actual owner identity. Known-function argument domains request separate
    descriptor entries for existing exact-receiver protocols, exported as
    `FUNCTION.protocol`; generic bodies retain unknown receiver behavior.
    This transfers the proof through the actual dictionary get/set probe loops,
    not merely through an unused specialized table helper.
  - IR2 borrows an eligible temporary slot read through one same-block consumer
    when intervening operations cannot replace the slot or invoke user code.
    An explicit nullable owner holds an owned fallback lookup result, while
    actual TN operands keep that owner and the receiver root alive. Cache hits
    omit the field retain. Cache misses preserve owned descriptor results and
    normal/error cleanup. Native plans and unsupported local/loop lifetimes
    retain owned reads. Local overwrites must also prove destruction safe.
- **Implemented singleton fill**:
  - `seqtran.py` transforms `[elem] * n` and `n * [elem]` when the producer is a
    fresh singleton list and the count is a proven exact integer. Operand
    producers retain their original evaluation order. Unknown count types,
    including custom multiplication or index protocols, keep generic dispatch.
  - The compiled `_list_fill` algorithm validates index-sized count bounds
    before negative-count normalization, reserves storage once, and fills it
    with references to the original element. Rust supplies only physical
    capacity reservation and existing append/lifetime primitives. Allocation
    errors propagate, and every initialized element remains owned by the list.
  - The transform derives the element type from the item, so replacing a
    singleton allocation does not discard storage element facts.

The [layout lowering contract](native-lowering.md#general-fixed-slot-access-section-6j)
and [fill contract](sequences-and-iteration.md#singleton-list-fill-section-6j)
describe the Python mutation and ownership adaptations to SBCL.

### 6K. Compositional relational constraints (`equality-constraints.lisp`) and recursive container `CType`s (`ctype.lisp`)

- **Current special case**:
  - `_MidpointSumVars` (`mule/compiler/constraint.py:574-621`) only matches
    `(lo + hi) >> 1` and `_extract_linear_term` (`constraint.py:493-553`) caps
    additive offsets to `[-16, 16]`.
  - `_IsElementPreservingBuiltin` (`constraint.py:1103-1115`) uses an external
    `element_contracts` side-table that forwards nested container contracts only
    across `("list", "sorted")` because `ValueType` (`ctype.py`) truncates
    container nesting at depth 2–3.
- **SBCL reference**: operator-level `constraint-propagate-result` optimizers
  and arbitrary-integer `equality-constraint-amount` in
  `src/compiler/equality-constraints.lisp`, and recursive `ctype` descriptors in
  `src/compiler/ctype.lisp`.
- **Required implementation**:
  1. Remove the `[-16, 16]` offset cap in `_extract_linear_term` and replace
     `_MidpointSumVars` with compositional relational constraint propagation per
     operator (`+`, `-`, `>>`, `//`) in `constraint.py`.
  2. Represent arbitrary container nesting depth directly on `ListType`,
     `TupleType`, `DictType`, and `SetType` in `ctype.py` (with a `proven: bool`
     flag distinguishing runtime-guarded annotations from statically constructed
     containers) and remove the `element_contracts` side-table and
     `_IsElementPreservingBuiltin`.

### 6L. Pre-representation calling-convention selection (`locall.lisp`) and general inline string equality (`seqtran.lisp`) — Implemented

- **Implemented architecture**:
  - `NumericEntry`, `numeric_entry`, and `select_numeric_calls` live in
    `mule/compiler/callconv.py` alongside `CallConvention` and run in
    `compile_component` after `assign_variable_primitive_types(component)` and
    before `represent.py:select_representations(component)` (so representation
    selection runs once with final call operand/result restrictions).
    `select_numeric_calls` supports self-recursive and mutually recursive calls
    (both annotated with `-> int` / `-> float` and unannotated, tail and non-tail),
    closures, and guarded mixed `(SIGNED/DOUBLE, DESCRIPTOR)` conventions.
  - `_pack_inline_latin1_words` and `_emit_inline_latin1_compare`
    (`mule/compiler/vops.py`) cover length `0..24` Latin-1 literals (comparing
    only the 16-bit `(len << 8) | kind_tag` header for `""` and the header plus
    `words[0]` for single-character literals without materializing a borrowed
    text slot or synthesizing fake `inline.text` SSA names), and general `str`
    equality uses the `alwaysinline` `@mule.fast.text.eq` /
    `@mule.fast.text.ne` helpers before falling back to `@mule_text_eq`.

### 6M. `SetType` lattice, IR1 set/dict transforms, and pre-sized `_Table` set operations (`target-hash-table.lisp` / `containtran.py`) — Implemented

- **SBCL reference**: `src/code/target-hash-table.lisp` (`make-hash-table`,
  `define-ht-getter`, `define-gethash/flat`, `puthash`, `remhash`, `maphash`)
  and `src/compiler/seqtran.lisp`.
- **Implemented architecture**:
  1. **`SetType(InstanceType)` and `SetContract` (`mule/compiler/ctype.py`,
     `annotation_types.py`, `ir1tran.py`, `constraint.py`, `locall.py`)**:
     - Added `SetType(element, frozen=False, may_be_unbound=False)` (`SET_TYPE`,
       `EMPTY_SET_TYPE`, `FROZENSET_TYPE`, `EMPTY_FROZENSET_TYPE`) to `ctype.py`
       with element-preserving `join`, `fits`, `with_set_element`,
       `iter_element_type`, binary `&`/`|`/`-`/`^` result propagation, and
       interface serialization.
     - Added `SetContract(element, frozen=False)` and `"set"` / `"frozenset"`
       declarations to `annotation_types.py` and `_SetTypeBinding` to `ir1tran.py`
       so `set[T]` and `frozenset[T]` parameter, local, and return annotations
       and element contracts propagate through `s.add`, `s | other`, `s & other`,
       `s - other`, `s ^ other`, and `for x in s:`.
     - Updated `constraint.py` (`visit_Set`) so multi-assigned local variables
       whose every assignment preserves the same container element contract
       retain their element contract across reassignments (`current = current | s`
       / `current = current & s`).
  2. **IR1 `@deftransform`s for `set`, `frozenset`, and `dict` operations
     (`mule/compiler/containtran.py`, `seqtran.py`, `fndb.py`, `codegen.py`,
     `vops.py`, `ir2tran.py`, `escape.py`)**:
     - Added known sequence-protocol intrinsics and IR1 transforms for:
       - `set()` / `set` literal `{}` -> `empty_set`
       - `s.add(x)`, `s.discard(x)`, `s.remove(x)` on mutable `SetType` ->
         `set_add`, `set_discard`, `set_remove`
       - `s & other`, `s | other`, `s - other`, `s ^ other` on `SetType` ->
         `set_and`, `set_or`, `set_sub`, `set_xor`.
  3. **Fast `_Table` set helpers and pre-sized known-hash binary set operations
     (`stdlib/_dict.py`, `stdlib/_sequences.py`)**:
     - Eliminated the `try ... except TypeError:` handler frame from the hot path
       of `_set_lookup` and `_set_remove` in `stdlib/_dict.py` (probing directly
       when `_storage_hash(key)` is non-`None` and only converting `set -> frozenset`
       when `_is_mutable_set(key)` is true).
     - Added `_set_insert` and `_set_remove_entry` (and updated `_table_resize`
       and `set_add`) so sets never append `None` into `_values`.
     - Replaced `_table_live(self)` intermediate list allocations across
       `stdlib/_dict.py` with direct indexed `_storage_read` loops over `_keys`.
     - Added `_empty_set_sized(minimum)` and specialized `set_and`, `set_or`,
       `set_sub`, and `set_xor` in `stdlib/_sequences.py` to pre-size the output
       `_indices` table once and probe/insert using the already-stored fixnum hash
       `_storage_read(_instance_field(..., "_hashes"), index)` without
       re-hashing keys or intermediate table resizes.
- **Measured benchmark result**: `set_operations` (`mule-o2`) improved from
  **`2.473s` (`24.78x` CPython)** to **`0.244s` (`2.05x` CPython, vs `0.198s`
  SBCL)** — a **10.1x** speedup.

### 6N. Generator IR1 loop fusion and non-escaping state-machine lowering (`seqtran.lisp` / `stdlib/_generators.py`) (proposed)

- **Observed benchmark impact**: `generator_pipeline` (`0.3134s` Mule vs
  `0.1028s` CPython = `3.05x`, `0.0064s` SBCL = `49.23x`).
- **Current Mule defect**:
  1. Every Python generator invocation (`stdlib/_generators.py` and
     `mule/runtime/coroutine.rs`) allocates a separate stackful coroutine stack
     (`coroutine_new`), and every `next()` / `yield` performs a full Python
     method call chain (`generator.__next__` -> `generator.send` ->
     `generator._resume` -> `coroutine_state` + `coroutine_resume` stack switch)
     and raises/catches `StopIteration` on exhaustion.
  2. In `mule/compiler/represent.py:148`, `ir2tran.py:1937`, and
     `ir1opt.py:274`, `component.lambda_.coroutine_body` disables flow-proven
     unboxed fixnum locals (`return IMMORTAL_T`), container-element borrowing,
     and `ok_lvar_lambda_var` combination matching across the entire generator
     body.
  3. Consequently, a 3-stage pipeline `stage3_Running_sum(stage2_transform(stage1_filter(n, seed)))`
     consumed by `for val in pipeline:` performs ~3 stack context switches and
     boxed fixnum operations per element instead of fusing into a single
     unboxed register loop.
- **SBCL reference**: `src/compiler/seqtran.lisp` (`map`, `reduce`, `loop` /
  sequence iterator fusion) and `src/compiler/locall.lisp` (`let-convert`),
  which inline non-escaping producer/consumer pipelines into a single loop body
  in IR1.
- **Required implementation**:
  1. **IR1 generator pipeline fusion**: When a generator function's call result
     is consumed exclusively by `for x in gen(...):` (or chained non-escaping
     generator stages feeding a `for` loop, `sum(...)`, `any(...)`, `all(...)`,
     `list(...)`) and the generator body uses structured `yield` without `.send`,
     `.throw`, `.close`, or frame inspection, inline/LET-convert the generator
     body into the consumer loop in IR1, turning `yield val` into the consumer's
     loop body and allowing `represent.py` to keep all stage locals in unboxed
     `i64` / `double` registers.
  2. **State-machine lowering for escaping single-frame generators**: For
     generators whose suspension points do not occur inside nested Python calls
      requiring a separate C stack, lower the generator body to a flat heap-frame
     state machine (`switch` on resumption state index) and allow locals that do
     not cross a `yield` point (or whose binding state is proven initialized at
     every reachable `yield`) to use unboxed `SIGNED` / `DOUBLE` slots.

### 6O. `isinstance` / `match` class-pattern branch narrowing, polymorphic dispatch, and exception-raise fast paths (`constraint.lisp` / `ir1tran.py`) (proposed)

- **Observed benchmark impact**: `dataclass_ast` (`0.0257s` Mule vs `0.0022s`
  SBCL = `11.67x`), `expression_tree` (`5.8504s` Mule vs `4.3744s` CPython =
  `1.34x`, `1.5239s` SBCL = `3.84x`), and `exception_validation` (`3.1574s` Mule
  vs `0.6050s` SBCL = `5.22x`).
- **Current Mule defects**:
  1. **`match` class patterns and `%isinstance` do not narrow user classes in
     `constraint.py`**:
     - `_MatchLowering.class_pattern` (`mule/compiler/ir1tran.py:4148-4158`)
       emits `isinstance(subject, (ClassName,))` wrapped in a 1-element tuple
       even for a single class.
     - `_propagate_instance_check` (`mule/compiler/constraint.py:2675`) derives
       the boolean result of `%isinstance` when already statically proven, but
       there is **no `@defconstrain("%isinstance")` branch constrainer** in
       `constraint.py` (unlike `_constrain_builtin_call` for `CallKind.FULL`
       calls to `isinstance(x, int/str/bytes/tuple/list/dict)` at
       `constraint.py:3947`, which also ignores user-defined classes and
       1-element class tuples!).
     - As a result, inside every `case Lit(value=v):` / `case Add(left=l, right=r):`
       arm in `dataclass_ast.py`, `subject` remains `UNKNOWN` (or an unnarrowed
       union) instead of narrowing to `InstanceType(Lit)` / `InstanceType(Add)`.
       Every field extraction `subject.value`, `subject.left`, `subject.right`
       therefore misses `InstanceType` field-type inference (`locall.py`) and
       fixed-slot layout specialization (Section 6J)!
  2. **Polymorphic method calls and `super().__init__`**:
     - In `expression_tree.py`, `self.left.eval_node(env)` dispatches across a
       closed 5-class hierarchy (`LitExpr`, `VarExpr`, `AddExpr`, `MulExpr`,
       `LetExpr`) through generic method lookup on each recursive step.
     - In `exception_validation.py`, `ValidationError.__init__` and
       `RangeError.__init__` call `super().__init__(message)` via
       `RESOLVE_SUPER_METHOD` + `CALL_RESOLVED` and allocate an exception
       instance plus `_args` tuple on every invalid record before unwinding to
       the caller's `try/except ValidationError:` handler.
- **SBCL reference**: `src/compiler/constraint.lisp`
  (`add-test-constraint` on `%instance-typep` / `typep`, narrowing the tested
  variable to the target `structure-class` / `standard-class` on the consequent
  branch and subtracting it from `UnionType` on the alternative branch) and
  `src/compiler/typetran.lisp` (`source-transform-typep`).
- **Required implementation**:
  1. In `_MatchLowering.class_pattern` (`ir1tran.py:4148`), pass `ClassName`
     directly (not a 1-element tuple `(ClassName,)`) to `isinstance`, and add
     `@defconstrain("%isinstance")` (plus user-class support in
     `_constrain_builtin_call`) in `constraint.py` so `isinstance(x, Cls)` and
     `isinstance(x, (Cls1, ...))` narrow `x` to `InstanceType(Cls)` (or union of
     `InstanceType`s) on the true branch and subtract `InstanceType(Cls)` from a
     `UnionType` on the false branch.
  2. Specialize `RESOLVE_SUPER_METHOD` when the enclosing class and its single
     base class are statically known in the compilation unit into a direct call
     to the base class method.

### 6P. `bytearray` inline buffer VOPs and string `rsplit` / `lower` / `strip` / multi-concat transforms (`array-tran.lisp` / `seqtran.lisp`) (proposed)

- **Observed benchmark impact**: `bytes_codec` (`0.0216s` Mule vs `0.0027s`
  SBCL = `7.89x`), `log_aggregation` (`0.1389s` Mule vs `0.0572s` CPython =
  `2.43x`), and `http_routing` (`0.0707s` Mule vs `0.0506s` CPython = `1.40x`).
- **Current Mule defects**:
  1. **`bytearray` iteration and C-ABI call overhead (`bytes_codec`)**:
     - `seqtran.py` lowers `for x in seq:` to counted indexing for `list`,
       `tuple`, `str`, `bytes`, and `dict.items()`, but **not** `bytearray`. In
       `bytes_codec.py`, `for b in decoded:` allocates a heap iterator and calls
       `@mule.fast.next_item` + catches `StopIteration`.
     - `bytearray` indexing (`b[i]`, `b[i] = v`) and `b.append(v)` call external
       Rust functions (`@mule_bytearray_getitem_fast`,
       `@mule_bytearray_setitem_fast`, `@mule_bytearray_append_fast`) on every
       byte instead of loading/storing from the inline buffer pointer + length
       fields in LLVM IR (calling Rust only when `len == capacity` requires
       reallocation).
  2. **Missing `str.rsplit`, `int(s.strip())`, case-insensitive comparison, and
     n-ary string concatenation transforms (`log_aggregation`, `http_routing`)**:
     - `line.rsplit(".", 1)` and `route.rsplit("/", 1)` in `log_aggregation.py`
       allocate a 2-element Python `list` and two `str` slices via generic method
       dispatch; `ir2tran.py` only has `exact_text_split`, not `rsplit`, and
       `seqtran.py` has no bounded `rsplit(..., 1)` or `int(s.rsplit(sep, 1)[1])`
       slice fusion.
     - `int(value.strip())` in `http_routing.py:73` allocates a stripped `str`
       before calling `%parse_int_text`, even though `int(str)` already skips
       leading and trailing ASCII/Unicode whitespace.
     - `name.lower() == "content-length"` (`http_routing.py:72`) allocates a
       lowercased `str` on every header line instead of folding to an allocation-
       free ASCII case-insensitive comparison when compared against an ASCII
       lowercase literal.
     - Chained string concatenations `a + b + c + d` (`http_routing.py:41-50, 84, 88`)
       allocate `n - 1` intermediate `TextStorage` objects via binary `%add`
       instead of flattening into a single pre-sized `%format` build in
       `formattran.py`.
- **SBCL reference**: `src/compiler/array-tran.lisp`
  (`data-vector-ref-with-offset/simple-array-unsigned-byte-8`,
  `vector-push-extend`), and `src/compiler/seqtran.lisp` (`concatenate`,
  `string-equal`, `search`/`position` `:from-end t`).
- **Required implementation**:
  1. Add `BYTEARRAY_TYPE` counted loop lowering in `seqtran.py` (re-reading the
     current length on each step to preserve mutation semantics) and emit inline
     LLVM byte loads/stores and capacity-checked appends for `BYTEARRAY_TYPE` in
     `vops.py`.
  2. Add IR1 `@deftransform`s in `srctran.py`, `seqtran.py`, and `formattran.py`
     for:
     - `int(s.strip()) -> %parse_int_text(s)` when `s` is `TEXT_TYPE` and
       `strip()` is called with 0 arguments;
     - `s.lower() == <ascii-lower-literal>` and `s.upper() == <ascii-upper-literal>`
       into an allocation-free case-insensitive comparison;
     - Chained `TEXT_TYPE + TEXT_TYPE + ...` (`>= 3` operands) into `%format`;
     - `s.rsplit(sep, 1)` on `TEXT_TYPE` with a non-empty constant separator into
       a reverse find (`rpartition`-style index scan), fusing with `[0]` / `[1]`
       indexing and `%parse_int_slice` when non-escaping.

### 6Q. Unboxed and stack-promoted closure cells for non-escaping mutable `nonlocal` variables (`envanal.lisp` / `represent.lisp`) (proposed)

- **Observed benchmark impact**: `closure_pipeline` (`1.6020s` Mule vs `0.2638s`
  SBCL = `6.07x`).
- **Current Mule defect**:
  - In `perf/suite/closure_pipeline.py`, `make_stepper` returns a closure
    `_step(x: int) -> int` that mutates `nonlocal acc, count`. Because `_step`
    is returned from `make_stepper`, `acc` and `count` have `var.indirect = True`
    (`envanal.py`).
  - Even when every closure sharing an `indirect` cell proves `is_fixnum` across
    all cell stores (`var.optimistic_type.is_fixnum`), `value-cell-ref` and
    `value-cell-set` (`mule/compiler/vops.py:2325-2365`) store a boxed/tagged
    descriptor via runtime attribute helpers (`@mule_getattr_cached` /
    `@mule_setattr_cached` on `cell_contents`) and `represent.py:145` forces
    `if var.indirect: return OBJECT`, preventing unboxed `i64` cell payloads or
    direct fixed-offset cell slot loads/stores.
  - Furthermore, when `s1 = make_stepper(...)` is bound locally in `run` and
    called only via direct calls `s1(v)` without escaping `run`, inlining
    `make_stepper` (or tracking factory-returned local closures in `locall.py`
    and `escape.py`) would make `acc` and `count` ordinary local `i64`
    registers in `run`.
- **SBCL reference**: `src/compiler/envanal.lisp` (indirect value cells accessed
  via single-instruction `value-cell-ref` / `value-cell-set` VOPs at a fixed word
  offset in `src/compiler/x86-64/cell.lisp`) and `src/compiler/locall.lisp`
  (inlining small closure factories so captured cells become local `lambda-var`s).
- **Required implementation**:
  1. Lower `value-cell-ref` and `value-cell-set` (`vops.py:2325-2365`) on
     compiler-allocated internal `Cell` objects (when the cell never escapes to
     `__closure__` / `cell_contents` introspection) to direct fixed-offset word
     loads/stores at the `Cell` payload slot instead of attribute-cache helper
     calls, omitting reference-count traffic when `var.optimistic_type.is_fixnum`
     or `is_immortal`.
  2. When a small factory function (`make_stepper`) returns a closure and is
     called at a local call site whose returned closure does not escape the
     caller, inline the factory in IR1 so the closure becomes a closed local
     lambda in the caller and its `nonlocal` variables lower to caller frame
     registers (`escape.py`).


### 6R. Iterative zero-allocation `_sre` bytecode VM and `_INFO` prefix/charset search (`stdlib/_sre.py`, `stdlib/re/_compiler.py`) (proposed)

- **Observed benchmark impact**: `bm_regex_dna`, `bm_regex_effbot`, `bm_regex_v8`,
  and general `re` workloads in `pyperformance`.
- **Current Mule defect**:
  1. **Recursive and generator-allocating `_Engine` in `stdlib/_sre.py`**:
     - `_Engine._search_state` (`stdlib/_sre.py`) is an untyped, recursive Python
       interpreter over CPython SRE bytecode.
     - General repetitions (`_REPEAT`, `_POSSESSIVE_REPEAT` in `_Engine._repeat`)
       allocate a Python generator coroutine (`_repeat_states`) per repeat entry
       and yield `(at, list(marks))` tuples across every backtracking step.
     - Every capture group mark transition (`_MARK`) clones the entire `marks`
       list on the heap (`next_marks = list(marks)`) instead of saving/restoring
       mutated mark slots on a flat undo stack.
     - `_Engine.char(pos)` on `str` inputs indexes `self.string[pos]` (allocating
       a 1-character `str` object when not lowered to a raw codepoint primitive)
       and calls `ord(...)` on every character inspection.
  2. **Unused `_INFO` prefix/overlap and charset tables from `re._compiler`**:
     - `stdlib/re/_compiler.py` (`_compile_info`) already emits CPython's `_INFO`
       header (`SRE_INFO_PREFIX`, `SRE_INFO_LITERAL`, `SRE_INFO_CHARSET`) at
       `code[0]`, including minimum/maximum match lengths (`lo`, `hi`), the exact
       literal prefix array, the Knuth-Morris-Pratt (`overlap`) skip table, and
       the 256-bit starting `CHARSET` bitmap.
     - `stdlib/_sre.py` ignores the `_INFO` header altogether and steps
       `self._match_at(string, pos, endpos)` one character at a time from `pos`
       to `end`.
- **CPython and SBCL (CL-PPCRE) references**:
  - CPython `Modules/_sre/sre_lib.h` (`SRE(match)`, `SRE(search)`,
    `SRE(fast_search)`): flat iterative bytecode VM using an explicit integer data
    stack (`SRE_REPEAT` frames + `(mark_index, old_pos)` mark-undo stack) over
    direct UCS1/UCS2/UCS4/byte buffer reads, paired with `_INFO`-driven fast
    prefix/KMP overlap and charset search.
  - CL-PPCRE (`scanner.lisp`, `closures.lisp`): compiled closures/state machines
    with flat integer mark arrays and Boyer-Moore-Horspool / start-char skip
    loops derived from the pattern tree.
- **Required implementation**:
  1. **Unboxed codepoint access (`%text_codepoint_at`)**: Route `_Engine.char(pos)`
     on `str`, `bytes`, `bytearray`, and `memoryview` through direct unboxed
     integer codepoint/byte reads without allocating 1-character `str` slices.
  2. **Flat iterative typed `_Engine` VM (`stdlib/_sre.py`)**:
     - Type `_Engine` fields and VM loop locals (`code: list[int]`, `pc: int`,
       `pos: int`, `end: int`, `marks: list[int]`, `undo_stack: list[int]`,
       `state_stack: list[int]`) so Mule compiles opcode dispatch and character
       matching with unboxed `i64` registers and fixed-slot instance access
       (`__mule_slots__`).
     - Replace Python call recursion and `_repeat_states` generator coroutines
       with an explicit flat `state_stack: list[int]` and an in-place `marks`
       array restored via `undo_stack: list[int]` high-water marks (matching
       `MARK_PUSH` / `MARK_POP` in CPython's `sre_lib.h`).
  3. **Standard `_INFO`-driven search (`SRE_INFO_PREFIX` / `SRE_INFO_CHARSET`)**:
     - Parse the `_INFO` block emitted by `stdlib/re/_compiler.py` (`flags`,
       `min_width`, `prefix` + `overlap` KMP table, or `charset` bitmap) in
       `Pattern.__init__` and execute CPython's `sre_search` algorithm directly
       from the `_INFO` block — with no ad-hoc pattern heuristics or custom
       bytecode prefilters.
  4. **Compliance verification**: Validate against CPython's upstream
     `stdlib/test/test_re.py` test suite and `test/mule/compiler/test_regex_repeats.py`.


### 6S. `@final` directive support from `mule_types` and on classes and methods (`mule/compiler/final_functions.py`, `mule/compiler/ir1tran.py`) (proposed)

- **Observed impact**: Bootstrap `stdlib/` modules (`stdlib/_pyio.py`,
  `stdlib/_codecs.py`, `stdlib/_sequences.py`, `stdlib/_str.py`,
  `stdlib/_object.py`, `stdlib/_dict.py`, `stdlib/_format.py`) and user classes
  using `@final` from `mule_types` or `typing`.
- **Current Mule defect**:
  1. **`FinalFunctions` only recognizes `typing.final`, not `mule_types.final`**:
     - `mule_types/__init__.py` exports `final` in `__all__` specifically so
       modules (including bootstrap `stdlib/` modules that must not import
       `typing` at startup) can declare `@final` bindings without importing
       `typing`.
     - However, `FinalFunctions.visit_Import` and `FinalFunctions.visit_ImportFrom`
       in `mule/compiler/final_functions.py:41-66` only accept `module == "typing"`,
       ignoring `from mule_types import final` and `import mule_types`
       (`@mule_types.final`).
     - In addition, `_EXPORT_BINDINGS` in `mule/compiler/ir1tran.py:5928-5931`
       registers `"optimize"` as a zero-cost compile-time decorator binding, but
       does not register `"final"`, causing `from mule_types import final` to
       emit a runtime import and decorator call (`mule_types.final(fn)`) at
       module initialization instead of stripping the compile-time declaration.
  2. **`@final` on classes and methods is ignored (`visit_ClassDefinition` is `pass`)**:
     - `FinalFunctions.visit_ClassDefinition` (`mule/compiler/final_functions.py:81-82`)
       is currently `pass`.
     - Decorating a class or method with `@final` (`@typing.final` or
       `@mule_types.final`) does not mark the class layout as final/sealed or
       allow `ir1tran.py` to devirtualize/inline self-calls (`self._helper(...)`)
       on `@final` methods or classes without requiring a full-program closed-world
       subclass check.
- **Required implementation**:
  1. Recognize both `typing` and `mule_types` in `FinalFunctions`
     (`mule/compiler/final_functions.py`) and register `"final"` in
     `_EXPORT_BINDINGS` (`mule/compiler/ir1tran.py`) as a compile-time
     declaration binding (`runtime_decorators` returning `()`) when imported from
     `mule_types`.
  2. Extend `FinalFunctions` and `ir1tran.py` class lowering so `@final` on a
     class marks the class as disallowing subclassing and enables direct
     closed-call lowering for its methods, and `@final` on a method enables
     direct call lowering when the receiver is proven to be an instance of the
     declaring class (or a subclass that does not override the method).


### 6T. Function-scoped and codec lazy imports in bootstrap `stdlib/` modules trigger runtime `--dynamic-module` subprocess compilation (`mule/compiler/modules.py`) (proposed)

- **Observed impact**: Any compiled Mule binary calling `open()` or `str.encode` /
  `bytes.decode` at runtime incurs a multi-second pause on first file open or
  codec lookup.
- **Current Mule defect**:
  1. `stdlib/_codecs.py` (`_initialize`, line 95) performs a lazy function-scoped
     `import encodings`, and `encodings.search_function` dynamically imports
     `encodings.aliases` and common codec modules (`encodings.utf_8`,
     `encodings.latin_1`, `encodings.ascii`) via `__import__`.
  2. Because `_ModuleImportScanner` in `mule/compiler/modules.py` only pre-scans
     static module-level imports of non-primitive modules or explicitly listed
     dependencies in `ModuleTable.resolve`, `encodings` and its standard UTF-8 /
     Latin-1 / ASCII codecs are omitted from the AOT binary.
  3. At runtime, the first `open()` call invokes `_codecs.lookup("utf-8")`,
     which misses the static module table and spawns a fresh Python compiler
     subprocess (`python -m mule.compiler --dynamic-module encodings`) to
     compile `encodings` and `encodings.aliases` on the fly.
- **Required implementation**:
  1. In `ModuleTable.resolve` (`mule/compiler/modules.py`), when resolving
     `_codecs`, pre-resolve `encodings`, `encodings.aliases`, `encodings.utf_8`,
     `encodings.latin_1`, and `encodings.ascii` (or route standard UTF-8,
     Latin-1, and ASCII incremental encoders/decoders directly through `_codecs`
     before importing `encodings`) so standard file I/O never spawns a runtime
     `--dynamic-module` compiler subprocess.


### 6U. Runtime special-method helpers allocate heap `Data::BoundMethod` and `String` on every call (`mule/runtime/object.rs`, `mule/runtime/classes.rs`) (proposed)

- **Observed impact**: Any unspecialized special-method dispatch (`next()`,
  `iter()`, `x[i]`, `x[i] = v`, `del x[i]`, `+x`, `-x`, `~x`, `abs(x)`,
  `str(x)`, `repr(x)`, `format(x)`, `operator.index(x)`, `with` context
  managers) and `class_lookup_default` in `stdlib/_sequences.py`,
  `stdlib/_object.py`, and `stdlib/_format.py`.
- **Current Mule defect**:
  1. **Heap `Data::BoundMethod` allocation per special-method call**:
     - `mule_object_next_method`, `mule_object_getitem_method`,
       `mule_object_setitem_method`, `mule_object_delitem_method`,
       `mule_object_pos_method`, `mule_object_neg_method`,
       `mule_object_invert_method`, `mule_object_abs_method`,
       `mule_object_str_method`, `mule_object_repr_method`,
       `mule_object_format_method`, and `mule_object_index_method` in
       `mule/runtime/object.rs` look up the special method on `type(value)` and
       call `classes::bind_instance_attribute`, which allocates a boxed
       `Data::BoundMethod { function, receiver }` on the heap just so the
       caller can immediately invoke `method(...)` and drop it.
  2. **Heap `String` allocation and MRO hash walk in `mule_class_lookup_default`**:
     - `mule_class_lookup_default(class, name, default)` in
       `mule/runtime/classes.rs` calls `object::string_value(name)` (allocating
       a new Rust `String` on the heap on every call) and walks `mro_lookup`,
       ignoring the precomputed `ClassLayout::special_slot` table.
- **Required implementation**:
  1. In `mule/runtime/classes.rs`, make `mule_class_lookup_default` borrow
     `Data::Text` ASCII bytes directly (`text.as_ascii_bytes()`) without heap
     `String` allocation, and consult `ClassLayout::special_slot(name)` before
     falling back to `mro_lookup`.
  2. Provide direct unbound special-method invocation primitives (or lower
     `_next_method(x)()`, `_getitem_method(x)(i)`, etc.) that pass `receiver` as
     arg 0 to `Data::Function` without allocating a transient
     `Data::BoundMethod` on the heap, matching CPython's
  `_PyObject_VectorcallTstate` / `PY_VECTORCALL_ARGUMENTS_OFFSET` special
     method call path.

### 6V. SBCL conservative container types, word vectors, and declared instance fields (`constraint.py`, `ctype.py`, `locall.py`, `wordops.py`, `ir1tran.py`, `return_checks.py`) (implemented)

Mule tracked what mutable containers hold by joining the values written into
them, and tracked aliases and parent containers to invalidate those facts.
A write through an untracked alias left a stale element or length fact, which
miscompiled programs at `optimize=1` (including `heterogeneous_slot_mutation`
in `perf/topics/objects/cycles_and_lifetimes.py`). SBCL keeps no such facts: a
mutable object's contents type is used only at a reference to a variable with a
single reference (`lvar-conservative-type`, `node-conservative-type`, and
`conservative-type` in `src/compiler/ir1opt.lisp`), array element types come
from the array's representation, and slot types come from declarations. The
architecture is specified in [type-inference.md](type-inference.md)
Section 3.6.

Delivered:

1. `ValueType.conservative_type()` and `Fact.conservative()` apply SBCL's
   conservative type at references to multiply referenced variables and module
   globals, and vector-length constraints apply only to simple sequences: text,
   bytes, tuples, and word vectors (`%check-bound` and
   `vector-constraint-length` in `src/compiler/equality-constraints.lisp`).
2. Word vectors (`WordVectorType`, `WORD_VECTOR_T`, `mule/compiler/wordops.py`,
   and `Data::RawWords` in `mule/runtime/object.rs`) back `_Table._indices` and
   `_Table._hashes` in `stdlib/_dict.py`. Generic storage primitives
   (`fast_storage_length.ll`, `fast_storage_read.ll`,
   `fast_storage_read_borrow.ll`, `fast_storage_write.ll`, and
   `mule/runtime/sequences.rs`) dispatch on tag 28 (`Data::RawWords`) when the
   receiver is not statically proven to be a word vector, matching SBCL's
   `hairy-data-vector-ref`.
3. Declared container contracts (`list[T]`, `dict[K, V]`, `set[T]`) preserve
   element contracts across `Fact.conservative()` via `walk.local_contracts`
   and `walk.element_contracts` in `mule/compiler/constraint.py` without
   storing element types in the container's `ValueType`.
4. All static container-mutation, alias, and parent-container tracking
   (`update_mutated_container`, `mutate_container_element`, `mutate_list`,
   `mutate_dict`, `mutate_set`, `record_alias`, `walk.aliases`,
   `record_container_child`, `walk.parent_containers`, `parent_origins`,
   `widen_escaped_container`, `update_nested_*`, `_update_as_nested_*`, and
   `propagate_to_parent_container`) has been removed from `ctype.py`,
   `constraint.py`, `protocols.py`, `fndb.py`, and `locall.py`.
5. Declared instance fields (`ClassInfo.field_annotations` in `ir1tran.py`,
   `ClassFacts.declared_fields` and `unsafe_declared_fields` in `locall.py`,
   `_InstanceDeclaredField` in `constraint.py`, and `DeclaredFieldContract` /
   `assert_declared_field_stores` in `return_checks.py`) implement SBCL's
   `slot-access-transform` and `dsd-safe-p`: undeclared instance fields have
   type `UNKNOWN`, safe declared fields are trusted on read and checked on
   resolved stores (`safety > 0`), and fields narrowed by a subclass are
   checked on read.
6. Verified by `test_conservative_container_mutations_and_declared_instance_fields`
   in `test/mule/compiler/test_set_specialization.py` and
   `perf/topics/objects/cycles_and_lifetimes.py`.
