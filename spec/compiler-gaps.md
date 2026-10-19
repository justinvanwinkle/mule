# Remaining SBCL compiler and IR1 transform gaps

Status: **implemented** for general-context flow-type feedback into IR1
transforms (Section 1A); **proposed** for Sections 1B–5.

This document records the remaining architectural and transform gaps between
SBCL's compiler (`~/checkout/sbcl/src/compiler/`) and Mule (`mule/compiler/`),
together with the SBCL source references, Python semantic adaptations, and
staged implementation plan for each area.

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
    `Lvar` in the component and marks narrowed known calls and predictable
    builtin global calls (`len`, `str`, and any future `KNOWN` entry) for
    reoptimization via `_ReoptimizeNarrowedKnownCalls`.
  - `_DeriveNodeType` and `_PrepareNodeOptimize` (`mule/compiler/ir1opt.py`)
    preserve flow-derived types on `Ref(LambdaVar)` and non-`KNOWN`
    `Combination`s across IR1 reoptimization rounds when local syntactic
    derivation yields `UNKNOWN`.
  - `srctran.py` and `modarith.py` consult flow-proven singleton types on pure
    bound local references (`_is_pure_bound_ref`), enabling identity
    reductions, absorbing-zero folds, power-of-two strength reductions,
    redundant bitmask elimination, and associative constant folding on local
    variables proven constant by flow analysis, while unlinking dead
    `Ref(LambdaVar)` nodes.

### 1B. Entry-context IR1 specialization and local inlining (proposed)

- **SBCL reference**: `locall-analyze-component`, `convert-more-call`,
  `expand-local-inline` (`src/compiler/locall.lisp`, `src/compiler/ir1opt.lisp`).
  In SBCL, local calls to known functions or inline functions clone or
  specialize the IR1 lambda graph so that parameter types from call sites flow
  into `ir1-optimize` and `deftransform` inside the specialized body.
- **Current Mule state**: `locall.analyze` (`mule/compiler/locall.py`) solves
  per-entry-context types (`general`, `fixnum`, `observed`) and `ir2tran.py`
  emits separate IR2/LLVM bodies per `EntryContext`. However, `Lambda` owns a
  single IR1 graph whose `node.derived_type` reflects only the `general` entry
  context. Consequently, IR1 `@deftransform`s cannot rewrite the IR1 graph using
  `fixnum`- or `observed`-only parameter facts unless duplicated in IR2.
- **Proposed plan**:
  1. **Closed-call promotion to `general`**: When a nested `Lambda` does not
     escape (`envanal.py` / `locall.py` proves all references are direct call
     sites and the function object is never stored, returned, or passed as a
     value), seed its `general` parameter types directly from the join of its
     callers' argument types so IR1 `@deftransform`s fire directly on its IR1
     graph.
  2. **IR1 cloning for specialized entry contexts and small local helpers**:
     Add an IR1 lambda cloner in `ir1util.py` / `locall.py` (following SBCL's
     inline expansion) for specialized `EntryContext`s that unlock IR1
     transforms and for small non-recursive local functions (`LET` conversion /
     local inlining).

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

## 5. Representation selection and unboxed SSA values across blocks (`represent.lisp`) (proposed)

- **SBCL reference**: `src/compiler/represent.lisp` (`select-tn-representation`,
  automatic coercion VOPs at block boundaries).
- **Current Mule state**: `mule/compiler/represent.py` and `vops.py` keep raw
  `i64` / `double` values for `mule.int64` / `mule.float64` annotated locals and
  range-loop counters, and unbox fixnum arithmetic inside basic blocks, while
  normal `LambdaVar`s store tagged `Value` (`ptr`) across block boundaries.
- **Proposed plan**:
  1. **Flow-proven unboxed local TNs (`SC_SIGNED_REG` / `SC_DOUBLE_REG`)**:
     When `represent.py` sees that every write and read of a non-closed-over
     `LambdaVar` in an `IR2Component` (general or specialized `EntryContext`) has
     a proven `FIXNUM` (`IntegerType` fitting `[FIXNUM_MIN, FIXNUM_MAX]`, not
     `may_be_unbound`), `int64`, or `FLOAT` type, assign its local TN to
     `SC_SIGNED_REG` (`i64`) or `SC_DOUBLE_REG` (`double`) across the entire
     function body and insert tag/box coercions only at boundaries where a
     generic runtime helper requires a `ptr` `Value`.
