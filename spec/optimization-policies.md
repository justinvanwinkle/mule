# Optimization policies

Status: a signed-64-bit subset of native structs is implemented; the complete
policy matrix remains a target. Explicit `@mule_types.optimize(speed=3)` classes with
bare `mule_types.int64` fields use raw storage, require definite construction
initialization, and can select native internal method and caller entries.
The compiler keeps compiled generic entries for bodies that cannot establish
the required proofs. See [current behavior](native-compiler.md#annotation-contracts)
and [current classes](native-compiler.md#fixed-layout-classes) for the supported
source subset. The remaining level assignments below do not establish Python
data-model parity or support for other raw field widths.

## SBCL implementation correspondence

The inspected implementation is the local `~/checkout/sbcl` source. Native
class lowering must reproduce the following compiler mechanisms, rather than
move an attribute or arithmetic evaluator into Rust:

| SBCL source and entry | Required Mule counterpart |
| --- | --- |
| `src/code/defstruct.lisp:115`, `defstruct-slot-description`; `:907`, `choose-raw-slot-representation` | Store declared slot types and constant indices in compiler layout descriptions. Select raw storage from the declared numeric contract. SBCL explicitly keeps fixnum subtypes tagged; full machine-word numeric types can select raw slots. |
| `src/code/defstruct.lisp:212`, `dsd-primitives`; `:1288`, `slot-access-transform` | Transform a proven field access into its representation-specific reader or writer at a constant slot index. Propagate the declared type to the read result. Check writes at the boundary where the invariant is established, retaining read checks only when initialization or a narrower subtype invalidates that invariant. |
| `src/code/defstruct.lisp:1932`, `instance-constructor-form` | Lower construction to allocation plus typed slot initialization. The constructor describes each slot's raw type and index; it does not repeatedly perform name lookup. |
| `src/compiler/locall.lisp:42`, `propagate-to-args`; `:141`, `convert-call` | Assert declared argument types at local-call edges and record proven local targets. Local-call conversion enables body integration; the name of a mutable global alone is not a target proof. |
| `src/compiler/represent.lisp:285`, `select-tn-representation`; `:1029`, `select-representations` | Select internal value representations separately from semantic types, then insert required moves and coercions at representation boundaries. SBCL scores admissible storage classes from use costs; Mule must preserve this separation even when LLVM selects registers and machine instructions. |

SBCL's `defstruct.lisp:153` documents the underlying invariant: slot types are
normally checked on writes. Reads need additional checks for unsafe subtype
accessors or potentially unbound slots. Its raw numeric slots cannot store an
unbound marker. This is the reason construction proof is part of native field
lowering, rather than permission to treat every heap word as a valid integer.

### Concrete adaptations and restrictions

These are required differences, with their consequences stated explicitly.
They do not justify skipping SBCL's analysis or representation stages.

- **Declaration syntax.** Lisp `defstruct` slot descriptions and `optimize`
  declarations become Python class-body annotations and the imported
  `@mule_types.optimize` compiler directive. Python classes normally allow dynamic
  mutation, so native layout restrictions require the user's explicit
  `speed=3` class policy. The consequence is an intentionally restricted Python
  class contract; an undecorated spelling alone cannot establish that contract.
- **Machine representation.** Full signed `mule_types.int64` values select an LLVM
  `i64` field and internal argument/result representation, including values
  outside Mule's tagged-integer range. LLVM constant-offset addressing and
  loads/stores replace SBCL's raw-instance VOPs because LLVM is the required
  backend. Generic object adapters box or unbox at real entry/exit boundaries;
  internal arithmetic and field operations remain native. Initial support for
  only signed 64-bit native fields is an implementation boundary, not a reason
  to silently box other declared storage widths.
- **Construction.** Python `__init__` is executable code with observable order
  and effects; it cannot be replaced by Lisp's slot-value constructor form.
  Compile its body while preserving those effects, and establish initialization
  before an instance escapes, a raw field is read, or construction returns
  successfully. The initial signed-64-bit native subset requires definite
  initialization analysis and rejects a missing proof at compile time. This is
  a restriction of explicitly selected speed-three classes; existing ordinary
  classes retain their current unbound-field errors. Raw `i64` has no unused
  value for an unbound sentinel, and a hidden Rust initialization bitmap would
  move the construction semantics out of the compiler. Do not fabricate zero
  values or add that language-level Rust mechanism to bypass the analysis.
- **Exact receivers.** This level-three contract is final and requires its own
  native receiver. It rejects duck receivers and subclassing instead of
  inheriting SBCL structure subtype rules or ordinary Python's unbound-method
  permissiveness. This is an explicit semantic restriction that permits one
  stable field layout and direct method entry; a wrong generic receiver must
  receive `TypeError`, not have its storage reinterpreted.
- **Mutable bindings.** A Python module global holding a class or function can
  be rebound while the class descriptor itself remains immutable. Establish
  the actual descriptor/callable identity before entering a specialized region.
  A failed speculative guard must execute the already compiled generic path
  with the actual binding. Hoisting a guard requires proof that calls inside
  the region cannot invalidate the relevant binding; immutability of the
  original class does not provide that proof.
- **Errors.** The user requires statically detected annotation violations to be
  compilation errors. This is stricter than SBCL's warning plus compiled error
  path for some contradictory assertions. Known violations therefore fail even
  under safety zero; unknown violations use the applicable boundary and
  overflow policy. Ordinary Python `int` still has arbitrary precision.

Class construction, attribute semantics, descriptors, method dispatch and
conversion algorithms belong in compiled Mule Python, matching SBCL's compiled
Lisp language implementation. Rust may allocate storage, manage lifetime and
expose memory/foreign-library primitives. Existing Rust class and numeric
language semantics are bootstrap debt; extending a native payload primitive
does not authorize extending that language-level implementation. The existing
reference-counting implementation is also not an architectural justification
for omitting SBCL's storage and lifetime analyses.

## Policy axes and scope

```python
import mule_types

@mule_types.optimize(speed=3, safety=3, debug=0)
class Counter:
    value: mule_types.int64

    def __init__(self):
        self.value = 0

    def add(self, amount: mule_types.int64):
        self.value += amount
```

This syntax is implemented for signed 64-bit fields. Constructors and field
adapters are compiled from `stdlib/_native_class.py` using Mule's parser,
frontend, analysis, ownership lowering and LLVM emitter. Their physical
allocation and integer storage/conversion primitives are backend bindings.

`speed` selects a class's semantic restrictions and available representations.
It also controls optimization effort for functions. `safety` selects checks for
trusted contracts; it does not change field layout or restore dynamic features.
`debug` selects observability and optimization constraints, independently of
class layout. All axes use levels zero through three.

A class decorator fixes that class's policy when its definition executes. Its
methods inherit the policy, with explicit method options overriding their own
code-generation settings. A method's lower speed cannot make its receiver
dynamic, and a function's higher speed cannot make a dynamic argument sealed.
The receiver's class contract governs attribute operations.

Proposed default: an undecorated class uses `speed=1`, preserving ordinary Python
class semantics. Restrictions require an explicit class policy. This class-default
rule requires a compiler change.

The default policy is `speed=1, safety=1, debug=1`. An `optimize()` directive
changes only the qualities it names; omitted qualities keep the enclosing
policy, as an SBCL `(declare (optimize speed))` leaves safety alone. Checked
native classes use `safety=3` explicitly.

A module sets its own policy by assigning integer literals to `__speed__`,
`__safety__` and `__debug_level__` (Python forbids assigning `__debug__`), as
SBCL's top-level `declaim` sets the policy for the rest of a file. The policy
applies to the module's top-level code and is the default for every
definition in the module, which decorators may override. The module's
top-level code is one lambda with one policy, so these assignments must come
before any statement other than the docstring and imports; a later one is a
compilation error. They remain ordinary module bindings at run time.

```python
"""A module with dynamic builtin lookup and no inferred return contracts."""
__safety__ = 3
__debug_level__ = 3

import builtins
```

Optimized same-unit calls can establish checked inferred return contracts while
retaining mutable function bindings. A replacement returning an incompatible
value raises `TypeError` at the result assertion. These inferred assertions are
mandatory regardless of safety. `speed=0` or `debug=3` on the caller or predicted
callee prevents establishing the inferred contract; explicit annotations are
unaffected. See [checked inferred results](type-inference.md#checked-inferred-results-within-a-compilation-unit).

### Safety and global names

Safety also selects how much of Python's global-name semantics compiled code
assumes, alongside the contract checks above:

| Safety | Builtin names (`len`, `ValueError`, ...) | Other names |
|---|---|---|
| 3 | Exact Python: the module's own binding, else the builtin's current value, so rebinding a builtin is visible | `NameError` at the read |
| 2, 1 | Builtins are assumed never to be rebound once installed: a module's cell holds a copy of the builtin until the module binds the name, and a read is one load | `NameError` at the read |
| 0 | as at 2 | Unchecked: an unbound read yields the undefined object, which raises `NameError` when used |

SBCL's `symbol-value` likewise omits its unbound check only at `(safety 0)`. The
builtins unit itself reads globals as at safety 3, because its protocols
install the builtins while it runs, replacing runtime-provided ones such as
`tuple`.

## Target class speed levels

The current implementation provides the explicit signed-64-bit level-three
subset and the earlier boxed fixed-layout classes. It does not implement the
full dynamic behavior of levels zero and one or the declared-field level-two
contract in this table.

| Capability | 0: dynamic | 1: optimized dynamic | 2: fixed layout | 3: native struct |
| --- | --- | --- | --- | --- |
| Ordinary Python attribute, descriptor and metaclass semantics | Required | Required | Restricted as below | Restricted as below |
| Add or delete instance attributes after construction | Allowed, subject to Python hooks and slots | Same as 0 | Forbidden | Forbidden |
| Change a field's value type | Allowed | Allowed | Allowed for untyped fields; declared contracts enforced | Forbidden outside its declared contract |
| Replace methods or mutate the class namespace | Allowed | Allowed | Forbidden after class creation | Forbidden after class creation |
| Instance storage | Python-compatible dictionary/slots | Same semantics, with guarded caches | Fixed offsets, boxed slots permitted | Fixed offsets, native typed storage |
| Method dispatch | Dynamic | Guarded optimization with fallback | Immutable virtual dispatch; proven calls direct | Exact receiver and direct native calls |
| Subclassing | Python semantics | Python semantics | Compatible level-2 bases/subclasses | Final class; no subclassing |

Levels zero and one must implement the Python data model, including class body
execution, attribute dictionaries, `__slots__`, descriptors, metaclasses,
inheritance/MRO, custom attribute hooks, class mutation and method replacement.
They have the same observable class semantics. Level zero minimizes compiler
optimization; level one can cache or specialize only when guards and invalidation
preserve those semantics. Failed speculative guards use a generic native
implementation of the operation. They do not run a source or bytecode evaluator.
Python compatibility here is a target requirement, not a claim of current parity.

Level two fixes the field set at class definition. Fields must be declared, and
the constructor may initialize them but cannot introduce undeclared fields.
Untyped fields hold arbitrary Mule values; typed fields have explicit contracts.
Every declared field must be initialized before construction returns. Later
assignment changes a value, not the field set. Deletion is forbidden. The class
namespace and method table are immutable after creation. Custom metaclasses,
attribute interception, descriptors that override instance field access,
instance method replacement and `__class__` reassignment are forbidden.
Initial level-two inheritance supports one level-two base with stable inherited
offsets and immutable virtual method entries. A dynamic base is not compatible.

Level three additionally requires a storage contract for every field. Initial
implementation requires explicit declarations; an initializer such as `0` does
not choose a machine width implicitly. The class is final and has no base other
than the language's implicit root. It has no attribute dictionary or per-instance
method table. Fixed scalar fields use their native representations. Reference
fields use statically declared reference slots and retain their ownership rules.
Field values remain mutable within their contracts. Sealing shape and types
does not make the object's values immutable.

An instance cannot escape level-three construction until every field is
initialized. Reading a field before initialization is an error. Prove the
construction rules statically where possible, and check unresolved construction
paths before publishing the instance. A failed constructor must clean up the
fields it actually initialized without exposing an incomplete native payload.

Public class annotations have normal metadata semantics at levels zero and one;
they do not fix storage or enforce field types. At levels two and three the
declared field contracts are an explicit Mule extension. Numeric function/local
contracts from imported Mule markers remain compiler declarations as documented
in the current compiler specification. Ordinary Python annotations are not
silently converted into native field contracts for dynamic classes.

## Native representation and operations

For the example, the payload is equivalent to a native struct with one `i64`
member. Field order, alignment and padding are fixed by the target data layout.
The current raw field implementation supports only signed `int64`. The target
is for `int8` through `int128` and `uint8` through `uint128` to select LLVM integers
of their declared widths. Float markers select their corresponding LLVM floating
representations when those backends are implemented; unsupported representations
must be rejected rather than silently stored as binary64.

An exact `Counter` receiver permits field access through a constant offset and
an LLVM load/store. `Counter.add` uses a native calling convention with the
receiver and an `i64` argument. It can inline into its caller. Numeric field
access does not allocate objects, manipulate reference counts, look up a name,
consult an attribute cache or call a generic runtime arithmetic helper.

Generic interfaces may require an owning allocation with runtime identity and
lifetime metadata. That metadata must not force scalar payloads to become boxed
slots. A nonescaping allocation may be removed or its fields promoted to SSA
values when identity and constructor effects are preserved. Reference fields
still require appropriate lifetime management; a native struct is not a license
to discard Python-visible ownership or aliasing behavior.

Fixed-width integer contracts do not wrap or widen into GMP automatically.
Out-of-range assignments and results violate the contract. Statically proven
violations are compilation errors. Checked operations detect overflow and report
`OverflowError`; checked category/range violations at assignments or argument
boundaries report `TypeError`. Under unchecked trusted compilation, violation is
outside the valid program contract. Python's ordinary `int` remains arbitrary
precision and can use GMP; its presence in a field is not a fixed-width promise.

Annotations on operands do not constrain otherwise unannotated intermediate
results. For example, `x: mule_types.int64` does not make `x * x` a bounded integer.
Select raw arithmetic only when the operation's result itself has a declared
fixed-width contract or analysis proves its complete mathematical range fits.
Otherwise preserve Python's arbitrary-precision intermediate through a compiled
generic path. A later bounded return annotation is not a contract on every
operation that produced the return value.

Exact class identity and callable identity must be established before selecting
native operations. Mutable module bindings retain their semantics: spelling a
global `Counter` or `count` is not sufficient proof. Load and validate a binding
at the relevant boundary or prove that it cannot change across the optimized
region. Finality prevents class mutation, not module rebinding.

## Safety levels

Safety checks enforce contracts, not permission to mutate sealed classes.
Adding attributes, deleting fields, replacing methods and changing field
contracts remain forbidden at every safety level for levels two and three.
Known violations are compilation errors at every safety level. Generic code
accessing a restricted object receives a normal runtime error for a forbidden
operation; there is no fallback to a dynamic object layout.

| Safety | Required checking |
| --- | --- |
| 0 | Trust declared internal values and omit optional contract checks. Unknown values entering a native representation through a generic interface must still be validated before storage or native entry. |
| 1 | Check arguments and field writes when not proven valid; check fixed-width arithmetic overflow and representation conversions. |
| 2 | Also check declared return/local contracts and initialization where static construction analysis cannot prove them. |
| 3 | Fully enforce declared contracts, including calls within trusted compiled code; retain complete error paths for unknown values. |

Safety zero retains declared container-element types during propagation without
scheduling optional element checks. This applies to indexing, iteration,
unpacking, and method results that carry an element contract. Independently
proven contradictions still cause the compiler to ignore the conflicting
annotation. SBCL likewise keeps the asserted type in `make-cast`
(`src/compiler/ir1util.lisp`) while `maybe-weaken-check`
(`src/compiler/checkgen.lisp`) removes the runtime checking type at safety zero.

A check proven redundant can disappear at every level, including three. The
compiler must use actual proof rather than the annotation alone to eliminate
a safety-three check. Loops over proven native integers must not execute a type
check per arithmetic operation. A check for an incoming unknown receiver or
argument belongs at the boundary where its native representation becomes valid.
Numeric conversion, generic lookup and error reporting can remain in adapters;
they must not be repeated in the proven hot path.

Lower safety inside a method does not weaken the owning class's field contract
for generic external writes. The class descriptor retains that contract and its
adapter enforces it. Checked errors must preserve ownership and existing object
state, and must not reinterpret an invalid boxed value as native bits.

The current implementation distinguishes safety zero from positive safety for
annotation assertions and native checked arithmetic; it does not yet implement
all of these tier differences. Native generic entry/field-write adapters validate
the physical integer representation even under safety zero. Raw add, subtract,
multiply and negation use LLVM overflow intrinsics at positive safety; unchecked
contracted operations may use `nsw` and require the promised range to hold.

## Debug levels

| Debug | Target behavior |
| --- | --- |
| 0 | No exception traceback landing blocks (`@mule_traceback_here`) or active-frame records, and no debug constraints on inlining or optimization. Potentially recursive calls retain depth checks; proven acyclic calls can omit them. |
| 1 | Python tracebacks on optimized code (default): emit `@mule_traceback_here` on cold error edges for frames that exist, retain depth checks for potentially recursive calls, omit active-frame records on functions that cannot reach external frame inspection, and allow inlining and return-contract inference (inlined or eliminated calls do not appear as separate frames). |
| 2 | Full CPython-identical call frames and tracebacks: preserve every Python call frame and active-frame record, and constrain inlining (`prevents_inlining`) and transformations that would alter call stack frames. |
| 3 | Everything in `debug=2` plus source stepping and declared local variable observability. |

Debug affects optimization and code size, but not the class's field layout or
allowed mutations.

Recursion protection does not depend on safety. SBCL handles control-stack
exhaustion independently of compiler policy in `src/runtime/interrupt.c:1625–1682`.
Mule retains its existing counter-based depth guard rather than adding SBCL's
guard-page machinery.

## Implementation and acceptance

Class policy, field declarations and owner identity now pass through the IR.
The frontend proves signed-64-bit constructor initialization with forward
control-flow must analysis. Representation selection can produce raw integer
locals/arguments/results, exact native receivers, constant-offset field accesses
and direct internal method calls. An entry guard checks predicted mutable class
bindings and selects the compiled generic body when they differ. Generic entry
adapters retain owned values, convert at the boundary and clean up on failure.

Raw entry selection remains deliberately limited. Bodies with unsupported
operations or missing proofs retain their compiled boxed entries. Recursive call
cycles retain ordinary recursion guards; repeated allocation sites in loops
retain the generic lifetime path until escape/lifetime analysis can establish a
safe raw alternative. Native allocation elimination remains LLVM's opportunity,
not a claim that the compiler already proves arbitrary object escape behavior.

Remaining work includes other raw field widths and references, general
representation-cost selection, fuller local-call transformations, level-two
declared layouts, the level-zero/one dynamic object protocols, the debug/safety
matrix and migration of existing Rust language-level bootstrap code into compiled
Mule Python. None of these gaps is a reason to implement a Rust language evaluator.

Acceptance requires behavioral tests for the policy matrix, including writes
from callers with different policies, construction failures, mutation and
subclass rejection, wrong categories/ranges, checked overflow, aliasing, and
global rebinding. Dynamic levels also require descriptor, mutation, dictionary,
slot and metaclass behavior tests.

For level three, inspect optimized IR and machine code for the class counter.
Its proven loop must contain native arithmetic and direct field access, or
equivalent code after scalar replacement. It must not contain generic attribute,
method-resolution, boxed integer or per-iteration scalar reference-count calls.
Benchmark equivalent workloads and escape patterns against optimized Rust and
C++ structs, with matching widths, overflow contracts and safety assumptions,
recording actual generated code and results. The requirement is
comparable generated operations and measured performance; no performance claim
follows merely from selecting `speed=3`.

## Builtin implementation traceback visibility

Compiled modules implementing Python builtin APIs may declare
`__mule_builtin_implementation__ = True` in their module prologue. The literal
boolean declaration is inherited by their function policies and remains an
ordinary private binding at runtime. It suppresses only their own Python
exception traceback entries; user callbacks retain their own policies and
frames. Module initialization and all language code still compile through the
normal stages. It does not enable bootstrap linking or weaken safety checks.

SBCL `src/compiler/debug-dump.lisp` emits compiled function/location metadata
under compiler debug policies, and `src/code/debug-int.lisp` reconstructs native
frames from it. SBCL normally exposes its compiled Lisp implementation frames.
Python builtin functions have no Python source frame, so Mule deliberately
separates native debug metadata from Python traceback visibility. The explicit
implementation declaration records that adaptation without recognizing library
names or filtering source filenames. Native active-frame diagnostics retain the
implementation frames; error edges omit only their Python traceback append.
Ordinary source modules emit traceback entries at `debug >= 1` and omit them at
`debug = 0`.
