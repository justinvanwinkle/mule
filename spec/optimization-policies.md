# Optimization policies

Status: target contract for implementation, not a description of capabilities
already available. The user requires progressively restricted class semantics,
ending in native structs. The level assignments and defaults below define the
proposed API. See [current behavior](native-compiler.md#imported-annotation-contracts)
for the implemented decorator and [current classes](native-compiler.md#fixed-layout-classes)
for the existing boxed, fixed-layout runtime.

## Policy axes and scope

```python
import mule

@mule.optimize(speed=3, safety=3, debug=0)
class Counter:
    value: mule.int32

    def __init__(self):
        self.value = 0

    def add(self, amount: mule.int32):
        self.value += amount
```

This is target syntax; class field declarations are not implemented yet.

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
class semantics. Restrictions require an explicit class policy. The explicit
`optimize()` directive retains its current defaults of `speed=3`, `safety=0`,
`debug=1`; checked native classes use `safety=3` explicitly. This class-default
rule requires a compiler change; today's default policy remains `3, 0, 1`.

## Class speed levels

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

For the example, the payload is equivalent to a native struct with one `i32`
member. Field order, alignment and padding are fixed by the target data layout.
`int8` through `int128` and `uint8` through `uint128` select LLVM integers of
their declared widths. Float markers select their corresponding LLVM floating
representations when those backends are implemented; unsupported representations
must be rejected rather than silently stored as binary64.

An exact `Counter` receiver permits field access through a constant offset and
an LLVM load/store. `Counter.add` uses a native calling convention with the
receiver and an `i32` argument. It can inline into its caller. Numeric field
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
annotation assertions; it does not yet implement all of these tier differences.

## Debug levels

| Debug | Target behavior |
| --- | --- |
| 0 | Minimum debug metadata and no additional debug constraints on optimization. |
| 1 | Source positions and function identities sufficient for basic diagnostics. |
| 2 | Debug information for source stepping and recoverable local values where practical; transformations may lose some locals. |
| 3 | Preserve source stepping and declared local observability; constrain inlining and transformations that prevent it. |

Debug affects optimization and code size, but not the class's field layout or
allowed mutations. Current `debug=3` only prevents body inlining; frames, source
tracebacks and debug metadata still require implementation.

## Implementation and acceptance

The current runtime is closest to the shape restrictions of level two, but it
does not implement these level selections. All current class fields are generic
object slots; field annotations and native struct lowering are missing. Merely
adding annotations to method parameters does not complete level three.

Implement the contract in this order:

1. Carry a persistent class policy and field declarations through AST conversion
   and IR; distinguish class restrictions from method optimization options.
2. Implement level-three field storage and construction validation. Propagate
   exact native receiver facts and fixed-width values through fields, locals,
   calls and returns, with generic adapters only at actual boundaries.
3. Lower proven fields and methods directly to LLVM and allow inlining and
   allocation elimination. Keep the current boxed class path as an explicitly
   limited implementation until it meets level-two behavior.
4. Implement the level-zero/one dynamic object protocols and level-one guarded
   specialization. Do not claim Python-compatible dynamic classes while these
   protocols are missing.

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
