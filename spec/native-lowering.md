# Native implementation selection

Status: implemented for the limited subset in [Native compiler](native-compiler.md).
This is the first analysis and implementation-selection stage, not the complete
SBCL optimization pipeline.

## SBCL reference

The inspected source is `~/checkout/sbcl` at commit
`4a997dfc5029cddb4e7fa7f7393e66b427adcca7`, also used by the
[SBCL compiler reference](sbcl-compiler.md). These implementations guided Mule:

| SBCL source | Behavior carried into Mule |
| --- | --- |
| `src/compiler/ir1opt.lisp`, `lvar-type-using`, `%lvar-derived-type`, `lvar-conservative-type` (170–225) | Union producer types and use conservative facts where mutation can invalidate assumptions. |
| `src/compiler/ir1opt.lisp`, `accumulate-optimistic-arg-types`, `publish-optimistic-types`, `solve-optimistic-result-types` (3263–3448) | Propagate argument and result types across local calls, converge recursive facts, and publish only settled types. |
| `src/compiler/ltn.lisp`, `operand-restriction-ok`, `template-args-ok` (649–705) | Select implementation templates according to operand representations and constants. |
| `src/compiler/ir2tran.lisp`, `ir2-convert-local-known-call` (1029–1042) | Use an internal call convention with direct argument locations. |
| `src/compiler/x86-64/arith.lisp`, `+/signed=>integer` (949–975) | Emit machine addition and overflow checks, allocating a bigint only when needed. |

Mule reproduces these mechanisms in Python with LLVM as the required backend.
The specific native class adaptations and their reasons are recorded in
[optimization policies](optimization-policies.md#sbcl-implementation-correspondence).
Rust supplies low-level primitives; the existing language-level Rust runtime is
bootstrap debt. GMP supplies arbitrary-precision arithmetic. No additional
library is introduced.

## Analysis and representation facts

AST nodes and IR operations select visitor methods through their own `accept`
methods. Tree traversal, operand discovery, control-flow edges, and instruction
validation are virtual behaviors. IR constants retain concrete Mule literal
nodes, so neither lowering nor emission classifies host Python values.

Value facts are polymorphic objects with union, widening, arithmetic, narrowing,
and containment behavior. They do not carry string type tags. LLVM uses their
representation proofs to select native templates and their visitor dispatch to
emit entry guards. These compiler objects are separate from the native value
representation described below.

`mule/compiler/constraint.py` propagates facts over IR1 (`constraint.lisp`'s
role, adapted to a value-type lattice). It propagates
local value types and integer intervals through stores, loads, arithmetic, and
control-flow joins. Integer comparisons can constrain successor-edge ranges.
Facts settle through a worklist; widening removes growing loop bounds so analysis
terminates without publishing optimistic intermediate facts.

An integer is a proven fixnum only when its complete interval fits
`[-2**62, 2**62-1]` and it cannot be unbound. Booleans are immortal heap objects,
not tagged integers, even though arithmetic accepts them as numeric operands.
`None` is also an immortal object. Joins preserve the possibility of an unbound
local; its runtime error must remain observable.

Unannotated general function parameters and unknown call results remain unknown. Imported
numeric annotation assertions refine parameter, variable, and return facts.
Assertions preserve a proven narrower interval, while an unknown value acquires
the declared contract. Known contradictions are compilation errors; a checked
policy validates unknown values before LLVM relies on the asserted facts.
A predicted
global function target is recorded separately from its type and never proves
the result type of the call. Mutable global function bindings also exist in SBCL;
both compilers must distinguish proven local targets from dynamic calls.

Explicit global stores can prove a subsequently loaded callable within the
current function. Calls, potentially overloaded operations, and user truth hooks
invalidate global-slot facts. An already-loaded callable retains its identity
when argument evaluation rebinds its global name. Different targets at a join
lose their identity proof even if a speculative target hint remains available.

Interprocedural analysis propagates observed integer parameter ranges through
call sites, including recursion, and derives return summaries for proven local
calls. Summaries start empty and are published only after convergence; growing
recursive intervals are widened. Each function can have an observed-range entry
and a full fixnum entry, in addition to its unchanged general entry. These guarded
contexts supply parameter facts inside the body without annotations.

## Native operations

`mule/compiler/llvm_ops.py` supplies always-inline LLVM templates for integer
addition, subtraction, multiplication, floor division, remainder, shifts, bitwise
operations, unary operations, comparisons, truth testing, and ownership.
Proven operands and bounded results emit LLVM primitives directly without tag
or overflow checks. Proven fixnum operands with wider results use templates
without tag checks but retain overflow recovery. Otherwise templates test tags
before decoding integers and executing native instructions.

Arithmetic checks both machine overflow and the signed 63-bit representation
range. Overflow takes the Rust/GMP path, which promotes and subsequently demotes
integers as appropriate. Floor division and remainder adjust machine truncation
to Python's signed semantics. Shift counts are checked before LLVM shifts;
division by zero and unsupported cases take runtime paths with normal errors.
Non-integer values retain their dynamic protocols. Boolean bitwise operations
retain boolean results, and user comparison/truth callbacks retain their order.

Tagged integers, booleans, and `None` need no reference-count updates. Ownership
templates skip those values; heap values retain the Rust ownership protocol.
Unknown local loads still check for the unbound marker.

## Internal calls

Every function has a public callable wrapper and an internal native body. The
wrapper checks positional arity and acquires owned arguments. The body receives
the globals and positional values directly, owns them, and cleans them on every
normal/error exit.

When analysis predicts a target with matching arity, generated code checks the
already-evaluated callee's native entry and globals. A match selects a specialized
entry after checking argument representations/ranges, or calls the general native
body. Proven caller facts remove redundant argument checks. A mismatch calls
the actual callee through the runtime. Both routes use the same recursion guard.
Arguments still evaluate after the callee and before the guard, so an argument
that rebinds the global does not replace the already-loaded callable.

## Raw int64 entries and class protocols

`native.py` selects physical representations after semantic analysis, following
SBCL's `represent.lisp` separation of semantic type from storage class. Native
class declarations carry stable compiler layout identities, aligned contracts,
slot indices, and method function identities. Receiver facts select typed slots
through virtual methods. There is no host-class inspection or class-name type
dispatch. Full signed 64-bit fields use `i64`, not Mule's 63-bit immediate values.

The pass solves local representations and unannotated method result summaries
to a fixed point. An arithmetic result requires a direct fixed-width contract
or a range proof before it may lose arbitrary precision. Signed bitwise
operations are closed over the selected 64-bit operand representation. Native
methods and scalar callers use raw entries; generic interfaces supply coercions
at entry and exit. Safety-zero contract arithmetic uses LLVM `nsw`; positive
safety uses overflow intrinsics and reports `OverflowError` before a field write.
The boxed path remains available for unsupported signatures and operations.

Local method edges require an available raw body with no mutable-global
dependencies. Cyclic call components remain boxed until native stack checking
is implemented. Predicted class globals are loaded and checked at entry against
the compiler layout and module environment. Calls in the selected region cannot
write globals or invoke unknown code. Failed identity guards transfer the owned
arguments to the compiled boxed fallback without re-evaluating arguments.
This is a conservative implementation boundary, not a replacement for SBCL's
full effect analysis and local-call transformations.

Following SBCL's generated Lisp constructors and accessor forms in
`defstruct.lisp`, `native_protocol.py` lowers `stdlib/_native_class.py` with Mule's
normal parser and frontend. Its private primitive bindings select allocation,
raw field load/store, boxing/unboxing, and a known initializer entry. Constructor
signatures are expanded from the actual initializer signature. Generic ABI
callbacks adapt borrowed arguments to these compiled owning functions. Rust
records callback addresses in the existing bootstrap class descriptor and
forwards construction and indexed field operations to them; it does not perform
new native-class initialization or field conversion algorithms.

An instance's owned allocation keeps borrowed payload pointers alive for the
native region. Aliased local pointers share the same storage; method field
operations do not retain or release scalars or receivers per iteration.
Constructor sites in loops and instance returns retain the general path until
escape and lifetime analysis can establish a safe ownership transformation.
Cleanup releases each allocation on normal and error exits. Allocation currently
uses a Rust primitive; inline allocation, stack allocation and scalar replacement
of the owning box still require SBCL's corresponding analyses and primitives.
LLVM can inline the raw methods and eliminate or unroll loops independently of
that remaining allocation work.

## Bigint storage and linking

GMP inputs are borrowed while owned runtime operands keep them alive. Fresh
results move into Mule objects without copying their limbs. Multiplication by
a machine integer uses `mpz_mul_si` without constructing a GMP right operand.
These operations follow GMP's [arithmetic interface](https://gmplib.org/manual/Integer-Arithmetic)
and [input/output aliasing conventions](https://gmplib.org/manual/Parameter-Conventions).

Add/subtract/multiply can reuse a bigint when its only reference is the consumed
temporary, or when its two references are the consumed operand and the exact
local slot being immediately replaced. The IR verifier requires that local reuse
be followed immediately by a store of the result into the same local. Runtime
reference counts exclude aliases; an RHS that changes the slot cannot authorize
reuse through that slot. Same-object operands and other numeric representations
take normal paths. Results still demote to tagged integers when they fit.

GMP is dynamically linked. Clang links with section garbage collection and
strips debug sections inherited from Rust's standard-library archive. Symbols
remain available for native-code inspection. Both LLVM code and the Rust runtime
retain optimization level 3.

## Remaining work and verification

This stage does not specialize arbitrary object/float signatures, keep every numeric
local unboxed across blocks, perform SBCL's full local-call and constraint
transformations, or provide a separate implementation-level IR. Speculative global
call returns remain unknown when the general fallback could return another type.
LLVM handles SSA promotion, instruction optimization, and register allocation.

Hand-built IR tests cover type/range joins, widening, unbound locals, and guarded
target predictions. Native executable tests cover overflow, boolean identity,
function rebinding, argument side effects, errors, recursion, and ownership.
A machine-code test verifies that a bounded integer kernel emits native arithmetic
without calling operator dispatch helpers. The four-language benchmark measures
the resulting executable independently of compilation and verification.
