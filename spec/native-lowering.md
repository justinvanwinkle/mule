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
| `src/compiler/ltn.lisp`, `operand-restriction-ok`, `template-args-ok`, `find-template-for-ltn-policy` (649–705) | Select the cheapest template whose operand restrictions, primitive-type requirements, and policy accept each known call (`ltn.py`). |
| `src/compiler/vop.lisp`, `vmdef.lisp` | Templates with operand restrictions, costs, and generators; VOPs over TNs with primitive types and storage classes (`vop.py`, `vmdef.py`). |
| `src/compiler/ir2tran.lisp`, `ir2-convert-block`, `ir2-convert-ref`, `ir2-convert-set`, `ir2-convert-template`, `ir2-convert-local-known-call` (1029–1042) | Convert each IR1 block into VOPs; references, assignments, branches, casts, and returns use fixed templates; calls use the selected template or the guarded full-call template (`ir2tran.py`). |
| `src/compiler/represent.lisp`, `select-tn-representation`, `select-representations` | Choose each TN's storage class from its primitive type by operand-restriction cost; natively planned bodies hold every value in a machine representation (`represent.py`). |
| `src/compiler/codegen.lisp`, `generate-code` | Invoke each VOP's generator in block order to emit the function (`codegen.py`, `vops.py`). |
| `src/compiler/x86-64/arith.lisp`, `+/signed=>integer` (949–975) | Emit machine addition and overflow checks, allocating a bigint only when needed. |

Mule reproduces these mechanisms in Python with LLVM as the required backend.
The specific native class adaptations and their reasons are recorded in
[optimization policies](optimization-policies.md#sbcl-implementation-correspondence).
Rust supplies low-level primitives; the existing language-level Rust runtime is
bootstrap debt. GMP supplies arbitrary-precision arithmetic. No additional
library is introduced.

## Analysis and representation facts

AST and IR1 nodes select visitor methods through their own `accept` methods.
Tree traversal, operand discovery, and control-flow edges are virtual
behaviors. Constant leaves retain concrete Mule literal nodes, so neither
conversion nor code generation classifies host Python values.

Value facts are polymorphic objects with union, widening, arithmetic, narrowing,
and containment behavior. They do not carry string type tags. Template
selection maps them to primitive types (`fixnum`, `boolean`, `none`,
`immortal`, `t`) and representation selection maps those to storage classes;
their visitor dispatch emits entry guards. These compiler objects are separate
from the native value representation described below.

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

`mule/compiler/vops.py` defines one template per known operation. Each
template names the known function it translates, restricts its operands to
storage classes, and carries the generator that emits its LLVM text; the
runtime helper templates it emits once per module (`helpers()`) cover integer
addition, subtraction, multiplication, floor division, remainder, shifts,
bitwise operations, unary operations, comparisons, truth testing, and
ownership as always-inline LLVM functions. Proven operands and bounded results
emit LLVM primitives directly without tag or overflow checks. Proven fixnum
operands with wider results use helpers without tag checks but retain overflow
recovery. Otherwise helpers test tags before decoding integers and executing
native instructions.

Every TN of an object body is either an owned descriptor (released on
overwrite and on every exit) or an immediate (a tagged fixnum, boolean, or
`None`, never released). A local is immediate when every value written into
it is immortal; a temporary is immediate when its derived type is. Owned
temporaries live in entry-block slots that the shared error path and the
handler-edge unwind blocks release.

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

## Raw int64, float64, and class entries

`represent.py` selects physical representations after semantic analysis,
following SBCL's `represent.lisp` separation of semantic type from storage
class. A native plan assigns every parameter, local, and value of a lambda a
machine representation (`signed64`, `float64`, `boolean`, `none`, an instance
payload, or a guarded class); the lambda is then generated a third time under that plan,
where template selection admits only the raw templates and storage-class
selection prefers machine storage. Native
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

`float` and `float64` contracts select the `double` storage class, as SBCL's
`double-float` declarations select `double-reg`. Float parameters are unboxed
once at the adapter (`mule_unbox_f64`, which rejects integers the way the
`float` contract does) and results are boxed at return (`mule_box_f64`).
Addition, subtraction, multiplication, true division, negation, and the ordered
comparisons use LLVM `fadd`/`fsub`/`fmul`/`fdiv`/`fneg`/`fcmp`; `fdiv` checks
for a zero divisor under positive safety and raises `ZeroDivisionError`. A mixed
operation with an `int64` operand converts the integer with `sitofp`, the same
rounding Python applies before a mixed operation. A mixed comparison does not
convert: Python compares the exact values, so the templates call an LLVM helper
that orders an `i64` against a `double` exactly. Floor division, modulo, power,
and narrower float widths (`float16`, `float32`, `float128`) keep the boxed
body: the runtime stores every float as binary64, and a narrower contract would
need its own rounding after each operation, as SBCL's `single-float` has.

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
local slot being immediately replaced. Conversion records the reuse local only
for an augmented assignment to a plain local, so the result's store follows
the operation immediately. Runtime
reference counts exclude aliases; an RHS that changes the slot cannot authorize
reuse through that slot. Same-object operands and other numeric representations
take normal paths. Results still demote to tagged integers when they fit.

GMP is dynamically linked. Clang links with section garbage collection and
strips debug sections inherited from Rust's standard-library archive. Symbols
remain available for native-code inspection. Both LLVM code and the Rust runtime
retain optimization level 3.

## Remaining work and verification

This stage does not specialize unannotated float signatures, store floats in
native class fields, mix machine
and object representations inside one body (a native plan is all or nothing,
with coercions only at the entry adapter), or perform SBCL's full local-call
and constraint transformations. Speculative global call returns remain unknown
when the general fallback could return another type. LLVM handles SSA
promotion, instruction optimization, and register allocation.

### Generated code volume

Unoptimized LLVM for the Benchmarks Game programs runs 100–200 instructions
per source line; nbody's `advance` (about 20 lines) is 4,804 instructions, of
which arithmetic is about 1%. `-O2` grows it to 9,012, with 612 retain/release
calls left. These items reduce it, each following an SBCL mechanism:

1. **Error checks (35% of `advance`).** Every fallible operation is followed by
   a null or status check and a branch. SBCL calls never return an error
   status: a check is a compare and branch to out-of-line code that
   `generate-error-code` (`x86-64/macros.lisp`) emits in the `:elsewhere`
   section, and the error unwinds non-locally, so callers pay nothing on the
   success path.
2. **Reference counting (19%).** Every local read retains (`mule.fast.local`)
   and every use releases. SBCL has no reference counts, but its lifetime
   analysis (`life.lisp`) supplies last uses, which lets reads of a definitely
   assigned local borrow, and constraint propagation (`constraint.lisp`)
   proves assignment, which removes the unbound check.
3. **Boxed floats and tag tests (14%).** `represent.lisp`
   (`select-tn-representation`, `emit-moves-and-coercions`) gives each TN a
   storage class by VOP costs, keeps derived `double-float` values unboxed and
   boxes only at coercions. Mule selects machine representations only for
   annotated `int64`/`bool`/native-class bodies, all or nothing.
4. **Frame enter/leave around compiled-builtin calls (15%).** SBCL allocates
   the frame in the prologue and detects stack exhaustion with a guard page;
   local call conversion (`locall.lisp`) skips the external entry. Mule wraps
   each call in `mule_enter_frame`/`mule_leave_frame` and refetches
   `mule_protocol_globals()` at each site.
5. **Per-type function clones.** SBCL compiles one body per function. Mule
   emits `.body`, `.spec.N`, and `.boxed` copies; nbody's `advance` `spec.0` is
   6% smaller than its `body`, so the clone doubles code for little gain.

Source-level IR1 tests cover type/range joins, widening, unbound locals, and
guarded target predictions. Native executable tests cover overflow, boolean identity,
function rebinding, argument side effects, errors, recursion, and ownership.
A machine-code test verifies that a bounded integer kernel emits native arithmetic
without calling operator dispatch helpers. The four-language benchmark measures
the resulting executable independently of compilation and verification.
