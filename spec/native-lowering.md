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
| `src/compiler/ir1util.lisp`, `find-constant` (3068–3106) | Coalesce equal immutable constant literals into shared `Constant` leaves in `*ir1-namespace*` (`ir1tran.py`). |
| `src/compiler/seqtran.lisp`, `string=*` (1271–1388); `src/compiler/equality-constraints.lisp` (1286–1303) | Fold disjoint-length string equality, narrow `TextType` across `==`/`!=` branch edges, and inline short Latin-1 word comparisons (`typetran.py`, `constraint.py`, `vops.py`). |
| `src/compiler/x86-64/arith.lisp`, `+/signed=>integer` (949–975) | Emit machine addition and overflow checks, allocating a bigint only when needed. |

Mule reproduces these mechanisms in Python with LLVM as the required backend.
The specific native class adaptations and their reasons are recorded in
[optimization policies](optimization-policies.md#sbcl-implementation-correspondence).
Rust supplies low-level primitives; the existing language-level Rust runtime is
bootstrap debt. GMP supplies arbitrary-precision arithmetic. No additional
library is introduced.

LTN derives a shared value's primitive type by joining every producer's type
under the current entry context. Multiple producers do not by themselves force
the unknown descriptor representation. This follows SBCL's
`src/compiler/ltn.lisp`, `annotate-ordinary-lvar`, which selects the primitive
type from the lvar's derived type. A common float or fixnum representation can
therefore survive a return or control-flow merge; genuinely heterogeneous
alternatives retain the existing object representation.

SBCL caches this type on the lvar in `src/compiler/node.lisp`,
`lvar-derived-type`, and invalidates it when the IR1 value changes. Mule's
producer facts vary by entry context, so LTN keeps the joined type on its
temporary IR2 lvar annotation instead. IR2 conversion reuses it only when its
LTN token matches the annotation's token. The caller must pass the token from
the preceding LTN invocation and keep the graph and context facts unchanged
between those phases. Older annotations and native-plan selections that did
not compute a join use the current facts instead. The annotation is cleared
after conversion.

## Proven floats in object entries

LTN distinguishes a proven, bound Python float from an unknown descriptor and
selects binary64 addition, subtraction, multiplication, and division templates
for pairs of these values, including augmented assignment. This follows
`src/compiler/x86-64/float.lisp`'s `double-float-op` templates and its `frob`
definitions for `+`, `-`, `*`, and `/`, and `src/compiler/ltn.lisp`'s
`find-template-for-ltn-policy`.

The Python adaptation keeps observable operands and results as owned descriptors:
Python exposes float identity and aliases even in an otherwise numeric function.
Each template extracts the physical binary64 payload through the existing
checked primitive, emits LLVM arithmetic without fast-math flags, and boxes the
result at a descriptor boundary. Division checks both signs of zero, including
at safety zero, and follows the active exception handler with ownership cleanup.
Unknown values and Python `float` annotations (which also admit integers) remain
eligible for normal protocol dispatch rather than assuming an exact float.
Subclass arithmetic in that pre-existing generic path has a separate known
compatibility gap; these templates do not extend its coverage.

Float literals in object entries are cached as unit-owned constants, following
`src/compiler/ir2tran.lisp`'s `constant-tn` and `emit-constant`: repeated references
load an existing constant rather than allocate a float on every execution of a
loop. The cache key includes all binary64 bits, so positive and negative zero
remain distinct. Each use receives an owned reference; unit shutdown releases
the constant root after cached tuples. Unlike SBCL's image constant space, Mule
initializes these native object roots on first use and releases them explicitly,
as it already does for tuple constants. Python permits sharing immutable float
constants; observable arithmetic results still receive result boxes. This removes
literal allocation without unboxing observable locals or changing float aliases.

For an augmented assignment immediately stored back into its owned local, the
boxed float template may reuse the old physical float box. The existing IR1/IR2
`reuse_local` proof identifies the two owning references (local and operand);
the physical primitive additionally checks that the candidate is an ordinary
float with exactly two references, or an exclusive operand moved out of its
local by the existing last-read analysis. Extra aliases, subclasses, and nonlocal
assignments allocate a fresh box. Extraction and zero-divisor checks precede
mutation, so a failed operation leaves the local unchanged.

This differs from SBCL's `src/compiler/x86-64/float.lisp` `double-float-op` and
`move-from-double` VOPs: SBCL normally keeps arithmetic in non-descriptor registers and
allocates boxes when crossing a descriptor boundary. Mule's object entries still
keep observable Python locals boxed. Mule's explicit owned-reference model
permits a proof of exclusive ownership that SBCL's tracing collector does not
provide through reference counts; when that proof holds, reusing a physical box
cannot change an observable alias. The Rust
primitive only validates physical ownership, stores a binary64 payload, and
transfers the consumed operand reference into the result on reuse. Otherwise it
allocates the result and releases that operand. This avoids an atomic retain
followed immediately by a release of the same box; alias counts and the local's
separate ownership remain intact. Arithmetic and specialization remain in the compiler.
The optimization saves allocation, without relaxing arithmetic checks or relying
on annotations to discard observable aliases.

Representation selection keeps numeric-only float intermediates and locals in
the `double` storage class when their operand and copy costs favor it. This follows
`src/compiler/represent.lisp`'s `add-representation-costs` and
`select-tn-representation`, followed by `emit-moves-and-coercions`. Mule inserts
explicit descriptor/double coercion VOPs after internal call conventions settle.
It preserves local-read move/borrow metadata, assignment overwrite semantics,
and the original exception region, then rebuilds operand indices for cleanup.

Unspecified float operand restrictions still require descriptors at object
boundaries. Identity demand propagates backward through every copy writer,
including mixed-type destinations and mutable-local redefinitions. Numeric-only
copies can independently extract a double without replacing an observable object.
Deletion and possibly unbound reads retain descriptor storage. An object-returned
accumulator still remains boxed because this analysis does not split local live
ranges. Flow-proven fixnum coercions are described below. Other general coercion
VOPs remain future work. Arithmetic order is preserved. LLVM receives separate
operations without fast-math or reassociation flags.
The constant VOP explicitly admits machine scalar results. A float literal used
only by these arithmetic templates therefore becomes an exact LLVM double
constant, as SBCL's float VOPs load `fp-immediate` constants. Literal uses that
cross object boundaries still load the pooled object and preserve its identity.

## Proven fixnum locals in object entries

Representation selection retains `FIXNUM` for locals whose writers all prove
exact integers in `[-2**62, 2**62 - 1]`. It costs signed machine storage and
tagged object storage per TN, including copy edges, without requiring a
whole-function `NativePlan`. Signed locals use `i64` slots across blocks.
Explicit coercion VOPs decode and restore tags at numeric/object boundaries.
These operations do not allocate integer boxes. They follow SBCL's
`select-tn-representation`, `coerce-vop-operands`, and
`emit-moves-and-coercions` stages.

Ordinary-entry signed arithmetic requires exact operand and result proofs.
Operations with potentially wider results retain Python's arbitrary-precision
path. Boolean and mixed-type joins do not qualify as exact fixnums. Locals
with unbound checks, deletion, or pointer-address closure access retain tagged
slots. Generator and coroutine local slots remain tagged because frame inspection
can observe them before assignment without an explicit local read. Arithmetic
temporaries in those bodies may still use signed storage. Coercions preserve
local read metadata, overwrite order, and exception cleanup.

## General fixed-slot access (Section 6J)

SBCL `src/code/defstruct.lisp`, `slot-access-transform`, obtains the physical
slot index and declared type from the structure description, then emits
`%instance-ref` or `%instance-set`. Mule's corresponding `ClassFacts` and
`InstanceType` layout facts come from class construction, not the spelling of
an attribute or protocol function. They retain field order and a known
single-inheritance prefix, separately from inferred field value types.
Class metadata does not prove a constructor's result: a custom `__new__` can
return a different class. Constructor result inference retains its separate
proof, while guarded receiver entries can still use the declaring class layout.
Compiled classes with fixed layouts also retain that metadata when their
constructor has a bootstrap type target.
Own-field value predictions still require the existing constructor proof.
Custom allocators can initialize fields through paths that ordinary constructor
propagation does not cover. Their layout remains usable, but incomplete stores
must not establish a value type. Proven inherited field predictions remain
available independently.

The existing inferred field-value assertions remain in force. A descriptor
replacement that returns a value outside a predicted scalar range can still
raise `TypeError` instead of selecting a general arithmetic path. A probe that
replaced a fixnum-valued field with a bigint-valued property observed this
limitation. Section 6J preserves descriptor lookup and ownership, but does not
implement deoptimization of those pre-existing value assertions.

Python permits descriptor replacement and class mutation after compilation.
An ordinary instance type therefore does not authorize an unconditional load
from a predicted field index. The existing attribute cache validates its
class epoch and field encoding before constant-index access. Cache misses and
unset fields retain ordinary lookup and exception behavior. Layout metadata
describes physical order independently of inferred field value types. Unknown
and multiple-base layouts retain dynamic access.

Method exports condition their result prediction on the owning receiver class.
A specialized entry contributes only when every other parameter still accepts
the complete general-entry domain. The ordinary callable summary remains based
on its general entry. This retains useful field-derived method predictions
without assuming that an unknown receiver has the declaring class or exporting
a result valid only for narrowed ordinary arguments.

Known-function metadata also declares argument domains for exact-receiver
compiled protocol entries. This follows the typed `gethash` and `%instance-ref`
declarations in SBCL's `src/compiler/fndb.lisp` and
`src/compiler/generic/vm-fndb.lisp`. Preparation resolves those semantic domains
through the defining class target's independent `layout_type`, then requests a
separate local-call context. Missing or ambiguous owner metadata disables that
specialization. The general body remains valid for unknown callers.

The defining unit exports the requested descriptor body as `FUNCTION.protocol`.
It retains that explicit context even without a local call or further
optimization. The imported function interface carries its resolved entry
domains, so a protocol wrapper can call the stable symbol without reconstructing
the owner's private variants. Only wrappers whose registered contract supplies
the caller proof use it. Ordinary external binding still calls the general
body. The exported protocol body preserves borrowed arguments and object return
conventions rather than introducing a native adapter or a forwarding algorithm.
Layouts do not imply constructor return types, immutable attributes, or missing
field-value proofs.

Unlike SBCL's tracing collector, Mule must preserve owned references when a
field is overwritten or read. A borrowed read additionally needs a rooted owner
and a region that cannot replace its slot or invoke arbitrary Python code.
Neither an attribute's spelling nor an inferred element type establishes that
lifetime proof. The current borrow pass handles a temporary with one same-block
consumer, excluding native plans. Intervening local overwrites also require
destruction-safe prior values. Broader alias and loop lifetimes retain ownership.
Function and class destruction can invoke weak-reference callbacks even when
ordinary operations on those values do not run user code. The slot borrow pass
uses the inert-value destruction proof for overwritten bindings, including dead
stores that still release the old binding.
An eligible physical storage read or length operation can borrow its receiver
from a fixed slot through the same no-callback proof. Mutation and Python method
consumers do not acquire that permission merely from their argument types.

A guarded borrowed read produces a nullable owned descriptor as well as its
borrowed result. On the direct hit, that owner is null. On fallback, it owns the
ordinary lookup result, which need not belong to the receiver. Separate LLVM
values preserve their distinct ownership identities. An explicit end VOP uses
both the owner and receiver root after the consumer, so packing, expiration and
error cleanup observe their actual live ranges. This avoids field retains on
hits without leaking or prematurely releasing descriptor results on misses.

Compiler-private `_instance_field` and `_set_instance_field` operations have
different semantics from Python attribute access. They access the actual
reference-slot map directly, without descriptors, attribute hooks, or the
instance dictionary. Their runtime fallback shares the existing physical
`instance_field` / `set_instance_field` implementation. An owned read retains
the value. A write retains the replacement before storing it and releasing the
previous value. Missing and unset fields raise `AttributeError`.

An IR1 `deftransform` canonicalizes a proven constant field name into
`PhysicalFieldInfo`. It preserves nonconstant name evaluation and unbound
checks. LTN selects the physical access plan before ordinary IR2 conversion;
it does not remove name operands or rewrite access templates in a VOP post-pass.
An unconditional constant-index operation requires an unmodified parameter of
a runtime-guarded entry with matching reference-layout metadata. An inferred
`InstanceType` from a mutable field is only a prediction, not that proof.
Predicted layouts receive an actual reference-layout guard and a physical-name
fallback. Other receivers probe a per-site attribute cache keyed by class
layout and attribute epoch, then read or write the cached slot index inline.
A miss, an unset slot, or a receiver with an instance dictionary calls the
UTF-8-name runtime ABI (`mule_instance_field_name_cached` and
`mule_set_instance_field_name_cached`) without constructing a text object for
the constant name; that call records the slot for the receiver's layout or
raises. The compiled dict and set table code, whose receiver is any table
class, therefore pays the by-name lookup once per class per site rather than
on every access. Physical-call entry selection retains
actual argument guards even when predicted dispatch facts match.
IR1 retains useful argument narrowing through known calls even when their
general return remains unknown. Call planning preserves guarded layout
specialization instead of selecting a less-specialized scalar body.
Native payloads, incomplete slot declarations, private-name layout expansion,
nonunique class bindings, and unknown class construction retain the fallback.
Builtin storage domains are scoped to compiled protocol definitions, not user
classes that happen to share a builtin spelling.

A proven physical read uses the same rooted, callback-free borrowing region as
ordinary slots. It needs only an explicit receiver-root end operand: there is
no successful descriptor fallback and no nullable fallback owner to release.
The unset arm reports the original null observation without rereading the slot.
Owned reads use the selected representation's ownership category: immediate
values need no retain, and proven heap values use heap-only retention.
Rooted physical fields also supply container-element borrowing. The local owner
remains live through the consumer, including when its receiver read was the
last use. Element borrowing still rejects callbacks or owner mutation inside
the protected region.
An array borrow may end at a physical storage write or append. These primitives
finish array access before releasing an overwritten element or a failed moved
append value. The compiler does not dereference that array after the callback.
Unrelated assignments that can destroy an old binding remain barriers, even
when the assigned value is otherwise unused.
Dictionary and set algorithms use these explicit physical operations for their
private table fields. Their stores evaluate the RHS first and delete their
single-assignment staging locals immediately after the write. Copied-list
inference follows a single-read forwarding alias without discarding that
lifetime-ending deletion.

## Closure and value-cell slot lowering

SBCL's `src/compiler/envanal.lisp`, `compute-closure` and
`determine-lambda-var-and-nlx-extent`, distinguish copied captures and shared
value cells. `src/compiler/ir2tran.lisp:90-117` selects cell-slot operations;
`src/compiler/x86-64/cell.lisp:459-494` emits physical closure loads and cell
stores. Mule retains its environment analysis and lowers these operations as
explicit IR2 closure-reference and value-cell VOPs.

Python exposes shared cells through `__closure__` and `cell_contents`. The
authoritative cell remains the existing compiled Python object, with `bound`
and `value` fields. Unlike SBCL's fixed cell layout, these Python class fields
permit descriptor and class mutation. Value-cell VOPs therefore use the
existing layout/attribute-epoch guards before direct field loads and stores.
Cache misses execute ordinary attribute access. No scalar mirror or separate
cell storage is introduced. Reads preserve truth conversion of `bound`, and
unbound errors remain compiled Python code. Stores replace `value`, finish
old-value destruction, and only then access `bound`, preserving reentrant
mutation. The hot field operations require no cell-protocol call or associated
recursion-depth and traceback bookkeeping.

Object closure entries reside in an immutable tuple. A low-level primitive exposes
its borrowed entry-storage address once at function entry; constant-index LLVM
loads acquire the captured references. This avoids both a dependency on Rust
`Vec` field order and the general argument-binding accessor.

## Local allocation extent

The existing `escape.py` analysis also describes allocation results, aliases,
and implicit closure captures. An unknown use remains escaping. The analysis
distinguishes semantic indirection from the need for a heap cell.

SBCL `src/compiler/envanal.lisp`, `determine-lambda-var-and-nlx-extent` and
`analyze-escaping-closure-environments`, propagate environment lifetimes.
`src/compiler/ir2tran.lisp`, `closure-initial-value` and `ir2-convert-enclose`,
consume those decisions. Local indirect captures can reference the defining
activation's frame instead of an explicit heap cell. Mule retains this
producer/consumer distinction using an internal LLVM environment ABI.

An eligible local function has only direct uses and a binding whose possible
values share one proven code identity and capture locations. Stable aliases
retain raw-pointer unbound checks, so conditional
alias assignment and sibling calls before initialization still raise correctly.
Its environment belongs to that particular defining activation.
Copied captures occupy owned slots. Shared mutable captures reference their home
slots only if every retaining environment has local extent. An escaping sibling
or transitive closure requires the existing authoritative heap cell instead.
These raw slot addresses must never enter Python object protocols.

Function identity, metadata, defaults, decorators, suspension, and indirect calls
can require an ordinary function object. Such uses retain the existing path.
Generator and coroutine owning activations also retain their existing storage.
Local entries preserve frame publication, recursion accounting, traceback
metadata, contract checks, and local-versus-free-variable unbound errors.
Supported frame records expose parent, code, and globals, not local bindings.
Tracebacks do not retain local values. This change preserves existing function-error
cleanup rather than adding CPython's traceback-local lifetimes.

Fixed tuple/list allocations instead become scalar locals when every use,
alias, and lifetime is representable. The consumer supports constant-index
reads, bounded runtime-index reads, fixed-index list replacement, length,
truthiness (`bool`, `not`, and conditional branches), disjoint-type identity
comparisons (`is` and `is not` against `None`, `bool`, `int`, `float`, `str`,
`bytes`, `bytearray`, `dict`, or `range`), reflexive-equality containment
(`in` and `not in` for arity 1–8), structural unpack, and local aliases.
Runtime projections require a builtin
fixnum index whose complete inferred range lies within the aggregate's positive
and negative index bounds. Negative indices normalize before selecting an
existing scalar child. The projection preserves the child's object identity and
acquires its own reference before the aggregate's final owner releases the
scalar slots. Unproved indices and dynamic replacement retain ordinary container
allocation and indexing behavior.

Aliasing families may close within one IR2 block or span multiple blocks
(`if/else` joins, conditional expression temporaries, multi-alias lifetimes,
explicit `del` across branches, and loop-carried state) when forward dataflow
proves consistent incoming live owners at every control-flow join and no local
exception handlers intervene. Element evaluation order and
fixed-index list replacement remain observable. Deleting or replacing the last
aggregate alias releases its children at the original destruction point. When
all children are inert (`int`, `float`, `bool`, `None`, `str`, `bytes`), the
final read of each scalar child moves ownership directly without extra
retain/release traffic, and unboxed representations (`DOUBLE_REG`) apply to
scalar child slots whose writers all produce unboxed values. Structural
mutation, unknown calls, and observable aggregate identity retain allocation
unless a separate proof supports them. Repeated allocation sites must not reuse
storage while an earlier logical object remains live.

For potentially observable children, a raising operation requires only unhidden
named owners and no local exception handler. Every such operation must share
the same last live owner in the backend's cleanup order. Scalar child slots
occupy that owner's cleanup position, in reverse element order. Normal last-owner
destruction clears the slots earlier. Temporary owners across fallible
operations, changing cleanup owners, and projections with pending
element-contract checks retain the ordinary allocation path. Child releases for
materialized sequences also follow reverse order, which weak-reference
callbacks can observe.

SBCL `find-lvar-dynamic-extents` starts from dynamic-extent declarations and
trusted downward-function-argument information, then proves compatible uses and
allocator eligibility. Mule's automatic Python escape proofs replace those
roots, extending its existing automatic borrowed-argument analysis. This is an
explicit adaptation, not a claim that SBCL automatically stack-allocates every
nonescaping aggregate. Scalar replacement avoids introducing a second runtime
container layout. Existing tuple projection and unpacked-return optimizations
remain authoritative where they already apply.

SBCL's `src/compiler/srctran.lisp`, the `values-list` optimizer, removes an
intermediate list by forwarding its constructor operands to a compatible
consumer. Mule extends its existing scalar-slot consumer in the same producer/
consumer separation. Unlike a materialized SBCL stack vector, the replaced Mule
aggregate has no container header or address. A bounded runtime projection must
therefore select from the scalar children rather than call a container getter.
This keeps the existing representation and lifetime proof authoritative instead
of adding another runtime layout or escape analysis.

### Repeated source-function lowering

SBCL `src/compiler/ir1tran.lisp`, `ir1-convert-local-combination`, retains the
existing functional identity except when its explicit inline-conversion rules
require another body. Mule's range lowering can visit the same Python function
AST twice, once for each iteration path. Those visits share one Lambda code
identity instead of preventing local-extent proofs with duplicate code bodies.

The frontend reuses code only for the exact same AST node in the same lexical
conversion context. The context includes its parent converter, class ownership,
defining class, policy, qualified name, body kind, and annotation-scope mode.
Active compiler bindings also form part of the conversion context. Reuse is
disabled when an ancestor has an active inline-comprehension scope, whose
captured binding identities can change between visits.
The memo retains identity-keyed context objects until all late cleanup conversion
finishes, then releases them with the other frontend conversion state.
Different source definitions and different effective lexical contexts remain
distinct. This is a Python source-lowering adaptation, not arbitrary function
deduplication or source-text equivalence.

Decorator evaluation, defaults, function creation, and closure capture remain at
each runtime definition site. Observable function instances remain distinct even
when their code identity is shared. Escape analysis can accept repeated binding
writes only when every possible value has that same proven code identity or a
stable alias to it. Raw local environments must continue to reference the same
home slots or immutable captures at every such site.
Each function-creation node contributes its own capture-containment edges to
escape analysis. Dynamic raw-binding checks establish initialization even when
alternative creation sites do not dominate each other's calls. Lambda-expression
lowering forwards the original expression's identity rather than the identity of
its synthesized function definition. Generic helper definitions without such an
origin remain distinct.

## Physical sequence storage operations

SBCL's `src/code/target-hash-table.lisp`, `define-gethash/flat` and
`define-ht-getter`, implement probes in Lisp over typed vectors. Its
`src/compiler/array-tran.lisp:2480-2525` lowers typed accessors to array
operations, and `src/compiler/x86-64/array.lisp:297-365` selects physical
data-vector readers and writers. Mule keeps dictionary algorithms in compiled
Python and lowers the general `_storage_read`, `_storage_write`, and
`_storage_length` primitives through LLVM storage operations.

SBCL's `src/compiler/generic/objdef.lisp:129-136` defines the native vector
length and data layout used by those VOPs. Mule likewise requires a stable
physical buffer layout, not an opaque accessor for every element. Rust `Vec`
field order cannot serve as that ABI. List and tuple object storage therefore
uses an explicit C-layout owned buffer with data, length, and capacity words.
The header owns the allocation; it does not mirror a second stored `Vec`.
Rust's allocation facilities still provide buffer growth and deallocation.

This adapts SBCL's inline vector data to Python's growable list storage: the
header points to a separately allocated buffer whose address changes on growth.
The compiler validates the physical object tag and directly loads the header
fields before checking bounds and accessing a slot. It never retains a raw
buffer pointer across calls that can resize storage, safepoints, or suspension.
Rust layout tests verify the header offsets used by the compiler.

Unsupported storage or indices use the existing checked primitive and preserve
its errors. Writes retain the replacement, publish it, perform GC tracking, and
only then release the old element. They do not reuse the old buffer pointer
after destruction can invoke Python. The dictionary algorithm remains Python;
Rust supplies physical memory ownership and the GC barrier only.

Current dictionary field inference does not establish exact-list field proofs.
The guarded physical lowering must not treat these fields as statically proven
lists or remove bounds checks without an independent proof. When `protocol-getitem`
is statically proven to index an exact `list` or exact `tuple` with a fixnum
index (`list_borrow`, `list_owned`, `tuple_borrow`, or `tuple_owned`), it emits
the physical buffer length check (or uses `TupleType.exact_length()` when known)
and element load inline in LLVM IR (skipping negative-index wrapping when the
index interval is proven non-negative) and calls `@mule_list_getitem_borrow` /
`@mule_list_getitem` or `@mule_getitem_borrow` / `@mule_getitem` only on the
cold out-of-bounds slow path. When the loaded element type is a proven fixnum
or immortal (`is_fixnum` or `is_immortal`), `protocol-getitem`,
`primitive-_storage_read`, and `unpack-ref` omit `@mule.fast.retain`. Making
storage operations visible is an implementation capability; speedups require
separate measurement.

## Fixed-length sequence unpacking

SBCL `src/compiler/seqtran.lisp` transforms `elt` and `%setelt` into storage-specific
operations for proven arrays or lists. Mule's existing structural-unpack VOP
likewise guards a fixed-size exact tuple before using physical slots. It now
also guards an exact builtin list of the required length and creates an owned
tuple snapshot through the existing physical storage primitive. Snapshotting
before any target assignment preserves Python's prefetch order if a target's
setter mutates the source list. Wrong lengths and
custom iterables use the compiled Python unpack algorithm. Tuple/list subclasses
also use that algorithm, so overriding `__iter__` remains observable. The Rust
addition only checks physical kind, subtype storage, and length; it implements
neither iteration nor language-level unpacking errors.

### Tuple delivery and dead local assignments

IR1 display destructuring follows the capture temporary's references and
`Lvar` destinations, without inspecting assignment targets. Element temporaries
preserve RHS evaluation order, target effects, partial assignments, and error
cleanup. This also permits global, attribute, subscript, and nested targets.

`callconv.TupleReturnConvention` selects fixed tuple return locations from
`TupleType` facts on `Return.result` producers. The existing output-slot ABI
supports arities 1 through 8. Direct construction can avoid tuple allocation;
returns through aliases extract owned children at the return boundary instead.
Ordinary object entries preserve tuple identity. IR2 owns the delivery plan and
connects captures and projections through definitions and uses, not instruction
offsets. Single-block, nonoverlapping regions can share the caller's output
buffer. Other regions retain the ordinary object path.

Unlike SBCL's tracing collection, each Mule output slot owns a child reference.
A projection transfers that owner. Error edges release the remaining slots,
including when an intervening assignment target raises. Independently live tuple
aliases retain their own ownership. `ltn-analyze-return`, `ltn-analyze-mv-bind`,
`ir2-convert-return`, and `ir2-convert-mv-bind` in SBCL's `ltn.lisp` and
`ir2tran.lisp` provide the stage boundaries for this adaptation.

Backward liveness in `_mark_last_local_reads` marks dead local assignments at
each instruction, independently of the variable's other writes. It includes
exception successors and preserves captures, borrow roots, and inspectable
suspended locals. A destruction-specific proof permits only inert exact values
and immutable tuples of inert children. Operation purity alone is insufficient:
classes can expose weak-reference callbacks, and mutable containers can acquire
observable children through aliases. A dead assignment releases its previous
binding at the original assignment point. Its input can borrow from a stable
owner without creating a retained copy. SBCL's corresponding backward analysis
is in `src/compiler/life.lisp`, not `lifetime.lisp`.

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
`None` is also an immortal object. Empty and single-character Latin-1 strings
are static immortal singletons (`is_immortal`). Multi-character string literals
are pooled heap objects that are interned in a process-wide runtime table on
first initialization (`mule_literal_text`) with their hashes precomputed, so
equal string literals share one canonical `Value` across functions and compilation
units while still releasing their unit slots on teardown. `sys.intern` and
`sys._is_interned` operate on the same runtime table, pinning dynamically
interned exact `str` instances until process finalization and rejecting `str`
subclasses in `sys.intern`. Following SBCL's `find-constant` and
`equal-constant-value` coalescing in `src/compiler/ir1util.lisp`, `eq` transforms
in `src/compiler/typetran.lisp`, string equality and length transforms in
`src/compiler/seqtran.lisp`, and equality constraints in
`src/compiler/equality-constraints.lisp`:
- IR1 coalesces equal immutable constant literals (`int`, bit-exact `float`,
  `str`, `bytes`, `bool`, `None`, `Ellipsis`, and `tuple`s of coalescible
  literals) into a single `Constant` leaf per compilation namespace while
  retaining per-reference source coordinates.
- IR1 folds `is` and `is not` on proven string literals, narrows `TextType`
  variables across `==` and `!=` branch edges (including known string length
  from 1-character indexing and constant-bound slices), and folds `==` and `!=`
  when known string values or lengths are disjoint.
- IR2 lowers `==` and `!=` to pointer comparison (`icmp eq`/`ne`) when either
  operand is a universal singleton or both operands are proven interned strings
  (`is_interned_text`), and emits inline `i16` header + `i64` word comparisons
  against the `#[repr(C)]` `Text` / `#[repr(u8)]` `Kind` `InlineLatin1` payload
  for 2..=24-byte Latin-1 constant strings without materializing a heap constant
  or calling `@mule_text_eq`.
Joins preserve the possibility of an unbound
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
ownership as internal LLVM functions. Proven operands and bounded results
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

SBCL `src/code/numbers.lisp`, `%negate`, owns generic numeric negation in
compiled Lisp; `src/compiler/float-tran.lisp`'s `%negate` transforms specialize
proven representations. Mule's unary VOPs likewise keep proven scalar/fixnum
paths and direct tagged-integer guards, but unknown non-tagged values call
compiled `positive`, `negative` or `invert` protocols. These resolve special
methods on the class, including descriptor binding, and return the method's
result unchanged. Python permits user unary methods outside the numeric class
hierarchy, unlike SBCL's `number` dispatch, so missing methods raise Python
`TypeError` rather than failing a native numeric-storage assertion. Builtin int
and float classes own their unary methods; Rust exposes only existing physical
negation/inversion of canonical numeric storage. Float negation preserves signed
zero, and numeric subclasses' inherited unary methods produce base scalar values.
`abs` uses the same compiled special-method boundary; scalar classes own
absolute-value conversion and sign handling, with binary64 magnitude as a physical
primitive. User `__abs__` results and exceptions propagate unchanged.

Tagged integers, booleans, and `None` need no reference-count updates. Ownership
templates skip those values; heap values retain the Rust ownership protocol.
Unknown local loads still check for the unbound marker.

Fixnum addition and subtraction operate on tagged words, following
`overflow+-fixnum` and `overflow-fixnum` in SBCL's
`src/compiler/x86-64/arith.lisp`. SBCL uses a zero fixnum tag; Mule encodes
`x` as `2*x + 1`. Addition clears the left tag before adding the right word.
Subtraction clears the right tag before subtracting it from the left word.
The result already has Mule's tag, and signed machine overflow exactly matches
overflow of the fixnum range. LLVM's signed-overflow intrinsics replace SBCL's
instruction and overflow-flag branch. Overflow uses the existing
arbitrary-precision fallback with the original operands, rather than SBCL's
checked-arithmetic error path. Both proven-fixnum and dynamically guarded VOPs use
this representation, avoiding operand decoding and result re-encoding.

Add/sub VOPs separate tagged success from the runtime fallback. Tagged success
is non-null and its operands are immediate, so it needs neither a sentinel check
nor reference-count operations. The fallback preserves the original operands
and in-place reuse slot, releases consumed operands, and checks its nullable
result before joining success. The merge publishes an owned result only after
success; unrelated live values retain ordinary failure cleanup.

## Internal calls

Every function has a public callable wrapper and an internal native body. The
wrapper checks positional arity and acquires owned arguments. The body receives
the globals and positional values directly, owns them, and cleans them on every
normal/error exit.

Traceback recording identifies each live call with a separate one-byte stack
allocation. The runtime compares and stores its address without reading its
contents. Return-value storage does not escape through this interface, so LLVM
can promote it to SSA across success and error paths, including native adapters.
SBCL's `src/code/debug-int.lisp` represents a frame with its control-stack
pointer independently of returned values. Mule retains its existing explicit
traceback interface rather than reconstructing LLVM-managed frames; the separate
address preserves that identity contract without forcing result loads and stores.

### Live activation records

Generated object bodies keep a stack-resident record with parent, code metadata,
and borrowed globals. A runtime-owned, constant-initialized thread-local atomic
head identifies the current chain. LLVM initializes every field before publishing
the record with a release store, before fallible module initialization. Cleanup
reacquires the current thread's head slot and reloads the record's parent before
unlinking it. Neither value may be cached across generator suspension. Existing
raw entries and native adapters keep their existing frame visibility.

Suspended coroutine stacks retain detached record chains, not copied vectors.
Resume links the detached bottom to the resumer before publishing the coroutine
head. Yield restores the resumer head before clearing the detached bottom's
parent. Only detached records can have their parent changed. Empty or completed
coroutines retain no detached records. This preserves sequential resumption;
it does not introduce concurrent-resume support.

Frame inspection and fatal-signal diagnostics share this chain. Explicit-chain
signal traversal uses immutable preformatted metadata without allocating. The
optional libc C-stack diagnostic remains separate. Materialized Python frames
and traceback values retain their existing ownership; live records borrow the
globals already kept alive by the callable/module lifetime.

SBCL `src/code/debug-int.lisp`, `top-frame` and `compute-calling-frame`, recovers
frames from native control-stack and compiled debug metadata. Mule does not yet
emit the corresponding metadata for LLVM-managed frames. Explicit stack records
are the LLVM adaptation: they remove runtime vector updates and duplicate
fault-record allocation without pretending that an unwinder can recover missing
Python globals. Rust exposes only the physical TLS slot. Without LTO, entry and
exit retain small slot-accessor calls; LLVM performs the record loads and stores.

When analysis predicts a target with matching arity, generated code checks the
already-evaluated callee's native entry and globals. A match selects a specialized
entry after checking argument representations/ranges, or calls the general native
body. Proven caller facts remove redundant argument checks. A mismatch calls
the actual callee through the runtime. Both routes use the same recursion guard.
Arguments still evaluate after the callee and before the guard, so an argument
that rebinds the global does not replace the already-loaded callable.

When an external entry rejects a conjunction of full-range fixnum argument
guards, it calls a dedicated fallback body with that rejected predicate.
The predicate refers to the original incoming SSA arguments, not mutable
parameter locals. It does not imply that either individual argument is a
non-fixnum. IR2 follows single-definition argument moves, exact copies, and
bound local loads to prove operand origins. When an operation contains every
argument from the rejected conjunction, its VOP emits the runtime fallback
directly instead of repeating the impossible fixnum path. Reassigned, cyclic,
or ambiguous origins keep their ordinary checks. The canonical general body remains
unrestricted for other callers. Mixed class, range, or other guards retain
the unrestricted fallback rather than claiming a stronger negative fact.
SBCL's `constraint.not-p`, `add-test-constraints`, and
`constraint-propagate-if` in `src/compiler/constraint.lisp` preserve negative
branch facts. Mule carries this relational entry fact separately from its
per-value type lattice and applies it to representation-specific VOP emission.

### Numeric conventions in mixed bodies

Same-unit proven and guarded positional calls can select a numeric internal entry
without requiring a whole-function native plan. The selected semantic context must
fit the caller's proven argument facts without guards. A numeric-use float result
returns as `double`; a non-boolean integer proven within the fixnum range returns
as `i64`. A numeric entry can still execute object operations and retains the
ordinary frame, traceback, handler, and descriptor cleanup paths.

Float parameters use `double` only when every assignment-connected callee use
permits it. Proven fixnum parameters use `i64` at the call boundary and can use
the existing tagged representation inside object operations. Other parameters
transfer owned object references normally. Numeric calls keep the actual callable
alive while borrowing its closure. They check the existing error-flag convention
before delivering the result; zero and NaN are not failure sentinels.

Object and unknown callers retain the ordinary object entry. In particular,
numeric float entries are selected only when every assignment-connected caller
consumer admits doubles, not for identity comparisons or escaping objects.
Self-recursive, mutually recursive, closure, and guarded same-unit calls use
numeric conventions when their argument and return representations qualify;
keywords, default/reordered arguments, and foreign units retain the object
convention. See
[numeric calling conventions](compiler-gaps.md#numeric-internal-calling-conventions-implemented).

## Borrowed local lifetimes

Status: implemented for stable local roots and supported pooled constants,
including control-flow branches, loop backedges, and exception successors.

An owned local slot can keep a loaded temporary alive without giving that
temporary a second counted reference. This does not require the object itself
to be private or nonescaping. The proof concerns one reference's lifetime:
every use must borrow it, or forward it into another temporary with the same
proof, while the original local retains its ownership.

The proof must cover every normal and exception path between the load and its
uses. An overwrite, unbind, ownership move, suspension, ambiguous definition,
or potentially aliasing uniqueness-dependent mutation invalidates it. Loop
backedges require the same proof: an owner's next iteration is not evidence
that the previous iteration's value remains alive. A copy into an independently
owned local is not a borrowed forwarding operation.

The proof follows program points backward from consumers and stops at the
reference's acquisition. Unrelated operations after its last use do not extend
that lifetime through the next loop iteration. Treating the entire loop as one
live interval would incorrectly restore retain/release traffic for otherwise
safe iteration-local borrows.

Borrowed lifetime information belongs to a TN/reference acquisition, not the
underlying object or pointer. It must survive stack spills and reloads. Borrowed
and owned stack slots have different cleanup policies and must not be packed
together. Normal cleanup, exception unwinding, slot replacement, and repeated
reads must not consume borrowed ownership. Acquiring a genuinely owned copy
from a borrowed reference still requires a retain and a corresponding release.
The owner's existing overwrite and cleanup points are not moved, preserving
the observable lifetime boundary rather than deferring destruction.

When a single-assignment local variable (or a fixed-arity `structural-unpack` /
`unpack-ref` sequence) is initialized from an exact `list`, exact `tuple`, or
`_storage_read` indexed off a stable local container root, IR2 specialization
(`_try_borrow_container_element_local`, `_try_borrow_unpacked_getitem`) extends
this borrowed-lifetime proof across multi-block non-mutating regions. As long as
all consumers of the element local are borrowing operations (or a single final
mutation consumer such as `protocol-setitem`) and every reachable CFG path from
the container read to those consumers preserves the container binding without
mutating the container or running arbitrary user code, the element local and
any unpacked slot locals borrow directly from the container root without
retaining or releasing the loaded element.

### Local alias copy propagation

Status: implemented for single-definition local aliases in proven nonthrowing,
callback-free regions.

IR2 copy propagation can replace reads of a single-definition, uncaptured local
alias with reads of its original owning local. This proof is independent of the
Python value's type. It must establish assignment dominance, source stability,
and equivalent ownership lifetimes over normal paths, loop backedges, and
exception successors. Source overwrites, unbound paths, capture, suspension,
and potentially aliasing uniqueness-sensitive operations prevent substitution.
Checking only paths to alias reads is insufficient: an alias can retain ownership
until cleanup on a path that does not read it.

Unknown operations, potentially throwing operations, unknown truth conversions,
and safepoints remain barriers while the alias owns a reference. An unread exit
also prevents substitution because it could change final cleanup order. The
region ends at proven final consumption; unrelated effects afterward do not
extend the alias's lifetime.

After substitution, the existing final-read and borrowed-lifetime analyses
recompute transfers and releases. A final read can transfer the original owner
into a temporary. Borrowing across that transfer requires proof that the moved
owner remains alive through the borrower's final consumer. It must not suppress
the final release or postpone it to function exit.

SBCL's `src/compiler/copyprop.lisp`, `copy-propagate`, propagates single-writer
MOVE temporaries while source writes kill the available copy. Mule performs
its corresponding transformation over IR2, where the selected operations also
expose reference ownership and uniqueness-sensitive mutation. Its extra lifetime
and effect restrictions preserve reference-counted reclamation and Python-visible
behavior; they are not type-specific optimizations or LLVM text rewrites.

### SBCL correspondence and reference-counting adaptation

SBCL's `src/compiler/envanal.lisp`, `ref-good-for-dx-p` and
`leaf-refs-not-escape-elsewhere-p`, follow known operation contracts and local
callee uses to establish bounded lifetimes. `src/compiler/life.lisp`,
`lifetime-flow-analysis` and `propagate-live-tns`, propagate TN lifetime facts
over the IR2 control-flow graph. Mule keeps that phase organization and uses
the same normal/exception control flow already used by its last-use analysis.

SBCL's tracing collector does not need to distinguish a counted owner from a
borrowed reference. Mule does: it uses explicit ownership metadata to suppress
only the redundant reference's retain/release operations. This is a concrete
adaptation to Mule's reference-counted runtime, not a change to Python's
evaluation or mutation semantics. It reduces ownership traffic without
disabling reclamation or weakening alias-sensitive in-place reuse rules.

Nonescaping argument metadata alone does not establish that a backend call
borrows its operand. The private numeric-box cleanup below does not establish
a generic container drop-fusion implementation.

### Ownership-specialized calls

An ownership-specialized `EntryContext` describes a body whose selected
incoming parameters borrow independently live caller owners. This uses the
existing per-context body compilation mechanism; it does not change the public
ABI or the ordinary owning body. An owning temporary or last-use transfer must
continue to use the owning body: releasing its parameter in a helper after the
body returns could reorder destruction relative to the callee's other locals.

Candidate parameter contracts must be established before caller IR2 conversion,
then combined with its lifetime proof. A contract alone cannot establish that
an actual caller owner survives. Borrowed roots cannot move on last use or
participate in the two-count reuse rule, whose counted-local premise would no
longer hold. A consuming use must acquire an independently owned reference.

The ownership mask is in signature order; call helpers map it to evaluated
arguments. A direct arm neither retains nor releases borrowed inputs. A
frame-entry rejection releases only helper-owned inputs. A dynamic-target
mismatch first acquires owners for borrowed inputs before calling the existing
consuming fallback. Context identity, helper caching, symbol lookup, and lazy
body emission must all agree on the same ownership mask. Ownership variants
must not appear in ordinary type-guard selection used by owning callers.

Type-analysis entry facts are converted to a backend `EntryContext` before
ownership specialization. The ownership mask covers counted object references,
not tagged integers, booleans, or `None`. A lifetime annotation on an uncounted
immediate must not block an otherwise valid numeric calling convention.

This extends SBCL's local-call use analysis and entry-context specialization
to reference-counted parameter lifetimes. Foreign, generator, native, numeric,
and protocol entry paths remain owning until their contracts explicitly support
ownership specialization. Generic container drop fusion remains separate.

Call-site support covers same-unit calls (positional, keyword, and
default-parameter calls mapped through `call_order`) with stable, noncaptured
parameter locals. Variadic, method, non-local closure, unpacked-return, and
exported cross-unit entry contracts do not select borrowed bodies directly,
though cross-unit `ForeignFunction` interfaces (`BuiltinsInterface` and
`ModuleInterface`) export `borrowed_parameters` summaries so callers' escape
analyses can prove that arguments passed to non-capturing imported functions do
not escape. Escape analysis also classifies non-user-code truthiness, value
comparisons, `math.sqrt`, exact builtin container subscript keys, and exact
builtin method calls (`str`, `list`, `bytearray`, `dict`) as non-escaping.
Other operations conservatively escape until their effects are declared. In
particular, unknown truth and callable operations may publish their receivers
through user callbacks. Guarded recursive forwarding remains escaping; the local
dependency solver is not a claim of general interprocedural borrowing for
dynamic calls.

Exact-dictionary protocol calls transfer operands they already own and acquire
references only for borrowed operands. The consuming compiled protocol releases
those references on both success and failure; the original ownership must not
be lost by retaining an extra copy and abandoning the taken operand.

## LLVM-visible boxes

Boxing separates raw pooled storage from LLVM-visible initialization and
payload loads. This follows SBCL's `move-from-double` in
`src/compiler/x86-64/float.lisp`: allocation and the payload store are separate
backend operations. `src/compiler/x86-64/alloc.lisp` similarly separates raw
allocation from header initialization. Mule uses its existing C-layout object
ABI and pool rather than introducing another object representation.

Raw object storage and GMP limb storage have separate matching LLVM `allockind`
and `alloc-family` contracts for allocation and terminal deallocation.
Allocator-internal bookkeeping may disappear
with an unobservable allocation. Memory effects remain unspecified: these
functions do access TLS. Generic `new_value` performs canonicalization and GC
work; conditional `release` can leave references alive. Neither is an allocator
or unconditional free for this purpose.

A representation-stage ownership proof marks complete copy-connected groups
private only when every origin creates fresh boxed storage and every use has an
explicit noncapture contract. Unknown uses, parameters, pooled constants,
globals, containers, closures, and ordinary returns prevent this proof. A
proven same-unit factory result can qualify only when every selected normal
object entry has a fresh return with no other escape. Numeric entry conventions
are not used to infer object-return ownership. In-place box reuse is not fresh
provenance. Physical slots with different ownership modes cannot be coalesced.

Private groups use ordinary local reference counts, codec-defined payload
cleanup, and terminal raw storage deallocation. Other groups retain their
existing ownership helpers and generic
destruction. This adapts SBCL's lifetime/representation separation to Mule's
reference-counted runtime. It does not cancel box/unbox values: LLVM performs
the payload forwarding and allocation elimination. Layouts cover a scalar float
and tagged/GMP signed words; allocation and privacy mechanisms are layout-driven.

Float type alone does not authorize raw deallocation. The physical weak-handle
API can register heap floats even though public Python weakref constructors
reject builtin floats. Such registration captures the pointer and therefore
prevents private ownership. This preserves weak callbacks, alias identity,
shared-reference behavior, and pending-error cleanup.

### Scalar and native-reference conversions

SBCL's `src/compiler/x86-64/move.lisp`, word/descriptor moves, separates
fixnum tagging from bignum allocation and digit loads. Mule retains its existing
tagged/GMP integer representation: signed words within the fixnum range encode
directly, while wider words initialize a normalized one-limb GMP value on the
64-bit, nail-free limb ABI. Other GMP limb configurations retain checked Rust
conversion and cleanup paths. The GMP tuple wrapper has an explicit transparent
layout, and runtime tests assert all generated offsets.

Box payload codecs describe initialization, extraction, and owned-resource
cleanup. Wide integer limb bytes come from GMP's configured allocation callback
and return through its matching sized-free callback. No Rust/system allocation
is paired with GMP cleanup. Matching allocation-family attributes apply only to
compiler-generated raw storage operations, never to public GMP functions or the
callbacks themselves. As for the object pool, compiler-internal allocation
bookkeeping and hook call counts are not language guarantees when an allocation
is eliminated. Actual allocations still honor the configured callbacks. User
finalizers, weak callbacks, and other language effects remain outside that
contract. Unknown memory effects are not advertised as pure.

Booleans select the existing immortal objects directly, and `None` remains its
singleton. Existing string/container object references require no reboxing.

Native instance views carry the original borrowed object descriptor, following
SBCL's descriptor-based instance access. Field operations derive the immutable
payload pointer from that descriptor. This replaces detached payload pointers;
there is no owner lookup, backlink, or parallel ownership cache. Instance,
class, and method reference results become owned before raw-body cleanup.
Native callers put those results in their existing per-function owner slots.
Adapters consume that ownership. First-class method attributes are materialized
at the attribute access through the existing compiled binding protocol, before
subsequent effects or raw-body cleanup. Internal resolve-call recipes remain
separate from materialized method objects. Owned-result producers in loops retain
the ordinary path until replacement/liveness analysis can safely release roots.

This ownership convention is a necessary adaptation to Mule's reference-counted
runtime, unlike SBCL's tracing collector. NativeInstance tag, payload pointer,
and length offsets are asserted against real allocations before generated
direct loads depend on them. Checked public numeric entry boundaries keep their
pending-error, type, and width validation.

## Raw int64, float64, and class entries

`represent.py` selects physical representations after semantic analysis,
following SBCL's `represent.lisp` separation of semantic type from storage
class. A native plan assigns every parameter, local, and value of a lambda a
machine representation (`signed64`, `float64`, `boolean`, `none`, a borrowed
native-instance descriptor, or a guarded class). The lambda is generated under
that plan,
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
at entry and exit when those conversions preserve object semantics. A generic
Float64 result adapter requires every return copy group to have fresh producers
and no identity-bearing use. Integer adapters distinguish forwarded object
identities from computed machine values. When reboxing is not safe, an adapter
still validates its parameters and class layout before transferring the original
objects to its boxed body. The raw entry remains available to native callers.
Safety-zero contract arithmetic uses LLVM
`nsw`; positive safety uses overflow intrinsics and reports `OverflowError`
before a field write.
The boxed path remains available for unsupported signatures and operations.

`mule_types.float64` contracts and concrete inferred float facts can select `double`,
as SBCL's
`double-float` declarations select `double-reg`. Float parameters are unboxed
once at the adapter (`mule_unbox_f64`, which rejects integers the way the
machine float contract does) and results use the LLVM-visible boxing helper.
Builtin `float` annotations alone describe a real-number union and do not imply
a binary64 representation.
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

Local method edges require an available raw body. Class dependencies propagate
through the native call graph and travel as guarded, borrowed hidden arguments;
conflicting layout requirements reject the plan. Cyclic call components remain
boxed until native stack checking
is implemented. Predicted class globals are loaded and checked at entry against
the compiler layout and module environment. These speculative reads do not
raise for an absent binding: they select the boxed fallback, whose actual
lookup preserves conditional evaluation and NameError behavior. Calls in the
selected region cannot
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

An instance's owned descriptor keeps its derived payload view alive for the
native region. Aliased local references share the same object; method field
operations do not retain or release scalars or receivers per iteration.
Constructor and owned-call-result sites in loops retain the general path until
replacement/liveness analysis is available. Native instance, class, and method
returns transfer ownership before cleanup; callers root owned results until
their last use or next ownership transfer. Cleanup releases every root on normal
and error exits. Allocation currently
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

This stage does not assign raw float signatures to unknown calls, store floats
in native class fields, or perform SBCL's full local-call and constraint
transformations. A native plan remains all or nothing, with coercions only at
the entry adapter. Ordinary object bodies can nevertheless retain float locals
in raw storage across blocks when their entire assignment-connected group has
only compatible numeric uses. Proven numeric internal calls can carry those
representations between mixed bodies. Object boundaries keep groups boxed where
identity requires it; see [closed numeric-use float groups](compiler-gaps.md#closed-numeric-use-float-groups-implemented).
Speculative global call returns remain unknown
when the general fallback could return another type. LLVM handles SSA
promotion, instruction optimization, and register allocation.

### Generated code volume

The recorded investigation below predates several optimizations, including
last-read ownership transfers, local borrowing, and closed numeric-use float
storage. Its counts describe that snapshot, not the current checkout.

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
