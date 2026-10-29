# Sequences, iteration, and exception edges

Byte padding methods share compiled storage traversal and fill validation;
bytes results are immutable and bytearray results are fresh mutable values.
SBCL src/code/seq.lisp implements sequence filling and concatenation in Lisp
over storage primitives. Mule retains that boundary, adapting Python's padding
direction, center rounding and one-byte fill validation in compiled Python.

List and tuple representations execute in compiled Python and dispatch each
element through its representation protocol, including builtin subclasses.
SBCL's src/code/print.lisp, output-object and check-for-circularity, likewise
runs recursive object output in compiled Lisp. Python requires recursion markers
rather than Lisp's print-circle labels: the compiled sequence formatter tracks
active container identities and removes them on both success and error.
Physical reads bypass subclass item and iteration overrides, as Python's
builtin container representations do.

Builtin list and tuple iteration reads physical sequence storage independently
of subclass `__getitem__` overrides. Custom objects that provide only indexing
still use the language-level indexing iterator. Storage iterators observe list
growth before exhaustion and remain exhausted afterward. SBCL
`src/code/seq.lisp`, `%map-to-list-arity-1` and `%map-for-effect`, likewise uses
sequence-specific traversal; Python adds explicit iterator objects with
permanent exhaustion. The iterator algorithm remains compiled Mule Python.

Dictionary key and item views provide shared set operations and comparisons,
including reflected operators with iterable operands; values views retain
sequence behavior. Operations return ordinary sets and membership continues to
observe the live dictionary. Item membership accepts two-element tuples;
intersection can reject nonmatching unhashable entries without first hashing
the entire view. SBCL `src/code/list.lisp`, `set-difference` and `intersection`,
similarly implements membership-based selection in compiled language code.
Python adds live views and reflected operators. Set slots return NotImplemented
for unsupported families so the other operand's view protocol can answer.

Byte joining validates contiguous buffer exports, copies their bytes with
compiled traversal and inserts the separator between entries. Hexadecimal
conversion is shared by bytes, bytearray and memoryview, including ASCII
separators and grouping from either end. Byte trimming (`strip`, `lstrip`,
`rstrip`), prefix/suffix removal (`removeprefix`, `removesuffix`), splitting
(`split`, `rsplit`, `splitlines`), partitioning (`partition`, `rpartition`),
replacement (`replace`), and subsequence containment (`__contains__`) share
compiled byte-span helpers across `bytes` and `bytearray` over physical byte
storage (`_storage_read`/`_storage_length`) and `byte_copy`. SBCL
`src/code/string.lisp` (`generic-string-trim`, `string-left-trim`,
`string-right-trim`, `string-trim`), `src/code/seq.lisp` (`search`, `subseq`),
and `src/code/print.lisp` (digit output) provide the corresponding
language/storage boundary; Python requires contiguous bytes-like argument
validation, ASCII whitespace defaults when `chars` or `sep` is `None`, `bytes`
identity preservation for unchanged exact `bytes` instances, and fresh mutable
`bytearray` results. Rust continues to supply byte storage and lifetime
services.

Contiguous memoryviews over immutable byte storage copy their bounded byte
range through the native `byte_copy` primitive. Compiled Python checks release
state and computes the offset and byte count from the view's format and stride;
strided and mutable storage retain compiled traversal. SBCL
`src/compiler/generic/vm-tran.lisp`, `%byte-blt`, similarly lowers contiguous
storage movement to a physical memory copy. Python's `tobytes()` requires a
fresh immutable result, so Mule's primitive also allocates that result. It
performs no indexing hooks, format interpretation or buffer protocol policy.
Contiguous immutable byte slices reuse this primitive after compiled slice
normalization. Strided slices reuse the shared storage selection loop with the
already normalized positions, so each bound's integer protocol runs once.

Generic membership uses compiled `contains`/`not_contains` protocols. SBCL
`src/code/seq.lisp`, `%find-position` and `find`, iterates sequences through
compiled tests, specializing known vectors. Python additionally tries the class's
`__contains__` first, otherwise obtains an iterator (including indexed sequence
fallback) and tests identity before equality. Only iterator exhaustion terminates
the search; equality/truth exceptions propagate. Explicit `__contains__ = None`
disables membership. Optional class-table lookup takes an explicit missing-value
sentinel, distinguishing an absent method from a stored None. Python 3.14 treats
`AttributeError` during special-method descriptor binding as absence; other
descriptor errors and exceptions raised by invoking the bound method propagate.

Status: implemented the initial sequence, iteration, and exception slice.

## Singleton-list fill (Section 6J)

SBCL `src/compiler/seqtran.lisp`, `fill-transform`, selects `vector-fill/t`
for general object vectors after deriving their storage and bounds. Mule's
corresponding singleton-list repetition transform preserves Python operand
evaluation order and shared element identity. It removes the intermediate
singleton allocation only when the repetition count cannot invoke overloaded
multiplication. Unknown count types retain ordinary protocol dispatch.

The `_list_fill` algorithm executes in compiled Mule Python. A low-level storage
reservation primitive allocates capacity without implementing Python repetition
policy. Count-range validation precedes negative-count normalization, and
allocation failure remains an exception. Each initialized slot owns its element
reference, including partial-fill cleanup on failure. The fill loop uses native
integer comparisons and increments with existing physical append primitives.
These choices adapt SBCL's tracing-collected vector fill to Mule's owned mutable
list storage. Both operand orders specialize. Unknown counts retain ordinary
multiplication, including custom index and reflected multiplication behavior.

## SBCL sources and adaptations

SBCL `src/code/macros.lisp`, `dolist` and `dotimes`, expands iteration into
ordinary bindings, tests, and control transfers. `src/compiler/seqtran.lisp`,
`build-sequence-iterator`, specializes known vectors to indexed loops and leaves
unknown sequences on compiled language protocol paths. Mule likewise lowers
`for` to explicit IR blocks; Python requires `__iter__`/`__next__`, exhaustion by
`StopIteration`, and an `else` path distinct from `break`. Language algorithms
and iterator state machines belong in compiled Mule Python, not Rust.

SBCL `src/code/seq.lisp`, `elt` and `%setelt`, separates language-level sequence
selection from physical access. `src/compiler/array-tran.lisp`, the `aref` and
`(setf aref)` transforms, separates bounds checks from data-vector operations.
Mule's Python indexing normalization and protocol selection are compiled Python;
Rust supplies guarded physical length, element access, and replacement. Python
negative indices and mutable list lengths require different language rules from
Common Lisp vectors. Storage guards are not source-language type tags: builtin
storage uses shared class method tables, just as user instances do.

List storage exposes its physical length through the compiled `list.__len__`
descriptor. The descriptor accepts list subclasses, bypasses subclass length
overrides when called explicitly, and rejects non-list receivers. Sequence
patterns materialize a list and use this descriptor to check its length.
SBCL `src/code/seq.lisp`, `length`, dispatches to sequence-specific length
operations. Mule retains that language/storage boundary and adds Python's
explicit descriptor receiver rules; no language algorithm moves into Rust.

Tuple storage exposes its compiled hash as `tuple.__hash__`, so class-level
hashability queries agree with `hash(tuple_value)`. The combination reads
physical tuple elements and recursively calls the language hash operation;
subclass iteration, indexing, and length overrides do not affect those reads.
SBCL `src/code/target-sxhash.lisp`, `sxhash` and `sxhash-recurse`, likewise
combine hashes over owned structure storage. Mule uses Python's tuple
xxHash-style combination and 64-bit signed result instead of SBCL's bounded
depth and fixnum hash, preserving Python's tuple hash values and element errors.

The root object's default inequality binds the class's equality descriptor
through the special-method lookup path, then negates its result unless it is
`NotImplemented`. Ordinary lookup of `type(value).__eq__` can instead select a
metaclass method and incorrectly compare identities. This uses the existing
compiled descriptor boundary, following SBCL `src/pcl/slots-boot.lisp`'s
compiled method-function selection; Python's `object_richcompare` equality
delegation and `NotImplemented` continuation are its concrete language rules.
The existing native class-table shortcut also stops suppressing the compiled
`object.__ne__` entry: its behavior depends on the receiver's equality method,
so native identity inequality is not an equivalent default. The language
algorithm stays in Python; the physical method table retains its callable.

SBCL `src/compiler/ir2tran.lisp`, `ir2-convert-throw` and `emit-nlx-start`, records
nonlocal targets and cleanup/dynamic state explicitly. Mule uses explicit IR
exception successors and native return/error edges rather than SBCL's assembly
stack-unwinding VOPs. LLVM functions and reference-counted ownership require
releasing expression temporaries at a handler transfer while preserving locals.
Exceptional analysis must join states before and after a potentially failing
operation; successful stores must not be assumed on an exceptional edge.
Caught exceptions and handler state are ordinary owned Mule class instances.
`KeyError` owns its report behavior: one argument is formatted with `repr`,
while zero or multiple arguments retain `BaseException.__str__`. SBCL's
`src/code/target-error.lisp` similarly installs reports on condition classes;
the single-key quoting rule is Python's concrete exception-report adaptation.
`divmod` uses class special-method lookup and descriptor binding, gives an
overriding reflected method on a strict subclass priority, and reports its own
unsupported-operands error after both methods decline. Numeric classes own
their forward and reflected quotient/remainder operations. SBCL's
`src/code/numbers.lisp` implements its quotient/remainder algorithms in compiled
Lisp; Python's two-operand special-method negotiation is the added boundary.
The tuple class exposes its physical `__len__` descriptor, including explicit
receiver validation. Calls through that descriptor bypass subclass length
overrides, as required when an OS argument protocol inspects a tuple result.
Native primitive errors are converted to those instances at the catch boundary.
SBCL `src/compiler/envanal.lisp`, `emit-cleanups` and `find-cleanup-points`,
derive cleanup code from the dynamic environments at each control-flow edge.
Mule likewise keeps an empty exception suite's exit inside its handler cleanup
until environment analysis inserts the handler pop. Even an `except: pass`
must restore the enclosing handled exception before subsequent statements;
creating its filler exit in the enclosing environment would erase that edge.
The error-path `__exit__` call likewise runs inside an explicit handler-body
cleanup and receives the caught exception's traceback. SBCL's
`src/compiler/ir1-translators.lisp`, `unwind-protect`, preserves the transfer
while running cleanup code and then resumes it. Python additionally exposes
the handled exception through `sys.exception()` during `__exit__`, so Mule
pushes that instance for the call and pops it on suppression, propagation,
or a replacement error. This uses the existing cleanup-edge machinery.
The uncaught-error boundary uses the same conversion after protocol initialization;
builtin errors are reported through their compiled exception classes and `__str__`.
Exception hierarchy matching runs in compiled Python using actual class ancestry;
there are no name-based exception matching rules or type-tag attributes.

SBCL `src/code/debug.lisp`, `debugger-disabled-hook`, isolates condition printing
and backtrace printing so a failure in user printing code cannot prevent reporting
the original condition's stack. Mule saves the entire pending exception and
traceback while calling compiled `__str__`, discards any formatting error, then
restores the original state. A failing formatter reports `<exception str() failed>`.
Python requires an owned `__traceback__` chain that survives handler transfers and
reraises, whereas SBCL walks the native stack through its debug information.
Mule's generated error landings record source frames during propagation and
transfer the chain to the ordinary exception instance at a handler or uncaught
boundary. This avoids walking already-unwound LLVM activations; frame allocation
is restricted to failing paths. Tracebacks expose code, line, globals and caller
links; captured local-variable mappings and complete traceback compatibility
remain gaps.

SBCL `src/code/debug.lisp`, `print-backtrace` and `print-frame-call`, reports
compiled implementation frames as part of its debugger backtrace. Python's
observable exception traceback omits builtin implementation calls: a failure
inside a generator consumed by `list` includes the calling source frame and
the generator, without a `list` constructor frame. Mule therefore omits source
traceback records for the compiled builtins unit, including its private
argument, descriptor, and generator helpers. This is a concrete Python
adaptation rather than removing debug metadata: live-frame registration and
fatal-signal diagnostics still retain those activations. User functions and
ordinary separately compiled library modules continue to record source frames.
Cleanup edges and owned-reference releases are unchanged; the omission avoids
allocating artificial builtin traceback entries on failure paths. Separately
compiled replacements for other C-backed APIs still need their own transparency
classification for complete parity.

SBCL `src/compiler/ir1-translators.lisp`'s `unwind-protect` translator resumes
the saved `(next, start, count)` transfer through `%unwind` after cleanup;
`src/compiler/ir2tran.lisp`, `ir2-convert-throw` and `emit-nlx-start`, separately
initiates transfers and prepares nonlocal targets. Mule likewise separates
explicit Python `raise` from reinstating an
already caught exception after `finally`, a nonmatching handler, context exit,
or an iteration exhaustion check. Reinstatement preserves the original source
traceback without adding the cleanup construct's line. Explicit `raise error`
adds its own source entry even when that activation already occurs in the chain;
bare `raise` continues the existing entry in the handling activation.
The compiled raising protocol validates and prepares an ordinary exception, then
returns it successfully; the caller's terminal VOP installs pending state and
records the source raise. Successful preparation does not become an artificial
traceback frame. Python's observable traceback values require this distinction
beyond SBCL's stack debug records; it changes neither exception ancestry nor
ownership cleanup and allocates source entries only on the transfer path.

SBCL `src/compiler/debug-dump.lisp`, `debug-source-for-info`, preserves source
positions alongside compiled code. Mule additionally plans single-line traceback
highlights from its owned AST spans and emits constant marker strings with the
failure-site metadata. Python 3.14 underlines the call parentheses with carets,
the callee with tildes, and a failing name with carets; complete simple return-call
and assignment-call expressions suppress those highlights. The compiler uses
Mule's parser for the enclosing source statement, never the host AST. The native
runtime stores and writes these constant debug records without parsing or
evaluating source. `PYTHONNODEBUGRANGES` suppresses marker lines as in Python.
Multiline highlighting and complete expression-specific anchor formatting remain
gaps; the verbatim oracle tests cover single-line names and calls, nested cleanup,
and bare and explicit reraises with ranges both enabled and disabled.

SBCL `src/code/target-error.lisp`, `define-condition` and `make-condition`,
implements condition classes, inherited slots and report functions in compiled
language code. `src/code/cross-condition.lisp`'s `encapsulated-condition` stores
another condition as an ordinary slot. Mule's exception groups similarly use
ordinary compiled Python classes and tuple fields, not a new Rust object tag.
Python requires a nonempty sequence of exception instances, readonly message and
children, automatic `BaseExceptionGroup` to `ExceptionGroup` selection when all
children are ordinary exceptions, and an `ExceptionGroup` class inheriting both
`BaseExceptionGroup` and `Exception`. Matching uses the existing full class MRO,
including secondary bases. Tree selection is compiled Python: `subgroup`, `split`
and `derive` preserve nesting, matching identity and traceback/cause/context/notes
on derived groups. Generic subscription uses the shared generic alias protocol.
Exception cause/context descriptors validate exception-or-None values; assigning
a cause enables context suppression, and suppression accepts only booleans.
`BaseException` owns argument repr and validated string notes shared by all groups.
Unlike SBCL's restart handling, group selection partitions a Python exception
tree; it neither executes handlers nor resumes computation. `except*` lowering
and structured uncaught-group rendering remain separate, unimplemented contracts.

The first slice implements ordinary `try`/`except`/`else`, raising and reraising,
list/tuple/text indexing, list replacement/append, sequence iteration, range,
enumerate, and non-starred unpacking. `finally`, exception groups/chaining,
slices, deletion, and extended unpacking remain explicit compilation gaps.
Iterator exhaustion is caught only around the `__next__` call, never around the
loop body or target assignment. Break/continue/return must unwind exited handler
state and delete an `except ... as` binding, including exceptional handler exits.
Private protocol code has its own compiled namespace and cannot observe user
bindings of builtin or intrinsic names. Variadic positional entries materialize
owned tuples from borrowed native argument buffers; argument handling is compiler
machinery, not a Rust implementation of range or iterator algorithms.

SBCL `src/compiler/ir2tran.lisp`, `init-xep-environment`, verifies external
argument counts and copies excess arguments before entering internal bodies.
Mule's variadic external entry similarly checks its fixed prefix and constructs
an owned rest tuple. Internal fixed-arity predictions must not bypass this
entry for variadic functions, even when the source argument count happens to
match the number of internal parameters.

## Fixed-length unpacking of builtin tuples

Non-starred unpacking lowers to a structural check/capture followed by ordinary
target bindings. On exact builtin tuples of matching arity, the check transfers
the existing owned value directly into the capture: no protocol call, runtime
frame, count conversion, or container allocation executes. A `TupleType` arity
proof removes the check too. Otherwise `stdlib/_sequences.py` handles iteration
and shortage/excess `ValueError`, returning a normalized immutable tuple.
Other iterables still collect all required elements and check exhaustion before
any target assignment. Lists are not reused: an earlier target's side effects
could otherwise change the elements seen by later targets.

Successful `%unpack` establishes exact builtin tuple arity even when the input
was a general iterable. `%build_tuple` and literal tuples also establish arity
facts; incompatible control-flow joins discard only the arity proof. Elements
remain unknown, and tuple comparison can still run their user callbacks.

Extraction lowers to `%unpack_ref` with a compile-time integer index. Where tuple
kind and bounds are proved, its VOP calls the unchecked physical `mule_tuple_slot`
primitive, which loads and retains the element without generic indexing, boxed
index conversion, or repeated type/bounds checks. This unsafe ABI requires live
`Data::Args` storage, an in-range index, and initialized non-null elements.
Unproved reads retain the existing checked storage primitive.

The compiler temporary keeps the tuple alive across target evaluation. Its sole
structural-read consumers borrow that owning TN directly, without copying or
retaining the parent. Each read still owns its returned element. The expression
capture's explicit discard and boxed-entry cleanup handle
both successful and exceptional assignments. Nested unpacking retains Python's
target order and can fail after earlier outer targets have been assigned.
Unpack captures are explicitly expression temporaries: the backend includes
their slots in handler-transfer cleanup, rather than retaining them as function
locals after a caught assignment failure. User locals and long-lived compiler
state such as iterators keep their existing lifetimes. This follows SBCL
`src/compiler/ir2tran.lisp`, `ir2-convert-throw` and `emit-nlx-start`, which
distinguish temporary result lifetimes and preserved nonlocal-exit state;
Mule's reference-counted LLVM paths additionally release the captured tuples.

SBCL `src/code/macros.lisp`, `destructuring-bind`, invokes `expand-ds-bind` in
`src/compiler/parse-lambda-list.lisp`, which binds the checker result and emits
ordinary `pop` accesses. Its `check-ds-list` validates shape without constructing
another list. Primitive references then pass through `ir2-convert-reffer` and the
backend's `slot` VOP. Mule follows the same check-then-bind representation;
Python's arbitrary-iterable fallback remains compiled Python, not Rust dispatch.

SBCL `src/compiler/seqtran.lisp`, the `elt` transform, selects `aref` for known
simple arrays. `src/compiler/array-tran.lisp`, the vector `aref` transform,
selects `data-vector-ref` with a separate policy-controlled bounds check.
Mule follows that specialization boundary: unpack establishes builtin tuple
storage, then extraction uses physical access. Python additionally requires
exact unpack cardinality and complete iterable capture before target stores;
those rules remain compiled Python, not Rust dispatch. Mule's reference-counted
LLVM backend requires explicit owned children and a dominating captured owner
rather than SBCL's garbage-collected vector roots. Raw native entries do not yet
support these operations; their separate cleanup must be extended before adding
owned tuple temporaries there. No new caches or per-element tuple types are used.

The known-function database also records successful result types for the tuple
storage predicate, physical sequence length, and integer-storage validation.
IR1 flow propagation invokes `FunInfo.derive_type`, following SBCL
`src/compiler/knownfun.lisp`, `%defknown`, and `src/compiler/ir1opt.lisp`,
`derive-combination-type`. The predicate is boolean, length is a nonnegative
integer, and validated integer storage excludes booleans and user objects.
The private unpack count passes through integer-storage validation, so its
length comparisons have two proven integer operands without source annotations.
These primitives cannot run user callbacks or mutate language bindings. They
remain non-flushable because invalid storage must still raise; no successful
result fact removes their error path. Tuple length is not assumed to fit a
fixnum, and arbitrary-precision counts retain their normal representation.

## Character construction

`chr(value, /)` is compiled Python in `stdlib/_sequences.py`. It reuses the
integer-index protocol, accepts booleans and user `__index__` methods, and checks
the Python range `0..0x10ffff` before constructing a length-one string. Invalid
types raise `TypeError`; negative or oversized integers raise `ValueError`.
The ordinary compiled argument binder rejects missing, extra, and keyword
arguments. Builtin lookup remains dynamic, including aliasing and rebinding.

SBCL `src/code/target-char.lisp`, `code-char`, is a language function backed by
the compiler's character operation. `src/compiler/fndb.lisp` records its known
argument/result types; `src/compiler/x86-64/char.lisp`, the `code-char` VOP,
performs the representation conversion. Mule retains compiled language rules
and a physical construction boundary. Python differs concretely: `chr` returns
an owned string, not SBCL's immediate character, and uses `__index__` with
`ValueError` outside the Unicode codepoint range. Rust's explicitly imported
native `text_from_codepoints` callable only encodes guarded codepoint storage
and allocates text; `chr` passes a one-element list. The existing text
representation permits surrogate codepoints; `chr` preserves them rather than
using Rust's scalar-only `char` type. Construction has constant work and allocates
one 1-to-4-byte text payload and a temporary one-element list; this is not a
complete string or codec implementation.

## Minimum and maximum reductions

`min` and `max` are ordinary compiled Python functions in `_sequences.py`,
installed as builtins with generated positional-rest and keyword/default binders.
They accept one iterable or two or more positional values, with optional `key`.
`default` is permitted only in the one-iterable form. Empty iterables return the
exact supplied default, including `None`, without invoking `key`; without a
default they raise `ValueError`. Zero arguments and a default supplied with
multiple positional values raise `TypeError`.

SBCL `src/code/numbers.lisp`, `min` and `max`, reduces rest arguments in compiled
Lisp and replaces the winner only for strict `<` or `>`. Mule retains that
architecture and first-item tie behavior. Python additionally requires iterable
input, key projection, an optional empty-input default and arbitrary supported
ordered values, rather than only Lisp real-number arguments. The reduction
streams values without materializing the iterable, retaining only the current
winner and key. Each yielded item's key is evaluated once. Iterator exhaustion
is caught only around `next_item`; failures from key, comparison or truth
conversion propagate, including `StopIteration`.

The omitted-default marker reuses the argument protocol's physical missing
sentinel; `None` remains a valid value. Selection and iteration execute in
compiled Python, with no new Rust language dispatch. General reflected
rich-comparison, `NotImplemented` handling and lexicographic list/tuple ordering
remain limitations of the shared comparison implementation, not implemented by
these reductions. Call-site argument expansion remains unsupported.

## Generator exception boundaries

Open-coded generator-expression consumers retain the generator's exception
boundary without allocating a coroutine. Target binding, filters, nested
iterable evaluation and element evaluation run inside that boundary. An escaping
`StopIteration`, including a subclass, becomes `RuntimeError` with the original
exception as its cause and context. The compiled Python `generator_exception`
helper in `_sequences.py` uses the runtime-owned exception classes, independent
of bindings in the consuming module. Other exceptions resume unchanged through
the existing owned-exception transfer.

The outer iterator is obtained at generator-expression creation, before later
consumer arguments are evaluated. Iteration begins only after those arguments.
The shared `for` preparation retains the iterator and range-call evidence for
the loop rather than calling `__iter__` again. Normal exhaustion and consumer
operations, including truth tests, additions, key functions and the surrounding
`for` body, remain outside the generator's conversion boundary.

SBCL `src/compiler/ir1-translators.lisp`, `%within-cleanup` and `catch`, attaches
the protected body's cleanup to its lexical environment. Mule uses the same IR1
cleanup mechanism while keeping consumer code in the enclosing cleanup.
`src/compiler/seqtran.lisp`, `build-sequence-iterator`, keeps sequence preparation
and the mapped function's result consumption in one open-coded loop. Python
additionally distinguishes generator creation, yielded-value production and
consumer execution, and requires PEP 479 exception conversion only while
producing values. The compiled Python helper implements that language rule;
there is no new Rust evaluator or iterator wrapper.

Throwing an existing exception into a suspended generator resumes its saved
traceback before installing the pending error. The suspended yield therefore
prepends its frame without discarding the original failure chain. SBCL
`src/compiler/ir2tran.lisp`'s `ir2-convert-throw` separates transfer state from
the destination; Python additionally requires the exception's owned traceback
to accompany that transfer. Reentrant generator throws reject the running state
before installing the requested exception.

List and tuple comparison methods now return `NotImplemented` for incompatible
storage families, allowing the shared comparison boundary to invoke the other
operand's reflected method. Compatible sequences retain compiled lexicographic
element comparison. SBCL `src/code/pred.lisp`'s `equalp` similarly separates
compatible sequence contents from other representations, but Python's reflected
methods require deferral instead of Lisp's immediate false result. General
subclass precedence in rich comparisons remains incomplete.

## Guarded range loop specialization

Following `build-sequence-iterator` and `dotimes`, a direct call spelled
`range(...)` is a candidate for a counted native loop, not proof of builtin
identity. The callable is loaded before arguments, invoked normally, and retained
for one entry guard. Only the actual private compiled range function and a fresh
private RangeIterator with fixnum bounds/step may take the specialization. Other
bindings and wider integers use the same compiled iterator protocol as any object.
The iterator is fresh and cannot be aliased by user code on the accepted path;
its count can therefore live in compiler-owned stack storage. LLVM tests and
increments that count directly. Signed fixnum operands fit an int64 sum even
when the last increment crosses the fixnum boundary, and the next bounds test
occurs before boxing. This preserves unbounded Python range semantics without
integer overflow or a per-iteration type/identity guard. Constant argument facts
may narrow yielded integers only behind that entry guard. No user annotation is
required. Internal iterator state never becomes a language attribute/type tag.


## Implemented subset and limits

Immutable text and bytes operations preserve exact builtin identity for a
one-times repetition. Operations on subclasses produce builtin results unless
an overridden operator handles the operation. Bytes repetition checks both the
index-sized count and the result-size product before materializing values.

Tuple equality compares shared elements before rejecting unequal lengths.
Their equality and truth-conversion effects remain observable. Singleton
membership compares the element with the query, converts equality to a boolean,
and negates that boolean for `not in`; it does not substitute `__ne__`.

`int` accepts `bytes` and `bytearray` digit sequences, with a default decimal
base or an explicit base. Its compiled `stdlib/_object.py` constructor uses the
base class's Latin-1 decoder to preserve byte values without invoking a subclass
decoder override. The existing integer parser validates signs, prefixes,
underscores, digits, and whitespace; malformed byte sequences raise `ValueError`,
including non-ASCII bytes and embedded NULs. This also supports the regex
compiler's bytearray-to-integer bitmap conversion.

SBCL `src/code/reader.lisp`, `parse-integer`, parses character strings in compiled
Lisp, selecting word arithmetic for short decimal/hexadecimal strings and general
integer arithmetic otherwise. Python additionally accepts byte sequences, so
Mule normalizes these in compiled Python before parsing. Normalization takes
linear time and allocates a decoded string; it does not add Rust language-level
dispatch. The existing Rust integer parser remains bootstrap debt, rather than
an implementation of SBCL's compiled-language parsing boundary.

List/tuple/str/bytes storage shares ordinary compiled method tables. Indexing
supports integers, booleans, and user `__index__`; lists support replacement and
append. Sequence equality is compiled Python, including identity shortcuts and
recursive element comparisons. Iterators observe list appends/replacements until
exhaustion and remain exhausted thereafter. Iteration accepts user protocols and
the legacy indexed-sequence fallback. Constructors `list` and `tuple` consume
iterables; `range`, `enumerate`, `iter`, and `next` are compiled Python entries.
`iter(callable, sentinel)` uses a compiled callable iterator. Construction checks
callability without invoking the callback. Each step calls it once; sentinel
identity or truth of `sentinel == value` ends iteration. Callback `StopIteration`
also exhausts it permanently, releasing the saved callback and sentinel. Other
callback errors and comparison errors propagate without exhausting the iterator.
In particular, `StopIteration` raised by comparison does not clear its state.
SBCL `src/code/reader.lisp`, `%read-preserving-whitespace`, similarly calls a
compiled stream operation until its EOF sentinel is returned. Python additionally
requires rich equality, callback-exception handling and an explicit persistent
iterator object; those rules remain compiled Python, not Rust dispatch.
Keyword arguments are not implemented. Range's
initial implementation is a private compiled class behind the builtin factory;
full builtin type identity, repr, comparison, and
slicing are not implemented. Exception classes have real single-base ancestry,
owned `args`, a compiled `__str__`, and source tracebacks; full exception metadata,
chaining display, exception groups, and specialized exception constructors remain gaps.
Existing Rust scalar printing/truth/containment and dynamic class allocation
remain bootstrap debt; this slice does not extend their language algorithms.

Range membership is implemented by compiled `range.__contains__`. Exact integers
and booleans use the signed-step bounds and `(value - start) % step == 0`, without
enumerating elements or computing a machine-sized length. Other operands use
range iteration with identity/equality matching, preserving custom equality and
propagating comparison errors; membership never coerces operands with `__index__`.
General iterable containment fallback for classes without `__contains__` runs
through the compiled membership protocol described above.

SBCL `src/code/seq.lisp`, `%find-position`, keeps sequence search in compiled Lisp;
`src/compiler/seqtran.lisp`, `build-sequence-iterator`, selects representation-aware
iteration when types are known. Mule keeps range search in compiled Python.
Python's integer range progression additionally permits direct bounds/modulo
membership, while arbitrary equality operands require iteration. These are
concrete Python-semantic adaptations, not new Rust search or dispatch algorithms.

Variadic parameter annotations preserve their syntax metadata and apply numeric
contracts to each rest element, not the rest tuple. Checks follow the function's
existing safety policy. Defaults and keyword-only parameters use the ordinary
compiled argument binder; variadic keyword parameters remain unsupported.
Exception bindings are cleared on every handler exit.

Validation includes native protocol/edge tests, unchanged corpus execution, and
a Valgrind ownership probe with repeated unpacking, iterator calls, and caught
exceptions. A 20-million-element range sum without annotations measured 42-47 ms
in three native runs versus 324-328 ms in host Python 3.14.7. The optimized
machine code retains a counted loop with direct integer instructions; the sum
is not replaced with a constant. Its accumulator still has overflow/representation
slow paths, so this is evidence for this loop, not a claim of general optimization
parity with SBCL. Benchmark inputs/results and disassembly are in ignored build
artifacts `range-iteration-benchmark.py`, `.json`, and `.asm`.
