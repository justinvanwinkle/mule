# Compiled modules and the first upstream test execution

Status: implemented initial static module linking and native test execution.
All six methods in the unchanged Python 3.14 `test.test_int_literal` module execute
and pass. General unittest support remains incomplete.

## SBCL correspondence and required adaptations

### Pattern helper dependencies

IR1 match lowering resolves `_match` only when a pattern emits a sequence,
mapping or class helper call. Literal, value, capture and wildcard patterns
lower directly to comparisons and assignments, including their combinations
with `as` and `or`, and do not link the helper module or its abstract-base-class
dependencies. When helpers are needed, their import still precedes subject
evaluation. SBCL `src/compiler/ir1tran.lisp`, `find-free-fun`, likewise creates
function dependencies when conversion references them; Mule's Python pattern
visitor determines which compiled helper references the generated AST needs.

### Compiler source search paths

Compiler module resolution combines the main source directory, `PYTHONPATH`,
Mule's bootstrap and standard-library roots, and the active `sys.path`, preserving
precedence and deduplicating resolved paths. This uses the same installation
paths embedded in a native image rather than asking CPython's `sysconfig` for
its ABI-specific build data. Runtime `sys.path` additions and its empty
current-directory entry also participate. Host standard-library sources can be
compiled when Mule has no replacement; execution still uses Mule's parser,
compiler and runtime, never the host interpreter.

SBCL `src/code/module.lisp`, `require`, consults explicit paths or its active
`*module-provider-functions*` in order; `module-provide-contrib` uses the running
image's installation root. Python's ordered `sys.path` replaces that provider
list, with Mule's compiled standard-library roots inserted before host sources.
This changes source discovery only and does not claim CPython build-variable
or extension ABI compatibility.

### File timestamp updates

`os.replace` exposes the existing Linux atomic rename service for ordinary
path-like source and destination arguments. SBCL's `sb-posix` `rename` likewise
uses the OS rename operation. Directory descriptor variants remain explicitly
unsupported, as they are for Mule's existing `os.rename` wrapper.

`os.utime` accepts path-like values and descriptors, seconds or integer
nanosecond pairs, directory-relative paths and symlink timestamp updates.
Compiled Python validates arguments, normalizes negative timestamps and selects
ordinary `OSError` subclasses from returned errno values. Rust supplies only
`utimensat`/`futimens` calls with fixed native timespec storage. SBCL
`contrib/sb-posix/interface.lisp`, `utime` and `utimes`, similarly prepare the
time records in Lisp before invoking foreign OS services. Python's nanosecond,
descriptor and directory/symlink options require the newer POSIX entry points.

### Repeated file digest validation

Within a compiler process, file digests are reused only while device, inode,
size, nanosecond modification time and nanosecond change time remain identical.
The file version is checked around reading and hashing; changes during that
interval prevent memoization. Rewrites with restored modification times and
atomic file replacements invalidate the recorded version. Missing paths and
directory inventories retain their ordinary discovery behavior. Persistent
artifact manifests still store SHA3 content digests, not timestamps.

SBCL `src/compiler/main.lisp`, `make-file-source-info`, retains the source file's
`file-write-date` alongside its identity and forms. Mule's persistent artifact
cache additionally needs content digests; process-local file-version reuse
avoids recomputing those digests for every build graph while retaining content
checks on changed files. Linux's inode and nanosecond change metadata distinguish
replacement and same-size writes even when modification time is restored.

The builtins artifact also keys the resolved standard-library root. Its compiled
protocols and interface contain source paths, so two checkouts with identical
compiler files must validate and compile their own protocol sources. A cache
entry from another checkout cannot establish that the current checkout's
protocol edits are unchanged. This retains the source identity represented by
SBCL's `make-file-source-info` while adapting it to shared native artifact caches.

### Dictionary allocation and weak proxies

Mutable list allocation likewise produces empty storage in `list.__new__`;
compiled `list.__init__` owns iterable consumption and repeated initialization.
Subclass initializers can accept noniterable constructor arguments without an
eager conversion in their allocator. SBCL `src/code/seq.lisp`'s `make-sequence`
uses allocation/initial-element operations separately from sequence coercion;
Python additionally exposes allocation and initialization as separate methods.
Repeated initialization clears the list before iterating, including self and
iterators over self, and retains items consumed before an iterator raises.

SBCL `src/code/target-hash-table.lisp`, `%make-hash-table`, constructs complete
empty index/entry storage before the hash table is exposed. Mule's compiled
`dict.__new__` likewise initializes an empty table before any subclass initializer
runs, so subclasses such as `OrderedDict` can override `__init__` safely.
`dict.__init__` updates that existing table rather than resetting it; private
empty-dictionary allocation shares the same storage factory and bypasses argument
binding to avoid recursive allocation of a binder's keyword dictionaries.
The raw allocation primitive no longer rejects layouts with an overriding
`__new__`: creator selection belongs to the compiled constructor protocol, while
the primitive only allocates the requested physical fields.
The weak pointer storage boundary follows SBCL `src/code/weak.lisp`,
`make-weak-pointer` and `weak-pointer-value`. Python's proxy factory selects its
callable/noncallable compiled proxy class using the shared `callable` protocol;
absence of a `__call__` attribute is a normal noncallable result, not an error.
The compiled reference constructors also own weak-reference eligibility:
ordinary subclasses gain support from an implicit or explicit weak-reference
slot, inheritance preserves it, and immutable integer/tuple/bytes families
remain excluded. Builtin implementation classes do not gain support just because
their bodies are compiled Python. The runtime handle checks only that the
referent has independently owned heap storage, registers a borrowed address,
and clears it before destruction. Thus a list subclass with `__weakref__`
uses the same lifetime mechanism as an ordinary instance; no language-level
class or slot policy is added to Rust.

### Direct staticmethod calls

Staticmethod objects are themselves callable and forward positional and keyword
arguments through compiled `__call__`, as required since Python 3.10. This
supports `_pyio.open`'s module-level staticmethod wrapper. SBCL
`src/pcl/dfun.lisp`'s `set-funcallable-instance-function` similarly gives a
callable wrapper its target; Python's wrapper uses the ordinary compiled call
protocol, with no dedicated Rust callable dispatch.

### Attribute enumeration

Builtin get/set and member descriptors are distinct compiled Python classes,
with their name, defining class and qualified name. They reuse the existing
accessor functions and validate the receiver before accessing storage; user
properties retain their separate type. SBCL `src/pcl/slots.lisp`,
`slot-value-using-class`, similarly carries slot-definition metadata beside
the accessor and physical instance storage. Python's descriptor type identity
is an additional language-level introspection contract, needed by static
attribute inspection. The stored MRO property reads the class's finalized
precedence list, as SBCL `src/pcl/std-class.lisp` stores
`%class-precedence-list`; it does not re-run an overridable MRO method.

SBCL `src/code/defpackage.lisp`, `apropos-list`, enumerates names from existing
package storage in compiled Lisp and sorts the result. Mule's `dir(object)`
similarly calls compiled `__dir__` and sorts its iterable result. Each class
family owns enumeration: ordinary objects combine their instance dictionary
with all class MRO namespaces, classes expose their MRO namespaces, and modules
expose only their own namespace or their namespace's custom `__dir__` hook.
Names are enumerated without evaluating properties. Python's object attributes
and module hooks require these separate methods instead of SBCL package symbols;
Rust's existing storage accessors supply only names/dictionary views. Zero-argument
`dir()` remains an explicit gap requiring compiler local-variable snapshots.
The complete-MRO binding primitive includes `object.__dir__`. Older bootstrap
hooks retain their explicit native-default filtering until their scalar default
methods migrate to compiled Python; both lookup modes share class/metaclass
resolution and descriptor binding instead of branching on receiver storage.
SBCL `src/pcl/methods.lisp`, `find-method`, likewise separates method selection
from method invocation. This lookup policy changes which stored root entry is
eligible, without implementing attribute enumeration in Rust.

### Inactive monitoring controls

SBCL `src/code/profile.lisp`, `profile-encapsulation-lambdas`, keeps registration
and callback behavior in compiled Lisp closures around instrumented functions.
Mule's initial `sys.monitoring` namespace similarly keeps Python 3.14 event
constants, reserved tool IDs, tool ownership and callback replacement in compiled
Python. Clearing/freeing tools releases callback registrations; empty global and
local event masks can be read and reset normally. Nonempty event activation is
explicitly rejected with `NotImplementedError`: native compiler instrumentation,
event positions, per-site disabling and event delivery are not implemented yet.
This is an inactive control interface that lets debugger modules import their
event declarations, not an implementation of active tracing or profiling.
Python's source/exception events require IR instrumentation beyond SBCL's
function encapsulation; adding callbacks without probes would silently lose
events, so activation remains unsupported until that compiler stage exists.

### Filesystem path protocols

Filesystem wrappers normalize path objects through compiled `os.fspath` before
passing str/bytes storage to OS primitives. This preserves special-method lookup
and errors in Python and covers both source `Path` objects and custom path-like
classes. `stat` accepts indexable descriptor arguments through `fstat`; directory
names and symlink targets retain bytes results when given bytes paths. SBCL
`src/code/unix.lisp`'s `unix-open` and `unix-stat` likewise take already converted
Unix pathnames at the foreign boundary. No path protocol is added in Rust.

### Terminal dimensions

SBCL `src/code/unix.lisp`, `unix-ioctl`, passes an OS file descriptor, request
word and output buffer to libc. Mule's `_posix` primitive uses Linux
`TIOCGWINSZ` through the same OS boundary and returns physical columns/rows;
the compiled `posix.get_terminal_size` and `terminal_size` tuple subclass own
argument conversion, named fields and repr. OS failures raise ordinary `OSError`,
allowing compiled `shutil.get_terminal_size` to apply its environment/fallback
policy. The supported platform is 64-bit Linux; no new linked dependency or
Python interpreter service is involved.

`posix.isatty`, also exported by `os`, converts `__index__` arguments and checks
the signed C-int range in compiled Python. The existing physical stream query
calls libc `isatty`, following SBCL `src/code/unix.lisp`'s `unix-isatty` boundary.
Python requires a boolean false for closed, negative or nonterminal descriptors,
rather than surfacing libc's errno; no language dispatch runs in Rust.

### Descriptor multiplexing

SBCL `src/code/unix.lisp`'s `unix-poll` and `unix-fast-select` marshal pollfd
arrays and fd sets across libc; `unix-simple-poll` and `src/code/serve-event.lisp`
maintain deadlines across interrupted waits. Mule follows that OS boundary:
compiled `select` owns file-object conversion, poll registration, event-mask
validation, timeout conversion and preservation of original select objects.
Rust owns temporary syscall buffers and monotonic deadlines, returning ready
descriptor rows. Python's automatic EINTR retry requires recomputing remaining
time rather than returning SBCL's raw errno. Active Python signal callbacks
remain unavailable until compiler interrupt checks exist, as described below.
The Linux poll and select interfaces are implemented; epoll, kqueue and devpoll
are not advertised.

### Manual cycle collection

SBCL `src/code/gc.lisp`'s `sub-gc` separates collection from language callbacks;
`src/runtime/gc-common.c`'s `scav_closure` and `scav_instance` enumerate physical
pointer fields while excluding raw payload words. Mule enumerates exactly its
owned heap references at the same low-level boundary. Unlike SBCL's moving
generational collector, the existing reference-counted ABI requires stable
addresses and counts (see `python-changes.md`). A manual cycle pass subtracts
internal owned references from a snapshot of counts, marks objects reachable
from remaining external owners, then breaks unreachable cycles. All candidates
are temporarily retained while edges are cleared, avoiding premature freeing.
This supplements normal immediate reference-count destruction; it does not
move objects or require LLVM stack maps. Suspended coroutine stacks remain
opaque: their owned references conservatively count as external roots, so cycles
through those stacks are not reclaimed. User `__del__` methods and automatic
generational scheduling remain unimplemented. The compiled `gc` interface owns
generation validation and collection callbacks; Rust owns allocation tracking,
pointer enumeration and reclamation. All three public generation arguments
currently request the same complete manual pass.

### Integer conversion and representation

Integer `__repr__` uses a compiled receiver check and the approved GMP decimal
conversion primitive; bool owns its distinct True/False spelling. This makes
`IntEnum`'s inherited `int.__repr__` string conversion work without falling back
to object's address representation. SBCL `src/code/print.lisp`'s integer printer
likewise separates numeric digit output from higher-level print policy. Raw
integer storage is read directly, so a subclass's `__int__` override does not
change base integer representation.
One-argument integer construction invokes the source's `__int__` descriptor
in compiled Python before raw conversion, including overrides on integer
subclasses. Base `int.__int__` and `__index__` read canonical integer storage;
they do not retain a subclass as their result. This moves override selection out
of the bootstrap numeric conversion path, with the same descriptor binding and
result validation as other compiled protocols.

### POSIX signal dispositions and masks

SBCL `src/code/target-signal.lisp`, `enable-interrupt` and `%install-handler`,
translates default/ignore dispositions before crossing the native OS boundary.
Mule's compiled `_signal` similarly owns signal-number conversion, default/ignore
handler validation, mask iteration and Python sets; Rust binds libc disposition,
mask, pending-signal and synchronous wait services. Linux realtime limits come
from libc, and valid signals come from its filled mask. These operations do not
execute Python in an asynchronous signal handler. Callable-handler activation
is explicitly rejected until compiler interrupt checks and deferred delivery
are implemented, following SBCL `src/code/signal.lisp`'s separation of pending
interrupt state and safe language-handler invocation. The normal compiled
`default_int_handler` raises `KeyboardInterrupt` when called directly; automatic
SIGINT delivery is not yet installed. No new library is linked.

### POSIX clock records

Clock queries retain integer seconds and nanoseconds from the kernel until
compiled Python constructs a nanosecond integer or floating seconds. The public
`clock_gettime`, `clock_gettime_ns`, and `clock_getres` functions convert
indexable clock identifiers to signed C ints and use the requested kernel clock;
invalid identifiers raise ordinary errno-bearing exceptions. Named nanosecond
clocks share this path and never round through floating point.
Nanosecond results retain Python 3.14's signed 64-bit time range. The
`clock_gettime*` argument adapters preserve its legacy conversion-error rewrite;
`clock_getres` preserves ordinary index and signed-range conversion exceptions.

SBCL `src/code/unix.lisp`, `clock-gettime`, returns separate integer seconds
and nanoseconds from a physical timespec. Mule uses the same foreign-storage
boundary; Rust returns errno and those two fields, and compiled Python owns
argument conversion, time-unit arithmetic, and error construction. Python exposes
arbitrary Linux clock identifiers and resolutions rather than SBCL's selected
internal clock units. No clock-resolution constant replaces a kernel result.

### Exact float hexadecimal rendering

`float.hex()` separates binary64 storage access from Python rendering. SBCL
`src/code/float.lisp`, `integer-decode-double-float`, extracts exponent,
significand and sign using `double-float-bits`; Mule's physical
`math_float_bits` export copies that same stored word into an unsigned Python
integer. It does not call numeric conversion hooks or perform formatting.
Compiled `_object.float.hex` owns descriptor validation, special-value names,
signed zero, the thirteen hexadecimal fractional digits, and the signed
binary exponent. Python requires a hexadecimal string rather than SBCL's
multiple integer-decode values, and represents NaNs and infinities without
raising the decoding condition. This adaptation preserves each finite stored
value exactly and costs a word extraction plus bounded integer/text work,
with no decimal conversion or foreign formatting routine.

`float.as_integer_ratio()` follows SBCL `float.lisp`, `rational`: decode the
integer significand and power of two, remove trailing zero bits while the
exponent is negative, then shift either numerator or denominator. Python
returns a coprime integer pair instead of a Lisp ratio object; its infinity
and NaN exceptions are selected in compiled Python. `float.is_integer()` also
uses the stored exponent and fraction, so signed zeros return true and
nonfinite values return false without invoking subclass conversion hooks.

### Descriptor metadata and builtin error frames

`fstat` passes a signed file descriptor directly to libc and returns errno plus
physical metadata fields. No Rust `File` or `OwnedFd` is constructed from a
possibly invalid descriptor, and no descriptor ownership is acquired or released.
SBCL `src/code/unix.lisp`, `unix-fstat`, uses its `fstat_wrapper` OS boundary;
Mule declares the Linux x86-64 stat ABI in Rust instead of adding a C wrapper.
The current generator runtime already requires x86-64. The fixed ABI layout is
checked at compile time, and compiled `posix` constructs `stat_result` and
ordinary errno-bearing exceptions. This preserves descriptor lifetime and
kernel `EBADF` behavior without adding an extra validation/duplication syscall.

The compiled `posix`, `_csv`, and math implementation modules use the explicit
builtin-implementation traceback declaration. Python builtin APIs omit these
implementation source frames; user callbacks remain visible. Factorial's
negative-argument message matches Python 3.14.

### Native executable identity

`sys.executable` and `_base_executable` name the actual running ELF image,
independently of the script/module/command spelling in `sys.argv[0]`. SBCL
`src/runtime/linux-os.c`, `os_get_runtime_executable_path`, reads
`/proc/self/exe` separately from `posix_argv`, and `src/code/common-os.lisp`,
`os-cold-init-or-reinit`, initializes `*runtime-pathname*` separately from
`*posix-argv*`. Mule follows that Linux OS boundary through
`_posix.executable_path_bytes`; compiled `sys` decodes the byte pathname with
UTF-8 and surrogateescape and snapshots it during module initialization. An
OS lookup failure yields the empty string, matching Python's unavailable-path
contract. Generated programs remain individual native images: the returned
path restarts that program, rather than selecting a new source file to compile.

### Registered text codecs

String encoding and byte decoding retain their physical UTF-8, ASCII, Latin-1,
and escape-codec paths; other names use the existing compiled codec registry.
The selected entry must be a text codec, and its encoder/decoder tuple and
result type are checked before returning. Decoder callbacks receive a
memoryview over the physical bytes, preserving subclass-independent buffer
access and callback retention of that view. UTF-8 signature handling uses the
existing `encodings.utf_8_sig` module, including its incremental BOM state.
Omitted error arguments remain omitted when invoking registered callbacks;
an explicit error-handler name is passed as the callback's second argument.
The existing UTF-16/32/7 primitive adapters still reject unsupported conversions;
they cannot reenter their own registry entry through the text-method fallback.

SBCL `src/code/octets.lisp`, `parse-external-format` and `octets-to-string`,
selects registered external-format functions before invoking them over physical
array data. Mule keeps lookup and conversion policy in compiled language code;
Python adds codec search callbacks, the text-codec flag, callback result checks,
and byte-buffer views. No native codec implementation or host evaluator is added.

### Linux CPU affinity

`posix.sched_getaffinity` and `sched_setaffinity` convert process IDs and CPU
identifiers in compiled Python. The getter grows its physical bit mask when
Linux reports `EINVAL`, then returns a Python set of enabled CPU identifiers.
The setter consumes an arbitrary iterable, validates indexable CPU identifiers,
and builds the mask before changing the calling thread's or selected process's
affinity. Native services only allocate aligned mask storage, call libc, and
return physical bytes and errno; compiled Python constructs `OSError` values.
No fixed CPU-count limit or synthetic affinity replaces the kernel result.

SBCL `contrib/sb-posix/macros.lisp`, `define-call-internally`, generates compiled
argument conversion and syscall error handling around `alien-funcall`;
`contrib/sb-posix/interface.lisp` uses this for process and descriptor services.
SBCL does not expose affinity through that interface in this checkout. Mule
adds Linux's dynamic mask services to preserve Python's public OS API, keeping
mask iteration and validation in compiled language code. The packed mask avoids
passing Python container policy into Rust and grows only as required by the
kernel or a validated CPU identifier.

The system module snapshots `PYTHON_CPU_COUNT` during initialization. A positive
signed C-int value overrides `cpu_count` and `process_cpu_count`; absent or
`default` values leave the real system/affinity query active. Later environment
changes do not alter the startup setting. Invalid values raise a normal
`ValueError` during compiled system-module initialization, rather than CPython's
pre-interpreter fatal configuration diagnostic: standalone Mule has no CPython
preinitialization phase. SBCL `src/code/toplevel.lisp` processes language options
in compiled Lisp, separate from native runtime option parsing; this Python
configuration likewise stays in compiled language code. The Mule command does
not yet accept `-X cpu_count`.

### Heap class descriptor cleanup

Explicit `types.MethodType(function, instance)` construction validates a
callable and non-None receiver in compiled Python, then uses the same physical
bound-method pair as descriptor binding. SBCL `src/pcl/boot.lisp`,
`make-method-function` and `real-make-method-initargs-form`, constructs callable
method machinery in compiled language code. Python's explicit receiver pair
differs from CLOS specializer dispatch; this exposes the existing storage and
call protocol without adding native language-level argument rules.

The physical keyword argument view also retains both receiver prefixes for a
bound callable object: the callable's own `__call__` receiver precedes the
explicitly bound receiver. Ordinary calls still borrow their existing buffers;
the nested binding case uses a temporary prefixed buffer. SBCL
`src/pcl/combin.lisp`'s method-call machinery likewise passes method call
arguments separately from callable machinery; Python requires ordered receiver
prefix composition through its existing native ABI.

Dynamic `type` construction validates and mangles declared reference slots in
compiled Python, passes their names to the existing fixed-layout allocator, and
installs compiled member descriptors backed by direct storage primitives.
SBCL `src/pcl/std-class.lisp`, `std-compute-slots` and
`std-compute-slots-around`, similarly derives effective slot locations before
allocating instances. Mule retains fixed physical slots for dynamically created
classes instead of treating slotted dataclass fields as dictionary attributes.
Existing runtime layout merging still shares repeated inherited field names;
distinct storage for Python's shadowed base slots remains unavailable.
Class-body namespaces contain `__doc__` only for an actual leading docstring;
`type.__new__` supplies the default later. This permits a declared `__doc__`
slot without falsely reporting a conflict with an implicit None binding.

`sys._clear_type_descriptors` removes a heap class's own `__dict__` and
`__weakref__` attributes before dataclasses copy its namespace into a replacement
slotted class. Other attributes and inherited descriptors remain intact;
builtin implementation classes reject the operation. SBCL
`src/pcl/defclass.lisp`'s `defclass` canonicalizes direct slots separately from
inherited layout metadata; Python requires this additional namespace cleanup
when decorators replace a class. Descriptor mutation remains compiled Python.

### Unicode character names

`unicodedata.lookup` searches generated Unicode 16 canonical names, formal
aliases, and named sequences. ASCII case normalization and argument validation
run in compiled Mule Python. Hexadecimal ideograph names use generated assigned
ranges and reject noncanonical padding; Hangul names use the ordinary name
table. The generator uses host UCD APIs only while producing checked-in data,
plus the versioned Unicode Consortium alias and sequence files in
`tools/unicode/`. Native programs never call the host Unicode implementation.

SBCL `tools-for-build/ucd.lisp` builds character-name data; its
`src/code/target-unicode.lisp`, `name-char`, uses generated perfect hashes and
verifies names through the inverse character-name map. Mule uses sorted packed
records and binary search instead: Python lookup also returns multicharacter
named sequences, and the existing normalization data already uses sorted packed
records. This adds logarithmic name comparisons rather than SBCL's constant
hash lookup, while avoiding one heap object per stored name. Names and results
remain immutable byte/codepoint data; Rust adds no Unicode algorithm.
Python's accepted aliases, sequence results, and failure exceptions replace
Common Lisp's base-character aliases and `U+` fallback.

### Object reconstruction

Object reconstruction for copy/deepcopy uses the pickle reduction protocol:
allocate through `copyreg.__newobj__` or `__newobj_ex__`, then restore dictionary
and slot state without invoking `__init__`. State and new-argument hooks remain
compiled Python, with physical instance namespace reads provided by the runtime.
SBCL `src/code/target-defstruct.lisp`, `copy-structure` and `%copy-instance-slots`,
similarly allocate an instance before transferring its slots, independently of
its constructor. Python adds memoized recursive copies, state hooks, reduction
overrides and keyword arguments to `__new__`; those policies stay in compiled
language code. This does not establish complete pickle coverage for every native
storage family.

### Computed import dependencies

SBCL `src/code/target-load.lisp` loads precompiled FASLs or source through its
resident compiler; its `src/compiler/seqtran.lisp` `concatenate` transform reasons
about constant sequence operands before lowering. Mule's standalone images do
not yet contain a self-hosted compiler. Its dependency scan therefore records
literal import names and literal package prefixes in `import_module` or
`__import__` arguments before linking. Local assignment aliases and conditional
arms are inspected conservatively; no branch is evaluated and no program source
is executed. A prefix such as `"package." + selected` makes immediate package
modules available as compiled artifacts, without recursively bundling descendants.
This is an artifact-availability adaptation rather than a Python import rewrite:
runtime lookup, module initialization and failures still follow importlib.
The public builtin `__import__` delegates to compiled importlib's existing
implementation, including fromlist and relative-package handling; level arguments
use the integer protocol. Literal dependencies reserve parent packages as well
as children so this entry point can initialize the same package chain.
Directory inventories and all source reads are cache inputs. Other arbitrary
computed names still require an explicitly linked module. The scan may retain
extra modules when a syntactically named import method is shadowed; this costs
build time and image size but executes no additional module bodies.

Image contents follow imports alone: the modules a program imports, those they
import, and literal `importlib` and `__import__` names the scan above finds. No
project file or directory membership adds modules. A name selected only at run
time from a path, such as a pytest `conftest.py` or an installed entry point,
is not linked unless some compiled source imports it, and importing it then
raises `ModuleNotFoundError`. SBCL `src/code/target-load.lisp`, `load-stream-1`,
distinguishes compiled-file loading from source loading. Mule keeps that
compiled-artifact boundary here: imports determine image contents, while module
bodies initialize only when imported.

Native startup also installs the image's configured search roots before
compiled `sys` initializes its mutable `path` list. The compiled finder delegates
distribution discovery to `importlib.metadata.MetadataPathFinder`, so installed
entry-point metadata can be read along those paths. Loading an entry point
still requires its module to be linked by an import. SBCL `src/code/toplevel.lisp` similarly
exposes startup facts through language-owned state such as `*posix-argv*`.
Python additionally requires module roots for import and distribution discovery;
Rust retains only the image's immutable strings and builds their physical tuple.

Module resolution also supports namespace packages: directories without
`__init__.py` are accumulated across search roots unless a regular package or
source module wins. Such a package has an empty compiled initialization body,
no source filename, and the collected package search paths. Its children can
use relative imports. Search probes and directory inventories invalidate cached
resolution when a regular package is added. Native image search paths are fixed
at build time; runtime extension of namespace paths is not yet supported.

`posix.fspath` is the shared compiled path conversion used by `os` and
filesystem wrappers. It binds the type's `__fspath__` descriptor and validates
its text/bytes result before an OS primitive runs. SBCL
`src/code/target-pathname.lisp`, `physicalize-pathname`, similarly resolves
language-level pathname representations before Unix services; Python requires
its special-method protocol and preservation of text versus bytes instead.

### Format string parsing

Regex repetition enumerates exits with an explicit depth-first stack instead
of one language call per matched character. Greedy repeats visit child matches
before their exit; lazy repeats offer the exit first; possessive repeats retain
only the first selected path. Zero-width iterations can satisfy a bounded
minimum without continuing forever. SBCL's list reader in
src/code/reader.lisp similarly traverses long input in a loop while keeping
nested syntax separate from input length. Python's regex protocol additionally
requires an explicit backtracking stack and capture-state preservation. This
matching algorithm is compiled Mule Python and does not evaluate Python code.
For the regex compiler's `REPEAT_ONE` variants, the body is proven to consume
one character without capture marks. Mule scans those atoms in a loop and
enumerates greedy or lazy tail positions directly, avoiding a coroutine per
character. General repeated groups retain the explicit backtracking stack.
The semantic boundary follows SBCL's `src/code/reader.lisp` token-scanning
loops: nested syntax needs saved state, while successive independent characters
stay in one activation. This changes allocation and call overhead, not capture
or backtracking order; possessive repetition still tries only its maximal exit.

Compiled `print` selects the current `sys.stdout` or an explicit stream,
validates separators/endings, converts objects in order, writes each component
and optionally flushes. Native images link the compiled system-module unit even
when user source does not import it, so streams remain Python-owned state.
SBCL `src/code/print.lisp`, `print` and `princ`, similarly resolve stream
designators and invoke compiled output protocols. Python adds keyword-only
`sep`, `end`, `file` and `flush`, supports a replaced stdout, and skips output
when that stream is None. No Rust printing algorithm is added.

Formatting's compiled module body now runs between sequence setup and print
installation, so its helper bindings and imported physical services are
initialized before formatting calls. This follows SBCL's execution of compiled
top-level load forms instead of assuming unexecuted module globals are valid.
Named percent fields accept objects with a class-level indexing protocol,
including live instance dictionary views. Tuple arguments retain positional
expansion; strings remain scalar arguments. Ordinary and named conversions
preserve argument consumption order. Numeric `float.__int__` exposes the
existing storage truncation primitive after a compiled receiver check,
following SBCL `src/code/numbers.lisp`'s `truncate` boundary.

Streaming hash objects follow SBCL `contrib/sb-md5/md5.lisp`,
`md5-state`, `update-md5-state` and `finalize-md5-state`: retain working words,
partial block bytes and total length; process complete blocks in language code;
append algorithm-specific padding for finalization. Python's digest methods
finalize a copy so update and repeated digest calls preserve the original state.
Input validation, compression rounds and byte encoding are compiled Python,
with existing native integer and bytes storage. This uses no new library or
CPython linkage and makes no optimization or security-policy claim.

CSV dialect validation, streaming field parsing, quoting/escaping and output
assembly belong in compiled `_csv` Python. SBCL `src/code/reader.lisp`,
`internal-read-extended-token`, keeps escape and delimiter handling in compiled
language code over character buffers; `src/code/stream.lisp`, `write-sequence`,
similarly keeps output traversal above stream primitives. CSV adapts that
boundary to Python's iterator-of-text-lines and dialect protocol, with explicit
states for quoted fields, escapes, delimiters and record endings. It requires no
new Rust algorithm or linked library. Module/type identity of CPython's C
implementation is separate from the CSV data protocol.

SBCL `src/code/format.lisp`'s `%tokenize-control-string` and `parse-directive`
separate literal text from parsed directives in compiled language code. Mule's
compiled `_string` provides the Python-specific equivalent: incremental format
tuples and lazy field-name traversal. Brace escaping, bracket keys, conversion
characters and nested format specifications follow Python's syntax rather than
Lisp's tilde directives. Unicode decimal field indices use the existing Unicode
tables; parser errors arise when the corresponding iterator is advanced.
`string.Formatter` consumes this shared parser through its normal Python API.

### Process environment and its Python mapping

`posix.putenv` and `posix.unsetenv` change the process environment through native
OS primitives. They do not mutate the imported `posix.environ` dictionary.
The compiled `os._Environ` methods own updates to their backing dictionary;
deletion therefore removes a key once, and mapping `pop` can return its value
without a second deletion raising `KeyError`. Direct POSIX changes leave the
Python mapping unchanged, as in CPython.

SBCL `contrib/sb-posix/interface.lisp`, the `setenv` and `unsetenv` `define-call`
forms and `putenv`, changes native environment storage through foreign calls.
Mule keeps that OS boundary. Python additionally exposes a cached dictionary
and an `os.environ` wrapper over it; that mapping policy stays in compiled
Python. Removing dictionary mutation from the POSIX wrappers avoids duplicate
updates and does not add runtime dispatch or change the native call cost.

### POSIX child execution and waiting

SBCL `src/code/run-program.lisp`, `run-program`, prepares argument/environment
vectors and descriptors before invoking native `spawn`, then reads an error
channel to distinguish fork success from exec success. Mule's compiled
`_posixsubprocess.fork_exec` follows that boundary: Python converts paths,
arguments and numeric options; Rust prepares C buffers before fork and performs
only OS operations in the child. Descriptor remapping, close-on-exec error pipes,
cwd/session/group/identity setup and exec candidates use libc. Child errors use
the subprocess error-channel format, with no heap allocation or language
execution after fork. Parent `waitpid` returns raw OS status; the compiled POSIX
module owns status decoding. Python pre-exec callbacks are explicitly unsupported
until a safe compiled callback path exists. This adds no linked dependency or
source evaluator. Pipe descriptors are noninheritable by default, as required by
Python's descriptor lifetime contract.

### Native process exit

`SystemExit` asks its code value for an exit-status pair in compiled Python:
integers (including bool and subclasses) supply a process status, `None`
supplies zero, and other values supply status one and their text. Integer
status handling follows the 64-bit Linux native-long boundary and the OS's
low byte; integers outside native-long range yield 255. The native runner
only reads the resulting status/text storage and writes the requested message.

SBCL `src/code/cold-init.lisp`, `exit`, keeps status policy in compiled Lisp
before handing the status to native termination. Mule follows that boundary;
Python's `SystemExit.code` and integer subclasses require the value-owned
protocol. This migrates the runner's previous tag-only exit policy out of Rust
and prevents boxed `IntEnum` statuses from being printed as errors.

### Writable buffer reads and resizing

Descriptor truncation converts indexable arguments and signed native ranges in
compiled `posix.ftruncate`; a physical libc call changes the file length without
changing the descriptor's seek offset. Interrupted calls retry. SBCL
`contrib/sb-posix/interface.lisp`'s `ftruncate` `define-call` follows the same
descriptor/off-t/error boundary. This enables normal `_pyio` truncation used by
pytest's output capture.

`os.readinto` validates a writable contiguous view in compiled Python and copies
the physical read result into that view. `bytearray.resize` pads with zero bytes
or truncates, and all length-changing operations reject outstanding buffer
exports. Each memoryview owns a native lifetime guard; releasing or destroying
the view drops its guard, and cycle collection traverses its backing owner.
SBCL `src/code/stream.lisp`'s `read-sequence/read-function` similarly has generic
element-copy paths and a specialized `read-vector/fast` path. Mule bytearrays
currently use reference-array storage rather than a raw byte payload, so this
generic path allocates an intermediate bytes object and copies its elements.
A raw-payload specialization remains future representation work; no zero-copy
claim is made. Resize policy stays in Python, with Rust limited to owner counts
and guard lifetime.

### Codec registry initialization

SBCL `src/code/fd-stream.lisp`'s `register-external-format` records compiled
encoder/decoder functions for stream construction. Mule's compiled `_codecs`
likewise initializes `encodings`, normalizes names, queries registered search
functions and caches positive entries. Custom registration/unregistration and
encoding/decoding dispatch remain Python algorithms; existing text primitives
continue to own physical storage. Initialization is lazy and recursion-safe
because Python's `encodings` package imports `codecs` while registering itself.
This adapts CPython's interpreter-startup initialization to standalone images.
The dependency scan supplies encoding modules as linked artifacts and runtime
lookup initializes only the selected codec. This registry increment does not
establish complete codec or incremental-decoding coverage.
Codec services are declared as callable staticmethod wrappers so assigning a
compiled service to a codec class preserves the unbound calling convention of
CPython's builtin codec functions. The existing descriptor forwards the call
and copies function metadata. This changes descriptor behavior without adding
Rust call dispatch; exact builtin function type identity remains incomplete.
The compiled `operator` services use the same nonbinding wrappers. A service
stored as a class attribute, such as `glob._StringGlobber.concat_path`, receives
only its explicit arguments when accessed through an instance. Callable
getter classes retain their own normal class behavior. SBCL
`src/code/simple-fun.lisp` keeps ordinary function calling separate from
funcallable-instance metadata; Python additionally needs the descriptor rule
distinguishing builtin services from source-defined functions. The adaptation
uses the existing compiled `staticmethod` descriptor and adds no Rust dispatch.

The UTF-8 decoder reports consumed bytes separately from decoded text and
buffers incomplete terminal sequences when `final` is false. Continuation
validation uses the existing compiled UTF-8 scanner; decoding a complete
sequence still applies the requested error policy. Incomplete surrogate
sequences defer their error until completion, matching incremental decoding.
SBCL `src/code/external-formats/enc-basic.lisp`'s
`define-bytes-per-utf8-character` similarly separates continuation validation
from the end-of-input boundary; Mule exposes Python's `(text, consumed)` codec
protocol instead of SBCL's stream refill protocol.

The compiled text layer also implements `unicode_escape` and
`raw_unicode_escape`: ordinary bytes map to Latin-1 codepoints; escape decoding
handles hexadecimal Unicode values, surrogates, octal and simple escapes,
and preserves incomplete terminal escapes for incremental decoding. Raw escape
decoding interprets only `\\u` and `\\U`. Encoding walks physical text storage;
no host codec or evaluator is called. SBCL's `enc-basic.lisp` external formats
similarly keep decoding, partial-input handling, and error policy in compiled
language code. Python's escape syntax and `(text, consumed)` interface are the
concrete adaptations. Named `\\N{...}` escapes use the compiled Unicode name
database, preserve incomplete input for incremental decoding, and reject named
sequences because an escape denotes one codepoint.
Malformed escapes use the shared `_codecs` error registry, including custom
replacement and resume positions. Invalid ordinary and octal escapes issue one
deprecation warning per successful call. The private warning backend can
initialize its own module state when a builtin warns before public `warnings`
is imported; subsequent public initialization installs the usual mutable
warning-module interface.

`UnicodeEncodeError` and `UnicodeDecodeError` expose encoding, input, start,
end, reason, their original argument tuple, and the codec-specific message.
These remain ordinary compiled exception classes, following SBCL's
`src/code/target-error.lisp` condition slots and report functions. Python's
five-argument Unicode errors and byte/codepoint-specific messages require these
class methods; Rust does not format or dispatch them.

### OS random bytes

The compiled `fcntl` module exposes descriptor flags, integer and bounded
byte-buffer `fcntl` arguments, `flock`, and `lockf` on Linux. Descriptor
conversion, lock-mode policy, EINTR retries, and exception construction run in
Python. `_posix` supplies the corresponding kernel calls and returns errno
with the physical result. Byte arguments use a zero-initialized 1024-byte
scratch buffer and return exactly the supplied length; unlike a native stack
buffer with unspecified trailing contents, this does not expose uninitialized
memory to a kernel command. Oversized inputs are rejected before the syscall.
`ioctl` supports integer arguments and contiguous buffers. Immutable arguments
are bounded to 1024 bytes; mutable buffers can be larger. The OS boundary uses
an owned scratch buffer, and compiled Python copies successful mutable results
back through a byte-format memoryview. Typed contiguous views therefore use
their byte length and retain their original format and storage.
SBCL `contrib/sb-posix/interface.lisp`, `fcntl-with-int-arg`,
`fcntl-with-pointer-arg`, and its `flock` protocol class, likewise separate
language-side argument conversion from native descriptor services. Mule uses
Python's byte-buffer interface and Linux's `struct flock` ABI instead of SBCL's
alien protocol object. No CPython code or headers are linked.

Compiled `OSError` construction selects the standard errno-specific subclasses
and retains errno, strerror, optional filenames, and blocking-write counts.
Its class formats the corresponding `[Errno ...]` message. This extends the
compiled condition-slot/report-function boundary described above; syscall
wrappers can raise ordinary structured exceptions without Rust selecting their
language-level class. Older native services that still report a preformatted
single message do not yet supply structured errno fields.
`posix.strerror` obtains the OS error message as bytes and decodes it in compiled
Python, following SBCL `src/code/misc-aliens.lisp`'s `strerror` foreign-service
wrapper. It no longer substitutes an unknown-error message for every errno.

The compiled `resource` module provides Linux limits, `getrlimit`, `setrlimit`,
`prlimit`, `getrusage`, and `getpagesize`. `_posix` exposes the kernel results
as primitive integers and tuples. Python validates resource IDs, unsigned native
limit values (with `-1` representing infinity) and two-element limit sequences,
maps setting errors to the
specified exceptions, and wraps usage rows in a tuple subclass with named
properties. SBCL `src/code/unix.lisp`, `unix-getrusage`, similarly marshals the
OS usage record at the foreign boundary. Mule returns Python's floating-point
CPU seconds and `struct_rusage` interface instead of SBCL's microsecond multiple
values. Windows limits and complete struct-sequence constructor/repr parity
remain outside this Linux implementation.

The Linux `termios` wrapper validates the seven-element attribute list and its
control-character list, converts canonical-mode control characters to bytes
and noncanonical `VMIN`/`VTIME` to integers, and exposes terminal actions and
window-size operations. `pty.openpty` uses the new POSIX allocation service;
both returned descriptors are noninheritable and remain caller-owned. Native
services copy the Linux libc `termios` record and invoke OS functions, while
Python owns errors, attribute policy and buffer conversion. SBCL
`contrib/sb-posix/interface.lisp`, `termios`, `tcgetattr` and `tcsetattr`, uses
the same foreign-record boundary with a language-owned protocol class. Mule
adapts it to Python's list interface, and `src/code/unix.lisp`'s `unix-ioctl`
provides the reference for descriptor/request/buffer services. Terminal ABI
constants are checked-in data generated explicitly on 64-bit Linux by
`tools/generate_terminal_constants.py`; no host Python API or CPython library
is consulted by native programs.

The compiled `posix.urandom` wrapper, also exported through `os`, applies the
integer-index protocol and rejects negative lengths. Its `_posix.entropy_bytes`
primitive allocates byte storage and fills it through Linux `getrandom` with
flags zero, retrying interrupted and short reads. Zero length returns empty
bytes; unrepresentable lengths and allocation/OS failures raise errors.

SBCL `src/code/target-random.lisp`, `os-random-seed`, obtains OS entropy through
`/dev/urandom` and passes it to compiled Lisp random-state initialization. Mule
keeps entropy acquisition as an OS service; Python's `os.urandom` additionally
requires a byte result of the requested length. Linux `getrandom` blocks until
the kernel RNG is initialized and avoids descriptor management. Unlike SBCL's
noncryptographic seed fallback, this API does not substitute time or PID when
entropy acquisition fails. The wrapper is compiled Python, and Rust contains
only allocation and syscall handling; no PRNG algorithm is added there.

### Compiled random core

`stdlib/_random.py` supplies the MT19937 core used by the existing compiled
`random` module. All initialization, array seeding, twisting, tempering,
53-bit fraction generation, bit generation, and state transfer execute as
compiled Python. The initial seed boundary supports `None` (OS entropy) and
integer-index seeds; generic hashable core seeds and the public module's
SHA-512 string seed dependency remain gaps. No native evaluator or Rust PRNG
implementation is used.

SBCL `src/code/target-random.lisp`, `init-random-state`, `seed-random-state`,
`random-mt19937-update`, and `random-chunk`, provides the same 624-word MT19937
state, 397-word recurrence, array initialization multipliers, and tempering
masks. Mule stores the state as packed unsigned 32-bit words in byte storage;
a compiled word-buffer class owns extraction and replacement. Unlike SBCL's
machine-word array VOPs, each word currently requires four byte accesses.
The explicit little-endian packing keeps physical state independent of the
host's byte order and reuses Mule's existing mutable byte storage; it adds no
boxed object per stored word. Combining those byte accesses into a direct
word load/store remains compiler optimization work, not a speed claim.
Python's stable random sequence requires little-endian seed word extraction,
two tempered words for each 53-bit fraction, and low-word-first `getrandbits`.
These sampling and state API adaptations stay in compiled Python.

### Initial sys module

`sys` is compiled from `stdlib/sys.py`, with shared decoding in
`stdlib/_mule_codecs.py`. Both resolve to repository implementation sources before
source-local files or packages. `_mule_native` is a built-in native module, not
a Python source file. Ordinary modules retain their existing resolution order.
Python source imports native exports explicitly; there are no undeclared process
intrinsics or module-specific identifier substitutions.

Generated native `main(argc, argv)` publishes borrowed, process-lifetime argument
pointers before any compiled module initialization. The native module exports the
raw startup count and an opaque read-only view of the pointer vector. Compiled
Python reads its elements, copies each C string to bytes, decodes those bytes with
the shared compiled UTF-8 decoder, and constructs the mutable `sys.argv` list.
Rust does not select Python arguments, decode their bytes, or construct the list.
The module cache owns it through normal module globals: repeated imports share
mutations and reassignment, and
each executable invocation constructs its own state. `argv[0]` is the native
executable invocation string, not a canonical path or the compiler's source path.
An empty native argument vector produces an empty list.

SBCL `src/runtime/runtime.c`, `main`, publishes `options.argv` as `posix_argv`
before entering Lisp. `src/code/common-os.lisp`, `os-cold-init-or-reinit`, uses
foreign pointer access to construct the language-level `*posix-argv*` list. Mule
keeps that boundary: the OS storage is native, while the list and module state
are compiled Python. Python requires a mutable list in a module rather than a
Lisp special variable. LLVM's executable entry receives the platform argc/argv
ABI directly because Mule has no separate native launcher processing options.

SBCL `src/code/target-alieneval.lisp`, `extern-alien` and
`define-alien-routine`, declares foreign access explicitly and constructs callable
interfaces. `src/code/c-call.lisp`, C-string naturalization, delegates encoding
conversion to the shared language-level external-format implementation;
`src/code/external-formats/enc-basic.lisp`, `read-from-c-string/utf-8/lf`, validates
and constructs decoded strings in Lisp. Mule's explicit native module exports
provide ordinary imported callable values using Mule's existing boxed-value ABI.
This is an initial fixed set of foreign-storage facilities, not arbitrary foreign
symbol loading or a general user-authored FFI declaration system. Python decoding
and argument traversal remain normal compiled language code.

The private decoder remains strictly UTF-8. Compiled Python checks leading and
continuation bytes, length, overlong sequences, surrogates, and the maximum
codepoint, raising `UnicodeDecodeError` with encoding, data, offsets, and reason
in its argument tuple. No bytes are replaced or ignored. Unlike Python's POSIX
filesystem decoding, locale-derived encoding remains a gap for that entry.
The exception exposes specialized properties and formatting through its
compiled class methods. Text storage already permits lone surrogates. Native
argv now uses the shared compiled bytes decoder with UTF-8
and surrogateescape, preserving undecodable POSIX argument bytes instead of
raising during system-module initialization. Strict decoding is linear in byte
length, collects codepoints
in a Python list, then calls generic physical text construction once. This avoids
repeated immutable-string concatenation and its quadratic copying cost.

### Explicit native storage module

The existing generated import cache owns the `_mule_native` namespace and its
real function objects. Native initialization is an explicit import variant: it
populates the same namespace, returns owned success or null with an error, and
uses the existing failure cache reset and reference cleanup. It has `__name__`
but no fabricated `__file__`. Imports, aliases, passing callables, and module
attribute rebinding retain ordinary Python behavior; no call-site spelling is
rewritten. This does not introduce a separate runtime module registry or evaluator.

Explicit module bindings, including import aliases, take precedence over implicit
protocol primitive bindings, both for stores and for global reads inside functions.
Unshadowed protocol primitives still resolve to known functions. SBCL separates
these lookups in `src/compiler/ir1tran.lisp`, `find-free-var` and `find-free-fun`;
Mule must account for Python's shared variable/callable namespace while retaining
SBCL's invariant that assignment destinations are variable leaves, not function
leaves. Shadowed primitives use ordinary mutable globals rather than unconditional
primitive lowering.

Exports are positional-only physical operations:

| Export | Storage contract |
| --- | --- |
| argc | Native startup integer count. |
| argv | Read-only borrowed pointer vector; includes the NULL sentinel for nonempty native vectors. |
| read_pointer(vector, index) | Guard the vector kind and index; return an opaque borrowed C pointer, or None for NULL. |
| read_c_string(pointer) | Copy valid NUL-terminated foreign storage into owned bytes without decoding. |
| byte_length(data) | Guard byte storage and return its physical byte length. |
| read_byte(data, index) | Guard byte storage and bounds, then return an unsigned byte integer. |
| text_from_codepoints(values) | Guard list/tuple integer storage and codepoint bounds, encode the existing text representation, and allocate owned text. |
| math_number_kind(value) | Return physical kind 0 for other storage, 1 for integer/boolean storage, or 2 for binary64 storage. |
| math_float_bits(value) | Return the exact unsigned binary64 storage word; reject non-float storage without conversion. |
| math_to_double(value) | Convert builtin numeric storage to an owned binary64 value using the existing checked conversion. |
| math_integer_value(value) | Validate/retain integer storage and normalize boolean storage to an integer. |
| math_integer_bit_length(value) | Return the bit length of the stored integer magnitude. |
| math_special_method(value, attribute) | Return the existing special-method binding or None; validate text attribute storage. |
| math_sqrt(value) | Apply physical binary64 square root to float storage, returning the raw IEEE result; Python chooses domain errors. |
| math_inf, math_nan | Owned binary64 infinity and quiet-NaN constants. |

Only native-created pointer views are available; Python cannot construct one
from an arbitrary integer. Their storage is process-lifetime borrowed and is
never freed by the wrappers. The startup ABI guarantees valid NUL termination
and vector storage; generic C-string access relies on that explicit foreign-memory
contract. Vector bounds and pointer kinds are checked before dereferencing.
The view is read-only, but native-module attributes are ordinarily rebindable.
Compared with SBCL's raw alien pointers, these opaque checked views are a safety
adaptation with one small wrapper allocation per pointer read and a byte copy per
C-string read. Rust handles physical access, allocation, and storage checks; it
does not choose a decoding policy or expose argument-to-text operations.

The `chr` builtin added with this work also imports `text_from_codepoints`
explicitly. Older class/sequence/format private intrinsics remain bootstrap debt
outside this migration; new sys and codec code do not depend on that convention.

Only `argv` is available in this increment. Standard streams, descriptor access,
buffering, text codecs, and `detach` remain work to enable the existing compiled
Python `_pyio` implementation. This increment does not add a second stream
hierarchy or extend Rust with stream algorithms, and does not establish that
the unmodified Benchmarks Game programs compile or execute.

### Compiled initialization and imports

The public `ast.parse` interface and the `compile` builtin use Mule's
handwritten parser and owned syntax nodes, then a compiled visitor converts
them to the Python AST interface classes in `_ast`. When `PyCF_ONLY_AST` is
requested, `compile` returns that converted `_ast.AST` tree; otherwise it
validates load/store/del target contexts and returns a `code` object retaining
the validated `_ast.AST` node and mode. `compile` also accepts an already
constructed `_ast.AST` tree (including synthetic trees with non-identifier
`ast.Name` nodes used by test-runner expression matchers). The `eval` builtin
evaluates expression-mode `code` objects or source strings through a compiled
AST expression walker (`_ExpressionEvaluator`), following SBCL
`src/code/eval.lisp`'s `simple-eval-in-lexenv` tree-walk evaluator for runtime
forms before a resident native JIT is linked into standalone executables.
The compiler continues consuming its owned nodes directly for all static modules
and functions. No host parser, bytecode interpreter or CPython linkage is used.
The standard `mule` package exposes the same parser/AST implementation sources;
numeric annotation imports remain compiler declarations.

Standard streams (`sys.stdin`, `sys.stdout`, `sys.stderr`) and their binary
buffers support buffered `read`, `readline`, `readlines`, line iteration, and
`write`/`writelines` in compiled Python over `_posix.read` and `_posix.write`,
preserving incomplete multi-byte UTF-8 sequences across chunk reads via
`_codecs.utf_8_decode(..., final=False)`.

Imports from mule reserve recognized numeric contracts and optimize as lexical
compiler declarations; other names remain ordinary runtime package imports.
Mixed imports preserve each path independently. SBCL src/compiler/ir1tran.lisp
distinguishes compiler-recognized declaration/known-function information from
ordinary package symbols. Python's module shares its spelling with the compiler
package, so recognizing directive exports must not hide mule.ast or mule.parser.

POSIX write validates a contiguous buffer and its descriptor in compiled Python,
then passes bytes to the physical write service. SBCL src/code/fd-stream.lisp
likewise separates buffered stream operations from the Unix write boundary.
Temporary buffer views retain their owner through the syscall and release on
success or failure. Binary buffer subclasses use physical storage rather than
their Python indexing or iteration overrides.

chmod and fchmod use compiled PathLike, descriptor and mode validation over
libc fchmodat/fchmod. SBCL contrib/sb-posix/interface.lisp exposes chmod and
fchmod through define-call with filename, file-descriptor and mode-t inputs.
Mule keeps the same OS boundary; Python adds dir_fd and follow_symlinks policy
above it. The OS reports unsupported symlink permission operations.

Integer byte conversion accepts one-shot byte iterables and the __bytes__
protocol, validates indexed octets, and supports either byte order and signed
two's-complement values. Byte constructors share special-method and integer
index dispatch. SBCL src/code/bignum.lisp implements byte/shift arithmetic over
integer storage; Mule likewise keeps conversion and iteration in compiled
language code over integer primitives. Python adds stream exhaustion, byte
range errors and subclass construction; no iterator or conversion evaluator
is added to Rust.
Immutable bytes use a direct storage loop after the `__bytes__` hook, bypassing
subclass indexing/iteration overrides as before. Little-endian input is visited
from its high byte so both orders use shift-and-or accumulation; signed
correction follows accumulation. SBCL `contrib/sb-md5/md5.lisp`, `fill-block-ub8`,
similarly reads octet-vector storage directly while language code assembles
words. Python's arbitrary length and subclass constructor remain compiled
policy. This path avoids an intermediate generator and its per-byte resumes.

Mule's sys.version identifies its language version, native build and LLVM
backend. Compiled platform parses this format as implementation "Mule" and
reports its version, build label and compiler; unknown build dates stay empty.
SBCL src/code/misc.lisp, lisp-implementation-type and
lisp-implementation-version, likewise reports target implementation metadata
without importing compiler-host process state. This is Python's platform
reporting adaptation for a separate implementation.

Function creation retains its leading string literal as __doc__, applying
Python 3.14's tab expansion and indentation cleanup and permitting later
assignment. Leading and trailing blank lines remain. Class and module docs use
the same conversion; the parser's source literal remains unchanged.
SBCL src/code/simple-fun.lisp,
%simple-fun-doc and its setter, similarly associates documentation with the
compiled callable instead of executing documentation text. Mule extracts the
literal during IR1 conversion and emits a retained function attribute during
IR2 function creation. Decorator processing follows creation, so decorators
observe the original documentation.

Text translation table construction runs in compiled Python and preserves
mapping values unchanged, including deletion markers and multi-character
strings. SBCL src/code/string.lisp, string-case and %capitalize, likewise
traverses physical character storage in compiled language code. Python's
translation permits replacement strings of arbitrary length, so its existing
compiled translate method accumulates codepoints instead of requiring a
one-to-one character transform. Construction validates keys and text storage
without invoking string-subclass indexing or length overrides.

Code metadata supports replace for its exposed fields. Installing a replacement
on a native function currently accepts only its own compiled code with the
iterable-coroutine marker changed; layout, executable flags, debug metadata and
foreign code transplantation are rejected explicitly. SBCL's
src/code/simple-fun.lisp separates callable native entries from mutable function
metadata in %fun-name and its setter. Python's code objects require immutable
metadata copies and per-function assignment; Mule preserves its compiled entry.
This metadata operation does not yet make a marked generator awaitable.

Socket ownership and timeout/address policy are compiled Python. SBCL's
contrib/sb-bsd-sockets/sockets.lisp keeps a descriptor in its socket class,
initializes it through an OS call and closes it through finalization; family
specific code prepares sockaddr storage above the foreign boundary. Mule uses
the same division: compiled socket methods pack Python address tuples, choose
timeouts and handle OS status, while Rust owns descriptor lifetime and libc
socket operations. Closing or detaching consumes the descriptor lease, so
language object destruction needs no Python finalizer to release the resource.
Linux sockaddr bytes are an explicit platform adaptation within the existing
64-bit Linux target. No network implementation library is linked.

SHA-1 and SHA-224/256/384/512 use the same compiled streaming state and block
compression boundary as MD5 and SBCL contrib/sb-md5/md5.lisp,
update-md5-state and finalize-md5-state. Their word widths, padding, schedules,
constants and compression follow NIST FIPS 180-4:
https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf.
All algorithm code is compiled Python; Rust supplies existing integer and byte
storage primitives. This is functional algorithm coverage, not certification.

SHA-3 and SHAKE likewise keep absorption, permutation and output selection in
compiled Python. Unlike SBCL's MD5 block driver, their FIPS 202 sponge pads a
domain-separated rate block and squeezes as many bytes as requested, so they
use a separate state class rather than MD5's length-trailer finalization.
The Keccak team's specification summary supplies the permutation, constants
and byte-oriented domain separators: https://keccak.team/keccak_specs_summary.html.
The permutation retains its 25 lanes in local variables across all 24 rounds,
with fixed theta, rho/pi and chi assignments and one final state writeback.
SBCL `contrib/sb-md5/md5.lisp`, `update-md5-block` and `with-md5-round`, similarly
load hash registers into locals and expand fixed round operations. Keccak's
25-lane mapping and constants differ from MD5's four registers; the compiled
language boundary remains the same. This removes repeated indexed container
access without changing the sponge, word arithmetic or digest API.
The sponge loads each input lane through a bounded eight-byte little-endian
storage read. SBCL's `assemble-ub32` and `fill-block-ub8` similarly assemble
fixed-width compression words from byte storage. Mule's native primitive only
copies and boxes that physical word; lane XOR, permutation, padding and
streaming remain compiled Python. This avoids creating a bytes slice and
running the general integer-conversion protocol for each lane. Final padding
is frozen once before those reads, retaining immutable physical input storage.

BLAKE2b and BLAKE2s follow RFC 7693's compiled compression boundary, retaining
the final block until digest so its final-block flag and byte counter are exact.
This differs from SBCL's MD5 length-trailer driver because BLAKE2 carries the
byte count and final flags into compression. Keys, personalization, salt and
tree parameters initialize the compiled state; copy and digest do not mutate
the caller's streaming state. No hash implementation library is linked.

Fatal-fault diagnostics use compiler-emitted immutable ASCII labels for native
activations. SBCL src/runtime/backtrace.c, print_lisp_backtrace and
print_backtrace_frame, traverses the selected control stack using compiled debug
information. Mule's existing activation registration supplies that stack; the
compiler preformats its code labels, and the OS signal service only traverses
stable native records and writes those bytes. Python-level file selection and
argument validation remain compiled Python. Signal delivery runs no Python code
and acquires no allocator or language-object locks. Records are allocated only
while fault diagnostics are enabled, then unlinked before being released.
This uses the implemented code metadata's first line, matching the current
live-frame contract; exact interrupted instruction lines remain unavailable.
Native C-stack output uses the existing libc backtrace service, warmed before
fatal handlers are installed. It does not link another implementation library.
The current single compiled thread is supported; delayed timeout scheduling
and additional user-signal registrations are not yet implemented.

Live frame metadata follows the active coroutine stack. Switching into a
generator attaches its saved activation records above the resumer's records;
yielding detaches and retains those records with their globals. A suspended
generator is therefore absent from sys._getframe's live back-chain. SBCL's
runtime backtrace machinery likewise follows the selected control stack rather
than unrelated inactive stacks. Python generators require this ownership
transfer in addition to Mule's existing native recursion-depth transfer.

Exception handlers are transferred at the same context boundary. A generator
suspended inside an except suite retains its own handled exception while the
caller sees its previous exception state. Resumption places the saved handlers
above the caller's handlers; yielding removes them again. This prevents a
suspended StopIteration handler from changing a caller's bare raise or the
GeneratorExit used by close. The existing retained-handler storage and compiler
cleanup edges are preserved; native switching only moves their ownership.

SBCL `src/compiler/main.lisp`, `sub-compile-file`, processes top-level forms
through IR1 and emits compiled functions and load-time initialization. Its
`src/code/load.lisp`, `load-as-fasl` and `fop-funcall*`, loads artifacts and invokes
compiled initialization; compiling a file does not execute its tests. Mule must
likewise compile module bodies and functions, then execute initialization at the
import site. Python requires distinct module globals, once-only initialization,
module names, and access to exported bindings. These are Python adaptations of
compiled initialization and namespace resolution, not an evaluator fallback.

The first implementation statically includes reachable Python modules in one
LLVM artifact. Each module has separately named compiler functions, its own
namespace and a cached initializer. Generated LLVM performs initialization;
Rust supplies existing namespace storage and reference ownership. Module caches
own their namespaces until executable shutdown, preserving captured globals.
Dynamic module discovery, extension loading, and concurrent imports remain
future work. Unsupported source remains a compilation error.

The executable's `__main__` namespace enters the same module registry before
its first source statement, after generated LLVM links its global cells. If
`sys` is imported later, its registration receives this namespace along with
the early imports. `import __main__`, `sys.modules['__main__']`, and functions'
captured globals therefore refer to the same mutable namespace. String
`ClassVar` and `InitVar` annotations resolve through that namespace's actual
imports and aliases; there is no fallback to an unrelated typing namespace.
Static resolution of `__main__` selects registry lookup instead of compiling
a same-named source file that would create a second executable namespace.

SBCL `src/code/load.lisp`, `aux-fop-intern`, establishes package symbol identity
before `fop-funcall*` executes compiled initialization. Python additionally
requires the executable namespace to be an importable module object, even when
no source file named `__main__.py` exists. Generated LLVM publishes the existing
namespace through the same registry primitive used for imported modules; this
adds one startup registration, without another registry or runtime evaluator.
Before `sys` creates its compiled dictionary, registry lookup borrows the
matching entry from the existing early-registration storage and returns a
retained reference. This keeps early `import __main__` on the same object;
after registration, lookup uses the compiled dictionary as usual.
Shutdown releases early registry references even if `sys` was never imported.

Failed initialization removes the module's registry entry before releasing its
namespace and resetting the initializer cache. This applies both before `sys`
registers `sys.modules` and afterward. A later import executes the body again;
the original exception is preserved across cleanup. Generated LLVM selects this
failure path; the existing native registry owner releases its retained entry,
using compiled dictionary `pop` when `sys.modules` is available. Successful
dependencies remain registered.

SBCL `src/code/load.lisp`, `aux-fop-intern`, resolves package symbols before
compiled references use them; `src/compiler/ir1tran.lisp`, `find-free-var`,
canonicalizes global leaves, and `src/compiler/x86-64/cell.lisp`,
`%set-symbol-global-value`, writes the symbol's value slot directly. Mule retains
fixed global cells and compiled initialization, but Python imports additionally
require removing a failed module registration so it can be retried. This cleanup
adds a compiled dictionary call only on initialization failure; it introduces no
source evaluation or Rust dictionary algorithm. Registry storage remains existing
native bootstrap infrastructure, not a new language-level import implementation.

This supports retries when the registry and initializer cache are the failed
namespace's only remaining owners. A failed namespace retained through a circular
import remains a gap: its compiled functions and a later retry share fixed static
global cells. Independent simultaneous namespaces for one compiled module require
additional compiler cell-ownership machinery; registry cleanup does not provide it.

Ordinary module attribute access routes through the compiled `module` class's
`__getattribute__`. Stored namespace values, including None, retain precedence
under the existing namespace lookup. After ordinary lookup raises AttributeError,
compiled Python looks up the namespace's `__getattr__` without invoking hooks and
calls it with the requested attribute string, without binding a module receiver.
An absent hook preserves the original AttributeError; a present non-callable hook
or any exception from the hook propagates normally. Hooks can cache results through
`globals()`. Global-variable reads and explicit `object.__getattribute__` bypass
this fallback. Direct access, `getattr`, `hasattr`, and ordinary from-import
attribute lookup use this path. Raw child-submodule presence checks and star
imports still bypass it and remain compatibility gaps.

SBCL `src/code/target-package.lisp`, `find-symbol`, implements namespace policy in
compiled Lisp over physical lookup; `src/compiler/ir1tran.lisp`, `find-free-var`,
keeps global-leaf resolution separate. Python's module fallback has no identical
SBCL hook. Mule implements that semantic adaptation in compiled Python, reusing
the runtime's existing class-hook invocation and hooks-disabled storage lookup.
It adds a compiled call to ordinary module attribute access, not a new Rust
namespace algorithm, evaluator, cache, or binding store.

The module dictionary property chooses the live view of its existing storage:
imported native namespaces use `_GlobalsDict`, while constructed module objects
and their subclasses use `_InstanceDict`. Dictionary writes, attribute writes,
and deletion therefore share the same owner; copying an imported module's
bindings into a constructed module does not create another attribute store.
SBCL `src/pcl/slots.lisp`, `standard-instance-access` and `slot-value-using-class`,
similarly read and write the instance's existing slot storage. Python's module
dictionary view is an additional compiled-language interface to that storage,
not a new namespace allocation or class-identity scheme.
Frame `f_globals` exposes the same compiled live mapping view, including
containment and `setdefault`, instead of exposing the physical namespace as a
module object. Writes therefore update the module's existing cells. Mapping
wrapper identity and true dictionary type identity remain compatibility gaps.
Dictionary `get` uses the shared private storage-read operation: hash tables
read their indexed entries, and internal instance/global dictionary views read
their owner's existing bindings. Static inspection can therefore use the base
dictionary reader without routing through a user's `__getitem__` override or
copying the owner's attributes into a second table. Full dictionary storage and
descriptor type validation for these views remain unfinished.

Relative names are resolved against the importing module's package, following
the same static artifact resolution as absolute imports. SBCL's FASL loader
resolves compiled initialization and named bindings (`load-as-fasl` and
`fop-funcall*`); Python additionally requires package-relative module names and
`from package import child` fallback to a submodule. Mule checks the package's
binding at runtime first, so an existing binding (including None) wins. Only an
absent binding initializes the compiled submodule and attaches it to its parent.
Potential submodule source is statically compiled, never interpreted on demand.
Directly executed source without a package rejects relative imports. Python's
dynamic search-path mutation and runtime source compilation remain unsupported.

Compiled module initializers publish `__package__` and, for source packages,
the one-directory `__path__` before executing their bodies. They initialize
`__spec__` and `__loader__` to None. The dynamic importlib loader fills those
attributes after initialization; ordinary static imports still lack complete
spec/loader descriptions. This is metadata for statically linked artifacts, following
SBCL's named FASL initialization, not permission to evaluate runtime source.
The module IR1 prologue assigns its leading text docstring to `__doc__`, or
None when absent, before user statements. It uses the ordinary compiled
global-assignment operation rather than a runtime source scan. SBCL
`src/compiler/ir1tran-lambda.lisp`, `functional-documentation`, similarly retains
documentation during source conversion; Python additionally exposes module
documentation as a mutable global binding.
After successful initialization and on later imports, the import result uses
the current registry binding, so a body can replace `sys.modules[__name__]`
with another module. The once-only cache continues to own the original compiled
namespace: escaped functions borrow that unit's scope, and its linked global
cells must remain valid until unit finalization. This Python registry
replacement has no direct SBCL package equivalent; it adapts the existing
compiled initializer's result policy without adding runtime source execution.
Each import adds a registry lookup while preserving the unit's existing root.

Linked modules also register their compiled initialization entry and immutable
source/package facts before main. A compiled importlib finder looks up those
facts and supplies importlib's `create_module` and `exec_module` loader phases.
Creation allocates a namespace and its module metadata without running the
body or inserting it into `sys.modules`. Execution links the namespace's
global cells and calls the native module body after importlib installs its
spec and registry entry. Static imports use a wrapper around these same phases
with the existing once-only cache. SBCL `src/code/load.lisp`,
`load-as-fasl` and `fop-funcall*`, likewise invokes previously compiled load
forms; Mule adapts that boundary to named Python modules in a linked image.
Rust only stores image addresses and calls an entry through its native ABI.
Finder/loader policy, ModuleSpec construction and importlib registry handling
remain compiled Python. Separating allocation from load-form execution adapts
SBCL's compiled-load machinery to Python's loader protocol, allowing callers
to observe an unexecuted module and reload an existing namespace. The legacy
`load_module` entry remains available. Unlinked names are not evaluated or
compiled at runtime.

Relinking a namespace to the same global-cell addresses is idempotent, so reload
preserves existing dictionary bindings. A second unit claiming an already
linked binding remains an error. The static wrapper owns failed-import registry
cleanup; the modern execution entry leaves that cleanup to importlib, preserving
the original exception rather than deleting the registry entry twice.


SBCL `tests/test-util.lisp`, `run-test`, invokes a test function and records errors
without abandoning later tests. Mule currently has no general exception handlers.
The initial test harness therefore runs each selected test method in a separate
invocation of the same native executable. It reports assertion failures and
runtime errors and continues with the remaining cases. Process isolation costs
startup time and does not preserve cross-test module state; it is an explicit
bootstrap test-runner limitation, not general unittest compatibility.

Runtime imports of `typing` and `typing_extensions` are ordinary module imports;
they are no longer silently discarded by the compiler. Their imported sources
must compile before execution can succeed. Arbitrary deferred annotations remain
legal independently of whether the compiler understands their metadata.

The host harness discovers ordinary undecorated TestCase classes and test methods
using Mule AST visitors, then generates direct native calls. It does not evaluate
test bodies, change assertions, prune methods, or run them in host Python.
Hooks, parameterized tests, fixtures, inheritance-based discovery and the complete
upstream unittest loader remain unsupported by this initial harness and must not
be silently omitted. The full runner remains available for those entries.

`stdlib/bootstrap/unittest.py` is a small Mule Python implementation of the
assertion interface and ordinary SkipTest exception used by the selected module.
TestCase.skipTest raises that exception; skip accounting still belongs to the
runner and is not implemented by the bootstrap harness. It precedes the vendored stdlib
in native module resolution; the released unittest source remains unchanged.
Assertion predicates and diagnostic construction compile through the normal
frontend. An assertion statement branches to a primitive that records an
AssertionError and follows the compiler's existing owned-reference cleanup.
No Rust test framework, source interpreter or CPython linkage is introduced.

SBCL `src/code/macros.lisp`, `assert`, expands a predicate into control flow and
calls `assert-error` only on failure. Mule likewise lowers the predicate and lazy
diagnostic into ordinary IR branches. Python assertions signal AssertionError;
they do not offer Common Lisp's corrective places and restarts. This semantic
adaptation preserves message evaluation only when the predicate fails.
