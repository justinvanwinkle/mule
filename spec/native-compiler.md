# Initial native compiler

Status: implemented end-to-end compiler for a limited Python subset. The priority
is a real executable before broader language support or optimization.

## Build and run

Run from the repository with Python 3.14 and `uv`:

```sh
uv run python -m mule.compiler examples/factorial.py -o build/factorial --emit-llvm build/factorial.ll
./build/factorial
```

The target is 64-bit Linux. Clang and Rust must be on `PATH`, or supplied through
`--clang` and `--rustc`. GMP's shared library is required. Generated executables
link Mule's Rust runtime, GMP, and platform services required by Rust. They
have no CPython headers, API dependencies, library links, interpreter initialization,
or Python installation paths. Program source files are read as UTF-8.

`uv run mule` runs a program the way the `python` command does:
`mule script.py args`, `mule -m module args` (a package runs its `__main__`
submodule), `mule -c command args` and `mule - args`. Options end at the program;
the remaining arguments become `sys.argv[1:]`, and `sys.argv[0]` is the script as
given, the module's absolute file path, `-c` or `-`. Only those options and `-h`
are accepted, and there is no interactive mode. Imports search the script's
directory (or the working directory for `-m`, `-c` and `-`), `PYTHONPATH`,
Mule's `stdlib/`, then the host environment's site-packages, in `sys.path` order.
Third-party packages compile from the same sources CPython would import.

The program is compiled to an executable in `$XDG_CACHE_HOME/mule` (default
`~/.cache/mule`) that then replaces the `mule` process through `execv`, so the
program's exit status and standard streams are its own. An entry is keyed by the
compiler sources, the Clang and Rust versions, and the main program; its manifest
records the digest of every source file the compilation read and every module
path it probed and found absent. The entry is reused only while all of them are
unchanged, so editing an imported module or adding a file that would shadow one
rebuilds the program. A first run costs a full native build; later runs start the
cached executable directly.

The build-step thread pool defaults to one worker per physical core available
in the calling thread's Linux CPU affinity mask. Linux sysfs
`thread_siblings_list` groups logical CPUs belonging to the same core, so SMT
siblings count once, including when only some siblings are allowed. An explicit
`Graph(workers=...)` overrides automatic sizing. Missing or empty topology is
reported as a toolchain error rather than guessed from the logical CPU count.
This sizes Mule's existing build scheduler; it does not change compiler stages
or the linker's separate thread setting. SBCL's `make-host-2.sh` and
`make-host-2.lisp` invoke the cold compiler through
`src/cold/compile-cold-sbcl.lisp`, rather than this Python worker pool. The
affinity/sysfs query is a Linux host-build adaptation to bound independent Mule
build steps by available physical cores; it changes resource use, not generated
program semantics.

SBCL `src/code/toplevel.lisp`, `toplevel-init`, also stops option processing
at `--script` and leaves the remaining arguments in `*posix-argv*` for user code;
`process-script` then `load`s the script, compiling each toplevel form in process. Mule has no
in-process loader, so it builds the whole program into an executable first; the
cache plays the role of reusing `compile-file` output.

`python -m mule.compiler` builds an executable without running it. `-o` defaults to the source path without its suffix.
`-O3` (the default, and what the `mule` command uses) compiles and links with full
LTO across the program and the runtime. `-O0` compiles the program without
optimization and links it without LTO, which is roughly ten times faster to
build; the test suite uses it except where a test inspects optimized machine
code, and the conformance suite keeps `-O3`. The Rust runtime static library is
built once per runtime source, `rustc` version and LTO mode, and kept in
`$XDG_CACHE_HOME/mule/runtime`. `--emit-llvm PATH` optionally
saves the generated IR. Output paths must differ from the source and each other.
An existing executable is replaced only after native compilation and linking
succeed. A later failure saving optional LLVM output does not undo that executable.

Python entry points in `mule.compiler` are:

```python
from mule.compiler import build, emit_llvm, lower

graph = lower("print(40 + 2)\n", filename="answer.py")
llvm = emit_llvm("print(40 + 2)\n", filename="answer.py")
executable = build("examples/factorial.py", "build/factorial")
```

## Compilation and execution

The pipeline is Mule's lexer/Pratt parser → Mule AST → IR1 flow graph → IR1
phases (optimization, local-call analysis, flow propagation, check generation)
→ IR2 per lambda and entry context (template selection, VOP conversion,
representation selection) → textual LLVM IR from the templates' generators →
Clang-generated native executable. IR1 and IR2 are SBCL's representations
adapted to Python and LLVM. See [Python and LLVM changes](python-changes.md#ir1-implementation).
There is no host AST conversion, program source embedding, bytecode generation,
or evaluator fallback.

IR1 separates control transfer (`Ctran`) from single-consumer value flow
(`Lvar`); IR2 holds every value in a TN whose storage class decides whether it
is an owned object pointer, an immortal tagged pointer, or a machine value.
`check_ir1_consistency` verifies the graph before conversion. LLVM receives
entry-block slots for TNs rather than hand-built SSA; its `mem2reg` promotes
them. Flow propagation joins local value types and integer intervals to a
fixed point, widening growing loop bounds. Local-call analysis derives
integer parameter contexts and return summaries for proven local calls; guarded
specialized entries preserve general function behavior. Proven representation facts eliminate
unnecessary checks; unknown operands use guarded native instructions with runtime
slow paths. See [Native implementation selection](native-lowering.md). Clang
builds at `-O3`, applying LLVM optimization and native code generation. Rust builds
the runtime with `opt-level=3` and `panic=abort`, as a statically linked archive.
The linker discards unused sections and removes Rust dependency debug sections.
Rust's code generation also uses LLVM. No C runtime source is compiled.

### Optimization across the runtime boundary

The native build uses full cross-language LLVM LTO: rustc produces a
bitcode static library with `-C linker-plugin-lto -C lto=fat`, and Clang compiles
Mule IR with `-flto=full`. The LLD shipped with the selected Rust toolchain links
and optimizes both at level 3. Using that linker avoids reading newer Rust LLVM
bitcode with an older system linker. Native runtime-probe `rlib` builds continue
to use rustc's ordinary Rust library format.

SBCL `src/compiler/main.lisp`, `compile-component` and `%compile-component`,
optimize IR1 and IR2 components before generating machine code, including known
local calls. Mule's semantic analyses retain that responsibility. This build
adaptation additionally exposes the Rust primitive bodies to LLVM's optimizer,
so it can inline and eliminate calls across generated code and runtime code.
SBCL does not need cross-language LLVM LTO because its backend generates code
directly and its implementation is predominantly compiled Lisp. Mule requires
this extra link-time stage because its approved Rust primitives and Python
frontend feed LLVM through different compilation drivers. It can increase build
time; it does not replace semantic proofs, representation selection, or migration
of language-level Rust bootstrap code into Mule Python.

Rust's [linker-plugin LTO documentation](https://doc.rust-lang.org/rustc/linker-plugin-lto.html)
defines the matching full-LTO modes and compatible-linker requirement. The build
must report an error when that toolchain is unavailable, rather than silently
produce a binary without cross-language optimization.

Each source function becomes a native LLVM function. Definitions construct Mule
function objects whose entry pointers refer to those native functions and whose
context carries the module globals. Functions have general callable entries and
internal entries taking owned positional arguments directly. Predicted calls
check the actual function entry and globals before using the internal entry;
other calls use Mule's general callable protocol. Both paths retain dynamic
rebinding, argument evaluation order, recursion guards, and error cleanup.
The native `main` creates the module environment
and invokes the compiled module initializer directly.

`mule/runtime/` implements Mule's objects, strings, truth testing, operators,
calls, builtins, module globals, errors, and reference ownership in Rust.
These language-level Rust implementations are bootstrap debt, not the required
architecture. New native class constructors and generic field adapters are Mule
Python in `stdlib/_native_class.py`, compiled through the normal frontend. Rust
provides their raw storage, boxing, layout validation, and lifetime primitives.
`numeric.rs` implements native small-integer operations and GMP bigint arithmetic;
`gmp.rs` owns the GMP integer storage and bindings. Signed integers in
the range `[-2**62, 2**62-1]` are encoded as immediate tagged values; small integer
literals are emitted directly as LLVM constants. Overflow promotes to a Mule
bigint object containing a GMP integer. Results that fit the native range are
demoted. GMP operands are borrowed and fresh results move into objects without
limb copies. Unique temporaries and verified local augmented assignments can
reuse bigint storage without changing saved aliases. Other heap values include floats, text, bytes, functions, and native
namespace/type/instance objects. This representation belongs to Mule and is not
CPython's `PyObject` layout. No runtime evaluates program source or bytecode.

## Supported source subset

- Literal integers, floats, text, bytes, booleans, and `None`.
- Local/global name lookup, identifier assignment, chained assignment, augmented
  assignment, and assignment expressions.
- Binary and unary operators, comparison chains, short-circuit `and`/`or`, and
  conditional expressions.
- Expression statements and positional/explicit keyword calls. Mule implements a limited builtin
  set, including `print`, `abs`, `int`, `input`, `len`, `bool`, `ascii`, `repr`,
  `str`, and `float`; arbitrary CPython builtins are not available.
- `if`/`else`, `while`/`else`, `for`/`else`, `break`, `continue`, and `pass`.
  Known fresh builtin ranges have a guarded counted LLVM loop; other iterables
  use compiled Mule protocols, including user iterators and indexed sequences.
- `try`/`except`/`else`/`finally`, exception classes and hierarchy matching,
  `raise`, and bare reraising. Handler transfers release temporaries and
  preserve locals; handler exits clear exception bindings and restore active
  exception state. A `finally` suite runs on fall-through, `return`, `break`,
  `continue`, and exception paths; a transfer inside it replaces a pending
  exception. `with` statements look up `__exit__` then call `__enter__`, call
  `__exit__(None, None, None)` on every normal exit, pass the raised exception
  class and instance (traceback `None`) on error, and suppress it when the
  result is true.
- Exception chaining: `raise X from Y` sets `__cause__` and
  `__suppress_context__`; an exception raised while another is being handled
  records it as `__context__`. `BaseException` instances carry `__cause__`,
  `__context__`, and `__suppress_context__`. Exception groups remain
  unsupported.
- Module-level function definitions with positional-only, positional-or-keyword,
  keyword-only, default and variadic positional parameters, native
  recursion, return statements, implicit `None` returns, and `global` declarations.
  Definitions can occur inside module-level control flow. Defaults are evaluated
  once per definition; compiled external entries bind general calls, and guarded
  direct entries normalize known calls. See [Function argument binding](argument-binding.md).
- Module-level fixed-layout classes with zero or one compiled-class base,
  positional/keyword construction, instance methods, bound-method values, attribute reads,
  and assignment or augmented assignment to declared instance fields. Class bodies
  contain an optional leading string-literal docstring, method definitions,
  bare annotated declarations and `pass`;
  explicitly selected native classes may use `mule.int64` fields as described below.
- Unconditional module-level imports of numeric annotation markers and
  `optimize` from `mule`, including aliases. Parameter, return, and identifier
  variable annotations support arbitrary expressions; recognized builtin and
  imported numeric contracts additionally guide compiler analysis.
  `from __future__ import annotations` is accepted as a compiler directive.
- Static imports of Python source modules, aliases, packages and named exports.
  Imported globals have separate mutable namespaces; initialization occurs once
  at execution and functions capture the defining module. Package-relative imports
  and from-import submodule fallback work. Wildcard, extension and dynamic imports
  remain unsupported. `typing` imports use this
  ordinary path and require their library sources to compile.
- Assertion statements with lazy message evaluation and native error cleanup.
- List and tuple displays with left-to-right element evaluation and owned
  contiguous storage. Lists/tuples/text/bytes support integer and `__index__`
  indexing; lists support replacement and append. List/tuple equality, iterable
  construction, `range`, `enumerate`, `iter`, `next`, and non-starred unpacking
  run through compiled Mule Python. See [Sequences and iteration](sequences-and-iteration.md).
  Starred displays, slicing, and other container algorithms remain incomplete.
- F-strings with `!s`, `!r`, debug fields, nested specification expressions,
  string width/alignment and Unicode fill characters. Formatting protocols run
  as compiled Mule Python and honor class `__format__`, `__str__`, and `__repr__`.
  Numeric format specifications, precision and zero padding are not implemented.

Function-wide binding classification makes a later assignment local even when
an earlier read occurs first. Unbound local slots raise `UnboundLocalError`;
missing global names raise `NameError`. Globals are looked up at execution time,
so builtin shadowing, function aliases, and reassignment remain observable.
Boolean operations return operands; chained comparisons evaluate shared middle
operands once. Owned references are cleaned on normal and exceptional exits,
and an active error survives cleanup.

Class docstrings accept only a text literal in the first statement, including
empty, multiline and adjacent string literals. Bytes, f-strings, computed text
and later expression statements remain unsupported in class bodies. Docstring
acceptance does not yet provide runtime `__doc__` metadata for classes, functions
or modules. SBCL `src/pcl/defclass.lisp`, `canonize-defclass-options`, validates
and preserves its `:documentation` string for class creation; Mule still needs
the corresponding documentation metadata and lookup support. This increment
only removes the class-body syntax rejection, without allocating runtime
documentation or changing class layout, construction or method dispatch.

- Nested function definitions, closures, and `nonlocal`. A variable captured
  by a nested function and assigned anywhere lives in a shared cell, so later
  assignments in either scope are visible to both and reading it before any
  assignment raises `NameError`. A captured variable that is never assigned
  after binding (a parameter, for example) is copied into the closure when
  the function object is created. Nested class definitions remain unsupported.
- `lambda` expressions. A lambda converts as a function named `<lambda>` whose
  body is a single `return`, through the same function-creation path as `def`:
  defaults are evaluated left to right in the enclosing scope when the lambda
  expression runs, and bindings inside the body (an assignment expression, for
  example) belong to the lambda's own scope, not the enclosing one.
- `del name` for locals, globals, and captured variables. Deleting an unbound
  name raises the same error a read would; later reads see the name unbound.

Unsupported constructs raise `UnsupportedSyntax`, a `SyntaxError` subclass,
before native compilation. These include dynamic/wildcard imports, nested classes,
`**kwargs` parameters, call argument expansion, general decorators,
async/generators, exception groups, dictionaries/sets/comprehensions,
slicing, extended unpacking, starred displays, template strings and `!a` conversion.
Unsupported source is
also rejected in unreachable branches. Parser support is broader than compiler
support; acceptance by the parser does not imply native compilation support.

## Print endings

`print(*objects, end="\n")` is compiled from `stdlib/_print.py` and installed as
an ordinary builtin with a generated keyword/default argument binder. `end`
accepts text or `None`; `None` selects the default newline. Empty endings,
Unicode and embedded NUL characters are written without truncation. Objects
are separated by one space; a call without objects writes only its ending.
Invalid endings raise `TypeError` before conversion or output. Conversion uses
the shared compiled string protocol, and each object and separator is written
in order, preserving partial output when a later conversion or write fails.
The result is `None`.

SBCL `src/code/stream.lisp`, `write-string` and `%write-string`, implements output
dispatch in compiled Lisp; `src/code/fd-stream.lisp`, `flush-output-buffer`,
calls the physical OS write boundary. Mule likewise keeps ending, separator and
conversion rules in compiled Python; Rust only tests physical text storage and
writes its exact bytes to the existing native stdout. Sequence/exception
protocols initialize before the print builtin is installed, and both share the
existing owned protocol namespace. The wrapper releases intermediate initializer
results and stops on failure. Existing shutdown stdout flushing is unchanged.

This is an explicitly scoped Python adaptation: print retains Mule's fixed
native stdout destination. `sep`, `file`, `flush` and dynamic `sys.stdout`
selection remain unsupported; this increment adds only `end`. It introduces
neither a second stream hierarchy nor a Rust language-level print algorithm.

## Initial math support

`math` is a compiled Python package providing `sqrt`, `factorial`, `comb`,
`perm`, `gcd`, `lcm` and `isqrt`, plus `pi`, `e`, `tau`, `inf` and `nan`.
It also provides `log`, `log2`, `exp`, `sin`, `cos`, `acos`, `lgamma`, `fabs`,
`floor`, `ceil`, `isfinite`, `isinf`, and `isnan` for standard-library startup.
The integer functions are implemented in `math.integer` and re-exported by
`math` as the same function objects. This is initial math support, not the
complete Python math API; the remaining transcendental functions and
floating-point reductions are not implemented yet.

Argument conversion, integer algorithms and domain checks execute in compiled
Python. Square root accepts builtin real numbers and the supported `__float__`
or `__index__` protocols, requires a float result from `__float__`, preserves
negative zero, propagates NaN/positive infinity, and rejects negative values
with `ValueError`. Integer operations accept builtin integers/booleans and
`__index__`, rejecting float arguments and invalid conversion results. Arithmetic
remains arbitrary precision; integer square root uses SBCL's normalized 64-bit
approximation table and successively refined high-bit prefixes with a final
exact square check. Empty `gcd` and `lcm` return zero and one respectively.

The approximation table is immutable byte storage, corresponding to SBCL's
unsigned-byte-8 vector. The large-integer leading word is extracted with a
compiled integer shift instead of SBCL's raw bignum-part accessor because Mule
bigint storage belongs to GMP, not SBCL's limb layout. The extracted 64-bit
value and seed/refinement arithmetic are unchanged; the shift can allocate a
temporary bigint rather than reading an SBCL limb directly.

SBCL `src/code/irrat.lisp`, `def-math-rtn` and `sqrt`, separates compiled language
dispatch from physical floating-point operations. Python's concrete difference
is that a negative real square root raises `ValueError` rather than producing
a complex value. `src/code/numbers.lisp`, `gcd`, `lcm` and `isqrt`, supplies the
language-level integer reduction/refinement structure. Python additionally
requires its integer-conversion protocols and combinatorial functions.

The physical bit-length query returns Python's magnitude length, including zero
for zero. SBCL's signed `integer-length` instead counts the two's-complement
representation without redundant sign bits. The difference matters for negative
integers; the square-root algorithm itself supplies nonnegative integers.

The existing `_mule_native` module supplies raw binary64 square root,
numeric-kind/bit-length queries, storage conversion and special-method access.
It uses the existing module cache and system math services; no new library,
CPython linkage or Rust integer/math algorithms are introduced. Binary64
operations currently use boxed primitive entries; unboxed float representation
selection remains compiler work, and this increment makes no speed claim.

The added transcendental operations follow SBCL `src/code/irrat.lisp`,
`def-math-rtn`, `handle-reals`, `exp`, `log`, `sin`, `cos`, and `acos`:
compiled Python converts arguments and checks domains/overflow, then calls a
physical binary64 primitive. `lgamma` uses the already-linked system libm;
it adds no library dependency. Python rejects real-domain errors instead of
returning SBCL's complex results, and reports finite-input overflow as
`OverflowError` while accepting infinite inputs where Python does.

For large integer logarithms, SBCL's `log2/nonnegative-integer` extracts the
leading double-float precision bits and adds the removed binary exponent.
Mule performs that normalization with compiled integer shifts and the existing
bit-length primitive, avoiding overflow during conversion to binary64. `log`
with a base divides the two independently computed logarithms and raises
`ZeroDivisionError` for base one, following Python. This keeps bigint
normalization and operation selection in compiled Python, at the cost of a
temporary shifted integer rather than SBCL's direct limb extraction.

`floor` and `ceil` call the value's special rounding method, falling back to
real conversion and the existing float rounding methods. Predicates and `fabs`
also perform normal real conversion in compiled Python. Physical primitives
remain boxed entries; this change adds no representation proof or speed claim.
The compiled integer `__floor__` and `__ceil__` methods normalize their result
to an ordinary integer through the existing conversion primitive, including
inherited boolean methods. The math callers do not inspect operand classes;
subclass overrides and user rounding methods keep ordinary dispatch. SBCL
`src/code/numbers.lisp`, `floor` and `ceiling`, likewise produce an integer
quotient through their numeric operations. Python's rounding special methods
are the concrete dispatch adaptation.

## Implicit special-method binding

Implicit special-method calls use the same descriptor binder as explicit
attribute calls. Plain compiled functions retain the borrowed receiver call
path; `staticmethod`, `classmethod`, and user descriptors first obtain their
callable through compiled `__get__`, then receive only the operation's arguments.
Instance attributes remain excluded from special-method lookup.

SBCL `src/pcl/dfun.lisp`, `get-secondary-dispatch-function1`, obtains an effective
method via `compute-effective-method` before invoking it. Python selects methods
from a class's special-method table and additionally applies descriptor binding;
it has no Common Lisp method combination at this boundary. Mule reuses its
existing class-attribute binder for that concrete semantic adaptation, keeping
descriptor algorithms in compiled Python. The plain-function path introduces
no bound-method allocation; descriptor calls have their normal binding cost.

## Annotation contracts

### Builtin Python annotations and SBCL correspondence

All parsed annotation expressions are accepted, including forward references,
user classes, generic subscriptions, unions, and calls. Their Mule ASTs are
retained separately from the compiler's semantic contracts. An annotation does
not require the compiler to understand its type in order to compile the body.
Function/module/class annotations are deferred, following Python 3.14; local
annotations are not evaluated. Functions with annotations have a compiled
`__annotate__` thunk. The function class lazily evaluates and caches
`__annotations__`; supported contract expressions additionally guide analysis.

### Function annotation thunks

SBCL `src/compiler/ir1tran-lambda.lisp`, `ir1-convert-lambda` and
`ir1-convert-lambda-body`, converts nested lambdas with their lexical
environments, and its environment analysis captures referenced bindings.
Mule retains that path for annotation thunks: it builds owned syntax nodes,
converts a normal nested lambda through IR1/IR2, and attaches its native
function object before applying decorators. Python requires annotations to
execute later, with current globals and retained enclosing cells; SBCL's
compile-time type declarations do not have this deferred runtime protocol.
No source parsing, host evaluation, or Rust annotation evaluator occurs at
annotation access.
Method annotation closures retain a shared class-namespace cell. It starts as
the prepared namespace while the class body and metaclass run, then refers to
the constructed class's live dictionary view before decorators execute. Later
class attribute changes are therefore visible to an uncached annotation call.
The class dictionary property now wraps a live compiled view over its own
stored bindings, rather than copying them. SBCL `src/pcl/slots.lisp`,
`slot-value-using-class`, similarly reads an existing object's slot storage.
Python's class dictionary view additionally excludes inherited names and
returns raw descriptor objects. The view implements those rules in compiled
Python over the existing class storage primitives.

The thunk accepts VALUE (1), resolved FORWARDREF (2), and STRING (4). STRING
returns the retained annotation source text without evaluating it. VALUE and
resolved FORWARDREF evaluate ordinary annotation expressions in source order;
unknown names raise `NameError`. Creating unresolved `ForwardRef` proxies
remains a gap. `from __future__ import annotations` returns source strings
for VALUE and FORWARDREF. STRING currently preserves source spelling rather
than CPython's complete source normalization. Runtime value introspection of
compiler-only Mule numeric marker imports raises `NotImplementedError`; their
STRING metadata remains available. Class/module annotation callbacks and
general evaluation of dynamically supplied annotation strings remain gaps.

The compiled function class owns lazy evaluation, cache invalidation and
annotation assignment checks. Native storage primitives only retain and
replace its annotation callable and cache slots. An annotation failure does
not fill the cache, and annotations do not execute merely because a function
is defined or called. Physical slots keep this state separate from the
function's ordinary attribute dictionary. This adds a native closure per
annotated definition and an owned dictionary on first VALUE access; it makes
no optimization claim about annotation access.
Existing assigned `__defaults__`/`__kwdefaults__` metadata remains writable;
dynamic signature/default replacement in the compiled binder is still a gap.

Function identity analysis follows a temporary with one assignment back to its
function-making node, so attaching metadata preserves native method discovery
and guarded local calls. SBCL `src/compiler/ir1util.lisp`, `lvar-fun-name`,
likewise resolves the function represented by an IR value. This adds no
runtime guard bypass: mutable global aliases remain subject to normal guarded
call analysis, and decorated call results are not assumed to be their inputs.

`NoneType` supplies its `__class__` read through the existing physical type
query. An ordinary Python property uses `None` as its class-access argument,
which cannot represent the actual singleton receiver at this boundary. This
owned method implements the singleton's physical class attribute without
requiring callers such as ABC checks to inspect the value. User-defined
descriptor protocols continue to receive their ordinary Python arguments.

The compiled `type.__subclasses__` method returns a snapshot of its direct
children, allowing compiled ABC checks to recurse through real subclasses and
their virtual registrations. SBCL `src/pcl/std-class.lisp`,
`add-direct-subclass` and `remove-direct-subclass`, maintains corresponding
backpointers. Mule's existing native class layout stores the physical links;
the child retains its bases, while parent-to-child pointers are borrowed and
removed before the child releases those bases. Python requires creation order,
so registration appends rather than SBCL's `pushnew`. The storage primitive
retains a snapshot; traversal and ABC policy remain compiled Python. No class
is kept alive merely by its parent's subclass list.

Builtin `int` denotes arbitrary-precision Python integers, including booleans.
It does not impose a machine width. Builtin `float` denotes Python's binary64
floating-point category. Both are available without a compiler import for
parameters, returns, and module/local declarations. Rebinding a builtin name
makes its annotations opaque to contract analysis; it does not make them illegal.

SBCL's `src/code/type.lisp`, `def-type-translator integer`, resolves `integer`
with unbounded endpoints unless explicit bounds are provided. Its
`src/compiler/ir1opt.lisp`, `assert-lvar-type`, inserts a CAST into IR1;
`src/compiler/ctype.lisp`, `assert-definition-type`, applies argument and return
declarations; `src/compiler/checkgen.lisp`, `generate-type-checks`, chooses checks
after propagation and according to policy. Mule uses the same existing semantic
contracts, IR assertions, propagation, and policy-controlled emission for builtin
and imported annotations. Proven integer intervals can still select small-integer
operations; an unbounded declaration alone cannot justify a raw `i64` value.

For representation selection, SBCL's `src/compiler/generic/primtype.lisp`,
`primitive-type`, distinguishes bounded signed-byte-64 from descriptor-backed
unbounded integers; `src/compiler/represent.lisp`, `select-tn-representation`,
chooses among permitted storage classes. Mule may derive an ordinary `int`
local's scalar representation from its operations and proven bounds instead of
preassigning its declaration a width. A builtin `int` assertion preserves a
previously selected integer or boolean representation, and arithmetic still
requires the existing range proof or a separate fixed-width result contract.
Unbounded `int` parameters do not acquire a raw `i64` entry from their declaration.
This is the corresponding limited local selection pass; it does not implement
SBCL's full storage-class costing machinery.

The concrete Python adaptation is that booleans belong to `int`, whereas Common
Lisp booleans are symbols. Mule must preserve boolean object identity and cannot
refine an unknown `int` into its integer-only tagged representation. A semantic
integer-or-boolean fact records that distinction. Checked boundaries recognize
boolean singletons in compiled LLVM code and otherwise use the existing physical
integer assertion with unbounded endpoints. No new Rust language implementation
is required. Imported fixed-width Mule contracts continue to exclude booleans.

SBCL's `src/code/type.lisp`, `%parse-type`, retains legal unresolved specifiers
with `make-unknown-type`, while rejecting invalid type-specifier forms. Mule
likewise retains unresolved annotation expressions rather than rejecting names
it cannot resolve. The concrete Python adaptation is that annotations can carry
arbitrary user expressions and metadata, not just Lisp type-specifier forms;
these expressions must be accepted without executing them during compilation or
adding unjustified assertions. Runtime annotation evaluation is a separate
language feature, whereas SBCL's declaration path resolves compiler type objects.
Bare class annotations use boxed instance storage unless their declared fields
and policy support the native representation. Unsupported representation widths
do not make an otherwise valid class annotation illegal.

`stdlib/mule.py` exposes these annotation-only types, without a NumPy dependency:

| Category | Names |
| --- | --- |
| Signed integers | `int8`, `int16`, `int32`, `int64`, `int128` |
| Unsigned integers | `uint8`, `uint16`, `uint32`, `uint64`, `uint128` |
| Floats | `float16`, `float32`, `float64`, `float128` |

There are no native-size or 256-bit public types. Ordinary Python integer values
remain arbitrary precision. These markers are declarations, not constructors.
Mule resolves them through its own compiler bindings rather than importing the
host compiler package or executing library source. For example:

```python
import mule as m

@m.optimize(speed=3, safety=3, debug=0)
def increment(value: m.int32) -> m.int32:
    result: m.int32 = value + 1
    return result

print(increment(41))
```

`from mule import int32, optimize` and renamed imported symbols also work.
Imports must be unconditional module-level statements. Compiler-only bindings
cannot be rebound, shadowed by parameters/locals, or used as runtime values.
Wildcard compiler imports remain unsupported. Raw class-field declarations use
the explicit native `int64` class subset below; other bare class annotations do
not force a native layout. A module annotation with a recognized contract
constrains all writes to that global, including writes in functions defined before
the declaration. A local annotation constrains stores from its declaration onward.
A bare declaration leaves the variable unbound until assigned. Repeated identical
declarations are allowed; conflicting declarations are errors.

Integer contracts constrain values to their signed or unsigned range and exclude
booleans. They are not wrapping conversions: assigning a known out-of-range
result is a compilation error. Known nonnumeric values, invalid return values,
and proven call arguments contradicting a contract also cause source-positioned
compilation errors. Annotations propagate trusted facts into the existing
analysis and native operation selection. Eligible signed `int64` functions and
native class methods have raw `i64` internal entries and locals. Native fields
retain all 64 bits without GMP storage. Generic interfaces box full-width values
outside the tagged range into GMP-backed objects. Other widths and general
operations retain the boxed convention; annotations alone do not establish
eligibility for raw arithmetic or erase ordinary Python integer semantics.

The float widths are registered annotation markers. Currently they assert the
floating-point category, and runtime arithmetic/storage remains binary64.
Distinct binary16, binary32, and binary128 representation, rounding, and
arithmetic lowering are not implemented by these marker declarations.

`optimize` takes literal keyword levels from zero through three for `speed`,
`safety`, and `debug`. Defaults are `speed=3`, `safety=0`, `debug=1`. Class policies
are inherited by methods, and method decorators override specified options.
On the host interpreter the library decorator returns the original function.
It does not wrap calls in the generated executable.

- `safety=0` trusts declarations and omits optional annotation checks.
  Generic values entering raw `int64` storage or an internal native entry still
  undergo physical representation validation at that boundary.
  Statically detected contradictions are errors at every safety level. An
  undetected violation under trusted compilation need not produce a clean error.
- `safety=1`, `2`, or `3` checks an unknown category or range before using its
  annotated facts. Checks disappear when analysis already proves the contract.
  Failures produce `TypeError`, and error cleanup owns the unrefined value until
  the check succeeds. These levels currently share the same annotation checks.
- `speed=0` emits `noinline optnone` native bodies. Levels one through three
  retain the existing `-O3` compilation pipeline.
- `debug=3` prevents body inlining. Other levels retain existing behavior;
  source tracebacks and DWARF debug metadata are not implemented by this option.

These options currently control the behavior above, not every future compiler
optimization or Python runtime check. Ownership, argument arity, dynamic errors,
and recursion handling retain their existing contracts.

## Default subclass hook

The compiled root `object` provides `__subclasshook__` as an inherited classmethod
with one positional-only argument. It returns `NotImplemented`, allowing the
compiled `ABCMeta.__subclasscheck__` to continue its ordinary inheritance and
virtual-subclass registry checks. Access through a class or an instance binds
the hook to that class; user overrides retain normal descriptor semantics.

SBCL `src/pcl/std-class.lisp`, `validate-superclass`, supplies default class
protocol methods in compiled Lisp; `src/pcl/cpl.lisp`,
`compute-class-precedence-list` and `compute-std-cpl`, also keep inheritance rules
in Lisp. Mule follows that language-level boundary with compiled Python and its
existing classmethod descriptor. Python's ABC hook, `NotImplemented` result,
and class binding have no identical SBCL protocol and are required Python
semantics. The default adds no Rust dispatch or inheritance analysis; it declines
to decide virtual subclass membership.

Bound methods expose their receiver through the compiled `method.__self__`
read-only property. A checked native storage primitive retains the receiver
already owned by the bound method; it does not perform attribute lookup or
method selection. SBCL `src/compiler/x86-64/cell.lisp`, `closure-ref`, similarly
reads captured callable storage directly. Python's separate bound-method object
and public receiver property are a semantic adaptation, implemented with the
existing compiled descriptor protocol and no additional receiver storage.

## Builtin-subclass attributes

Tuple, list, and bytes subclass values retain their physical sequence storage,
their actual class, and an owned attribute namespace allocated on demand. The
same namespace-access boundary serves int, float, and str subclasses. Exact
builtin values do not gain dictionaries or arbitrary attributes. Compiled builtin
storage classes declare empty slots, so a user subclass's empty slots can suppress
its dictionary, while an unslotted subclass enables its own dictionary.

Attribute lookup and assignment reuse the shared class/descriptor machinery;
the compiled root methods select descriptor setters and deleters before invoking
hook-bypassing physical access. Explicit `object` calls bypass user hooks.
Dictionary views use raw binding primitives so dictionary writes and deletion do
not invoke descriptors or user attribute hooks. Attribute and dictionary access
share one owner. Sequence destruction releases that owner's namespace exactly
once. Constructing a bytes subclass from existing bytes copies physical bytes
instead of changing the source's class or sharing its attributes.

SBCL `src/pcl/slots.lisp`, `standard-instance-access` and `slot-value-using-class`,
keep values in the instance's own storage and perform access through its class
metadata. Mule retains that ownership boundary. Python additionally requires
native sequence payloads and mutable per-instance dictionaries on subclasses;
the added namespace pointer is a physical-storage adaptation, not a Rust class
evaluator or an external attribute registry. Attribute rules remain compiled
Python over physical access and the existing shared dispatch boundary. Direct
LLVM native field access still targets guarded `NativeInstance` payloads, while
tuple element extraction uses its physical runtime primitive; neither assumes
the internal Rust sequence variant's layout. Nonempty slots on builtin storage
subclasses remain outside this dictionary-storage increment.

Ordinary attribute access uses compiled `get_attribute` for exception-aware
fallback. Its physical primary lookup invokes `__getattribute__` once but never
`__getattr__`; compiled Python catches AttributeError and calls the class fallback.
Module namespace hooks stay in the compiled module class and remain unbound
functions, distinct from a constructed module subclass's bound class hook. This
keeps explicit generic object lookup free of fallback and avoids invoking a hook
twice when it raises AttributeError.

Generated startup registers a static adapter only after sequence and print
protocol initialization succeeds. The adapter retains borrowed receiver/name
arguments before transferring them to the compiled body in a runtime frame.
Protocol shutdown clears the adapter before releasing its globals. Before
language protocol registration, the native-library bootstrap interface retains
physical lookup, matching the existing compiled argument-constructor boundary;
there is no environment or test selector. SBCL `src/code/load.lisp`, compiled
initialization, and `src/pcl/slots.lisp`, class-aware storage access, retain the
corresponding load/access separation. Python's exception-aware fallback requires
the compiled wrapper rather than matching native exception text in Rust.

The generic path boxes attribute text and crosses a compiled frame; it currently
does not preserve per-source-site attribute caches. Guarded native-payload field
access and tuple physical-element primitives are unchanged. These checks establish
semantics and ownership, not an attribute-lookup performance improvement.

## Fixed-layout classes

This section describes current behavior. The target
[optimization policies](optimization-policies.md) distinguish Python-compatible
dynamic classes, fixed layouts, and native typed structs; those policy-selected
dynamic levels are not yet available. This section describes the existing boxed
class path. The restricted native subset is described separately below.

The compiler discovers fields from assignments to attributes of each method's
first parameter, regardless of that parameter's spelling. This includes writes
outside `__init__` and in branches. A subclass keeps inherited field indices and
adds its own fields. Each instance owns a fixed contiguous array of object
references, with no attribute dictionary. Field values can change to arbitrary
Mule objects; field discovery does not impose type annotations or value types.
Undeclared writes and reads of missing or not-yet-initialized fields raise
`AttributeError`. An initialized field containing `None` is distinct from an
uninitialized field.

Immutable class descriptors store field indices and native method objects.
Method overrides replace inherited entries; inherited methods calling
`self.method()` dispatch through the instance's actual class. Attribute sites use
two machine words of cache storage: a monotonically assigned class identity and
a resolved field or method index. A cache hit accesses that index directly;
class lookup tables are consulted on a miss. Class identities do not repeat when
an earlier class is released, so address reuse cannot validate an old cache.

For `obj.method(args)`, lowering resolves and owns the callable and receiver
before evaluating arguments. A predicted method call checks the actual native
entry, globals, and presence of a bound receiver before calling the native body
directly. It passes the receiver separately, avoiding allocation of a bound-method
object or general argument view on that path. General method argument binding
uses a borrowed receiver-prefixed argument view rather than constructing a heap
argument vector. Saving `method = obj.method` does allocate a bound-method
value that owns the receiver and method. Calling `Class.method(obj, args)` exposes
the underlying unbound native function. Construction uses inherited or overridden
`__init__`; it must return `None`. `__call__`, `__bool__`, and `__len__` participate
in the callable, truth, and length protocols. Truth testing prefers `__bool__`
and otherwise uses `__len__`. Length results must be nonnegative integers that
fit the native signed size range, and `__bool__` must return a boolean.

These choices intentionally restrict Python compatibility. Compiled class
attribute mutation raises `TypeError`, and a field/method name collision raises
`TypeError` during class construction. Instances have no `__dict__`. Multiple
inheritance, arbitrary class-body execution, class attributes,
`__slots__` declarations, metaclasses, properties/descriptors, `super()`, and
general method decorators are unsupported; the imported `optimize` directive is
accepted. The compiled f-string formatter uses custom formatting hooks; existing
Rust `str` and `repr` bootstrap paths still require migration. Existing
empty dynamic `type()` objects and
static special-method hooks remain a limited runtime facility; they do not
change the fixed-layout compiled-class contract. The class implementation has
behavioral native tests; no performance comparison is established by these tests.

## Native int64 classes

An explicit `@mule.optimize(speed=3)` class may declare bare `mule.int64` fields.
Every field must be declared; defaults, bases, attribute interception, and
undeclared field writes are rejected. Each instance has a contiguous signed
64-bit payload. Constructor CFG analysis requires initialization of every field
before a field read, receiver exposure, or successful return. Missing proofs
are compilation errors. Method annotations are unrestricted. Proven signed
`mule.int64` arguments permit raw entries; other methods use their compiled
general entries and field adapters.

Representation selection creates `@mule.native.N` internal entries for eligible
methods and scalar callers. They use raw scalar locals, constant-offset field
loads/stores, and direct native method calls. Generic entries validate incoming
values and receivers once and box outgoing results. Construction and generic
field access compile `stdlib/_native_class.py`, binding only its storage and
conversion primitives to backend operations. Native initialization retains the
normal recursion guard and `__init__` return validation.

Unannotated arithmetic remains arbitrary precision unless its range is proven
to fit. A fixed-width result contract permits raw arithmetic: positive safety
checks overflow, while safety zero treats overflow as a violated trusted
contract. Bitwise operations on raw signed `int64` operands preserve that range.
Known annotation contradictions remain compilation errors at all safety levels.

Class globals remain mutable. A scalar caller predicts a compiled layout,
validates its actual module binding before entering the raw region, and falls
back to its compiled boxed body if the binding differs. Native local edges must
be closed over supported bodies without global mutation or unknown callbacks;
this establishes that the guard remains valid. Unsupported methods retain their
compiled general body and use compiled field adapters. Recursion, escaping
native results, and constructor sites inside loops retain the general path until
the corresponding stack and lifetime analyses exist.

`examples/class_counter.py` exercises this path with an explicit native field.
With the caller selecting `speed=3`, at `-O3` LLVM eliminates its counted loop
into an update by `3 * max(limit, 0)`. Lower caller policies retain the loop.
The comparison harness in `benchmarks/classes.py` also measures an XOR/add
recurrence whose loop remains in the optimized code. This does not establish
the complete SBCL pipeline or implementation of the target class policy matrix.

## Current limits and verification

### Complex literal materialization

Complex literals retain IR1 `Constant` leaves and `Ref` nodes, following SBCL
`src/compiler/ir1tran.lisp`, `reference-constant`. The boxed constant VOP
materializes the real and imaginary binary64 components through the compiled
`complex_literal` helper in `stdlib/_sequences.py`. That helper uses
`stdlib/_object.py`'s existing complex class and `_make_complex` allocator;
construction and field initialization remain compiled Python. User rebinding
of `complex` does not affect the private protocol binding. Zero imaginary
literals remain complex values, and floating-point components preserve their
bits, including signed zero and infinity.

Unary `+` and `-` on complex constant operands fold in IR1, following SBCL
`src/compiler/ir1opt.lisp`, `constant-fold-call`. Negation reverses both
component signs, so `-0j` has a negative-zero real component as well as a
negative-zero imaginary component. This does not add unary dispatch for
nonconstant complex instances.

SBCL `src/compiler/generic/vm-type.lisp` defines dedicated complex numeric
representations, and `src/compiler/x86-64/float.lisp` supplies specialized
construction VOPs. Mule's existing complex representation instead uses a compiled
class instance with two boxed float fields. Literal materialization must establish
that class identity at runtime, rather than embedding an SBCL object layout or
adding a second Rust numeric representation. It allocates a fresh instance and
two float arguments; it adds neither raw complex TNs nor constant-pool sharing.
`mule_literal_float` cannot return a recoverable error; the compiled helper call
takes ownership of both arguments even when frame entry fails. The result uses
the ordinary boxed constant's owned slot and error check.

The sequence module imports `_object` and its allocator before other dependencies
can evaluate literals. This requires `_object`'s complex class to be initialized:
complex literals evaluated earlier within `_object`'s own initialization remain
a bootstrap limitation. General complex arithmetic specialization and full
complex-constructor/formatting compatibility are not established by this slice.

### Other limits

SBCL `src/compiler/ir1tran.lisp`, `find-free-var`, makes constant-symbol
references constant leaves instead of dynamic global variables. Mule similarly
converts the language constant `__debug__` to a boolean literal and rejects
attempts to bind its name. LLVM optimization levels control generated native
code, not Python's assertion-eliding `-O` semantics: `__debug__` remains true at
all current native optimization levels. The builtin namespace also exposes its
initial value; mutating that dictionary does not change source constant reads.

Compilation errors retain filename and AST source positions. Mule reports runtime
errors with a nonzero exit status and source tracebacks, including builtin errors
and errors propagated through handlers and `finally`. Exception formatting
preserves the original traceback even if compiled `__str__` raises or handles
another error. Traceback frames expose code, line, globals and caller links;
local-variable mappings and full function introspection remain incomplete. Functions
use Mule's native callable ABI. The runtime has a recursion depth guard. Module
startup does not yet implement Python's command-line argument or import/module
metadata protocols. Error cleanup preserves the first pending runtime error.

Heap ownership currently uses reference counting without cycle collection or
user finalizers. Function objects borrow the module environment, which remains
alive throughout execution. Instance fields and saved bound methods participate
in reference counting. Self references and other cycles are not collected unless
the program breaks them. Dynamic type construction is limited to empty bases
and static special methods; general dynamic inheritance and descriptors are not
available.
Unicode representation formatting and float formatting have basic implementations,
without complete compatibility guarantees. Complex literals and the existing
compiled complex-class methods are available; real-number runtime operations
such as fractional powers do not automatically promote their results to complex.

The runtime does not provide the CPython-compatible C extension interface yet.
That interface must be implemented by Mule without linking CPython. See the
[CPython interface requirements](cpython-interface.md) and
[dependency policy](dependencies.md).

`test/mule/compiler/` tests graph conversion and verification, Rust runtime helpers,
LLVM emission, build failures, and native programs. End-to-end tests assert ELF
output, remove the source file before executing the binary, and check independent
expected outputs and runtime failures. They cover recursion, loops, dynamic
globals, short-circuit side effects, large integers, Unicode/bytes constants,
fixed instance fields, method dispatch, inheritance, and class errors.
