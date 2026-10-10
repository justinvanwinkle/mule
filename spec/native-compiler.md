# Initial native compiler

Status: implemented end-to-end compiler for a limited Python subset. The priority
is a real executable before broader language support or optimization.

## Build and run

Run from the repository with Python 3.14 and `uv`:

```sh
uv run python -m mule examples/factorial.py -o build/factorial --emit-llvm build/factorial.ll
./build/factorial
```

The target is 64-bit Linux. Clang and Rust must be on `PATH`, or supplied through
`--clang` and `--rustc`. GMP's shared library is required. Generated executables
link Mule's Rust runtime, GMP, and platform services required by Rust. They
have no CPython headers, API dependencies, library links, interpreter initialization,
or Python installation paths. Program source files are read as UTF-8.

`-o` defaults to the source path without its suffix. `--emit-llvm PATH` optionally
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

The pipeline is Mule's lexer/Pratt parser → Mule AST → control-flow IR → type/range
analysis and implementation selection → textual LLVM IR → Clang-generated native
executable. There is no host AST conversion,
program source embedding, bytecode generation, or evaluator fallback.

The IR separates control-flow blocks from single-consumer object results.
Variables are explicit local slots or dynamic module globals. Verification checks
definitions, consumption, control-flow targets, and dominance before emission.
LLVM currently uses slots for these results and variables rather than optimized
SSA construction. Analysis joins local value types and integer intervals to a
fixed point, widening growing loop bounds. Interprocedural analysis derives
integer parameter contexts and return summaries for proven local calls; guarded
specialized entries preserve general function behavior. Proven representation facts eliminate
unnecessary checks; unknown operands use guarded native instructions with runtime
slow paths. See [Native implementation selection](native-lowering.md). Clang
builds at `-O3`, applying LLVM optimization and native code generation. Rust builds
the runtime with `opt-level=3` and `panic=abort`, as a statically linked archive.
The linker discards unused sections and removes Rust dependency debug sections.
Rust's code generation also uses LLVM. No C runtime source is compiled.

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
- Expression statements and positional calls. Mule implements a limited builtin
  set, including `print`, `abs`, `int`, `input`, `len`, `bool`, `ascii`, `repr`,
  `str`, and `float`; arbitrary CPython builtins are not available.
- `if`/`else`, `while`/`else`, `break`, `continue`, and `pass`.
- Module-level function definitions with fixed positional parameters, native
  recursion, return statements, implicit `None` returns, and `global` declarations.
  Definitions can occur inside module-level control flow.
- Module-level fixed-layout classes with zero or one compiled-class base,
  positional construction, instance methods, bound-method values, attribute reads,
  and assignment or augmented assignment to declared instance fields. Class bodies
  contain method definitions, bare annotated declarations and `pass`;
  explicitly selected native classes may use `mule.int64` fields as described below.
- Unconditional module-level imports of numeric annotation markers and
  `optimize` from `mule`, including aliases. Parameter, return, and identifier
  variable annotations support arbitrary expressions; recognized builtin and
  imported numeric contracts additionally guide compiler analysis. Imports from
  `typing` and `typing_extensions` can bind names for annotation use, and
  `from __future__ import annotations` is accepted. Runtime typing APIs are not
  implemented by these annotation imports.

Function-wide binding classification makes a later assignment local even when
an earlier read occurs first. Unbound local slots raise `UnboundLocalError`;
missing global names raise `NameError`. Globals are looked up at execution time,
so builtin shadowing, function aliases, and reassignment remain observable.
Boolean operations return operands; chained comparisons evaluate shared middle
operands once. Owned references are cleaned on normal and exceptional exits,
and an active error survives cleanup.

Unsupported constructs raise `UnsupportedSyntax`, a `SyntaxError` subclass,
before native compilation. These include general imports, nested functions or classes,
closures/`nonlocal`, defaults, keyword/variadic calls, general decorators,
async/generators, `for`, exception handling, containers/comprehensions,
subscriptions, unpacking, and formatted/template strings. Unsupported source is
also rejected in unreachable branches. Parser support is broader than compiler
support; acceptance by the parser does not imply native compilation support.

## Annotation contracts

### Builtin Python annotations and SBCL correspondence

All parsed annotation expressions are accepted, including forward references,
user classes, generic subscriptions, unions, and calls. Their Mule ASTs are
retained separately from the compiler's semantic contracts. An annotation does
not require the compiler to understand its type in order to compile the body.
Function/module/class annotations are deferred, following Python 3.14; local
annotations are not evaluated. Runtime annotation introspection/evaluation is
not implemented yet. Supported contract expressions additionally guide analysis.

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
accepted. Custom `__repr__` and `__str__` hooks are not
yet used by formatting. Existing empty dynamic `type()` objects and
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

Compilation errors retain filename and AST source positions. Mule reports runtime
errors with a nonzero exit status, but native functions do not
have Python frames, source tracebacks, or full function introspection. Functions
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
without complete compatibility guarantees. Numeric operations producing complex
values are explicitly unsupported.

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
