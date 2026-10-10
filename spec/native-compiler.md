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
  currently contain method definitions and `pass` only.

Function-wide binding classification makes a later assignment local even when
an earlier read occurs first. Unbound local slots raise `UnboundLocalError`;
missing global names raise `NameError`. Globals are looked up at execution time,
so builtin shadowing, function aliases, and reassignment remain observable.
Boolean operations return operands; chained comparisons evaluate shared middle
operands once. Owned references are cleaned on normal and exceptional exits,
and an active error survives cleanup.

Unsupported constructs raise `UnsupportedSyntax`, a `SyntaxError` subclass,
before native compilation. These include imports, nested functions or classes,
closures/`nonlocal`, defaults, keyword/variadic calls, decorators, annotations,
async/generators, `for`, exception handling, containers/comprehensions,
subscriptions, unpacking, and formatted/template strings. Unsupported source is
also rejected in unreachable branches. Parser support is broader than compiler
support; acceptance by the parser does not imply native compilation support.

## Fixed-layout classes

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
method decorators are unsupported. Custom `__repr__` and `__str__` hooks are not
yet used by formatting. Existing empty dynamic `type()` objects and
static special-method hooks remain a limited runtime facility; they do not
change the fixed-layout compiled-class contract. The class implementation has
behavioral native tests; no performance comparison is established by these tests.

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
