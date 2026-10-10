# Initial native compiler

Status: implemented end-to-end compiler for a limited Python subset. The priority
is a real executable before broader language support or optimization.

## Build and run

Run from the repository with Python 3.14 and `uv`:

```sh
uv run python -m mule examples/factorial.py -o build/factorial --emit-llvm build/factorial.ll
./build/factorial
# 3628800
```

The target is 64-bit Linux with a GIL-enabled CPython 3.14 compiler host. Clang
must be on `PATH`, or supplied through `--clang`. The host must provide `Python.h`
and its shared `libpython3.14` library. The build uses the host runtime's include
and library directories. Generated executables contain its library search path
and base installation path for initialization; that shared library and standard
library installation must remain available. Packaging a portable runtime is
future work. Program source files are read as UTF-8.

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

The pipeline is Mule's lexer/Pratt parser → Mule AST → control-flow IR → textual
LLVM IR → Clang-generated native executable. There is no host AST conversion,
program source embedding, bytecode generation, or evaluator fallback.

The IR separates control-flow blocks from single-consumer object results.
Variables are explicit local slots or dynamic module globals. Verification checks
definitions, consumption, control-flow targets, and dominance before emission.
LLVM currently uses slots for these results and variables rather than optimized
SSA construction. There are no optimization passes; Clang builds at `-O0`.

Each source function becomes a native LLVM function. Definitions construct
CPython C callable objects whose entry pointers refer to those native functions
and whose context carries the module globals. Calls use the runtime callable
protocol, retaining dynamic rebinding and recursion. The native `main` initializes
the object runtime and invokes the compiled module initializer directly.

`runtime.c` uses CPython's C object APIs for numbers, strings, truth testing,
operators, calls, builtin lookup, exceptions, and reference ownership. This
preserves arbitrary-precision integer operations and Python operator dispatch
without implementing those services from scratch. The compiled program's
functions and branches execute as native code. Calling an existing Python callable
uses that callable's own implementation; it does not translate Mule functions
back into source or bytecode.

## Supported source subset

- Literal integers, floats, text, bytes, booleans, and `None`.
- Local/global name lookup, identifier assignment, chained assignment, augmented
  assignment, and assignment expressions.
- Binary and unary operators, comparison chains, short-circuit `and`/`or`, and
  conditional expressions.
- Expression statements and positional calls, including builtin calls such as
  `print`. Frame-sensitive builtins do not have full Python frame semantics.
- `if`/`else`, `while`/`else`, `break`, `continue`, and `pass`.
- Module-level function definitions with fixed positional parameters, native
  recursion, return statements, implicit `None` returns, and `global` declarations.
  Definitions can occur inside module-level control flow.

Function-wide binding classification makes a later assignment local even when
an earlier read occurs first. Unbound local slots raise `UnboundLocalError`;
missing global names raise `NameError`. Globals are looked up at execution time,
so builtin shadowing, function aliases, and reassignment remain observable.
Boolean operations return operands; chained comparisons evaluate shared middle
operands once. Owned references are cleaned on normal and exceptional exits,
and an active exception survives cleanup callbacks.

Unsupported constructs raise `UnsupportedSyntax`, a `SyntaxError` subclass,
before native compilation. These include imports, classes, nested functions,
closures/`nonlocal`, defaults, keyword/variadic calls, decorators, annotations,
async/generators, `for`, exception handling, containers/comprehensions, attributes,
subscriptions, unpacking, and formatted/template strings. Unsupported source is
also rejected in unreachable branches. Parser support is broader than compiler
support; acceptance by the parser does not imply native compilation support.

## Current limits and verification

Compilation errors retain filename and AST source positions. Runtime errors print
through CPython and produce a nonzero exit status, but native functions do not
have Python frames, source tracebacks, or full function introspection. Their
callable representation is a C builtin-style object rather than `PyFunction`.
The native runtime has recursion guards, but does not promise CPython's Python
frame recursion-depth behavior. Module startup does not yet implement Python's
command-line argument or import/module metadata protocols.

The CPython object runtime supplies the C API implementation, but this slice does
not provide an extension-build SDK or establish arbitrary C extension or wheel
compatibility. The broader requirements remain in [CPython interface](cpython-interface.md).

`test/mule/compiler/` tests graph conversion and verification, runtime C helpers,
LLVM emission, build failures, and native programs. End-to-end tests assert ELF
output, remove the source file before executing the binary, and check independent
expected outputs and runtime failures. They cover recursion, loops, dynamic
globals, short-circuit side effects, large integers, and Unicode/bytes constants.
