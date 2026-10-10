# Compiler design reference

Status: documentation of SBCL's compiler and the changes required to compile
Python. An initial native compiler now implements the limited subset documented
in [Native compiler](native-compiler.md).

Mule is to follow SBCL's compiler architecture, implemented in Python 3.14,
compiling Python source, with LLVM as its only backend. The reference is SBCL's
compiler historically called **Python**; that compiler compiles Common Lisp and
is unrelated to the Python programming language.

## Documents

1. [SBCL compiler](sbcl-compiler.md): the existing compiler's conversion, IR1,
   inference, optimization, environments, IR2, native backend, and compiled output.
2. [Changes for Python and LLVM](python-changes.md): what carries over, what Lisp
   facilities do not apply, and what Python semantics require.
3. [CPython C extension interface](cpython-interface.md): compatibility requirements
   for objects, ownership, calls, exceptions, loading, and extension builds.
4. [Native compiler](native-compiler.md): implemented pipeline, runtime, supported
   syntax, executable builds, and current limitations.

The SBCL document describes existing behavior, with references pinned to the
inspected checkout. The adaptation documents describe requirements for Mule and
mark unresolved choices explicitly. The initial implementation emits textual LLVM
IR and links native Linux executables against Mule's runtime and GMP. It does
not yet implement the full analysis, optimization, or compatibility requirements.
The compiler is written in Python with eventual self hosting required. The
runtime is written in Rust; these are the approved implementation languages.

## Implemented compiler boundary

The handwritten lexer and Pratt parser produce `mule.ast` objects without the
host Python parser or its `ast` module. See [parser](parser.md), [AST](ast.md),
and [lexing](lexing.md). Conversion (`ir1tran.py`, `ir1tran_lambda.py`)
classifies local/global bindings, validates supported constructs, and builds
IR1: SBCL's node graph with control transfers (`Ctran`), single-consumer value
flow (`Lvar`), leaves, lambdas, blocks, cleanups, and components (`node.py`),
verified by `check_ir1_consistency` and ordered by `dfo.py`. The IR1 phases
(`main.ir1_phases`) run the optimization loop (`ir1opt.py`), local-call
analysis with guarded calls and entry contexts (`locall.py`), flow propagation
of variable facts (`constraint.py`), and check generation (`checkgen.py`). A
temporary bridge (`ir1bridge.py`) lowers IR1 and the established facts to the
earlier control-flow IR, which representation selection and LLVM emission
still consume; a Rust runtime implements objects and module startup.
See [Python and LLVM changes](python-changes.md#ir1-implementation).
See [Native implementation selection](native-lowering.md). Further optimization,
broader language support, and an extension-build SDK remain future work.
CPython linking is prohibited; see [dependency policy](dependencies.md).
Earlier experiments under `old/` do not define the new compiler's contracts.
