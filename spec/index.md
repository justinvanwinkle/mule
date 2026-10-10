# Mule specifications

These specifications record the current Python frontend and native compiler
contracts and document SBCL's compiler as the reference for further work. The
compiler documents separate the existing SBCL architecture from required Python,
LLVM, and CPython C interface changes. Each file identifies its status; tested
frontend coverage does not establish complete Python compatibility.

## Contents

- [Parser](parser.md): public API, modes, grammar coverage, and errors.
- [Syntax tree](ast.md): node representation, traversal, and source coordinates.
- [Lexing and strings](lexing.md): tokenization and interpolated literals.
- [Compiler design reference](compiler.md): scope and reading order.
- [Native compiler](native-compiler.md): implemented subset and executable builds.
- [SBCL compiler](sbcl-compiler.md): source-backed baseline architecture.
- [Python and LLVM changes](python-changes.md): retained analysis and required adaptations.
- [CPython C interface](cpython-interface.md): extension build/runtime compatibility.

## Maintaining specifications

Keep externally meaningful contracts here and operational agent guidance in
`AGENTS.md`. Update the relevant file alongside a behavior change. Label proposed
behavior as proposed until its contract is agreed and implemented. When code and
specifications disagree, identify whether the implementation has a defect or the
specification needs an agreed change; do not silently redefine the contract.

The current syntax reference is the
[Python 3.14 grammar](https://docs.python.org/3.14/reference/grammar.html).
`old/` is historical material, not a source of current requirements.
