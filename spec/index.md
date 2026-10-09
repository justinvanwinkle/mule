# Mule specifications

These initial specifications record the current Python frontend contracts and
the boundary between implemented behavior and future compiler work. Each file
identifies its scope and supporting code/tests. A feature being covered by tests
does not establish complete Python compatibility.

## Contents

- [Parser](parser.md): public API, modes, grammar coverage, and errors.
- [Syntax tree](ast.md): node representation, traversal, and source coordinates.
- [Lexing and strings](lexing.md): tokenization and interpolated literals.
- [Compiler scope](compiler.md): implemented stages and unresolved future work.

## Maintaining specifications

Keep externally meaningful contracts here and operational agent guidance in
`AGENTS.md`. Update the relevant file alongside a behavior change. Label proposed
behavior as proposed until its contract is agreed and implemented. When code and
specifications disagree, identify whether the implementation has a defect or the
specification needs an agreed change; do not silently redefine the contract.

The current syntax reference is the
[Python 3.14 grammar](https://docs.python.org/3.14/reference/grammar.html).
`old/` is historical material, not a source of current requirements.
