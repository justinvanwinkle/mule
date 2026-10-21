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
- [Compiled modules](compiled-modules.md): static imports and the first native
  upstream test execution, including bootstrap runner limitations.
- [Function argument binding](argument-binding.md): captured defaults, compiled external entries,
  keyword calls, and guarded direct internal calls.
- [Sequences and iteration](sequences-and-iteration.md): compiled container protocols,
  native loops, variadic entries, and owned exception transfers.
- [Sequence displays and formatting](sequence-displays-and-formatting.md):
  evaluated list/tuple construction and compiled Python formatting protocols.
- [Native implementation selection](native-lowering.md): implemented analysis,
  templates, representation selection, guarded calls, and their SBCL source references.
- [Type inference and flow propagation](type-inference.md): optimization-only type charter,
  open/closed-world compilation model, value-type lattice, constraint propagation, and local-call entry contexts.
- [Optimization policies](optimization-policies.md): target class restrictions,
  native struct representation, safety checks, and debug levels.
- [Compiled program performance](performance.md): runtime comparisons,
  diagnostic workloads, and measured optimization progress.
- [Dependency policy](dependencies.md): prohibited CPython linking and library approvals.
- [SBCL compiler](sbcl-compiler.md): source-backed baseline architecture.
- [Python and LLVM changes](python-changes.md): retained analysis and required adaptations.
- [Remaining compiler and IR1 transform gaps](compiler-gaps.md): implemented flow-type feedback (1A) and roadmap for remaining SBCL parity gaps (1B–5).
- [Runtime migration](runtime-migration.md): Rust-to-Mule migration roadmap, C23 evaluation, and pure-Mule runtime requirements.
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
