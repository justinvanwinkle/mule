# Compiler scope and open decisions

Status: current implementation boundary and future work, not an approved backend
design.

## Implemented today

The active `mule/` package performs lexical scanning and parsing into Mule AST
objects. It exposes parsing as a Python API. `pyproject.toml` describes the project
as a Python compiler with an LLVM backend, but the active package contains no
semantic analysis, intermediate representation, LLVM emission, runtime, or
executable compiler entry point.

`old/` contains earlier compiler/runtime experiments and generated outputs. Their
presence does not establish support in the active implementation or commit the
new implementation to their architecture.

## Decisions still required

Before implementing subsequent stages, specify:

- Semantic validation: scope, binding, control-flow context, and pattern capture
  rules, including how failures report original source positions.
- Language/runtime compatibility: which Python behaviors and standard-library
  facilities are required, and which differences are intentional.
- Type and object model: how values, calls, exceptions, and object lifetimes are
  represented and handled.
- Lowering and LLVM integration: intermediate representation, backend dependency,
  supported targets, and compilation/linking boundaries.
- User interface: compilation inputs/outputs, executable or library usage, and
  diagnostic behavior.
- Verification: reference programs and observable results for compiler/runtime
  correctness, beyond successful parsing.

The current parser's separation of syntax and semantic validation is an existing
boundary. It is not a completed specification of those later stages.
