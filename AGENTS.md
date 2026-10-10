# Mule

Mule compiles a small Python subset to native executables through LLVM. It uses
a handwritten parser, a custom syntax tree, a control-flow IR, and Mule's own
native runtime. Functions and control flow become native code;
the compiler does not delegate program execution to a source or bytecode evaluator.
The broader compiler architecture follows SBCL's compiler, historically called
Python. See `spec/native-compiler.md` for the implemented subset and target.

This file is both the project overview and repository guidance for coding agents.
`README.md` is a relative symlink to `AGENTS.md`; edit this file, not a separate
README copy.

## Setup and use

Use Python 3.14 and `uv`. `.python-version` selects 3.14; `pyproject.toml`
requires Python >=3.14. Runtime dependencies are currently empty. Development
dependencies are pytest and Ruff, resolved in `uv.lock`.

Native builds additionally require Clang, 64-bit Linux, and GMP development
headers and a shared library. Small integers use Mule's tagged native values;
arbitrary-precision integers use the approved GMP dependency. Executables do not
depend on a Python runtime installation.

```sh
uv sync
uv run pytest
uv run ruff check mule test
uv run ruff format --check mule test
uv run python -m mule examples/factorial.py -o build/factorial --emit-llvm build/factorial.ll
./build/factorial
```

The project uses `tool.uv.package = false`: run from the repository rather than
assuming an installed distribution or console entry point.

```python
from mule.parser import parse

module = parse("answer = 40 + 2\n", filename="example.py")
expression = parse("40 + 2", mode="eval")
```

Parsing returns Mule AST objects; it does not execute the source.

## Repository map

- `mule/ast.py`: syntax node dataclasses, operator enums, source coordinates,
  and tree traversal.
- `mule/parser/__init__.py`: public `parse` export.
- `mule/parser/core.py`: entry points, token cursor, diagnostics, identifier
  normalization, and shared parser state.
- `mule/parser/lexer.py`: handwritten scanner, indentation, tokens, and lexical
  source positions.
- `mule/parser/expressions.py`: expression grammar and precedence.
- `mule/parser/statements.py`: statements, suites, definitions, and type parameters.
- `mule/parser/patterns.py`: structural pattern matching grammar.
- `mule/parser/strings.py`: literal decoding and formatted/template interpolation.
- `mule/compiler/frontend.py`: semantic validation and AST-to-IR conversion.
- `mule/compiler/ir.py`: control-flow graph, object results, and graph verification.
- `mule/compiler/llvm.py`: LLVM emission and reference ownership/cleanup.
- `mule/compiler/runtime.c`: object operations and native callable runtime.
- `mule/compiler/runtime.h`: Mule value representation and runtime interfaces.
- `mule/compiler/numeric.c`: native integer fast paths and GMP bigint operations.
- `mule/compiler/driver.py`: Clang compilation and executable linking.
- `mule/__main__.py`: `python -m mule` command-line interface.
- `test/mule/parser/`: parser and AST tests, fixtures, and AST query helpers.
- `test/mule/compiler/`: lowering, LLVM, runtime, and native executable tests.
- `examples/`: source programs that the native compiler can build.
- `spec/`: initial specifications and explicit boundaries of implemented behavior.
- `old/`: earlier implementations, including Python bootstrap code, Common Lisp,
  LLVM experiments, a C++ runtime, and generated artifacts. Treat these as
  historical reference, not the active build or a specification for new behavior.

## Contracts to preserve

- Mule is an alternative implementation of Python. Never link generated programs,
  Mule's runtime, or native test executables against CPython, for any reason.
  Do not include CPython headers or use its runtime APIs or evaluator fallbacks.
  CPython-compatible C extension support must be implemented by Mule.
- Discuss every new third-party library with the user before including it.
  Linked libraries must be LGPL or more permissively licensed; GPL is allowed
  for non-linked uses. GMP is approved under its LGPLv3-or-later option.
  See `spec/dependencies.md`. Running the Python compiler host or an explicitly
  requested external CPython benchmark does not permit CPython linking.
- The parser builds `mule.ast` nodes directly, independently of the host Python
  parser. Do not substitute `ast.parse`, `compile`, or host-AST conversion for
  Mule's parser.
- Python 3.14 grammar is the current syntax reference. Coverage includes modern
  syntax, but the existing suite does not establish complete CPython parity.
- `parse` supports `exec`, `eval`, and `single`; preserve their distinct root
  types and input boundaries. See `spec/parser.md`.
- Lines start at one, columns at zero, and columns count Unicode characters,
  not UTF-8 bytes. Spans exclude the end position. See `spec/ast.md`.
- Assignment targets and values share expression node types. Target grammar is
  checked by the parser; scope and other semantic compiler validation are a
  separate responsibility.
- Source errors carry the supplied filename and source location. Do not recover
  from invalid syntax by returning a partial or empty tree.
- Keep shared token state in `Parser`; grammar mixins operate on that state.
  Interpolation fragments must retain coordinates relative to the original source.
- Reject unsupported compiler syntax explicitly, including unreachable source.
  Do not add execution fallbacks through the host parser, compiler, or evaluator.
- Preserve evaluation order, dynamic global lookup, and owned-reference cleanup
  on success and error paths. `None` is an object, not an unbound-slot marker.

## Change and verification workflow

Read the relevant specification, implementation, and nearby tests before changing
a contract. Keep changes in the active package unless the task explicitly concerns
`old/`. Preserve unrelated worktree edits.

Use pytest-native functions and parameterization. Existing tests construct
expected Mule trees by hand; use `test/mule/parser/ast_query.py` for structural
comparisons and targeted node/span queries. Do not make the parser under test
generate its own expected tree. Test valid syntax and relevant rejection
boundaries, including source positions when the change affects them.

Run focused tests while developing, then the full suite for parser or AST changes:

```sh
uv run pytest test/mule/parser/test_entry_points.py
uv run pytest test/mule/compiler
uv run pytest
```

Check changed Python files with Ruff; format only intended files. Documentation
changes do not require rerunning parser tests unless behavior also changes.
Update affected specifications when changing behavior. Separate implemented
contracts from proposals, and do not describe future compilation stages as available.

## Specifications

Start with `spec/index.md`. The files describe the current frontend and native
compiler, document SBCL's architecture, and record requirements for later stages.
They do not establish complete Python or C extension compatibility.
