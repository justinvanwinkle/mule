# Mule

Mule compiles a small Python subset to native executables through LLVM. It uses
a handwritten parser, a custom syntax tree, a control-flow IR, and Mule's own
native runtime. Functions and control flow become native code;
the compiler does not delegate program execution to a source or bytecode evaluator.
The required architecture is a beat-by-beat clone of SBCL's compiler, historically
called Python, adapted to Python semantics and an LLVM backend. See
`spec/native-compiler.md` for the implemented subset and target.

This file is both the project overview and repository guidance for coding agents.
`README.md` is a relative symlink to `AGENTS.md`; edit this file, not a separate
README copy.

## Setup and use

Use Python 3.14 and `uv`. `.python-version` selects 3.14; `pyproject.toml`
requires Python >=3.14. Python package dependencies are currently empty. Development
dependencies are pytest and Ruff, resolved in `uv.lock`.

Native builds additionally require Clang, Rust with its bundled LLVM LLD linker,
64-bit Linux, and the GMP shared library. The Rust runtime uses handwritten GMP
bindings without external crates. Generated LLVM and Rust runtime bitcode are
optimized together with full link-time optimization at level 3.
Small integers use Mule's tagged native values;
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
- `mule/compiler/frontend.py`: public `lower` entry: IR1 conversion then the bridge to the control-flow IR.
- `mule/compiler/main.py`: compilation driver (`compile_toplevel`), following SBCL `main.lisp`.
- `mule/compiler/node.py`: IR1 structures (ctrans, lvars, nodes, leaves, lambdas, blocks, components, cleanups), following `node.lisp`.
- `mule/compiler/ir1tran.py`: AST-to-IR1 conversion with scope classification and semantic validation, following `ir1tran.lisp`.
- `mule/compiler/ir1tran_lambda.py`: lambda and module-body conversion, native initialization proof, following `ir1tran-lambda.lisp`.
- `mule/compiler/ir1util.py`: IR1 graph construction and surgery, following `ir1util.lisp`.
- `mule/compiler/dfo.py`: depth-first ordering, unreachable-block deletion, and component splitting, following `dfo.lisp`.
- `mule/compiler/debug.py`: `check_ir1_consistency`, following `debug.lisp`.
- `mule/compiler/knownfun.py` and `mule/compiler/fndb.py`: known-operation metadata and the database of Python operations and runtime protocols, following `fun-info.lisp`/`fndb.lisp`.
- `mule/compiler/policy.py`: optimization policy qualities, following `policy.lisp`.
- `mule/compiler/ir1bridge.py`: temporary lowering of IR1 to the control-flow IR until later phases consume IR1 directly.
- `mule/compiler/modules.py`: static module resolution and compiled initialization.
- `mule/compiler/ir.py`: control-flow graph, object results, and graph verification.
- `mule/compiler/analysis.py`: fixed-point value types, integer ranges, and guarded call predictions.
- `mule/compiler/ctype.py`: language-level value types (SBCL CTYPE): polymorphic facts, joins, arithmetic, and representation proofs.
- `mule/compiler/annotation_types.py`: imported numeric contracts and optimization policies.
- `mule/compiler/native.py`: raw int64 representation selection and direct local entries.
- `mule/compiler/native_protocol.py`: compile Mule class protocols and bind physical primitives.
- `mule/compiler/protocols.py`: shared bindings and calls for compiled language protocols.
- `mule/compiler/argument_protocol.py`: generate compiled signature binders and constructor forwarding.
- `mule/compiler/sequence_protocol.py`: compile sequence/exception protocols and bind physical primitives.
- `mule/compiler/format_protocol.py`: compile formatting protocols and bind text storage primitives.
- `mule/compiler/llvm.py`: LLVM emission and reference ownership/cleanup.
- `mule/compiler/llvm_ops.py`: inline native operation templates and runtime slow paths.
- `mule/runtime/lib.rs`: Rust runtime crate and native ABI.
- `mule/runtime/object.rs`: tagged values, heap objects, calls, and ownership.
- `mule/runtime/classes.rs`: fixed instance layouts, immutable method tables,
  attribute caches, and receiver binding.
- `mule/runtime/arguments.rs`: borrowed positional/keyword buffers and captured default storage.
- `mule/runtime/sequences.rs`: physical sequence storage, protocol tables, and exception state.
- `mule/runtime/builtins.rs`: builtin functions and module execution.
- `mule/runtime/numeric.rs`: native integer fast paths and bigint operations.
- `mule/runtime/gmp.rs`: direct GMP bindings and owned bigint wrapper.
- `mule/runtime/annotations.rs`: borrowed assertions at checked annotation boundaries.
- `mule/runtime/native_storage.rs`: raw payload allocation, layout guards, and int64 boxing.
- `mule/compiler/driver.py`: Clang compilation and executable linking.
- `mule/__main__.py`: `python -m mule` command-line interface.
- `test/mule/parser/`: parser and AST tests, fixtures, and AST query helpers.
- `test/mule/compiler/`: lowering, LLVM, runtime, and native executable tests.
- `examples/`: native compiler examples and upstream performance reference programs.
- `examples/benchmarks-game/`: unmodified Python 3, Rust, SBCL, and Go comparison sources,
  upstream licenses, expected outputs, and source hashes in `UPSTREAM.json`.
- `stdlib/mule.py`: numeric annotation markers and the `optimize` compiler directive.
- `stdlib/_native_class.py`: compiler-private class construction and field protocols compiled by Mule.
- `stdlib/_arguments.py`: compiled general constructor forwarding.
- `stdlib/_sequences.py`: compiled sequence, iterator, and exception algorithms.
- `stdlib/_format.py`: compiler-private formatting rules compiled by Mule.
- `stdlib/bootstrap/unittest.py`: initial assertion API compiled by Mule for simple upstream suites.
- `tools/run_stdlib_tests.py`: attempt all upstream suites and report native execution separately from compilation.
- `examples/mule.py`: relative symlink to the annotation library for direct host Python runs.
- `spec/`: initial specifications and explicit boundaries of implemented behavior.
- `old/`: earlier implementations, including Python bootstrap code, Common Lisp,
  LLVM experiments, a C++ runtime, and generated artifacts. Treat these as
  historical reference, not the active build or a specification for new behavior.

## Benchmarks Game performance examples

`examples/benchmarks-game/` contains all Python 3, Rust, SBCL, and Go variants
from the official Computer Language Benchmarks Game source archive for these
ten workloads: binarytrees, fannkuchredux, fasta, knucleotide, mandelbrot, nbody,
pidigits, regexredux, revcomp, and spectralnorm. There are 183 programs: 41 Python,
61 Rust, 33 SBCL, and 48 Go. Source bytes and contributor notices are unchanged;
only extensions were changed to `.py`, `.rs`, and `.lisp`. Upstream variant
identifiers remain in the filenames. Each workload retains its BSD license and
small-input expected output. `UPSTREAM.json` records the download URL, archive
SHA-256, original paths, local paths, and individual file hashes.

These are external comparison programs, not Mule runtime dependencies or a claim
that Mule can compile their Python features. Some variants require external
libraries, older toolchain features, SIMD, or multiple workers. Do not install
new libraries implicitly or rewrite reference algorithms to obtain a successful
Mule build. Record the variant, toolchain, flags, input, and worker count with
performance results; different upstream variants need not use identical
algorithms or parallelism.

Start with the dependency-free base nbody variants. Build outside the timed run:

```sh
mkdir -p build/benchmarks-game
rustc -C opt-level=3 --crate-name nbody examples/benchmarks-game/nbody/nbody.rs \
  -o build/benchmarks-game/nbody-rust
go build -o build/benchmarks-game/nbody-go examples/benchmarks-game/nbody/nbody.go
sbcl --noinform --no-sysinit --no-userinit --non-interactive \
  --eval '(declaim (optimize (speed 3)))' \
  --load examples/benchmarks-game/nbody/nbody.lisp \
  --eval '(sb-ext:save-lisp-and-die "build/benchmarks-game/nbody-sbcl" :toplevel (function main) :executable t)'
```

Validate against the upstream output before timing. These four invocations have
been checked at input 1000 and produce byte-identical output:

```sh
uv run python examples/benchmarks-game/nbody/nbody.py 1000 > build/benchmarks-game/nbody-python.out
cmp examples/benchmarks-game/nbody/1000_out build/benchmarks-game/nbody-python.out
for language in rust go sbcl; do
  build/benchmarks-game/nbody-$language 1000 > build/benchmarks-game/nbody-$language.out
  cmp examples/benchmarks-game/nbody/1000_out build/benchmarks-game/nbody-$language.out
done
```

For repeated wall-clock and peak resident-memory measurements on Linux:

```sh
for run in 1 2 3; do
  uv run /usr/bin/time -f 'python elapsed=%e seconds maxrss=%M KiB' \
    python examples/benchmarks-game/nbody/nbody.py 1000000 > /dev/null
  for language in rust go sbcl; do
    /usr/bin/time -f "$language elapsed=%e seconds maxrss=%M KiB" \
      build/benchmarks-game/nbody-$language 1000000 > /dev/null
  done
done
```

These timings include process startup but exclude Rust/Go builds, SBCL source
loading/image creation, and uv startup. Increase the input when native runs are
too short for useful measurements. Use an otherwise idle machine and compare
repeated runs, not a single measurement. For stdin workloads (knucleotide,
regexredux, and revcomp), generate one FASTA input with a fasta reference and
reuse that same file across implementations; input generation is not part of
their timed execution. Mandelbrot output is binary PBM, so compare bytes rather
than decoding it as text.

Mule compilation can be attempted separately:

```sh
uv run python -m mule examples/benchmarks-game/nbody/nbody.py \
  -o build/benchmarks-game/nbody-mule --emit-llvm build/benchmarks-game/nbody-mule.ll
```

The current nbody attempt stops at `import sys` with
`no compilable Python module named 'sys'`. There is no Mule timing result for
this program yet. Once it compiles, validate the executable against the same
upstream output before including it in comparisons. No compiler or runtime
behavior was changed to import these examples.

## Mandatory SBCL architecture

ALWAYS refer to how SBCL does it. Before designing or implementing any compiler
or runtime feature, inspect the corresponding source in `~/checkout/sbcl`.
Follow its compiler stages, data structures, analyses, invariants, representation
selection, optimization and safety policies, and runtime boundaries. This is a
beat-by-beat compiler clone in the Python context, not loose inspiration.

Doing anything differently from SBCL requires CLEAR REASONS AND JUSTIFICATION.
Before implementing a departure, document the relevant SBCL files and functions,
what SBCL does, the proposed difference, why it is necessary, and its semantic
and performance consequences in the relevant `spec/` document. Python semantics,
LLVM, and the approved implementation languages require adaptations, but they
are not blanket justifications: explain each concrete departure. Convenience,
missing compiler infrastructure, or an easier Rust implementation do not by
themselves justify a different architecture. Identify missing stages explicitly
and implement the corresponding compiler machinery.

The compiler is written in Python and must eventually self host. Language-level
runtime and standard-library code must be written in Mule Python and compiled
by Mule, as SBCL compiles its Lisp language implementation. This includes class
construction, attribute and descriptor semantics, method dispatch, containers,
and builtin algorithms. Rust is limited to low-level primitives such as memory
allocation and lifetime support, OS and thread services, and approved foreign
library interfaces. Existing Rust language-level implementations are bootstrap
debt to migrate, not a precedent for extending that boundary.

When compiler proofs permit it, emit direct LLVM arithmetic, field access,
native layouts, and direct calls, following SBCL's corresponding mechanisms.
Dynamic language paths belong in compiled Mule code backed by low-level
primitives. Do not replace missing compiler work with a Rust evaluator or
per-operation Rust language dispatch. Verify optimization claims with optimized
IR or machine code and representative benchmarks. Keep LLVM at `-O3` and Rust
at optimization level 3; those flags alone do not establish efficient code.

## Contracts to preserve

- Approved implementation languages are Python and Rust. The compiler is written
  in Python and must eventually self host; language-level runtime code is compiled
  Mule Python, and low-level runtime primitives are written in Rust.
  Do not add C/C++ implementations or C test probes. LLVM IR is the compiler's
  output. Historical `old/` code and explicitly requested comparison programs
  do not establish permission to use other implementation languages.
- Mule is an alternative implementation of Python. Never link generated programs,
  Mule's runtime, or native test executables against CPython, for any reason.
  Do not include CPython headers or use its runtime or evaluator fallbacks.
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
