# Small Python program corpus

44 independently sourced programs with deterministic output contracts. These
cover text processing, queues, dictionaries, classes, properties, decorators,
nested functions, dataclasses, comprehensions, generators, interactive input,
exceptions, file I/O, command-line arguments and control flow.

The implementations come from these pinned, MIT-licensed sources:

| Source | Programs | Examples |
| --- | ---: | --- |
| [Real Python materials](https://github.com/realpython/materials/tree/518d392755b2c993fd1e7362274dbd3d9e0f3876) | 15 | Queue, pizza factory, task formatting, properties, decorators, business dictionaries |
| [100 Page Python Intro](https://github.com/learnbyexample/100_page_python_intro/tree/47f92dfae336a650849e5598deeb9f37e2d05996) | 16 | Greeting banners, input retry, file processing, extension sorter, brace validation, filtering and name pairs |
| [Exercism Python](https://github.com/exercism/python/tree/6105f6d9d3ef66ce3d5c8f529d2882af3d76b706) | 13 | Word processing, Unicode reversal, conversational responses, validation, clock normalization and allergy queries |

License notices are preserved in `licenses/`. Each program records its specific
source URL, original upstream file SHA256, expectation source and adaptations.
Complete source implementations are preserved after the metadata. Four tutorial
console snippets have their prompts removed and displayed expressions wrapped in
`print`. Some examples have an appended deterministic print driver. Upstream
styles are preserved; these fixtures are outside the repository's `mule test`
formatting commands.

## Run the tests

```sh
uv run pytest test/mule/compiler/test_corpus.py
uv run pytest test/mule/compiler/test_corpus.py --corpus-strict
```

Every program has two tests. The reference test runs isolated CPython with `-I -S`
and checks the stored stdout, stderr and exit code exactly. The native test builds
an ELF executable with Mule and runs it against the same contract. Metadata is
read using Mule's parser and an AST visitor; fixtures are never imported to read
their docstrings. Reference execution is an external test oracle and is not part
of Mule's generated executable or runtime.

Each execution gets a fresh temporary working directory, declared file inputs,
stdin, command-line arguments, UTF-8 output decoding and a timeout. The selected
programs use only the standard library and need no network access or installed
third-party packages.

Currently 44 reference tests and 13 native tests pass. The remaining 31 native
cases document compiler gaps: 26 stop during compilation and 5 encounter missing
string methods at runtime. Ordinary runs report these as expected failures;
`--corpus-strict` makes them failures. A compile gap must match the recorded
`SyntaxError` or `UnsupportedSyntax` message and source file. A runtime gap must
match the entire recorded failure output and exit code. Different errors,
crashes, timeouts, toolchain failures and
unexpected output fail the test. Cases that become fully supported pass without
changing their metadata.

These programs retain features Mule has yet to implement. Their contracts provide
targets for extending the language without tailoring examples to the current
compiler subset.

## Module docstring format

The first statement is a raw string containing TOML:

```python
r'''
schema_version = 1
name = "Example name"
source = "https://github.com/owner/project/blob/COMMIT/path.py"
source_sha256 = "SHA256_OF_ORIGINAL_UPSTREAM_FILE"
expectation_source = "https://github.com/owner/project/blob/COMMIT/example_test.py"
license = "MIT"
license_file = "licenses/Project-MIT.txt"
adaptation = "Prepended metadata and appended a deterministic print driver."

[test]
stdout = "Hello!\n"
stderr = ""
exit_code = 0
stdin = ""
timeout = 5.0
args = []

[mule]
status = "supported"
phase = "compile"
reason = ""
'''
```

`args` defaults to an empty list. An optional `[files]` table maps relative input
filenames to their contents. Pending cases use `status = "pending"`, an observed
`reason`, and `phase = "compile"` or `"run"`. Runtime gaps additionally include
`[mule.failure]` with the observed `stdout`, `stderr` and `exit_code`.
Compile gaps match the diagnostic and originating
source file; imported-library gaps use `compile_error_file` relative to the
repository and `compile_error_line` to pin the known blocker. Other source errors
remain failures. Class bodies in `abc` currently block argparse and collections,
and the missing `builtins` module blocks dataclasses through `enum`; string
initialization additionally needs the `_string` module.

Expected outputs come from published examples or fixed inputs verified against
isolated CPython, independently of Mule. `source_sha256` identifies the upstream
file before metadata or drivers were added; tutorial snippets identify their
original Markdown file and link to the relevant lines.
