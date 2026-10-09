# Parser

Status: initial specification of the implemented public entry point and current
syntax scope, not an exhaustive grammar or compatibility claim.

## Public interface

```python
from mule.parser import parse

parse(source, filename="<unknown>", mode="exec", *, type_comments=False)
```

`source` must be a string; other input types raise `TypeError`. An unsupported
mode raises `ValueError`. The parser consumes the complete input and returns a
Mule node, without executing code or invoking the host Python parser.

| Mode | Root node | Input boundary |
| --- | --- | --- |
| exec | Module | Zero or more statements |
| eval | ExpressionInput | An expression, including a comma-separated tuple; no assignment statement |
| single | InteractiveInput | One statement unit, including a compound suite or semicolon-separated simple statements |

`single` rejects multiple independent statement units. Empty module input and
comment-only module input are valid. A final physical newline is not required.

With `type_comments=False`, type comments are ignored. With `True`, supported
statement/parameter comment fields are retained and module-level `TypeIgnore`
nodes record `# type: ignore` directives and their tags.

## Syntax scope

The frontend targets Python 3.14 syntax. Current code and tests cover:

- Literal and container expressions, operators and precedence, comparison
  chains, calls, attributes, subscripts, slices, lambdas, and comprehensions.
- Named expressions, unpacking, yield, await, and asynchronous constructs.
- Simple statements, assignments, imports, control flow, suites, function and
  class definitions, decorators, and exception handling including `except*`.
- Structural pattern matching, generic type parameters and defaults, type
  aliases, formatted strings, template strings, and Python 3.14 exception syntax.

These categories describe existing coverage, not proof that every valid Python
program parses or that every invalid program is rejected.

## Validation and errors

Grammar and lexical failures raise `SyntaxError` or an applicable indentation
subclass with the supplied filename and a source-local location. Syntax error
offsets use the exception's one-based convention; AST columns use zero-based
Unicode-character coordinates. Exact CPython diagnostic wording is not specified.

The parser checks syntactic target restrictions and grammar ordering. It does
not perform all semantic compiler validation: context checks such as `return`
outside a function and duplicate pattern captures are deliberately outside the
parser error suite. Parsing success is not a promise that a program can compile.

## Evidence

- `mule/parser/core.py`: `parse`, shared state, and diagnostic construction.
- `mule/parser/expressions.py`, `statements.py`, and `patterns.py`: grammar.
- `test/mule/parser/test_entry_points.py`: modes, filenames, and type comments.
- `test/mule/parser/test_errors.py`: malformed syntax and validation boundary.
- `test/mule/parser/test_modern_syntax.py`: modern syntax expectations.
