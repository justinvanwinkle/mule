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

The parser builds typed literal nodes directly from lexical syntax. Expression
nodes own target validation, boolean/comparison chain extension, and collection
restrictions. Literal and interpolation nodes own adjacent string-fragment
merging, while numeric literal subclasses validate pattern-literal roles.
These operations use node behavior rather than runtime AST-class checks.
Tokens retain lexical `kind` categories; operator and parameter enums describe
grammar roles. Tokens and positions expose `source_start()` for span construction.

### Known parser defects

- **`NamedExpression.require_dict_key` crashes with `AttributeError`
  (`mule/ast.py:596-600`, `mule/parser/core.py:213-220`):** An unparenthesized
  named expression used as a dictionary key (`{x := 1: 2}`) calls
  `parser.error("Invalid assignment expression target or context", self.source_start())`,
  passing a `Position` where `Parser.error` expects a `Token | None` and
  accesses `token.start.line`, raising `AttributeError` instead of
  `SyntaxError`.
- **Empty tuple `()` omitted from `Parser.grouped` (`mule/parser/expressions.py:375-378`,
  `mule/ast.py:747-748`):** `atom()` returns `ast.Tuple([])` for `()` without
  adding it to `self.grouped`, so `del ()` invokes
  `Tuple.delete_operands(grouped=False)` and produces `ast.Delete(targets=[])`
  instead of `ast.Delete(targets=[ast.Tuple([])])`.
- **Parenthesized `with` item lookahead rejects `with (a + b), c:`
  (`mule/parser/statements.py:338`):** `_with_item_parentheses` treats a
  closing `)` followed by `","` as a parenthesized `with`-item group, causing a
  grouped first context expression followed by a second item (`with (a + b), c:`)
  to fail with `SyntaxError`.
- **`type` statement lookahead accepts hard keywords as alias names
  (`mule/parser/statements.py:131-136`):** Statement dispatch checks
  `text == "type" and self.peek().kind == "NAME" and self.peek(2).text in {"[", "="}`
  without excluding hard keywords (`in`, `is`, `and`, `or`, `not`, `if`,
  `else`), misclassifying valid expressions such as `type in [1, 2]` as type
  alias statements.
- **Unpacked slice bounds accepted by `slice_item`
  (`mule/parser/expressions.py:239-242`):** `slice_item` parses the lower slice
  bound with `star_expression()`, accepting invalid starred slice bounds such as
  `a[*b:c]`.

## Evidence

- `mule/parser/core.py`: `parse`, shared state, and diagnostic construction.
- `mule/parser/expressions.py`, `statements.py`, and `patterns.py`: grammar.
- `test/mule/parser/test_entry_points.py`: modes, filenames, and type comments.
- `test/mule/parser/test_errors.py`: malformed syntax and validation boundary.
- `test/mule/parser/test_modern_syntax.py`: modern syntax expectations.
