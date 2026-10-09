# Lexing and strings

Status: initial specification of the handwritten frontend's lexical boundaries.

## Tokenization

`mule/parser/lexer.py` scans source into tokens containing `kind`, `text`,
`start`, and `end`. Coordinates follow `spec/ast.md`. The lexer handles logical
newlines, indentation/dedentation, delimiter nesting, comments, numeric literals,
identifiers, operators, and string boundaries. Malformed input raises a
source-local syntax or indentation error rather than producing a usable partial
parse result.

Identifier tokens are normalized to Unicode NFKC when consumed as identifiers
by the parser. Hard keywords cannot be consumed as identifiers; soft keyword
syntax is interpreted in its grammatical context.

## String representation

Ordinary text and bytes literals become `Literal` values. String parsing handles
prefixes, escape decoding, triple quotes, and valid adjacent literal combinations.
Mixing adjacent bytes and text literals is rejected.

Formatted strings become `FormattedString` nodes; template strings become
`TemplateString` nodes. Their parts contain literal text and `Interpolation`
nodes. Interpolations carry an expression plus conversion, format specification,
and expression text where applicable. Embedded expressions are parsed by Mule's
parser with source-relative coordinates.

Invalid escapes that cannot be decoded, unterminated literals, invalid
interpolation conversions, empty interpolation expressions, and unmatched braces
are among the tested error cases. Template/plain-string concatenation is rejected.
Exact warning parity with CPython is not specified by this initial document.

## Evidence

- `mule/parser/lexer.py`: scanner and lexical errors.
- `mule/parser/core.py`: identifier normalization and fragment parsing.
- `mule/parser/strings.py`: decoding and interpolation parsing.
- `test/mule/parser/test_lexer.py` and `test_strings.py`: lexical expectations.
- `test/mule/parser/test_errors.py`: invalid literals and delimiter boundaries.
