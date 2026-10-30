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

**Current lexical defects:**

- **Comment-only `# type: ignore` lines emit a duplicate `NEWLINE` token
  (`mule/parser/lexer.py:377-395`, `mule/parser/core.py:72-86`):** When
  `type_comments=True`, a line containing only `# type: ignore` sets
  `line_has_code = True` in `lexer.py:382` and emits an extra `NEWLINE` token at
  the end of the line. `Parser.__init__` then strips `TYPE_COMMENT` tokens that
  begin with `ignore` (`core.py:76-82`) without removing that `NEWLINE`, leaving
  two consecutive `NEWLINE` tokens that cause `suite()`, `if`/`elif`/`else`,
  `try`/`except`/`finally`, `match`/`case`, and `@decorator` headers to fail
  with `SyntaxError` or `IndentationError`.
- **Escaped closing braces inside format specifiers
  (`mule/parser/lexer.py:228-251`):** `scan_format` returns immediately on the
  first `}` at nesting depth 0 without skipping `}}` escaped closing braces in
  f-string and template-string format specifiers.

## Standard-library tokenizer

The standard-library `_tokenize.TokenizerIter` is independent of the compiler
lexer. Its implementation belongs entirely in `stdlib/`; neither implementation
imports the other or shares scanner source. It consumes a readline callable and
returns `(type, text, start, end, line)` tuples. `extra_tokens=True` preserves
comments, nonlogical newlines and source-layout text with generic operator codes;
the false mode uses exact operator codes and parser-facing layout coordinates.
The iterator does not add `ENCODING`: `tokenize.py` owns encoding detection and
that token. Byte callbacks with explicit encoding decode each returned line using
the available codec implementation, matching the low-level iterator boundary.
Native comparisons cover UTF-8 and Latin-1; the current native codec layer does
not supply every codec accepted by CPython. The public byte-input
`tokenize.tokenize` wrapper uses the compiled `iter(callable, sentinel)` builtin
to read through byte EOF, while `tokenize.generate_tokens` accepts text directly.

SBCL `src/code/reader.lisp`, `read-token` and `%read-preserving-whitespace`, keeps
stream reading and token-buffer behavior in compiled Lisp. Mule likewise keeps
this language-level scanner in compiled Python, with no Rust tokenizer or host
parser fallback. Python requires source-preserving token tuples, indentation and
f-string/template-string token modes rather than Common Lisp reader forms.
The iterator retains the current physical line and unfinished-token text, not
the entire input. CPython 3.14 is the token and error oracle; selected comparisons
do not establish exhaustive grammar or codec compatibility.

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
