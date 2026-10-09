# Syntax tree and source coordinates

Status: initial specification of the implemented Mule AST representation.

## Representation

`mule.ast` defines an independent syntax tree using slotted dataclasses and
string-valued operator/category enums. All syntax nodes derive from `Node`,
which has an optional, keyword-only `span`. Hand-constructed nodes can omit
spans; parser tests verify coordinates on parsed constructs.

Roots are `Module`, `ExpressionInput`, and `InteractiveInput`. Expressions,
statements, and patterns have separate base classes. Supporting nodes represent
parameters, call arguments, comprehension clauses, handlers, and other syntax
components. The dataclass definitions specify the concrete fields.

Assignment targets use the same expression types as values. Their owning
statement or comprehension determines their role; nodes do not carry a host-AST
load/store context. Scope validation belongs to a future compiler stage.

## Coordinates

- `Position.line` is one-based.
- `Position.column` is zero-based and counts Unicode characters, not encoded bytes.
- `Span.start` is inclusive; `Span.end` is exclusive.
- CRLF and CR source line endings are normalized to LF during parsing.
- Interpolated expression positions remain relative to the enclosing source,
  not a separately numbered fragment.

Do not substitute CPython AST byte offsets for these character coordinates.
Span behavior for particular grammar constructs is protected by targeted tests,
including compound statements and match cases.

## Traversal

`children(node)` yields immediate child nodes in dataclass field order and list
order, excluding the span. Non-node scalar fields are not children.
`walk(node)` visits the root and recursively visits children in preorder.

## Evidence

- `mule/ast.py`: node types, coordinate definitions, `children`, and `walk`.
- `test/mule/parser/test_ast.py`: representation and traversal behavior.
- `test/mule/parser/test_entry_points.py`: Unicode columns and newline handling.
- `test/mule/parser/test_compound_spans.py` and `test_match_spans.py`: compound spans.
- `test/mule/parser/test_strings.py`: interpolated syntax and positions.
