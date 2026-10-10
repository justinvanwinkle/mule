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

Literal syntax selects a concrete node at the grammar boundary:
`IntegerLiteral`, `FloatLiteral`, `ComplexLiteral`, `BooleanLiteral`,
`NoneLiteral`, `TextLiteral`, `BytesLiteral`, or `EllipsisLiteral`. Each retains
its raw scalar `value` and derives from `Literal`. The base `Literal` can be
used in hand-built trees but does not classify its value into a concrete node.
`NoneLiteral()` and `EllipsisLiteral()` supply their singleton values by default.

Assignment targets use the same expression types as values. Their owning
statement or comprehension determines their role; nodes do not carry a host-AST
load/store context. Scope validation belongs to the compiler.

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

Each concrete node explicitly implements `children()`, yielding its owned syntax
children in field and list order. Optional missing children are omitted; scalar
metadata and spans are not children. `children(node)` delegates to that method,
and `walk(node)` visits the root and recursively visits children in preorder.
Traversal does not inspect dataclass fields or classify their values.

Each concrete node also explicitly implements `accept(visitor)` and calls its
corresponding visitor method, such as `visit_Name` or `visit_IntegerLiteral`.
`Visitor` provides those methods and forwards their defaults to `visit_Node`.
Consumers override the syntax they support; dispatch does not use class-name
lookup, type tests, or a class registry.

Test queries use the same visitor protocol: `nodes(tree, q.Name)` selects name
nodes, and `q.Literal` explicitly visits all concrete literal subclasses.
Structural assertions compare dataclass equality, optionally clearing spans on
deep copies so the original trees keep their source coordinates. Expected trees
use explicit literal constructors.

`assigned_targets()` exposes syntactic targets on assignments, named expressions,
deletions, loops, with items, and comprehension clauses. Other nodes return an
empty tuple; an ordinary name read is not an assignment. Target expressions own
their validation behavior, and `Name.binding_name()` returns its identifier.

## Evidence

- `mule/ast.py`: node types, coordinate definitions, `children`, and `walk`.
- `test/mule/parser/test_ast.py`: representation and traversal behavior.
- `test/mule/parser/test_entry_points.py`: Unicode columns and newline handling.
- `test/mule/parser/test_compound_spans.py` and `test_match_spans.py`: compound spans.
- `test/mule/parser/test_strings.py`: interpolated syntax and positions.
