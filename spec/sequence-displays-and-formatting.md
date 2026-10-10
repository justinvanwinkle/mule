# Sequence displays and compiled formatting

Status: implemented list and tuple displays, and basic compiled f-strings.

SBCL `src/compiler/ir2tran.lisp`, `(list ir2-convert)`, takes evaluated argument
lvars and emits known-size construction VOPs; `src/compiler/srctran.lisp`,
`(list derive-type)`, derives constructor facts separately from allocation.
`src/code/list.lisp`, `list`, is a compiled language-level interface. Mule's
list and tuple displays likewise evaluate elements in source order and lower
to explicit construction IR. Python lists require contiguous, mutable storage
rather than Common Lisp cons cells; tuples require immutable contiguous storage.
Rust primitives allocate and retain the already evaluated element buffer. They
do not evaluate expressions or implement container algorithms. Existing Rust
container printing and comparison remain documented bootstrap debt.

SBCL `src/compiler/srctran.lisp`, the `format` transform near line 8934, compiles
constant format strings through `formatter`. Mule's parsed f-strings similarly
become explicit expression evaluation, compiled formatting calls, and string
concatenation. Python's `__format__`, `__str__`, and `__repr__` protocols require
method lookup on the class, bypassing instance attributes. Their selection and
fallback logic belongs in `stdlib/_format.py`, compiled through Mule's normal
frontend. Rust supplies method-table lookup, storage validation, and the existing
primitive value renderer. It does not interpret a format string. The initial
support includes ordinary interpolation, `!s`, `!r`, debug fields, and string
width/alignment with an optional fill character. Nested specification fields
execute as ordinary compiled interpolation. Custom `__format__` methods receive
the complete specification. String specification parsing and padding execute
in compiled Python using physical text length/character access. Numeric format
specifications, string precision, and zero padding remain explicit runtime
errors; other conversions remain compilation errors. Full formatting and
migration of the existing Rust scalar renderer remain separate work.

All formatting methods execute at the interpolation site. Conversion and
formatting must finish before evaluating the next interpolation, including its
side effects. Compiler-private primitive bindings cannot observe user globals
named `str` or `repr`. Invalid protocol results raise TypeError; method failures
follow ordinary owned-reference cleanup.
