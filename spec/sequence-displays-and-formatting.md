# Sequence displays and compiled formatting

Status: implemented list and tuple displays, immutable tuple constant loading,
basic compiled f-strings, and integer decimal percent formatting.

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

## Immutable tuple constants

IR1 folds `%build_tuple` when every evaluated operand is already a supported
immutable constant: `None`, boolean, integer, float, text, bytes, or a recursively
constant tuple. It uses the normal constant-reference graph rewrite and preserves
source spans. Lists, unresolved bindings, calls, and other dynamic operands remain
ordinary construction operations. Existing scalar folding determines eligibility;
this increment does not add scalar unary folding or new literal kinds.

The LLVM module owns a pool of these tuples. Typed recursive keys keep boolean,
integer, float, text, and byte values distinct; floating-point keys preserve the
IEEE binary64 bits, including signed zero and NaN payloads. Nested tuples are
initialized before their parents. Identical constants share an ordinary immutable
tuple; they are not marked immortal. Each pool slot owns one reference, and each
evaluation obtains another owned reference through its generated load helper.
Tuples containing mutable or dynamically evaluated values remain fresh.

SBCL `src/compiler/ir1opt.lisp`, `constant-fold-call` and `%constant-fold-call`,
rewrites eligible construction calls to constants. `src/compiler/dump.lisp`,
`dump-object` and `fasl-constant-already-dumped-p`, loads constant objects and
preserves sharing. Mule follows these separate fold and constant-loading stages.
Its LLVM executable does not use an SBCL FASL or forge Rust `Object`/`Vec` layouts:
generated startup code constructs tuple storage with the existing physical Rust
allocator. This is a logically immutable pool, not memory mapped read-only storage.

Pool initialization runs inside the existing runtime startup error boundary,
before any compiled module body, including private protocol modules. The argument
buffer owns scalar and child references until construction completes. Reported
allocation failures release those temporaries and abort startup; `main` releases
every initialized pool root, in reverse dependency order, after runtime/import
cleanup on both successful and failed execution. Null uninitialized slots are safe
to release. A load helper's non-null result contract applies only after successful
initialization; initialization itself checks every fallible allocation.

Python permits sharing equal immutable values, so this changes tuple identity
without changing element values or mutation behavior. The concrete LLVM adaptation
eagerly allocates all emitted tuple constants before module execution, rather than
allocating them at individual expression visits or serializing them in a Lisp
image. Native-reported allocation errors can therefore occur during startup before
user side effects. No user expression, callback, or mutable constructor is moved
into this initialization phase. Dynamic tuple allocation and the existing runtime's
infallible-allocation limitations are unchanged.

## Compiled formatting

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

## Decimal percent formatting

Text `%` supports literal characters, `%%`, and `%d` with integer or boolean
operands, either a scalar or a tuple consumed in order. Arbitrary-precision
integers retain their full decimal value. Missing and surplus arguments raise
`TypeError`; incomplete or unsupported directives raise `ValueError`. Flags,
widths, precision, other conversion directives, mappings, float-to-decimal and
custom numeric conversion are not implemented in this increment.

SBCL `src/code/format.lisp`, `expand-format-integer`, expands the decimal
directive into compiled `format-integer` or `format-print-integer` calls.
`src/code/target-format.lisp` implements dynamic directive interpretation and
sign/padding rules in Lisp. Mule's corresponding parser, argument consumption,
sign handling and decimal digit generation execute in compiled `_format.py`.
Rust supplies existing text/tuple/integer storage access and arithmetic only;
this new formatter does not extend the Rust scalar renderer.

Python overloads `%` for numeric remainder and text formatting, unlike SBCL's
separate FORMAT operation, and uses `%d`/`%%` rather than `~D`/`~~`. When compiler
facts prove a numeric left operand, LLVM retains the existing numeric modulo
path. Unknown operands enter a compiled Python dispatch that tests physical
text storage and selects formatting or the existing numeric operation. Both
operands are evaluated once before dispatch. Numeric modulo retains its existing
negative-operand, float, bigint and zero-division behavior. Literal and dynamic
format strings use the same compiled formatter, with normal owned-reference
cleanup on errors.

A text literal whose directives are only `%s`, `%r`, `%a`, and `%%`, applied to
a tuple display without `*` elements and with one element per directive, is
converted as the equivalent formatted string (`FunctionConverter.percent_format`
in `ir1tran.py`). SBCL's `format` transform in `src/compiler/srctran.lisp`
expands a constant control string the same way. The literal's `%` is
`str.__mod__` and the display is the exact argument tuple, so the conversion
keeps Python's order of effects: every element is evaluated first, then each
is converted with `str`, `repr`, or `ascii` in order. Other patterns, other
right operands, and arity mismatches keep the general operation and its errors.
The compiled formatter also handles bare `%s`, `%r`, and `%a` directives before
the full specification parse, and copies literal runs as slices joined once.
