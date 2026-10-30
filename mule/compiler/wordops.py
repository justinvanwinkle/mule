"""Fixed-length fixnum word vectors, SBCL's ``(simple-array fixnum (*))``.

A word vector's representation, not flow analysis, makes every element a
fixnum, and its length never changes. Compiled code owns the whole layout:
``mule.word_vector.new`` allocates it with ``malloc``, and the generic
``_storage_read``, ``_storage_write`` and ``_storage_length`` primitives select
these templates when their receiver is a proven word vector, as SBCL selects
the ``data-vector-ref`` VOP by the array's specialized element type.

The runtime knows the object only as ``Data::RawWords``: a block of words with
no references, which ``destroy`` frees. ``object.rs`` tests these offsets.
Bounds stay with the compiled Python that indexes the vector.
"""

from .byteops import _integer_result, _none_result
from .ctype import WORD_VECTOR_TYPE
from .fndb import defknown
from .vmdef import define_vop
from .vop import DESCRIPTOR, IMMEDIATE, NULL, REFERENCE, SIGNED, WORD_VECTOR_T

RAW_WORDS_TAG = 28
RAW_WORDS_DATA_OFFSET = 24
RAW_WORDS_LENGTH_OFFSET = 32

OBJECT = (DESCRIPTOR, IMMEDIATE, REFERENCE)
WORD = (SIGNED, DESCRIPTOR, IMMEDIATE)
# Allocation is bounded by malloc; an index or length is a nonnegative fixnum.
_POLICIES = ("fast-safe", "raw")

HELPERS = f"""declare noalias ptr @malloc(i64)
declare void @abort() cold noreturn
define internal ptr @mule.word_vector.new(i64 %length, i64 %initial) {{
entry:
  %empty = icmp eq i64 %length, 0
  %words = select i1 %empty, i64 1, i64 %length
  %bytes = shl nuw i64 %words, 3
  %data = call noalias ptr @malloc(i64 %bytes)
  %failed = icmp eq ptr %data, null
  br i1 %failed, label %exhausted, label %allocated
exhausted:
  call void @abort() cold
  unreachable
allocated:
  %box = call noalias nonnull align 8 ptr @mule_allocate_object_storage()
  store i64 1, ptr %box
  %tracked = getelementptr i8, ptr %box, i64 8
  store i8 0, ptr %tracked
  %gc.slot = getelementptr i8, ptr %box, i64 12
  store i32 0, ptr %gc.slot
  %kind = getelementptr i8, ptr %box, i64 16
  store i32 {RAW_WORDS_TAG}, ptr %kind
  %data.addr = getelementptr i8, ptr %box, i64 {RAW_WORDS_DATA_OFFSET}
  store ptr %data, ptr %data.addr
  %length.addr = getelementptr i8, ptr %box, i64 {RAW_WORDS_LENGTH_OFFSET}
  store i64 %length, ptr %length.addr
  br i1 %empty, label %done, label %initialize
initialize:
  %index = phi i64 [0, %allocated], [%next, %initialize]
  %slot = getelementptr i64, ptr %data, i64 %index
  store i64 %initial, ptr %slot
  %next = add nuw i64 %index, 1
  %more = icmp ult i64 %next, %length
  br i1 %more, label %initialize, label %done
done:
  ret ptr %box
}}
define internal i1 @mule.word_vector.is_vector(ptr %value) {{
entry:
  %null = icmp eq ptr %value, null
  %bits = ptrtoint ptr %value to i64
  %lowtag = and i64 %bits, 1
  %immediate = icmp ne i64 %lowtag, 0
  %invalid = or i1 %null, %immediate
  br i1 %invalid, label %no, label %heap
heap:
  %kind.addr = getelementptr i8, ptr %value, i64 16
  %kind = load i32, ptr %kind.addr
  %vector = icmp eq i32 %kind, {RAW_WORDS_TAG}
  ret i1 %vector
no:
  ret i1 false
}}
define internal i64 @mule.word_vector.length(ptr %vector) {{
entry:
  %length.addr = getelementptr i8, ptr %vector, i64 {RAW_WORDS_LENGTH_OFFSET}
  %length = load i64, ptr %length.addr
  ret i64 %length
}}
define internal i64 @mule.word_vector.read(ptr %vector, i64 %index) {{
entry:
  %data.addr = getelementptr i8, ptr %vector, i64 {RAW_WORDS_DATA_OFFSET}
  %data = load ptr, ptr %data.addr
  %slot = getelementptr i64, ptr %data, i64 %index
  %value = load i64, ptr %slot
  ret i64 %value
}}
define internal void @mule.word_vector.write(ptr %vector, i64 %index, i64 %value) {{
entry:
  %data.addr = getelementptr i8, ptr %vector, i64 {RAW_WORDS_DATA_OFFSET}
  %data = load ptr, ptr %data.addr
  %slot = getelementptr i64, ptr %data, i64 %index
  store i64 %value, ptr %slot
  ret void
}}
"""


def helpers() -> str:
    return HELPERS


def _type_error_unless(gen, condition, label):
    bad = gen.fresh(f"{label}.bad")[1:]
    good = gen.fresh(f"{label}.ok")[1:]
    gen.line(f"br i1 {condition}, label %{good}, label %{bad}")
    gen.lines.append(bad + ":")
    gen.line("call i32 @mule_native_type_error()")
    gen.propagate()
    gen.lines.append(good + ":")


def _vector(gen, ref):
    """The receiver, checked once: a writable field may hold anything."""
    vector = gen.take(ref)
    is_vector = gen.operation(f"call i1 @mule.word_vector.is_vector(ptr {vector})")
    _type_error_unless(gen, is_vector, "word.vector")
    return vector


def _fixnum_word(gen, ref):
    """A raw word from a fixnum operand; unproven operands get a tag check."""
    if ref.tn.sc is SIGNED:
        return gen.take(ref)
    value = gen.take(ref)
    bits = gen.operation(f"ptrtoint ptr {value} to i64")
    if ref.tn.type is None or not ref.tn.type.is_fixnum:
        lowtag = gen.operation(f"and i64 {bits}, 1")
        tagged = gen.operation(f"icmp ne i64 {lowtag}, 0")
        _type_error_unless(gen, tagged, "word.fixnum")
    return gen.operation(f"ashr i64 {bits}, 1")


def _new(gen, vop):
    length = _fixnum_word(gen, vop.args[0])
    initial = _fixnum_word(gen, vop.args[1])
    vector = gen.operation(
        f"call ptr @mule.word_vector.new(i64 {length}, i64 {initial})"
    )
    gen.publish(vop.results[0], vector)


def _length(gen, vop):
    vector = _vector(gen, vop.args[0])
    length = gen.operation(f"call i64 @mule.word_vector.length(ptr {vector})")
    _integer_result(gen, vop, length)
    gen.release(vector)


def _read(gen, vop):
    vector = _vector(gen, vop.args[0])
    index = _fixnum_word(gen, vop.args[1])
    value = gen.operation(f"call i64 @mule.word_vector.read(ptr {vector}, i64 {index})")
    _integer_result(gen, vop, value)
    gen.release(vector)


def _write(gen, vop):
    vector = _vector(gen, vop.args[0])
    index = _fixnum_word(gen, vop.args[1])
    value = _fixnum_word(gen, vop.args[2])
    gen.line(
        f"call void @mule.word_vector.write(ptr {vector}, i64 {index}, i64 {value})"
    )
    gen.release(vector)
    _none_result(gen, vop)


NEW = defknown(
    "_word_vector_new",
    "call",
    "unsafe",
    derive_type=lambda node: WORD_VECTOR_TYPE,
)
PRIMITIVES = {NEW.name: NEW}
for _policy in _POLICIES:
    define_vop(
        f"{_policy}-_word_vector_new",
        translate=NEW,
        args=(WORD, WORD),
        results=((DESCRIPTOR,),),
        ltn_policy=_policy,
    )(_new)


def define_storage_templates(storage_primitives) -> None:
    """Specialize the generic storage primitives for word-vector receivers."""
    word_result = (WORD,)
    none_result = ((DESCRIPTOR, IMMEDIATE, NULL, REFERENCE),)
    for name, emit, args, results in (
        ("_storage_length", _length, (OBJECT,), word_result),
        ("_storage_read", _read, (OBJECT, WORD), word_result),
        ("_storage_write", _write, (OBJECT, WORD, WORD), none_result),
    ):
        for policy in _POLICIES:
            define_vop(
                f"{policy}-word-vector{name}",
                translate=storage_primitives[name],
                args=args,
                arg_types=((WORD_VECTOR_T,), *((None,) * (len(args) - 1))),
                results=results,
                cost=-1,
                ltn_policy=policy,
            )(emit)
