"""Physical byte-storage templates selected before LLVM emission.

Language conversion, bounds and buffer-export policy remain in compiled Python.
The mutable payload has an authoritative repr(C) header; only cold growth calls
the allocator. Immutable byte views retain their owner in the IR1 loop local.
"""

from .ctype import BOOLEAN, BYTE_STORAGE_TYPE, NONE_TYPE, IntegerType
from .fndb import _arg_type, defknown
from .knownfun import defpropagate
from .vmdef import define_vop
from .vop import DESCRIPTOR, IMMEDIATE, NULL, REFERENCE, SIGNED

OBJECT = (DESCRIPTOR, IMMEDIATE, REFERENCE)
WORD = (SIGNED, DESCRIPTOR, IMMEDIATE)
# Rust Vec<u8> allocations are bounded by isize::MAX on the 64-bit target.
BYTE_LENGTH = IntegerType(0, (1 << 63) - 1)
BYTE_VALUE = IntegerType(0, 255)


def _word(gen, ref):
    value = gen.take(ref)
    if ref.tn.sc is SIGNED:
        return value
    bits = gen.operation(f"ptrtoint ptr {value} to i64")
    return gen.operation(f"ashr i64 {bits}, 1")


def _integer_result(gen, vop, value):
    if vop.results[0].tn.sc is SIGNED:
        gen.store(vop.results[0], value)
    elif vop.results[0].tn.type.is_fixnum:
        gen.publish(vop.results[0], gen.encode(value))
    else:
        boxed = gen.operation(f"call ptr @mule_box_i64(i64 {value})")
        gen.publish(vop.results[0], boxed)
        gen.check_pointer(boxed)


def _none_result(gen, vop):
    if vop.results[0].tn.sc is NULL:
        gen.store(vop.results[0], "0")
    else:
        gen.publish(vop.results[0], "@mule.none")


def _address(gen, storage, offset):
    return gen.operation(f"getelementptr i8, ptr {storage}, i64 {offset}")


def _read_word(gen, storage, offset):
    address = _address(gen, storage, offset)
    return gen.operation(f"load i64, ptr {address}")


def _checked_storage(gen, storage):
    """Validate a writable Python field before reading the physical header."""
    bits = gen.operation(f"ptrtoint ptr {storage} to i64")
    lowtag = gen.operation(f"and i64 {bits}, 1")
    integer = gen.operation(f"icmp ne i64 {lowtag}, 0")
    missing = gen.operation(f"icmp eq ptr {storage}, null")
    invalid = gen.operation(f"or i1 {integer}, {missing}")
    bad = gen.fresh("byte.storage.bad")[1:]
    object_label = gen.fresh("byte.storage.object")[1:]
    valid = gen.fresh("byte.storage.valid")[1:]
    gen.line(f"br i1 {invalid}, label %{bad}, label %{object_label}")
    gen.lines.append(object_label + ":")
    tag_address = _address(gen, storage, 16)
    tag = gen.operation(f"load i32, ptr {tag_address}")
    matches = gen.operation(f"icmp eq i32 {tag}, 27")
    gen.line(f"br i1 {matches}, label %{valid}, label %{bad}")
    gen.lines.append(bad + ":")
    gen.line("call i32 @mule_native_type_error()")
    gen.propagate()
    gen.lines.append(valid + ":")
    return storage


def _new(gen, vop):
    value = gen.operation("call ptr @mule_byte_storage_new()")
    gen.publish(vop.results[0], value)
    gen.check_pointer(value)


def _length(gen, vop):
    storage = _checked_storage(gen, gen.take(vop.args[0]))
    _integer_result(gen, vop, _read_word(gen, storage, 32))
    gen.release(storage)


def _exports(gen, vop):
    storage = _checked_storage(gen, gen.take(vop.args[0]))
    _integer_result(gen, vop, _read_word(gen, storage, 48))
    gen.release(storage)


def _set_length(gen, vop):
    storage = _checked_storage(gen, gen.take(vop.args[0]))
    length = _word(gen, vop.args[1])
    address = _address(gen, storage, 32)
    gen.line(f"store i64 {length}, ptr {address}")
    gen.release(storage)
    _none_result(gen, vop)


def _reserve(gen, vop):
    storage = _checked_storage(gen, gen.take(vop.args[0]))
    additional = _word(gen, vop.args[1])
    status = gen.operation(
        f"call i32 @mule_byte_storage_reserve(ptr {storage}, i64 {additional})"
    )
    gen.release(storage)
    gen.check_status(status)
    _none_result(gen, vop)


def _data(gen, storage):
    address = _address(gen, storage, 24)
    return gen.operation(f"load ptr, ptr {address}")


def _read(gen, vop):
    storage = _checked_storage(gen, gen.take(vop.args[0]))
    index = _word(gen, vop.args[1])
    data = _data(gen, storage)
    address = gen.operation(f"getelementptr i8, ptr {data}, i64 {index}")
    byte = gen.operation(f"load i8, ptr {address}")
    value = gen.operation(f"zext i8 {byte} to i64")
    _integer_result(gen, vop, value)
    gen.release(storage)


def _write(gen, vop):
    storage = _checked_storage(gen, gen.take(vop.args[0]))
    index = _word(gen, vop.args[1])
    value = _word(gen, vop.args[2])
    data = _data(gen, storage)
    address = gen.operation(f"getelementptr i8, ptr {data}, i64 {index}")
    byte = gen.operation(f"trunc i64 {value} to i8")
    gen.line(f"store i8 {byte}, ptr {address}")
    gen.release(storage)
    _none_result(gen, vop)


def _append(gen, vop):
    storage = _checked_storage(gen, gen.take(vop.args[0]))
    value = _word(gen, vop.args[1])
    _append_storage(gen, storage, value)
    gen.release(storage)
    _none_result(gen, vop)


def _append_storage(gen, storage, value):
    length = _read_word(gen, storage, 32)
    capacity = _read_word(gen, storage, 40)
    room = gen.operation(f"icmp ult i64 {length}, {capacity}")
    grow, store = gen.fresh("byte.grow")[1:], gen.fresh("byte.store")[1:]
    gen.line(f"br i1 {room}, label %{store}, label %{grow}")
    gen.lines.append(grow + ":")
    status = gen.operation(f"call i32 @mule_byte_storage_reserve(ptr {storage}, i64 1)")
    gen.check_status(status)
    gen.line(f"br label %{store}")
    gen.lines.append(store + ":")
    data = _data(gen, storage)
    address = gen.operation(f"getelementptr i8, ptr {data}, i64 {length}")
    byte = gen.operation(f"trunc i64 {value} to i8")
    gen.line(f"store i8 {byte}, ptr {address}")
    following = gen.operation(f"add nuw i64 {length}, 1")
    length_address = _address(gen, storage, 32)
    gen.line(f"store i64 {following}, ptr {length_address}")


_OPERATIONS = {
    "_byte_storage_new": (_new, (), BYTE_STORAGE_TYPE, False),
    "_byte_storage_length": (_length, (OBJECT,), BYTE_LENGTH, True),
    "_byte_storage_exports": (_exports, (OBJECT,), BYTE_LENGTH, True),
    "_byte_storage_read": (_read, (OBJECT, WORD), BYTE_VALUE, True),
    "_byte_storage_write": (_write, (OBJECT, WORD, WORD), NONE_TYPE, False),
    "_byte_storage_append": (_append, (OBJECT, WORD), NONE_TYPE, False),
    "_byte_storage_reserve": (_reserve, (OBJECT, WORD), NONE_TYPE, False),
    "_byte_storage_set_length": (_set_length, (OBJECT, WORD), NONE_TYPE, False),
}

PRIMITIVES = {}
for _name, (_emit, _args, _fact, _pure) in _OPERATIONS.items():
    _info = defknown(
        _name,
        *(("flushable",) if _pure else ("call", "unsafe")),
        derive_type=lambda node, fact=_fact: fact,
        borrowed_args=frozenset(range(len(_args))),
        non_integer_first_arg=bool(_args),
        numeric=_fact in (BYTE_LENGTH, BYTE_VALUE),
    )
    PRIMITIVES[_name] = _info
    _results = (
        (WORD,)
        if _fact in (BYTE_LENGTH, BYTE_VALUE)
        else ((DESCRIPTOR, IMMEDIATE, NULL, REFERENCE),)
    )
    for _policy in ("fast-safe", "raw"):
        define_vop(
            f"{_policy}-{_name}",
            translate=_info,
            args=_args,
            results=_results,
            ltn_policy=_policy,
        )(_emit)


ITERATE = defknown(
    "%byte_iterate",
    "call",
    "unsafe",
    derive_type=lambda node: _arg_type(node.args[0]).iterate_type(),
)
AVAILABLE = defknown(
    "%byte_available",
    "call",
    "unsafe",
    derive_type=lambda node: BOOLEAN,
    borrowed_args=frozenset({0}),
)
NEXT = defknown(
    "%byte_next",
    "unsafe",
    derive_type=lambda node: _arg_type(node.args[0]).next_item_type(),
    borrowed_args=frozenset({0}),
    numeric=True,
)


@defpropagate("%byte_iterate")
def _propagate_iterate(visitor, node):
    from .constraint import _propagate_iterate as propagate_sequence

    return propagate_sequence(visitor, node)


@defpropagate("%byte_available")
def _propagate_available(visitor, node):
    from .constraint import _propagate_sequence_available

    if visitor.type_of(node.args[0]).may_run_user_code:
        visitor.invalidate()
    return _propagate_sequence_available(visitor, node)


@defpropagate("%byte_next")
def _propagate_next(visitor, node):
    from .constraint import _propagate_next_item

    return _propagate_next_item(visitor, node)


def _cursor_index(gen, vop):
    state = vop.args[-1].tn.slot
    address = gen.operation(f"getelementptr i64, ptr {state}, i64 2")
    index = gen.operation(f"load i64, ptr {address}")
    return address, index


def _array_storage(gen, array):
    fields_address = _address(gen, array, 32)
    fields = gen.operation(f"load ptr, ptr {fields_address}")
    storage = gen.operation(f"load ptr, ptr {fields}")
    return _checked_storage(gen, storage)


def _append_array_byte(gen, vop):
    array = gen.load(vop.args[0])
    value = _word(gen, vop.args[1])
    storage = _array_storage(gen, array)
    exports = _read_word(gen, storage, 48)
    exported = gen.operation(f"icmp ne i64 {exports}, 0")
    bad = gen.fresh("byte.exported")[1:]
    append = gen.fresh("byte.append")[1:]
    done = gen.fresh("byte.append.done")[1:]
    gen.line(f"br i1 {exported}, label %{bad}, label %{append}")
    gen.lines.append(bad + ":")
    boxed = gen.encode(value)
    result = gen.operation(
        f"call ptr @mule_bytearray_append_fast(ptr {array}, ptr {boxed})"
    )
    gen.check_pointer(result)
    gen.line(f"br label %{done}")
    gen.lines.append(append + ":")
    _append_storage(gen, storage, value)
    gen.line(f"br label %{done}")
    gen.lines.append(done + ":")
    gen.release(gen.take(vop.args[0]))
    _none_result(gen, vop)


def _append_array_general(gen, vop):
    array, item = (gen.load(ref) for ref in vop.args)
    result = gen.operation(
        f"call ptr @mule_bytearray_append_fast(ptr {array}, ptr {item})"
    )
    gen.check_pointer(result)
    for ref in vop.args:
        gen.release(gen.take(ref))
    _none_result(gen, vop)


APPEND_BYTE = defknown(
    "%bytearray_append_byte",
    "call",
    "unsafe",
    derive_type=lambda node: NONE_TYPE,
    borrowed_args=frozenset({0, 1}),
    non_integer_first_arg=True,
)
APPEND_TEMPLATES = {}
for _policy in ("fast-safe", "raw"):
    APPEND_TEMPLATES[_policy, True] = define_vop(
        f"{_policy}-bytearray-append-byte",
        translate=APPEND_BYTE,
        args=(OBJECT, WORD),
        results=((DESCRIPTOR, IMMEDIATE, NULL, REFERENCE),),
        ltn_policy=_policy,
    )(_append_array_byte)
    APPEND_TEMPLATES[_policy, False] = define_vop(
        f"{_policy}-bytearray-append-general",
        translate=APPEND_BYTE,
        args=(OBJECT, OBJECT),
        results=((DESCRIPTOR, IMMEDIATE, NULL, REFERENCE),),
        ltn_policy=_policy,
        cost=1,
    )(_append_array_general)


def _iterate_immutable_bytes(gen, vop):
    sequence = gen.take(vop.args[0])
    state = vop.args[-1].tn.slot
    status = gen.operation(f"call i32 @mule_bytes_storage(ptr {sequence}, ptr {state})")
    gen.check_status(status)
    index = gen.operation(f"getelementptr i64, ptr {state}, i64 2")
    gen.line(f"store i64 0, ptr {index}")
    if not gen.owns_reference(sequence):
        sequence = gen.retain(sequence)
    gen.publish(vop.results[0], sequence)
    gen.refine(vop.results[0].tn, sequence)


def _iterate_mutable_sequence(gen, vop):
    sequence = gen.take(vop.args[0])
    state = vop.args[-1].tn.slot
    index = gen.operation(f"getelementptr i64, ptr {state}, i64 2")
    gen.line(f"store i64 0, ptr {index}")
    if not gen.owns_reference(sequence):
        sequence = gen.retain(sequence)
    gen.publish(vop.results[0], sequence)
    gen.refine(vop.results[0].tn, sequence)


def _available_immutable_bytes(gen, vop):
    gen.load(vop.args[0])
    _, index = _cursor_index(gen, vop)
    state = vop.args[-1].tn.slot
    length_address = gen.operation(f"getelementptr i64, ptr {state}, i64 1")
    length = gen.operation(f"load i64, ptr {length_address}")
    _publish_available(gen, vop, index, length)


def _iterate_tuple(gen, vop):
    sequence = gen.take(vop.args[0])
    state = vop.args[-1].tn.slot
    gen.line(f"store ptr {_data(gen, sequence)}, ptr {state}")
    length = _read_word(gen, sequence, 32)
    length_address = gen.operation(f"getelementptr i64, ptr {state}, i64 1")
    gen.line(f"store i64 {length}, ptr {length_address}")
    index_address = gen.operation(f"getelementptr i64, ptr {state}, i64 2")
    gen.line(f"store i64 0, ptr {index_address}")
    if not gen.owns_reference(sequence):
        sequence = gen.retain(sequence)
    gen.publish(vop.results[0], sequence)
    gen.refine(vop.results[0].tn, sequence)


def _available_list(gen, vop):
    sequence = gen.load(vop.args[0])
    _, index = _cursor_index(gen, vop)
    _publish_available(gen, vop, index, _read_word(gen, sequence, 32))


def _next_tuple(gen, vop):
    sequence = gen.take(vop.args[0])
    data = gen.operation(f"load ptr, ptr {vop.args[-1].tn.slot}")
    _advance_object_cursor(gen, vop, sequence, data)


def _next_list(gen, vop):
    sequence = gen.take(vop.args[0])
    _advance_object_cursor(gen, vop, sequence, _data(gen, sequence))


def _advance_object_cursor(gen, vop, sequence, data):
    index_address, index = _cursor_index(gen, vop)
    address = gen.operation(f"getelementptr ptr, ptr {data}, i64 {index}")
    item = gen.operation(f"load ptr, ptr {address}")
    following = gen.operation(f"add nuw i64 {index}, 1")
    gen.line(f"store i64 {following}, ptr {index_address}")
    gen.publish(vop.results[0], gen.retain(item))
    gen.refine(vop.results[0].tn, item)
    gen.release(sequence)


def _available_bytearray(gen, vop):
    sequence = gen.load(vop.args[0])
    _, index = _cursor_index(gen, vop)
    length = _read_word(gen, _array_storage(gen, sequence), 32)
    _publish_available(gen, vop, index, length)


def _publish_available(gen, vop, index, length):
    available = gen.operation(f"icmp ult i64 {index}, {length}")
    result = gen.operation(f"select i1 {available}, ptr @mule.true, ptr @mule.false")
    gen.publish(vop.results[0], result)
    gen.refine(vop.results[0].tn, result)


def _next_immutable_bytes(gen, vop):
    sequence = gen.take(vop.args[0])
    index_address, index = _cursor_index(gen, vop)
    data = gen.operation(f"load ptr, ptr {vop.args[-1].tn.slot}")
    _advance_byte_cursor(gen, vop, sequence, index_address, index, data)


def _next_bytearray(gen, vop):
    sequence = gen.take(vop.args[0])
    index_address, index = _cursor_index(gen, vop)
    data = _data(gen, _array_storage(gen, sequence))
    _advance_byte_cursor(gen, vop, sequence, index_address, index, data)


def _advance_byte_cursor(gen, vop, sequence, index_address, index, data):
    address = gen.operation(f"getelementptr i8, ptr {data}, i64 {index}")
    byte = gen.operation(f"load i8, ptr {address}")
    value = gen.operation(f"zext i8 {byte} to i64")
    following = gen.operation(f"add nuw i64 {index}, 1")
    gen.line(f"store i64 {following}, ptr {index_address}")
    _integer_result(gen, vop, value)
    gen.release(sequence)


def _iterate_generic(gen, vop):
    sequence = gen.take(vop.args[0])
    move = gen.owns_reference(sequence)
    helper = gen.module.iterate_helper(move=move)
    result = gen.operation(f"call ptr {helper}(ptr {sequence})")
    if not move:
        gen.release(sequence)
    gen.publish(vop.results[0], result)
    gen.check_pointer(result)
    gen.refine(vop.results[0].tn, result)


def _available_generic(gen, vop):
    iterator = gen.load(vop.args[0])
    helper = gen.module.next_item_helper()
    status = gen.operation(
        f"call i32 {helper}(ptr {iterator}, ptr {gen.out_pointer()})"
    )
    gen.check_status(status)
    available = gen.operation(f"icmp ne i32 {status}, 0")
    result = gen.operation(f"select i1 {available}, ptr @mule.true, ptr @mule.false")
    gen.publish(vop.results[0], result)
    gen.refine(vop.results[0].tn, result)


def _next_generic(gen, vop):
    iterator = gen.take(vop.args[0])
    gen.release(iterator)
    item = gen.operation(f"load ptr, ptr {gen.out_pointer()}")
    gen.publish(vop.results[0], item)
    gen.refine(vop.results[0].tn, item)


IMMUTABLE_BYTES_CURSOR = object()
MUTABLE_BYTEARRAY_CURSOR = object()
IMMUTABLE_TUPLE_CURSOR = object()
MUTABLE_LIST_CURSOR = object()
GENERIC_CURSOR = object()
CURSOR_TEMPLATES = {}
for _family, _family_name, _emitters, _next_result in (
    (
        IMMUTABLE_BYTES_CURSOR,
        "immutable-bytes",
        (_iterate_immutable_bytes, _available_immutable_bytes, _next_immutable_bytes),
        WORD,
    ),
    (
        MUTABLE_BYTEARRAY_CURSOR,
        "mutable-bytearray",
        (_iterate_mutable_sequence, _available_bytearray, _next_bytearray),
        WORD,
    ),
    (
        IMMUTABLE_TUPLE_CURSOR,
        "immutable-tuple",
        (_iterate_tuple, _available_immutable_bytes, _next_tuple),
        OBJECT,
    ),
    (
        MUTABLE_LIST_CURSOR,
        "mutable-list",
        (_iterate_mutable_sequence, _available_list, _next_list),
        OBJECT,
    ),
    (
        GENERIC_CURSOR,
        "generic",
        (_iterate_generic, _available_generic, _next_generic),
        OBJECT,
    ),
):
    for _info, _emit in zip((ITERATE, AVAILABLE, NEXT), _emitters, strict=True):
        for _raw in (False, True):
            _result = _next_result if _info is NEXT else OBJECT
            _template = define_vop(
                f"byte-cursor-{_family_name}-{_info.name}-{_raw}",
                translate=_info,
                args=(OBJECT, None),
                results=(_result,),
                ltn_policy="raw" if _raw else "fast-safe",
            )(_emit)
            CURSOR_TEMPLATES[_family, _info, _raw] = _template
