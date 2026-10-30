"""The LLVM backend's templates: one ``define_vop`` per operation.

This is the counterpart of SBCL's ``src/compiler/<backend>/*.lisp`` files.
Each template names the known function it translates (or is a fixed template
the IR2 converter emits for references, assignments, branches, calls, casts,
and returns), restricts its operands' storage classes, and carries the
generator that writes the LLVM text for one VOP.

Object bodies use the templates over tagged object pointers; the raw
templates (``ltn_policy="raw"``) implement the same operations over machine
integers, booleans, and native instance payloads for bodies whose
representation plan proved every value's machine type. The runtime helper
functions shared by the object templates are emitted once per module from
``helpers()``.
"""

from __future__ import annotations

from . import fndb
from .ctype import (
    PYTHON_INTEGER,
    UNKNOWN,
)
from .vmdef import define_vop
from .vop import (
    BOXED_I64,
    DESCRIPTOR,
    DOUBLE,
    FLOAT_BOX_LAYOUT,
    IMMEDIATE,
    INT64_BOX_LAYOUT,
    SIGNED,
)
from .vops_core import (
    ARGS_TAG,
    ATTR_CACHE_WORDS,
    COLD_BRANCH,
    CONSTANT,
    DATA_OFFSET,
    HOT_BRANCH,
    INSTANCE_FIELDS_OFFSET,
    INTRINSICS,
    OBJECT,
    TERMINATORS,
    TEXT_HEAP_DATA_OFFSET,
    TEXT_HEAP_LEN_OFFSET,
    TEXT_INLINE_LEN_OFFSET,
    TEXT_INLINE_WORDS_OFFSET,
    TEXT_KIND_TAG_OFFSET,
    TEXT_TAG,
    VALUE_BUFFER_DATA_OFFSET,
    VALUE_BUFFER_LENGTH_OFFSET,
    BorrowedLiteralEmitter,
    LiteralEmitter,
    _ContractAssertion,
    _decode_fixnum_operand,
    _ownership,
    _ReturnValueKind,
    assert_unpacked_return,
    branch,
    checks_unpacked_return,
    coerce,
    discard,
    double_constant,
    fast_len,
    full_call,
    insert_safepoint,
    jump,
    load_closure,
    load_global,
    load_local,
    local_address,
    local_call,
    local_cell_clear,
    local_cell_ref,
    local_cell_set,
    local_extent_call,
    local_function,
    local_load_closure,
    move,
    move_argument,
    parse_int_slice,
    parse_int_text,
    return_,
    store_global,
    type_check,
    unbind_global,
    unbind_local,
)
from .vops_numeric import _binary_generic, raw_construct, unbox_float_operand
from .vops_object import (
    call_resolved,
    getattr_,
    getattr_borrow,
    physical_field_borrow_end,
    physical_field_read,
    physical_field_read_borrow,
    physical_field_read_fixed,
    physical_field_read_guarded,
    physical_field_read_name,
    physical_field_write,
    physical_field_write_fixed,
    physical_field_write_guarded,
    physical_field_write_name,
    slot_borrow_end,
    value_cell_clear,
    value_cell_ref,
    value_cell_set,
)


def _sequence(gen, vop, primitive):
    values = [gen.take(element) for element in vop.args]
    gen.argv(values)
    buf = "%argv" if values else "null"
    gen.pointer_result(
        vop.results[0], f"call ptr @{primitive}(i64 {len(values)}, ptr {buf})", values
    )


@define_vop("build-list", translate=fndb.BUILD_LIST, results=(OBJECT,))
def build_list(gen, vop):
    _sequence(gen, vop, "mule_sequence_list")


@define_vop("build-tuple", translate=fndb.BUILD_TUPLE, results=(OBJECT,))
def build_tuple(gen, vop):
    values = [gen.take(element) for element in vop.args]
    if gen.is_unpacked_return_vop(vop):
        gen.unpacked_return_result(vop.results[0], values)
        return
    gen.argv(values)
    buf = "%argv" if values else "null"
    gen.pointer_result(
        vop.results[0],
        f"call ptr @mule_sequence_tuple_owned(i64 {len(values)}, ptr {buf})",
    )


@define_vop("format", translate=fndb.FORMAT, args=(OBJECT, OBJECT), results=(OBJECT,))
def format_(gen, vop):
    result = vop.results[0]
    value = gen.take(vop.args[0])
    conversion = vop.codegen_info.conversion
    if conversion is not None:
        protocol = {"s": "string", "r": "representation", "a": "ascii_representation"}[
            conversion
        ]
        converted = gen.compiled_call_named("mule_format_" + protocol, [value])
        gen.publish(result, converted)
        gen.check_pointer(converted)
        value = gen.take(result)
    specification = gen.take(vop.args[1])
    formatted = gen.compiled_call_named(
        "mule_format_format_value", [value, specification]
    )
    gen.publish(result, formatted)
    gen.check_pointer(formatted)
    gen.refine(result.tn, formatted)


@define_vop(
    "range-prepare",
    translate=fndb.RANGE_PREPARE,
    results=(OBJECT, OBJECT),
    arg_escapes=(False, False, False, False),
)
def range_prepare(gen, vop):
    info = vop.codegen_info
    if info is not None and info.direct:
        state = vop.args[-1].tn.slot
        arity = len(vop.args) - 1
        if arity == 1:
            start = "0"
            stop = _decode_fixnum_operand(gen, vop.args[0])
            step = "1"
        elif arity == 2:
            start = _decode_fixnum_operand(gen, vop.args[0])
            stop = _decode_fixnum_operand(gen, vop.args[1])
            step = "1"
        else:
            start = _decode_fixnum_operand(gen, vop.args[0])
            stop = _decode_fixnum_operand(gen, vop.args[1])
            step = _decode_fixnum_operand(gen, vop.args[2])
        for index, word in enumerate((start, stop, step)):
            address = gen.operation(f"getelementptr i64, ptr {state}, i64 {index}")
            gen.line(f"store i64 {word}, ptr {address}")
        if vop.results:
            gen.publish(vop.results[0], "@mule.true")
            gen.refine(vop.results[0].tn, "@mule.true")
        return
    if info is not None and info.fused:
        state = vop.args[-1].tn.slot
        arg_operands = vop.args[:-1]
        arity = len(arg_operands)
        result, iterator_out = vop.results
        if info.step_sign != 0 and all(
            op.tn.type is not None
            and op.tn.type.fits(PYTHON_INTEGER)
            and op.tn.type.is_fixnum
            and not op.tn.type.may_be_unbound
            for op in arg_operands
        ):
            if arity == 1:
                start = "0"
                stop = _decode_fixnum_operand(gen, arg_operands[0])
                step = "1"
            elif arity == 2:
                start = _decode_fixnum_operand(gen, arg_operands[0])
                stop = _decode_fixnum_operand(gen, arg_operands[1])
                step = "1"
            else:
                start = _decode_fixnum_operand(gen, arg_operands[0])
                stop = _decode_fixnum_operand(gen, arg_operands[1])
                step = _decode_fixnum_operand(gen, arg_operands[2])
            for index, word in enumerate((start, stop, step)):
                address = gen.operation(f"getelementptr i64, ptr {state}, i64 {index}")
                gen.line(f"store i64 {word}, ptr {address}")
            gen.publish(result, "@mule.true")
            gen.refine(result.tn, "@mule.true")
            return
        arg_values = [gen.take(op) for op in arg_operands]
        bits_list = []
        fast_ok = None
        for val in arg_values:
            bits = gen.operation(f"ptrtoint ptr {val} to i64")
            bits_list.append(bits)
            tag = gen.operation(f"and i64 {bits}, 1")
            is_fix = gen.operation(f"icmp ne i64 {tag}, 0")
            fast_ok = (
                is_fix
                if fast_ok is None
                else gen.operation(f"and i1 {fast_ok}, {is_fix}")
            )
        step_decoded = None
        if arity == 3 and info.step_sign == 0:
            step_decoded = gen.operation(f"ashr i64 {bits_list[2]}, 1")
            step_nz = gen.operation(f"icmp ne i64 {step_decoded}, 0")
            fast_ok = gen.operation(f"and i1 {fast_ok}, {step_nz}")
        fast_label, slow_label, done_label = (
            gen.fresh(name)[1:] for name in ("range.fast", "range.slow", "range.done")
        )
        gen.line(
            f"br i1 {fast_ok}, label %{fast_label}, label %{slow_label}, {HOT_BRANCH}"
        )
        gen.lines.append(f"{fast_label}:")
        if arity == 1:
            start = "0"
            stop = gen.operation(f"ashr i64 {bits_list[0]}, 1")
            step = "1"
        elif arity == 2:
            start = gen.operation(f"ashr i64 {bits_list[0]}, 1")
            stop = gen.operation(f"ashr i64 {bits_list[1]}, 1")
            step = "1"
        else:
            start = gen.operation(f"ashr i64 {bits_list[0]}, 1")
            stop = gen.operation(f"ashr i64 {bits_list[1]}, 1")
            step = (
                step_decoded
                if step_decoded is not None
                else gen.operation(f"ashr i64 {bits_list[2]}, 1")
            )
        for index, word in enumerate((start, stop, step)):
            address = gen.operation(f"getelementptr i64, ptr {state}, i64 {index}")
            gen.line(f"store i64 {word}, ptr {address}")
        fast_end = gen.current_block()
        gen.line(f"br label %{done_label}")
        gen.lines.append(f"{slow_label}:")
        with gen.cold_block():
            callee = gen.operation(
                gen.module.load_global(info.callee_leaf, gen.lambda_.policy.safety)
            )
            call_helper = gen.module.positional_call_helper(arity, borrowed=True)
            operands = ", ".join(
                [f"ptr {callee}", *(f"ptr {val}" for val in arg_values)]
            )
            range_obj = gen.operation(f"call ptr {call_helper}({operands})")
            gen.release(callee)
            for val in arg_values:
                gen.release(val)
            gen.check_pointer(range_obj)
            iter_helper = gen.module.iterate_helper(move=True)
            iter_obj = gen.operation(f"call ptr {iter_helper}(ptr {range_obj})")
            gen.check_pointer(iter_obj)
            gen.store(iterator_out, iter_obj)
            stored = gen.operation(
                f"call i32 @mule_range_storage(ptr {iter_obj}, ptr %range.buffer)"
            )
            words = gen.operation("load [3 x i64], ptr %range.buffer")
            gen.line(f"store [3 x i64] {words}, ptr {state}")
            accepted = gen.operation(f"icmp ne i32 {stored}, 0")
            slow_res = gen.operation(
                f"select i1 {accepted}, ptr @mule.true, ptr @mule.false"
            )
            slow_end = gen.current_block()
            gen.line(f"br label %{done_label}")
        gen.lines.append(f"{done_label}:")
        final_res = gen.operation(
            f"phi ptr [@mule.true, %{fast_end}], [{slow_res}, %{slow_end}]"
        )
        gen.publish(result, final_res)
        gen.refine(result.tn, final_res)
        return
    callee, iterator = gen.take(vop.args[0]), gen.take(vop.args[1])
    state = vop.args[2].tn.slot
    # No callable-identity guard: `range` is a class, and the storage check
    # only accepts iterators of the protocol's own RangeIterator class.
    stored = gen.operation(
        f"call i32 @mule_range_storage(ptr {iterator}, ptr %range.buffer)"
    )
    words = gen.operation("load [3 x i64], ptr %range.buffer")
    gen.line(f"store [3 x i64] {words}, ptr {state}")
    accepted = gen.operation(f"icmp ne i32 {stored}, 0")
    result = gen.operation(f"select i1 {accepted}, ptr @mule.true, ptr @mule.false")
    gen.publish(vop.results[0], result)
    gen.refine(vop.results[0].tn, result)
    gen.release(callee)
    gen.release(iterator)


def _range_words(gen, state):
    words = []
    for index in range(3):
        address = gen.operation(f"getelementptr i64, ptr {state}, i64 {index}")
        words.append(gen.operation(f"load i64, ptr {address}"))
    return words


@define_vop("range-available", translate=fndb.RANGE_AVAILABLE, results=(OBJECT,))
def range_available(gen, vop):
    info = vop.codegen_info
    state = vop.args[0].tn.slot
    if info is not None and info.step_sign > 0:
        current = gen.operation(f"load i64, ptr {state}")
        stop_addr = gen.operation(f"getelementptr i64, ptr {state}, i64 1")
        stop = gen.operation(f"load i64, ptr {stop_addr}")
        available = gen.operation(f"icmp slt i64 {current}, {stop}")
    elif info is not None and info.step_sign < 0:
        current = gen.operation(f"load i64, ptr {state}")
        stop_addr = gen.operation(f"getelementptr i64, ptr {state}, i64 1")
        stop = gen.operation(f"load i64, ptr {stop_addr}")
        available = gen.operation(f"icmp sgt i64 {current}, {stop}")
    else:
        current, stop, step = _range_words(gen, state)
        forward = gen.operation(f"icmp sgt i64 {step}, 0")
        below = gen.operation(f"icmp slt i64 {current}, {stop}")
        above = gen.operation(f"icmp sgt i64 {current}, {stop}")
        available = gen.operation(f"select i1 {forward}, i1 {below}, i1 {above}")
    result = gen.operation(f"select i1 {available}, ptr @mule.true, ptr @mule.false")
    gen.publish(vop.results[0], result)
    gen.refine(vop.results[0].tn, result)


@define_vop(
    "range-advance",
    translate=fndb.RANGE_ADVANCE,
    results=((DESCRIPTOR, IMMEDIATE, SIGNED),),
)
def range_advance(gen, vop):
    info = vop.codegen_info
    state = vop.args[0].tn.slot
    if info is not None and len(info.arguments) <= 2:
        current = gen.operation(f"load i64, ptr {state}")
        following = gen.operation(f"add nsw i64 {current}, 1")
    else:
        current, _, step = _range_words(gen, state)
        following = gen.operation(f"add nsw i64 {current}, {step}")
    gen.line(f"store i64 {following}, ptr {state}")
    if vop.results[0].tn.sc is SIGNED:
        gen.store(vop.results[0], current)
        return
    encoded = gen.operation(f"shl nsw i64 {current}, 1")
    tagged = gen.operation(f"or disjoint i64 {encoded}, 1")
    result = gen.operation(f"inttoptr i64 {tagged} to ptr")
    gen.publish(vop.results[0], result)
    gen.refine(vop.results[0].tn, result)


@define_vop("sequence-available", translate=fndb.SEQUENCE_AVAILABLE, results=(OBJECT,))
def sequence_available(gen, vop):
    info = vop.codegen_info
    if info is not None:
        sequence = gen.load(vop.args[0])
        pos = gen.operation(f"load i64, ptr {info.index_tn.slot}")
        exact_len = info.seq_type.exact_length()
        if exact_len is not None:
            length = str(exact_len)
        else:
            len_addr = gen.operation(
                f"getelementptr i64, ptr {sequence}, i64 {VALUE_BUFFER_LENGTH_OFFSET // 8}"
            )
            length = gen.operation(f"load i64, ptr {len_addr}")
        available = gen.operation(f"icmp ult i64 {pos}, {length}")
        result = gen.operation(
            f"select i1 {available}, ptr @mule.true, ptr @mule.false"
        )
        gen.publish(vop.results[0], result)
        gen.refine(vop.results[0].tn, result)
        return
    iterator = gen.load(vop.args[0])
    out = gen.out_pointer()
    unpack_arity = gen.unpacked_sequence_available_arity(vop)
    if unpack_arity is not None:
        helper = gen.module.next_unpack_helper(unpack_arity)
        status = gen.operation(
            f"call i32 {helper}(ptr {iterator}, ptr %unpack.out, ptr {out})"
        )
        gen.check_status(status)
        available = gen.operation(f"icmp ne i32 {status}, 0")
        result = gen.operation(
            f"select i1 {available}, ptr @mule.true, ptr @mule.false"
        )
        gen.publish(vop.results[0], result)
        gen.refine(vop.results[0].tn, result)
        return
    owned = gen.retain(iterator)
    item = gen.sequence_call("next_item", [owned])
    gen.line(f"store ptr {item}, ptr {out}")
    gen.check_pointer(item)
    gen.publish(vop.results[0], "@mule.true")
    gen.refine(vop.results[0].tn, "@mule.true")


@define_vop(
    "protocol-next_item", translate=fndb.PROTOCOLS["next_item"], results=(OBJECT,)
)
def protocol_next_item(gen, vop):
    info = vop.codegen_info
    if info is not None:
        gen.take(vop.args[0])
        sequence = gen.operation(f"load ptr, ptr {info.iterator_local.slot}")
        pos = gen.operation(f"load i64, ptr {info.index_tn.slot}")
        next_pos = gen.operation(f"add nuw nsw i64 {pos}, 1")
        gen.line(f"store i64 {next_pos}, ptr {info.index_tn.slot}")
        data_addr = gen.operation(
            f"getelementptr ptr, ptr {sequence}, i64 {VALUE_BUFFER_DATA_OFFSET // 8}"
        )
        data = gen.operation(f"load ptr, ptr {data_addr}")
        slot = gen.operation(f"getelementptr ptr, ptr {data}, i64 {pos}")
        result = gen.operation(f"load ptr, ptr {slot}, !nonnull !{{}}")
        res_tn = vop.results[0].tn
        res_type = res_tn.type or UNKNOWN
        unchecked_contract = gen._has_unchecked_element_contract(res_tn)
        immortal_elem = not unchecked_contract and (
            not res_tn.sc.owned or res_type.is_fixnum or res_type.is_immortal
        )
        if not info.borrow_element and not immortal_elem:
            if not unchecked_contract and res_type.is_heap_object:
                gen.heap_values.add(result)
            result = gen.retain(result)
        gen.publish(vop.results[0], result)
        gen.refine(vop.results[0].tn, result)
        return
    iterator = gen.take(vop.args[0])
    if iterator in ("null", "@mule.none"):
        item = gen.operation(f"load ptr, ptr {gen.out_pointer()}")
        gen.publish(vop.results[0], item)
        gen.refine(vop.results[0].tn, item)
        return
    result = gen.sequence_call("next_item", [iterator])
    gen.publish(vop.results[0], result)
    gen.check_pointer(result)
    gen.refine(vop.results[0].tn, result)


@define_vop("protocol-iterate", translate=fndb.PROTOCOLS["iterate"], results=(OBJECT,))
def protocol_iterate(gen, vop):
    info = vop.codegen_info
    if info is not None:
        sequence = gen.take(vop.args[0])
        gen.line(f"store i64 0, ptr {info.index_tn.slot}")
        if vop.results[0].tn.borrowed_from is not None or gen.owns_reference(sequence):
            result = sequence
        else:
            result = gen.retain(sequence)
        gen.publish(vop.results[0], result)
        gen.refine(vop.results[0].tn, result)
        return
    sequence = gen.take(vop.args[0])
    if not gen.owns_reference(sequence):
        sequence = gen.retain(sequence)
    result = gen.sequence_call("iterate", [sequence])
    gen.publish(vop.results[0], result)
    gen.check_pointer(result)
    gen.refine(vop.results[0].tn, result)


_SUSPENSION_PROTOCOLS = frozenset(
    {"yield_from", "await_value", "async_generator_yield"}
)

_BORROWED_PROTOCOL_NAMES: frozenset[str] = frozenset()


def _define_protocol(name, info):
    suspends = name in _SUSPENSION_PROTOCOLS
    borrowed = name in _BORROWED_PROTOCOL_NAMES

    @define_vop(f"protocol-{name}", translate=info, results=(OBJECT,))
    def protocol(gen, vop):
        arguments = [gen.take(value) for value in vop.args]
        if suspends:
            gen.sync_coroutine_locals()
        result = gen.sequence_call(name, arguments)
        if borrowed:
            for argument in arguments:
                gen.release(argument)
        gen.publish(vop.results[0], result)
        gen.check_pointer(result)
        gen.refine(vop.results[0].tn, result)


@define_vop("structural-unpack", translate=fndb.PROTOCOLS["unpack"], results=(OBJECT,))
def structural_unpack(gen, vop):
    value, count = [gen.take(argument) for argument in vop.args]
    if gen.is_unpacked_unpack(vop):
        gen.immortal_values.add("@mule.none")
        gen.publish(vop.results[0], "@mule.none")
        gen.release(count)
        return
    slot_arity = gen.unpacked_slot_arity(vop)
    if slot_arity is not None:
        if vop.codegen_info.proved:
            # The exact tuple and its arity are already proven. Acquire every
            # child before a target can mutate another binding to the source.
            # Iterator delivery uses null to signal that these slots are filled.
            already = gen.operation(f"icmp eq ptr {value}, null")
            copy_label = gen.fresh("unpack.copy")[1:]
            done_label = gen.fresh("unpack.done")[1:]
            gen.line(f"br i1 {already}, label %{done_label}, label %{copy_label}")
            gen.lines.append(copy_label + ":")
            for index in range(slot_arity):
                child = gen.operation(
                    f"call ptr @mule.fast.tuple.slot(ptr {value}, i64 {index})"
                )
                slot = gen.operation(f"getelementptr ptr, ptr %unpack.out, i64 {index}")
                gen.line(f"store ptr {child}, ptr {slot}")
            gen.release(value)
            gen.line(f"br label %{done_label}")
            gen.lines.append(done_label + ":")
            gen.release(count)
            gen.immortal_values.add("@mule.none")
            gen.publish(vop.results[0], "@mule.none")
            return
        helper = gen.module.unpack_slots_helper(slot_arity)
        gen.pointer_result(
            vop.results[0],
            f"call ptr {helper}(ptr {value}, ptr {count}, ptr %unpack.out)",
        )
        return
    if vop.codegen_info.proved:
        gen.publish(vop.results[0], value)
        gen.refine(vop.results[0].tn, value)
        gen.release(count)
        return
    helper = gen.module.unpack_helper(vop.codegen_info.arity)
    gen.pointer_result(vop.results[0], f"call ptr {helper}(ptr {value}, ptr {count})")


@define_vop("protocol-getitem", translate=fndb.PROTOCOLS["getitem"], results=(OBJECT,))
def protocol_getitem(gen, vop):
    seq_type = vop.args[0].tn.type or UNKNOWN
    idx_type = vop.args[1].tn.type or UNKNOWN
    if seq_type.fits_bytearray() and idx_type.is_fixnum:
        sequence = gen.take(vop.args[0])
        raw_index = vop.args[1].tn.sc is SIGNED
        index = (
            gen.encode(gen.take(vop.args[1])) if raw_index else gen.take(vop.args[1])
        )
        result = gen.operation(
            f"call ptr @mule_bytearray_getitem_fast(ptr {sequence}, ptr {index})"
        )
        gen.immortal_values.add(result)
        gen.release(sequence)
        if not raw_index:
            gen.release(index)
        gen.publish(vop.results[0], result)
        gen.check_pointer(result)
        gen.refine(vop.results[0].tn, result)
        return
    if seq_type.fits_bytes(seq_type) and idx_type.is_fixnum:
        sequence = gen.take(vop.args[0])
        raw_index = vop.args[1].tn.sc is SIGNED
        index = (
            gen.encode(gen.take(vop.args[1])) if raw_index else gen.take(vop.args[1])
        )
        result = gen.operation(
            f"call ptr @mule_bytes_getitem(ptr {sequence}, ptr {index})"
        )
        gen.immortal_values.add(result)
        gen.release(sequence)
        if not raw_index:
            gen.release(index)
        gen.publish(vop.results[0], result)
        gen.check_pointer(result)
        gen.refine(vop.results[0].tn, result)
        return
    sequence = gen.take(vop.args[0])
    index = gen.take(vop.args[1])
    raw_index_sc = vop.args[1].tn.sc is SIGNED
    info = vop.codegen_info
    if info is None and idx_type.is_fixnum:
        if seq_type.is_exact_list:
            info = "list_owned"
        elif seq_type.is_exact_tuple:
            info = "tuple_owned"
    if info in ("list_borrow", "list_owned", "tuple_borrow", "tuple_owned"):
        if info == "list_borrow":
            helper = "@mule_list_getitem_borrow"
        elif info == "list_owned":
            helper = "@mule_list_getitem"
        else:
            helper = gen.module.getitem_helper()
        seq_type = vop.args[0].tn.type or UNKNOWN
        idx_type = vop.args[1].tn.type or UNKNOWN
        non_negative = idx_type.minimum is not None and idx_type.minimum >= 0
        in_bounds = vop.node in gen.context.in_bounds_nodes and non_negative
        if raw_index_sc:
            raw = index
        else:
            bits = gen.operation(f"ptrtoint ptr {index} to i64")
            raw = gen.operation(f"ashr i64 {bits}, 1")
        res_tn = vop.results[0].tn
        res_type = res_tn.type or UNKNOWN
        unchecked_contract = gen._has_unchecked_element_contract(res_tn)
        immortal_elem = not unchecked_contract and (
            not res_tn.sc.owned or res_type.is_fixnum or res_type.is_immortal
        )
        if in_bounds:
            data_addr = gen.operation(
                f"getelementptr ptr, ptr {sequence}, i64 {VALUE_BUFFER_DATA_OFFSET // 8}"
            )
            data = gen.operation(f"load ptr, ptr {data_addr}")
            slot = gen.operation(f"getelementptr ptr, ptr {data}, i64 {raw}")
            result = gen.operation(f"load ptr, ptr {slot}, !nonnull !{{}}")
            if info in ("list_owned", "tuple_owned") and not immortal_elem:
                if not unchecked_contract and res_type.is_heap_object:
                    gen.heap_values.add(result)
                result = gen.retain(result)
            gen.nonnull_values.add(result)
        else:
            exact_len = seq_type.exact_length()
            if exact_len is not None:
                length = str(exact_len)
            else:
                len_addr = gen.operation(
                    f"getelementptr i64, ptr {sequence}, i64 {VALUE_BUFFER_LENGTH_OFFSET // 8}"
                )
                length = gen.operation(f"load i64, ptr {len_addr}")
            if non_negative:
                pos = raw
            else:
                neg = gen.operation(f"icmp slt i64 {raw}, 0")
                wrapped = gen.operation(f"add i64 {raw}, {length}")
                pos = gen.operation(f"select i1 {neg}, i64 {wrapped}, i64 {raw}")
            inside = gen.operation(f"icmp ult i64 {pos}, {length}")
            read_lbl = gen.fresh("list.read")[1:]
            slow_lbl = gen.fresh("list.slow")[1:]
            done_lbl = gen.fresh("list.done")[1:]
            gen.line(
                f"br i1 {inside}, label %{read_lbl}, label %{slow_lbl}, "
                f'!prof !{{!"branch_weights", i32 2000, i32 1}}'
            )
            gen.lines.append(read_lbl + ":")
            data_addr = gen.operation(
                f"getelementptr ptr, ptr {sequence}, i64 {VALUE_BUFFER_DATA_OFFSET // 8}"
            )
            data = gen.operation(f"load ptr, ptr {data_addr}")
            slot = gen.operation(f"getelementptr ptr, ptr {data}, i64 {pos}")
            fast_val = gen.operation(f"load ptr, ptr {slot}, !nonnull !{{}}")
            if info in ("list_owned", "tuple_owned") and not immortal_elem:
                if not unchecked_contract and res_type.is_heap_object:
                    gen.heap_values.add(fast_val)
                fast_val = gen.retain(fast_val)
            gen.line(f"br label %{done_lbl}")
            gen.lines.append(slow_lbl + ":")
            with gen.cold_block():
                boxed_index = gen.encode(index) if raw_index_sc else index
                slow_val = gen.operation(
                    f"call ptr {helper}(ptr {sequence}, ptr {boxed_index})"
                )
                gen.line(f"br label %{done_lbl}")
            gen.lines.append(done_lbl + ":")
            result = gen.operation(
                f"phi ptr [{fast_val}, %{read_lbl}], [{slow_val}, %{slow_lbl}]"
            )
    else:
        in_bounds = False
        helper = gen.module.getitem_helper()
        boxed_index = gen.encode(index) if raw_index_sc else index
        result = gen.operation(f"call ptr {helper}(ptr {sequence}, ptr {boxed_index})")
    gen.release(sequence)
    if not raw_index_sc:
        gen.release(index)
    gen.publish(vop.results[0], result)
    if not in_bounds:
        gen.check_pointer(result)
    gen.refine(vop.results[0].tn, result)


@define_vop("protocol-getslice", translate=fndb.GETSLICE, results=(OBJECT,))
def protocol_getslice(gen, vop):
    sequence = gen.take(vop.args[0])
    start = gen.take(vop.args[1])
    stop = gen.take(vop.args[2])
    step = gen.take(vop.args[3])
    helper = gen.module.getslice_helper()
    result = gen.operation(
        f"call ptr {helper}(ptr {sequence}, ptr {start}, ptr {stop}, ptr {step})"
    )
    gen.release(sequence)
    gen.release(start)
    gen.release(stop)
    gen.release(step)
    gen.publish(vop.results[0], result)
    gen.check_pointer(result)
    gen.refine(vop.results[0].tn, result)


@define_vop(
    "text-slice-compare",
    translate=fndb.TEXT_SLICE_COMPARE,
    args=(OBJECT, OBJECT, OBJECT, OBJECT),
    results=(OBJECT,),
    arg_escapes=(False, False, False, False),
)
def text_slice_compare(gen, vop):
    sequence = gen.take(vop.args[0])
    start = gen.take(vop.args[1])
    stop = gen.take(vop.args[2])
    target = gen.take(vop.args[3])
    invert = int(bool(vop.codegen_info))
    boolean = gen.operation(
        f"call ptr @mule_text_slice_eq(ptr {sequence}, ptr {start}, ptr {stop}, "
        f"ptr {target}, i32 {invert})"
    )
    gen.native_result(vop.results[0], boolean, (sequence, start, stop, target))


@define_vop(
    "text-slice2-compare",
    translate=fndb.TEXT_SLICE2_COMPARE,
    args=(OBJECT, OBJECT, OBJECT, OBJECT, OBJECT, OBJECT),
    results=(OBJECT,),
    arg_escapes=(False, False, False, False, False, False),
)
def text_slice2_compare(gen, vop):
    left = gen.take(vop.args[0])
    start1 = gen.take(vop.args[1])
    stop1 = gen.take(vop.args[2])
    right = gen.take(vop.args[3])
    start2 = gen.take(vop.args[4])
    stop2 = gen.take(vop.args[5])
    invert = int(bool(vop.codegen_info))
    boolean = gen.operation(
        f"call ptr @mule_text_slice2_eq(ptr {left}, ptr {start1}, ptr {stop1}, "
        f"ptr {right}, ptr {start2}, ptr {stop2}, i32 {invert})"
    )
    gen.native_result(
        vop.results[0], boolean, (left, start1, stop1, right, start2, stop2)
    )


@define_vop(
    "protocol-setitem",
    translate=fndb.PROTOCOLS["setitem"],
    args=(OBJECT, OBJECT, (*OBJECT, DOUBLE)),
    results=(OBJECT,),
)
def protocol_setitem(gen, vop):
    seq_type = vop.args[0].tn.type or UNKNOWN
    idx_type = vop.args[1].tn.type or UNKNOWN
    if vop.args[2].tn.sc is DOUBLE:
        sequence = gen.take(vop.args[0])
        index = gen.take(vop.args[1])
        raw_item = gen.take(vop.args[2])
        boxed_item = gen.operation(
            f"call ptr @{FLOAT_BOX_LAYOUT.symbol()}(double {raw_item})"
        )
        slow_index = gen.encode(index) if vop.args[1].tn.sc is SIGNED else index
        helper = gen.module.setitem_helper(move=True)
        result = gen.operation(
            f"call ptr {helper}(ptr {sequence}, ptr {slow_index}, ptr {boxed_item})"
        )
        gen.immortal_values.add(result)
        gen.release(sequence)
        if vop.args[1].tn.sc in OBJECT:
            gen.release(index)
        gen.publish(vop.results[0], result)
        gen.check_pointer(result)
        gen.refine(vop.results[0].tn, result)
        return
    if seq_type.is_exact_list and idx_type.is_fixnum:
        sequence = gen.take(vop.args[0])
        index = gen.take(vop.args[1])
        item = gen.take(vop.args[2])
        if vop.args[2].tn.sc is SIGNED:
            item = gen.encode(item)
            gen.immortal_values.add(item)
            move_item = True
        else:
            move_item = gen.owns_reference(item) and item != sequence and item != index
        val_type = vop.args[2].tn.type or UNKNOWN
        elem_type = seq_type.iterate_type().next_item_type()
        non_negative = idx_type.minimum is not None and idx_type.minimum >= 0
        in_bounds = vop.node in gen.context.in_bounds_nodes and non_negative
        if vop.args[1].tn.sc is SIGNED:
            raw_idx = index
        else:
            bits = gen.operation(f"ptrtoint ptr {index} to i64")
            raw_idx = gen.operation(f"ashr i64 {bits}, 1")
        if in_bounds:
            pos = raw_idx
        else:
            len_addr = gen.operation(
                f"getelementptr i64, ptr {sequence}, i64 {VALUE_BUFFER_LENGTH_OFFSET // 8}"
            )
            length = gen.operation(f"load i64, ptr {len_addr}")
            if non_negative:
                pos = raw_idx
            else:
                neg = gen.operation(f"icmp slt i64 {raw_idx}, 0")
                wrapped = gen.operation(f"add i64 {raw_idx}, {length}")
                pos = gen.operation(f"select i1 {neg}, i64 {wrapped}, i64 {raw_idx}")
            inside = gen.operation(f"icmp ult i64 {pos}, {length}")
            write_lbl = gen.fresh("list.set")[1:]
            slow_lbl = gen.fresh("list.sslow")[1:]
            done_lbl = gen.fresh("list.sdone")[1:]
            gen.line(
                f"br i1 {inside}, label %{write_lbl}, label %{slow_lbl}, "
                f'!prof !{{!"branch_weights", i32 2000, i32 1}}'
            )
            gen.lines.append(write_lbl + ":")
        data_addr = gen.operation(
            f"getelementptr ptr, ptr {sequence}, i64 {VALUE_BUFFER_DATA_OFFSET // 8}"
        )
        data = gen.operation(f"load ptr, ptr {data_addr}")
        slot = gen.operation(f"getelementptr ptr, ptr {data}, i64 {pos}")
        old_val = gen.operation(f"load ptr, ptr {slot}, !nonnull !{{}}")
        if move_item or val_type.is_fixnum or val_type.is_immortal:
            owned_val = item
        else:
            if val_type.is_heap_object:
                gen.heap_values.add(item)
            owned_val = gen.retain(item)
        gen.line(f"store ptr {owned_val}, ptr {slot}")
        if not (
            val_type.is_fixnum or val_type.is_immortal or val_type.fits_text(val_type)
        ):
            gen.line(f"call void @mule_storage_track(ptr {sequence}, ptr {item})")
        if not (elem_type.is_fixnum or elem_type.is_immortal):
            gen.release(old_val)
        if in_bounds:
            result = "@mule.none"
        else:
            write_end = gen.current_block()
            gen.line(f"br label %{done_lbl}")
            gen.lines.append(slow_lbl + ":")
            with gen.cold_block():
                slow_index = gen.encode(index) if vop.args[1].tn.sc is SIGNED else index
                helper = gen.module.setitem_helper(move=move_item)
                slow_res = gen.operation(
                    f"call ptr {helper}(ptr {sequence}, ptr {slow_index}, ptr {item})"
                )
                slow_end = gen.current_block()
                gen.line(f"br label %{done_lbl}")
            gen.lines.append(done_lbl + ":")
            result = gen.operation(
                f"phi ptr [@mule.none, %{write_end}], [{slow_res}, %{slow_end}]"
            )
        gen.immortal_values.add(result)
        gen.release(sequence)
        if vop.args[1].tn.sc in OBJECT:
            gen.release(index)
        if not move_item and vop.args[2].tn.sc in OBJECT:
            gen.release(item)
        gen.publish(vop.results[0], result)
        if not in_bounds:
            gen.check_pointer(result)
        gen.refine(vop.results[0].tn, result)
        return
    if seq_type.fits_bytearray() and idx_type.is_fixnum:
        sequence = gen.take(vop.args[0])
        index = gen.take(vop.args[1])
        item = gen.take(vop.args[2])
        move_item = gen.owns_reference(item) and item != sequence and item != index
        status = gen.operation(
            f"call i32 @mule_bytearray_setitem_fast(ptr {sequence}, ptr {index}, ptr {item})"
        )
        hit = gen.operation(f"icmp ne i32 {status}, 0")
        done_lbl, slow_lbl = (
            gen.fresh("bytearray.set.done")[1:],
            gen.fresh("bytearray.set.slow")[1:],
        )
        fast_end = gen.current_block()
        gen.line(f"br i1 {hit}, label %{done_lbl}, label %{slow_lbl}, {HOT_BRANCH}")
        gen.lines.append(slow_lbl + ":")
        with gen.cold_block():
            helper = gen.module.setitem_helper(move=move_item)
            slow = gen.operation(
                f"call ptr {helper}(ptr {sequence}, ptr {index}, ptr {item})"
            )
            slow_end = gen.current_block()
            gen.line(f"br label %{done_lbl}")
        gen.lines.append(done_lbl + ":")
        result = gen.operation(
            f"phi ptr [@mule.none, %{fast_end}], [{slow}, %{slow_end}]"
        )
        gen.immortal_values.add(result)
        gen.release(sequence)
        gen.release(index)
        if not move_item:
            gen.release(item)
        gen.publish(vop.results[0], result)
        gen.check_pointer(result)
        gen.refine(vop.results[0].tn, result)
        return
    sequence = gen.take(vop.args[0])
    index = gen.take(vop.args[1])
    item = gen.take(vop.args[2])
    move_item = gen.owns_reference(item) and item != sequence and item != index
    helper = gen.module.setitem_helper(move=move_item)
    result = gen.operation(
        f"call ptr {helper}(ptr {sequence}, ptr {index}, ptr {item})"
    )
    gen.immortal_values.add(result)
    gen.release(sequence)
    gen.release(index)
    if not move_item:
        gen.release(item)
    gen.publish(vop.results[0], result)
    gen.check_pointer(result)
    gen.refine(vop.results[0].tn, result)


for _name, _info in fndb.PROTOCOLS.items():
    if _name not in ("unpack", "getitem", "setitem", "next_item", "iterate"):
        _define_protocol(_name, _info)


@define_vop("unpack-ref", translate=fndb.UNPACK_REF, results=(OBJECT,))
def unpack_ref(gen, vop):
    slot_idx = gen.unpacked_ref_slot(vop)
    if slot_idx is not None:
        position = gen.take(vop.args[1])
        slot = gen.operation(f"getelementptr ptr, ptr %unpack.out, i64 {slot_idx}")
        result = gen.operation(f"load ptr, ptr {slot}")
        gen.unpacked_remaining_slots = gen.unpacked_ref_remaining(vop)
        if gen.is_unpacked_immortal_ref(vop):
            gen.immortal_values.add(result)
        gen.publish(vop.results[0], result)
        gen.release(position)
        gen.refine(vop.results[0].tn, result)
        return
    access = vop.codegen_info
    parent = gen.load(vop.args[0]) if access.borrow_parent else gen.take(vop.args[0])
    position = gen.take(vop.args[1])
    res_tn = vop.results[0].tn
    res_type = res_tn.type or UNKNOWN
    immortal_elem = not gen._has_unchecked_element_contract(res_tn) and (
        not res_tn.sc.owned or res_type.is_fixnum or res_type.is_immortal
    )
    if access.proved:
        slot_helper = (
            "@mule.fast.tuple.borrow"
            if access.borrow_element or immortal_elem
            else "@mule.fast.tuple.slot"
        )
        result = gen.operation(
            f"call ptr {slot_helper}(ptr {parent}, i64 {access.index})"
        )
    else:
        result = gen.operation(
            f"call ptr @mule_storage_read(ptr {parent}, ptr {position})"
        )
    gen.publish(vop.results[0], result)
    if not access.borrow_parent:
        gen.release(parent)
    gen.release(position)
    if not access.proved:
        gen.check_pointer(result)
    gen.refine(vop.results[0].tn, result)


@define_vop("catch-exception", translate=fndb.CATCH_EXCEPTION, results=(OBJECT,))
def catch_exception(gen, vop):
    gen.pointer_result(vop.results[0], "call ptr @mule_catch_exception()")


@define_vop("enter-handler", translate=fndb.ENTER_HANDLER, args=(OBJECT,))
def enter_handler(gen, vop):
    value = gen.take(vop.args[0])
    status = gen.operation(f"call i32 @mule_enter_handler(ptr {value})")
    gen.release(value)
    gen.check_status(status)


@define_vop("leave-handler", translate=fndb.LEAVE_HANDLER)
def leave_handler(gen, vop):
    gen.line("call void @mule_leave_handler()")


@define_vop("assertion-failure", translate=fndb.ASSERTION_FAILURE, args=(OBJECT,))
def assertion_failure(gen, vop):
    if vop.codegen_info is False:
        unused = gen.take(vop.args[0])
        gen.release(unused)
        status = gen.operation("call i32 @mule_assertion_failure(ptr null)")
        gen.check_status(status)
        return
    message = gen.take(vop.args[0])
    status = gen.operation(f"call i32 @mule_assertion_failure(ptr {message})")
    gen.release(message)
    gen.check_status(status)


@define_vop("raise", translate=fndb.RAISE, args=(OBJECT,))
def raise_(gen, vop):
    value = gen.take(vop.args[0])
    result = gen.sequence_call("raise_value", [value])
    if not gen.module.program.bootstrap and gen.lambda_.policy.records_traceback:
        gen.check_pointer(result)
    gen.propagate(explicit_raise=True)


@define_vop("resume-exception", translate=fndb.RESUME_EXCEPTION, args=(OBJECT,))
def resume_exception(gen, vop):
    value = gen.take(vop.args[0])
    gen.line(f"call ptr @mule_raise_pending(ptr {value})")
    gen.release(value)
    gen.propagate(record_traceback=False)


@define_vop("reraise", translate=fndb.RERAISE)
def reraise(gen, vop):
    gen.line("call i32 @mule_reraise()")
    gen.propagate()


@define_vop("unwind", translate=fndb.UNWIND)
def unwind(gen, vop):
    gen.propagate()


# Compiled-protocol primitives: the physical operations the compiled stdlib
# modules call by underscored name.


def primitive(name, info, emit, *, consumes_arguments=False):
    """Register ``emit(gen, vop, values)`` as the template of ``info``: the
    operands leave their TNs and the result is published before error checking.
    Borrowed physical primitives need caller release; compiled calls consume
    their arguments on every arm."""

    @define_vop(f"primitive-{name}", translate=info, results=(OBJECT,))
    def generator(gen, vop):
        values = [gen.take(value) for value in vop.args]
        result = emit(gen, vop, values)
        if vop.results and vop.results[0].tn.sc is SIGNED:
            gen.store(vop.results[0], result)
            if not consumes_arguments:
                for arg, value in zip(vop.args, values):
                    if arg.tn.sc in OBJECT:
                        gen.release(value)
            if result not in gen.nonnull_values and not result[0].isdigit():
                gen.guard(gen.operation(f"icmp slt i64 {result}, 0"))
            return
        known_nonnull = result.startswith("@mule.") or result in gen.immortal_values
        gen.publish(vop.results[0], result)
        if not consumes_arguments:
            for ref, value in zip(vop.args, values, strict=True):
                if ref.tn.sc is not SIGNED:
                    gen.release(value)
        if not known_nonnull:
            gen.check_pointer(result)
        gen.refine(vop.results[0].tn, result)

    return generator


def text_access(function):
    storage_helper = {
        "mule_storage_read": "@mule.fast.storage.read",
        "mule_storage_write": "@mule.fast.storage.write",
        "mule_storage_length": "@mule.fast.storage.length",
    }.get(function)

    def emit(gen, vop, values):
        arg_type = (vop.args[0].tn.type or UNKNOWN) if vop.args else UNKNOWN
        if storage_helper is not None:
            if function == "mule_storage_length" and (
                arg_type.is_exact_list or arg_type.is_exact_tuple
            ):
                exact_len = arg_type.exact_length()
                if exact_len is not None:
                    count = str(exact_len)
                else:
                    len_addr = gen.operation(
                        f"getelementptr i64, ptr {values[0]}, i64 {VALUE_BUFFER_LENGTH_OFFSET // 8}"
                    )
                    count = gen.operation(f"load i64, ptr {len_addr}")
                gen.nonnull_values.add(count)
                if vop.results and vop.results[0].tn.sc is SIGNED:
                    return count
                encoded = gen.encode(count)
                gen.immortal_values.add(encoded)
                return encoded
            if (
                function == "mule_storage_read"
                and (arg_type.is_exact_list or arg_type.is_exact_tuple)
                and len(vop.args) == 2
                and (vop.args[1].tn.type or UNKNOWN).is_fixnum
                and vop.results
            ):
                sequence, index = values[0], values[1]
                raw_index_sc = vop.args[1].tn.sc is SIGNED
                idx_type = vop.args[1].tn.type or UNKNOWN
                non_negative = idx_type.minimum is not None and idx_type.minimum >= 0
                raw = index if raw_index_sc else gen.decode(index)
                res_tn = vop.results[0].tn
                res_type = res_tn.type or UNKNOWN
                unchecked_contract = gen._has_unchecked_element_contract(res_tn)
                immortal_elem = not unchecked_contract and (
                    not res_tn.sc.owned or res_type.is_fixnum or res_type.is_immortal
                )
                if vop.node in gen.context.in_bounds_nodes and non_negative:
                    data_addr = gen.operation(
                        f"getelementptr ptr, ptr {sequence}, i64 {VALUE_BUFFER_DATA_OFFSET // 8}"
                    )
                    data = gen.operation(f"load ptr, ptr {data_addr}")
                    slot = gen.operation(f"getelementptr ptr, ptr {data}, i64 {raw}")
                    fast_val = gen.operation(f"load ptr, ptr {slot}, !nonnull !{{}}")
                    if vop.codegen_info != "storage_borrow" and not immortal_elem:
                        if not unchecked_contract and res_type.is_heap_object:
                            gen.heap_values.add(fast_val)
                        fast_val = gen.retain(fast_val)
                    gen.nonnull_values.add(fast_val)
                    return fast_val
                exact_len = arg_type.exact_length()
                if exact_len is not None:
                    length = str(exact_len)
                else:
                    len_addr = gen.operation(
                        f"getelementptr i64, ptr {sequence}, i64 {VALUE_BUFFER_LENGTH_OFFSET // 8}"
                    )
                    length = gen.operation(f"load i64, ptr {len_addr}")
                if non_negative:
                    pos = raw
                else:
                    neg = gen.operation(f"icmp slt i64 {raw}, 0")
                    wrapped = gen.operation(f"add i64 {raw}, {length}")
                    pos = gen.operation(f"select i1 {neg}, i64 {wrapped}, i64 {raw}")
                inside = gen.operation(f"icmp ult i64 {pos}, {length}")
                read_lbl = gen.fresh("storage.read")[1:]
                slow_lbl = gen.fresh("storage.slow")[1:]
                done_lbl = gen.fresh("storage.done")[1:]
                gen.line(
                    f"br i1 {inside}, label %{read_lbl}, label %{slow_lbl}, "
                    f'!prof !{{!"branch_weights", i32 2000, i32 1}}'
                )
                gen.lines.append(read_lbl + ":")
                data_addr = gen.operation(
                    f"getelementptr ptr, ptr {sequence}, i64 {VALUE_BUFFER_DATA_OFFSET // 8}"
                )
                data = gen.operation(f"load ptr, ptr {data_addr}")
                slot = gen.operation(f"getelementptr ptr, ptr {data}, i64 {pos}")
                fast_val = gen.operation(f"load ptr, ptr {slot}, !nonnull !{{}}")
                if vop.codegen_info != "storage_borrow" and not immortal_elem:
                    if not unchecked_contract and res_type.is_heap_object:
                        gen.heap_values.add(fast_val)
                    fast_val = gen.retain(fast_val)
                read_end = gen.current_block()
                gen.line(f"br label %{done_lbl}")
                gen.lines.append(slow_lbl + ":")
                boxed_index = gen.encode(index) if raw_index_sc else index
                slow_val = gen.operation(
                    f"call ptr @mule_storage_read(ptr {sequence}, ptr {boxed_index})"
                )
                slow_end = gen.current_block()
                gen.line(f"br label %{done_lbl}")
                gen.lines.append(done_lbl + ":")
                return gen.operation(
                    f"phi ptr [{fast_val}, %{read_end}], [{slow_val}, %{slow_end}]"
                )
            if (
                function == "mule_storage_write"
                and arg_type.is_exact_list
                and len(vop.args) == 3
                and (vop.args[1].tn.type or UNKNOWN).is_fixnum
            ):
                sequence, index, replacement = values[0], values[1], values[2]
                if vop.args[2].tn.sc is SIGNED:
                    replacement = gen.encode(replacement)
                    gen.immortal_values.add(replacement)
                raw_index_sc = vop.args[1].tn.sc is SIGNED
                idx_type = vop.args[1].tn.type or UNKNOWN
                val_type = vop.args[2].tn.type or UNKNOWN
                elem_type = arg_type.iterate_type().next_item_type()
                non_negative = idx_type.minimum is not None and idx_type.minimum >= 0
                raw = index if raw_index_sc else gen.decode(index)
                if vop.node in gen.context.in_bounds_nodes and non_negative:
                    data_addr = gen.operation(
                        f"getelementptr ptr, ptr {sequence}, i64 {VALUE_BUFFER_DATA_OFFSET // 8}"
                    )
                    data = gen.operation(f"load ptr, ptr {data_addr}")
                    slot = gen.operation(f"getelementptr ptr, ptr {data}, i64 {raw}")
                    old_val = gen.operation(f"load ptr, ptr {slot}, !nonnull !{{}}")
                    if val_type.is_fixnum or val_type.is_immortal:
                        owned_val = replacement
                    else:
                        if val_type.is_heap_object:
                            gen.heap_values.add(replacement)
                        owned_val = gen.retain(replacement)
                    gen.line(f"store ptr {owned_val}, ptr {slot}")
                    if not (
                        val_type.is_fixnum
                        or val_type.is_immortal
                        or val_type.fits_text(val_type)
                    ):
                        gen.line(
                            f"call void @mule_storage_track(ptr {sequence}, ptr {replacement})"
                        )
                    if not (elem_type.is_fixnum or elem_type.is_immortal):
                        gen.release(old_val)
                    return "@mule.none"
                len_addr = gen.operation(
                    f"getelementptr i64, ptr {sequence}, i64 {VALUE_BUFFER_LENGTH_OFFSET // 8}"
                )
                length = gen.operation(f"load i64, ptr {len_addr}")
                if non_negative:
                    pos = raw
                else:
                    neg = gen.operation(f"icmp slt i64 {raw}, 0")
                    wrapped = gen.operation(f"add i64 {raw}, {length}")
                    pos = gen.operation(f"select i1 {neg}, i64 {wrapped}, i64 {raw}")
                inside = gen.operation(f"icmp ult i64 {pos}, {length}")
                write_lbl = gen.fresh("storage.write")[1:]
                slow_lbl = gen.fresh("storage.slow")[1:]
                done_lbl = gen.fresh("storage.done")[1:]
                gen.line(
                    f"br i1 {inside}, label %{write_lbl}, label %{slow_lbl}, "
                    f'!prof !{{!"branch_weights", i32 2000, i32 1}}'
                )
                gen.lines.append(write_lbl + ":")
                data_addr = gen.operation(
                    f"getelementptr ptr, ptr {sequence}, i64 {VALUE_BUFFER_DATA_OFFSET // 8}"
                )
                data = gen.operation(f"load ptr, ptr {data_addr}")
                slot = gen.operation(f"getelementptr ptr, ptr {data}, i64 {pos}")
                old_val = gen.operation(f"load ptr, ptr {slot}, !nonnull !{{}}")
                if val_type.is_fixnum or val_type.is_immortal:
                    owned_val = replacement
                else:
                    if val_type.is_heap_object:
                        gen.heap_values.add(replacement)
                    owned_val = gen.retain(replacement)
                gen.line(f"store ptr {owned_val}, ptr {slot}")
                if not (
                    val_type.is_fixnum
                    or val_type.is_immortal
                    or val_type.fits_text(val_type)
                ):
                    gen.line(
                        f"call void @mule_storage_track(ptr {sequence}, ptr {replacement})"
                    )
                if not (elem_type.is_fixnum or elem_type.is_immortal):
                    gen.release(old_val)
                write_end = gen.current_block()
                gen.line(f"br label %{done_lbl}")
                gen.lines.append(slow_lbl + ":")
                boxed_index = gen.encode(index) if raw_index_sc else index
                slow_val = gen.operation(
                    f"call ptr @mule_storage_write(ptr {sequence}, ptr {boxed_index}, ptr {replacement})"
                )
                slow_end = gen.current_block()
                gen.line(f"br label %{done_lbl}")
                gen.lines.append(done_lbl + ":")
                return gen.operation(
                    f"phi ptr [@mule.none, %{write_end}], [{slow_val}, %{slow_end}]"
                )
            helper = storage_helper
            if function == "mule_storage_read" and vop.results:
                res_tn = vop.results[0].tn
                res_type = res_tn.type or UNKNOWN
                immortal_elem = not gen._has_unchecked_element_contract(res_tn) and (
                    not res_tn.sc.owned or res_type.is_fixnum or res_type.is_immortal
                )
                if vop.codegen_info == "storage_borrow" or immortal_elem:
                    helper = "@mule.fast.storage.read.borrow"
            operands = ", ".join(f"ptr {value}" for value in values)
            return gen.operation(f"call ptr {helper}({operands})")
        if function == "mule_integer_storage":
            if arg_type.is_fixnum:
                return values[0]
            return gen.operation(
                f"call ptr @mule.fast.integer_storage(ptr {values[0]})"
            )
        if function == "mule_text_storage":
            if arg_type.fits_text(arg_type) and not arg_type.may_be_unbound:
                return "@mule.true"
            bits = gen.operation(f"ptrtoint ptr {values[0]} to i64")
            tag = gen.operation(f"and i64 {bits}, 1")
            is_heap = gen.operation(f"icmp eq i64 {tag}, 0")
            heap_lbl = gen.fresh("text.storage.heap")[1:]
            done_lbl = gen.fresh("text.storage.done")[1:]
            start_blk = gen.current_block()
            gen.line(f"br i1 {is_heap}, label %{heap_lbl}, label %{done_lbl}")
            gen.lines.append(heap_lbl + ":")
            kind_addr = gen.operation(
                f"getelementptr inbounds i8, ptr {values[0]}, i64 {DATA_OFFSET}"
            )
            data_kind = gen.operation(
                f"load i32, ptr {kind_addr}, !invariant.load !{{}}"
            )
            is_text = gen.operation(f"icmp eq i32 {data_kind}, {TEXT_TAG}")
            heap_res = gen.operation(
                f"select i1 {is_text}, ptr @mule.true, ptr @mule.false"
            )
            heap_blk = gen.current_block()
            gen.line(f"br label %{done_lbl}")
            gen.lines.append(done_lbl + ":")
            res = gen.operation(
                f"phi ptr [@mule.false, %{start_blk}], [{heap_res}, %{heap_blk}]"
            )
            gen.immortal_values.add(res)
            return res
        if function == "mule_text_length" and len(vop.args) == 1:
            seq = values[0]
            signed_res = bool(vop.results and vop.results[0].tn.sc is SIGNED)
            if arg_type.fits_text(arg_type) and not arg_type.may_be_unbound:
                exact_len = arg_type.exact_length()
                if exact_len is not None:
                    count = str(exact_len)
                else:
                    repr_addr = gen.operation(
                        f"getelementptr inbounds i8, ptr {seq}, i64 {TEXT_KIND_TAG_OFFSET}"
                    )
                    repr_val = gen.operation(
                        f"load i8, ptr {repr_addr}, !invariant.load !{{}}"
                    )
                    inline_len_addr = gen.operation(
                        f"getelementptr inbounds i8, ptr {seq}, i64 {TEXT_INLINE_LEN_OFFSET}"
                    )
                    inline_len8 = gen.operation(
                        f"load i8, ptr {inline_len_addr}, !invariant.load !{{}}"
                    )
                    inline_len = gen.operation(f"zext i8 {inline_len8} to i64")
                    heap_len_addr = gen.operation(
                        f"getelementptr inbounds i8, ptr {seq}, i64 {TEXT_HEAP_LEN_OFFSET}"
                    )
                    heap_len = gen.operation(
                        f"load i64, ptr {heap_len_addr}, !invariant.load !{{}}"
                    )
                    is_inline = gen.operation(f"icmp eq i8 {repr_val}, 0")
                    count = gen.operation(
                        f"select i1 {is_inline}, i64 {inline_len}, i64 {heap_len}"
                    )
                gen.nonnull_values.add(count)
                if signed_res:
                    return count
                encoded = gen.encode(count)
                gen.immortal_values.add(encoded)
                return encoded
            bits = gen.operation(f"ptrtoint ptr {seq} to i64")
            tag = gen.operation(f"and i64 {bits}, 1")
            is_heap = gen.operation(f"icmp eq i64 {tag}, 0")
            heap_lbl = gen.fresh("text.len.heap")[1:]
            read_lbl = gen.fresh("text.len.read")[1:]
            slow_lbl = gen.fresh("text.len.slow")[1:]
            done_lbl = gen.fresh("text.len.done")[1:]
            gen.line(
                f"br i1 {is_heap}, label %{heap_lbl}, label %{slow_lbl}, "
                f'!prof !{{!"branch_weights", i32 2000, i32 1}}'
            )
            gen.lines.append(heap_lbl + ":")
            kind_addr = gen.operation(
                f"getelementptr inbounds i8, ptr {seq}, i64 {DATA_OFFSET}"
            )
            data_kind = gen.operation(
                f"load i32, ptr {kind_addr}, !invariant.load !{{}}"
            )
            is_text = gen.operation(f"icmp eq i32 {data_kind}, {TEXT_TAG}")
            gen.line(
                f"br i1 {is_text}, label %{read_lbl}, label %{slow_lbl}, "
                f'!prof !{{!"branch_weights", i32 2000, i32 1}}'
            )
            gen.lines.append(read_lbl + ":")
            repr_addr = gen.operation(
                f"getelementptr inbounds i8, ptr {seq}, i64 {TEXT_KIND_TAG_OFFSET}"
            )
            repr_val = gen.operation(f"load i8, ptr {repr_addr}, !invariant.load !{{}}")
            inline_len_addr = gen.operation(
                f"getelementptr inbounds i8, ptr {seq}, i64 {TEXT_INLINE_LEN_OFFSET}"
            )
            inline_len8 = gen.operation(
                f"load i8, ptr {inline_len_addr}, !invariant.load !{{}}"
            )
            inline_len = gen.operation(f"zext i8 {inline_len8} to i64")
            heap_len_addr = gen.operation(
                f"getelementptr inbounds i8, ptr {seq}, i64 {TEXT_HEAP_LEN_OFFSET}"
            )
            heap_len = gen.operation(
                f"load i64, ptr {heap_len_addr}, !invariant.load !{{}}"
            )
            is_inline = gen.operation(f"icmp eq i8 {repr_val}, 0")
            fast_len = gen.operation(
                f"select i1 {is_inline}, i64 {inline_len}, i64 {heap_len}"
            )
            fast_out = fast_len if signed_res else gen.encode(fast_len)
            read_end = gen.current_block()
            gen.line(f"br label %{done_lbl}")
            gen.lines.append(slow_lbl + ":")
            slow_ptr = gen.operation(f"call ptr @mule_text_length(ptr {seq})")
            if signed_res:
                slow_null = gen.operation(f"icmp eq ptr {slow_ptr}, null")
                slow_decoded = gen.decode(slow_ptr)
                slow_out = gen.operation(
                    f"select i1 {slow_null}, i64 -1, i64 {slow_decoded}"
                )
            else:
                slow_out = slow_ptr
            slow_end = gen.current_block()
            gen.line(f"br label %{done_lbl}")
            gen.lines.append(done_lbl + ":")
            phi_ty = "i64" if signed_res else "ptr"
            return gen.operation(
                f"phi {phi_ty} [{fast_out}, %{read_end}], [{slow_out}, %{slow_end}]"
            )
        if function == "mule_text_codepoint" and len(vop.args) == 2:
            index_type = vop.args[1].tn.type or UNKNOWN
            if index_type.is_fixnum:
                sequence = values[0]
                raw = (
                    values[1] if vop.args[1].tn.sc is SIGNED else gen.decode(values[1])
                )
                signed_res = bool(vop.results and vop.results[0].tn.sc is SIGNED)
                proven_text = (
                    arg_type.fits_text(arg_type) and not arg_type.may_be_unbound
                )
                non_negative = (
                    index_type.minimum is not None and index_type.minimum >= 0
                )
                if (
                    proven_text
                    and non_negative
                    and vop.node in gen.context.in_bounds_nodes
                ):
                    res_type = (
                        (vop.results[0].tn.type or UNKNOWN) if vop.results else UNKNOWN
                    )
                    exact_cp = res_type.exact_integer()
                    if exact_cp is not None:
                        code = str(exact_cp)
                        gen.nonnull_values.add(code)
                        if signed_res:
                            return code
                        encoded = gen.encode(code)
                        gen.immortal_values.add(encoded)
                        return encoded
                    repr_addr = gen.operation(
                        f"getelementptr inbounds i8, ptr {sequence}, i64 {TEXT_KIND_TAG_OFFSET}"
                    )
                    repr_val = gen.operation(
                        f"load i8, ptr {repr_addr}, !invariant.load !{{}}"
                    )
                    inline_base = gen.operation(
                        f"getelementptr inbounds i8, ptr {sequence}, i64 {TEXT_INLINE_WORDS_OFFSET}"
                    )
                    heap_base_addr = gen.operation(
                        f"getelementptr inbounds i8, ptr {sequence}, i64 {TEXT_HEAP_DATA_OFFSET}"
                    )
                    heap_base = gen.operation(
                        f"load ptr, ptr {heap_base_addr}, !invariant.load !{{}}"
                    )
                    is_inline = gen.operation(f"icmp eq i8 {repr_val}, 0")
                    base = gen.operation(
                        f"select i1 {is_inline}, ptr {inline_base}, ptr {heap_base}"
                    )
                    is_latin1 = gen.operation(f"icmp ult i8 {repr_val}, 2")
                    latin1_lbl = gen.fresh("text.latin1")[1:]
                    wide_lbl = gen.fresh("text.wide")[1:]
                    ucs2_lbl = gen.fresh("text.ucs2")[1:]
                    ucs4_lbl = gen.fresh("text.ucs4")[1:]
                    done_lbl = gen.fresh("text.done")[1:]
                    gen.line(
                        f"br i1 {is_latin1}, label %{latin1_lbl}, label %{wide_lbl}, "
                        f'!prof !{{!"branch_weights", i32 2000, i32 1}}'
                    )
                    gen.lines.append(latin1_lbl + ":")
                    slot = gen.operation(
                        f"getelementptr inbounds i8, ptr {base}, i64 {raw}"
                    )
                    byte = gen.operation(f"load i8, ptr {slot}, !invariant.load !{{}}")
                    latin1_code = gen.operation(f"zext i8 {byte} to i64")
                    latin1_end = gen.current_block()
                    gen.line(f"br label %{done_lbl}")
                    gen.lines.append(wide_lbl + ":")
                    is_ucs2 = gen.operation(f"icmp eq i8 {repr_val}, 2")
                    gen.line(f"br i1 {is_ucs2}, label %{ucs2_lbl}, label %{ucs4_lbl}")
                    gen.lines.append(ucs2_lbl + ":")
                    u16_slot = gen.operation(
                        f"getelementptr inbounds i16, ptr {heap_base}, i64 {raw}"
                    )
                    u16_val = gen.operation(
                        f"load i16, ptr {u16_slot}, !invariant.load !{{}}"
                    )
                    u16_code = gen.operation(f"zext i16 {u16_val} to i64")
                    ucs2_end = gen.current_block()
                    gen.line(f"br label %{done_lbl}")
                    gen.lines.append(ucs4_lbl + ":")
                    u32_slot = gen.operation(
                        f"getelementptr inbounds i32, ptr {heap_base}, i64 {raw}"
                    )
                    u32_val = gen.operation(
                        f"load i32, ptr {u32_slot}, !invariant.load !{{}}"
                    )
                    u32_code = gen.operation(f"zext i32 {u32_val} to i64")
                    ucs4_end = gen.current_block()
                    gen.line(f"br label %{done_lbl}")
                    gen.lines.append(done_lbl + ":")
                    code = gen.operation(
                        f"phi i64 [{latin1_code}, %{latin1_end}], [{u16_code}, %{ucs2_end}], [{u32_code}, %{ucs4_end}]"
                    )
                    gen.nonnull_values.add(code)
                    if signed_res:
                        return code
                    encoded = gen.encode(code)
                    gen.immortal_values.add(encoded)
                    return encoded
                read_lbl = gen.fresh("text.read")[1:]
                latin1_lbl = gen.fresh("text.latin1")[1:]
                wide_lbl = gen.fresh("text.wide")[1:]
                ucs2_lbl = gen.fresh("text.ucs2")[1:]
                ucs4_lbl = gen.fresh("text.ucs4")[1:]
                slow_lbl = gen.fresh("text.slow")[1:]
                done_lbl = gen.fresh("text.done")[1:]
                if not proven_text:
                    bits = gen.operation(f"ptrtoint ptr {sequence} to i64")
                    tag = gen.operation(f"and i64 {bits}, 1")
                    is_heap = gen.operation(f"icmp eq i64 {tag}, 0")
                    heap_lbl = gen.fresh("text.heap")[1:]
                    gen.line(
                        f"br i1 {is_heap}, label %{heap_lbl}, label %{slow_lbl}, "
                        f'!prof !{{!"branch_weights", i32 2000, i32 1}}'
                    )
                    gen.lines.append(heap_lbl + ":")
                    kind_addr = gen.operation(
                        f"getelementptr inbounds i8, ptr {sequence}, i64 {DATA_OFFSET}"
                    )
                    data_kind = gen.operation(
                        f"load i32, ptr {kind_addr}, !invariant.load !{{}}"
                    )
                    is_text = gen.operation(f"icmp eq i32 {data_kind}, {TEXT_TAG}")
                    text_lbl = gen.fresh("text.kind")[1:]
                    gen.line(
                        f"br i1 {is_text}, label %{text_lbl}, label %{slow_lbl}, "
                        f'!prof !{{!"branch_weights", i32 2000, i32 1}}'
                    )
                    gen.lines.append(text_lbl + ":")
                repr_addr = gen.operation(
                    f"getelementptr inbounds i8, ptr {sequence}, i64 {TEXT_KIND_TAG_OFFSET}"
                )
                repr_val = gen.operation(
                    f"load i8, ptr {repr_addr}, !invariant.load !{{}}"
                )
                is_inline = gen.operation(f"icmp eq i8 {repr_val}, 0")
                inline_len_addr = gen.operation(
                    f"getelementptr inbounds i8, ptr {sequence}, i64 {TEXT_INLINE_LEN_OFFSET}"
                )
                inline_len8 = gen.operation(
                    f"load i8, ptr {inline_len_addr}, !invariant.load !{{}}"
                )
                inline_len = gen.operation(f"zext i8 {inline_len8} to i64")
                heap_len_addr = gen.operation(
                    f"getelementptr inbounds i8, ptr {sequence}, i64 {TEXT_HEAP_LEN_OFFSET}"
                )
                heap_len = gen.operation(
                    f"load i64, ptr {heap_len_addr}, !invariant.load !{{}}"
                )
                length = gen.operation(
                    f"select i1 {is_inline}, i64 {inline_len}, i64 {heap_len}"
                )
                inside = gen.operation(f"icmp ult i64 {raw}, {length}")
                gen.line(
                    f"br i1 {inside}, label %{read_lbl}, label %{slow_lbl}, "
                    f'!prof !{{!"branch_weights", i32 2000, i32 1}}'
                )
                gen.lines.append(read_lbl + ":")
                inline_base = gen.operation(
                    f"getelementptr inbounds i8, ptr {sequence}, i64 {TEXT_INLINE_WORDS_OFFSET}"
                )
                heap_base_addr = gen.operation(
                    f"getelementptr inbounds i8, ptr {sequence}, i64 {TEXT_HEAP_DATA_OFFSET}"
                )
                heap_base = gen.operation(
                    f"load ptr, ptr {heap_base_addr}, !invariant.load !{{}}"
                )
                base = gen.operation(
                    f"select i1 {is_inline}, ptr {inline_base}, ptr {heap_base}"
                )
                is_latin1 = gen.operation(f"icmp ult i8 {repr_val}, 2")
                gen.line(
                    f"br i1 {is_latin1}, label %{latin1_lbl}, label %{wide_lbl}, "
                    f'!prof !{{!"branch_weights", i32 2000, i32 1}}'
                )
                gen.lines.append(latin1_lbl + ":")
                slot = gen.operation(
                    f"getelementptr inbounds i8, ptr {base}, i64 {raw}"
                )
                byte = gen.operation(f"load i8, ptr {slot}, !invariant.load !{{}}")
                latin1_code = gen.operation(f"zext i8 {byte} to i64")
                latin1_out = latin1_code if signed_res else gen.encode(latin1_code)
                latin1_end = gen.current_block()
                gen.line(f"br label %{done_lbl}")
                gen.lines.append(wide_lbl + ":")
                is_ucs2 = gen.operation(f"icmp eq i8 {repr_val}, 2")
                gen.line(f"br i1 {is_ucs2}, label %{ucs2_lbl}, label %{ucs4_lbl}")
                gen.lines.append(ucs2_lbl + ":")
                u16_slot = gen.operation(
                    f"getelementptr inbounds i16, ptr {heap_base}, i64 {raw}"
                )
                u16_val = gen.operation(
                    f"load i16, ptr {u16_slot}, !invariant.load !{{}}"
                )
                u16_code = gen.operation(f"zext i16 {u16_val} to i64")
                u16_out = u16_code if signed_res else gen.encode(u16_code)
                ucs2_end = gen.current_block()
                gen.line(f"br label %{done_lbl}")
                gen.lines.append(ucs4_lbl + ":")
                u32_slot = gen.operation(
                    f"getelementptr inbounds i32, ptr {heap_base}, i64 {raw}"
                )
                u32_val = gen.operation(
                    f"load i32, ptr {u32_slot}, !invariant.load !{{}}"
                )
                u32_code = gen.operation(f"zext i32 {u32_val} to i64")
                u32_out = u32_code if signed_res else gen.encode(u32_code)
                ucs4_end = gen.current_block()
                gen.line(f"br label %{done_lbl}")
                gen.lines.append(slow_lbl + ":")
                slow_ptr = gen.operation(
                    f"call ptr @mule_text_codepoint_at(ptr {sequence}, i64 {raw})"
                )
                if signed_res:
                    slow_null = gen.operation(f"icmp eq ptr {slow_ptr}, null")
                    slow_decoded = gen.decode(slow_ptr)
                    slow_out = gen.operation(
                        f"select i1 {slow_null}, i64 -1, i64 {slow_decoded}"
                    )
                else:
                    slow_out = slow_ptr
                slow_end = gen.current_block()
                gen.line(f"br label %{done_lbl}")
                gen.lines.append(done_lbl + ":")
                phi_ty = "i64" if signed_res else "ptr"
                return gen.operation(
                    f"phi {phi_ty} [{latin1_out}, %{latin1_end}], [{u16_out}, %{ucs2_end}], [{u32_out}, %{ucs4_end}], [{slow_out}, %{slow_end}]"
                )
            return gen.operation(
                f"call ptr @mule_text_codepoint_getitem(ptr {values[0]}, ptr {values[1]})"
            )
        if function == "mule_read_byte" and len(vop.args) == 2:
            index_type = vop.args[1].tn.type or UNKNOWN
            if index_type.is_fixnum:
                index = (
                    values[1] if vop.args[1].tn.sc is SIGNED else gen.decode(values[1])
                )
                return gen.operation(
                    f"call ptr @mule_read_byte_at(ptr {values[0]}, i64 {index})"
                )
        operands = ", ".join(f"ptr {value}" for value in values)
        return gen.operation(f"call ptr @{function}({operands})")

    return emit


def lookup_method(name, *, include_object_defaults=False):
    entry = (
        "mule_lookup_special_including_defaults"
        if include_object_defaults
        else "mule_lookup_special"
    )

    def emit(gen, vop, values):
        if name == "__index__":
            if (vop.args[0].tn.type or UNKNOWN).is_fixnum:
                return "@mule.none"
            return gen.operation(
                f"call ptr @mule.fast.index_method(ptr {values[0]}, ptr {gen.module.text(name)})"
            )
        return gen.operation(
            f"call ptr @{entry}(ptr {values[0]}, ptr {gen.module.text(name)})"
        )

    return emit


def render_text(representation):
    def emit(gen, vop, values):
        return gen.operation(
            f"call ptr @mule_render_text(ptr {values[0]}, i32 {int(representation)})"
        )

    return emit


def text_result(gen, vop, values):
    return gen.operation(f"call ptr @mule_text_result(ptr {values[0]})")


def compiled_call(name):
    def emit(gen, vop, values):
        return gen.compiled_call_named(name, values)

    return emit


def numeric_modulo(gen, vop, values):
    return gen.operation(
        f"call ptr @mule.fast.binary.5(ptr {values[0]}, ptr {values[1]}, i32 0, ptr null, ptr null)"
    )


def allocate(gen, vop, values):
    return gen.operation(f"call ptr @mule_native_new(ptr {values[0]})")


def initialize(gen, vop, values):
    layout = vop.codegen_info
    return gen.compiled_call(layout.methods["__init__"], values)


def invalid_initializer(gen, vop, values):
    gen.line("call i32 @mule_native_initializer_error()")
    return "null"


def read_int64(gen, vop, values):
    payload = gen.operation(f"call ptr @mule_native_payload(ptr {values[0]})")
    gen.check_pointer(payload)
    index = gen.decode(values[1])
    address = gen.operation(f"getelementptr i64, ptr {payload}, i64 {index}")
    value = gen.operation(f"load i64, ptr {address}")
    return gen.operation(f"call ptr @{INT64_BOX_LAYOUT.symbol()}(i64 {value})")


def write_int64(gen, vop, values):
    payload = gen.operation(f"call ptr @mule_native_payload(ptr {values[0]})")
    gen.check_pointer(payload)
    fact = vop.args[2].tn.type or UNKNOWN
    if fact.is_fixnum or vop.args[2].tn.primitive_type is BOXED_I64:
        value = gen.operation(
            f"call i64 @{INT64_BOX_LAYOUT.symbol('load')}(ptr {values[2]})"
        )
    else:
        slot = gen.operation("alloca i64")
        status = gen.operation(f"call i32 @mule_unbox_i64(ptr {values[2]}, ptr {slot})")
        gen.check_status(status)
        value = gen.operation(f"load i64, ptr {slot}")
    index = gen.decode(values[1])
    address = gen.operation(f"getelementptr i64, ptr {payload}, i64 {index}")
    gen.line(f"store i64 {value}, ptr {address}")
    return "@mule.none"


INSERT_SAFEPOINT = insert_safepoint
GETATTR_BORROW = getattr_borrow
SLOT_BORROW_END = slot_borrow_end
PHYSICAL_FIELD_READ = physical_field_read_fixed
PHYSICAL_FIELD_WRITE = physical_field_write_fixed
PHYSICAL_FIELD_READ_BORROW = physical_field_read_borrow
PHYSICAL_FIELD_BORROW_END = physical_field_borrow_end
PHYSICAL_FIELD_READ_NAME = physical_field_read_name
PHYSICAL_FIELD_WRITE_NAME = physical_field_write_name
PHYSICAL_FIELD_READ_GUARDED = physical_field_read_guarded
PHYSICAL_FIELD_WRITE_GUARDED = physical_field_write_guarded
LOAD_LOCAL = load_local
LOAD_GLOBAL = load_global
MOVE = move
COERCE = coerce
MOVE_ARGUMENT = move_argument
LOAD_CLOSURE = load_closure
LOCAL_ADDRESS = local_address
LOCAL_LOAD_CLOSURE = local_load_closure
LOCAL_CELL_REF = local_cell_ref
LOCAL_CELL_SET = local_cell_set
LOCAL_CELL_CLEAR = local_cell_clear
LOCAL_FUNCTION = local_function
LOCAL_EXTENT_CALL = local_extent_call
VALUE_CELL_REF = value_cell_ref
VALUE_CELL_SET = value_cell_set
VALUE_CELL_CLEAR = value_cell_clear
UNBIND_LOCAL = unbind_local
STORE_GLOBAL = store_global
UNBIND_GLOBAL = unbind_global
DISCARD = discard
TYPE_CHECK = type_check
TEXT_SLICE_COMPARE = text_slice_compare
TEXT_SLICE2_COMPARE = text_slice2_compare
BRANCH = branch
JUMP = jump
RETURN = return_
FULL_CALL = full_call
CALL_RESOLVED = call_resolved
LOCAL_CALL = local_call
FAST_LEN = fast_len
PARSE_INT_TEXT = parse_int_text
PARSE_INT_SLICE = parse_int_slice
RAW_CONSTRUCT = raw_construct


__all__ = [
    "ARGS_TAG",
    "ATTR_CACHE_WORDS",
    "BRANCH",
    "CALL_RESOLVED",
    "COERCE",
    "COLD_BRANCH",
    "CONSTANT",
    "DATA_OFFSET",
    "DISCARD",
    "FAST_LEN",
    "FULL_CALL",
    "GETATTR_BORROW",
    "HOT_BRANCH",
    "INSERT_SAFEPOINT",
    "INSTANCE_FIELDS_OFFSET",
    "INTRINSICS",
    "JUMP",
    "LOAD_CLOSURE",
    "LOAD_GLOBAL",
    "LOAD_LOCAL",
    "LOCAL_ADDRESS",
    "LOCAL_CALL",
    "LOCAL_CELL_CLEAR",
    "LOCAL_CELL_REF",
    "LOCAL_CELL_SET",
    "LOCAL_EXTENT_CALL",
    "LOCAL_FUNCTION",
    "LOCAL_LOAD_CLOSURE",
    "MOVE",
    "MOVE_ARGUMENT",
    "OBJECT",
    "PHYSICAL_FIELD_BORROW_END",
    "PHYSICAL_FIELD_READ",
    "PHYSICAL_FIELD_READ_BORROW",
    "PHYSICAL_FIELD_READ_GUARDED",
    "PHYSICAL_FIELD_READ_NAME",
    "PHYSICAL_FIELD_WRITE",
    "PHYSICAL_FIELD_WRITE_GUARDED",
    "PHYSICAL_FIELD_WRITE_NAME",
    "RAW_CONSTRUCT",
    "RETURN",
    "SLOT_BORROW_END",
    "STORE_GLOBAL",
    "TERMINATORS",
    "TYPE_CHECK",
    "UNBIND_GLOBAL",
    "UNBIND_LOCAL",
    "VALUE_BUFFER_DATA_OFFSET",
    "VALUE_BUFFER_LENGTH_OFFSET",
    "VALUE_CELL_CLEAR",
    "VALUE_CELL_REF",
    "VALUE_CELL_SET",
    "BorrowedLiteralEmitter",
    "LiteralEmitter",
    "_ContractAssertion",
    "_ReturnValueKind",
    "_binary_generic",
    "_ownership",
    "allocate",
    "assert_unpacked_return",
    "checks_unpacked_return",
    "compiled_call",
    "double_constant",
    "getattr_",
    "initialize",
    "invalid_initializer",
    "lookup_method",
    "numeric_modulo",
    "physical_field_read",
    "physical_field_write",
    "primitive",
    "read_int64",
    "render_text",
    "text_access",
    "text_result",
    "unbox_float_operand",
    "write_int64",
]
