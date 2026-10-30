"""Tagged integer, boxed/unboxed float, comparison, and raw scalar VOP definitions."""

from __future__ import annotations

import math

from mule import ast

from . import fndb
from .ctype import (
    REAL,
    UNKNOWN,
)
from .node import CallKind, LeafVisitor, NodeVisitor
from .vmdef import define_vop
from .vop import (
    BOOLEAN,
    BOOLEAN_T,
    BOXED_FLOAT,
    BOXED_REAL,
    CLASS,
    DESCRIPTOR,
    DOUBLE,
    FIXNUM,
    FLOAT_BOX_LAYOUT,
    IMMEDIATE,
    INSTANCE,
    NULL,
    REFERENCE,
    SIGNED,
)
from .vops_core import (
    _PRIMITIVE_BINARY,
    _RAW_BINARY,
    _RAW_COMPARE,
    _RAW_FLOAT_BINARY,
    _RAW_FLOAT_COMPARE,
    BINARY_OPCODES,
    COLD_BRANCH,
    COMPARE_OPCODES,
    HOT_BRANCH,
    OBJECT,
    RAW,
    TEXT_INLINE_WORDS_OFFSET,
    TEXT_KIND_TAG_OFFSET,
    UNARY_OPCODES,
    _decode_fixnum_operand,
    _pack_inline_latin1_words,
    _tn_exact_text,
    _uses_pointer_text_compare,
)

# Known functions over objects.


def _binary_generic(gen, vop, opcode, *, inplace):
    result = vop.results[0]
    left, right = vop.args
    lhs, rhs = gen.take(left), gen.take(right)
    left_type = left.tn.type or UNKNOWN
    if opcode == 5 and not inplace and not left_type.fits(REAL):
        value = gen.compiled_call_named("mule_format_modulo", (lhs, rhs))
        gen.publish(result, value)
        gen.check_pointer(value)
        gen.refine(result.tn, value)
        return
    right_type = right.tn.type or UNKNOWN
    fixnums = left_type.is_fixnum and right_type.is_fixnum
    result_type = result.tn.type or UNKNOWN
    if fixnums and result_type.is_fixnum:
        prim_op = _PRIMITIVE_BINARY.get(opcode)
        if (
            prim_op is None
            and opcode in (8, 9)
            and right_type.minimum is not None
            and right_type.maximum is not None
            and 0 <= right_type.minimum
            and right_type.maximum <= 62
        ):
            prim_op = "shl" if opcode == 8 else "ashr"
        if (
            prim_op is None
            and opcode in (4, 5)
            and left_type.minimum is not None
            and left_type.minimum >= 0
            and right_type.minimum is not None
            and right_type.minimum > 0
        ):
            prim_op = "udiv" if opcode == 4 else "urem"
        if prim_op is not None:
            a, b = gen.decode(lhs), gen.decode(rhs)
            integer = gen.operation(f"{prim_op} i64 {a}, {b}")
            gen.native_result(result, gen.encode(integer), (lhs, rhs))
            return
        if opcode in (4, 5) and (
            (right_type.minimum is not None and right_type.minimum > 0)
            or (right_type.maximum is not None and right_type.maximum < 0)
        ):
            a, b = gen.decode(lhs), gen.decode(rhs)
            rem = gen.operation(f"srem i64 {a}, {b}")
            pred = (
                "slt"
                if right_type.minimum is not None and right_type.minimum > 0
                else "sgt"
            )
            adjust = gen.operation(f"icmp {pred} i64 {rem}, 0")
            if opcode == 5:
                adjusted = gen.operation(f"add nsw i64 {rem}, {b}")
                integer = gen.operation(
                    f"select i1 {adjust}, i64 {adjusted}, i64 {rem}"
                )
            else:
                quot = gen.operation(f"sdiv i64 {a}, {b}")
                corr = gen.operation(f"zext i1 {adjust} to i64")
                integer = gen.operation(f"sub nsw i64 {quot}, {corr}")
            gen.native_result(result, gen.encode(integer), (lhs, rhs))
            return
    prefix = "fixnum" if fixnums else "fast"
    reuse = "null"
    reuse_slot = "null"
    info = vop.codegen_info
    if (
        inplace
        and not fixnums
        and opcode in (0, 1, 2)
        and info is not None
        and info.reuse_local is not None
        and gen.local_owned(info.reuse_local)
    ):
        reuse = gen.operation(f"load ptr, ptr {gen.local_slot(info.reuse_local)}")
        reuse_slot = gen.reuse_slot()
    rejected = gen.context.rejected_fixnum_guard
    if rejected is not None and rejected.rejects_operands(gen.component, (left, right)):
        value = _binary_fallback(gen, lhs, rhs, opcode, inplace, reuse, reuse_slot)
        gen.native_result(result, value)
        gen.refine(result.tn, value)
        return
    if opcode in (0, 1):
        _tagged_add_sub(
            gen, result, lhs, rhs, opcode, fixnums, inplace, reuse, reuse_slot
        )
        return
    if opcode in (10, 11, 12):
        _tagged_bitwise(
            gen, result, lhs, rhs, opcode, fixnums, inplace, reuse, reuse_slot
        )
        return
    if opcode in (2, 4, 5, 8, 9):
        operation = (
            f"call ptr @mule.{prefix}.binary.{opcode}(ptr {lhs}, ptr {rhs}, "
            f"i32 {int(inplace)}, ptr {reuse}, ptr {reuse_slot})"
        )
    else:
        operation = (
            f"call ptr @mule_binary_reuse(i32 {opcode}, ptr {lhs}, ptr {rhs}, "
            f"i32 {int(inplace)}, ptr null)"
        )
    gen.pointer_result(result, operation, (lhs, rhs))


def _tagged_bitwise(gen, result, lhs, rhs, opcode, fixnums, inplace, reuse, reuse_slot):
    """Emit inline tagged bitwise AND/XOR/OR without overflow or fast-path null checks."""
    left_bits = gen.operation(f"ptrtoint ptr {lhs} to i64")
    right_bits = gen.operation(f"ptrtoint ptr {rhs} to i64")
    if fixnums:
        if opcode == 10:
            bits = gen.operation(f"and i64 {left_bits}, {right_bits}")
        elif opcode == 12:
            bits = gen.operation(f"or i64 {left_bits}, {right_bits}")
        else:
            xored = gen.operation(f"xor i64 {left_bits}, {right_bits}")
            bits = gen.operation(f"or i64 {xored}, 1")
        value = gen.operation(f"inttoptr i64 {bits} to ptr")
        gen.native_result(result, value)
        gen.refine(result.tn, value)
        return
    slow = gen.fresh("bitwise.slow")[1:]
    fast = gen.fresh("bitwise.fast")[1:]
    done = gen.fresh("bitwise.done")[1:]
    tags = gen.operation(f"and i64 {left_bits}, {right_bits}")
    tag = gen.operation(f"and i64 {tags}, 1")
    integers = gen.operation(f"icmp ne i64 {tag}, 0")
    gen.line(f"br i1 {integers}, label %{fast}, label %{slow}, {HOT_BRANCH}")
    gen.lines.append(fast + ":")
    if opcode == 10:
        bits = gen.operation(f"and i64 {left_bits}, {right_bits}")
    elif opcode == 12:
        bits = gen.operation(f"or i64 {left_bits}, {right_bits}")
    else:
        xored = gen.operation(f"xor i64 {left_bits}, {right_bits}")
        bits = gen.operation(f"or i64 {xored}, 1")
    value = gen.operation(f"inttoptr i64 {bits} to ptr")
    gen.line(f"br label %{done}")

    gen.lines.append(slow + ":")
    with gen.cold_block():
        fallback = _binary_fallback(gen, lhs, rhs, opcode, inplace, reuse, reuse_slot)
        slow_success = gen.current_block()
        gen.line(f"br label %{done}")
    gen.lines.append(done + ":")
    merged = gen.operation(f"phi ptr [{value}, %{fast}], [{fallback}, %{slow_success}]")
    gen.native_result(result, merged)
    gen.refine(result.tn, merged)


def _tagged_add_sub(gen, result, lhs, rhs, opcode, fixnums, inplace, reuse, reuse_slot):
    """Keep infallible tagged arithmetic separate from fallible object dispatch."""
    slow = gen.fresh("arithmetic.slow")[1:]
    fast = gen.fresh("arithmetic.fast")[1:]
    done = gen.fresh("arithmetic.done")[1:]
    left_bits = gen.operation(f"ptrtoint ptr {lhs} to i64")
    right_bits = gen.operation(f"ptrtoint ptr {rhs} to i64")
    if not fixnums:
        tags = gen.operation(f"and i64 {left_bits}, {right_bits}")
        tag = gen.operation(f"and i64 {tags}, 1")
        integers = gen.operation(f"icmp ne i64 {tag}, 0")
        arithmetic = gen.fresh("arithmetic.fixnums")[1:]
        gen.line(f"br i1 {integers}, label %{arithmetic}, label %{slow}, {HOT_BRANCH}")
        gen.lines.append(arithmetic + ":")
    intrinsic = "sadd" if opcode == 0 else "ssub"
    cleared = gen.operation(f"and i64 {left_bits if opcode == 0 else right_bits}, -2")
    left = cleared if opcode == 0 else left_bits
    right = right_bits if opcode == 0 else cleared
    checked = gen.operation(
        f"call {{i64, i1}} @llvm.{intrinsic}.with.overflow.i64(i64 {left}, i64 {right})"
    )
    overflow = gen.operation(f"extractvalue {{i64, i1}} {checked}, 1")
    gen.line(f"br i1 {overflow}, label %{slow}, label %{fast}, {COLD_BRANCH}")
    gen.lines.append(fast + ":")
    bits = gen.operation(f"extractvalue {{i64, i1}} {checked}, 0")
    value = gen.operation(f"inttoptr i64 {bits} to ptr")
    # This edge proves both operands are immediate fixnums, including when
    # their incoming TNs were unknown. Neither operand owns heap storage here.
    gen.line(f"br label %{done}")

    gen.lines.append(slow + ":")
    with gen.cold_block():
        fallback = _binary_fallback(gen, lhs, rhs, opcode, inplace, reuse, reuse_slot)
        slow_success = gen.current_block()
        gen.line(f"br label %{done}")
    gen.lines.append(done + ":")
    merged = gen.operation(f"phi ptr [{value}, %{fast}], [{fallback}, %{slow_success}]")
    gen.native_result(result, merged)
    gen.refine(result.tn, merged)


def _binary_fallback(gen, lhs, rhs, opcode, inplace, reuse, reuse_slot):
    """Dispatch owned operands and check only this fallible operation's result."""
    if reuse_slot != "null":
        gen.line(f"store ptr {reuse}, ptr {reuse_slot}")
    fallback = gen.operation(
        f"call ptr @mule_binary_reuse(i32 {opcode}, ptr {lhs}, ptr {rhs}, "
        f"i32 {int(inplace)}, ptr {reuse_slot})"
    )
    # The operands were already consumed. Release them on failure too,
    # while leaving unrelated live temporaries to the ordinary failure landing.
    for operand in (lhs, rhs):
        gen.release(operand)
    gen.check_pointer(fallback)
    return fallback


def _define_binary(operator, opcode):
    @define_vop(
        f"binary-{operator.name.lower()}",
        translate=fndb.BINARY[operator],
        args=(OBJECT, OBJECT),
        results=(OBJECT,),
    )
    def generic(gen, vop):
        _binary_generic(gen, vop, opcode, inplace=False)

    @define_vop(
        f"inplace-{operator.name.lower()}",
        translate=fndb.INPLACE_BINARY[operator],
        args=(OBJECT, OBJECT),
        results=(OBJECT,),
    )
    def inplace(gen, vop):
        _binary_generic(gen, vop, opcode, inplace=True)


for _operator, _opcode in BINARY_OPCODES.items():
    _define_binary(_operator, _opcode)


def _fixnum_binary(operator, opcode):
    @define_vop(
        f"fixnum-{operator.name.lower()}",
        translate=fndb.BINARY[operator],
        also_translate=(fndb.INPLACE_BINARY[operator],),
        args=((SIGNED,), (SIGNED,)),
        results=((SIGNED,),),
        arg_types=((FIXNUM,), (FIXNUM,)),
        result_types=((FIXNUM,),),
        cost=-1,
        arg_escapes=(False, False),
    )
    def generator(gen, vop):
        # The result range proof excludes Python integer overflow. No
        # int64 contract or whole-function native representation is needed.
        left, right = (gen.take(operand) for operand in vop.args)
        gen.store(vop.results[0], gen.operation(f"{opcode} i64 {left}, {right}"))


for _operator, _opcode in _RAW_BINARY.items():
    _fixnum_binary(_operator, _opcode)


@define_vop(
    "fixnum-left_shift",
    translate=fndb.BINARY[ast.BinaryOperator.LEFT_SHIFT],
    also_translate=(fndb.INPLACE_BINARY[ast.BinaryOperator.LEFT_SHIFT],),
    args=((SIGNED,), (SIGNED,)),
    results=((SIGNED,),),
    arg_types=((FIXNUM,), (FIXNUM,)),
    result_types=((FIXNUM,),),
    guard=lambda call: (
        call.args[1].info is not None
        and call.args[1].info.value_type.minimum is not None
        and call.args[1].info.value_type.minimum >= 0
        and call.args[1].info.value_type.maximum is not None
        and call.args[1].info.value_type.maximum <= 62
    ),
    cost=-1,
    arg_escapes=(False, False),
)
def fixnum_left_shift(gen, vop):
    left, right = (gen.take(operand) for operand in vop.args)
    gen.store(vop.results[0], gen.operation(f"shl nsw i64 {left}, {right}"))


@define_vop(
    "fixnum-right_shift",
    translate=fndb.BINARY[ast.BinaryOperator.RIGHT_SHIFT],
    also_translate=(fndb.INPLACE_BINARY[ast.BinaryOperator.RIGHT_SHIFT],),
    args=((SIGNED,), (SIGNED,)),
    results=((SIGNED,),),
    arg_types=((FIXNUM,), (FIXNUM,)),
    result_types=((FIXNUM,),),
    guard=lambda call: (
        call.args[1].info is not None
        and call.args[1].info.value_type.minimum is not None
        and call.args[1].info.value_type.minimum >= 0
        and call.args[1].info.value_type.maximum is not None
        and call.args[1].info.value_type.maximum <= 62
    ),
    cost=-1,
    arg_escapes=(False, False),
)
def fixnum_right_shift(gen, vop):
    left, right = (gen.take(operand) for operand in vop.args)
    gen.store(vop.results[0], gen.operation(f"ashr i64 {left}, {right}"))


@define_vop(
    "fixnum-floor_divide",
    translate=fndb.BINARY[ast.BinaryOperator.FLOOR_DIVIDE],
    also_translate=(fndb.INPLACE_BINARY[ast.BinaryOperator.FLOOR_DIVIDE],),
    args=((SIGNED,), (SIGNED,)),
    results=((SIGNED,),),
    arg_types=((FIXNUM,), (FIXNUM,)),
    result_types=((FIXNUM,),),
    guard=lambda call: (
        call.args[1].info is not None
        and (
            (
                call.args[1].info.value_type.minimum is not None
                and call.args[1].info.value_type.minimum > 0
            )
            or (
                call.args[1].info.value_type.maximum is not None
                and call.args[1].info.value_type.maximum < 0
            )
        )
    ),
    cost=-1,
    arg_escapes=(False, False),
)
def fixnum_floor_divide(gen, vop):
    a, b = (gen.take(operand) for operand in vop.args)
    left_type = vop.args[0].tn.type or UNKNOWN
    right_type = vop.args[1].tn.type or UNKNOWN
    if (
        left_type.minimum is not None
        and left_type.minimum >= 0
        and right_type.minimum is not None
        and right_type.minimum > 0
    ):
        gen.store(vop.results[0], gen.operation(f"udiv i64 {a}, {b}"))
        return
    rem = gen.operation(f"srem i64 {a}, {b}")
    pred = "slt" if right_type.minimum is not None and right_type.minimum > 0 else "sgt"
    adjust = gen.operation(f"icmp {pred} i64 {rem}, 0")
    quot = gen.operation(f"sdiv i64 {a}, {b}")
    corr = gen.operation(f"zext i1 {adjust} to i64")
    gen.store(vop.results[0], gen.operation(f"sub nsw i64 {quot}, {corr}"))


@define_vop(
    "fixnum-modulo",
    translate=fndb.BINARY[ast.BinaryOperator.MODULO],
    also_translate=(fndb.INPLACE_BINARY[ast.BinaryOperator.MODULO],),
    args=((SIGNED,), (SIGNED,)),
    results=((SIGNED,),),
    arg_types=((FIXNUM,), (FIXNUM,)),
    result_types=((FIXNUM,),),
    guard=lambda call: (
        call.args[1].info is not None
        and (
            (
                call.args[1].info.value_type.minimum is not None
                and call.args[1].info.value_type.minimum > 0
            )
            or (
                call.args[1].info.value_type.maximum is not None
                and call.args[1].info.value_type.maximum < 0
            )
        )
    ),
    cost=-1,
    arg_escapes=(False, False),
)
def fixnum_modulo(gen, vop):
    a, b = (gen.take(operand) for operand in vop.args)
    left_type = vop.args[0].tn.type or UNKNOWN
    right_type = vop.args[1].tn.type or UNKNOWN
    if (
        left_type.minimum is not None
        and left_type.minimum >= 0
        and right_type.minimum is not None
        and right_type.minimum > 0
    ):
        gen.store(vop.results[0], gen.operation(f"urem i64 {a}, {b}"))
        return
    rem = gen.operation(f"srem i64 {a}, {b}")
    pred = "slt" if right_type.minimum is not None and right_type.minimum > 0 else "sgt"
    adjust = gen.operation(f"icmp {pred} i64 {rem}, 0")
    adjusted = gen.operation(f"add nsw i64 {rem}, {b}")
    gen.store(
        vop.results[0],
        gen.operation(f"select i1 {adjust}, i64 {adjusted}, i64 {rem}"),
    )


def _fixnum_compare(operator, predicate):
    @define_vop(
        f"fixnum-compare-{operator.name.lower()}",
        translate=fndb.COMPARE[operator],
        args=((SIGNED,), (SIGNED,)),
        results=(OBJECT,),
        arg_types=((FIXNUM,), (FIXNUM,)),
        cost=-1,
        arg_escapes=(False, False),
    )
    def generator(gen, vop):
        left, right = (gen.take(operand) for operand in vop.args)
        condition = gen.operation(f"icmp {predicate} i64 {left}, {right}")
        gen.native_result(
            vop.results[0],
            gen.operation(f"select i1 {condition}, ptr @mule.true, ptr @mule.false"),
        )


for _operator, _predicate in _RAW_COMPARE.items():
    _fixnum_compare(_operator, _predicate)


def _fixnum_unary(operator, opcode):
    @define_vop(
        f"fixnum-unary-{operator.name.lower()}",
        translate=fndb.UNARY[operator],
        args=((SIGNED,),),
        results=((SIGNED,),),
        arg_types=((FIXNUM,),),
        result_types=((FIXNUM,),),
        cost=-1,
        arg_escapes=(False,),
    )
    def generator(gen, vop):
        value = gen.take(vop.args[0])
        if opcode == 1:
            value = gen.operation(f"sub i64 0, {value}")
        elif opcode == 2:
            value = gen.operation(f"xor i64 {value}, -1")
        gen.store(vop.results[0], value)


for _operator, _opcode in UNARY_OPCODES.items():
    if _opcode < 3:
        _fixnum_unary(_operator, _opcode)


def unbox_float_operand(gen, operand):
    if operand.tn.sc is DOUBLE:
        return gen.load(operand)
    ptr_val = gen.load(operand)
    if operand.tn.primitive_type is BOXED_REAL:
        bits = gen.operation(f"ptrtoint ptr {ptr_val} to i64")
        tag = gen.operation(f"and i64 {bits}, 1")
        is_fix = gen.operation(f"icmp ne i64 {tag}, 0")
        fix_lbl, flt_lbl, done_lbl = (
            gen.fresh("real.fix")[1:],
            gen.fresh("real.flt")[1:],
            gen.fresh("real.done")[1:],
        )
        gen.line(f"br i1 {is_fix}, label %{fix_lbl}, label %{flt_lbl}")
        gen.lines.append(fix_lbl + ":")
        fix_i64 = gen.operation(f"ashr i64 {bits}, 1")
        fix_f64 = gen.operation(f"sitofp i64 {fix_i64} to double")
        gen.line(f"br label %{done_lbl}")
        gen.lines.append(flt_lbl + ":")
        flt_f64 = gen.operation(
            f"call double @{FLOAT_BOX_LAYOUT.symbol('load')}(ptr {ptr_val})"
        )
        gen.line(f"br label %{done_lbl}")
        gen.lines.append(done_lbl + ":")
        return gen.operation(
            f"phi double [{fix_f64}, %{fix_lbl}], [{flt_f64}, %{flt_lbl}]"
        )
    return gen.operation(
        f"call double @{FLOAT_BOX_LAYOUT.symbol('load')}(ptr {ptr_val})"
    )


def _boxed_float_binary(operator, opcode):
    @define_vop(
        f"boxed-float-{operator.name.lower()}",
        translate=fndb.BINARY[operator],
        also_translate=(fndb.INPLACE_BINARY[operator],),
        args=((DESCRIPTOR, DOUBLE), (DESCRIPTOR, DOUBLE)),
        results=((DESCRIPTOR, DOUBLE),),
        arg_types=((BOXED_FLOAT,), (BOXED_FLOAT,)),
        result_types=((BOXED_FLOAT,),),
        cost=-1,
        fresh_result=True,
        reuses_local=True,
        arg_escapes=(False, False),
    )
    def generator(gen, vop):
        # Keep both operand slots owned through a possible division error.
        operands = [unbox_float_operand(gen, operand) for operand in vop.args]
        value = gen.float_arithmetic(opcode, *operands, check_zero=True)
        left, right = (gen.take(operand) for operand in vop.args)
        consumed = [
            value
            for operand, value in zip(vop.args, (left, right))
            if operand.tn.sc is DESCRIPTOR
        ]
        if vop.results[0].tn.sc is DOUBLE:
            gen.store(vop.results[0], value)
            for operand in consumed:
                gen.release(operand)
            return
        info = vop.codegen_info
        if (
            info is not None
            and info.reuse_local is not None
            and gen.local_owned(info.reuse_local)
            and vop.args[0].tn.sc is DESCRIPTOR
        ):
            local = gen.operation(f"load ptr, ptr {gen.local_slot(info.reuse_local)}")
            operation = (
                f"call ptr @mule_box_f64_move(double {value}, ptr {left}, ptr {local})"
            )
            consumed = [right] if vop.args[1].tn.sc is DESCRIPTOR else []
        else:
            operation = f"call ptr @{FLOAT_BOX_LAYOUT.symbol()}(double {value})"
        gen.pointer_result(vop.results[0], operation, consumed)


for _operator, _opcode in _RAW_FLOAT_BINARY.items():
    _boxed_float_binary(_operator, _opcode)


def _mixed_float_binary(operator, opcode, left_float: bool):
    arg_scs = (
        ((DESCRIPTOR, DOUBLE), (IMMEDIATE,))
        if left_float
        else ((IMMEDIATE,), (DESCRIPTOR, DOUBLE))
    )
    arg_ptypes = (
        ((BOXED_FLOAT,), (FIXNUM, BOOLEAN_T))
        if left_float
        else ((FIXNUM, BOOLEAN_T), (BOXED_FLOAT,))
    )
    side = "left" if left_float else "right"

    @define_vop(
        f"mixed-float-{operator.name.lower()}-{side}",
        translate=fndb.BINARY[operator],
        also_translate=(fndb.INPLACE_BINARY[operator],),
        args=arg_scs,
        results=((DESCRIPTOR, DOUBLE),),
        arg_types=arg_ptypes,
        result_types=((BOXED_FLOAT,),),
        cost=-1,
        fresh_result=True,
        arg_escapes=(False, False),
    )
    def generator(gen, vop):
        if left_float:
            left_f64 = unbox_float_operand(gen, vop.args[0])
            right_i64 = _decode_fixnum_operand(gen, vop.args[1])
            right_f64 = gen.operation(f"sitofp i64 {right_i64} to double")
        else:
            left_i64 = _decode_fixnum_operand(gen, vop.args[0])
            left_f64 = gen.operation(f"sitofp i64 {left_i64} to double")
            right_f64 = unbox_float_operand(gen, vop.args[1])
        value = gen.float_arithmetic(opcode, left_f64, right_f64, check_zero=True)
        float_op = vop.args[0] if left_float else vop.args[1]
        taken = gen.take(float_op)
        consumed = [taken] if float_op.tn.sc is DESCRIPTOR else []
        if vop.results[0].tn.sc is DOUBLE:
            gen.store(vop.results[0], value)
            for operand in consumed:
                gen.release(operand)
            return
        operation = f"call ptr @{FLOAT_BOX_LAYOUT.symbol()}(double {value})"
        gen.pointer_result(vop.results[0], operation, consumed)


for _operator, _opcode in _RAW_FLOAT_BINARY.items():
    _mixed_float_binary(_operator, _opcode, True)
    _mixed_float_binary(_operator, _opcode, False)


def _mixed_real_float_binary(operator, opcode, left_float: bool):
    arg_scs = (
        ((DESCRIPTOR, DOUBLE), (DESCRIPTOR,))
        if left_float
        else ((DESCRIPTOR,), (DESCRIPTOR, DOUBLE))
    )
    arg_ptypes = (
        ((BOXED_FLOAT,), (BOXED_REAL,))
        if left_float
        else ((BOXED_REAL,), (BOXED_FLOAT,))
    )
    side = "left" if left_float else "right"

    @define_vop(
        f"mixed-real-float-{operator.name.lower()}-{side}",
        translate=fndb.BINARY[operator],
        also_translate=(fndb.INPLACE_BINARY[operator],),
        args=arg_scs,
        results=((DESCRIPTOR, DOUBLE),),
        arg_types=arg_ptypes,
        result_types=((BOXED_FLOAT,),),
        cost=-1,
        fresh_result=True,
        reuses_local=not left_float,
        arg_escapes=(False, False),
    )
    def generator(gen, vop):
        operands = [unbox_float_operand(gen, operand) for operand in vop.args]
        value = gen.float_arithmetic(opcode, *operands, check_zero=True)
        left, right = (gen.take(operand) for operand in vop.args)
        consumed = [
            val
            for operand, val in zip(vop.args, (left, right))
            if operand.tn.sc is DESCRIPTOR
        ]
        if vop.results[0].tn.sc is DOUBLE:
            gen.store(vop.results[0], value)
            for operand in consumed:
                gen.release(operand)
            return
        info = vop.codegen_info
        if (
            not left_float
            and info is not None
            and info.reuse_local is not None
            and gen.local_owned(info.reuse_local)
            and vop.args[0].tn.sc is DESCRIPTOR
        ):
            local = gen.operation(f"load ptr, ptr {gen.local_slot(info.reuse_local)}")
            operation = (
                f"call ptr @mule_box_f64_move(double {value}, ptr {left}, ptr {local})"
            )
            consumed = [right] if vop.args[1].tn.sc is DESCRIPTOR else []
        else:
            operation = f"call ptr @{FLOAT_BOX_LAYOUT.symbol()}(double {value})"
        gen.pointer_result(vop.results[0], operation, consumed)


for _operator, _opcode in _RAW_FLOAT_BINARY.items():
    _mixed_real_float_binary(_operator, _opcode, True)
    _mixed_real_float_binary(_operator, _opcode, False)


@define_vop(
    "boxed-float-math-sqrt",
    translate=fndb.MATH_SQRT,
    args=((DESCRIPTOR, DOUBLE),),
    results=((DESCRIPTOR, DOUBLE),),
    arg_types=((BOXED_FLOAT,),),
    result_types=((BOXED_FLOAT,),),
    cost=-1,
    fresh_result=True,
    arg_escapes=(False,),
)
def boxed_float_math_sqrt(gen, vop):
    operand = vop.args[0]
    raw = unbox_float_operand(gen, operand)
    value = gen.float_sqrt(raw)
    taken = gen.take(operand)
    consumed = [taken] if operand.tn.sc is DESCRIPTOR else []
    if vop.results[0].tn.sc is DOUBLE:
        gen.store(vop.results[0], value)
        for item in consumed:
            gen.release(item)
        return
    gen.pointer_result(
        vop.results[0],
        f"call ptr @{FLOAT_BOX_LAYOUT.symbol()}(double {value})",
        consumed,
    )


class _SameLeafVar(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return None

    def visit_Ref(self, node):
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return None

    def visit_LambdaVar(self, leaf):
        return leaf if not leaf.indirect else None


_SAME_LEAF_VAR = _SameLeafVar()


class _NonNegativeLiteral(ast.Visitor):
    def visit_Node(self, node) -> bool:
        return False

    def visit_IntegerLiteral(self, node) -> bool:
        return node.value >= 0

    def visit_FloatLiteral(self, node) -> bool:
        return node.value >= 0.0


_NON_NEGATIVE_LITERAL = _NonNegativeLiteral()


class _IntegralExponentLiteral(ast.Visitor):
    def visit_Node(self, node) -> bool:
        return False

    def visit_IntegerLiteral(self, node) -> bool:
        return True

    def visit_FloatLiteral(self, node) -> bool:
        return math.isfinite(node.value) and node.value.is_integer()


_INTEGRAL_EXPONENT_LITERAL = _IntegralExponentLiteral()


class _IsIntegralExponentNode(NodeVisitor, LeafVisitor):
    def visit_Node(self, node) -> bool:
        return False

    def visit_Ref(self, node) -> bool:
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf) -> bool:
        return False

    def visit_Constant(self, leaf) -> bool:
        return leaf.value.accept(_INTEGRAL_EXPONENT_LITERAL)


_IS_INTEGRAL_EXPONENT_NODE = _IsIntegralExponentNode()


class _IsNonNegativeFloatNode(NodeVisitor, LeafVisitor):
    def __init__(self):
        self.visited = set()

    def visit_Node(self, node) -> bool:
        return False

    def visit_Ref(self, node) -> bool:
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf) -> bool:
        return False

    def visit_Constant(self, leaf) -> bool:
        return leaf.value.accept(_NON_NEGATIVE_LITERAL)

    def visit_LambdaVar(self, leaf) -> bool:
        if (
            leaf.indirect
            or len(leaf.sets) != 1
            or leaf.sets[0].value is None
            or len(leaf.sets[0].value.uses) != 1
            or leaf in self.visited
        ):
            return False
        self.visited.add(leaf)
        return leaf.sets[0].value.use().accept(self)

    def visit_Cast(self, node) -> bool:
        return len(node.value.uses) == 1 and node.value.use().accept(self)

    def visit_Combination(self, node) -> bool:
        if node.kind is not CallKind.KNOWN or len(node.args) != 2:
            return False
        if len(node.args[0].uses) != 1 or len(node.args[1].uses) != 1:
            return False
        u0, u1 = node.args[0].use(), node.args[1].use()
        if node.fun_info is fndb.BINARY[ast.BinaryOperator.MULTIPLY]:
            v0 = u0.accept(_SAME_LEAF_VAR)
            if v0 is not None and v0 is u1.accept(_SAME_LEAF_VAR):
                return True
            return u0.accept(self) and u1.accept(self)
        if node.fun_info is fndb.BINARY[ast.BinaryOperator.ADD]:
            return u0.accept(self) and u1.accept(self)
        return False


def _is_real_float_power_call(call) -> bool:
    if len(call.args) != 2:
        return False
    if len(call.args[1].uses) == 1 and call.args[1].use().accept(
        _IS_INTEGRAL_EXPONENT_NODE
    ):
        return True
    return len(call.args[0].uses) == 1 and call.args[0].use().accept(
        _IsNonNegativeFloatNode()
    )


@define_vop(
    "boxed-float-power",
    translate=fndb.BINARY[ast.BinaryOperator.POWER],
    also_translate=(fndb.INPLACE_BINARY[ast.BinaryOperator.POWER],),
    args=((DESCRIPTOR, DOUBLE), (DESCRIPTOR, DOUBLE)),
    results=((DESCRIPTOR, DOUBLE),),
    arg_types=((BOXED_FLOAT,), (BOXED_FLOAT,)),
    result_types=((BOXED_FLOAT,),),
    guard=_is_real_float_power_call,
    cost=-1,
    fresh_result=True,
    reuses_local=True,
    arg_escapes=(False, False),
)
def boxed_float_power(gen, vop):
    operands = [unbox_float_operand(gen, operand) for operand in vop.args]
    value = gen.float_pow(*operands)
    left, right = (gen.take(operand) for operand in vop.args)
    consumed = [
        val
        for operand, val in zip(vop.args, (left, right))
        if operand.tn.sc is DESCRIPTOR
    ]
    if vop.results[0].tn.sc is DOUBLE:
        gen.store(vop.results[0], value)
        for operand in consumed:
            gen.release(operand)
        return
    info = vop.codegen_info
    if (
        info is not None
        and info.reuse_local is not None
        and gen.local_owned(info.reuse_local)
        and vop.args[0].tn.sc is DESCRIPTOR
    ):
        local = gen.operation(f"load ptr, ptr {gen.local_slot(info.reuse_local)}")
        operation = (
            f"call ptr @mule_box_f64_move(double {value}, ptr {left}, ptr {local})"
        )
        consumed = [right] if vop.args[1].tn.sc is DESCRIPTOR else []
    else:
        operation = f"call ptr @{FLOAT_BOX_LAYOUT.symbol()}(double {value})"
    gen.pointer_result(vop.results[0], operation, consumed)


@define_vop(
    "math-sqrt",
    translate=fndb.MATH_SQRT,
    args=(OBJECT,),
    results=(OBJECT,),
)
def math_sqrt(gen, vop):
    operand = vop.args[0]
    op_type = operand.tn.type or UNKNOWN
    if op_type.is_fixnum:
        raw = gen.decode(gen.take(operand))
        fval = gen.operation(f"sitofp i64 {raw} to double")
        check_domain = op_type.minimum is None or op_type.minimum < 0
        res = gen.float_sqrt(fval, check_domain=check_domain)
        gen.pointer_result(
            vop.results[0],
            f"call ptr @{FLOAT_BOX_LAYOUT.symbol()}(double {res})",
        )
        return
    val = gen.take(operand)
    slot = gen.operation("alloca double")
    status = gen.operation(f"call i32 @mule_unbox_real(ptr {val}, ptr {slot})")
    gen.release(val)
    gen.check_status(status)
    fval = gen.operation(f"load double, ptr {slot}")
    res = gen.float_sqrt(fval)
    gen.pointer_result(
        vop.results[0],
        f"call ptr @{FLOAT_BOX_LAYOUT.symbol()}(double {res})",
    )


def _define_minmax2(name, info, is_max: bool):
    int_pred = "slt" if is_max else "sgt"
    cmp_opcode = (
        COMPARE_OPCODES[ast.ComparisonOperator.GREATER]
        if is_max
        else COMPARE_OPCODES[ast.ComparisonOperator.LESS]
    )

    @define_vop(
        name,
        translate=info,
        args=(OBJECT, OBJECT),
        results=(OBJECT,),
    )
    def generator(gen, vop):
        result = vop.results[0]
        left, right = vop.args
        lhs, rhs = gen.take(left), gen.take(right)
        left_type = left.tn.type or UNKNOWN
        right_type = right.tn.type or UNKNOWN
        if left_type.is_fixnum and right_type.is_fixnum:
            a, b = gen.decode(lhs), gen.decode(rhs)
            pick_right = gen.operation(f"icmp {int_pred} i64 {a}, {b}")
            chosen = gen.operation(f"select i1 {pick_right}, ptr {rhs}, ptr {lhs}")
            gen.native_result(result, chosen, (lhs, rhs))
            return
        left_bits = gen.operation(f"ptrtoint ptr {lhs} to i64")
        right_bits = gen.operation(f"ptrtoint ptr {rhs} to i64")
        tags = gen.operation(f"and i64 {left_bits}, {right_bits}")
        tag = gen.operation(f"and i64 {tags}, 1")
        both_fix = gen.operation(f"icmp ne i64 {tag}, 0")
        fast_lbl, slow_lbl, done_lbl = (
            gen.fresh("minmax.fast")[1:],
            gen.fresh("minmax.slow")[1:],
            gen.fresh("minmax.done")[1:],
        )
        gen.line(
            f"br i1 {both_fix}, label %{fast_lbl}, label %{slow_lbl}, {HOT_BRANCH}"
        )
        gen.lines.append(fast_lbl + ":")
        fast_pick_right = gen.operation(
            f"icmp {int_pred} i64 {left_bits}, {right_bits}"
        )
        fast_chosen = gen.operation(
            f"select i1 {fast_pick_right}, ptr {rhs}, ptr {lhs}"
        )
        gen.line(f"br label %{done_lbl}")
        gen.lines.append(slow_lbl + ":")
        with gen.cold_block():
            cmp_obj = gen.operation(
                f"call ptr @mule.fast.compare.{cmp_opcode}(ptr {rhs}, ptr {lhs})"
            )
            if not cmp_obj.startswith("@"):
                bad = gen.operation(f"icmp eq ptr {cmp_obj}, null")
                fail_lbl, ok_lbl = (
                    gen.fresh("minmax.fail")[1:],
                    gen.fresh("minmax.ok")[1:],
                )
                gen.line(
                    f"br i1 {bad}, label %{fail_lbl}, label %{ok_lbl}, {COLD_BRANCH}"
                )
                gen.lines.append(fail_lbl + ":")
                gen.release(lhs)
                gen.release(rhs)
                gen.line(f"br label %{gen.failure_target('error')}")
                gen.lines.append(ok_lbl + ":")
            truth = gen.operation(f"call i32 @mule.fast.truth(ptr {cmp_obj})")
            gen.release(cmp_obj)
            bad_truth = gen.operation(f"icmp slt i32 {truth}, 0")
            tfail_lbl, tok_lbl = (
                gen.fresh("minmax.tfail")[1:],
                gen.fresh("minmax.tok")[1:],
            )
            gen.line(
                f"br i1 {bad_truth}, label %{tfail_lbl}, label %{tok_lbl}, {COLD_BRANCH}"
            )
            gen.lines.append(tfail_lbl + ":")
            gen.release(lhs)
            gen.release(rhs)
            gen.line(f"br label %{gen.failure_target('error')}")
            gen.lines.append(tok_lbl + ":")
            pick_right = gen.operation(f"icmp ne i32 {truth}, 0")
            slow_chosen = gen.operation(f"select i1 {pick_right}, ptr {rhs}, ptr {lhs}")
            discarded = gen.operation(f"select i1 {pick_right}, ptr {lhs}, ptr {rhs}")
            gen.release(discarded)
            slow_end = gen.current_block()
            gen.line(f"br label %{done_lbl}")
        gen.lines.append(done_lbl + ":")
        merged = gen.operation(
            f"phi ptr [{fast_chosen}, %{fast_lbl}], [{slow_chosen}, %{slow_end}]"
        )
        gen.publish(result, merged)
        gen.refine(result.tn, merged)


_define_minmax2("min2", fndb.MIN2, False)
_define_minmax2("max2", fndb.MAX2, True)


def _boxed_float_unary(operator, negate: bool):
    @define_vop(
        f"boxed-float-{operator.name.lower()}",
        translate=fndb.UNARY[operator],
        args=((DESCRIPTOR, DOUBLE),) if negate else (None,),
        results=((DESCRIPTOR, DOUBLE),) if negate else (None,),
        arg_types=((BOXED_FLOAT,),),
        result_types=((BOXED_FLOAT,),),
        cost=-1,
        fresh_result=negate,
        arg_escapes=(not negate,),
    )
    def generator(gen, vop):
        operand = vop.args[0]
        if not negate:
            result = vop.results[0]
            if operand.tn.sc is not result.tn.sc:
                raise ValueError("float identity coercion requires matching storage")
            taken = gen.take(operand)
            if operand.tn.sc is DOUBLE:
                gen.store(result, taken)
                return
            class_slot = gen.operation(
                f"getelementptr i8, ptr {taken}, i64 {FLOAT_BOX_LAYOUT.null_pointer_offsets[0]}"
            )
            class_value = gen.operation(f"load ptr, ptr {class_slot}")
            exact = gen.operation(f"icmp eq ptr {class_value}, null")
            forward, convert, joined = (
                gen.fresh("positive.identity")[1:],
                gen.fresh("positive.convert")[1:],
                gen.fresh("positive.done")[1:],
            )
            gen.line(f"br i1 {exact}, label %{forward}, label %{convert}, {HOT_BRANCH}")
            gen.lines.append(forward + ":")
            gen.line(f"br label %{joined}")
            gen.lines.append(convert + ":")
            with gen.cold_block():
                raw = gen.operation(
                    f"call double @{FLOAT_BOX_LAYOUT.symbol('load')}(ptr {taken})"
                )
                boxed = gen.operation(
                    f"call ptr @{FLOAT_BOX_LAYOUT.symbol()}(double {raw})"
                )
                gen.line(f"br label %{joined}")
            gen.lines.append(joined + ":")
            value = gen.operation(
                f"phi ptr [{taken}, %{forward}], [{boxed}, %{convert}]"
            )
            consumed = gen.operation(f"select i1 {exact}, ptr null, ptr {taken}")
            gen.store(result, value)
            gen.release(consumed)
            return
        raw = unbox_float_operand(gen, operand)
        value = gen.operation(f"fneg double {raw}")
        taken = gen.take(operand)
        consumed = [taken] if operand.tn.sc is DESCRIPTOR else []
        if vop.results[0].tn.sc is DOUBLE:
            gen.store(vop.results[0], value)
            for item in consumed:
                gen.release(item)
            return
        gen.pointer_result(
            vop.results[0],
            f"call ptr @{FLOAT_BOX_LAYOUT.symbol()}(double {value})",
            consumed,
        )


_boxed_float_unary(ast.UnaryOperator.POSITIVE, False)
_boxed_float_unary(ast.UnaryOperator.NEGATIVE, True)


def _boxed_float_compare(operator, predicate):
    @define_vop(
        f"boxed-float-compare-{operator.name.lower()}",
        translate=fndb.COMPARE[operator],
        args=((DESCRIPTOR, DOUBLE), (DESCRIPTOR, DOUBLE)),
        results=(OBJECT,),
        arg_types=((BOXED_FLOAT,), (BOXED_FLOAT,)),
        cost=-1,
        arg_escapes=(False, False),
    )
    def generator(gen, vop):
        operands = [unbox_float_operand(gen, operand) for operand in vop.args]
        condition = gen.operation(
            f"fcmp {predicate} double {operands[0]}, {operands[1]}"
        )
        boolean = gen.operation(
            f"select i1 {condition}, ptr @mule.true, ptr @mule.false"
        )
        left, right = (gen.take(operand) for operand in vop.args)
        consumed = [
            val
            for operand, val in zip(vop.args, (left, right))
            if operand.tn.sc is DESCRIPTOR
        ]
        gen.native_result(vop.results[0], boolean, consumed)


for _operator, _predicate in _RAW_FLOAT_COMPARE.items():
    _boxed_float_compare(_operator, _predicate)


def _define_unary(operator, opcode):
    @define_vop(
        f"unary-{operator.name.lower()}",
        translate=fndb.UNARY[operator],
        args=(OBJECT,),
        results=(OBJECT,),
    )
    def generic(gen, vop):
        result = vop.results[0]
        operand = vop.args[0]
        value = gen.take(operand)
        operand_type = operand.tn.type or UNKNOWN
        fixnum = operand_type.is_fixnum
        if fixnum and (result.tn.type or UNKNOWN).is_fixnum and opcode < 3:
            decoded = gen.decode(value)
            integer = (
                decoded
                if opcode == 0
                else gen.operation(
                    f"sub i64 0, {decoded}" if opcode == 1 else f"xor i64 {decoded}, -1"
                )
            )
            gen.native_result(result, gen.encode(integer), (value,))
            return
        if fixnum and opcode == 3:
            bits = gen.operation(f"ptrtoint ptr {value} to i64")
            zero = gen.operation(f"icmp eq i64 {bits}, 1")
            boolean = gen.operation(
                f"select i1 {zero}, ptr @mule.true, ptr @mule.false"
            )
            gen.native_result(result, boolean, (value,))
            return
        if operand.tn.primitive_type is BOOLEAN_T and opcode == 3:
            is_false = gen.operation(f"icmp eq ptr {value}, @mule.false")
            boolean = gen.operation(
                f"select i1 {is_false}, ptr @mule.true, ptr @mule.false"
            )
            gen.native_result(result, boolean, (value,))
            return
        if opcode < 3 and not operand_type.fits(REAL):
            tagged = gen.operation(f"ptrtoint ptr {value} to i64")
            tag = gen.operation(f"and i64 {tagged}, 1")
            integer = gen.operation(f"icmp ne i64 {tag}, 0")
            fast, protocol, joined = (
                gen.fresh(name)[1:]
                for name in ("unary.integer", "unary.protocol", "unary.join")
            )
            gen.line(f"br i1 {integer}, label %{fast}, label %{protocol}, {HOT_BRANCH}")
            gen.lines.append(fast + ":")
            scalar = gen.operation(f"call ptr @mule.fixnum.unary.{opcode}(ptr {value})")
            gen.line(f"br label %{joined}")
            gen.lines.append(protocol + ":")
            with gen.cold_block():
                name = ("positive", "negative", "invert")[opcode]
                dynamic = gen.sequence_call(name, [value])
                protocol_end = gen.current_block()
                gen.line(f"br label %{joined}")
            gen.lines.append(joined + ":")
            gen.pointer_result(
                result, f"phi ptr [{scalar}, %{fast}], [{dynamic}, %{protocol_end}]"
            )
            return
        prefix = "fixnum" if fixnum and opcode < 3 else "fast"
        gen.pointer_result(
            result, f"call ptr @mule.{prefix}.unary.{opcode}(ptr {value})", (value,)
        )


for _operator, _opcode in UNARY_OPCODES.items():
    _define_unary(_operator, _opcode)


def _emit_inline_latin1_compare(
    gen, value: str, length: int, words: tuple[int, ...], opcode: int
) -> str:
    header_ptr = gen.operation(
        f"getelementptr inbounds i8, ptr {value}, i64 {TEXT_KIND_TAG_OFFSET}"
    )
    header = gen.operation(f"load i16, ptr {header_ptr}, align 8")
    expected_header = length << 8
    matched = gen.operation(f"icmp eq i16 {header}, {expected_header}")
    for index, word in enumerate(words):
        word_offset = TEXT_INLINE_WORDS_OFFSET + index * 8
        word_ptr = gen.operation(
            f"getelementptr inbounds i8, ptr {value}, i64 {word_offset}"
        )
        loaded_word = gen.operation(f"load i64, ptr {word_ptr}, align 8")
        word_match = gen.operation(f"icmp eq i64 {loaded_word}, {word}")
        matched = gen.operation(f"and i1 {matched}, {word_match}")
    if opcode == 1:
        matched = gen.operation(f"xor i1 {matched}, true")
    return gen.operation(f"select i1 {matched}, ptr @mule.true, ptr @mule.false")


def _define_compare(operator, opcode):
    @define_vop(
        f"compare-{operator.name.lower()}",
        translate=fndb.COMPARE[operator],
        args=(OBJECT, OBJECT),
        results=(OBJECT,),
        arg_escapes=(False, False) if opcode in (6, 7) else None,
    )
    def generic(gen, vop):
        result = vop.results[0]
        left, right = vop.args
        lhs, rhs = gen.take(left), gen.take(right)
        if opcode in (8, 9):
            name = "contains" if opcode == 8 else "not_contains"
            value = gen.sequence_call(name, [rhs, lhs])
            gen.publish(result, value)
            gen.check_pointer(value)
            gen.refine(result.tn, value)
            return
        if opcode in (6, 7):
            predicate = "eq" if opcode == 6 else "ne"
            condition = gen.operation(f"icmp {predicate} ptr {lhs}, {rhs}")
            boolean = gen.operation(
                f"select i1 {condition}, ptr @mule.true, ptr @mule.false"
            )
            gen.native_result(result, boolean, (lhs, rhs))
            return
        left_type = left.tn.type or UNKNOWN
        right_type = right.tn.type or UNKNOWN
        if opcode < 6 and left_type.is_fixnum and right_type.is_fixnum:
            a, b = gen.decode(lhs), gen.decode(rhs)
            predicate = ("eq", "ne", "slt", "sle", "sgt", "sge")[opcode]
            condition = gen.operation(f"icmp {predicate} i64 {a}, {b}")
            boolean = gen.operation(
                f"select i1 {condition}, ptr @mule.true, ptr @mule.false"
            )
            gen.native_result(result, boolean, (lhs, rhs))
            return
        if (
            opcode < 6
            and left_type.fits_text(left_type)
            and right_type.fits_text(right_type)
        ):
            if opcode in (0, 1) and _uses_pointer_text_compare(
                gen, left.tn, left_type, right.tn, right_type
            ):
                predicate = "eq" if opcode == 0 else "ne"
                condition = gen.operation(f"icmp {predicate} ptr {lhs}, {rhs}")
                boolean = gen.operation(
                    f"select i1 {condition}, ptr @mule.true, ptr @mule.false"
                )
            elif opcode in (0, 1):
                right_exact = _tn_exact_text(gen, right.tn, right_type)
                left_exact = _tn_exact_text(gen, left.tn, left_type)
                right_inline = _pack_inline_latin1_words(right_exact)
                left_inline = (
                    None
                    if right_inline is not None
                    else _pack_inline_latin1_words(left_exact)
                )
                if right_inline is not None:
                    boolean = _emit_inline_latin1_compare(
                        gen, lhs, right_inline[0], right_inline[1], opcode
                    )
                elif left_inline is not None:
                    boolean = _emit_inline_latin1_compare(
                        gen, rhs, left_inline[0], left_inline[1], opcode
                    )
                elif right_exact is not None or left_exact is not None:
                    matched = gen.operation(
                        f"call i32 @mule_text_eq(ptr {lhs}, ptr {rhs})"
                    )
                    predicate = "ne" if opcode == 0 else "eq"
                    condition = gen.operation(f"icmp {predicate} i32 {matched}, 0")
                    boolean = gen.operation(
                        f"select i1 {condition}, ptr @mule.true, ptr @mule.false"
                    )
                else:
                    helper_name = "eq" if opcode == 0 else "ne"
                    boolean = gen.operation(
                        f"call ptr @mule.fast.text.{helper_name}(ptr {lhs}, ptr {rhs})"
                    )
            else:
                boolean = gen.operation(
                    f"call ptr @mule_compare(i32 {opcode}, ptr {lhs}, ptr {rhs})"
                )
            gen.native_result(result, boolean, (lhs, rhs))
            return
        if vop.codegen_info is True and opcode < 6:
            operation = (
                f"call ptr @mule.fast.compare.bool.{opcode}(ptr {lhs}, ptr {rhs})"
            )
            gen.pointer_result(result, operation, (lhs, rhs))
            return
        operation = (
            f"call ptr @mule.fast.compare.{opcode}(ptr {lhs}, ptr {rhs})"
            if opcode < 8
            else f"call ptr @mule_compare(i32 {opcode}, ptr {lhs}, ptr {rhs})"
        )
        gen.pointer_result(result, operation, (lhs, rhs))


for _operator, _opcode in COMPARE_OPCODES.items():
    _define_compare(_operator, _opcode)


# Raw templates: native bodies over machine representations.

RAW_SCALAR = (SIGNED, BOOLEAN, NULL, DOUBLE)


def _raw_binary(operator, opcode):
    @define_vop(
        f"raw-binary-{operator.name.lower()}",
        translate=fndb.BINARY[operator],
        also_translate=(fndb.INPLACE_BINARY[operator],),
        args=((SIGNED,), (SIGNED,)),
        results=((SIGNED,),),
        ltn_policy=RAW,
    )
    def generator(gen, vop):
        left, right = gen.take(vop.args[0]), gen.take(vop.args[1])
        gen.store(vop.results[0], gen.arithmetic(opcode, left, right))


for _operator, _opcode in _RAW_BINARY.items():
    _raw_binary(_operator, _opcode)


def _raw_float_binary(operator, opcode, restriction):
    """Float arithmetic over ``double`` operands, or a mix with one ``i64``
    operand that is converted first, as Python converts the integer."""
    suffix = "-".join(sc.name for sc in restriction)

    @define_vop(
        f"raw-float-{operator.name.lower()}-{suffix}",
        translate=fndb.BINARY[operator],
        also_translate=(fndb.INPLACE_BINARY[operator],),
        args=tuple((sc,) for sc in restriction),
        results=((DOUBLE,),),
        cost=1 if restriction == (DOUBLE, DOUBLE) else 2,
        ltn_policy=RAW,
    )
    def generator(gen, vop):
        left, right = (gen.double(operand) for operand in vop.args)
        gen.store(vop.results[0], gen.float_arithmetic(opcode, left, right))


_MIXED = ((DOUBLE, DOUBLE), (SIGNED, DOUBLE), (DOUBLE, SIGNED))
for _operator, _opcode in _RAW_FLOAT_BINARY.items():
    for _restriction in _MIXED:
        _raw_float_binary(_operator, _opcode, _restriction)


@define_vop(
    "raw-not",
    translate=fndb.UNARY[ast.UnaryOperator.NOT],
    args=((SIGNED, BOOLEAN, DOUBLE),),
    results=((BOOLEAN,),),
    ltn_policy=RAW,
)
def raw_not(gen, vop):
    operand = vop.args[0]
    truth = gen.truth(operand.tn, gen.take(operand))
    gen.store(vop.results[0], gen.operation(f"xor i1 {truth}, true"))


@define_vop(
    "raw-positive",
    translate=fndb.UNARY[ast.UnaryOperator.POSITIVE],
    args=((SIGNED,),),
    results=((SIGNED,),),
    ltn_policy=RAW,
)
def raw_positive(gen, vop):
    gen.store(vop.results[0], gen.take(vop.args[0]))


@define_vop(
    "raw-negative",
    translate=fndb.UNARY[ast.UnaryOperator.NEGATIVE],
    args=((SIGNED,),),
    results=((SIGNED,),),
    ltn_policy=RAW,
)
def raw_negative(gen, vop):
    gen.store(vop.results[0], gen.arithmetic("sub", "0", gen.take(vop.args[0])))


@define_vop(
    "raw-float-positive",
    translate=fndb.UNARY[ast.UnaryOperator.POSITIVE],
    args=((DOUBLE,),),
    results=((DOUBLE,),),
    cost=1,
    ltn_policy=RAW,
)
def raw_float_positive(gen, vop):
    gen.store(vop.results[0], gen.take(vop.args[0]))


@define_vop(
    "raw-float-negative",
    translate=fndb.UNARY[ast.UnaryOperator.NEGATIVE],
    args=((DOUBLE,),),
    results=((DOUBLE,),),
    cost=1,
    ltn_policy=RAW,
)
def raw_float_negative(gen, vop):
    gen.store(vop.results[0], gen.operation(f"fneg double {gen.take(vop.args[0])}"))


@define_vop(
    "raw-invert",
    translate=fndb.UNARY[ast.UnaryOperator.INVERT],
    args=((SIGNED,),),
    results=((SIGNED,),),
    ltn_policy=RAW,
)
def raw_invert(gen, vop):
    value = gen.take(vop.args[0])
    gen.store(vop.results[0], gen.operation(f"xor i64 {value}, -1"))


def _raw_compare(operator, predicate):
    @define_vop(
        f"raw-compare-{operator.name.lower()}",
        translate=fndb.COMPARE[operator],
        args=((SIGNED,), (SIGNED,)),
        results=((BOOLEAN,),),
        ltn_policy=RAW,
    )
    def generator(gen, vop):
        left, right = gen.take(vop.args[0]), gen.take(vop.args[1])
        gen.store(
            vop.results[0], gen.operation(f"icmp {predicate} i64 {left}, {right}")
        )


for _operator, _predicate in _RAW_COMPARE.items():
    _raw_compare(_operator, _predicate)


def _raw_float_compare(operator, predicate):
    @define_vop(
        f"raw-float-compare-{operator.name.lower()}",
        translate=fndb.COMPARE[operator],
        args=((DOUBLE,), (DOUBLE,)),
        results=((BOOLEAN,),),
        cost=1,
        ltn_policy=RAW,
    )
    def generator(gen, vop):
        left, right = (gen.take(operand) for operand in vop.args)
        gen.store(
            vop.results[0], gen.operation(f"fcmp {predicate} double {left}, {right}")
        )


for _operator, _predicate in _RAW_FLOAT_COMPARE.items():
    _raw_float_compare(_operator, _predicate)

# Python compares an integer with a float exactly, never by converting the
# integer, so a mixed comparison orders the pair through a helper that returns
# -1, 0, 1, or 2 (unordered) and tests the code it needs.
_ORDER_TESTS = {
    ast.ComparisonOperator.EQUAL: "icmp eq i32 {code}, 0",
    ast.ComparisonOperator.NOT_EQUAL: "icmp ne i32 {code}, 0",
    ast.ComparisonOperator.LESS: "icmp eq i32 {code}, -1",
    ast.ComparisonOperator.LESS_EQUAL: "icmp sle i32 {code}, 0",
    ast.ComparisonOperator.GREATER: "icmp eq i32 {code}, 1",
    ast.ComparisonOperator.GREATER_EQUAL: "icmp ult i32 {code}, 2",
}
_SWAPPED = {
    ast.ComparisonOperator.EQUAL: ast.ComparisonOperator.EQUAL,
    ast.ComparisonOperator.NOT_EQUAL: ast.ComparisonOperator.NOT_EQUAL,
    ast.ComparisonOperator.LESS: ast.ComparisonOperator.GREATER,
    ast.ComparisonOperator.LESS_EQUAL: ast.ComparisonOperator.GREATER_EQUAL,
    ast.ComparisonOperator.GREATER: ast.ComparisonOperator.LESS,
    ast.ComparisonOperator.GREATER_EQUAL: ast.ComparisonOperator.LESS_EQUAL,
}


def _raw_mixed_compare(operator, restriction):
    integer_first = restriction[0] is SIGNED
    tested = operator if integer_first else _SWAPPED[operator]

    @define_vop(
        f"raw-mixed-compare-{operator.name.lower()}-{restriction[0].name}",
        translate=fndb.COMPARE[operator],
        args=tuple((sc,) for sc in restriction),
        results=((BOOLEAN,),),
        cost=2,
        ltn_policy=RAW,
    )
    def generator(gen, vop):
        left, right = (gen.take(operand) for operand in vop.args)
        integer, double = (left, right) if integer_first else (right, left)
        code = gen.operation(
            f"call i32 @mule.compare.i64.double(i64 {integer}, double {double})"
        )
        gen.store(vop.results[0], gen.operation(_ORDER_TESTS[tested].format(code=code)))


for _operator in _RAW_FLOAT_COMPARE:
    _raw_mixed_compare(_operator, (SIGNED, DOUBLE))
    _raw_mixed_compare(_operator, (DOUBLE, SIGNED))


@define_vop(
    "raw-getattr",
    translate=fndb.GETATTR,
    args=((INSTANCE,),),
    results=((SIGNED,),),
    ltn_policy=RAW,
)
def raw_getattr(gen, vop):
    receiver = vop.args[0]
    address = gen.field_address(receiver.tn, gen.take(receiver), vop.codegen_info.name)
    gen.store(vop.results[0], gen.operation(f"load i64, ptr {address}"))


@define_vop(
    "raw-setattr",
    translate=fndb.SETATTR,
    args=((INSTANCE,), (SIGNED,)),
    ltn_policy=RAW,
)
def raw_setattr(gen, vop):
    receiver, value = vop.args
    address = gen.field_address(receiver.tn, gen.take(receiver), vop.codegen_info.name)
    gen.line(f"store i64 {gen.take(value)}, ptr {address}")


@define_vop(
    "raw-method-attribute",
    translate=fndb.GETATTR,
    args=((INSTANCE,),),
    results=((REFERENCE,),),
    ltn_policy=RAW,
)
def raw_method_attribute(gen, vop):
    value = gen.bind_method(gen.take(vop.args[0]), vop.codegen_info.name)
    gen.line(f"store ptr {value}, ptr {gen.owner_slot(vop.node)}")
    gen.check_pointer(value)
    gen.store(vop.results[0], value)


@define_vop(
    "raw-resolve-method",
    translate=fndb.RESOLVE_METHOD,
    args=((INSTANCE,),),
    results=((INSTANCE,), None),
    ltn_policy=RAW,
)
def raw_resolve_method(gen, vop):
    gen.store(vop.results[0], gen.take(vop.args[0]))


@define_vop(
    "raw-call-resolved",
    translate=fndb.CALL_RESOLVED,
    results=((*RAW_SCALAR, INSTANCE, CLASS, REFERENCE),),
    ltn_policy=RAW,
)
def raw_call_resolved(gen, vop):
    method, _bound, *arguments = vop.args
    target = gen.plan.method_target(vop.node.args[0])
    plan = gen.module.native_plans[target]
    gen.line("store i1 false, ptr %call.error")
    parameters = ["ptr %globals", "ptr %call.error", f"ptr {gen.take(method)}"]
    parameters.extend(
        f"{argument.tn.sc.llvm_type} {gen.take(argument)}" for argument in arguments
    )
    parameters.extend(
        f"ptr {gen.dependencies[variable]}" for variable in plan.dependencies
    )
    result = gen.operation(
        f"call {plan.returned.llvm_type} {plan.entry}({', '.join(parameters)})"
    )
    if plan.returned.owns_result:
        gen.line(f"store ptr {result}, ptr {gen.owner_slot(vop.node)}")
    failed = gen.operation("load i1, ptr %call.error")
    gen.guard(failed)
    gen.store(vop.results[0], result)


@define_vop("raw-construct", results=((INSTANCE,),), ltn_policy=RAW)
def raw_construct(gen, vop):
    """Allocate a native instance through its compiled constructor and keep
    the owning reference in the body's owner slot."""
    callee, *arguments = vop.args
    allocation = gen.plan.allocation(vop.node)
    boxed = [gen.box(argument.tn, gen.take(argument)) for argument in arguments]
    owned_class = gen.operation(f"call ptr @mule.fast.retain(ptr {gen.take(callee)})")
    parameters = [
        "ptr %globals",
        f"ptr {owned_class}",
        *(f"ptr {value}" for value in boxed),
    ]
    body = gen.module.bodies[allocation.layout.protocols["construct"]]
    instance = gen.operation(f"call ptr {body}({', '.join(parameters)})")
    gen.line(f"store ptr {instance}, ptr {gen.owner_slot(vop.node)}")
    gen.check_pointer(instance)
    gen.store(vop.results[0], instance)


@define_vop(
    "raw-min2",
    translate=fndb.MIN2,
    args=((SIGNED,), (SIGNED,)),
    results=((SIGNED,),),
    ltn_policy=RAW,
)
def raw_min2(gen, vop):
    left, right = gen.take(vop.args[0]), gen.take(vop.args[1])
    cmp = gen.operation(f"icmp sgt i64 {left}, {right}")
    gen.store(
        vop.results[0], gen.operation(f"select i1 {cmp}, i64 {right}, i64 {left}")
    )


@define_vop(
    "raw-max2",
    translate=fndb.MAX2,
    args=((SIGNED,), (SIGNED,)),
    results=((SIGNED,),),
    ltn_policy=RAW,
)
def raw_max2(gen, vop):
    left, right = gen.take(vop.args[0]), gen.take(vop.args[1])
    cmp = gen.operation(f"icmp slt i64 {left}, {right}")
    gen.store(
        vop.results[0], gen.operation(f"select i1 {cmp}, i64 {right}, i64 {left}")
    )


@define_vop(
    "raw-float-min2",
    translate=fndb.MIN2,
    args=((DOUBLE,), (DOUBLE,)),
    results=((DOUBLE,),),
    cost=1,
    ltn_policy=RAW,
)
def raw_float_min2(gen, vop):
    left, right = gen.take(vop.args[0]), gen.take(vop.args[1])
    cmp = gen.operation(f"fcmp ogt double {left}, {right}")
    gen.store(
        vop.results[0], gen.operation(f"select i1 {cmp}, double {right}, double {left}")
    )


@define_vop(
    "raw-float-max2",
    translate=fndb.MAX2,
    args=((DOUBLE,), (DOUBLE,)),
    results=((DOUBLE,),),
    cost=1,
    ltn_policy=RAW,
)
def raw_float_max2(gen, vop):
    left, right = gen.take(vop.args[0]), gen.take(vop.args[1])
    cmp = gen.operation(f"fcmp olt double {left}, {right}")
    gen.store(
        vop.results[0], gen.operation(f"select i1 {cmp}, double {right}, double {left}")
    )


@define_vop(
    "raw-math-sqrt",
    translate=fndb.MATH_SQRT,
    args=((DOUBLE, SIGNED),),
    results=((DOUBLE,),),
    cost=1,
    ltn_policy=RAW,
)
def raw_math_sqrt(gen, vop):
    value = gen.double(vop.args[0])
    gen.store(vop.results[0], gen.float_sqrt(value))
