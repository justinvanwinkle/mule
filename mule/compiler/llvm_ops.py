"""Guarded native operations over Mule's tagged integer representation.

These are LLVM implementation templates, selected separately from language IR.
Unknown object cases retain Python's dynamic protocols through runtime slow paths.
"""

INTRINSICS = """declare {i64, i1} @llvm.sadd.with.overflow.i64(i64, i64)
declare {i64, i1} @llvm.ssub.with.overflow.i64(i64, i64)
declare {i64, i1} @llvm.smul.with.overflow.i64(i64, i64)
@mule.true = external global i8
@mule.false = external global i8
@mule.none = external global i8
"""


def _ownership() -> str:
    functions = []
    for name in ("retain", "release"):
        result = "ptr" if name == "retain" else "void"
        returned = "ret ptr %value" if name == "retain" else "ret void"
        call = (
            "%owned = call ptr @mule_retain(ptr %value)\n  ret ptr %owned"
            if name == "retain"
            else "call void @mule_release(ptr %value)\n  ret void"
        )
        functions.append(f"""define internal {result} @mule.fast.{name}(ptr %value) alwaysinline {{
entry:
  %bits = ptrtoint ptr %value to i64
  %tag = and i64 %bits, 1
  %integer = icmp ne i64 %tag, 0
  %null = icmp eq ptr %value, null
  %true = icmp eq ptr %value, @mule.true
  %false = icmp eq ptr %value, @mule.false
  %none = icmp eq ptr %value, @mule.none
  %a = or i1 %integer, %null
  %b = or i1 %true, %false
  %c = or i1 %a, %b
  %skip = or i1 %c, %none
  br i1 %skip, label %done, label %heap
heap:
  {call}
done:
  {returned}
}}""")
    return "\n\n".join(functions)


def _truth() -> str:
    return """define internal i32 @mule.fast.truth(ptr %value) alwaysinline {
entry:
  %bits = ptrtoint ptr %value to i64
  %tag = and i64 %bits, 1
  %integer = icmp ne i64 %tag, 0
  br i1 %integer, label %fixnum, label %object
fixnum:
  %nonzero = icmp ne i64 %bits, 1
  %truth = zext i1 %nonzero to i32
  ret i32 %truth
object:
  %true = icmp eq ptr %value, @mule.true
  br i1 %true, label %yes, label %other
other:
  %false = icmp eq ptr %value, @mule.false
  %none = icmp eq ptr %value, @mule.none
  %zero = or i1 %false, %none
  br i1 %zero, label %no, label %slow
yes:
  ret i32 1
no:
  ret i32 0
slow:
  %result = call i32 @mule_truth(ptr %value)
  ret i32 %result
}

define internal ptr @mule.fast.local(ptr %value, ptr %name) alwaysinline {
entry:
  %missing = icmp eq ptr %value, null
  br i1 %missing, label %slow, label %loaded
loaded:
  %owned = call ptr @mule.fast.retain(ptr %value)
  ret ptr %owned
slow:
  %error = call ptr @mule_load_local(ptr null, ptr %name)
  ret ptr %error
}

define internal ptr @mule.fast.local.move(ptr %value, ptr %name) alwaysinline {
entry:
  %missing = icmp eq ptr %value, null
  br i1 %missing, label %slow, label %loaded
loaded:
  ret ptr %value
slow:
  %error = call ptr @mule_load_local(ptr null, ptr %name)
  ret ptr %error
}"""


_ENCODE = """  %shifted = shl i64 %result, 1
  %tagged = or i64 %shifted, 1
  %value = inttoptr i64 %tagged to ptr
  ret ptr %value"""
_RANGE = """  %test.shifted = shl i64 %result, 1
  %test.decoded = ashr i64 %test.shifted, 1
  %range = icmp eq i64 %result, %test.decoded"""


def _binary(op: int, *, fixnums: bool = False) -> str:
    if op in (0, 1, 2):
        intrinsic = {0: "sadd", 1: "ssub", 2: "smul"}[op]
        operation = f"""  %checked = call {{i64, i1}} @llvm.{intrinsic}.with.overflow.i64(i64 %a, i64 %b)
  %result = extractvalue {{i64, i1}} %checked, 0
  %overflow = extractvalue {{i64, i1}} %checked, 1
{_RANGE}
  %no.overflow = xor i1 %overflow, true
  %fits = and i1 %range, %no.overflow
  br i1 %fits, label %encode, label %slow"""
    elif op in (4, 5):
        result = "%floor" if op == 4 else "%modulo"
        operation = f"""  %nonzero = icmp ne i64 %b, 0
  br i1 %nonzero, label %divide, label %slow
divide:
  %quotient = sdiv i64 %a, %b
  %remainder = srem i64 %a, %b
  %has.remainder = icmp ne i64 %remainder, 0
  %sign.bits = xor i64 %a, %b
  %different.signs = icmp slt i64 %sign.bits, 0
  %adjust = and i1 %has.remainder, %different.signs
  %correction = zext i1 %adjust to i64
  %floor = sub i64 %quotient, %correction
  %adjusted.remainder = add i64 %remainder, %b
  %modulo = select i1 %adjust, i64 %adjusted.remainder, i64 %remainder
  %result = add i64 {result}, 0
{_RANGE}
  br i1 %range, label %encode, label %slow"""
    elif op == 8:
        operation = f"""  %nonnegative = icmp sge i64 %b, 0
  %bounded = icmp slt i64 %b, 63
  %valid = and i1 %nonnegative, %bounded
  br i1 %valid, label %shift, label %slow
shift:
  %result = shl i64 %a, %b
  %restored = ashr i64 %result, %b
  %no.overflow = icmp eq i64 %restored, %a
{_RANGE}
  %fits = and i1 %range, %no.overflow
  br i1 %fits, label %encode, label %slow"""
    elif op == 9:
        operation = """  %nonnegative = icmp sge i64 %b, 0
  br i1 %nonnegative, label %shift, label %slow
shift:
  %bounded = icmp slt i64 %b, 63
  %count = select i1 %bounded, i64 %b, i64 63
  %result = ashr i64 %a, %count
  br label %encode"""
    else:
        instruction = {10: "and", 11: "xor", 12: "or"}[op]
        operation = f"  %result = {instruction} i64 %a, %b\n  br label %encode"
    prefix = "fixnum" if fixnums else "fast"
    guard = (
        ""
        if fixnums
        else """  %tags = and i64 %left.bits, %right.bits
  %tag = and i64 %tags, 1
  %integers = icmp ne i64 %tag, 0
  br i1 %integers, label %fixnums, label %slow
fixnums:
"""
    )
    return f"""define internal ptr @mule.{prefix}.binary.{op}(ptr %left, ptr %right, i32 %inplace, ptr %reuse) alwaysinline {{
entry:
  %left.bits = ptrtoint ptr %left to i64
  %right.bits = ptrtoint ptr %right to i64
{guard}
  %a = ashr i64 %left.bits, 1
  %b = ashr i64 %right.bits, 1
{operation}
encode:
{_ENCODE}
slow:
  %fallback = call ptr @mule_binary_reuse(i32 {op}, ptr %left, ptr %right, i32 %inplace, ptr %reuse)
  ret ptr %fallback
}}"""


def _unary(op: int, *, fixnum: bool = False) -> str:
    if op == 3:
        return """define internal ptr @mule.fast.unary.3(ptr %operand) alwaysinline {
entry:
  %truth = call i32 @mule.fast.truth(ptr %operand)
  %error = icmp slt i32 %truth, 0
  br i1 %error, label %failed, label %done
failed:
  ret ptr null
done:
  %zero = icmp eq i32 %truth, 0
  %value = select i1 %zero, ptr @mule.true, ptr @mule.false
  ret ptr %value
}"""
    if op == 0:
        operation = "  ret ptr %operand"
    elif op == 1:
        operation = f"""  %minimum = icmp eq i64 %decoded, -4611686018427387904
  br i1 %minimum, label %slow, label %negate
negate:
  %result = sub i64 0, %decoded
{_ENCODE}"""
    else:
        operation = f"  %result = xor i64 %decoded, -1\n{_ENCODE}"
    prefix = "fixnum" if fixnum else "fast"
    guard = (
        ""
        if fixnum
        else """  %tag = and i64 %bits, 1
  %integer = icmp ne i64 %tag, 0
  br i1 %integer, label %fixnum, label %slow
fixnum:
"""
    )
    return f"""define internal ptr @mule.{prefix}.unary.{op}(ptr %operand) alwaysinline {{
entry:
  %bits = ptrtoint ptr %operand to i64
{guard}
  %decoded = ashr i64 %bits, 1
{operation}
slow:
  %fallback = call ptr @mule_unary(i32 {op}, ptr %operand)
  ret ptr %fallback
}}"""


def _compare(op: int) -> str:
    predicate = ("eq", "ne", "slt", "sle", "sgt", "sge", "eq", "ne")[op]
    operation = f"""  %compared = icmp {predicate} i64 %left.bits, %right.bits
  %value = select i1 %compared, ptr @mule.true, ptr @mule.false
  ret ptr %value"""
    if op in (6, 7):
        return f"""define internal ptr @mule.fast.compare.{op}(ptr %left, ptr %right) alwaysinline {{
entry:
  %left.bits = ptrtoint ptr %left to i64
  %right.bits = ptrtoint ptr %right to i64
{operation}
}}"""
    return f"""define internal ptr @mule.fast.compare.{op}(ptr %left, ptr %right) alwaysinline {{
entry:
  %left.bits = ptrtoint ptr %left to i64
  %right.bits = ptrtoint ptr %right to i64
  %tags = and i64 %left.bits, %right.bits
  %tag = and i64 %tags, 1
  %integers = icmp ne i64 %tag, 0
  br i1 %integers, label %fixnums, label %slow
fixnums:
{operation}
slow:
  %fallback = call ptr @mule_compare(i32 {op}, ptr %left, ptr %right)
  ret ptr %fallback
}}"""


def helpers() -> str:
    """Native guarded templates; LLVM removes unused templates after inlining."""
    return "\n\n".join(
        [
            INTRINSICS,
            _ownership(),
            _truth(),
            *(_binary(op) for op in (0, 1, 2, 4, 5, 8, 9, 10, 11, 12)),
            *(_binary(op, fixnums=True) for op in (0, 1, 2, 4, 5, 8, 9, 10, 11, 12)),
            *(_unary(op) for op in range(4)),
            *(_unary(op, fixnum=True) for op in range(3)),
            *(_compare(op) for op in range(8)),
        ]
    )
