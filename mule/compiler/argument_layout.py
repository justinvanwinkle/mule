"""Physical argument-view access for generated external entries.

SBCL's ``init-xep-environment`` verifies argument counts before moving values
from standard argument locations. Mule's entry receives an existing borrowed
``Data::ArgumentView`` instead. Its tag selects that physical layout once;
the signature-specific entry then checks counts and acquires values directly.

The Rust layout test in ``runtime/arguments.rs`` checks these offsets against
real ``repr(C)`` objects. The buffer, count, defaults and closure stay immutable
through an invocation. Only the ownership mask changes as references transfer.
"""

DATA_OFFSET = 16
ARGUMENT_VIEW_TAG = 10
ARGUMENT_VALUES_OFFSET = 24
ARGUMENT_COUNT_OFFSET = 32
ARGUMENT_DEFAULTS_OFFSET = 40
ARGUMENT_CLOSURE_OFFSET = 48
ARGUMENT_OWNED_MASK_OFFSET = 56


def helpers() -> str:
    """LLVM physical primitives, all inlined into generated binding entries.

    ``is_view`` accepts any runtime value. ``positional`` and ``take`` require
    the ordinary-view proof. ``take`` also requires a nonnegative in-range
    index. ``arg_or_default`` permits an omitted nonnegative index and delegates
    missing values to the existing captured-default accessor.
    A set mask bit transfers an existing owned reference exactly once;
    an unset bit, or an index outside the mask's 64 bits, retains the value.
    """
    return f"""define internal i1 @mule.argument_view.is_view(ptr %args) alwaysinline {{
entry:
  %null = icmp eq ptr %args, null
  %bits = ptrtoint ptr %args to i64
  %tag = and i64 %bits, 1
  %immediate = icmp ne i64 %tag, 0
  %invalid = or i1 %null, %immediate
  br i1 %invalid, label %no, label %heap
heap:
  %kind.addr = getelementptr i8, ptr %args, i64 {DATA_OFFSET}
  %kind = load i32, ptr %kind.addr
  %view = icmp eq i32 %kind, {ARGUMENT_VIEW_TAG}
  ret i1 %view
no:
  ret i1 false
}}

define internal i32 @mule.argument_view.positional(ptr %args, i32 %required, i32 %positional, i32 %rest) alwaysinline {{
entry:
  %count.addr = getelementptr i8, ptr %args, i64 {ARGUMENT_COUNT_OFFSET}
  %count = load i64, ptr %count.addr
  %required.wide = zext i32 %required to i64
  %positional.wide = zext i32 %positional to i64
  %enough = icmp uge i64 %count, %required.wide
  %bounded = icmp ule i64 %count, %positional.wide
  %has.rest = icmp ne i32 %rest, 0
  %upper.ok = or i1 %bounded, %has.rest
  %fits = and i1 %enough, %upper.ok
  %narrow = trunc i64 %count to i32
  %result = select i1 %fits, i32 %narrow, i32 -1
  ret i32 %result
}}

define internal ptr @mule.argument_view.take(ptr %args, i32 %index) alwaysinline {{
entry:
  %values.addr = getelementptr i8, ptr %args, i64 {ARGUMENT_VALUES_OFFSET}
  %values = load ptr, ptr %values.addr
  %offset = zext i32 %index to i64
  %slot = getelementptr ptr, ptr %values, i64 %offset
  %value = load ptr, ptr %slot
  %tracked = icmp ult i32 %index, 64
  br i1 %tracked, label %mask, label %borrowed
mask:
  %mask.addr = getelementptr i8, ptr %args, i64 {ARGUMENT_OWNED_MASK_OFFSET}
  %owned = load i64, ptr %mask.addr
  %bit = shl i64 1, %offset
  %selected = and i64 %owned, %bit
  %transfer = icmp ne i64 %selected, 0
  br i1 %transfer, label %owned.value, label %borrowed
owned.value:
  %keep = xor i64 %bit, -1
  %remaining = and i64 %owned, %keep
  store i64 %remaining, ptr %mask.addr
  ret ptr %value
borrowed:
  %retained = call ptr @mule.fast.retain(ptr %value)
  ret ptr %retained
}}

define internal ptr @mule.argument_view.arg_or_default(ptr %args, i32 %index, i32 %default) alwaysinline {{
entry:
  %count.addr = getelementptr i8, ptr %args, i64 {ARGUMENT_COUNT_OFFSET}
  %count = load i64, ptr %count.addr
  %offset = zext i32 %index to i64
  %supplied = icmp ult i64 %offset, %count
  br i1 %supplied, label %argument, label %defaulted
argument:
  %value = call ptr @mule.argument_view.take(ptr %args, i32 %index)
  ret ptr %value
defaulted:
  %captured = call ptr @mule_binding_default_at(ptr %args, i32 %default)
  ret ptr %captured
}}"""
