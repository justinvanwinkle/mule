"""LLVM-visible payload construction and destruction for private boxes.

SBCL's word/descriptor coercions separate fixnum tagging, bignum allocation,
and digit access (``x86-64/move.lisp``). These codecs expose the corresponding
operations to LLVM while preserving Mule's existing object and GMP layouts.
They describe storage, not Python type dispatch or box/unbox cancellation.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from .vop import BoxLayout


def _header(layout: BoxLayout) -> str:
    """Initialize every active common header and nullable ownership field."""
    pointers = "\n".join(
        f"  %empty.{index} = getelementptr i8, ptr %box, i64 {offset}\n"
        f"  store ptr null, ptr %empty.{index}"
        for index, offset in enumerate(layout.null_pointer_offsets)
    )
    return f"""  store i64 1, ptr %box
  %tracked = getelementptr i8, ptr %box, i64 8
  store i8 0, ptr %tracked
  %gc.slot = getelementptr i8, ptr %box, i64 12
  store i32 0, ptr %gc.slot
  %kind = getelementptr i8, ptr %box, i64 16
  store i32 {layout.tag}, ptr %kind
  %payload = getelementptr i8, ptr %box, i64 {layout.payload_offset}
{pointers}"""


def _private_helpers(layout: BoxLayout, skip: str, cleanup: str) -> str:
    """Local counts are legal only after the whole-group privacy proof."""
    return f"""define internal ptr @{layout.symbol("private.retain")}(ptr %box) alwaysinline {{
entry:
{skip}
  br i1 %skip, label %done, label %retain
retain:
  %refs = load i64, ptr %box
  %next = add i64 %refs, 1
  store i64 %next, ptr %box
  br label %done
done:
  ret ptr %box
}}

define internal void @{layout.symbol("private.release")}(ptr %box) alwaysinline {{
entry:
{skip}
  br i1 %skip, label %done, label %release
release:
  %refs = load i64, ptr %box
  %next = sub i64 %refs, 1
  store i64 %next, ptr %box
  %last = icmp eq i64 %next, 0
  br i1 %last, label %free, label %done
free:
{cleanup}
done:
  ret void
}}"""


class _Payload:
    def helpers(self, layout: BoxLayout) -> str:
        return "\n\n".join(
            (
                self._box(layout),
                self._load(layout),
                _private_helpers(layout, self._skip(), self._cleanup(layout)),
            )
        )

    def _box(self, layout: BoxLayout) -> str:
        raise NotImplementedError

    def _load(self, layout: BoxLayout) -> str:
        raise NotImplementedError

    def _skip(self) -> str:
        raise NotImplementedError

    def _cleanup(self, layout: BoxLayout) -> str:
        raise NotImplementedError


@dataclass(frozen=True, slots=True)
class ScalarPayload(_Payload):
    """One trivially droppable scalar in an otherwise ordinary object."""

    def _box(self, layout: BoxLayout) -> str:
        llvm_type = layout.storage.llvm_type
        return f"""define internal nonnull align 8 ptr @{layout.symbol()}({llvm_type} %value) alwaysinline {{
entry:
  %box = call noalias nonnull align 8 ptr @mule_allocate_object_storage()
{_header(layout)}
  store {llvm_type} %value, ptr %payload
  ret ptr %box
}}"""

    def _load(self, layout: BoxLayout) -> str:
        llvm_type = layout.storage.llvm_type
        return f"""define internal {llvm_type} @{layout.symbol("load")}(ptr %box) alwaysinline {{
entry:
  %payload = getelementptr i8, ptr %box, i64 {layout.payload_offset}
  %value = load {llvm_type}, ptr %payload
  ret {llvm_type} %value
}}"""

    def _skip(self) -> str:
        return "  %skip = icmp eq ptr %box, null"

    def _cleanup(self, layout: BoxLayout) -> str:
        return """  call void @mule_deallocate_object_storage(ptr %box)
  br label %done"""


@dataclass(frozen=True, slots=True)
class IntegerPayload(_Payload):
    """Tagged words or an existing GMP integer with one native-width limb.

    Direct GMP access requires its 64-bit, nail-free limb ABI. The external
    constant guards that path; other GMP builds use the existing conversions.
    Loads require an actual integer within signed 64-bit bounds and no pending
    error. Unknown checked entry boundaries do not use this typed operation.
    """

    def _box(self, layout: BoxLayout) -> str:
        return f"""define internal ptr @{layout.symbol()}(i64 %value) alwaysinline {{
entry:
  %lower = icmp sge i64 %value, -4611686018427387904
  %upper = icmp sle i64 %value, 4611686018427387903
  %fixnum = and i1 %lower, %upper
  br i1 %fixnum, label %immediate, label %abi.check
immediate:
  %shifted = shl i64 %value, 1
  %tagged = or i64 %shifted, 1
  %encoded = inttoptr i64 %tagged to ptr
  ret ptr %encoded
abi.check:
  %limb.bits = load i32, ptr @__gmp_bits_per_limb, !invariant.load !{{}}
  %native.limb = icmp eq i32 %limb.bits, 64
  br i1 %native.limb, label %allocate, label %fallback
allocate:
  %box = call noalias nonnull align 8 ptr @mule_allocate_object_storage()
  %limbs = call noalias nonnull align 8 ptr @mule_allocate_integer_limbs(i64 8)
{_header(layout)}
  %negative = icmp slt i64 %value, 0
  %negated = sub i64 0, %value
  %magnitude = select i1 %negative, i64 %negated, i64 %value
  store i64 %magnitude, ptr %limbs
  store i32 1, ptr %payload
  %size.addr = getelementptr i8, ptr %payload, i64 4
  %size = select i1 %negative, i32 -1, i32 1
  store i32 %size, ptr %size.addr
  %limbs.addr = getelementptr i8, ptr %payload, i64 8
  store ptr %limbs, ptr %limbs.addr
  ret ptr %box
fallback:
  %legacy = call ptr @mule_box_i64(i64 %value)
  ret ptr %legacy
}}"""

    def _load(self, layout: BoxLayout) -> str:
        return f"""define internal i64 @{layout.symbol("load")}(ptr %box) alwaysinline {{
entry:
  %output = alloca i64
  %bits = ptrtoint ptr %box to i64
  %tag = and i64 %bits, 1
  %fixnum = icmp ne i64 %tag, 0
  br i1 %fixnum, label %immediate, label %heap
immediate:
  %decoded = ashr i64 %bits, 1
  ret i64 %decoded
heap:
  %kind.addr = getelementptr i8, ptr %box, i64 16
  %kind = load i32, ptr %kind.addr
  %integer = icmp eq i32 %kind, {layout.tag}
  br i1 %integer, label %abi.check, label %fallback
abi.check:
  %limb.bits = load i32, ptr @__gmp_bits_per_limb, !invariant.load !{{}}
  %native.limb = icmp eq i32 %limb.bits, 64
  br i1 %native.limb, label %size.check, label %fallback
size.check:
  %payload = getelementptr i8, ptr %box, i64 {layout.payload_offset}
  %size.addr = getelementptr i8, ptr %payload, i64 4
  %size = load i32, ptr %size.addr
  %zero = icmp eq i32 %size, 0
  br i1 %zero, label %empty, label %magnitude
empty:
  ret i64 0
magnitude:
  %positive.unit = icmp eq i32 %size, 1
  %negative.unit = icmp eq i32 %size, -1
  %unit = or i1 %positive.unit, %negative.unit
  call void @llvm.assume(i1 %unit)
  %limbs.addr = getelementptr i8, ptr %payload, i64 8
  %limbs = load ptr, ptr %limbs.addr
  %unsigned = load i64, ptr %limbs
  %negated = sub i64 0, %unsigned
  %signed = select i1 %negative.unit, i64 %negated, i64 %unsigned
  ret i64 %signed
fallback:
  %status = call i32 @mule_unbox_i64(ptr %box, ptr %output)
  %success = icmp eq i32 %status, 0
  call void @llvm.assume(i1 %success)
  %legacy = load i64, ptr %output
  ret i64 %legacy
}}"""

    def _skip(self) -> str:
        return """  %null = icmp eq ptr %box, null
  %bits = ptrtoint ptr %box to i64
  %tag = and i64 %bits, 1
  %immediate = icmp ne i64 %tag, 0
  %skip = or i1 %null, %immediate"""

    def _cleanup(self, layout: BoxLayout) -> str:
        return f"""  %payload = getelementptr i8, ptr %box, i64 {layout.payload_offset}
  %limb.bits = load i32, ptr @__gmp_bits_per_limb, !invariant.load !{{}}
  %native.limb = icmp eq i32 %limb.bits, 64
  br i1 %native.limb, label %allocation.check, label %legacy.drop
allocation.check:
  %allocated = load i32, ptr %payload
  %owned = icmp sgt i32 %allocated, 0
  br i1 %owned, label %limb.free, label %object.free
limb.free:
  %limbs.addr = getelementptr i8, ptr %payload, i64 8
  %limbs = load ptr, ptr %limbs.addr
  %capacity = zext i32 %allocated to i64
  %bytes = mul i64 %capacity, 8
  call void @mule_deallocate_integer_limbs(ptr %limbs, i64 %bytes)
  br label %object.free
legacy.drop:
  call void @mule_clear_integer_storage(ptr %payload)
  br label %object.free
object.free:
  call void @mule_deallocate_object_storage(ptr %box)
  br label %done"""
