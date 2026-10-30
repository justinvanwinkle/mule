; Binary arithmetic/bitwise helper `@mule.fixnum.binary.10` (opcode 10).

define internal ptr @mule.fixnum.binary.10(ptr %left, ptr %right, i32 %inplace, ptr %reuse, ptr %reuse.slot) {
entry:
  %left.bits = ptrtoint ptr %left to i64
  %right.bits = ptrtoint ptr %right to i64
  %a = ashr i64 %left.bits, 1
  %b = ashr i64 %right.bits, 1
  %result = and i64 %a, %b
  br label %encode
encode:
  %shifted = shl i64 %result, 1
  %tagged = or i64 %shifted, 1
  %value = inttoptr i64 %tagged to ptr
  ret ptr %value
slow:
  %fallback = call ptr @mule_binary_reuse(i32 10, ptr %left, ptr %right, i32 %inplace, ptr null) cold
  ret ptr %fallback
}
