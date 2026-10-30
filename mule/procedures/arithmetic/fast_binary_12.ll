; Binary arithmetic/bitwise helper `@mule.fast.binary.12` (opcode 12).

define internal ptr @mule.fast.binary.12(ptr %left, ptr %right, i32 %inplace, ptr %reuse, ptr %reuse.slot) {
entry:
  %left.bits = ptrtoint ptr %left to i64
  %right.bits = ptrtoint ptr %right to i64
  %tags = and i64 %left.bits, %right.bits
  %tag = and i64 %tags, 1
  %integers = icmp ne i64 %tag, 0
  br i1 %integers, label %fixnums, label %slow
fixnums:
  %a = ashr i64 %left.bits, 1
  %b = ashr i64 %right.bits, 1
  %result = or i64 %a, %b
  br label %encode
encode:
  %shifted = shl i64 %result, 1
  %tagged = or i64 %shifted, 1
  %value = inttoptr i64 %tagged to ptr
  ret ptr %value
slow:
  %fallback = call ptr @mule_binary_reuse(i32 12, ptr %left, ptr %right, i32 %inplace, ptr null)
  ret ptr %fallback
}
