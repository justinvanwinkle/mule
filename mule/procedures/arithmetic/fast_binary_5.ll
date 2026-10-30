; Binary arithmetic/bitwise helper `@mule.fast.binary.5` (opcode 5).

define internal ptr @mule.fast.binary.5(ptr %left, ptr %right, i32 %inplace, ptr %reuse, ptr %reuse.slot) {
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
  %nonzero = icmp ne i64 %b, 0
  br i1 %nonzero, label %divide, label %slow
divide:
  %remainder = srem i64 %a, %b
  %has.remainder = icmp ne i64 %remainder, 0
  %sign.bits = xor i64 %a, %b
  %different.signs = icmp slt i64 %sign.bits, 0
  %adjust = and i1 %has.remainder, %different.signs
  %adjusted.remainder = add i64 %remainder, %b
  %result = select i1 %adjust, i64 %adjusted.remainder, i64 %remainder
  br label %encode
encode:
  %shifted = shl i64 %result, 1
  %tagged = or i64 %shifted, 1
  %value = inttoptr i64 %tagged to ptr
  ret ptr %value
slow:
  %fallback = call ptr @mule_binary_reuse(i32 5, ptr %left, ptr %right, i32 %inplace, ptr null)
  ret ptr %fallback
}
