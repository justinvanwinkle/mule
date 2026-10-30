; Binary arithmetic/bitwise helper `@mule.fixnum.binary.4` (opcode 4).

define internal ptr @mule.fixnum.binary.4(ptr %left, ptr %right, i32 %inplace, ptr %reuse, ptr %reuse.slot) {
entry:
  %left.bits = ptrtoint ptr %left to i64
  %right.bits = ptrtoint ptr %right to i64
  %a = ashr i64 %left.bits, 1
  %b = ashr i64 %right.bits, 1
  %nonzero = icmp ne i64 %b, 0
  br i1 %nonzero, label %divide, label %slow
divide:
  %quotient = sdiv i64 %a, %b
  %remainder = srem i64 %a, %b
  %has.remainder = icmp ne i64 %remainder, 0
  %sign.bits = xor i64 %a, %b
  %different.signs = icmp slt i64 %sign.bits, 0
  %adjust = and i1 %has.remainder, %different.signs
  %correction = zext i1 %adjust to i64
  %result = sub i64 %quotient, %correction
  %test.shifted = shl i64 %result, 1
  %test.decoded = ashr i64 %test.shifted, 1
  %range = icmp eq i64 %result, %test.decoded
  br i1 %range, label %encode, label %slow
encode:
  %shifted = shl i64 %result, 1
  %tagged = or i64 %shifted, 1
  %value = inttoptr i64 %tagged to ptr
  ret ptr %value
slow:
  %fallback = call ptr @mule_binary_reuse(i32 4, ptr %left, ptr %right, i32 %inplace, ptr null)
  ret ptr %fallback
}
