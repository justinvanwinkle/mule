; Known-fixnum unary helper `@mule.fixnum.unary.2` (opcode 2).

define internal ptr @mule.fixnum.unary.2(ptr %operand) {
entry:
  %bits = ptrtoint ptr %operand to i64
  %decoded = ashr i64 %bits, 1
  %result = xor i64 %decoded, -1
  %shifted = shl i64 %result, 1
  %tagged = or i64 %shifted, 1
  %value = inttoptr i64 %tagged to ptr
  ret ptr %value
slow:
  %fallback = call ptr @mule_unary(i32 2, ptr %operand)
  ret ptr %fallback
}
