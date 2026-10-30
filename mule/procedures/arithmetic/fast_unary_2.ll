; Unary helper `@mule.fast.unary.2` (opcode 2).

define internal ptr @mule.fast.unary.2(ptr %operand) {
entry:
  %bits = ptrtoint ptr %operand to i64
  %tag = and i64 %bits, 1
  %integer = icmp ne i64 %tag, 0
  br i1 %integer, label %fixnum, label %slow, !prof !{!"branch_weights", i32 2000, i32 1}
fixnum:
  %decoded = ashr i64 %bits, 1
  %result = xor i64 %decoded, -1
  %shifted = shl i64 %result, 1
  %tagged = or i64 %shifted, 1
  %value = inttoptr i64 %tagged to ptr
  ret ptr %value
slow:
  %fallback = call ptr @mule_unary(i32 2, ptr %operand) cold
  ret ptr %fallback
}
