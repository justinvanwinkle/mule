; Unary helper `@mule.fast.unary.1` (opcode 1).

define internal ptr @mule.fast.unary.1(ptr %operand) {
entry:
  %bits = ptrtoint ptr %operand to i64
  %tag = and i64 %bits, 1
  %integer = icmp ne i64 %tag, 0
  br i1 %integer, label %fixnum, label %slow, !prof !{!"branch_weights", i32 2000, i32 1}
fixnum:
  %decoded = ashr i64 %bits, 1
  %minimum = icmp eq i64 %decoded, -4611686018427387904
  br i1 %minimum, label %slow, label %negate, !prof !{!"branch_weights", i32 1, i32 2000}
negate:
  %result = sub i64 0, %decoded
  %shifted = shl i64 %result, 1
  %tagged = or i64 %shifted, 1
  %value = inttoptr i64 %tagged to ptr
  ret ptr %value
slow:
  %fallback = call ptr @mule_unary(i32 1, ptr %operand) cold
  ret ptr %fallback
}
