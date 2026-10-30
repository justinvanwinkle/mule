; Unary helper `@mule.fast.unary.0` (opcode 0).

define internal ptr @mule.fast.unary.0(ptr %operand) {
entry:
  %bits = ptrtoint ptr %operand to i64
  %tag = and i64 %bits, 1
  %integer = icmp ne i64 %tag, 0
  br i1 %integer, label %fixnum, label %slow
fixnum:
  %decoded = ashr i64 %bits, 1
  ret ptr %operand
slow:
  %fallback = call ptr @mule_unary(i32 0, ptr %operand)
  ret ptr %fallback
}
