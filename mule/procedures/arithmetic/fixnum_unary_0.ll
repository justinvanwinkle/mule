; Known-fixnum unary helper `@mule.fixnum.unary.0` (opcode 0).

define internal ptr @mule.fixnum.unary.0(ptr %operand) {
entry:
  %bits = ptrtoint ptr %operand to i64
  %decoded = ashr i64 %bits, 1
  ret ptr %operand
slow:
  %fallback = call ptr @mule_unary(i32 0, ptr %operand)
  ret ptr %fallback
}
