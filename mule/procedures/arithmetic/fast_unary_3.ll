; Unary helper `@mule.fast.unary.3` (opcode 3).

define internal ptr @mule.fast.unary.3(ptr %operand) {
entry:
  %truth = call i32 @mule.fast.truth(ptr %operand)
  %error = icmp slt i32 %truth, 0
  br i1 %error, label %failed, label %done, !prof !{!"branch_weights", i32 1, i32 2000}
done:
  %zero = icmp eq i32 %truth, 0
  %value = select i1 %zero, ptr @mule.true, ptr @mule.false
  ret ptr %value
failed:
  ret ptr null
}
