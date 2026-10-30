; Binary arithmetic/bitwise helper `@mule.fixnum.binary.9` (opcode 9).

define internal ptr @mule.fixnum.binary.9(ptr %left, ptr %right, i32 %inplace, ptr %reuse, ptr %reuse.slot) {
entry:
  %left.bits = ptrtoint ptr %left to i64
  %right.bits = ptrtoint ptr %right to i64
  %a = ashr i64 %left.bits, 1
  %b = ashr i64 %right.bits, 1
  %nonnegative = icmp sge i64 %b, 0
  br i1 %nonnegative, label %shift, label %slow, !prof !{!"branch_weights", i32 2000, i32 1}
shift:
  %bounded = icmp slt i64 %b, 63
  %count = select i1 %bounded, i64 %b, i64 63
  %result = ashr i64 %a, %count
  br label %encode
encode:
  %shifted = shl i64 %result, 1
  %tagged = or i64 %shifted, 1
  %value = inttoptr i64 %tagged to ptr
  ret ptr %value
slow:
  %fallback = call ptr @mule_binary_reuse(i32 9, ptr %left, ptr %right, i32 %inplace, ptr null) cold
  ret ptr %fallback
}
