; Binary arithmetic/bitwise helper `@mule.fast.binary.8` (opcode 8).

define internal ptr @mule.fast.binary.8(ptr %left, ptr %right, i32 %inplace, ptr %reuse, ptr %reuse.slot) {
entry:
  %left.bits = ptrtoint ptr %left to i64
  %right.bits = ptrtoint ptr %right to i64
  %tags = and i64 %left.bits, %right.bits
  %tag = and i64 %tags, 1
  %integers = icmp ne i64 %tag, 0
  br i1 %integers, label %fixnums, label %slow, !prof !{!"branch_weights", i32 2000, i32 1}
fixnums:
  %a = ashr i64 %left.bits, 1
  %b = ashr i64 %right.bits, 1
  %nonnegative = icmp sge i64 %b, 0
  %bounded = icmp slt i64 %b, 63
  %valid = and i1 %nonnegative, %bounded
  br i1 %valid, label %shift, label %slow, !prof !{!"branch_weights", i32 2000, i32 1}
shift:
  %result = shl i64 %a, %b
  %restored = ashr i64 %result, %b
  %no.overflow = icmp eq i64 %restored, %a
  %test.shifted = shl i64 %result, 1
  %test.decoded = ashr i64 %test.shifted, 1
  %range = icmp eq i64 %result, %test.decoded
  %fits = and i1 %range, %no.overflow
  br i1 %fits, label %encode, label %slow, !prof !{!"branch_weights", i32 2000, i32 1}
encode:
  %shifted = shl i64 %result, 1
  %tagged = or i64 %shifted, 1
  %value = inttoptr i64 %tagged to ptr
  ret ptr %value
slow:
  %fallback = call ptr @mule_binary_reuse(i32 8, ptr %left, ptr %right, i32 %inplace, ptr null) cold
  ret ptr %fallback
}
