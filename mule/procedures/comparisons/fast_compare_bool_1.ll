; Boolean-context comparison helper `@mule.fast.compare.bool.1` (opcode 1).

define internal ptr @mule.fast.compare.bool.1(ptr %left, ptr %right) {
entry:
  %left.bits = ptrtoint ptr %left to i64
  %right.bits = ptrtoint ptr %right to i64
  %tags = and i64 %left.bits, %right.bits
  %tag = and i64 %tags, 1
  %integers = icmp ne i64 %tag, 0
  br i1 %integers, label %fixnums, label %slow
fixnums:
  %compared = icmp ne i64 %left.bits, %right.bits
  %value = select i1 %compared, ptr @mule.true, ptr @mule.false
  ret ptr %value
slow:
  %fallback = call ptr @mule_compare_bool(i32 1, ptr %left, ptr %right)
  ret ptr %fallback
}
