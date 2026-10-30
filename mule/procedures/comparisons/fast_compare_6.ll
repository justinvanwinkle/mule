; Object/fixnum comparison helper `@mule.fast.compare.6` (opcode 6).

define internal ptr @mule.fast.compare.6(ptr %left, ptr %right) {
entry:
  %left.bits = ptrtoint ptr %left to i64
  %right.bits = ptrtoint ptr %right to i64
  %compared = icmp eq i64 %left.bits, %right.bits
  %value = select i1 %compared, ptr @mule.true, ptr @mule.false
  ret ptr %value
}
