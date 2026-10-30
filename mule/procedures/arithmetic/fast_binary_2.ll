; Binary arithmetic/bitwise helper `@mule.fast.binary.2` (opcode 2).

define internal ptr @mule.fast.binary.2(ptr %left, ptr %right, i32 %inplace, ptr %reuse, ptr %reuse.slot) {
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
  %checked = call {i64, i1} @llvm.smul.with.overflow.i64(i64 %a, i64 %b)
  %result = extractvalue {i64, i1} %checked, 0
  %overflow = extractvalue {i64, i1} %checked, 1
  %test.shifted = shl i64 %result, 1
  %test.decoded = ashr i64 %test.shifted, 1
  %range = icmp eq i64 %result, %test.decoded
  %no.overflow = xor i1 %overflow, true
  %fits = and i1 %range, %no.overflow
  br i1 %fits, label %encode, label %slow, !prof !{!"branch_weights", i32 2000, i32 1}
encode:
  %shifted = shl i64 %result, 1
  %tagged = or i64 %shifted, 1
  %value = inttoptr i64 %tagged to ptr
  ret ptr %value
slow:
  %has.slot = icmp ne ptr %reuse.slot, null
  br i1 %has.slot, label %slow.store, label %slow.call
slow.store:
  store ptr %reuse, ptr %reuse.slot
  br label %slow.call
slow.call:
  %fallback = call ptr @mule_binary_reuse(i32 2, ptr %left, ptr %right, i32 %inplace, ptr %reuse.slot) cold
  ret ptr %fallback
}
