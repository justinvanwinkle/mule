; Binary arithmetic/bitwise helper `@mule.fixnum.binary.2` (opcode 2).

define internal ptr @mule.fixnum.binary.2(ptr %left, ptr %right, i32 %inplace, ptr %reuse, ptr %reuse.slot) {
entry:
  %left.bits = ptrtoint ptr %left to i64
  %right.bits = ptrtoint ptr %right to i64
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
  br i1 %fits, label %encode, label %slow
encode:
  %shifted = shl i64 %result, 1
  %tagged = or i64 %shifted, 1
  %value = inttoptr i64 %tagged to ptr
  ret ptr %value
slow:
  %fallback = call ptr @mule_binary_reuse(i32 2, ptr %left, ptr %right, i32 %inplace, ptr null)
  ret ptr %fallback
}
