; Boxed scalar/integer payload helper `@mule.box.i64.load` (`mule/compiler/boxing.py`).

define internal i64 @mule.box.i64.load(ptr %box) {
entry:
  %output = alloca i64
  %bits = ptrtoint ptr %box to i64
  %tag = and i64 %bits, 1
  %fixnum = icmp ne i64 %tag, 0
  br i1 %fixnum, label %immediate, label %heap
immediate:
  %decoded = ashr i64 %bits, 1
  ret i64 %decoded
heap:
  %kind.addr = getelementptr i8, ptr %box, i64 16
  %kind = load i32, ptr %kind.addr
  %integer = icmp eq i32 %kind, 2
  call void @llvm.assume(i1 %integer)
  br label %abi.check
abi.check:
  %limb.bits = load i32, ptr @__gmp_bits_per_limb, !invariant.load !{}
  %native.limb = icmp eq i32 %limb.bits, 64
  br i1 %native.limb, label %size.check, label %fallback
size.check:
  %payload = getelementptr i8, ptr %box, i64 24
  %size.addr = getelementptr i8, ptr %payload, i64 4
  %size = load i32, ptr %size.addr
  %zero = icmp eq i32 %size, 0
  br i1 %zero, label %empty, label %magnitude
empty:
  ret i64 0
magnitude:
  %positive.unit = icmp eq i32 %size, 1
  %negative.unit = icmp eq i32 %size, -1
  %unit = or i1 %positive.unit, %negative.unit
  call void @llvm.assume(i1 %unit)
  %limbs.addr = getelementptr i8, ptr %payload, i64 8
  %limbs = load ptr, ptr %limbs.addr
  %unsigned = load i64, ptr %limbs
  %negated = sub i64 0, %unsigned
  %signed = select i1 %negative.unit, i64 %negated, i64 %unsigned
  ret i64 %signed
fallback:
  %status = call i32 @mule_unbox_i64(ptr %box, ptr %output)
  %success = icmp eq i32 %status, 0
  call void @llvm.assume(i1 %success)
  %legacy = load i64, ptr %output
  ret i64 %legacy
}
