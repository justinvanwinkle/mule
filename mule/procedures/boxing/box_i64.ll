; Boxed scalar/integer payload helper `@mule.box.i64` (`mule/compiler/boxing.py`).

define internal ptr @mule.box.i64(i64 %value) {
entry:
  %lower = icmp sge i64 %value, -4611686018427387904
  %upper = icmp sle i64 %value, 4611686018427387903
  %fixnum = and i1 %lower, %upper
  br i1 %fixnum, label %immediate, label %abi.check
immediate:
  %shifted = shl i64 %value, 1
  %tagged = or i64 %shifted, 1
  %encoded = inttoptr i64 %tagged to ptr
  ret ptr %encoded
abi.check:
  %limb.bits = load i32, ptr @__gmp_bits_per_limb, !invariant.load !{}
  %native.limb = icmp eq i32 %limb.bits, 64
  br i1 %native.limb, label %allocate, label %fallback
allocate:
  %box = call noalias nonnull align 8 ptr @mule_allocate_object_storage()
  %limbs = call noalias nonnull align 8 ptr @mule_allocate_integer_limbs(i64 8)
  store i64 1, ptr %box
  %tracked = getelementptr i8, ptr %box, i64 8
  store i8 0, ptr %tracked
  %gc.slot = getelementptr i8, ptr %box, i64 12
  store i32 0, ptr %gc.slot
  %kind = getelementptr i8, ptr %box, i64 16
  store i32 2, ptr %kind
  %payload = getelementptr i8, ptr %box, i64 24
  %empty.0 = getelementptr i8, ptr %box, i64 40
  store ptr null, ptr %empty.0
  %empty.1 = getelementptr i8, ptr %box, i64 48
  store ptr null, ptr %empty.1
  %negative = icmp slt i64 %value, 0
  %negated = sub i64 0, %value
  %magnitude = select i1 %negative, i64 %negated, i64 %value
  store i64 %magnitude, ptr %limbs
  store i32 1, ptr %payload
  %size.addr = getelementptr i8, ptr %payload, i64 4
  %size = select i1 %negative, i32 -1, i32 1
  store i32 %size, ptr %size.addr
  %limbs.addr = getelementptr i8, ptr %payload, i64 8
  store ptr %limbs, ptr %limbs.addr
  ret ptr %box
fallback:
  %legacy = call ptr @mule_box_i64(i64 %value)
  ret ptr %legacy
}
