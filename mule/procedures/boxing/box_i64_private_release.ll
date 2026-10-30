; Boxed scalar/integer payload helper `@mule.box.i64.private.release` (`mule/compiler/boxing.py`).

define internal void @mule.box.i64.private.release(ptr %box) {
entry:
  %null = icmp eq ptr %box, null
  %bits = ptrtoint ptr %box to i64
  %tag = and i64 %bits, 1
  %immediate = icmp ne i64 %tag, 0
  %skip = or i1 %null, %immediate
  br i1 %skip, label %done, label %release
release:
  %refs = load i64, ptr %box
  %next = sub i64 %refs, 1
  store i64 %next, ptr %box
  %last = icmp eq i64 %next, 0
  br i1 %last, label %free, label %done
free:
  %payload = getelementptr i8, ptr %box, i64 24
  %limb.bits = load i32, ptr @__gmp_bits_per_limb, !invariant.load !{}
  %native.limb = icmp eq i32 %limb.bits, 64
  br i1 %native.limb, label %allocation.check, label %legacy.drop
allocation.check:
  %allocated = load i32, ptr %payload
  %owned = icmp sgt i32 %allocated, 0
  br i1 %owned, label %limb.free, label %object.free
limb.free:
  %limbs.addr = getelementptr i8, ptr %payload, i64 8
  %limbs = load ptr, ptr %limbs.addr
  %capacity = zext i32 %allocated to i64
  %bytes = mul i64 %capacity, 8
  call void @mule_deallocate_integer_limbs(ptr %limbs, i64 %bytes)
  br label %object.free
legacy.drop:
  call void @mule_clear_integer_storage(ptr %payload)
  br label %object.free
object.free:
  call void @mule_deallocate_object_storage(ptr %box)
  br label %done
done:
  ret void
}
