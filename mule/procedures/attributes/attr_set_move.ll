; Cached attribute move-write helper `@mule.attr.set.move`.

define internal i32 @mule.attr.set.move(ptr %receiver, ptr %name, ptr %value, ptr %cache) {
entry:
  %encoded = call i64 @mule.attr.hit(ptr %receiver, ptr %cache)
  %kind = and i64 %encoded, 7
  %hit = icmp eq i64 %kind, 1
  br i1 %hit, label %field, label %slow, !prof !{!"branch_weights", i32 2000, i32 1}
field:
  %slot = call ptr @mule.attr.field(ptr %receiver, i64 %encoded)
  %old = load ptr, ptr %slot
  store ptr %value, ptr %slot
  call void @mule.fast.release(ptr %old)
  ret i32 0
slow:
  %status = call i32 @mule_set_attr_cached(ptr %receiver, ptr %name, ptr %value, ptr %cache) cold noinline
  call void @mule.fast.release(ptr %value) cold noinline
  ret i32 %status
}
