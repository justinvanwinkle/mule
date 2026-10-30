; Cached attribute read helper `@mule.attr.get`.

define internal ptr @mule.attr.get(ptr %receiver, ptr %name, ptr %cache) {
entry:
  %encoded = call i64 @mule.attr.hit(ptr %receiver, ptr %cache)
  %kind = and i64 %encoded, 7
  %hit = icmp eq i64 %kind, 1
  br i1 %hit, label %field, label %slow, !prof !{!"branch_weights", i32 2000, i32 1}
field:
  %slot = call ptr @mule.attr.field(ptr %receiver, i64 %encoded)
  %value = load ptr, ptr %slot
  %unset = icmp eq ptr %value, null
  br i1 %unset, label %slow, label %loaded, !prof !{!"branch_weights", i32 1, i32 2000}
loaded:
  call ptr @mule.fast.retain(ptr %value)
  ret ptr %value
slow:
  %resolved = call ptr @mule_get_attr_cached(ptr %receiver, ptr %name, ptr %cache) cold noinline
  ret ptr %resolved
}
