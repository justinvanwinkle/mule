; Cached method resolution helper `@mule.attr.method`.

define internal ptr @mule.attr.method(ptr %receiver, ptr %name, ptr %cache, ptr %out) {
entry:
  %encoded = call i64 @mule.attr.hit(ptr %receiver, ptr %cache)
  %kind = and i64 %encoded, 7
  %hit = icmp eq i64 %kind, 2
  br i1 %hit, label %matched, label %slow, !prof !{!"branch_weights", i32 2000, i32 1}
matched:
  %cached.addr = getelementptr i64, ptr %cache, i64 3
  %cached = load ptr, ptr %cached.addr
  call ptr @mule.heap.retain(ptr %cached)
  call ptr @mule.heap.retain(ptr %receiver)
  store ptr %receiver, ptr %out
  ret ptr %cached
slow:
  %resolved = call ptr @mule_resolve_method(ptr %receiver, ptr %name, ptr %cache, ptr %out) cold noinline
  ret ptr %resolved
}
