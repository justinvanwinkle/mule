; Lazy module literal helper `@mule.cached.tuple`.

define internal ptr @mule.cached.tuple(ptr %slot, i64 %count, ptr %items) {
entry:
  %cached = load ptr, ptr %slot
  %present = icmp ne ptr %cached, null
  br i1 %present, label %ready, label %build, !prof !{!"branch_weights", i32 2000, i32 1}
build:
  %tuple = call ptr @mule_sequence_tuple_owned(i64 %count, ptr %items) cold
  store ptr %tuple, ptr %slot
  %bad = icmp eq ptr %tuple, null
  br i1 %bad, label %failed, label %ready, !prof !{!"branch_weights", i32 1, i32 2000}
ready:
  %result = phi ptr [%cached, %entry], [%tuple, %build]
  %owned = call ptr @mule.heap.retain(ptr %result)
  ret ptr %owned
failed:
  ret ptr null
}
