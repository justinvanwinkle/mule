; Lazy module literal helper `@mule.borrowed.tuple.fn`.

define internal ptr @mule.borrowed.tuple.fn(ptr %slot, ptr %builder) {
entry:
  %cached = load ptr, ptr %slot
  %present = icmp ne ptr %cached, null
  br i1 %present, label %ready, label %build, !prof !{!"branch_weights", i32 2000, i32 1}
build:
  %built = call ptr %builder() cold
  call void @mule.heap.release(ptr %built) cold
  br label %ready
ready:
  %result = phi ptr [%cached, %entry], [%built, %build]
  ret ptr %result
}
