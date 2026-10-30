; Lazy module literal helper `@mule.cached.float`.

define internal ptr @mule.cached.float(ptr %slot, double %value) {
entry:
  %cached = load ptr, ptr %slot
  %present = icmp ne ptr %cached, null
  br i1 %present, label %ready, label %build, !prof !{!"branch_weights", i32 2000, i32 1}
build:
  %built = call ptr @mule_literal_float(double %value) cold
  store ptr %built, ptr %slot
  br label %ready
ready:
  %result = phi ptr [%cached, %entry], [%built, %build]
  %owned = call ptr @mule.heap.retain(ptr %result)
  ret ptr %owned
}
