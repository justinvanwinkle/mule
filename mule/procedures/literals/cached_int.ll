; Lazy module literal helper `@mule.cached.int`.

define internal ptr @mule.cached.int(ptr %slot, ptr %integer) {
entry:
  %cached = load ptr, ptr %slot
  %present = icmp ne ptr %cached, null
  br i1 %present, label %ready, label %build
build:
  %built = call ptr @mule_literal_integer(ptr %integer)
  %bad = icmp eq ptr %built, null
  br i1 %bad, label %failed, label %built.ok
built.ok:
  store ptr %built, ptr %slot
  br label %ready
ready:
  %result = phi ptr [%cached, %entry], [%built, %built.ok]
  %owned = call ptr @mule.heap.retain(ptr %result)
  ret ptr %owned
failed:
  ret ptr null
}
