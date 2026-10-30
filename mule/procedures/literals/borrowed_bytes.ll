; Lazy module literal helper `@mule.borrowed.bytes`.

define internal ptr @mule.borrowed.bytes(ptr %slot, ptr %data, i64 %length) {
entry:
  %cached = load ptr, ptr %slot
  %present = icmp ne ptr %cached, null
  br i1 %present, label %ready, label %build
build:
  %built = call ptr @mule_literal_bytes(ptr %data, i64 %length)
  store ptr %built, ptr %slot
  br label %ready
ready:
  %result = phi ptr [%cached, %entry], [%built, %build]
  ret ptr %result
}
