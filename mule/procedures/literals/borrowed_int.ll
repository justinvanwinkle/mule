; Lazy module literal helper `@mule.borrowed.int`.

define internal ptr @mule.borrowed.int(ptr %slot, ptr %integer) {
entry:
  %cached = load ptr, ptr %slot
  %present = icmp ne ptr %cached, null
  br i1 %present, label %ready, label %build
build:
  %built = call ptr @mule_literal_integer(ptr %integer)
  store ptr %built, ptr %slot
  br label %ready
ready:
  %result = phi ptr [%cached, %entry], [%built, %build]
  ret ptr %result
}
