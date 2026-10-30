; Lazy module literal helper `@mule.borrowed.float`.

define internal ptr @mule.borrowed.float(ptr %slot, double %value) {
entry:
  %cached = load ptr, ptr %slot
  %present = icmp ne ptr %cached, null
  br i1 %present, label %ready, label %build
build:
  %built = call ptr @mule_literal_float(double %value)
  store ptr %built, ptr %slot
  br label %ready
ready:
  %result = phi ptr [%cached, %entry], [%built, %build]
  ret ptr %result
}
