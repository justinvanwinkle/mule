; Lazy module literal helper `@mule.borrowed.text`.

define internal ptr @mule.borrowed.text(ptr %slot, ptr %data, i64 %length) {
entry:
  %cached = load ptr, ptr %slot
  %present = icmp ne ptr %cached, null
  br i1 %present, label %ready, label %build, !prof !{!"branch_weights", i32 2000, i32 1}
build:
  %built = call ptr @mule_literal_text(ptr %data, i64 %length) cold
  store ptr %built, ptr %slot
  br label %ready
ready:
  %result = phi ptr [%cached, %entry], [%built, %build]
  ret ptr %result
}
