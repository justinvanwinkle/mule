; Truthiness or binding lookup helper `@mule.fast.local.move`.

define internal ptr @mule.fast.local.move(ptr %value, ptr %name) {
entry:
  %missing = icmp eq ptr %value, null
  br i1 %missing, label %slow, label %loaded, !prof !{!"branch_weights", i32 1, i32 2000}
loaded:
  ret ptr %value
slow:
  %error = call ptr @mule_load_local(ptr null, ptr %name) cold
  ret ptr %error
}
