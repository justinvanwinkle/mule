; Truthiness or binding lookup helper `@mule.fast.local`.

define internal ptr @mule.fast.local(ptr %value, ptr %name) {
entry:
  %missing = icmp eq ptr %value, null
  br i1 %missing, label %slow, label %loaded, !prof !{!"branch_weights", i32 1, i32 2000}
loaded:
  %owned = call ptr @mule.fast.retain(ptr %value)
  ret ptr %owned
slow:
  %error = call ptr @mule_load_local(ptr null, ptr %name) cold
  ret ptr %error
}
