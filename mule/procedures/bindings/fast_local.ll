; Truthiness or binding lookup helper `@mule.fast.local`.

define internal ptr @mule.fast.local(ptr %value, ptr %name) {
entry:
  %missing = icmp eq ptr %value, null
  br i1 %missing, label %slow, label %loaded
loaded:
  %owned = call ptr @mule.fast.retain(ptr %value)
  ret ptr %owned
slow:
  %error = call ptr @mule_load_local(ptr null, ptr %name)
  ret ptr %error
}
