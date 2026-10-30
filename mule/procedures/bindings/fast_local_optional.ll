; Truthiness or binding lookup helper `@mule.fast.local.optional`.

define internal ptr @mule.fast.local.optional(ptr %value) {
entry:
  %missing = icmp eq ptr %value, null
  br i1 %missing, label %unbound, label %loaded
loaded:
  %owned = call ptr @mule.fast.retain(ptr %value)
  ret ptr %owned
unbound:
  %sentinel = call ptr @mule_binding_missing()
  ret ptr %sentinel
}
