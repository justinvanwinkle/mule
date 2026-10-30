; Truthiness or binding lookup helper `@mule.fast.local.optional`.

define internal ptr @mule.fast.local.optional(ptr %value) {
entry:
  %missing = icmp eq ptr %value, null
  br i1 %missing, label %unbound, label %loaded, !prof !{!"branch_weights", i32 1, i32 2000}
loaded:
  %owned = call ptr @mule.fast.retain(ptr %value)
  ret ptr %owned
unbound:
  %sentinel = call ptr @mule_binding_missing() cold
  ret ptr %sentinel
}
