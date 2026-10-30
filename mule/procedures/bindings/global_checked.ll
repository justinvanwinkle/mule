; Truthiness or binding lookup helper `@mule.global.checked`.

define internal ptr @mule.global.checked(ptr %cell, ptr %name) {
entry:
  %value = load ptr, ptr %cell, !nonnull !{}
  %unbound = icmp eq ptr %value, @mule.undefined
  br i1 %unbound, label %slow, label %bound, !prof !{!"branch_weights", i32 1, i32 2000}
bound:
  %owned = call ptr @mule.fast.retain(ptr %value)
  ret ptr %owned
slow:
  %result = call ptr @mule_load_global(ptr %cell, ptr %name) cold
  ret ptr %result
}
