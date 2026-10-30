; Truthiness or binding lookup helper `@mule.global.live`.

define internal ptr @mule.global.live(ptr %cell, ptr %name) {
entry:
  %value = call ptr @mule.global.peek.live(ptr %cell, ptr %name)
  %missing = icmp eq ptr %value, @mule.undefined
  br i1 %missing, label %slow, label %found
found:
  ret ptr %value
slow:
  %result = call ptr @mule_load_global(ptr %cell, ptr %name)
  ret ptr %result
}
