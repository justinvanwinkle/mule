; Ownership/protocol helper `@mule.fast.index_method`.

define internal ptr @mule.fast.index_method(ptr %value, ptr %name) {
entry:
  %bits = ptrtoint ptr %value to i64
  %tag = and i64 %bits, 1
  %integer = icmp ne i64 %tag, 0
  br i1 %integer, label %fixnum, label %slow
fixnum:
  ret ptr @mule.none
slow:
  %result = call ptr @mule_lookup_special(ptr %value, ptr %name)
  ret ptr %result
}
