; Ownership/protocol helper `@mule.fast.integer_storage`.

define internal ptr @mule.fast.integer_storage(ptr %value) {
entry:
  %bits = ptrtoint ptr %value to i64
  %tag = and i64 %bits, 1
  %integer = icmp ne i64 %tag, 0
  br i1 %integer, label %fixnum, label %slow
fixnum:
  ret ptr %value
slow:
  %result = call ptr @mule_integer_storage(ptr %value)
  ret ptr %result
}
