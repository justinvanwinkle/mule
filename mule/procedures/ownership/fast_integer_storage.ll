; Ownership/protocol helper `@mule.fast.integer_storage`.

define internal ptr @mule.fast.integer_storage(ptr %value) {
entry:
  %bits = ptrtoint ptr %value to i64
  %tag = and i64 %bits, 1
  %integer = icmp ne i64 %tag, 0
  br i1 %integer, label %fixnum, label %slow, !prof !{!"branch_weights", i32 2000, i32 1}
fixnum:
  ret ptr %value
slow:
  %result = call ptr @mule_integer_storage(ptr %value) cold
  ret ptr %result
}
