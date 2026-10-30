; Inline attribute/class layout cache helper `@mule.fast.isinstance.1` (`mule/runtime/object.rs`).

define internal ptr @mule.fast.isinstance.1(ptr %value, ptr %class) {
entry:
  %bits = ptrtoint ptr %value to i64
  %tag = and i64 %bits, 1
  %integer = icmp ne i64 %tag, 0
  %null = icmp eq ptr %value, null
  %skip = or i1 %integer, %null
  br i1 %skip, label %slow, label %object, !prof !{!"branch_weights", i32 1, i32 2000}
object:
  %tag.addr = getelementptr i8, ptr %value, i64 16
  %kind = load i32, ptr %tag.addr
  %kind.off = sub i32 %kind, 16
  %is.inst = icmp ult i32 %kind.off, 2
  br i1 %is.inst, label %inst, label %slow, !prof !{!"branch_weights", i32 2000, i32 1}
inst:
  %actual.addr = getelementptr i8, ptr %value, i64 24
  %actual = load ptr, ptr %actual.addr
  %exact = icmp eq ptr %actual, %class
  br i1 %exact, label %hit, label %slow, !prof !{!"branch_weights", i32 2000, i32 1}
hit:
  ret ptr @mule.true
slow:
  %res = call ptr @mule_isinstance_single(ptr %value, ptr %class) cold
  ret ptr %res
}
