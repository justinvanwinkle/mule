; Inline attribute/class layout cache helper `@mule.class.layout` (`mule/runtime/object.rs`).

define internal i64 @mule.class.layout(ptr %value) {
entry:
  %bits = ptrtoint ptr %value to i64
  %tag = and i64 %bits, 1
  %integer = icmp ne i64 %tag, 0
  %null = icmp eq ptr %value, null
  %skip = or i1 %integer, %null
  br i1 %skip, label %none, label %object, !prof !{!"branch_weights", i32 1, i32 2000}
object:
  %tag.addr = getelementptr i8, ptr %value, i64 16
  %kind = load i32, ptr %tag.addr
  %class = icmp eq i32 %kind, 15
  br i1 %class, label %layout, label %none, !prof !{!"branch_weights", i32 2000, i32 1}
layout:
  %layout.addr = getelementptr i8, ptr %value, i64 24
  %address = load i64, ptr %layout.addr
  ret i64 %address
none:
  ret i64 0
}
