; Inline attribute/class layout cache helper `@mule.attr.layout` (`mule/runtime/object.rs`).

@mule_attribute_epoch = external global i64
define internal i64 @mule.attr.layout(ptr %value) {
entry:
  %bits = ptrtoint ptr %value to i64
  %tag = and i64 %bits, 1
  %integer = icmp ne i64 %tag, 0
  %null = icmp eq ptr %value, null
  %skip = or i1 %integer, %null
  br i1 %skip, label %none, label %object
object:
  %tag.addr = getelementptr i8, ptr %value, i64 16
  %kind = load i32, ptr %tag.addr
  %instance = icmp eq i32 %kind, 16
  br i1 %instance, label %fields, label %none
fields:
  %attrs.addr = getelementptr i8, ptr %value, i64 48
  %attrs = load ptr, ptr %attrs.addr
  %plain = icmp eq ptr %attrs, null
  br i1 %plain, label %owner, label %none
owner:
  %class.addr = getelementptr i8, ptr %value, i64 24
  %class = load ptr, ptr %class.addr
  %layout.addr = getelementptr i8, ptr %class, i64 24
  %layout = load i64, ptr %layout.addr
  ret i64 %layout
none:
  ret i64 0
}
