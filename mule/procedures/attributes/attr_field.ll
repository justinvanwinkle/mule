; Inline attribute/class layout cache helper `@mule.attr.field` (`mule/runtime/object.rs`).

define internal ptr @mule.attr.field(ptr %value, i64 %encoded) {
entry:
  %fields.addr = getelementptr i8, ptr %value, i64 32
  %fields = load ptr, ptr %fields.addr
  %index = lshr i64 %encoded, 3
  %slot = getelementptr ptr, ptr %fields, i64 %index
  ret ptr %slot
}
