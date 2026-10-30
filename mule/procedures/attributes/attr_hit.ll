; Inline attribute/class layout cache helper `@mule.attr.hit` (`mule/runtime/object.rs`).

define internal i64 @mule.attr.hit(ptr %value, ptr %cache) {
entry:
  %layout = call i64 @mule.attr.layout(ptr %value)
  %cached = load i64, ptr %cache
  %same = icmp eq i64 %layout, %cached
  %known = icmp ne i64 %layout, 0
  %match = and i1 %same, %known
  br i1 %match, label %epoch, label %miss, !prof !{!"branch_weights", i32 2000, i32 1}
epoch:
  %epoch.addr = getelementptr i64, ptr %cache, i64 2
  %cache.epoch = load i64, ptr %epoch.addr
  %current = load atomic i64, ptr @mule_attribute_epoch monotonic, align 8
  %fresh = icmp eq i64 %cache.epoch, %current
  br i1 %fresh, label %hit, label %miss, !prof !{!"branch_weights", i32 2000, i32 1}
hit:
  %encoded.addr = getelementptr i64, ptr %cache, i64 1
  %encoded = load i64, ptr %encoded.addr
  ret i64 %encoded
miss:
  ret i64 0
}
