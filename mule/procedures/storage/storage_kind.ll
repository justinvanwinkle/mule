; Physical sequence (`List`/`Args`) storage helper `@mule.storage.kind` (`mule/runtime/object.rs`).

define internal i32 @mule.storage.kind(ptr %sequence) {
entry:
  %bits = ptrtoint ptr %sequence to i64
  %tag = and i64 %bits, 1
  %fixnum = icmp ne i64 %tag, 0
  %null = icmp eq ptr %sequence, null
  %invalid = or i1 %fixnum, %null
  br i1 %invalid, label %unsupported, label %object, !prof !{!"branch_weights", i32 1, i32 2000}
object:
  %kind.addr = getelementptr i8, ptr %sequence, i64 16
  %kind = load i32, ptr %kind.addr
  ret i32 %kind
unsupported:
  ret i32 0
}
