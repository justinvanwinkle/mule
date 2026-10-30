; Physical sequence (`List`/`Args`/`RawWords`) storage helper `@mule.fast.storage.length` (`mule/runtime/object.rs`).

define internal ptr @mule.fast.storage.length(ptr %sequence) {
entry:
  %kind = call i32 @mule.storage.kind(ptr %sequence)
  %list = icmp eq i32 %kind, 13
  %tuple = icmp eq i32 %kind, 9
  %words = icmp eq i32 %kind, 28
  %list.or.tuple = or i1 %list, %tuple
  %supported = or i1 %list.or.tuple, %words
  br i1 %supported, label %length, label %slow, !prof !{!"branch_weights", i32 2000, i32 1}
length:
  %length.addr = getelementptr i8, ptr %sequence, i64 32
  %count = load i64, ptr %length.addr
  %shifted = shl i64 %count, 1
  %tagged = or i64 %shifted, 1
  %result = inttoptr i64 %tagged to ptr
  ret ptr %result
slow:
  %fallback = call ptr @mule_storage_length(ptr %sequence) cold
  ret ptr %fallback
}
