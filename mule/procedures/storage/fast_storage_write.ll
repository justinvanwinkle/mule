; Physical sequence (`List`/`Args`) storage helper `@mule.fast.storage.write` (`mule/runtime/object.rs`).

define internal ptr @mule.fast.storage.write(ptr %sequence, ptr %position, ptr %replacement) {
entry:
  %bits = ptrtoint ptr %position to i64
  %tag = and i64 %bits, 1
  %fixnum = icmp ne i64 %tag, 0
  br i1 %fixnum, label %check.kind, label %slow
check.kind:
  %kind = call i32 @mule.storage.kind(ptr %sequence)
  %supported = icmp eq i32 %kind, 13
  br i1 %supported, label %bounds, label %slow
bounds:
  %length.addr = getelementptr i8, ptr %sequence, i64 32
  %length = load i64, ptr %length.addr
  %index = ashr i64 %bits, 1
  %inside = icmp ult i64 %index, %length
  br i1 %inside, label %write, label %slow
write:
  %data.addr = getelementptr i8, ptr %sequence, i64 24
  %data = load ptr, ptr %data.addr
  %slot = getelementptr ptr, ptr %data, i64 %index
  %old = load ptr, ptr %slot
  %owned = call ptr @mule.fast.retain(ptr %replacement)
  store ptr %owned, ptr %slot
  call void @mule_storage_track(ptr %sequence, ptr %replacement)
  call void @mule.fast.release(ptr %old)
  ret ptr @mule.none
slow:
  %fallback = call ptr @mule_storage_write(ptr %sequence, ptr %position, ptr %replacement)
  ret ptr %fallback
}
