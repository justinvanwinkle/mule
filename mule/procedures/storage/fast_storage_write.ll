; Physical sequence (`List`/`RawWords`) storage helper `@mule.fast.storage.write` (`mule/runtime/object.rs`).

define internal ptr @mule.fast.storage.write(ptr %sequence, ptr %position, ptr %replacement) {
entry:
  %bits = ptrtoint ptr %position to i64
  %tag = and i64 %bits, 1
  %fixnum = icmp ne i64 %tag, 0
  br i1 %fixnum, label %check.kind, label %slow, !prof !{!"branch_weights", i32 2000, i32 1}
check.kind:
  %kind = call i32 @mule.storage.kind(ptr %sequence)
  %supported = icmp eq i32 %kind, 13
  br i1 %supported, label %bounds, label %check.words
check.words:
  %words = icmp eq i32 %kind, 28
  br i1 %words, label %words.check, label %slow, !prof !{!"branch_weights", i32 2000, i32 1}
bounds:
  %length.addr = getelementptr i8, ptr %sequence, i64 32
  %length = load i64, ptr %length.addr
  %index = ashr i64 %bits, 1
  %inside = icmp ult i64 %index, %length
  br i1 %inside, label %write, label %slow, !prof !{!"branch_weights", i32 2000, i32 1}
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
words.check:
  %rbits = ptrtoint ptr %replacement to i64
  %rtag = and i64 %rbits, 1
  %rfixnum = icmp ne i64 %rtag, 0
  br i1 %rfixnum, label %words.bounds, label %slow, !prof !{!"branch_weights", i32 2000, i32 1}
words.bounds:
  %wlength.addr = getelementptr i8, ptr %sequence, i64 32
  %wlength = load i64, ptr %wlength.addr
  %windex = ashr i64 %bits, 1
  %winside = icmp ult i64 %windex, %wlength
  br i1 %winside, label %words.write, label %slow, !prof !{!"branch_weights", i32 2000, i32 1}
words.write:
  %wdata.addr = getelementptr i8, ptr %sequence, i64 24
  %wdata = load ptr, ptr %wdata.addr
  %wslot = getelementptr i64, ptr %wdata, i64 %windex
  %word = ashr i64 %rbits, 1
  store i64 %word, ptr %wslot
  ret ptr @mule.none
slow:
  %fallback = call ptr @mule_storage_write(ptr %sequence, ptr %position, ptr %replacement) cold
  ret ptr %fallback
}
