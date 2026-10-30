; Physical sequence (`List`/`Args`/`RawWords`) storage helper `@mule.fast.storage.read.borrow` (`mule/runtime/object.rs`).

define internal ptr @mule.fast.storage.read.borrow(ptr %sequence, ptr %position) {
entry:
  %bits = ptrtoint ptr %position to i64
  %tag = and i64 %bits, 1
  %fixnum = icmp ne i64 %tag, 0
  br i1 %fixnum, label %check.kind, label %slow, !prof !{!"branch_weights", i32 2000, i32 1}
check.kind:
  %kind = call i32 @mule.storage.kind(ptr %sequence)
  %list = icmp eq i32 %kind, 13
  %tuple = icmp eq i32 %kind, 9
  %words = icmp eq i32 %kind, 28
  %list.or.tuple = or i1 %list, %tuple
  br i1 %list.or.tuple, label %bounds, label %check.words
check.words:
  br i1 %words, label %words.bounds, label %slow, !prof !{!"branch_weights", i32 2000, i32 1}
bounds:
  %length.addr = getelementptr i8, ptr %sequence, i64 32
  %length = load i64, ptr %length.addr
  %index = ashr i64 %bits, 1
  %inside = icmp ult i64 %index, %length
  br i1 %inside, label %read, label %slow, !prof !{!"branch_weights", i32 2000, i32 1}
read:
  %data.addr = getelementptr i8, ptr %sequence, i64 24
  %data = load ptr, ptr %data.addr
  %slot = getelementptr ptr, ptr %data, i64 %index
  %value = load ptr, ptr %slot
  ret ptr %value
words.bounds:
  %wlength.addr = getelementptr i8, ptr %sequence, i64 32
  %wlength = load i64, ptr %wlength.addr
  %windex = ashr i64 %bits, 1
  %winside = icmp ult i64 %windex, %wlength
  br i1 %winside, label %words.read, label %slow, !prof !{!"branch_weights", i32 2000, i32 1}
words.read:
  %wdata.addr = getelementptr i8, ptr %sequence, i64 24
  %wdata = load ptr, ptr %wdata.addr
  %wslot = getelementptr i64, ptr %wdata, i64 %windex
  %word = load i64, ptr %wslot
  %shifted = shl i64 %word, 1
  %tagged = or i64 %shifted, 1
  %wvalue = inttoptr i64 %tagged to ptr
  ret ptr %wvalue
slow:
  %fallback = call ptr @mule_storage_read(ptr %sequence, ptr %position) cold
  ret ptr %fallback
}
