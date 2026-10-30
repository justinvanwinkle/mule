; Physical sequence (`List`/`Args`) storage helper `@mule.fast.storage.read.borrow` (`mule/runtime/object.rs`).

define internal ptr @mule.fast.storage.read.borrow(ptr %sequence, ptr %position) {
entry:
  %bits = ptrtoint ptr %position to i64
  %tag = and i64 %bits, 1
  %fixnum = icmp ne i64 %tag, 0
  br i1 %fixnum, label %check.kind, label %slow
check.kind:
  %kind = call i32 @mule.storage.kind(ptr %sequence)
  %list = icmp eq i32 %kind, 13
  %tuple = icmp eq i32 %kind, 9
  %supported = or i1 %list, %tuple
  br i1 %supported, label %bounds, label %slow
bounds:
  %length.addr = getelementptr i8, ptr %sequence, i64 32
  %length = load i64, ptr %length.addr
  %index = ashr i64 %bits, 1
  %inside = icmp ult i64 %index, %length
  br i1 %inside, label %read, label %slow
read:
  %data.addr = getelementptr i8, ptr %sequence, i64 24
  %data = load ptr, ptr %data.addr
  %slot = getelementptr ptr, ptr %data, i64 %index
  %value = load ptr, ptr %slot
  ret ptr %value
slow:
  %fallback = call ptr @mule_storage_read(ptr %sequence, ptr %position)
  ret ptr %fallback
}
