; Physical sequence (`List`/`Args`) storage helper `@mule.fast.tuple.borrow` (`mule/runtime/object.rs`).

define internal ptr @mule.fast.tuple.borrow(ptr %tuple, i64 %index) {
entry:
  %data.addr = getelementptr i8, ptr %tuple, i64 24
  %data = load ptr, ptr %data.addr
  %slot = getelementptr ptr, ptr %data, i64 %index
  %value = load ptr, ptr %slot
  ret ptr %value
}
