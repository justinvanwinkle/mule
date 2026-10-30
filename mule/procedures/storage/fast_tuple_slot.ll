; Physical sequence (`List`/`Args`) storage helper `@mule.fast.tuple.slot` (`mule/runtime/object.rs`).

define internal ptr @mule.fast.tuple.slot(ptr %tuple, i64 %index) {
entry:
  %value = call ptr @mule.fast.tuple.borrow(ptr %tuple, i64 %index)
  %owned = call ptr @mule.fast.retain(ptr %value)
  ret ptr %owned
}
