; Boxed scalar/integer payload helper `@mule.box.f64` (`mule/compiler/boxing.py`).

define internal nonnull align 8 ptr @mule.box.f64(double %value) {
entry:
  %box = call noalias nonnull align 8 ptr @mule_allocate_object_storage()
  store i64 1, ptr %box
  %tracked = getelementptr i8, ptr %box, i64 8
  store i8 0, ptr %tracked
  %gc.slot = getelementptr i8, ptr %box, i64 12
  store i32 0, ptr %gc.slot
  %kind = getelementptr i8, ptr %box, i64 16
  store i32 3, ptr %kind
  %payload = getelementptr i8, ptr %box, i64 24
  %empty.0 = getelementptr i8, ptr %box, i64 32
  store ptr null, ptr %empty.0
  %empty.1 = getelementptr i8, ptr %box, i64 40
  store ptr null, ptr %empty.1
  store double %value, ptr %payload
  ret ptr %box
}
