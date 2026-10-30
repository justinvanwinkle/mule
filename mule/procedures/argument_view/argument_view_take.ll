; Physical `Data::ArgumentView` entry helper `@mule.argument_view.take` (`mule/runtime/arguments.rs`).

define internal ptr @mule.argument_view.take(ptr %args, i32 %index) {
entry:
  %values.addr = getelementptr i8, ptr %args, i64 24
  %values = load ptr, ptr %values.addr
  %offset = zext i32 %index to i64
  %slot = getelementptr ptr, ptr %values, i64 %offset
  %value = load ptr, ptr %slot
  %tracked = icmp ult i32 %index, 64
  br i1 %tracked, label %mask, label %borrowed
mask:
  %mask.addr = getelementptr i8, ptr %args, i64 56
  %owned = load i64, ptr %mask.addr
  %bit = shl i64 1, %offset
  %selected = and i64 %owned, %bit
  %transfer = icmp ne i64 %selected, 0
  br i1 %transfer, label %owned.value, label %borrowed
owned.value:
  %keep = xor i64 %bit, -1
  %remaining = and i64 %owned, %keep
  store i64 %remaining, ptr %mask.addr
  ret ptr %value
borrowed:
  %retained = call ptr @mule.fast.retain(ptr %value)
  ret ptr %retained
}
