; Physical `Data::ArgumentView` entry helper `@mule.argument_view.arg_or_default` (`mule/runtime/arguments.rs`).

define internal ptr @mule.argument_view.arg_or_default(ptr %args, i32 %index, i32 %default) {
entry:
  %count.addr = getelementptr i8, ptr %args, i64 32
  %count = load i64, ptr %count.addr
  %offset = zext i32 %index to i64
  %supplied = icmp ult i64 %offset, %count
  br i1 %supplied, label %argument, label %defaulted
argument:
  %value = call ptr @mule.argument_view.take(ptr %args, i32 %index)
  ret ptr %value
defaulted:
  %captured = call ptr @mule_binding_default_at(ptr %args, i32 %default)
  ret ptr %captured
}
