; Physical `Data::ArgumentView` entry helper `@mule.argument_view.is_view` (`mule/runtime/arguments.rs`).

define internal i1 @mule.argument_view.is_view(ptr %args) {
entry:
  %null = icmp eq ptr %args, null
  %bits = ptrtoint ptr %args to i64
  %tag = and i64 %bits, 1
  %immediate = icmp ne i64 %tag, 0
  %invalid = or i1 %null, %immediate
  br i1 %invalid, label %no, label %heap, !prof !{!"branch_weights", i32 1, i32 2000}
heap:
  %kind.addr = getelementptr i8, ptr %args, i64 16
  %kind = load i32, ptr %kind.addr
  %view = icmp eq i32 %kind, 10
  ret i1 %view
no:
  ret i1 false
}
