; Physical `Data::ArgumentView` entry helper `@mule.argument_view.positional` (`mule/runtime/arguments.rs`).

define internal i32 @mule.argument_view.positional(ptr %args, i32 %required, i32 %positional, i32 %rest) {
entry:
  %count.addr = getelementptr i8, ptr %args, i64 32
  %count = load i64, ptr %count.addr
  %required.wide = zext i32 %required to i64
  %positional.wide = zext i32 %positional to i64
  %enough = icmp uge i64 %count, %required.wide
  %bounded = icmp ule i64 %count, %positional.wide
  %has.rest = icmp ne i32 %rest, 0
  %upper.ok = or i1 %bounded, %has.rest
  %fits = and i1 %enough, %upper.ok
  %narrow = trunc i64 %count to i32
  %result = select i1 %fits, i32 %narrow, i32 -1
  ret i32 %result
}
