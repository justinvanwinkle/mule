; Physical sequence (`List`/`Args`) storage helper `@mule.fast.tuple.shape` (`mule/runtime/object.rs`).

define internal i1 @mule.fast.tuple.shape(ptr %value, i64 %arity) {
entry:
  %bits = ptrtoint ptr %value to i64
  %tag = and i64 %bits, 1
  %fixnum = icmp ne i64 %tag, 0
  %null = icmp eq ptr %value, null
  %skip = or i1 %fixnum, %null
  br i1 %skip, label %no, label %object
object:
  %kind.addr = getelementptr i8, ptr %value, i64 16
  %kind = load i32, ptr %kind.addr
  %is.tuple = icmp eq i32 %kind, 9
  br i1 %is.tuple, label %check, label %no
check:
  %class.addr = getelementptr i8, ptr %value, i64 48
  %class = load ptr, ptr %class.addr
  %plain = icmp eq ptr %class, null
  %len.addr = getelementptr i8, ptr %value, i64 32
  %len = load i64, ptr %len.addr
  %len.ok = icmp eq i64 %len, %arity
  %ok = and i1 %plain, %len.ok
  ret i1 %ok
no:
  ret i1 false
}
