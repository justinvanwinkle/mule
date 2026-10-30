; Truthiness or binding lookup helper `@mule.fast.truth`.

define internal i32 @mule.fast.truth(ptr %value) {
entry:
  %bits = ptrtoint ptr %value to i64
  %tag = and i64 %bits, 1
  %integer = icmp ne i64 %tag, 0
  br i1 %integer, label %fixnum, label %object
fixnum:
  %nonzero = icmp ne i64 %bits, 1
  %truth = zext i1 %nonzero to i32
  ret i32 %truth
object:
  %true = icmp eq ptr %value, @mule.true
  br i1 %true, label %yes, label %other
other:
  %false = icmp eq ptr %value, @mule.false
  %none = icmp eq ptr %value, @mule.none
  %zero = or i1 %false, %none
  br i1 %zero, label %no, label %slow
yes:
  ret i32 1
no:
  ret i32 0
slow:
  %result = call i32 @mule_truth(ptr %value) cold
  ret i32 %result
}
