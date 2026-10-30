; Exact `i64` vs `double` ordering helper `@mule.compare.i64.double`.

define internal i32 @mule.compare.i64.double(i64 %integer, double %double) {
entry:
  %unordered = fcmp uno double %double, %double
  br i1 %unordered, label %nan, label %ordered
nan:
  ret i32 2
ordered:
  %above = fcmp oge double %double, 0x43E0000000000000
  br i1 %above, label %less, label %bounded.above
less:
  ret i32 -1
bounded.above:
  %below = fcmp olt double %double, 0xC3E0000000000000
  br i1 %below, label %greater, label %bounded
greater:
  ret i32 1
bounded:
  %truncated = fptosi double %double to i64
  %whole = sitofp i64 %truncated to double
  %fraction = fsub double %double, %whole
  %integer.less = icmp slt i64 %integer, %truncated
  %integer.equal = icmp eq i64 %integer, %truncated
  %fraction.positive = fcmp ogt double %fraction, 0.0
  %fraction.zero = fcmp oeq double %fraction, 0.0
  %tie.less = and i1 %integer.equal, %fraction.positive
  %is.less = or i1 %integer.less, %tie.less
  %is.equal = and i1 %integer.equal, %fraction.zero
  %sign = select i1 %is.less, i32 -1, i32 1
  %code = select i1 %is.equal, i32 0, i32 %sign
  ret i32 %code
}
