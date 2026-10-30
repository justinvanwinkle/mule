; Inline Latin-1 fast-path text equality helper `@mule.fast.text.eq`.

define internal ptr @mule.fast.text.eq(ptr %lhs, ptr %rhs) {
entry:
  %same.ptr = icmp eq ptr %lhs, %rhs
  br i1 %same.ptr, label %equal, label %check.header
check.header:
  %lh.ptr = getelementptr inbounds i8, ptr %lhs, i64 24
  %rh.ptr = getelementptr inbounds i8, ptr %rhs, i64 24
  %lh = load i16, ptr %lh.ptr, align 8
  %rh = load i16, ptr %rh.ptr, align 8
  %or.h = or i16 %lh, %rh
  %kinds = and i16 %or.h, 255
  %both.inline = icmp eq i16 %kinds, 0
  br i1 %both.inline, label %inline.header, label %slow, !prof !{!"branch_weights", i32 2000, i32 1}
inline.header:
  %same.header = icmp eq i16 %lh, %rh
  br i1 %same.header, label %inline.w0, label %unequal
inline.w0:
  %lw0.ptr = getelementptr inbounds i8, ptr %lhs, i64 32
  %rw0.ptr = getelementptr inbounds i8, ptr %rhs, i64 32
  %lw0 = load i64, ptr %lw0.ptr, align 8
  %rw0 = load i64, ptr %rw0.ptr, align 8
  %eq.w0 = icmp eq i64 %lw0, %rw0
  br i1 %eq.w0, label %check.len, label %unequal
check.len:
  %more = icmp ugt i16 %lh, 2048
  br i1 %more, label %inline.rest, label %equal
inline.rest:
  %lw1.ptr = getelementptr inbounds i8, ptr %lhs, i64 40
  %rw1.ptr = getelementptr inbounds i8, ptr %rhs, i64 40
  %lw1 = load i64, ptr %lw1.ptr, align 8
  %rw1 = load i64, ptr %rw1.ptr, align 8
  %lw2.ptr = getelementptr inbounds i8, ptr %lhs, i64 48
  %rw2.ptr = getelementptr inbounds i8, ptr %rhs, i64 48
  %lw2 = load i64, ptr %lw2.ptr, align 8
  %rw2 = load i64, ptr %rw2.ptr, align 8
  %eq.w1 = icmp eq i64 %lw1, %rw1
  %eq.w2 = icmp eq i64 %lw2, %rw2
  %eq.rest = and i1 %eq.w1, %eq.w2
  %inline.res = select i1 %eq.rest, ptr @mule.true, ptr @mule.false
  ret ptr %inline.res
equal:
  ret ptr @mule.true
unequal:
  ret ptr @mule.false
slow:
  %matched = call i32 @mule_text_eq(ptr %lhs, ptr %rhs) cold
  %cond = icmp ne i32 %matched, 0
  %slow.res = select i1 %cond, ptr @mule.true, ptr @mule.false
  ret ptr %slow.res
}
