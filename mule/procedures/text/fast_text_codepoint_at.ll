; Inline Latin-1 and heap Latin-1 fast path for indexed str codepoint reads.
;
; Layout of Data::Text (TEXT_TAG = 4 at byte offset 16):
;   offset 24: u8 Kind discriminant (0 = InlineLatin1, 1 = Latin1, 2 = Ucs2, 3 = Ucs4)
;   Kind::InlineLatin1 (0):
;     offset 25: u8 length (0..=24)
;     offset 32: [u8; 24] inline Latin-1 bytes
;   Kind::Latin1 (1):
;     offset 40: ptr Vec<u8> data pointer
;     offset 48: i64 Vec<u8> byte/character length
;
; For non-Latin-1 strings (UCS-2/UCS-4), negative or out-of-bounds indices, or
; non-text receivers, delegates to @mule_text_codepoint_at.

define internal ptr @mule.fast.text.codepoint.at(ptr %value, i64 %index) alwaysinline {
entry:
  %null = icmp eq ptr %value, null
  br i1 %null, label %slow, label %nonnull, !prof !{!"branch_weights", i32 1, i32 2000}
nonnull:
  %bits = ptrtoint ptr %value to i64
  %tag = and i64 %bits, 1
  %fixnum = icmp ne i64 %tag, 0
  br i1 %fixnum, label %slow, label %heap, !prof !{!"branch_weights", i32 1, i32 2000}
heap:
  %kind.addr = getelementptr i8, ptr %value, i64 16
  %data.kind = load i32, ptr %kind.addr
  %is.text = icmp eq i32 %data.kind, 4
  br i1 %is.text, label %text, label %slow, !prof !{!"branch_weights", i32 2000, i32 1}
text:
  %repr.addr = getelementptr i8, ptr %value, i64 24
  %repr = load i8, ptr %repr.addr
  switch i8 %repr, label %slow [
    i8 0, label %inline.latin1
    i8 1, label %heap.latin1
  ]
inline.latin1:
  %inline.len.addr = getelementptr i8, ptr %value, i64 25
  %inline.len8 = load i8, ptr %inline.len.addr
  %inline.len = zext i8 %inline.len8 to i64
  %inline.data = getelementptr i8, ptr %value, i64 32
  br label %check
heap.latin1:
  %heap.data.addr = getelementptr i8, ptr %value, i64 40
  %heap.data = load ptr, ptr %heap.data.addr
  %heap.len.addr = getelementptr i8, ptr %value, i64 48
  %heap.len = load i64, ptr %heap.len.addr
  br label %check
check:
  %data = phi ptr [%inline.data, %inline.latin1], [%heap.data, %heap.latin1]
  %len = phi i64 [%inline.len, %inline.latin1], [%heap.len, %heap.latin1]
  %in.bounds = icmp ult i64 %index, %len
  br i1 %in.bounds, label %read, label %slow, !prof !{!"branch_weights", i32 2000, i32 1}
read:
  %slot = getelementptr i8, ptr %data, i64 %index
  %byte = load i8, ptr %slot
  %code = zext i8 %byte to i64
  %shifted = shl nuw nsw i64 %code, 1
  %tagged = or disjoint i64 %shifted, 1
  %res = inttoptr i64 %tagged to ptr
  ret ptr %res
slow:
  %slow.res = call ptr @mule_text_codepoint_at(ptr %value, i64 %index)
  ret ptr %slow.res
}
