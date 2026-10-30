; Ownership/protocol helper `@mule.heap.release`.

define internal void @mule.heap.release(ptr %value) {
entry:
  %null = icmp eq ptr %value, null
  br i1 %null, label %done, label %check
check:
  %refs = load i64, ptr %value
  %immortal = icmp eq i64 %refs, -1
  br i1 %immortal, label %done, label %check_unique
check_unique:
  %unique = icmp eq i64 %refs, 1
  br i1 %unique, label %free, label %heap
heap:
  %prev = atomicrmw sub ptr %value, i64 1 release
  %last = icmp eq i64 %prev, 1
  br i1 %last, label %free, label %done
free:
  call void @mule_destroy(ptr %value)
  ret void
done:
  ret void
}
