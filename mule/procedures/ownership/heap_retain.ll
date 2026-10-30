; Ownership/protocol helper `@mule.heap.retain`.

define internal ptr @mule.heap.retain(ptr %value) {
entry:
  %null = icmp eq ptr %value, null
  br i1 %null, label %done, label %check
check:
  %refs = load i64, ptr %value
  %immortal = icmp eq i64 %refs, -1
  br i1 %immortal, label %done, label %heap
heap:
  atomicrmw add ptr %value, i64 1 monotonic
  br label %done
done:
  ret ptr %value
}
