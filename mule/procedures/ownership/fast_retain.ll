; Ownership/protocol helper `@mule.fast.retain`.

define internal ptr @mule.fast.retain(ptr %value) {
entry:
  %bits = ptrtoint ptr %value to i64
  %tag = and i64 %bits, 1
  %integer = icmp ne i64 %tag, 0
  %null = icmp eq ptr %value, null
  %skip = or i1 %integer, %null
  br i1 %skip, label %done, label %check
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
