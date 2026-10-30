; Boxed scalar/integer payload helper `@mule.box.i64.private.retain` (`mule/compiler/boxing.py`).

define internal ptr @mule.box.i64.private.retain(ptr %box) {
entry:
  %null = icmp eq ptr %box, null
  %bits = ptrtoint ptr %box to i64
  %tag = and i64 %bits, 1
  %immediate = icmp ne i64 %tag, 0
  %skip = or i1 %null, %immediate
  br i1 %skip, label %done, label %retain
retain:
  %refs = load i64, ptr %box
  %next = add i64 %refs, 1
  store i64 %next, ptr %box
  br label %done
done:
  ret ptr %box
}
