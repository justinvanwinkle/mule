; Boxed scalar/integer payload helper `@mule.box.f64.private.retain` (`mule/compiler/boxing.py`).

define internal ptr @mule.box.f64.private.retain(ptr %box) {
entry:
  %skip = icmp eq ptr %box, null
  br i1 %skip, label %done, label %retain
retain:
  %refs = load i64, ptr %box
  %next = add i64 %refs, 1
  store i64 %next, ptr %box
  br label %done
done:
  ret ptr %box
}
