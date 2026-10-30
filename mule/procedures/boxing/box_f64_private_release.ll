; Boxed scalar/integer payload helper `@mule.box.f64.private.release` (`mule/compiler/boxing.py`).

define internal void @mule.box.f64.private.release(ptr %box) {
entry:
  %skip = icmp eq ptr %box, null
  br i1 %skip, label %done, label %release
release:
  %refs = load i64, ptr %box
  %next = sub i64 %refs, 1
  store i64 %next, ptr %box
  %last = icmp eq i64 %next, 0
  br i1 %last, label %free, label %done
free:
  call void @mule_deallocate_object_storage(ptr %box)
  br label %done
done:
  ret void
}
