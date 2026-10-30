; Truthiness or binding lookup helper `@mule.global.peek.live`.

define internal ptr @mule.global.peek.live(ptr %cell, ptr %name) {
entry:
  %own.slot = getelementptr inbounds {ptr, i64, ptr, ptr}, ptr %cell, i64 0, i32 1
  %own = load i64, ptr %own.slot
  %bound = icmp ne i64 %own, 0
  br i1 %bound, label %module, label %builtin
module:
  %value = load ptr, ptr %cell, !nonnull !{}
  br label %found
builtin:
  %builtin.slot = getelementptr inbounds {ptr, i64, ptr, ptr}, ptr %cell, i64 0, i32 2
  %builtin.cell = load ptr, ptr %builtin.slot
  %builtin.missing = icmp eq ptr %builtin.cell, null
  br i1 %builtin.missing, label %slow, label %builtin.check, !prof !{!"branch_weights", i32 1, i32 2000}
builtin.check:
  %builtin.own.slot = getelementptr inbounds {ptr, i64, ptr, ptr}, ptr %builtin.cell, i64 0, i32 1
  %builtin.own = load i64, ptr %builtin.own.slot
  %builtin.bound = icmp ne i64 %builtin.own, 0
  br i1 %builtin.bound, label %builtin.value, label %slow, !prof !{!"branch_weights", i32 2000, i32 1}
builtin.value:
  %live = load ptr, ptr %builtin.cell, !nonnull !{}
  br label %found
found:
  %result.value = phi ptr [%value, %module], [%live, %builtin.value]
  %owned = call ptr @mule.fast.retain(ptr %result.value)
  ret ptr %owned
slow:
  ret ptr @mule.undefined
}
