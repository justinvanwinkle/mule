; Truthiness or binding lookup helper `@mule.global.unchecked`.

; A cell never holds NULL: an unbound name's cell holds the undefined object.
define internal ptr @mule.global.unchecked(ptr %cell, ptr %name) {
entry:
  %value = load ptr, ptr %cell, !nonnull !{}
  %owned = call ptr @mule.fast.retain(ptr %value)
  ret ptr %owned
}
