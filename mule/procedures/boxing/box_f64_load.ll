; Boxed scalar/integer payload helper `@mule.box.f64.load` (`mule/compiler/boxing.py`).

define internal double @mule.box.f64.load(ptr %box) {
entry:
  %payload = getelementptr i8, ptr %box, i64 24
  %value = load double, ptr %payload
  ret double %value
}
