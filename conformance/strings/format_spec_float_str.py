"""Format-spec mini language for floats and strings: precision, e/f/g/%, z, #, grouping, fill and align."""

vals = [0.0, -0.0, 1.0, -1.5, 3.14159265, 1234567.891, 1e-5, 1e16, 2.5, 0.125, float("inf"), float("-inf"), float("nan")]
for v in vals:
    print("float", repr(v), format(v, ""), format(v, "f"), format(v, ".2f"), format(v, "e"), format(v, ".3E"), format(v, "g"), format(v, "G"), format(v, ".2%"))

print("precision 0", format(2.5, ".0f"), format(3.5, ".0f"), format(2.5, ".0e"), format(2.5, "#.0f"), format(2.5, "#.0e"))
print("g details", format(123456789.0, "g"), format(0.0001234, "g"), format(1.0, "g"), format(1.0, "#g"), format(100.0, ".2g"), format(1e-5, ".3g"))
print("no type", format(1.0, ".3"), format(1234.5678, ".3"), format(1e20, ".3"), format(0.1, ".1"))
print("n type", format(1234.5, "n"), format(1234567.0, ".10n"))
print("grouping", format(1234567.891, ",.2f"), format(1234567.891, "_.1f"), format(-1234.5, "012,.1f"), format(1e6, ",e"))
print("sign", format(1.5, "+"), format(-1.5, " "), format(1.5, " .1f"), format(float("nan"), "+"), format(float("inf"), " "))
print("z option", format(-0.0, "z"), format(-0.0, "zf"), format(-0.001, "z.1f"), format(-0.001, ".1f"), format(-0.001, "+z.1f"), format(-0.0, "z.0e"))
print("zero pad", format(-3.5, "08.2f"), format(3.5, "+08.2f"), format(float("inf"), "08"), format(-1e-3, "010.2e"))
print("align fill", format(2.5, "*^10"), format(2.5, "<8") + "|", format(-2.5, "=+8"), format(-2.5, "x=8"))
print("percent", format(0.5, "%"), format(1, ".0%"), format(0.12345, ".1%"), format(-0.005, ".0%"))
print("large precision", format(0.1, ".20f"), format(1 / 3, ".17g"), format(2**60 + 0.0, ".0f"))
print("big float f", format(1e22, "f"), format(1.5e300, ".0e"))
print("int-valued round", format(0.5, ".0f"), format(1.5, ".0f"), format(2.675, ".2f"))

for v in ["hello", "", "é😀", "a" * 12]:
    print("str", repr(format(v, "")), repr(format(v, "<8")), repr(format(v, ">8")), repr(format(v, "^8")), repr(format(v, ".3")), repr(format(v, "*^9.2")))
print("str s type", format("x", "s"), format("abc", ">5s"), format("abc", "_<5"))
for spec in ["d", "=5", "+", ",", "#", "z"]:
    try:
        print("str error spec", repr(spec), format("abc", spec))
    except ValueError as e:
        print("str error spec", repr(spec), type(e).__name__)
print("str zero pad allowed", repr(format("ab", "05")), repr(format("ab", "<05")))
for spec in ["d", "x", "c", "b"]:
    try:
        print("float error", spec, format(1.5, spec))
    except ValueError as e:
        print("float error", spec, type(e).__name__)
print("complex specs", format(1 + 2j, ".1f"), format(-1.5j, "+.2e"), format(1 + 2j, ">16"), format(1j, ""))
print("None format", format(None, ""), f"{None!s:>6}")
try:
    format(None, ">6")
except TypeError as e:
    print("None with spec", type(e).__name__)
