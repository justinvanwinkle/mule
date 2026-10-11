"""Format-spec mini language for integers: fill, align, sign, #, 0, width, grouping, and type codes."""

values = [0, 7, -7, 255, -255, 1234567, 2**70]
for v in values:
    print("types", v, format(v, "d"), format(v, "b"), format(v, "o"), format(v, "x"), format(v, "X"), format(v, "n"))
    print("  alt", format(v, "#b"), format(v, "#o"), format(v, "#x"), format(v, "#X"))
    print("  grouping", format(v, ","), format(v, "_"), format(v, "_b"), format(v, "_x"), format(v, "#_o"))

print("align", repr(format(42, "<6")), repr(format(42, ">6")), repr(format(42, "^6")), repr(format(42, "=6")), repr(format(42, "6")))
print("fill", format(42, "*<6"), format(42, "*>6"), format(42, "*^7"), format(-42, "*=7"), format(42, "0=6"))
print("sign", format(5, "+"), format(-5, "+"), format(5, "-"), format(5, " "), format(-5, " "), format(0, "+"))
print("zero pad", format(42, "06"), format(-42, "06"), format(42, "+06"), format(42, "#010x"), format(-42, "#010b"))
print("zero pad grouping", format(1234, "08,"), format(1234, "09_"), format(-1234567, "012,d"))
print("width less than len", format(123456, "3"), format(-1, "#1x"))
print("char c", format(65, "c"), format(0x1F600, "c"), format(10, "^5c") + "|")
print("float types on int", format(5, "f"), format(5, ".2e"), format(5, "g"), format(5, "%"), format(12345678, ".3g"))
print("bool format", format(True, "d"), format(True, "#x"), format(True, ">5"), format(True, ""), format(False, "f"))
print("fill special chars", format(1, "{^5"), format(1, "}^5"), format(1, "0<5"), format(1, ":>3"))
print("f-string forms", f"{255:#06x}", f"{-3:+04}", f"{10**9:,}", f"{9:08b}")
print("non-ascii fill", format(7, "é^5"), format(7, "😀>3"))
for spec in ["s", ".2d", ",_", "_,", "=s", "z"]:
    try:
        print("error spec", repr(spec), format(10, spec))
    except ValueError as e:
        print("error spec", repr(spec), type(e).__name__)
try:
    format(-1, "c")
except OverflowError as e:
    print("negative c", type(e).__name__)
print("n locale-free", format(1234567, "n"), format(-1234567, "08n"))
print("str() vs format()", str(42) == format(42), format(42, "") == "42")
