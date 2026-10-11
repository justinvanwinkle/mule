"""center/ljust/rjust/zfill padding and expandtabs."""

for w in [0, 3, 6, 7, 10]:
    print("pad", w, repr("abcd".center(w)), repr("abcd".ljust(w)), repr("abcd".rjust(w)))
print("center odd", repr("ab".center(5)), repr("abc".center(6)), repr("a".center(4, "*")), repr("".center(3, "-")))
print("center fill", "x".center(7, "="), "hi".ljust(6, "."), "hi".rjust(6, "0"))
try:
    "x".center(5, "ab")
except TypeError as e:
    print("multi-char fill", type(e).__name__)
print("negative width", repr("abc".center(-5)), repr("abc".ljust(-1)))

print("zfill", "42".zfill(5), "-42".zfill(5), "+42".zfill(5), "abc".zfill(5), "".zfill(3), "12345".zfill(2), "-".zfill(3), "+-1".zfill(5))
print("zfill unicode digit", "٣".zfill(3))

tabbed = "a\tbc\tdef\tghij\tk"
print("expandtabs default", repr(tabbed.expandtabs()))
print("expandtabs 4", repr(tabbed.expandtabs(4)))
print("expandtabs 1", repr(tabbed.expandtabs(1)))
print("expandtabs 0", repr(tabbed.expandtabs(0)))
print("expandtabs negative", repr(tabbed.expandtabs(-3)))
print("expandtabs newline resets", repr("ab\tc\nx\ty\r\tz".expandtabs(4)))
print("expandtabs keyword", repr("\t".expandtabs(tabsize=3)))

rows = [("name", "qty", "price"), ("apple", "3", "1.25"), ("kiwi", "12", "0.5")]
for name, qty, price in rows:
    print("|" + name.ljust(8) + "|" + qty.rjust(4) + "|" + price.center(7) + "|")
print("non-ascii width", repr("é".center(5, "·")), repr("😀".ljust(3, "_")))
