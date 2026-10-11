"""int() conversions: bases 2-36 and 0, prefixes, underscores, whitespace, signs, and errors."""

print("plain", int("42"), int("  -7 "), int("+3"), int("\t10\n"))
print("base 2", int("1011", 2), int("0b1011", 2), int("-0b11", 0))
print("base 8", int("17", 8), int("0o17", 8), int("0o17", 0))
print("base 16", int("ff", 16), int("FF", 16), int("0xff", 16), int("0XfF", 0))
print("base 36", int("z", 36), int("Zz", 36), int("mule", 36))
print("base 0 decimal", int("123", 0), int("0", 0), int("000", 0), int("1_000", 0))
print("underscores", int("1_000_000"), int("0x_ff", 16), int("0b_1_0", 0))
print("base kw", int("10", base=3), int("12", base=5))
print("from float", int(3.9), int(-3.9), int(1e20), int(-0.0))
print("from bool", int(True), int(False))
print("from bytes", int(b"42"), int(bytearray(b"-5")), int(b"ff", 16))
print("no args", int())
print("unicode digits", int("١٢٣"), int("１０"))
print("big", int("9" * 40))

bad_inputs = [
    ("", 10), ("12a", 10), ("1__0", 10), ("_1", 10), ("1_", 10), ("0b2", 0), ("08", 0),
    ("0x", 16), ("2", 2), ("z", 35), ("1.5", 10), (" ", 10), ("- 1", 10), ("1 1", 10),
]
for text, base in bad_inputs:
    try:
        int(text, base)
        print("unexpected ok", repr(text), base)
    except ValueError as e:
        print("bad", repr(text), base, type(e).__name__)
for base in (1, 37, -1):
    try:
        int("1", base)
    except ValueError as e:
        print("bad base", base, type(e).__name__)
try:
    int(5, 10)
except TypeError as e:
    print("base with non string", type(e).__name__)
for value in (float("inf"), float("nan")):
    try:
        int(value)
    except (OverflowError, ValueError) as e:
        print("special float", type(e).__name__)
try:
    int(None)
except TypeError as e:
    print("int None", type(e).__name__)
try:
    int([1])
except TypeError as e:
    print("int list", type(e).__name__)


class HasInt:
    def __int__(self):
        return 7


class HasIndex:
    def __index__(self):
        return 8


class HasTrunc:
    def __trunc__(self):
        return 9


print("dunder int", int(HasInt()), int(HasIndex()))
try:
    print("dunder trunc", int(HasTrunc()))
except TypeError as e:
    print("trunc no longer used", type(e).__name__)


class IntSub(int):
    pass


v = int(IntSub(5))
print("subclass converted", v, type(v).__name__)
print("base with index", int("11", HasIndex()))
print("roundtrip", int(hex(12345), 16), int(bin(-9), 0), int(oct(64), 0))
