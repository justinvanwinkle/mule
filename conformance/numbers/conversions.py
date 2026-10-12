"""Conversions among int, float, complex, and strings, including base parsing."""

print("int from float", int(3.9), int(-3.9), int(0.999), int(-0.5), int(1e20))
print("int from str", int("42"), int(" 42 "), int("\t-17\n"), int("+5"), int("1_000"))
print("int base 0", int("0x1f", 0), int("0o17", 0), int("0b101", 0), int("123", 0), int("0", 0))
print("int explicit base", int("ff", 16), int("FF", 16), int("0xff", 16), int("z", 36), int("101", 2))
print("int base 36", int("Zz", 36), int("0b11", 16), int("777", 8))
print("int underscores base", int("1_0", 16), int("0x_ff", 0))
print("int from bool", int(True), int(False))
print("int big str", int("9" * 40))
print("int unicode digits", int("٣٤"), int("１２"))

bad = ["", " ", "1.5", "0x1f", "08", "1__0", "_1", "1_", "abc", "0b2", "--1"]
for s in bad:
    try:
        print("int bad", repr(s), int(s))
    except ValueError as e:
        print("int bad", repr(s), type(e).__name__)
for s, base in [("08", 0), ("10", 1), ("10", 37), ("12", 2)]:
    try:
        print("int bad base", repr(s), base, int(s, base))
    except ValueError as e:
        print("int bad base", repr(s), base, type(e).__name__)
try:
    int(1.5, 10)
except TypeError as e:
    print("int float with base", type(e).__name__)

print("float from str", float("1.5"), float("  -2.5e3 "), float("1e309"), float("-1e309"), float(".5"), float("5."))
print("float specials str", float("inf"), float("-Infinity"), float("nan"), float("1_000.5"))
for s in ["", "1.5.2", "1e", "0x10", "inf1", "1_"]:
    try:
        print("float bad", repr(s), float(s))
    except ValueError as e:
        print("float bad", repr(s), type(e).__name__)
print("float hex parse", float.fromhex("0x1.8p1"))

print("complex ctor", complex(1, 2), complex(1.5), complex("3+4j"), complex("-2j"), complex(" 1-1j "), 0j)
print("complex str forms", complex("j"), complex("1e1+2.5e-1j"), complex("(1+2j)"))
try:
    complex("1 + 2j")
except ValueError as e:
    print("complex spaces", type(e).__name__)
try:
    int(1 + 2j)
except TypeError as e:
    print("int(complex)", type(e).__name__)
try:
    float(3j)
except TypeError as e:
    print("float(complex)", type(e).__name__)

print("str of numbers", str(10), str(-0), str(2.0), str(1j), str(-1j), str(True))
print("bool conversions", bool(0), bool(-1), bool(0.0), bool(0j), bool(1e-300), bool("0"), bool(""))


class HasIndex:
    def __index__(self):
        return 7


class HasInt:
    def __int__(self):
        return 9


class HasFloat:
    def __float__(self):
        return 2.5


print("dunder conversions", int(HasInt()), float(HasFloat()), bin(HasIndex()), [0, 1, 2, 3, 4, 5, 6, 7, 8][HasIndex()])
print("int of index", int(HasIndex()), float(HasIndex()), complex(HasFloat()))
print("mixed arithmetic types", type(1 + 1.0).__name__, type(1 + 1j).__name__, type(1.0 + 1j).__name__, type(True + 1).__name__)
