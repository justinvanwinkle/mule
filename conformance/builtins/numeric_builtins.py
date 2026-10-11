"""chr, ord, hex, oct, bin, divmod, pow, round, and abs, including edge cases and errors."""

print("chr", chr(65), chr(0x263A), chr(0x10FFFF) == "\U0010ffff", repr(chr(0)))
print("ord", ord("A"), ord("é"), ord("\U0001F600"), ord(b"a"), ord(bytearray(b"z")))
for bad in (lambda: chr(-1), lambda: chr(0x110000), lambda: ord("ab"), lambda: ord(""), lambda: chr(1.0)):
    try:
        bad()
    except (ValueError, TypeError) as e:
        print("chr/ord error", type(e).__name__)

print("hex", hex(255), hex(-255), hex(0), hex(2**64))
print("oct", oct(8), oct(-8), oct(0))
print("bin", bin(5), bin(-5), bin(0), bin(True))


class Idx:
    def __index__(self):
        return 10


print("index protocol", hex(Idx()), bin(Idx()), oct(Idx()))
try:
    hex(1.5)
except TypeError as e:
    print("hex float", type(e).__name__)

print("divmod int", divmod(7, 2), divmod(-7, 2), divmod(7, -2), divmod(-7, -2))
print("divmod float", divmod(7.5, 2), divmod(-7.5, 2))
print("divmod big", divmod(10**30 + 7, 10**15))
try:
    divmod(1, 0)
except ZeroDivisionError as e:
    print("divmod zero", type(e).__name__)

print("pow", pow(2, 10), pow(2, -1), pow(-8, 1 / 3) == (-8) ** (1 / 3), pow(0, 0), pow(0.0, 0))
print("pow mod", pow(3, 4, 5), pow(3, -1, 7), pow(-3, 3, 7), pow(5, 0, 1), pow(38, -1, 97))
print("pow kw", pow(base=2, exp=5), pow(2, 5, mod=7))
print("pow big", pow(7, 100, 13), len(str(pow(3, 300))))
for args in [(2, -1, 4), (2, 3.0, 5), (2, 3, 0)]:
    try:
        pow(*args)
    except (ValueError, TypeError) as e:
        print("pow error", args, type(e).__name__)
try:
    pow(0, -1)
except ZeroDivisionError as e:
    print("pow zero neg", type(e).__name__)
print("pow complex", pow(-1, 0.5).real.__round__(10), type(pow(-1, 0.5)).__name__)

print("round half even", round(0.5), round(1.5), round(2.5), round(-0.5), round(-1.5))
print("round ndigits", round(2.675, 2), round(1.25, 1), round(1.35, 1), round(3.14159, 3))
print("round negative ndigits", round(1234.5, -2), round(1250, -2), round(1350, -2), round(-1250, -2))
print("round int type", type(round(2.5)).__name__, type(round(2.5, 0)).__name__, round(7, 1), type(round(7, None)).__name__)
print("round large", round(1e300, -300), round(123, -5))


class Rounder:
    def __round__(self, ndigits=None):
        return ("rounded", ndigits)


print("custom round", round(Rounder()), round(Rounder(), 2))
for value in (float("inf"), float("nan")):
    try:
        round(value)
    except (OverflowError, ValueError) as e:
        print("round special", type(e).__name__)
print("round special ndigits", round(float("inf"), 2), round(float("nan"), 1))

print("abs", abs(-5), abs(5), abs(-0.0), abs(-2.5), abs(3 + 4j), abs(True), abs(-10**30))


class Absolute:
    def __abs__(self):
        return "abs called"


print("custom abs", abs(Absolute()))
try:
    abs("x")
except TypeError as e:
    print("abs str", type(e).__name__)
