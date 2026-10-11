"""repr(), str(), and ascii() on scalars and nested data, quote selection, escapes, and custom dunders."""

values = [
    "plain", "it's", 'say "hi"', "both ' and \"", "tab\tnew\nline", "back\\slash", "\x00\x7f", "é", "日本",
    "\U0001F600", "\u200b", "\ud800",
]
for v in values:
    print("repr", repr(v), "ascii", ascii(v))
print("str of str", str("x"), str(""))
print("bytes", repr(b"a'b"), repr(b'a"b'), repr(b"\x00\xff\n"), ascii(b"\xe9"), str(b"x"))
print("bytearray", repr(bytearray(b"hi")))
print("numbers", repr(1.0), repr(1e16), repr(0.1 + 0.2), repr(-0.0), repr(1e-7), repr(2j), repr(-1 - 0j))
print("special", repr(float("inf")), repr(float("nan")), str(float("-inf")))
print("big int", repr(10**30), str(-(2**70)))
print("singletons", repr(None), repr(True), repr(...), repr(NotImplemented))

nested = {"key": ["é", ("t'", b"b")], 1: {2: None}, (1, 2): {3.5}}
print("nested repr", repr(nested))
print("nested str", str(nested))
print("nested ascii", ascii(nested))
print("containers use repr of items", str(["a", 1]), str(("a",)), str({"a"}))


class Both:
    def __repr__(self):
        return "Both<repr>"

    def __str__(self):
        return "Both<str>"


class ReprOnly:
    def __repr__(self):
        return "ReprOnly()é"


class StrOnly:
    def __str__(self):
        return "StrOnly-str"


b = Both()
print("both", repr(b), str(b), ascii(b), [b], f"{b}", f"{b!r}", f"{b!s}", f"{b!a}")
print("repr only", str(ReprOnly()), ascii(ReprOnly()), [ReprOnly()])
print("str only", str(StrOnly()), [StrOnly()][0].__str__())
print("str only repr is default", repr(StrOnly()).startswith("<"))
print("str in list uses repr", "<" in str([StrOnly()]))


class BadRepr:
    def __repr__(self):
        return 5


try:
    repr(BadRepr())
except TypeError as e:
    print("repr non str", type(e).__name__)


class StrSub(str):
    pass


class Sneaky:
    def __str__(self):
        return StrSub("sub")


s = str(Sneaky())
print("str subclass result", s, type(s).__name__)
print("str of type", str(int), repr(str), str(Both).endswith("Both'>"))
print("repr of function name", repr(len))
print("str with encoding", str(b"caf\xc3\xa9", "utf-8"), str(b"\xff", "latin-1"), str(b"\xff", "utf-8", "replace"))
try:
    str(b"\xff", "utf-8")
except UnicodeDecodeError as e:
    print("decode error", type(e).__name__)
print("repr of range slice", repr(range(3)), repr(slice(None)))
print("repr empty", repr([]), repr(()), repr({}), repr(set()), repr(""), repr(b""))
