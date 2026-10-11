"""str.translate and str.maketrans in all three forms, plus custom mapping objects."""

t1 = str.maketrans("abc", "xyz")
print("maketrans 2-arg", sorted(t1.items()), "aabbcc-d".translate(t1))
t2 = str.maketrans("ab", "AB", "xyz")
print("maketrans 3-arg", sorted(t2.items()), "abxyzab".translate(t2))
t3 = str.maketrans({"a": "1", "b": None, ord("c"): "CC", "d": 100})
print("maketrans dict", sorted(t3.items()), "abcd".translate(t3))
print("translate with plain dict", "hello".translate({ord("l"): "L", ord("o"): None}))
print("translate to longer", "a-b".translate({ord("-"): " -> "}))
print("translate unicode", "café".translate({ord("é"): "e"}), "x😀y".translate({0x1F600: "smile"}))
print("translate empty", "".translate(t1), "abc".translate({}))
try:
    str.maketrans("abc", "xy")
except ValueError as e:
    print("maketrans unequal", type(e).__name__)
try:
    str.maketrans({"ab": "x"})
except ValueError as e:
    print("maketrans long key", type(e).__name__)
try:
    "a".translate({ord("a"): 3.5})
except TypeError as e:
    print("translate bad value", type(e).__name__)


class Rot13:
    def __getitem__(self, code):
        if ord("a") <= code <= ord("z"):
            return (code - ord("a") + 13) % 26 + ord("a")
        if ord("A") <= code <= ord("Z"):
            return chr((code - ord("A") + 13) % 26 + ord("A"))
        raise LookupError(code)


print("custom mapping", "Hello, World!".translate(Rot13()))
print("custom mapping twice", "Hello, World!".translate(Rot13()).translate(Rot13()))


class Dropper(dict):
    def __missing__(self, key):
        return None if chr(key).isdigit() else key


print("dict subclass missing", "a1b2c3".translate(Dropper()))
vowels = str.maketrans("", "", "aeiouAEIOU")
print("delete vowels", "Programming Language".translate(vowels))
print("bytes maketrans", bytes.maketrans(b"ab", b"AB"), b"abcab".translate(bytes.maketrans(b"ab", b"AB")))
print("bytes translate delete", b"hello world".translate(None, b"lo"))
