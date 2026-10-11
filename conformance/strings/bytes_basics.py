"""bytes literals, indexing returning int, slicing, methods, hex/fromhex, and comparisons."""

b = b"hello\x00\xffworld"
print("literal", b, len(b), type(b).__name__)
print("index returns int", b[0], b[-1], b[5], b[6], type(b[0]).__name__)
print("slice returns bytes", b[1:4], b[::-1], b[::3], b[100:], type(b[1:2]).__name__)
print("iteration", list(b"abc"), [x for x in b"\x01\x02"])
print("in", b"ell" in b, 104 in b, 0 in b, 255 in b)
try:
    300 in b
except ValueError as e:
    print("int 300 in bytes", type(e).__name__)
try:
    "a" in b
except TypeError as e:
    print("str in bytes", type(e).__name__)
print("ctor", bytes(3), bytes([65, 66, 67]), bytes(range(5)), bytes(b"x"), bytes(), bytes("é", "utf-8"))
try:
    bytes([256])
except ValueError as e:
    print("byte out of range", type(e).__name__)
try:
    bytes("abc")
except TypeError as e:
    print("str without encoding", type(e).__name__)

print("concat/repeat", b"ab" + b"cd", b"ab" * 3, b"x" * 0, bytes(2) + b"z")
try:
    b"a" + "b"
except TypeError as e:
    print("bytes + str", type(e).__name__)
print("compare", b"abc" == b"abc", b"abc" < b"abd", b"" < b"\x00", b"a" == "a", b"\x80" > b"\x7f")
print("methods", b"a,b,c".split(b","), b"  x ".strip(), b"abc".upper(), b"ABC".lower(), b"hello".replace(b"l", b"L", 1))
print("find/count", b"banana".find(b"an"), b"banana".rfind(b"an"), b"banana".count(b"a"), b"banana".find(97), b"banana".index(b"n"))
print("startswith", b"hello".startswith(b"he"), b"hello".endswith((b"x", b"lo")))
print("join", b"-".join([b"a", b"b", bytearray(b"c")]))
print("partition", b"k=v".partition(b"="), b"a.b.c".rsplit(b".", 1))
print("predicates", b"abc".isalpha(), b"123".isdigit(), b"ab1".isalnum(), b" \t".isspace(), b"Ab".istitle(), b"\xe9".isalpha())
print("pad", b"ab".center(6, b"*"), b"7".zfill(3), b"ab".ljust(4), b"ab".rjust(4, b"."))
print("hex", b"\x00\xab\xff".hex(), b"\xde\xad\xbe\xef".hex(":"), b"\xde\xad\xbe\xef".hex("-", 2), b"\x01\x02\x03".hex(" ", -2))
print("fromhex", bytes.fromhex("deadbeef"), bytes.fromhex("de ad  be ef"), bytes.fromhex(""))
try:
    bytes.fromhex("abc")
except ValueError as e:
    print("fromhex odd", type(e).__name__)
print("repr escapes", repr(b"\t\n\r\\'\""), repr(b"'"), repr(b"\x7f\x80"), repr(b'"\''))
print("str of bytes", str(b"ab"))
print("removeprefix", b"prefix-x".removeprefix(b"prefix-"), b"x.gz".removesuffix(b".gz"))
print("expandtabs", b"a\tb".expandtabs(4), b"a\nb\r\nc".splitlines(), b"a\nb".splitlines(True))
print("hash/dict", {b"k": 1}[b"k"], b"k" in {b"k"})
print("bytes of int list from str", bytes(map(ord, "hi")))
print("isascii", b"abc".isascii(), b"\x80".isascii())
try:
    b"abc"[0] = 1
except TypeError as e:
    print("bytes immutable", type(e).__name__)
print("percent format", b"%s-%d-%x-%r" % (b"ab", 5, 255, b"q"), b"%c%c" % (65, b"B"))
print("memoryview", list(memoryview(b"ab")), memoryview(b"abc")[1:].tobytes())
