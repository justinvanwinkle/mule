"""strip variants, replace with count, find/rfind/index/rindex/count with ranges, startswith/endswith."""

s = "xxhello worldxx"
print("strip chars", s.strip("x"), s.lstrip("x"), s.rstrip("x"), s.strip("xd"), s.strip("hxo"))
print("strip default", repr("  \t\n hi \r\n ".strip()), repr("\u3000hi\u00a0".strip()), repr("  x ".lstrip()), repr(" x  ".rstrip()))
print("strip None", "  a ".strip(None), "".strip(), "xxx".strip("x"))

r = "a-b-c-d"
print("replace", r.replace("-", "+"), r.replace("-", "+", 2), r.replace("-", "+", 0), r.replace("-", "+", -1))
print("replace empty old", "abc".replace("", "|"), "abc".replace("", "|", 2), "".replace("", "X"))
print("replace to empty", "banana".replace("a", ""), "banana".replace("na", "NA"), "aaaa".replace("aa", "b"))
print("replace keyword count", "aaa".replace("a", "b", count=2))
print("replace missing", "abc".replace("z", "y"))

h = "hello hello hello"
print("find", h.find("llo"), h.find("llo", 3), h.find("llo", 3, 8), h.find("llo", 3, 9), h.find("zzz"))
print("find negative range", h.find("hello", -5), h.find("hello", -100), h.find("o", -3, -1))
print("find empty", h.find(""), h.find("", 5), h.find("", 100), "".find(""), h.find("", len(h)))
print("rfind", h.rfind("hello"), h.rfind("hello", 0, 10), h.rfind("z"), h.rfind(""), h.rfind("l", -4))
print("index", h.index("o"), h.rindex("o"), h.index("hello", 1))
for label, thunk in [("index missing", lambda: h.index("zz")), ("rindex range", lambda: h.rindex("hello", 0, 4))]:
    try:
        print(label, thunk())
    except ValueError as e:
        print(label, type(e).__name__)
print("count", h.count("l"), h.count("ll"), h.count("l", 5), h.count("l", 5, 9), h.count(""), "aaaa".count("aa"), "".count(""))
print("count negative", h.count("hello", -11), h.count("l", -100, -3))
print("in operator", "lo h" in h, "" in "", "" in "x", "HELLO" in h)

print("startswith", h.startswith("hell"), h.startswith("ello", 1), h.startswith("hello", 6, 10), h.startswith(""))
print("startswith tuple", h.startswith(("x", "he")), h.startswith(("x", "y")), h.startswith(()))
print("endswith", h.endswith("llo"), h.endswith("hello", 0, 11), h.endswith(("a", "lo")), h.endswith("o", -1))
print("startswith past end", "abc".startswith("", 3), "abc".startswith("", 4), "abc".endswith("", 5))
try:
    h.startswith(["he"])
except TypeError as e:
    print("startswith list", type(e).__name__)

print("removeprefix", "prefix_body".removeprefix("prefix_"), "body".removeprefix("x"), "aaa".removeprefix("a"), "x".removeprefix(""))
print("removesuffix", "file.tar.gz".removesuffix(".gz"), "file".removesuffix(".gz"), "aaa".removesuffix("aa"))
print("strip vs removesuffix", "test.py.py".rstrip(".py"), "test.py.py".removesuffix(".py"))
