"""String comparison, ordering, membership, iteration, and builtins over strings."""

print("eq", "abc" == "abc", "abc" == "ABC", "a" != "b", "" == "")
print("ordering", "abc" < "abd", "ab" < "abc", "" < "a", "B" < "a", "abc" > "ab", "a" <= "a")
print("chained", "a" < "b" < "c", "a" < "c" < "b")
print("sorted", sorted(["banana", "Apple", "cherry", "apple", "", "a"]))
print("sorted key", sorted(["banana", "Apple", "cherry", "apple"], key=str.lower))
print("sorted len", sorted(["ccc", "a", "bb", "aa"], key=len))
print("max/min", max(["x", "xy", "w"]), min("hello world"), max("abc", "abd", key=lambda s: s[-1]))
try:
    print("a" < 1)
except TypeError as e:
    print("str < int", type(e).__name__)
print("str == int", "1" == 1, "1" != 1)

print("in", "ell" in "hello", "" in "abc", "x" not in "abc", "abc" in "abc", "abcd" in "abc")
try:
    1 in "abc"
except TypeError as e:
    print("int in str", type(e).__name__)
print("iteration", list("héllo"), [c for c in "abc" if c != "b"], tuple("xy"))
print("enumerate", list(enumerate("ab", 1)))
print("zip", list(zip("abc", "12")))
print("set of chars sorted", sorted(set("mississippi")))
print("dict from chars", {c: "mississippi".count(c) for c in sorted(set("mississippi"))})
print("len", len(""), len("abc"), len("\n"), len("😀"))
print("reversed", list(reversed("abc")))
print("any/all", any(c.isdigit() for c in "ab1"), all(c.isalpha() for c in "abc"))
print("map ord", list(map(ord, "AZ")))
print("str of things", str(None), str([1, "a"]), str({"k": "v"}), str((1,)), str(b"x"))
print("repr of nested", repr(["a", 'b"', "c'"]))
print("bool", bool(""), bool(" "), bool("0"), bool("False"))
print("identity of equal strings is not checked; equality is", "ab" + "c" == "abc")
print("hash consistent", hash("hello") == hash("hel" + "lo"))
print("ascii", ascii("héllo"), ascii(["é"]))
print("str * in comparison", "ab" * 2 == "abab", "ab" * 2 < "abac")
