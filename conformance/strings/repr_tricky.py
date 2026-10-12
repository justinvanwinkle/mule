"""repr() of strings: quote selection, escapes for control and non-printable characters, unicode."""

cases = [
    "plain",
    "it's",
    'say "hi"',
    "both ' and \"",
    "back\\slash",
    "tab\tnewline\ncr\r",
    "\x00\x01\x1f\x7f",
    "\x80\x9f\xa0\xad",
    "é ñ 中 😀",
    "\u200b\u200e\u2028\u2029",
    "\ufeff\ufffd\uffff",
    "\U000e0001\U0010ffff",
    "\ud800",
    "",
    "'",
    '"',
    "'\"",
    "\\'",
    "\a\b\v\f",
    "\u0301",
    "a\u0301",
    "\u00ad",
    "\u3000",
]
for c in cases:
    print("repr", repr(c), "ascii", ascii(c), "len", len(c))

print("repr of repr", repr(repr("a'b")))
print("eval roundtrip", all(eval(repr(c)) == c for c in cases))
print("containers use repr", ["a\nb", ("'",), {"k\t": '"'}])
print("str vs repr", "a\nb" == "a\nb", repr("a\nb"))
print("%r", "%r" % ("x'y",), "%a" % ("é",))
print("f-string !r", f"{'q\"q'!r}", f"{'é'!a}")
print("format !r", "{!r}".format("z\0"))
