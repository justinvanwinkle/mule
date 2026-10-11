"""str.split/rsplit with maxsplit, partition/rpartition, splitlines, and join."""

s = "  a  b c   d  "
print("split default", s.split())
print("split default maxsplit", s.split(maxsplit=1), s.split(None, 2))
print("rsplit default maxsplit", s.rsplit(maxsplit=1), s.rsplit(None, 2))
print("split sep space", s.split(" "))
print("split sep", "a,b,,c,".split(","), ",".split(","), "".split(","), "".split())
print("split multi-char sep", "a::b::::c".split("::"), "abcabc".split("bc"))
print("split maxsplit", "a,b,c,d".split(",", 2), "a,b,c,d".rsplit(",", 2), "a,b,c".split(",", 0), "a,b".split(",", -1))
print("split keyword", "x-y-z".split(sep="-", maxsplit=1), "x-y-z".rsplit(sep="-", maxsplit=1))
print("split whitespace kinds", "a\tb\nc\x0bd\x0ce\rf\u3000g\x1ch".split())
try:
    "abc".split("")
except ValueError as e:
    print("split empty sep", type(e).__name__)

print("partition", "key=value=x".partition("="), "novalue".partition("="), "=lead".partition("="))
print("rpartition", "key=value=x".rpartition("="), "novalue".rpartition("="))
print("partition multi", "a->b->c".partition("->"), "a->b->c".rpartition("->"))
try:
    "abc".partition("")
except ValueError as e:
    print("partition empty", type(e).__name__)

text = "line1\nline2\r\nline3\rline4\x0bline5\x1c\u2028end\n\n"
print("splitlines", text.splitlines())
print("splitlines keepends", text.splitlines(keepends=True))
print("splitlines edge", "".splitlines(), "\n".splitlines(), "x".splitlines(True), "a\n\nb".splitlines())
print("split newline vs splitlines", "a\nb\n".split("\n"), "a\nb\n".splitlines())

print("join list", "-".join(["a", "b", "c"]), "".join(["x", "y"]), ", ".join([]))
print("join generator", "|".join(str(i) for i in range(5)))
print("join string iterable", ".".join("abc"), "/".join(("p", "q")))
print("join dict keys", "+".join({"k1": 1, "k2": 2}))
print("join single", "SEP".join(["only"]))
try:
    ",".join(["a", 1])
except TypeError as e:
    print("join non-str", type(e).__name__)
words = "the quick brown fox".split()
print("roundtrip", " ".join(words[::-1]), "_".join(w.upper() for w in words if len(w) > 3))
head, sep, tail = "user@example.com".partition("@")
print("unpack partition", head, sep, tail)
print("split then join count", len("a b  c".split(" ")), "|".join("a b  c".split(" ")))
