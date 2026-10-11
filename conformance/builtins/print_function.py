"""print() with sep, end, multiple args, no args, None separators, file=sys.stdout, flush, and sys.stdout.write."""

import sys

print()
print("one")
print("a", "b", "c")
print("a", "b", sep="")
print("a", "b", sep=" | ")
print("a", "b", sep=None)
print("no newline", end="")
print(" <- continued")
print("custom end", end="!\n")
print("end none", end=None)
print("x", "y", sep="\n")
print(*[1, 2, 3], sep=", ")
print(*[], sep="never", end="[empty star]\n")
print("mixed types", 1, 2.5, None, True, [1, "a"], {"k": (1,)}, b"by")
print("nested", [[1, [2, [3]]], {"a": {"b": {}}}], ((),))
print("file stdout", file=sys.stdout)
print("file none", file=None)
print("flush", flush=True)
print("", "", sep="-")
print("sep and end", 1, 2, sep="+", end="=3\n")

n = sys.stdout.write("written\n")
print("write return", n)
n2 = sys.stdout.write("")
print("empty write", n2)
n3 = sys.stdout.write("é日\n")
print("write counts chars", n3)
sys.stdout.write("partial ")
print("after write")
for bad in (lambda: print("x", sep=5), lambda: print("x", end=b"\n")):
    try:
        bad()
    except TypeError as e:
        print("bad kw", type(e).__name__)


class Out:
    def __init__(self):
        self.parts = []

    def write(self, s):
        self.parts.append(s)


buf = Out()
print("to", "buffer", sep="_", end=".", file=buf)
print("captured", buf.parts)


class Shown:
    def __str__(self):
        return "Shown.__str__"

    def __repr__(self):
        return "Shown.__repr__"


print(Shown(), [Shown()])
print("unicode", "café", "日本語", "\U0001F600")
print("tab\tsep", "quote'\"")
print(1, end="")
print(2, end="")
print()
