"""Raw strings, implicit literal concatenation, triple quotes, multiplication, and augmented concat."""

print("raw", r"a\nb", len(r"a\nb"), r"\d+\.\w*", r"C:\path\to")
print("raw quote escape", r"a\"b", len(r"a\"b"), r'it\'s', R"UP")
print("raw trailing backslash pair", r"ends\\", len(r"ends\\"))
print("raw unicode escapes untouched", r"\u0041\N{SNOWMAN}", len(r"\u0041"))
print("raw bytes", rb"\x00", br"\n", len(rb"\x00"), Rb"\t")

joined = "abc" 'def' """ghi""" r"\jkl"
print("implicit concat", joined)
print("implicit concat across lines", (
    "first "
    "second "
    f"third {1 + 1} "
    'fourth'
))
print("implicit with raw and normal", r"\n" "\n" == "\\n\n")
print("triple quoted", repr("""line1
line2 "quoted" ''' inner
  indented"""))
print("triple single", repr('''a "b" 'c' d'''))
print("triple escapes", repr("""tab\there\
joined"""))

print("multiply", "ab" * 3, 3 * "ab", "x" * 1, repr("x" * 0), repr("x" * -5), repr("" * 100))
print("multiply bool", "ab" * True, repr("ab" * False))
print("multiply big", len("ab" * 10000))
try:
    "a" * 2.0
except TypeError as e:
    print("multiply float", type(e).__name__)
try:
    "a" + 1
except TypeError as e:
    print("str + int", type(e).__name__)


class Times:
    def __index__(self):
        return 2


print("multiply __index__", "zz" * Times())

s = "a"
alias = s
s += "b"
s *= 2
print("augmented", s, alias)
parts = ""
for i in range(5):
    parts += str(i) * i
print("loop concat", parts)
print("concat empty", "" + "", "x" + "", "" + "y")
print("plus vs join", "a" + "b" + "c" == "".join(["a", "b", "c"]))
print("str() of str subclass", str(type("S", (str,), {})("hi")))
