"""Unicode handling: escapes, non-BMP characters, combining marks, len, ord, chr."""

print("named escapes", "\N{GREEK SMALL LETTER ALPHA}", "\N{SNOWMAN}", "\N{LATIN SMALL LETTER E WITH ACUTE}", ascii("\N{EM DASH}"))
print("u escapes", "\u0041\u00e9\u4e2d", ascii("\u2603"))
print("U escapes", "\U0001F600", ascii("\U0001F600"), "\U00000041")
print("x escapes", "\x41\x7a\xe9", ascii("\x00\x1f\x7f"))
print("octal escapes", "\101\142\60", ascii("\0"), ascii("\7"), ascii("\377"), "\1234" == "S4")
print("simple escapes", ascii("\a\b\f\n\r\t\v\\\'\""))
print("line continuation", "abc\
def")

s = "a\U0001F600b"
print("non-BMP len", len(s), len("\U0001F600"), len("😀👍🏽"))
print("non-BMP ord", ord(s[1]), hex(ord("😀")), chr(0x1F600) == "😀")
print("non-BMP iteration", [hex(ord(c)) for c in "x👍🏽y"])

e1 = "e\u0301"
e2 = "\u00e9"
print("combining", len(e1), len(e2), e1 == e2, ascii(e1), ascii(e2))
import unicodedata
print("normalize", unicodedata.normalize("NFC", e1) == e2, len(unicodedata.normalize("NFD", e2)))
print("unicodedata", unicodedata.name("é"), unicodedata.category("A"), unicodedata.category("\u0301"), unicodedata.lookup("SNOWMAN"))
print("decimal/digit", unicodedata.decimal("٣"), unicodedata.digit("²"), unicodedata.numeric("½"))

print("ord/chr", ord("A"), ord("\n"), ord("€"), chr(65), chr(0), repr(chr(0x10FFFF)), ascii(chr(0x10FFFF)))
for bad in [-1, 0x110000]:
    try:
        chr(bad)
    except ValueError as e:
        print("chr", bad, type(e).__name__)
try:
    ord("ab")
except TypeError as e:
    print("ord multi", type(e).__name__)
try:
    ord("")
except TypeError as e:
    print("ord empty", type(e).__name__)

surrogate = "\ud800"
print("lone surrogate", len(surrogate), ascii(surrogate), ord(surrogate))
print("surrogate pair not combined", len("😀"), "😀" == "😀")
print("string compare by code point", "a" < "b", "Z" < "a", "é" > "z", "😀" > "\uffff", sorted(["b", "A", "é", "a", "😀"]))
print("max/min code points", max("hello"), min("hello"), max("a😀b"))
print("unicode methods", "ÉCOLE".lower(), "ñ".upper(), "Ⅷ".lower(), "ǆ".title())
print("unicode identifiers", "π".isidentifier(), "x²".isidentifier())
π = 3.14
print("unicode variable", π)
