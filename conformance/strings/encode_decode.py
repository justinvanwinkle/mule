"""str.encode and bytes.decode across codecs and error handlers."""

text = "héllo €😀"
for codec in ["utf-8", "utf-16-le", "utf-16-be", "utf-32-le", "latin-1", "ascii", "cp1252"]:
    try:
        enc = text.encode(codec)
        print("encode", codec, enc, len(enc))
    except UnicodeEncodeError as e:
        print("encode", codec, type(e).__name__, e.start, e.end)

for errors in ["strict", "ignore", "replace", "xmlcharrefreplace", "backslashreplace", "namereplace"]:
    try:
        print("ascii errors", errors, text.encode("ascii", errors))
    except UnicodeEncodeError as e:
        print("ascii errors", errors, type(e).__name__)
print("latin-1 surrogateescape fail", end=" ")
try:
    "\udcff".encode("utf-8")
except UnicodeEncodeError as e:
    print(type(e).__name__)
print("surrogateescape roundtrip", "\udcff".encode("utf-8", "surrogateescape"), b"a\xffb".decode("utf-8", "surrogateescape").encode("utf-8", "surrogateescape"))
print("surrogatepass", "\ud800".encode("utf-8", "surrogatepass"))

bad = b"ok\xff\xfe end\xc3"
for errors in ["strict", "ignore", "replace", "backslashreplace", "surrogateescape"]:
    try:
        print("decode errors", errors, ascii(bad.decode("utf-8", errors)))
    except UnicodeDecodeError as e:
        print("decode errors", errors, type(e).__name__, e.start, e.end, e.reason)

print("decode default", b"caf\xc3\xa9".decode(), str(b"caf\xc3\xa9", "utf-8"), str(b"\xe9", "latin-1"))
print("decode keyword", b"\xff".decode(encoding="latin-1"), b"\xff".decode(errors="replace"))
print("bom", "a".encode("utf-16")[:2] in (b"\xff\xfe", b"\xfe\xff"), "a".encode("utf-8-sig"), b"\xef\xbb\xbfa".decode("utf-8-sig"))
print("aliases", "x".encode("UTF8"), "x".encode("utf_8"), "é".encode("Latin1"), "é".encode("iso-8859-1"))
print("roundtrip all", all(text.encode(c).decode(c) == text for c in ["utf-8", "utf-16", "utf-32"]))
try:
    "x".encode("no-such-codec")
except LookupError as e:
    print("unknown codec", type(e).__name__)
print("overlong rejected", end=" ")
try:
    b"\xc0\xaf".decode("utf-8")
except UnicodeDecodeError as e:
    print(type(e).__name__, e.start)
print("truncated 4-byte", ascii(b"\xf0\x9f\x98".decode("utf-8", "replace")))
print("len bytes vs str", len("😀"), len("😀".encode()), len("😀".encode("utf-16-le")))
print("unicode_escape", "é\n".encode("unicode_escape"), b"\\u00e9\\n".decode("unicode_escape"))
print("raw_unicode_escape", "éሴ".encode("raw_unicode_escape"))
import codecs
print("codecs", codecs.encode("abc", "rot13"), codecs.decode(b"616263", "hex"), codecs.lookup("utf8").name)
