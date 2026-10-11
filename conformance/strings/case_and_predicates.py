"""Case conversion methods and is* predicates, including non-ASCII behavior."""

samples = ["hello world", "HELLO", "they're bill's friends", "ß straße", "ǅungla", "ΣΑΣ", "İstanbul", "a1b2 c3", "", "ﬁne", "x_y-z"]
for s in samples:
    print(repr(s), "upper", repr(s.upper()), "lower", repr(s.lower()))
    print("   ", "title", repr(s.title()), "capitalize", repr(s.capitalize()), "swapcase", repr(s.swapcase()), "casefold", repr(s.casefold()))

print("len change", len("ß".upper()), len("İ".lower()), len("ﬁ".upper()))
print("casefold compare", "Straße".casefold() == "STRASSE".casefold(), "Straße".lower() == "STRASSE".lower())
print("swapcase twice", "ß".swapcase().swapcase())

preds = ["isalpha", "isdigit", "isdecimal", "isnumeric", "isalnum", "isspace", "isupper", "islower", "istitle", "isidentifier", "isprintable", "isascii"]
cases = ["abc", "ABC", "Abc Def", "abc123", "123", "", " \t\n", "½", "²", "٣", "Ⅷ", "_id", "1abc", "with space", "tab\t", "café", "\u00a0", "Ab1", "class", "ǅ"]
for c in cases:
    flags = "".join("1" if getattr(c, p)() else "0" for p in preds)
    print("preds", repr(c), flags)
print("pred order", " ".join(preds))

import keyword
print("identifier vs keyword", "class".isidentifier(), keyword.iskeyword("class"), "match".isidentifier(), keyword.issoftkeyword("match"))
print("upper/lower of mixed digits", "abc123DEF".upper(), "abc123DEF".lower(), "123".isupper(), "A1".isupper())
