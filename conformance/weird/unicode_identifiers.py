"""Non-ASCII identifiers: NFKC normalization of names, non-ASCII classes, functions, keyword arguments and attributes."""

import unicodedata

ﬁ = 1
print("ligature binds fi", fi)
ℌ = "fraktur H"
print("fraktur normalizes to H", H)
𝐱 = 5
print("math bold x is x", x)
ｗｉｄｅ = "fullwidth"
print("fullwidth normalizes", wide)
ª = "feminine ordinal"
print("ordinal indicator is a", a)
print("NFKC sample", unicodedata.normalize("NFKC", "ﬁℌ𝐱ｗ"))


class Größe:
    def __init__(self, wert):
        self.wert = wert
        self.ñ = wert * 2

    def __repr__(self):
        return f"Größe({self.wert})"


g = Größe(3)
print("non-ASCII class", g, type(g).__name__, g.ñ)
print("attribute via getattr", g.ñ)


def σύνολο(*αριθμοί, πολλαπλασιαστής=1):
    return sum(αριθμοί) * πολλαπλασιαστής


print("greek function and kwarg", σύνολο(1, 2, 3, πολλαπλασιαστής=2))
print("kwarg via dict", σύνολο(4, πολλαπλασιαστής=3))
print("function name", σύνολο.__name__)


def 函数(参数):
    结果 = 参数 * 2
    return 结果


print("CJK identifiers", 函数(21))

# Keyword argument written with a compatibility character is normalized at parse time
def takes_fi(fi):
    return fi


print("kwarg normalized", takes_fi(ﬁ=7))
print("but kwarg from dict is not", (lambda **k: sorted(k))(ﬁ=1))

# Attribute names normalize in source but not in getattr strings
class Box:
    pass


b = Box()
b.ﬁeld = "set via ligature"
print("attribute normalized", b.field, sorted(vars(b)))
setattr(b, "ﬁeld", "raw")
print("setattr keeps raw", sorted(vars(b)))

# Combining characters and identifiers
café = "precomposed"
café = "decomposed"
print("decomposed spelling rebinds the precomposed name", café)

# Identifier checks
for s in ["ﬁ", "x²", "℘", "_ñ", "1a", "a-b", "Ⅷ", "ℰ"]:
    print("isidentifier", repr(s), s.isidentifier())
print("subscript digit normalizes", unicodedata.normalize("NFKC", "x₂"))

# Non-ASCII in strings and f-strings with nested names
名字 = "世界"
print(f"你好, {名字}! {len(名字)} chars, upper {'straße'.upper()}")
Δ = lambda λ: λ ** 2
print("lambda with greek", Δ(4))
