"""Hash consistency across numerically equal int, float, bool, complex, Fraction, and Decimal values."""

from decimal import Decimal
from fractions import Fraction

groups = [
    (1, 1.0, True, 1 + 0j, Fraction(1), Decimal(1)),
    (0, 0.0, -0.0, False, 0j, Fraction(0), Decimal("0.0")),
    (2**64, float(2**64), Fraction(2**64)),
    (-5, -5.0, Fraction(-10, 2), Decimal(-5)),
    (0.5, Fraction(1, 2), Decimal("0.5")),
    (2.5 + 0j, 2.5),
]
for g in groups:
    print("group", [type(v).__name__ for v in g])
    print("  all equal", all(a == b for a in g for b in g))
    print("  hashes equal", len({hash(v) for v in g}) == 1)

print("int hash small", hash(0), hash(1), hash(-1) == hash(-2), hash(42))
print("hash -1 special", hash(-1))
print("bool hash", hash(True), hash(False))
print("big int hash consistent", hash(2**100) == hash(2**100), hash(2**100) == hash(float(2**100)))
print("float int-valued hash", hash(3.0) == hash(3), hash(1e20) == hash(10**20))
print("inf hash eq", hash(float("inf")) == hash(float("inf")), hash(float("-inf")) == -hash(float("inf")))
print("tuple hash eq", hash((1, 2.0)) == hash((1.0, 2)), hash((True,)) == hash((1,)))
print("frozenset hash eq", hash(frozenset({1, 2})) == hash(frozenset({2.0, 1.0})))
print("str hash consistent", hash("abc") == hash("ab" + "c"), hash(b"x") == hash(b"x"))

d = {1: "one", 2.0: "two"}
print("lookup by equal", d[1.0], d[True], d[2], d[Fraction(2)], d[2 + 0j])
s = {1, 2}
print("set size", len(s), sorted(s))


class Pt:
    def __init__(self, x):
        self.x = x

    def __eq__(self, other):
        return isinstance(other, Pt) and self.x == other.x

    def __hash__(self):
        return hash(self.x)


print("custom hash", Pt(1) in {Pt(1.0)}, len({Pt(1), Pt(True), Pt(2)}))


class NoHash:
    def __eq__(self, other):
        return True


try:
    hash(NoHash())
except TypeError as e:
    print("eq without hash", type(e).__name__)
try:
    hash([1])
except TypeError as e:
    print("list hash", type(e).__name__)
