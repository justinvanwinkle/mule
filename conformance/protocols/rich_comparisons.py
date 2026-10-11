"""Rich comparisons: all six operators, reflection (a < b -> b > a), chained comparisons, total_ordering, sorting."""

from functools import total_ordering

log = []


class Ver:
    def __init__(self, *parts):
        self.parts = parts

    def __repr__(self):
        return "v" + ".".join(map(str, self.parts))

    def _cmp(self, op, o):
        log.append(f"{op}:{self!r}:{o!r}")
        if not isinstance(o, Ver):
            return NotImplemented
        return {"lt": self.parts < o.parts, "le": self.parts <= o.parts,
                "gt": self.parts > o.parts, "ge": self.parts >= o.parts,
                "eq": self.parts == o.parts, "ne": self.parts != o.parts}[op]

    def __lt__(self, o): return self._cmp("lt", o)
    def __le__(self, o): return self._cmp("le", o)
    def __gt__(self, o): return self._cmp("gt", o)
    def __ge__(self, o): return self._cmp("ge", o)
    def __eq__(self, o): return self._cmp("eq", o)
    def __ne__(self, o): return self._cmp("ne", o)

    __hash__ = None


a, b = Ver(1, 2), Ver(1, 10)
print("all six", a < b, a <= b, a > b, a >= b, a == b, a != b)
log.clear()
print("sorted", sorted([Ver(2), Ver(1, 5), Ver(1, 0, 1)]))
print("sort uses only lt", sorted(set(e.split(":")[0] for e in log)))
print("max/min", max(a, b), min([b, a]))


# Reflection: a < b falls back to b > a when a lacks/declines __lt__.
class OnlyGt:
    def __init__(self, v):
        self.v = v

    def __gt__(self, o):
        log.append(f"OnlyGt({self.v}).__gt__")
        return self.v > getattr(o, "v", o)


class Plain:
    def __init__(self, v):
        self.v = v


log.clear()
print("lt reflects to gt", Plain(1) < OnlyGt(5), log)
log.clear()
print("int < custom reflects", 3 < OnlyGt(5), log)
try:
    Plain(1) > OnlyGt(5)
except TypeError as e:
    print("gt does not reflect to gt", type(e).__name__)


# __eq__ reflection: == tries left then right __eq__; != uses __ne__ default inverting __eq__.
class EqOnly:
    def __init__(self, k):
        self.k = k

    def __eq__(self, o):
        log.append(f"EqOnly({self.k}).__eq__")
        if isinstance(o, EqOnly):
            return self.k == o.k
        return NotImplemented

    __hash__ = object.__hash__


log.clear()
print("ne derived from eq", EqOnly(1) != EqOnly(1), EqOnly(1) != EqOnly(2), log)
log.clear()
print("eq with int both sides", EqOnly(1) == 1, 1 == EqOnly(1), log)


# Subclass reflected comparison priority
class Base:
    def __lt__(self, o):
        return "Base.__lt__"


class Derived(Base):
    def __gt__(self, o):
        return "Derived.__gt__"


print("subclass reflected first", Base() < Derived(), Derived() < Base())


# Chained comparisons evaluate middle once and short-circuit
def val(x, tag):
    log.append(tag)
    return x


log.clear()
print("chain", val(1, "a") < val(2, "b") < val(3, "c"), log)
log.clear()
print("chain short-circuit", val(3, "a") < val(2, "b") < val(9, "c"), log)
print("mixed chain", 1 < 2 > 0 == 0 != 1, 1 == 1.0 < 2 != None)


# Comparison returning non-bool values
class Lazy:
    def __lt__(self, o):
        return ["not", "a", "bool"]

    def __eq__(self, o):
        return 0


print("non-bool results", Lazy() < 1, Lazy() == 1, bool(Lazy() == 1))
print("in chain uses truthiness", 0 < 1 < 2 and Lazy() < Lazy())


@total_ordering
class Card:
    order = "23456789TJQKA"

    def __init__(self, r):
        self.r = r

    def __eq__(self, o):
        return self.r == o.r

    def __lt__(self, o):
        return self.order.index(self.r) < self.order.index(o.r)

    def __hash__(self):
        return hash(self.r)

    def __repr__(self):
        return self.r


cards = [Card(c) for c in "KT2A9"]
print("total_ordering", sorted(cards), Card("A") >= Card("K"), Card("2") > Card("3"), Card("T") <= Card("T"))

# Builtin container comparisons use element comparisons (lexicographic)
print("tuple lexi", (1, 2, 3) < (1, 3), [1, 2] < [1, 2, 0], (Ver(1),) < (Ver(2),))
print("identity shortcut in containers", [float("nan")] == [float("nan")], (lambda n: [n] == [n])(float("nan")))
