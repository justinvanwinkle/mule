"""Rich comparisons returning non-bool objects, chained comparisons using their truthiness, and __bool__ raising in conditions."""


class Truthy:
    def __init__(self, label, truth):
        self.label, self.truth = label, truth

    def __bool__(self):
        print(f"  bool({self.label}) -> {self.truth}")
        return self.truth

    def __repr__(self):
        return f"T<{self.label}>"


class V:
    def __init__(self, n):
        self.n = n

    def __eq__(self, other):
        return Truthy(f"{self.n}=={getattr(other, 'n', other)}", self.n == getattr(other, "n", other))

    def __lt__(self, other):
        return Truthy(f"{self.n}<{other.n}", self.n < other.n)

    def __repr__(self):
        return f"V{self.n}"


a, b, c = V(1), V(2), V(3)
print("single comparison returns object", a == b)
print("chain true path", a < b < c)
print("chain short-circuit", b < a < c)
r = a == a == V(2)
print("chain result is last object", r)
print("not on object", not (a == b))
print("and returns object", (a == a) and "rhs")
print("or returns object", (a == b) or "fallback")
print("list membership uses truthiness", V(2) in [V(1), V(2)])
print("ternary", "yes" if a < c else "no")


class Arr:
    """numpy-style elementwise comparison returning a list wrapper with ambiguous truth."""

    def __init__(self, xs):
        self.xs = list(xs)

    def __eq__(self, other):
        return Arr(x == y for x, y in zip(self.xs, other.xs))

    def __gt__(self, other):
        return Arr(x > other for x in self.xs)

    def __bool__(self):
        raise ValueError("ambiguous truth value")

    def __repr__(self):
        return f"Arr({self.xs})"

    __hash__ = None


p, q = Arr([1, 2, 3]), Arr([1, 0, 3])
print("elementwise eq", p == q)
print("elementwise gt", p > 1)
for label, thunk in [
    ("if", lambda: 1 if p == q else 0),
    ("and", lambda: (p == q) and 1),
    ("not", lambda: not p),
    ("chained", lambda: 0 < 1 < p),
    ("while", lambda: [None for _ in iter(int, 1) if p]),
    ("in list", lambda: q in [p]),
    ("all", lambda: all([p])),
    ("filter", lambda: list(filter(None, [p]))),
]:
    try:
        print(label, thunk())
    except ValueError as e:
        print(label, "raised", e)
print("in list identity skips eq", p in [p])
print("chained last link not converted", 3 > 1 < p)


class BoolTwice:
    def __bool__(self):
        return 2


try:
    bool(BoolTwice())
except TypeError as e:
    print("__bool__ returning int", type(e).__name__)


class LenOnly:
    def __init__(self, n):
        self.n = n

    def __len__(self):
        print("  len called")
        return self.n


print("len fallback", bool(LenOnly(0)), bool(LenOnly(5)))
print("or with len objects", (LenOnly(0) or "empty"))


class NotImpl:
    def __eq__(self, other):
        return NotImplemented

    def __lt__(self, other):
        return NotImplemented


x, y = NotImpl(), NotImpl()
print("eq falls back to identity", x == x, x == y, x != y)
try:
    x < y
except TypeError as e:
    print("lt with no fallback", type(e).__name__)


class Reflect:
    def __gt__(self, other):
        return f"Reflect.__gt__({other})"


print("reflected comparison", 5 < Reflect())


class Sub(int):
    def __eq__(self, other):
        return "sub wins"

    __hash__ = int.__hash__


print("subclass priority", 1 == Sub(1), Sub(1) == 1)

# Comparisons on a while loop whose condition object flips
class Countdown:
    def __init__(self, n):
        self.n = n

    def __bool__(self):
        self.n -= 1
        return self.n >= 0


cd = Countdown(3)
loops = 0
while cd:
    loops += 1
print("while with stateful __bool__", loops, cd.n)
