"""Unpacking: nested, starred in every position, for targets, swaps, and unpacking errors."""

a, b = 1, 2
a, b = b, a
print("swap", a, b)
x, y, z = "xyz"
print("string", x, y, z)
k1, k2 = {"first": 1, "second": 2}
print("dict keys", k1, k2)
g1, g2 = (n * 10 for n in range(2))
print("generator", g1, g2)
(p, q), r = (1, 2), 3
print("nested", p, q, r)
[s, [t, u]] = [1, (2, 3)]
print("nested list target", s, t, u)
((i,),) = ((7,),)
print("deep single", i)

first, *rest = [1, 2, 3, 4]
print("star end", first, rest)
*init, last = [1, 2, 3, 4]
print("star start", init, last)
h, *mid, tl = range(5)
print("star middle", h, mid, tl)
h2, *mid2, tl2 = (1, 2)
print("star empty", h2, mid2, tl2)
*everything, = "abc"
print("star only", everything)
[*lst] = (1, 2)
print("star only list target", lst)
a1, (b1, *c1), *d1 = 1, "xyz", 3, 4
print("star nested", a1, b1, c1, d1)
*star_from_gen, end = (c for c in "hello")
print("star from gen", star_from_gen, end)
print("star type always list", type(rest).__name__, type(mid).__name__)

for idx, (ch, *others) in enumerate(["ab", "cde"]):
    print("for target", idx, ch, others)
for (m, n), o in [((1, 2), 3), ((4, 5), 6)]:
    print("for nested", m, n, o)
for key, value in {"a": 1}.items():
    print("for items", key, value)

lst2 = [0, 0, 0]
i = 0
i, lst2[i] = 2, 9
print("left to right assign", i, lst2)

obj = {}
obj["a"], obj["b"] = obj["b"], obj["a"] = 1, 2
print("chained unpack", obj)

xs = [1, 2, 3]
xs[0], xs[2] = xs[2], xs[0]
print("swap items", xs)

aa = bb = [1]
aa += [2]
print("chained alias", bb)

errors = []
for value in ([1, 2, 3], [1], 5, "ab", None, (1,)):
    try:
        e1, e2 = value
        errors.append(("ok", e1, e2))
    except (ValueError, TypeError) as e:
        errors.append(type(e).__name__)
print("two target errors", errors)
try:
    a3, *b3, c3 = [1]
except ValueError as e:
    print("star too few", type(e).__name__)


def counting():
    for n in range(10):
        print("yield", n)
        yield n


try:
    c1, c2 = counting()
except ValueError as e:
    print("too many stops after extra", type(e).__name__)

it = iter([1, 2, 3, 4])
f1, f2 = next(it), next(it)
print("partial consume", f1, f2, list(it))
