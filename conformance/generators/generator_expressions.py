"""Generator expressions: laziness, first-iterable eager evaluation, exhaustion, nesting, and scoping."""

log = []


def src(label, items):
    log.append("iter " + label)
    return items


def f(x):
    log.append("f " + str(x))
    return x * 2


g = (f(x) for x in src("outer", [1, 2, 3]))
print("first iterable eager, body lazy", log)
print("first value", next(g), log)
print("rest", list(g), log)
print("exhausted", list(g), next(g, "empty"))

log.clear()
g = (x + y for x in src("first", [1, 2]) for y in src("second", [10, 20]))
print("second iterable lazy", log)
print("nested values", list(g))
print("second iterable per outer item", log)

try:
    g = (x for x in 5)
except TypeError as e:
    print("first iterable error at creation", type(e).__name__)

g = (x for x in [1] for y in 5)
print("inner iterable error deferred", type(g).__name__)
try:
    next(g)
except TypeError as e:
    print("inner iterable error on next", type(e).__name__)

g = (1 / x for x in [1, 0, 2])
print("lazy error 1", next(g))
try:
    next(g)
except ZeroDivisionError as e:
    print("lazy error 2", type(e).__name__)
print("dead after error", next(g, "dead"))

items = [1, 2, 3]
g = (x * 10 for x in items)
items.append(4)
print("sees mutation of same list", list(g))

items = [1, 2, 3]
g = (x for x in items)
items = [9, 9]
print("rebinding name has no effect", list(g))

n = 10
g = (x + n for x in range(3))
n = 100
print("free variables looked up lazily", list(g))

print("sum genexp", sum(x * x for x in range(10)))
print("any short-circuit", any(log.append(x) or x > 1 for x in range(100)), len(log))
log.clear()
print("all short-circuit", all(log.append(x) or x < 2 for x in range(100)), len(log))

nested = ((i, j) for i in range(3) for j in range(i))
print("dependent nested", list(nested))
print("genexp of genexps", [list(row) for row in ((i * j for j in range(3)) for i in range(3))])
print("filter clauses", list(x for x in range(20) if x % 2 if x % 3))
print("conditional expression", list("e" if x % 2 == 0 else "o" for x in range(5)))

g = (x for x in range(3))
print("type and iter", type(g).__name__, iter(g) is g)
print("send None to genexp", g.send(None))
g.close()
print("closed genexp", list(g))

x = "module x"
list(x for x in range(3))
print("loop var does not leak", x)


class C:
    vals = [1, 2, 3]
    factor = 10
    first = list(v for v in vals)
    try:
        scaled = list(v * factor for v in vals)
    except NameError as e:
        scaled = type(e).__name__


print("class scope first iterable ok", C.first)
print("class scope body cannot see class vars", C.scaled)

print("walrus in genexp", list((y := v * 2) for v in range(3)), y)

g = (i for i in range(5))
print("partially consumed", next(g), next(g), list(g))

print("sorted genexp", sorted(-v for v in [3, 1, 2]))
print("tuple from genexp", tuple(c.upper() for c in "abc"))
print("dict from genexp pairs", dict((c, ord(c)) for c in "ab"))
print("str join genexp", "-".join(str(i) for i in range(4)))
print("max genexp default", max((v for v in []), default="none"))
print("unpack genexp", [*(v for v in "xy"), *(v for v in "z")])
