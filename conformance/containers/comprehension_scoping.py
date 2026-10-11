"""Comprehension scoping: loop variables do not leak, walrus binds enclosing scope, class-body rules."""

x = "outer"
squares = [x * 2 for x in range(3)]
print("no leak", x, squares)
gen_var = "keep"
list(gen_var for gen_var in "ab")
print("gen no leak", gen_var)
d_var = "keep"
{d_var: 1 for d_var in "ab"}
print("dict no leak", d_var)

total = 0
running = [total := total + n for n in range(5)]
print("walrus running", running, total)

found = None
if any((found := n) > 3 for n in [1, 5, 2]):
    print("walrus any", found)

data = [1, 2, 3, 4]
filtered = [y for n in data if (y := n * 10) > 15]
print("walrus filter", filtered, y)


def inner_func():
    hits = [last := c for c in "abc"]
    return hits, last


print("walrus in function", inner_func())


def nested_walrus():
    out = [[(m := a * b) for b in range(2)] for a in range(3)]
    return out, m


print("walrus nested comps", nested_walrus())


def outer():
    result = []

    def inner():
        return [z for z in range(2)]

    z = "outer z"
    result.append(inner())
    result.append(z)
    return result


print("enclosing untouched", outer())


def closure_capture():
    base = 10
    return [lambda: base + i for i in range(2)]


print("closure in comp", [f() for f in closure_capture()])


class C:
    factor = 3
    values = [factor * 2]
    first_iter = [n for n in range(factor)]
    try:
        bad = [factor * n for n in range(2)]
    except NameError as e:
        bad = type(e).__name__


print("class body first iterable", C.first_iter, C.values)
print("class body inner scope", C.bad)

name = "global"


def shadow():
    return [name for name in ["local"]], name


print("shadow", shadow())

try:
    exec_free = [undefined_name for _ in range(1)]
except NameError as e:
    print("undefined in comp", type(e).__name__)

counter = 0


def bump():
    global counter
    counter += 1
    return counter


lazy = (bump() for _ in range(3))
print("generator lazy", counter)
next(lazy)
print("after one next", counter)
