"""Functions as values: multiple returns, containers of functions, higher-order functions, and builtins as values."""


def divmod_pair(a, b):
    return a // b, a % b


q, r = divmod_pair(17, 5)
print("multiple returns", q, r, divmod_pair(17, 5))


def stats(xs):
    return min(xs), max(xs), sum(xs) / len(xs)


lo, *rest = stats([4, 8, 6])
print("star unpack return", lo, rest)


def nothing():
    return


def implicit():
    pass


print("bare return / implicit", nothing(), implicit())


def early(x):
    if x < 0:
        return "neg"
    for i in range(10):
        if i == x:
            return "found", i
    return "none"


print("early returns", early(-1), early(3), early(50))


def square(x):
    return x * x


def neg(x):
    return -x


pipeline = [square, neg, abs, str, len]
value = 12
for fn in pipeline:
    value = fn(value)
print("pipeline", value)

by_name = {fn.__name__: fn for fn in (square, neg, abs)}
print("dict of functions", sorted(by_name), by_name["neg"](3))
print("tuple of functions", tuple(f(4) for f in (square, neg)))
print("sorted by function name", [f.__name__ for f in sorted([neg, square, abs], key=lambda f: f.__name__)])


def apply_n(fn, n, x):
    for _ in range(n):
        x = fn(x)
    return x


print("apply_n", apply_n(square, 3, 2), apply_n(lambda s: s + "!", 3, "hi"))


def compose(*fns):
    def composed(x):
        for f in reversed(fns):
            x = f(x)
        return x

    return composed


print("compose", compose(str, neg, square)(5))


def flip(fn):
    return lambda a, b: fn(b, a)


print("flip", flip(pow)(2, 10))


def make_multipliers():
    return [lambda x, k=k: x * k for k in range(1, 4)]


print("multipliers", [m(10) for m in make_multipliers()])

print("map with builtin", list(map(str.upper, ["a", "b"])))
print("map two iterables", list(map(pow, [2, 3], [3, 2])))
print("sorted with key fn", sorted(["Banana", "apple", "Cherry"], key=str.lower))
print("max with key", max(["aa", "b", "cccc"], key=len))
print("unbound method as fn", list(map(list.__len__, [[1], [1, 2]])))
print("bound method as value", list(map("-".join, [["a", "b"], ["c"]])))

import functools

print("reduce", functools.reduce(lambda acc, x: acc * x, range(1, 6), 1))
print("reduce no initial", functools.reduce(lambda a, b: a + b, ["x", "y", "z"]))

# Functions compared by identity, usable as dict keys
counts = {square: 0, neg: 0}
for f in [square, neg, square]:
    counts[f] += 1
print("functions as keys", counts[square], counts[neg])
print("equality", square == square, square == neg)


# Returning a function from a conditional
def chooser(kind):
    if kind == "up":
        def f(s):
            return s.upper()
    else:
        def f(s):
            return s.lower()
    return f


print("conditional def", chooser("up")("Mix"), chooser("down")("Mix"))


# Callable objects alongside functions
class Adder:
    def __init__(self, n):
        self.n = n

    def __call__(self, x):
        return x + self.n


print("callable object in map", list(map(Adder(10), [1, 2])))
print("mixed callables", [f(3) for f in (Adder(1), square, lambda v: v - 1, int.__neg__)])

# Function redefinition replaces binding
def redefined():
    return 1


old = redefined


def redefined():
    return 2


print("redefinition", old(), redefined())

# Generator of functions consumed lazily
gens = (lambda x, i=i: x + i for i in range(3))
print("genexp of functions", [g(100) for g in gens])
