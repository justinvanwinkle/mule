"""Starred displays and call arguments: [*a, *b], (*a,), {*s}, {**d}, and evaluation order."""

a = [1, 2]
b = (3, 4)
print("list", [*a, *b], [0, *a, 5, *b, 6])
print("tuple", (*a, *b), (*a,))
print("set", sorted({*a, *b, 1}))
print("from strings", [*"ab", *"cd"])
print("from range dict", [*range(3), *{"k": 1, "j": 2}])
print("from gen", [*(n * n for n in range(4))])
print("empty", [*[], *()], (*[],), {*()} == set())
print("dict", {**{"a": 1}, **dict(b=2)})
print("nested star", [*[*a, *b], *[[*a]]])
gen = iter(range(3))
print("consumes iterator", [*gen], [*gen])
print("star set of tuples", sorted({*[(1, 2), (1, 2)], (0, 0)}))

order = []


def tag(name, value):
    order.append(name)
    return value


result = [tag("a", 1), *tag("b", [2, 3]), tag("c", 4)]
print("eval order", result, order)
order.clear()
dresult = {tag("k1", "x"): tag("v1", 1), **tag("d", {"y": 2}), tag("k2", "z"): tag("v2", 3)}
print("dict eval order", dresult, order)


def f(*args, **kwargs):
    return args, kwargs


print("call star", f(*a, *b))
print("call mixed", f(0, *a, 9, *b, x=1, **{"y": 2}))
print("call star after kw", f(*a, k=1, *b))
print("call star string", f(*"hi"))

try:
    [*5]
except TypeError as e:
    print("star non iterable", type(e).__name__)
try:
    {*[[1]]}
except TypeError as e:
    print("star unhashable in set", type(e).__name__)

*xs, = {"only": 1}
print("assign star dict", xs)
pair = 1, *a
print("bare tuple star", pair)
for item in *a, *b:
    print("for over starred tuple", item)


def ret():
    return *a, 0


print("return starred", ret())
print("subscript starred", {(1, 2, 3): "hit"}[*a, 3])
