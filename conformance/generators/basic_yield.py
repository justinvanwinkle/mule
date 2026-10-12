"""Basic generators: lazy execution, next(), StopIteration, exhaustion, multiple independent instances."""

trace = []


def counter(n):
    trace.append("start")
    for i in range(n):
        trace.append("yield " + str(i))
        yield i
    trace.append("end")


g = counter(3)
print("created, nothing ran", trace, type(g).__name__)
print("first next", next(g), trace)
print("second next", next(g))
print("third next", next(g))
try:
    next(g)
except StopIteration as e:
    print("exhausted", type(e).__name__, e.value)
print("trace", trace)
try:
    next(g)
except StopIteration:
    print("still exhausted")
print("next with default", next(g, "dflt"))

print("list()", list(counter(4)))
print("sum()", sum(counter(5)))
print("in operator", 3 in counter(5), 9 in counter(5))

a = counter(3)
b = counter(3)
next(a)
next(a)
print("independent instances", next(a), next(b))


def empty():
    return
    yield  # makes this a generator function


print("empty generator", list(empty()), type(empty()).__name__)


def only_return():
    if False:
        yield
    return "result"


try:
    next(only_return())
except StopIteration as e:
    print("return value on first next", e.value)


def yields_none():
    yield
    yield None


print("bare yield", list(yields_none()))


def multiple_yields():
    yield "a"
    x = 1
    yield x + 1
    yield [x, x]
    for c in "yz":
        yield c


print("multiple yields", list(multiple_yields()))


def fib():
    a, b = 0, 1
    while True:
        yield a
        a, b = b, a + b


f = fib()
print("infinite partial", [next(f) for _ in range(12)])
import itertools

print("islice infinite", list(itertools.islice(fib(), 20, 25)))
print("takewhile infinite", list(itertools.takewhile(lambda v: v < 100, fib())))

g = counter(2)
print("iter(g) is g", iter(g) is g)


def unpack_target():
    yield 1
    yield 2
    yield 3


x, *ys = unpack_target()
print("unpacking", x, ys)
print("zip generators", list(zip(counter(3), "abc")))
print("enumerate generator", list(enumerate(unpack_target(), 10)))
print("dict from generator", dict((k, k * k) for k in range(3)))


def gen_with_local_state():
    seen = []
    for v in (3, 1, 2):
        seen.append(v)
        yield sorted(seen)


print("state across yields", list(gen_with_local_state()))


def yields_in_try():
    try:
        yield "in try"
    finally:
        trace.append("finally on exhaustion")


trace.clear()
print("try/finally exhaustion", list(yields_in_try()), trace)


def gen_raises():
    yield 1
    raise ValueError("boom")


g = gen_raises()
print("before raise", next(g))
try:
    next(g)
except ValueError as e:
    print("generator raised", e)
print("after raise exhausted", next(g, "done"))
