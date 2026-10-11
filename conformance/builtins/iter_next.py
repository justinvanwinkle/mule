"""iter() on iterables and with a sentinel callable, next() with and without default, StopIteration."""

it = iter([1, 2])
print("next", next(it), next(it))
print("next default", next(it, "done"), next(it, None))
try:
    next(it)
except StopIteration as e:
    print("exhausted", type(e).__name__, e.args)
print("iter of iterator is self", iter(it) is it)
try:
    next([1, 2])
except TypeError as e:
    print("next on list", type(e).__name__)
try:
    iter(5)
except TypeError as e:
    print("iter int", type(e).__name__)

state = [0]


def counter():
    state[0] += 1
    return state[0]


print("sentinel", list(iter(counter, 4)))
print("sentinel stops before", state[0])
chunks = iter(["a", "b", "", "c"])
print("sentinel empty string", list(iter(lambda: next(chunks), "")))
print("rest after sentinel", list(chunks))
nan_calls = iter([1.0, float("nan"), 2.0])
try:
    print("sentinel never equal", list(iter(lambda: next(nan_calls), float("nan"))))
except StopIteration as e:
    print("sentinel callable raised StopIteration", type(e).__name__)


def raiser_factory():
    values = [1, 2]

    def f():
        if not values:
            raise StopIteration
        return values.pop(0)

    return f


print("callable raising StopIteration ends", list(iter(raiser_factory(), object())))
try:
    iter(5, 0)
except TypeError as e:
    print("sentinel non callable", type(e).__name__)


class Seq:
    def __getitem__(self, i):
        if i > 2:
            raise IndexError
        return i * i


print("getitem protocol", list(iter(Seq())))


class Countdown:
    def __init__(self, n):
        self.n = n

    def __iter__(self):
        return self

    def __next__(self):
        if self.n == 0:
            raise StopIteration
        self.n -= 1
        return self.n


cd = Countdown(3)
print("custom iterator", next(cd), list(cd), next(cd, "empty"))


class NotIter:
    __iter__ = None


try:
    iter(NotIter())
except TypeError as e:
    print("iter None blocks", type(e).__name__)

d_it = iter({"a": 1, "b": 2})
print("dict iter", next(d_it), next(d_it), next(d_it, "-"))
s_it = iter("hé")
print("str iter", next(s_it), next(s_it))
gen = (x for x in range(2))
print("gen next", next(gen), next(gen), next(gen, "default"))
print("next on exhausted gen again", next(gen, "still"))
