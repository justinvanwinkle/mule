"""Membership and iteration protocols: __contains__ fallbacks, __iter__/__next__, legacy __getitem__ iteration, __reversed__, __length_hint__."""

import operator

log = []


class WithContains:
    def __contains__(self, x):
        log.append(f"contains {x}")
        return x == "yes"

    def __iter__(self):
        log.append("iter")
        return iter(["no"])


log.clear()
print("__contains__ used", "yes" in WithContains(), "no" in WithContains(), "no" not in WithContains(), log)


class ContainsTruthy:
    def __contains__(self, x):
        return [x]  # result coerced to bool


print("contains coerces", 1 in ContainsTruthy(), type(1 in ContainsTruthy()).__name__)


class IterOnly:
    def __iter__(self):
        log.append("iter start")
        for i in range(5):
            log.append(f"yield {i}")
            yield i


log.clear()
print("contains via __iter__", 2 in IterOnly(), log)
log.clear()
print("contains exhaust", 9 in IterOnly(), len(log))


class GetitemOnly:
    def __getitem__(self, i):
        log.append(f"getitem {i}")
        if i >= 4:
            raise IndexError(i)
        return i * 10


log.clear()
print("contains via __getitem__", 20 in GetitemOnly(), log)
log.clear()
print("legacy iteration", list(GetitemOnly()), log[-1])
print("legacy for loop", [x for x in GetitemOnly()])
print("legacy iter() object", type(iter(GetitemOnly())).__name__)


class Neither:
    pass


try:
    1 in Neither()
except TypeError as e:
    print("no protocol contains", type(e).__name__)
try:
    iter(Neither())
except TypeError as e:
    print("no protocol iter", type(e).__name__)


# __iter__ returning a separate iterator allows independent, repeated traversals
class Countdown:
    def __init__(self, n):
        self.n = n

    def __iter__(self):
        return CountdownIter(self.n)


class CountdownIter:
    def __init__(self, n):
        self.n = n

    def __iter__(self):
        return self

    def __next__(self):
        if self.n <= 0:
            raise StopIteration
        self.n -= 1
        return self.n + 1


cd = Countdown(3)
print("reiterable", list(cd), list(cd), [(a, b) for a in cd for b in cd if a == b])
it = iter(cd)
print("iterator self-iter", iter(it) is it, next(it), list(it), list(it))
print("next default", next(iter(Countdown(0)), "empty"))


# Iterator that is its own iterable: single pass
class OnePass:
    def __init__(self):
        self.i = 0

    def __iter__(self):
        return self

    def __next__(self):
        self.i += 1
        if self.i > 2:
            raise StopIteration
        return self.i


op = OnePass()
print("single pass", list(op), list(op))

# __iter__ returning a non-iterator
class BadIter:
    def __iter__(self):
        return [1, 2]


try:
    iter(BadIter())
except TypeError as e:
    print("iter returns non-iterator", type(e).__name__)


# iter() with sentinel
vals = iter([1, 2, 3, 0, 4])
print("iter sentinel", list(iter(lambda: next(vals), 0)))


# __reversed__ vs sequence fallback
class Rev:
    def __reversed__(self):
        return iter("cba")

    def __iter__(self):
        return iter("abc")


class SeqRev:
    def __len__(self):
        return 3

    def __getitem__(self, i):
        if i >= 3:
            raise IndexError
        return i


print("__reversed__", list(reversed(Rev())), "seq fallback", list(reversed(SeqRev())))
try:
    reversed(IterOnly())
except TypeError as e:
    print("reversed needs sequence", type(e).__name__)


class NoRev(SeqRev):
    __reversed__ = None


try:
    reversed(NoRev())
except TypeError as e:
    print("reversed None", type(e).__name__)


# __length_hint__
class Hinted:
    def __iter__(self):
        return iter([1, 2])

    def __length_hint__(self):
        return 10


class BadHint:
    def __length_hint__(self):
        return -1


print("length_hint", operator.length_hint(Hinted()), operator.length_hint(SeqRev()), operator.length_hint(object(), 7))
print("length_hint not len", list(Hinted()))
try:
    operator.length_hint(BadHint())
except ValueError as e:
    print("negative hint", type(e).__name__)
print("hint NotImplemented uses default", operator.length_hint(type("H", (), {"__length_hint__": lambda s: NotImplemented})(), 4))

# `in` on dict checks keys; on str checks substrings
print("builtin in", 1 in {1: 2}, 2 in {1: 2}, "ell" in "hello", "" in "x", [1] in [[1]], None in [0, False])
