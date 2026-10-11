"""yield from: delegation of values, send, throw, close, return values, and non-generator iterables."""

log = []


def inner():
    x = yield "inner 1"
    log.append("inner got " + repr(x))
    y = yield "inner 2"
    log.append("inner got " + repr(y))
    return "inner result"


def outer():
    result = yield from inner()
    log.append("outer got result " + result)
    yield "outer after"
    return "outer result"


g = outer()
print("delegate 1", next(g))
print("delegate send", g.send("A"))
print("delegate send 2", g.send("B"))
try:
    next(g)
except StopIteration as e:
    print("outer return", e.value)
print("log", log)

print("from list", list((lambda: (yield from [1, 2, 3]))()))


def from_iterables():
    yield from "ab"
    yield from range(2)
    yield from {"k": 1}
    yield from (x * 10 for x in range(2))
    yield from iter([None])


print("from iterables", list(from_iterables()))


def from_list_send():
    yield from [1, 2]


g = from_list_send()
next(g)
try:
    g.send("x")
except AttributeError as e:
    print("send to list delegate", type(e).__name__)


def handles():
    try:
        yield "h1"
    except ValueError:
        yield "handled in inner"
    return "inner done"


def delegator():
    r = yield from handles()
    yield "delegator saw " + r


g = delegator()
next(g)
print("throw delegated", g.throw(ValueError))
print("after throw", next(g))


def unhandled_inner():
    yield 1
    yield 2


def catching_outer():
    try:
        yield from unhandled_inner()
    except KeyError:
        yield "outer caught"


g = catching_outer()
next(g)
print("inner doesn't handle, outer does", g.throw(KeyError))

log.clear()


def closable_inner():
    try:
        yield 1
    finally:
        log.append("inner finally")


def closable_outer():
    try:
        yield from closable_inner()
    finally:
        log.append("outer finally")


g = closable_outer()
next(g)
g.close()
print("close propagates", log)


def tree(node):
    value, children = node
    yield value
    for child in children:
        yield from tree(child)


t = ("a", [("b", [("d", [])]), ("c", [("e", []), ("f", [])])])
print("recursive yield from", list(tree(t)))


def flatten(x):
    if isinstance(x, list):
        for item in x:
            yield from flatten(item)
    else:
        yield x


print("recursive flatten", list(flatten([1, [2, [3, [4, [5]]]], 6])))


def returns_only():
    return "just a value"
    yield


def take_value():
    v = yield from returns_only()
    yield v


print("yield from empty generator value", list(take_value()))


def chain(*its):
    for it in its:
        yield from it


print("chain", list(chain([1], (), "xy", range(3, 5))))


def count_delegated():
    total = 0
    while True:
        x = yield
        if x is None:
            return total
        total += x


def collector(results):
    while True:
        results.append((yield from count_delegated()))


results = []
c = collector(results)
next(c)
for v in (1, 2, 3, None, 10, 20, None):
    c.send(v)
print("delegated returns collected", results)


class CustomIter:
    def __init__(self):
        self.i = 0

    def __iter__(self):
        return self

    def __next__(self):
        self.i += 1
        if self.i > 3:
            raise StopIteration
        return self.i * 100


print("custom iterator yield from", list((lambda: (yield from CustomIter()))()))


def stop_value_from_iterator():
    class R:
        def __iter__(self):
            return self

        def __next__(self):
            raise StopIteration("iterator value")

    v = yield from R()
    yield v


print("StopIteration value from plain iterator", list(stop_value_from_iterator()))


def exhausted_delegate():
    g = iter([1])
    next(g)
    v = yield from g
    yield ("got", v)


print("delegate already exhausted", list(exhausted_delegate()))
