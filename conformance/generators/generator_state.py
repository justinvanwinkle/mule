"""Generator state via inspect.getgeneratorstate, gi_running/gi_frame/gi_suspended, and gi_yieldfrom."""

import inspect


def simple():
    yield inspect.getgeneratorstate(me)
    yield 2


me = simple()
print("created", inspect.getgeneratorstate(me))
print("running seen from inside", next(me))
print("suspended", inspect.getgeneratorstate(me), me.gi_suspended)
print("frame present", me.gi_frame is not None)
next(me)
next(me, None)
print("closed after exhaustion", inspect.getgeneratorstate(me))
print("frame cleared", me.gi_frame is None)

g = simple()
g.close()
print("closed before start", inspect.getgeneratorstate(g))

g = simple()
me = g
next(g)
g.close()
print("closed after close", inspect.getgeneratorstate(g))


def raising():
    yield 1
    raise ValueError


g = raising()
next(g)
try:
    next(g)
except ValueError:
    pass
print("closed after exception", inspect.getgeneratorstate(g))


def checker():
    yield checker_gen.gi_running


checker_gen = checker()
print("gi_running outside", checker_gen.gi_running)
print("gi_running inside", next(checker_gen))


def inner():
    yield 1


def outer():
    yield from inner()


o = outer()
print("gi_yieldfrom before", o.gi_yieldfrom)
next(o)
print("gi_yieldfrom during", o.gi_yieldfrom.__name__)
next(o, None)
print("gi_yieldfrom after", o.gi_yieldfrom)


def locals_snapshot(n):
    total = 0
    for i in range(n):
        total += i
        yield
    yield


g = locals_snapshot(4)
next(g)
next(g)
print("generator locals", sorted(inspect.getgeneratorlocals(g).items()))

print("gi_code name", g.gi_code.co_name)
print("generator name", g.__name__, g.__qualname__)


class Holder:
    def gen(self):
        yield


print("method generator qualname", Holder().gen().__qualname__)


def nested():
    def inner_gen():
        yield

    return inner_gen()


print("nested generator qualname", nested().__qualname__)
print("isgenerator", inspect.isgenerator(g), inspect.isgeneratorfunction(locals_snapshot), inspect.isgeneratorfunction(nested))
print("genexp is generator", inspect.isgenerator(x for x in ()))
