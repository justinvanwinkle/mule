"""Generator cleanup: for-loop break leaves generator suspended; finally runs on close or exhaustion; context managers in generators."""

log = []


def resource(name, n):
    log.append("open " + name)
    try:
        for i in range(n):
            yield i
    finally:
        log.append("close " + name)


g = resource("a", 10)
for v in g:
    if v == 2:
        break
print("after break, still suspended", log)
print("resume after break", next(g))
g.close()
print("explicit close", log)

log.clear()
for v in resource("b", 2):
    pass
print("exhaustion runs finally", log)

log.clear()
g = resource("c", 5)
try:
    for v in g:
        if v == 1:
            raise ValueError("stop")
except ValueError:
    log.append("caught in caller")
print("exception in caller body does not close", log)
g.close()
print("closed afterwards", log)


class Managed:
    def __init__(self, name):
        self.name = name

    def __enter__(self):
        log.append("enter " + self.name)
        return self

    def __exit__(self, *exc):
        log.append("exit " + self.name + (" with " + exc[0].__name__ if exc[0] else ""))
        return False


def with_manager():
    with Managed("m"):
        yield 1
        yield 2


log.clear()
g = with_manager()
next(g)
g.close()
print("close triggers __exit__ with GeneratorExit", log)

log.clear()
print("exhaust with manager", list(with_manager()), log)

import contextlib


@contextlib.contextmanager
def tag(name):
    log.append("<" + name + ">")
    try:
        yield name.upper()
    finally:
        log.append("</" + name + ">")


log.clear()
with tag("p") as t:
    log.append("body " + t)
print("contextmanager generator", log)

log.clear()
try:
    with tag("err"):
        raise KeyError("x")
except KeyError:
    log.append("propagated")
print("contextmanager exception", log)


@contextlib.contextmanager
def suppressing():
    try:
        yield
    except ZeroDivisionError:
        log.append("suppressed")


log.clear()
with suppressing():
    1 / 0
log.append("continued")
print("contextmanager suppress", log)


def nested_cleanup():
    try:
        try:
            yield "x"
        finally:
            log.append("inner finally")
    finally:
        log.append("outer finally")


log.clear()
g = nested_cleanup()
next(g)
g.close()
print("nested finally order", log)


def finally_raises():
    try:
        yield 1
    finally:
        raise ValueError("from finally")


g = finally_raises()
next(g)
try:
    g.close()
except ValueError as e:
    print("close propagates finally exception", e)

log.clear()
gens = [resource(str(i), 3) for i in range(3)]
for gg in gens:
    next(gg)
for gg in reversed(gens):
    gg.close()
print("close order controlled by caller", log)

log.clear()
with contextlib.closing(resource("closing", 5)) as g:
    for v in g:
        if v == 1:
            break
print("contextlib.closing", log)
