"""except* handling: multiple types, unhandled remainder re-raised as a group, raising inside except*."""


def names(g):
    if isinstance(g, BaseExceptionGroup):
        return "[" + ", ".join(names(x) for x in g.exceptions) + "]"
    return type(g).__name__


def basic():
    log = []
    try:
        raise ExceptionGroup("eg", [ValueError("a"), TypeError("b"), ValueError("c")])
    except* ValueError as g:
        log.append("ValueError group " + names(g) + " " + g.message)
    except* TypeError as g:
        log.append("TypeError group " + names(g))
    return log


print("basic:", basic())


def tuple_types():
    try:
        raise ExceptionGroup("eg", [KeyError(1), IndexError(2), OSError()])
    except* (KeyError, IndexError) as g:
        print("tuple handler got", names(g))
    except* OSError:
        print("OSError handler ran")


tuple_types()


def remainder():
    try:
        raise ExceptionGroup("orig", [ValueError("v"), KeyError("k"), ZeroDivisionError()])
    except* ValueError:
        print("handled ValueError only")


try:
    remainder()
except ExceptionGroup as g:
    print("remainder re-raised:", g.message, names(g))


# A naked exception is wrapped into a group for except*.
try:
    raise KeyError("naked")
except* KeyError as g:
    print("naked wrapped:", type(g).__name__, names(g), repr(g.message))


def naked_unhandled():
    try:
        raise OSError("naked")
    except* ValueError:
        print("wrong")


try:
    naked_unhandled()
except OSError as e:
    print("unhandled naked re-raised bare:", type(e).__name__)
except ExceptionGroup:
    print("re-raised as group")


# Nested groups: structure is preserved in the matched subgroup.
try:
    raise ExceptionGroup("top", [
        ValueError("1"),
        ExceptionGroup("mid", [ValueError("2"), TypeError("3")]),
    ])
except* ValueError as g:
    print("nested match structure:", names(g))
except* TypeError as g:
    print("nested TypeError structure:", names(g))


# Raising inside except*: new exceptions are combined with unhandled ones.
def raise_inside():
    try:
        raise ExceptionGroup("src", [ValueError("v"), TypeError("t"), KeyError("k")])
    except* ValueError:
        raise RuntimeError("from handler")
    except* TypeError:
        raise


try:
    raise_inside()
except ExceptionGroup as g:
    print("combined:", names(g), "message", repr(g.message))
    for sub in g.exceptions:
        ctx = sub.__context__
        print("  member", names(sub), "context", names(ctx) if ctx is not None else None)


# Each except* clause runs at most once; a handler may see several leaves.
count = 0
try:
    raise ExceptionGroup("many", [ValueError(i) for i in range(4)])
except* ValueError as g:
    count += 1
    print("leaves in one handler:", len(g.exceptions))
print("handler invocations:", count)


# else and finally with except*
for fail in (False, True):
    log = []
    try:
        if fail:
            raise ExceptionGroup("x", [ValueError()])
        log.append("body")
    except* ValueError:
        log.append("star handler")
    else:
        log.append("else")
    finally:
        log.append("finally")
    print("star else/finally fail=" + str(fail), log)


# Handling everything with except* Exception.
try:
    raise ExceptionGroup("all", [ValueError(), KeyError()])
except* Exception as g:
    print("except* Exception got", names(g))


# except* with BaseExceptionGroup containing BaseException
try:
    try:
        raise BaseExceptionGroup("b", [KeyboardInterrupt(), ValueError()])
    except* ValueError:
        print("handled ValueError beside KeyboardInterrupt")
except BaseExceptionGroup as g:
    print("leftover:", type(g).__name__, names(g))


# Raising the matched group itself (raise g) vs bare raise.
try:
    try:
        raise ExceptionGroup("orig", [ValueError("x")])
    except* ValueError as g:
        raise g
except ExceptionGroup as outer:
    print("raise g:", names(outer))
