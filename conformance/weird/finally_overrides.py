"""finally clauses that return/break/continue, overriding pending exceptions and returns, nested in loops and generators."""

def return_overrides_exception():
    try:
        raise ValueError("lost")
    finally:
        return "finally wins"


print("return swallows exception", return_overrides_exception())


def return_overrides_return():
    try:
        return "try"
    finally:
        return "finally"


print("finally return beats try return", return_overrides_return())


def finally_sees_but_does_not_change():
    x = "before"
    try:
        return x
    finally:
        x = "changed"


print("return value captured before finally", finally_sees_but_does_not_change())


def mutable_return():
    lst = []
    try:
        return lst
    finally:
        lst.append("added in finally")


print("mutable return value mutated", mutable_return())


def break_swallows():
    out = []
    for i in range(3):
        try:
            out.append(i)
            raise KeyError(i)
        finally:
            break
    return out


print("break in finally swallows exception", break_swallows())


def continue_swallows():
    out = []
    for i in range(4):
        try:
            if i % 2:
                raise RuntimeError(i)
            out.append(("ok", i))
        finally:
            continue
    return out


print("continue in finally swallows", continue_swallows())


def break_cancels_return():
    for i in range(5):
        try:
            return f"returned {i}"
        finally:
            if i < 2:
                continue
            break
    return "fell out of loop"


print("continue/break cancel return", break_cancels_return())


def nested_finally():
    log = []
    try:
        try:
            raise ValueError("inner")
        finally:
            log.append("inner finally")
            raise TypeError("replacement")
    except TypeError as e:
        log.append(("caught", str(e), type(e.__context__).__name__))
    finally:
        log.append("outer finally")
    return log


print("finally raising replaces with context", nested_finally())


def gen_with_finally():
    for i in range(5):
        try:
            yield i
            if i == 2:
                raise StopIteration  # becomes RuntimeError in a generator unless swallowed
        finally:
            if i == 2:
                return "early"


g = gen_with_finally()
items = []
while True:
    try:
        items.append(next(g))
    except StopIteration as e:
        items.append(("stop", e.value))
        break
print("generator finally return swallows", items)


def gen_finally_yields():
    try:
        yield "body"
    finally:
        yield "from finally"


g = gen_finally_yields()
print("first", next(g))
try:
    g.close()
except RuntimeError as e:
    print("yield in finally during close", type(e).__name__)


def gen_finally_break():
    for i in range(3):
        try:
            yield i
        finally:
            if i == 1:
                break
    yield "after loop"


g = gen_finally_break()
print("gen next", next(g), next(g))
print("throw swallowed by finally break", g.throw(ValueError("x")))


def finally_order():
    log = []
    for i in range(2):
        try:
            try:
                log.append(f"try{i}")
                if i:
                    return log
            finally:
                log.append(f"inner{i}")
        finally:
            log.append(f"outer{i}")
    return log


print("finally order on return", finally_order())


def exception_in_except_then_finally_return():
    try:
        raise ValueError
    except ValueError:
        raise TypeError
    finally:
        return "finally beats except raise"


print(exception_in_except_then_finally_return())


def while_else_with_finally():
    n = 0
    while n < 3:
        try:
            n += 1
            if n == 2:
                break
        finally:
            n += 10
    else:
        return ("else ran", n)
    return ("broke", n)


print("finally bumps loop variable past condition", while_else_with_finally())


def sys_exc_in_finally():
    import sys
    try:
        try:
            raise LookupError("pending")
        finally:
            pending = sys.exception()
    except LookupError:
        pass
    return type(pending).__name__


print("pending exception visible in finally", sys_exc_in_finally())
