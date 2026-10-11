"""yield inside control flow: loops with else, try/except/else/finally, with, match, while-break, and nested defs."""

log = []


def loop_else(items, stop):
    for x in items:
        if x == stop:
            yield "break at " + str(x)
            break
        yield x
    else:
        yield "loop else"


print("for-else completes", list(loop_else([1, 2], 9)))
print("for-else breaks", list(loop_else([1, 2, 3], 2)))


def while_loop():
    n = 0
    while n < 10:
        n += 1
        if n % 3:
            continue
        yield n
    else:
        yield "while else"


print("while with continue", list(while_loop()))


def try_branches(fail):
    try:
        yield "try"
        if fail:
            raise ValueError
    except ValueError:
        yield "except"
    else:
        yield "else"
    finally:
        yield "finally"


print("try success", list(try_branches(False)))
print("try failure", list(try_branches(True)))


def yield_in_except_handler():
    try:
        raise KeyError("k")
    except KeyError as e:
        yield type(e).__name__
        yield "still in handler"
    yield "after"


print("yield in handler", list(yield_in_except_handler()))


class CM:
    def __enter__(self):
        log.append("enter")
        return "resource"

    def __exit__(self, *a):
        log.append("exit")


def with_block():
    with CM() as r:
        yield r
        log.append("between")
        yield r.upper()


print("with block", list(with_block()), log)


def matcher(values):
    for v in values:
        match v:
            case int(n) if n > 0:
                yield "pos int " + str(n)
            case [a, b]:
                yield "pair " + str(a + b)
            case {"k": val}:
                yield "mapping " + str(val)
            case str() as s:
                yield "str " + s
            case _:
                yield "other"


print("match", list(matcher([5, -1, [1, 2], {"k": 9}, "hi", None])))


def nested_loops():
    for i in range(3):
        for j in range(3):
            if j > i:
                break
            yield (i, j)


print("nested loops", list(nested_loops()))


def generator_defines_function():
    def helper(x):
        return x * 100

    for i in range(3):
        yield helper(i)


print("inner function", list(generator_defines_function()))


def generator_defines_generator():
    def sub(n):
        yield from range(n)

    for i in range(4):
        yield list(sub(i))


print("inner generator", list(generator_defines_generator()))


def yield_in_comprehension_iterable():
    # The outermost iterable of a comprehension is evaluated in the enclosing scope,
    # but a yield expression there is a SyntaxError, so use a value computed by yield.
    received = yield "send me a list"
    yield [x * 2 for x in received]


g = yield_in_comprehension_iterable()
print("prompt", next(g))
print("comprehension over sent value", g.send([1, 2, 3]))


def conditional_yields(flag):
    if flag:
        yield "a"
    elif flag is None:
        yield "none"
    yield "always"


print("conditional", list(conditional_yields(True)), list(conditional_yields(None)), list(conditional_yields(False)))


def ternary_yield(x):
    v = (yield "even") if x % 2 == 0 else (yield "odd")
    yield v


g = ternary_yield(3)
print("ternary yield", next(g), g.send("sent"))


def yield_in_lambda_default():
    fns = []
    for i in range(2):
        fns.append(lambda i=i: i)
        yield fns[-1]()


print("loop lambda defaults", list(yield_in_lambda_default()))


def return_inside_finally_loop():
    for i in range(5):
        try:
            if i == 2:
                return
            yield i
        finally:
            log.append("finally " + str(i))


log.clear()
print("return in loop with finally", list(return_inside_finally_loop()), log)
