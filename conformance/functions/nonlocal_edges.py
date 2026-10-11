"""nonlocal edge cases: skipping levels, shadowing, augmented ops, deletion, multiple names, globals vs nonlocal."""

x = "global"


def skip_level():
    x = "outer"

    def middle():
        def inner():
            nonlocal x
            x = "set by inner"

        inner()

    middle()
    return x


print("nonlocal skips a level", skip_level(), x)


def nearest_binding():
    v = "outermost"

    def mid():
        v = "mid"

        def inner():
            nonlocal v
            v = "inner wrote mid"

        inner()
        return v

    return mid(), v


print("nearest binding", nearest_binding())


def multiple_names():
    a, b = 1, 2

    def swap():
        nonlocal a, b
        a, b = b, a

    swap()
    return a, b


print("multiple names", multiple_names())


def augmented():
    n = 10
    s = "a"
    lst = [1]

    def go():
        nonlocal n, s, lst
        n += 5
        n //= 2
        s *= 3
        lst += [2]
        lst = lst + [3]

    go()
    return n, s, lst


print("augmented", augmented())


def nonlocal_delete():
    v = 1

    def remove():
        nonlocal v
        del v

    def read():
        return v

    remove()
    try:
        return read()
    except NameError as e:
        return type(e).__name__


print("nonlocal del", nonlocal_delete())


def nonlocal_rebind_after_delete():
    v = 1

    def remove():
        nonlocal v
        del v

    def restore():
        nonlocal v
        v = "restored"

    remove()
    restore()
    return v


print("rebind after del", nonlocal_rebind_after_delete())


def global_vs_nonlocal():
    x = "enclosing"

    def g():
        global x
        x = "global set"

    def n():
        nonlocal x
        x = "enclosing set"

    g()
    n()
    return x


print("global vs nonlocal", global_vs_nonlocal(), x)


def nonlocal_in_loop():
    total = 0
    funcs = []
    for i in range(4):
        def add(i=i):
            nonlocal total
            total += i
        funcs.append(add)
    for f in funcs:
        f()
    return total


print("nonlocal in loop", nonlocal_in_loop())


def nonlocal_as_loop_target():
    last = None

    def run():
        nonlocal last
        for last in range(5):
            pass

    run()
    return last


print("nonlocal loop target", nonlocal_as_loop_target())


def nonlocal_in_except_and_with():
    caught = None

    def run():
        nonlocal caught
        try:
            1 / 0
        except ZeroDivisionError as caught_exc:
            caught = type(caught_exc).__name__

    run()
    return caught


print("nonlocal from except", nonlocal_in_except_and_with())


def nonlocal_walrus():
    hit = 0

    def scan(xs):
        nonlocal hit
        return [y for y in xs if (hit := y) > 2]

    out = scan([1, 2, 3, 4])
    return out, hit


print("walrus into nonlocal from comprehension", nonlocal_walrus())


def nonlocal_unpacking():
    a = b = c = None

    def setter():
        nonlocal a, b, c
        a, (b, *c) = 1, (2, 3, 4)

    setter()
    return a, b, c


print("nonlocal unpacking", nonlocal_unpacking())


def nonlocal_inside_class_method():
    seen = []

    class Holder:
        def record(self, v):
            nonlocal seen
            seen = seen + [v]

    h = Holder()
    h.record(1)
    h.record(2)
    return seen


print("nonlocal from method", nonlocal_inside_class_method())
