"""LEGB name resolution: local, enclosing, global, and builtin lookup order."""

x = "global x"


def local_wins():
    x = "local x"
    return x


def enclosing():
    x = "enclosing x"

    def inner():
        return x

    return inner()


def global_fallback():
    def inner():
        return x

    return inner()


def builtin_fallback():
    return len("abc"), type(len).__name__


print("local", local_wins())
print("enclosing", enclosing())
print("global", global_fallback())
print("builtin", builtin_fallback())


# multi-level enclosing: nearest enclosing binding wins
def level1():
    name = "level1"

    def level2():
        def level3():
            def level4():
                return name

            return level4()

        return level3()

    def level2_shadow():
        name = "level2"

        def level3():
            return name

        return level3()

    return level2(), level2_shadow()


print("nearest enclosing", level1())


# globals are looked up at call time, not definition time
def read_late():
    return late_global


try:
    read_late()
except NameError as e:
    print("late global before definition", type(e).__name__)
late_global = "now defined"
print("late global after definition", read_late())


# enclosing variables are looked up at call time too (cells)
def make():
    value = "first"

    def get():
        return value

    value = "second"
    return get


print("cell sees final value", make()())


# a class scope is skipped by nested functions; global is used instead
y = "global y"


class C:
    y = "class y"

    def method(self):
        return y

    lam = lambda self: y


print("method skips class scope", C().method(), C().lam())


# parameters are locals
def params(x, *args, key=None, **kw):
    return x, args, key, kw


print("params shadow global", params("param", 1, key=2, extra=3))
print("global x unchanged", x)


# name assigned anywhere in function is local everywhere in it
def conditional_local(flag):
    if flag:
        x = "assigned"
    try:
        return x
    except UnboundLocalError as e:
        return type(e).__name__


print("conditional local true", conditional_local(True))
print("conditional local false", conditional_local(False))


# import, def, class, for, with, except-as, and del all create local bindings
def binders():
    def f():
        return "f"
    class K:
        pass
    for i in range(1):
        pass
    with open("legb_tmp.txt", "w") as fh:
        pass
    return sorted(k for k in locals())


print("binding statements", binders())


# lambda default arguments are evaluated in the enclosing scope
z = "global z"


def lambda_defaults():
    z = "local z"
    return (lambda z=z: z)(), (lambda: z)()


print("lambda defaults", lambda_defaults())

# builtins can be found from deeply nested scopes
def deep():
    def a():
        def b():
            return max(1, 2), isinstance(1, int)
        return b()
    return a()


print("deep builtin", deep())
