"""Shadowing builtins at module and function scope and restoring them with del."""

print("builtin len", len([1, 2]))


def len(obj):
    return "module len"


print("module shadow", len([1, 2]))


def uses_len():
    return len("abc")


print("function sees module shadow", uses_len())

del len
print("restored after del", len([1, 2]), uses_len())


def local_shadow():
    list = tuple
    return list([1, 2])


print("local shadow", local_shadow(), list((1, 2)))


def local_shadow_then_del():
    sum = lambda xs: "local sum"
    first = sum([1, 2])
    del sum
    try:
        second = sum([1, 2])
    except UnboundLocalError as e:
        second = type(e).__name__
    return first, second


print("del local does not restore builtin", local_shadow_then_del())

# shadowing via assignment, for loop, import, class and parameter
str = "not a type"
print("str is", str)
del str
print("str restored", str(5))

for print_ in range(1):
    pass

for id in range(2):
    pass
print("id shadowed by loop", id)
del id
print("id restored is callable", callable(id))


class range:
    def __init__(self, n):
        self.n = n

    def __iter__(self):
        return iter(["custom"] * self.n)


print("class shadows range", list(range(2)))
del range
print("range restored", list(range(2)))


def param_shadow(min, max=None):
    return min, max


print("param shadow", param_shadow(1, 2), min(3, 4))

# shadowing in a nested function affects only it
def outer():
    def inner():
        abs = lambda v: "inner abs"
        return abs(-1)
    return inner(), abs(-1)


print("nested shadow", outer())

# enclosing-scope shadow visible to nested function
def enclosing_shadow():
    sorted = lambda xs: "enclosing sorted"

    def inner():
        return sorted([2, 1])

    return inner()


print("enclosing shadow", enclosing_shadow())

# deleting a name that only exists as a builtin at module level raises NameError
try:
    del abs
except NameError as e:
    print("del builtin from module", type(e).__name__)
print("abs still works", abs(-3))

# shadowing True/None is impossible, but shadowing Ellipsis and NotImplemented is allowed
Ellipsis = "shadowed ellipsis"
print("Ellipsis name", Ellipsis, ... is not Ellipsis)
del Ellipsis
print("Ellipsis restored", Ellipsis is ...)

# globals() update can shadow a builtin
globals()["max"] = lambda *a: "global max"
print("globals() shadow", max(1, 2))
del globals()["max"]
print("globals() restored", max(1, 2))

# comprehension iteration variable named like a builtin does not leak
vals = [type for type in (1, 2)]
print("comprehension var did not leak", vals, type(1).__name__)

# exception handler variable named like a builtin is deleted afterwards
try:
    raise ValueError
except ValueError as input:
    pass
print("except-as builtin name unbound, builtin visible again", callable(input))
