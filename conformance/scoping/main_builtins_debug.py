"""__name__ == "__main__", the builtins module, and __debug__."""

import builtins

print("__name__", __name__)
if __name__ == "__main__":
    print("running as main")
else:
    print("imported")

print("builtins.print is print", builtins.print is print)
builtins.print("called through builtins module")
print("builtins.len", builtins.len("abc"))
print("module __builtins__ refers to builtins", (__builtins__ if isinstance(__builtins__, type(builtins)) else None) is builtins)

# shadow print at module scope, still reachable via builtins
def print(*args):
    builtins.print("shadowed:", *args)


print("hello")
del print
print("unshadowed")

# add a new builtin: visible everywhere without import
builtins.brand_new_builtin = "visible everywhere"
print("new builtin", brand_new_builtin)


def reads_new_builtin():
    return brand_new_builtin


print("from function", reads_new_builtin())
del builtins.brand_new_builtin
try:
    reads_new_builtin()
except NameError as e:
    print("after removal", type(e).__name__)

# replace a builtin temporarily
original_len = builtins.len
builtins.len = lambda obj: -1
print("patched builtin len", len([1, 2, 3]))
builtins.len = original_len
print("restored builtin len", len([1, 2, 3]))

# module global shadows builtins module attribute
len = lambda obj: "module len"
builtins.len = lambda obj: "builtins len"
print("module global wins", len("x"))
del len
print("builtins attribute next", len("x"))
builtins.len = original_len
print("fully restored", len("x"))

# builtins namespace dict
print("vars(builtins) has print", "print" in vars(builtins))
print("getattr builtins", builtins.max(1, 5))

# __debug__
print("__debug__", __debug__)
if __debug__:
    print("debug block runs")
try:
    assert False, "assert message"
except AssertionError as e:
    print("assert active", e)

try:
    exec("__debug__ = False")
except SyntaxError as e:
    print("assign __debug__", type(e).__name__)

# other module-level dunders
print("__doc__", __doc__)
print("__spec__ is None for main script", __spec__ is None)
print("__package__", repr(__package__))

import sys

print("sys.modules['__main__'] is this module", sys.modules["__main__"].__dict__ is globals())

# __name__ inside functions and classes
def show_name():
    return __name__


class Named:
    module = __name__


print("function __name__ global", show_name(), Named.module, show_name.__module__)

# rebinding __name__ affects later class __module__
__name__ = "renamed"


class AfterRename:
    pass


print("class module after rename", AfterRename.__module__)
__name__ = "__main__"
