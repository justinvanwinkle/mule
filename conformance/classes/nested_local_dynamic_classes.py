"""Nested classes and __qualname__, classes inside functions capturing closures, and type(name, bases, dict)."""


class Outer:
    class Inner:
        class Deep:
            def where(self):
                return type(self).__qualname__

        def method(self):
            pass

    def make_inner(self):
        # Inner is not in scope unqualified inside methods
        try:
            return Inner  # noqa: F821
        except NameError as e:
            return type(e).__name__


print("qualname", Outer.Inner.__qualname__, Outer.Inner.Deep.__qualname__)
print("name", Outer.Inner.Deep.__name__, Outer.Inner.Deep().where())
print("method qualname", Outer.Inner.method.__qualname__)
print("inner not in method scope", Outer().make_inner())
print("module", Outer.Inner.__module__)


def factory(base_value):
    counter = 0

    class Local:
        value = base_value

        def get(self):
            return base_value

        def bump(self):
            nonlocal counter
            counter += 1
            return counter

        @classmethod
        def peek(cls):
            return counter

    return Local


L1 = factory(10)
L2 = factory(20)
print("local qualname", L1.__qualname__)
print("closures", L1().get(), L2().get(), L1.value, L2.value)
x1 = L1()
print("nonlocal shared within class", x1.bump(), L1().bump(), L1.peek(), L2.peek())
print("distinct classes", L1 is L2, isinstance(L1(), L2))


def make_hierarchy(n):
    classes = []
    base = object
    for i in range(n):
        class Step(base):
            idx = i  # bound at class creation
            def who(self, _i=i):
                return f"step{_i}"
        classes.append(Step)
        base = Step
    return classes


steps = make_hierarchy(3)
print("loop classes", [c.idx for c in steps], steps[-1]().who(), len(steps[-1].__mro__))


# Class body sees enclosing function locals, but class-level names don't leak to methods.
def scope_test():
    shared = "function-local"

    class C:
        shared = "class-level"
        seen_in_body = shared

        def meth(self):
            return shared

    return C.seen_in_body, C().meth()


print("class body vs method scope", scope_test())


# Class body referencing a function local that is later rebound
def late_binding():
    val = "first"

    class C:
        def get(self):
            return val

    val = "second"
    return C().get()


print("late binding", late_binding())


# type(name, bases, dict)
def dyn_init(self, x):
    self.x = x


Dyn = type("Dyn", (), {"__init__": dyn_init, "double": lambda self: self.x * 2, "kind": "dynamic"})
d = Dyn(21)
print("type() class", Dyn.__name__, Dyn.__qualname__, d.double(), d.kind)

DynSub = type("DynSub", (Dyn,), {"double": lambda self: super(DynSub, self).double() + 1})
print("type() subclass", DynSub(5).double(), [c.__name__ for c in DynSub.__mro__])

print("type one-arg", type(d).__name__, type(Dyn).__name__)
try:
    type("Bad", (5,), {})
except TypeError as e:
    print("type bad base", type(e).__name__)
try:
    type("Bad", ())
except TypeError as e:
    print("type wrong arity", type(e).__name__)

# type() with __slots__ and __qualname__ in dict
Slotted = type("Slotted", (), {"__slots__": ("a",), "__qualname__": "Custom.Slotted"})
sl = Slotted()
sl.a = 1
print("type slots", sl.a, Slotted.__qualname__, hasattr(sl, "__dict__"))

# Dynamic class with __init_subclass__ hook via kwargs
class HookBase:
    def __init_subclass__(cls, **kw):
        cls.kw = sorted(kw.items())


import types

Made = types.new_class("Made", (HookBase,), {"flag": 1}, lambda ns: ns.update(attr="from exec_body"))
print("new_class", Made.kw, Made.attr, Made.__name__)

# Nested class accessing outer class: must go through the name.
class Tree:
    class Node:
        def __init__(self, v):
            self.v = v

        def make_tree(self):
            return Tree(self)

    def __init__(self, root):
        self.root = root


tr = Tree.Node(3).make_tree()
print("nested refers outer", type(tr).__name__, tr.root.v)

# __qualname__ of a function inside a method inside a nested class
class A:
    class B:
        def m(self):
            def inner():
                pass

            return inner.__qualname__


print("deep function qualname", A.B().m())
print("lambda qualname", (lambda: 0).__qualname__)
