"""Entries type creation and the class-body compiler add to a class namespace."""

import weakref


def decorate(cls):
    return cls


class Plain:
    pass


@decorate

class Body:
    """documented"""

    value = 1

    def __init__(self, item):
        self.item = item
        self.first, [self.second, *self.rest] = 1, [2, 3]
        other = self
        other.not_counted = 0

        def inner():
            self.from_inner = 4

        inner()

    def method(this):
        this.not_self = 5

    def loops(self):
        [self.comprehension for self.comprehension in range(1)]
        for self.loop in range(1):
            pass
        self.augmented = 0
        self.augmented += 1
        del self.augmented
        self.annotated: int = 1
        self.declared: int

    class Nested:
        def set(self):
            self.nested_only = 1


class Slotted:
    __slots__ = ("a",)


class SlottedWeak:
    __slots__ = ("a", "__weakref__")


class ListChild(list):
    pass


class TupleChild(tuple):
    pass


class Meta(type):
    pass


class Child(Plain):
    pass


class OwnDict:
    @property
    def __dict__(self):
        return "own"


for cls in (Plain, Body, Slotted, SlottedWeak, ListChild, TupleChild, Meta, Child, OwnDict):
    namespace = vars(cls)
    print(cls.__name__, list(namespace))
    print("  firstlineno", namespace.get("__firstlineno__"))
    print("  static", namespace.get("__static_attributes__"))
    print("  doc", namespace.get("__doc__"))
    for name in ("__dict__", "__weakref__"):
        if name in namespace:
            print("  ", name, type(namespace[name]).__name__)

print("nested static", Body.Nested.__static_attributes__)

instance = Body("x")
instance.extra = 1
descriptor = type.__dict__["__dict__"].__get__(Plain)["__dict__"]
print("descriptor", type(descriptor).__name__, descriptor.__name__)
print("instance dict", sorted(descriptor.__get__(instance)))
print("own dict", OwnDict().__dict__, type(vars(OwnDict)["__dict__"]).__name__)

print("weakref before", instance.__weakref__)
reference = weakref.ref(instance)
print("weakref after", instance.__weakref__ is reference)

try:
    descriptor.__get__(5)
except TypeError as error:
    print("wrong receiver", type(error).__name__)
