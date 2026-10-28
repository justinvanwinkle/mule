"""Assigning a class's __bases__ recomputes its MRO and its subclasses'."""


class A:
    def who(self):
        return "A"


class B:
    def who(self):
        return "B"


class C(A):
    pass


class D(C):
    pass


d = D()
print("before", d.who(), [k.__name__ for k in D.__mro__])
C.__bases__ = (B,)
print("after", d.who(), [k.__name__ for k in D.__mro__], C.__base__.__name__)
print("subclasses", [k.__name__ for k in A.__subclasses__()], [k.__name__ for k in B.__subclasses__()])
C.__bases__ = (A, B)
print("two", d.who(), [k.__name__ for k in C.__mro__])

for value in [(), [A], (1,), (D,)]:
    try:
        C.__bases__ = value
    except TypeError as error:
        print("rejected", type(error).__name__, error)
print("unchanged", [k.__name__ for k in C.__mro__])

try:
    int.__bases__ = (object,)
except TypeError as error:
    print("builtin", type(error).__name__, error)


class Slotted:
    __slots__ = ("x",)


class Other:
    __slots__ = ("y",)


class Child(Slotted):
    __slots__ = ()


try:
    Child.__bases__ = (Other,)
except TypeError as error:
    print("layout", type(error).__name__)
