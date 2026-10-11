"""Single and multiple inheritance, diamond MRO, cooperative super() chains, and MRO errors."""


def mro_names(cls):
    return [c.__name__ for c in cls.__mro__]


class Animal:
    sound = "..."

    def speak(self):
        return f"{type(self).__name__} says {self.sound}"

    def describe(self):
        return "animal"


class Dog(Animal):
    sound = "woof"

    def describe(self):
        return "dog/" + super().describe()


class Puppy(Dog):
    def describe(self):
        return "puppy/" + super().describe()


print("single", Puppy().speak(), Puppy().describe())
print("mro puppy", mro_names(Puppy))
print("bases", [c.__name__ for c in Puppy.__bases__], Puppy.__base__.__name__)


# Diamond with cooperative super().__init__ passing kwargs.
class Base:
    def __init__(self, **kw):
        self.log = ["Base"]
        self.leftover = sorted(kw)

    def who(self):
        return ["Base"]


class Left(Base):
    def __init__(self, left=None, **kw):
        super().__init__(**kw)
        self.log.append(f"Left({left})")

    def who(self):
        return ["Left"] + super().who()


class Right(Base):
    def __init__(self, right=None, **kw):
        super().__init__(**kw)
        self.log.append(f"Right({right})")

    def who(self):
        return ["Right"] + super().who()


class Bottom(Left, Right):
    def __init__(self, **kw):
        super().__init__(**kw)
        self.log.append("Bottom")

    def who(self):
        return ["Bottom"] + super().who()


bt = Bottom(left=1, right=2, extra=3)
print("diamond mro", mro_names(Bottom))
print("diamond init log", bt.log)
print("diamond leftover", bt.leftover)
print("diamond who", bt.who())
print("mro() method", [c.__name__ for c in Bottom.mro()])


# Larger C3 linearization example.
class O: pass
class A(O): pass
class B(O): pass
class C(O): pass
class D(O): pass
class E(O): pass
class K1(A, B, C): pass
class K2(D, B, E): pass
class K3(D, A): pass
class Z(K1, K2, K3): pass


print("c3", mro_names(Z))

# Inconsistent MRO
try:
    class X(O): pass
    class Y(O, X): pass
except TypeError as e:
    print("bad mro", type(e).__name__)

try:
    class Dup(A, A): pass
except TypeError as e:
    print("duplicate base", type(e).__name__)


# Method resolution picks the first in MRO, attributes too.
class M1:
    x = "M1"

    def f(self):
        return "M1.f"


class M2:
    x = "M2"

    def f(self):
        return "M2.f"

    def g(self):
        return "M2.g"


class MM(M1, M2):
    pass


class MM2(M2, M1):
    pass


print("first wins", MM().f(), MM().x, MM().g(), MM2().f(), MM2().x)

# Explicit base call bypassing MRO
print("explicit base", M2.f(MM()))

# issubclass across hierarchy
print("issubclass", issubclass(Bottom, Base), issubclass(Base, Bottom), issubclass(Z, (int, K3)))
print("subclasses", sorted(c.__name__ for c in Base.__subclasses__()))

# Overriding then extending class attribute via inheritance
class Root:
    registry = ("root",)


class Child(Root):
    registry = Root.registry + ("child",)


print("extended attr", Child.registry, Root.registry)
print("object mro", mro_names(object), mro_names(bool))
