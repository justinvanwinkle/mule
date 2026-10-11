"""isinstance/issubclass with classes, tuples, nested tuples, X | Y unions, ABC-free hooks, and errors."""


class Animal:
    pass


class Dog(Animal):
    pass


class Cat(Animal):
    pass


d = Dog()
print("basic", isinstance(d, Dog), isinstance(d, Animal), isinstance(d, Cat), isinstance(d, object))
print("tuple", isinstance(d, (Cat, Dog)), isinstance(d, (Cat, int)), isinstance(d, ()))
print("nested tuple", isinstance(d, (int, (str, (Dog,)))))
print("union", isinstance(5, int | str), isinstance("s", int | str), isinstance(5.0, int | str))
print("union none", isinstance(None, int | None), isinstance(0, type(None) | float))
print("union in tuple", isinstance(b"", (int | str, bytes)))
print("bool is int", isinstance(True, int), issubclass(bool, int), isinstance(1, bool))
print("builtin types", isinstance([], list), isinstance((), tuple), isinstance({}, dict), isinstance(range(1), range))
print("type objects", isinstance(int, type), isinstance(type, type), isinstance(type, object), isinstance(object, type))

print("issubclass", issubclass(Dog, Animal), issubclass(Animal, Dog), issubclass(Dog, Dog))
print("issubclass tuple", issubclass(Cat, (Dog, Animal)), issubclass(Cat, (int, str)))
print("issubclass union", issubclass(bool, int | str), issubclass(float, int | str))
print("issubclass object", issubclass(Dog, object), issubclass(type, object))
print("mro names", [c.__name__ for c in Dog.__mro__])

for label, call in [
    ("isinstance non type", lambda: isinstance(1, 1)),
    ("issubclass instance", lambda: issubclass(d, Animal)),
    ("issubclass non type second", lambda: issubclass(Dog, d)),
    ("isinstance list of types", lambda: isinstance(1, [int])),
    ("isinstance generic alias", lambda: isinstance([], list[int])),
]:
    try:
        print(label, call())
    except TypeError as e:
        print(label, type(e).__name__)


class Meta(type):
    def __instancecheck__(cls, obj):
        return obj == "duck"

    def __subclasscheck__(cls, sub):
        return sub is str


class Duck(metaclass=Meta):
    pass


print("instancecheck hook", isinstance("duck", Duck), isinstance("goose", Duck))
print("subclasscheck hook", issubclass(str, Duck), issubclass(int, Duck))
print("exact type skips hook", isinstance(Duck(), Duck))


class Fake:
    @property
    def __class__(self):
        return Dog


print("__class__ override", isinstance(Fake(), Dog), type(Fake()).__name__)


class Multi(Dog, Cat):
    pass


print("multiple inheritance", isinstance(Multi(), Cat), issubclass(Multi, (Cat,)), [c.__name__ for c in Multi.__mro__])
type IntOrStr = int | str
print("type alias value", isinstance(3, IntOrStr.__value__))
try:
    isinstance(3, IntOrStr)
except TypeError as e:
    print("type alias direct", type(e).__name__)
