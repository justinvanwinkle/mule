"""abc.ABC and abstractmethod, ABC registration, __subclasshook__, and metaclass __instancecheck__/__subclasscheck__."""

import abc
from collections.abc import Iterable, Sized, Hashable, Container, Sequence, Mapping


class Shape(abc.ABC):
    @abc.abstractmethod
    def area(self):
        ...

    @property
    @abc.abstractmethod
    def name(self):
        ...

    def describe(self):
        return f"{self.name} with area {self.area()}"


try:
    Shape()
except TypeError as e:
    print("instantiate abstract", type(e).__name__)


class Partial(Shape):
    def area(self):
        return 0


try:
    Partial()
except TypeError as e:
    print("partially implemented", type(e).__name__)
print("abstract methods", sorted(Partial.__abstractmethods__))


class Square(Shape):
    def __init__(self, s):
        self.s = s

    def area(self):
        return self.s ** 2

    @property
    def name(self):
        return "square"


print("concrete", Square(3).describe(), sorted(Square.__abstractmethods__))


# Calling abstract implementation via super()
class Base(abc.ABC):
    @abc.abstractmethod
    def run(self):
        return "base-run"


class Impl(Base):
    def run(self):
        return "impl+" + super().run()


print("super to abstract", Impl().run())


# abstract classmethod / staticmethod
class Factory(abc.ABC):
    @classmethod
    @abc.abstractmethod
    def build(cls):
        ...


class ConcreteFactory(Factory):
    @classmethod
    def build(cls):
        return cls.__name__


print("abstract classmethod", ConcreteFactory.build(), sorted(Factory.__abstractmethods__))


# Using ABCMeta directly
class Plugin(metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def go(self):
        ...


print("ABCMeta direct", type(Plugin).__name__, Plugin.__abstractmethods__ == frozenset({"go"}))


# Virtual subclass registration
class Duck:
    pass


Plugin.register(Duck)
print("registered isinstance", isinstance(Duck(), Plugin), issubclass(Duck, Plugin))
print("registered not in mro", Plugin in Duck.__mro__)

# register can be used as a decorator and returns the class
@Plugin.register
class Goose:
    pass


print("register decorator", Goose.__name__, issubclass(Goose, Plugin))


# __subclasshook__
class HasQuack(abc.ABC):
    @classmethod
    def __subclasshook__(cls, C):
        if cls is HasQuack:
            if any("quack" in B.__dict__ for B in C.__mro__):
                return True
        return NotImplemented


class Mallard:
    def quack(self):
        return "quack"


class Robot:
    pass


class ToyMallard(Mallard):
    pass


print("subclasshook", issubclass(Mallard, HasQuack), issubclass(Robot, HasQuack), isinstance(ToyMallard(), HasQuack))


# Structural checks against collections.abc
class Bag:
    def __init__(self, *items):
        self.items = list(items)

    def __len__(self):
        return len(self.items)

    def __iter__(self):
        return iter(self.items)

    def __contains__(self, x):
        return x in self.items


b = Bag(1, 2)
print("collections.abc structural", isinstance(b, Sized), isinstance(b, Iterable), isinstance(b, Container))
print("not Sequence", isinstance(b, Sequence), isinstance([], Sequence), isinstance({}, Mapping))


class Unhashable:
    __hash__ = None


print("Hashable", isinstance(Unhashable(), Hashable), isinstance(1, Hashable), isinstance([], Hashable))


# Metaclass __instancecheck__ / __subclasscheck__
class EvenMeta(type):
    def __instancecheck__(cls, obj):
        return isinstance(obj, int) and obj % 2 == 0

    def __subclasscheck__(cls, sub):
        return sub is bool


class Even(metaclass=EvenMeta):
    pass


print("instancecheck", isinstance(4, Even), isinstance(3, Even), isinstance("4", Even))
print("subclasscheck", issubclass(bool, Even), issubclass(int, Even))
print("tuple of classes", isinstance(3, (str, Even)), isinstance(3, (int, Even)))

# Exact type match short-circuits __instancecheck__
class AlwaysNo(type):
    def __instancecheck__(cls, obj):
        return False


class Never(metaclass=AlwaysNo):
    pass


print("exact type shortcut", isinstance(Never(), Never), type(Never()) is Never)

# isinstance with union types (3.10+)
print("union isinstance", isinstance(3, int | str), isinstance(3.0, int | str), issubclass(bool, int | None))

# issubclass errors
try:
    issubclass(3, int)
except TypeError as e:
    print("issubclass non-class", type(e).__name__)

# Abstract methods added after class creation need update_abstractmethods
class Late(abc.ABC):
    pass


def needed(self):
    ...


Late.needed = abc.abstractmethod(needed)
print("before update", Late() is not None)
abc.update_abstractmethods(Late)
try:
    Late()
except TypeError as e:
    print("after update", type(e).__name__)
