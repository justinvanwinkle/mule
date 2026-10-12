"""dataclasses: fields, defaults, default_factory, repr/eq, order=True, frozen, __post_init__, InitVar."""

from dataclasses import FrozenInstanceError, InitVar, dataclass, field, fields


@dataclass
class Item:
    name: str
    price: float = 0.0
    tags: list = field(default_factory=list)
    hidden: int = field(default=7, repr=False, compare=False)


a = Item("apple", 1.5)
b = Item("apple", 1.5)
print("repr", a)
print("eq", a == b, a is b, a == Item("apple", 2.0))
a.tags.append("fruit")
print("default_factory distinct", a.tags, b.tags)
b.hidden = 99
print("compare=False field", a == b)
print("fields", [(f.name, f.type.__name__ if isinstance(f.type, type) else f.type) for f in fields(Item)])
print("hash disabled by eq", Item.__hash__)

try:
    @dataclass
    class Bad:
        xs: list = []
except ValueError as e:
    print("mutable default", type(e).__name__)

try:
    @dataclass
    class Order:
        a: int = 1
        b: int
except TypeError as e:
    print("non-default after default", type(e).__name__)


@dataclass(order=True)
class Version:
    major: int
    minor: int = 0
    label: str = field(default="", compare=False)


vs = [Version(1, 2), Version(1, 0, "x"), Version(0, 9), Version(1, 0, "y")]
print("sorted", sorted(vs))
print("compare", Version(1, 0) < Version(1, 1), Version(2) >= Version(1, 99), Version(1, 0, "a") == Version(1, 0, "b"))
try:
    Version(1) < (1, 0)
except TypeError as e:
    print("order vs tuple", type(e).__name__)


@dataclass(frozen=True)
class Coord:
    x: int
    y: int

    def moved(self, dx):
        return Coord(self.x + dx, self.y)


c = Coord(1, 2)
try:
    c.x = 5
except FrozenInstanceError as e:
    print("frozen set", type(e).__name__, isinstance(e, AttributeError))
try:
    del c.y
except FrozenInstanceError as e:
    print("frozen del", type(e).__name__)
print("frozen hashable", hash(c) == hash(Coord(1, 2)), {c: "v"}[Coord(1, 2)], c.moved(3))


@dataclass
class Rect:
    w: float
    h: float
    area: float = field(init=False)
    scale: InitVar[float] = 1.0

    def __post_init__(self, scale):
        self.w *= scale
        self.h *= scale
        self.area = self.w * self.h


r = Rect(2, 3, scale=2)
print("post_init", r, r.area)
print("InitVar not field", [f.name for f in fields(Rect)], hasattr(r, "scale"))
print("InitVar default class attr", Rect.scale)
try:
    Rect(1, 1, 5, 2)
except TypeError as e:
    print("init=False not accepted", type(e).__name__)


@dataclass
class Validated:
    value: int

    def __post_init__(self):
        if self.value < 0:
            raise ValueError(f"negative: {self.value}")


try:
    Validated(-1)
except ValueError as e:
    print("post_init validation", e)


@dataclass(eq=False)
class Identity:
    v: int


i1, i2 = Identity(1), Identity(1)
print("eq=False", i1 == i2, i1 == i1, Identity.__hash__ is object.__hash__)


@dataclass(unsafe_hash=True)
class Hashy:
    a: int
    b: list = field(default_factory=list, hash=False)


print("unsafe_hash", hash(Hashy(3)) == hash(Hashy(3, [1])))


@dataclass(repr=False)
class CustomRepr:
    x: int

    def __repr__(self):
        return f"<custom {self.x}>"


print("repr=False keeps custom", CustomRepr(4))


# ClassVar is excluded from fields
from typing import ClassVar


@dataclass
class WithClassVar:
    count: ClassVar[int] = 0
    v: int = 1


print("ClassVar excluded", [f.name for f in fields(WithClassVar)], WithClassVar(5))

# Positional and keyword construction, match_args
print("match_args", Item.__match_args__)
match Item("pear", 3.0):
    case Item(name, price=p) if p > 2:
        print("match dataclass", name, p)
