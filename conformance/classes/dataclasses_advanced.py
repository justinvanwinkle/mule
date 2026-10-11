"""dataclasses: kw_only, KW_ONLY, slots=True, replace, asdict/astuple, inheritance, field overriding, make_dataclass."""

import dataclasses
from dataclasses import dataclass, field, replace, asdict, astuple, KW_ONLY, fields, make_dataclass


@dataclass(kw_only=True)
class Opts:
    verbose: bool = False
    level: int


o = Opts(level=3)
print("kw_only", o)
try:
    Opts(True, 3)
except TypeError as e:
    print("kw_only positional", type(e).__name__)


@dataclass
class Mixed:
    a: int
    _: KW_ONLY
    b: int = 2
    c: int


print("KW_ONLY sentinel", Mixed(1, c=3), [f.name for f in fields(Mixed)])


@dataclass
class PerField:
    x: int
    y: int = field(default=0, kw_only=True)
    z: int = 5


print("per-field kw_only", PerField(1, 9), PerField(1, 9, y=4))


@dataclass(slots=True)
class Slim:
    a: int
    b: str = "s"


s = Slim(1)
print("slots", s, Slim.__slots__, hasattr(s, "__dict__"))
try:
    s.c = 3
except AttributeError as e:
    print("slots unknown attr", type(e).__name__)


@dataclass(frozen=True, slots=True)
class FrozenSlim:
    v: int


print("frozen slots", FrozenSlim(2), hash(FrozenSlim(2)) == hash(FrozenSlim(2)))


@dataclass
class Inner:
    n: int
    tags: list = field(default_factory=list)


@dataclass
class Outer:
    name: str
    inner: Inner
    items: list = field(default_factory=list)
    mapping: dict = field(default_factory=dict)


ou = Outer("o", Inner(1, ["t"]), [Inner(2), (Inner(3),)], {"k": Inner(4)})
print("asdict", asdict(ou))
print("astuple", astuple(ou))
d = asdict(ou)
d["inner"]["tags"].append("changed")
print("asdict deep copies", ou.inner.tags)
print("asdict dict_factory", asdict(Inner(5, ["a"]), dict_factory=lambda kv: sorted(kv)))

r = replace(ou, name="o2")
print("replace", r.name, r.inner is ou.inner, ou.name)
try:
    replace(ou, missing=1)
except TypeError as e:
    print("replace bad field", type(e).__name__)


@dataclass
class WithInitFalse:
    a: int
    b: int = field(init=False, default=10)


try:
    replace(WithInitFalse(1), b=5)
except (TypeError, ValueError) as e:
    print("replace init=False", type(e).__name__)
import copy

print("copy.replace protocol", copy.replace(Inner(1), n=2))


# Inheritance: fields accumulate in order, overriding keeps original position
@dataclass
class Base:
    x: int
    y: int = 0

    def total(self):
        return self.x + self.y


@dataclass
class Derived(Base):
    z: int = 1
    x: int = 100  # overrides default, keeps position


dv = Derived()
print("inherited fields", [f.name for f in fields(Derived)], dv, dv.total())
print("derived positional", Derived(1, 2, 3))
print("isinstance base", isinstance(dv, Base), Derived(1, 0, 1) == Base(1, 0))


@dataclass
class P:
    a: int

    def __post_init__(self):
        self.log = ["P"]


@dataclass
class Q(P):
    b: int = 0

    def __post_init__(self):
        super().__post_init__()
        self.log.append("Q")


print("inherited post_init", Q(1, 2).log)


# Non-dataclass base in hierarchy
class Mixin:
    def hello(self):
        return f"hello {self.name}"


@dataclass
class Greeter(Mixin):
    name: str


print("mixin", Greeter("bob").hello())

# make_dataclass
Dyn = make_dataclass("Dyn", [("a", int), ("b", str, field(default="x"))], namespace={"double": lambda self: self.a * 2})
print("make_dataclass", Dyn(4), Dyn(4).double(), dataclasses.is_dataclass(Dyn), dataclasses.is_dataclass(Dyn(1)))
print("is_dataclass plain", dataclasses.is_dataclass(object()), dataclasses.is_dataclass(int))

# field metadata
@dataclass
class Meta:
    v: int = field(default=0, metadata={"unit": "cm"})


print("metadata", fields(Meta)[0].metadata["unit"])

# Default value shared (immutable) vs factory call count
calls = []


def factory():
    calls.append(1)
    return len(calls)


@dataclass
class Counted:
    n: int = field(default_factory=factory)


print("factory per instance", Counted().n, Counted().n, Counted(42).n, len(calls))

# Annotations without dataclass field (no annotation -> not a field)
@dataclass
class NotAField:
    a: int = 1
    b = 2


print("unannotated not field", [f.name for f in fields(NotAField)], NotAField())
