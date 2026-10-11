"""typing: NamedTuple class syntax, TypedDict, runtime_checkable Protocol, cast, Generic classes and PEP 695 generics, Literal/Final/Annotated/Union introspection via get_args/get_origin."""
from typing import (NamedTuple, TypedDict, Protocol, runtime_checkable, cast, Generic, TypeVar,
                    Literal, Final, Annotated, Optional, Union, get_args, get_origin, get_type_hints,
                    NotRequired, Required, Callable, Any, ClassVar)
import collections.abc


class Employee(NamedTuple):
    name: str
    dept: str = "eng"
    level: int = 1

    def promote(self):
        return self._replace(level=self.level + 1)

    def __str__(self):
        return f"{self.name}@{self.dept}:L{self.level}"


e = Employee("ann")
print("NamedTuple", e, repr(e))
print("NamedTuple promote", str(e.promote()), e.level)
print("NamedTuple fields", Employee._fields, Employee._field_defaults)
print("NamedTuple annotations", [(k, v.__name__) for k, v in get_type_hints(Employee).items()])
print("NamedTuple is tuple", isinstance(e, tuple), e == ("ann", "eng", 1), len(e))
try:
    Employee()
except TypeError as err:
    print("NamedTuple missing arg", type(err).__name__)


class Movie(TypedDict):
    title: str
    year: int


class MovieExtra(Movie, total=False):
    rating: float


class Show(TypedDict):
    name: str
    seasons: NotRequired[int]


m: Movie = {"title": "Up", "year": 2009}
print("TypedDict is dict", type(m).__name__, m)
mv = Movie(title="Heat", year=1995)
print("TypedDict call", mv, type(mv) is dict)
print("TypedDict keys", sorted(Movie.__required_keys__), sorted(MovieExtra.__optional_keys__), sorted(MovieExtra.__required_keys__))
print("NotRequired", sorted(Show.__required_keys__), sorted(Show.__optional_keys__))
print("TypedDict total", Movie.__total__, MovieExtra.__total__)
try:
    isinstance(m, Movie)
except TypeError as err:
    print("TypedDict isinstance", type(err).__name__)


@runtime_checkable
class Closeable(Protocol):
    def close(self) -> None: ...


@runtime_checkable
class Sized2(Protocol):
    size: int


class File:
    def close(self):
        return None


class Box:
    size = 3


class Plain:
    pass


print("Protocol structural", isinstance(File(), Closeable), isinstance(Plain(), Closeable))
print("Protocol attribute", isinstance(Box(), Sized2), isinstance(File(), Sized2))
print("Protocol issubclass", issubclass(File, Closeable))


class NotChecked(Protocol):
    def run(self): ...


try:
    isinstance(File(), NotChecked)
except TypeError as err:
    print("non-runtime protocol", type(err).__name__)


class Explicit(Closeable):
    pass


print("explicit subclass default", isinstance(Explicit(), Closeable))

x = cast(int, "not really an int")
print("cast no-op", x, type(x).__name__)
print("cast string type", cast("list[int]", [1]))

T = TypeVar("T")


class Stack(Generic[T]):
    def __init__(self) -> None:
        self.items: list[T] = []

    def push(self, item: T) -> None:
        self.items.append(item)

    def pop(self) -> T:
        return self.items.pop()


s = Stack[int]()
s.push(1)
s.push(2)
print("Generic", s.pop(), s.items, type(s).__name__)
print("Generic alias", get_origin(Stack[int]) is Stack, get_args(Stack[int]))
print("Generic params", Stack.__parameters__ == (T,))


class Pair[K, V]:
    def __init__(self, k: K, v: V):
        self.k, self.v = k, v

    def swap(self) -> "Pair[V, K]":
        return Pair(self.v, self.k)


p = Pair("a", 1).swap()
print("PEP 695 class", p.k, p.v, [tp.__name__ for tp in Pair.__type_params__])


def first[U](xs: list[U]) -> U:
    return xs[0]


print("PEP 695 func", first([9, 8]), first.__type_params__[0].__name__)
type IntList = list[int]
print("type alias", IntList.__name__, IntList.__value__, get_origin(IntList.__value__) is list)

Mode = Literal["r", "w", "a"]
print("Literal args", get_args(Mode), get_origin(Mode) is Literal)
print("Literal nested flatten", get_args(Literal[Literal[1, 2], 3]))
MAX: Final = 10
print("Final", get_origin(Final[int]) is Final, get_args(Final[int]))
Dist = Annotated[float, "meters", {"min": 0}]
print("Annotated", get_origin(Dist) is Annotated, get_args(Dist)[0] is float, get_args(Dist)[1:], Dist.__metadata__)
print("Optional", get_args(Optional[int]), get_origin(Optional[int]) is Union)
print("Union flatten dedupe", get_args(Union[int, Union[str, int]]))
print("pipe union", get_args(int | None), get_origin(int | str) is Union)
print("Union eq", (int | str) == Union[int, str], Union[int, str] == Union[str, int])
print("builtin generics", get_origin(list[int]) is list, get_args(dict[str, list[int]]))
print("Callable", get_args(Callable[[int, str], bool]), get_origin(Callable[..., int]) is collections.abc.Callable)
print("get_args plain", get_args(int), get_origin(int))
print("ClassVar", get_origin(ClassVar[int]) is ClassVar)


class Hinted:
    a: int
    b: "list[str]"
    c: Annotated[int, "tag"]


hints = get_type_hints(Hinted)
print("get_type_hints", hints["a"] is int, hints["b"] == list[str], hints["c"] is int)
print("include_extras", get_type_hints(Hinted, include_extras=True)["c"] == Annotated[int, "tag"])
print("Required in TypedDict", get_args(Required[int]))
print("Any subclassable", type("FromAny", (Any,), {})().__class__.__name__)
try:
    isinstance(3, Any)
except TypeError as err:
    print("isinstance Any", type(err).__name__)
