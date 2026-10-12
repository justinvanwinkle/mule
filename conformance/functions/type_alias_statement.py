"""PEP 695 `type` alias statements: lazy __value__, generic aliases, recursion, and runtime attributes."""

from typing import TypeAliasType

events = []


def mark(label, value):
    events.append(label)
    return value


type Simple = int
type Lazy = mark("Lazy evaluated", list[int])

print("alias type", type(Simple).__name__, isinstance(Simple, TypeAliasType))
print("alias name", Simple.__name__, Lazy.__name__)
print("alias module", Simple.__module__)
print("not evaluated yet", events)
print("value", Lazy.__value__)
print("evaluated once", events)
Lazy.__value__
print("cached", events)
print("simple value", Simple.__value__.__name__)

type Forward = list[NotYetDefined]


class NotYetDefined:
    def __repr__(self):
        return "NotYetDefined"


print("forward reference value", Forward.__value__.__args__[0].__name__)

type Pair[T] = tuple[T, T]
print("generic alias params", [p.__name__ for p in Pair.__type_params__])
specialized = Pair[int]
print("specialized origin", specialized.__origin__ is Pair, specialized.__args__)

type Bounded[N: (int, float)] = list[N]
(bn,) = Bounded.__type_params__
print("constrained alias", [c.__name__ for c in bn.__constraints__])

type Variadic[*Ts] = tuple[int, *Ts]
type Callback[**P] = "callable with P"
print("variadic alias param", type(Variadic.__type_params__[0]).__name__)
print("paramspec alias param", type(Callback.__type_params__[0]).__name__, Callback.__value__)

type Json = dict[str, Json] | list[Json] | str | int | None
print("recursive alias name", Json.__name__)
print("recursive alias args count", len(Json.__value__.__args__))

type Never_Evaluated = 1 / 0
try:
    Never_Evaluated.__value__
except ZeroDivisionError as e:
    print("error deferred to access", type(e).__name__)
print("defining it did not fail", Never_Evaluated.__name__)


def local_alias():
    base = str

    type Local = list[base]
    base = bytes
    return Local.__value__


print("local alias sees latest binding", local_alias())


class Holder:
    type Member = int

    def get(self):
        return self.Member.__value__


print("class-scoped alias", Holder.Member.__name__, Holder().get().__name__)

try:
    Simple.__value__ = str
except AttributeError as e:
    print("value read-only", type(e).__name__)

try:
    Simple()
except TypeError as e:
    print("alias not callable", type(e).__name__)

try:
    Simple[int]
except TypeError as e:
    print("non-generic alias subscript", type(e).__name__)

print("alias repr", repr(Simple), repr(Pair[str]))
print("alias in union", (Simple | None).__args__[0] is Simple)

manual = TypeAliasType("Manual", list[str])
print("manual alias", manual.__name__, manual.__value__)
