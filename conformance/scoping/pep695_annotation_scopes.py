"""PEP 695 type parameter and annotation scopes interacting with outer, global, and nonlocal names."""

T = "global T"
Outer = int


def generic[T](x: T) -> T:
    return x


print("generic function call", generic(5))
print("type params", [p.__name__ for p in generic.__type_params__])
print("global T untouched", T)


class Box[T]:
    def __init__(self, item: T):
        self.item = item

    def get(self) -> T:
        return self.item

    def type_param(self):
        return T


print("generic class", Box(3).get(), Box.__type_params__[0].__name__)
print("method sees type param, not global", Box(1).type_param().__name__)

# type statement creates a lazily evaluated alias
type Pair = tuple[Later, Later]


class Later:
    pass


print("lazy alias", Pair.__name__, Pair.__value__ == tuple[Later, Later])

type ListOf[X] = list[X]
print("generic alias params", [p.__name__ for p in ListOf.__type_params__], ListOf[int].__args__[0].__name__ if hasattr(ListOf[int], "__args__") else ListOf[int])


# alias referring to outer function names resolves via closure at evaluation time
def make_alias():
    element = int
    type Local = list[element]
    element = str
    return Local


print("alias closure late binding", make_alias().__value__)


# bound and constraints evaluated lazily, may reference later names
class Constrained[S: Base, U: (int, Base)]:
    pass


class Base:
    pass


s, u = Constrained.__type_params__
print("lazy bound", s.__bound__.__name__, [c.__name__ for c in u.__constraints__])


# generic class body: type params visible in class body and methods, class names not visible in method
class Container[V]:
    kind = "container"
    alias = V

    def describe(self):
        try:
            return kind
        except NameError as e:
            return type(e).__name__


print("class body sees type param", Container.alias.__name__, Container().describe())


# nonlocal and global inside generic functions
counter = 0


def generic_global[K]():
    global counter
    counter += 1
    return counter


print("global in generic function", generic_global(), generic_global())


def outer_function():
    total = 0

    def add[N](n: N) -> None:
        nonlocal total
        total += n

    add(2)
    add(3)
    return total


print("nonlocal in generic function", outer_function())


# generic function nested in generic class referencing both scopes' params
class Mapper[A]:
    def map[B](self, value: A, fn) -> B:
        return fn(value), (A.__name__, B.__name__)


print("nested type params", Mapper().map(4, lambda v: v * 2))

# type param names do not leak into the enclosing scope
try:
    A
except NameError as e:
    print("type param does not leak", type(e).__name__)


# PEP 649/749 deferred annotations reference names defined later
def annotated(x: DefinedLater) -> DefinedLater:
    return x


class DefinedLater:
    pass


print("deferred annotations", annotated.__annotations__["x"].__name__, annotated.__annotations__["return"].__name__)


# annotation scope in class: refers to class-level names
class Annotated:
    Alias = int
    field: Alias


print("class annotation sees class name", Annotated.__annotations__["field"].__name__)


# outer-function name used as a default for a type parameter (PEP 696)
def with_default():
    fallback = float

    class Defaulted[D = fallback]:
        pass

    return Defaulted.__type_params__[0].__default__


print("type param default from closure", with_default().__name__)

# type alias can be recursive
type Tree = list[Tree] | int
print("recursive alias", Tree.__value__)
