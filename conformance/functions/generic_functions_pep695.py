"""PEP 695 generic functions: __type_params__, bounds, constraints, TypeVarTuple, ParamSpec, and runtime behavior."""

from typing import TypeVar, TypeVarTuple, ParamSpec, Callable


def first[T](xs: list[T]) -> T:
    return xs[0]


print("generic call", first([3, 4]), first("ab"))
params = first.__type_params__
print("type params", [p.__name__ for p in params], type(params[0]).__name__)
print("annotation uses T", first.__annotations__["return"] is params[0])


def bounded[N: int](n: N) -> N:
    return n


(nb,) = bounded.__type_params__
print("bound", nb.__name__, nb.__bound__.__name__, nb.__constraints__)


def constrained[S: (str, bytes)](s: S) -> S:
    return s + s


(sc,) = constrained.__type_params__
print("constraints", sc.__name__, sc.__bound__, [c.__name__ for c in sc.__constraints__])
print("constrained call", constrained("ab"), constrained(b"cd"))


def variadic[*Ts](*args: *Ts) -> tuple[*Ts]:
    return args


(ts,) = variadic.__type_params__
print("TypeVarTuple", ts.__name__, type(ts).__name__, isinstance(ts, TypeVarTuple))
print("variadic call", variadic(1, "a", None))


def call_with[**P, R](fn: Callable[P, R], *args: P.args, **kwargs: P.kwargs) -> R:
    return fn(*args, **kwargs)


p, r = call_with.__type_params__
print("ParamSpec", p.__name__, type(p).__name__, isinstance(p, ParamSpec), r.__name__)
print("ParamSpec call", call_with(lambda a, b=0: a - b, 10, b=3))


def several[A, B: str, *C, **D]() -> None:
    pass


print("mixed params", [(t.__name__, type(t).__name__) for t in several.__type_params__])


def default_params[T = int, *Ts = *tuple[str, ...], **P = [int]]():
    pass


dt, dts, dp = default_params.__type_params__
print("defaults", dt.__default__.__name__, dt.has_default(), dp.__default__)

# Bounds are lazily evaluated
order = []


def make_bound():
    order.append("bound evaluated")
    return float


def lazy_bound[X: make_bound()](x: X) -> X:
    return x


print("bound not yet evaluated", order)
(lx,) = lazy_bound.__type_params__
print("bound on access", lx.__bound__.__name__, order)


def uses_forward[T: LaterType](x: T) -> T:  # noqa: F821
    return x


class LaterType:
    pass


print("forward bound", uses_forward.__type_params__[0].__bound__.__name__)


class Box[T]:
    def __init__(self, item: T):
        self.item = item

    def map[U](self, fn: Callable[[T], U]) -> "Box[U]":
        return Box(fn(self.item))

    def __repr__(self):
        return "Box(" + repr(self.item) + ")"


print("generic class", Box(3).map(lambda v: v * 1.5))
print("class type params", [t.__name__ for t in Box.__type_params__])
print("method type params", [t.__name__ for t in Box.map.__type_params__])
print("subscript generic class", Box[int].__origin__ is Box, Box[int](7))

# Type params are distinct objects per function
def f1[T](): pass
def f2[T](): pass


print("distinct per function", f1.__type_params__[0] is f2.__type_params__[0], f1.__type_params__[0].__name__)
print("not a plain TypeVar() call", isinstance(f1.__type_params__[0], TypeVar))
print("no params", (lambda: 0).__type_params__)


def scope_check[T]():
    return T


print("T visible in body", scope_check().__name__)
try:
    T  # noqa: F821
except NameError as e:
    print("T not visible at module", type(e).__name__)
