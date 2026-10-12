"""PEP 649/749 deferred annotations: forward references, __annotations__, annotationlib Format.VALUE/STRING/FORWARDREF."""

import annotationlib

evaluations = []


def note(name):
    evaluations.append(name)
    return name


def uses_later(a: LaterClass, b: int = 3) -> LaterClass:
    return a


print("defined without evaluating", evaluations)


class LaterClass:
    pass


print("forward ref resolved", uses_later.__annotations__["a"].__name__)
print("return annotation", uses_later.__annotations__["return"].__name__)
print("keys", list(uses_later.__annotations__))


def lazy(x: note("x-ann"), y: note("y-ann")) -> note("ret-ann"):
    pass


print("not yet evaluated", evaluations)
first = lazy.__annotations__
print("evaluated on access", evaluations, first)
second = lazy.__annotations__
print("cached after first access", evaluations, first is second)


def missing(a: Undefined, b: int):
    pass


try:
    missing.__annotations__
except NameError as e:
    print("VALUE format NameError", type(e).__name__)

print("STRING format", annotationlib.get_annotations(missing, format=annotationlib.Format.STRING))
fwd = annotationlib.get_annotations(missing, format=annotationlib.Format.FORWARDREF)
print("FORWARDREF types", type(fwd["a"]).__name__, fwd["b"].__name__)
print("ForwardRef arg", fwd["a"].__forward_arg__)


class Undefined:
    pass


print("resolves once defined", missing.__annotations__["a"].__name__)


def complex_string(a: list[dict[str, int | None]], *args: tuple[int, ...], **kw: quoted) -> None:
    pass


print("STRING complex", annotationlib.get_annotations(complex_string, format=annotationlib.Format.STRING))
print("VALUE quoted stays str", complex_string.__annotations__["kw"])


def no_annotations(a, b):
    pass


print("empty annotations", no_annotations.__annotations__)


class Annotated:
    x: int
    y: Annotated
    z: Later2 = 5

    def method(self, other: Annotated) -> Annotated:
        return other


class Later2:
    pass


print("class annotations keys", list(Annotated.__annotations__))
print("class annotation types", Annotated.__annotations__["x"].__name__, Annotated.__annotations__["z"].__name__)
print("class attr without value absent", hasattr(Annotated, "x"), Annotated.z)
print("method self-reference", Annotated.method.__annotations__["other"].__name__)

annotate = lazy.__annotate__
print("__annotate__ callable", callable(annotate))
print("__annotate__ VALUE", annotate(annotationlib.Format.VALUE))

lazy.__annotations__ = {"replaced": int}
print("assignable", lazy.__annotations__["replaced"].__name__)


def outer():
    local_type = int

    def inner(v: local_type) -> local_type:
        return v

    return inner


print("closure annotation", outer().__annotations__["v"].__name__)


def anno_local_scope():
    T = str

    def f(x: T):
        pass

    T = bytes
    return f.__annotations__["x"].__name__


print("annotation sees later rebinding", anno_local_scope())

x: int = 5
print("module variable annotated", x)
