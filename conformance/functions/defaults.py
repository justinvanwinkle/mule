"""Default arguments: definition-time evaluation, mutable defaults persisting, sentinels, builtin-named params."""

counter = 0


def next_value():
    global counter
    counter += 1
    return counter


def f(x=next_value()):
    return x


print("default evaluated once", f(), f(), counter)

value = "early"


def g(x=value):
    return x


value = "late"
print("default captured at def", g(), value)


def append_to(item, target=[]):
    target.append(item)
    return target


first = append_to(1)
second = append_to(2)
print("mutable default persists", first, second, first is second)
print("explicit target", append_to(9, []))
print("default still shared", append_to(3))
print("__defaults__ sees mutation", append_to.__defaults__)


def counter_dict(key, seen={}):
    seen[key] = seen.get(key, 0) + 1
    return seen[key]


print("dict default", [counter_dict(k) for k in "abacab"])

_missing = object()


def sentinel(x=_missing):
    return "missing" if x is _missing else x


print("sentinel", sentinel(), sentinel(None), sentinel(0))


def none_default(target=None):
    if target is None:
        target = []
    target.append(1)
    return target


print("none idiom", none_default(), none_default())


# Parameters named like builtins shadow them locally only.
def shadow(list=None, len=3, type="kind", **kwargs):
    return (list, len, type, sorted(kwargs))


print("builtin-named params", shadow(list=[1], len=5))
print("builtin-named defaults", shadow())
print("builtin-named kwargs", shadow(print=1, id=2, max=3))
print("builtins untouched", list((1, 2)), len("ab"), int.__name__)


def uses_param_named_print(print=None):
    return print


print("param print", uses_param_named_print(print="shadowed"))

# Default referencing an earlier default-time name
base = 10


def chain(a=base, b=base * 2, c=[base]):
    return (a, b, c)


base = 0
print("chained defaults", chain())

# Mutating __defaults__ changes later calls
def adjustable(a=1, b=2):
    return a + b


adjustable.__defaults__ = (100, 200)
print("replaced __defaults__", adjustable(), adjustable(1))


def kwdef(*, k=1):
    return k


kwdef.__kwdefaults__["k"] = 42
print("mutated __kwdefaults__", kwdef())

# Defaults in nested function are evaluated each time the outer runs
def make():
    def inner(x=[]):
        x.append(len(x))
        return x

    return inner


a = make()
b = make()
print("nested defaults distinct", a(), a(), b())

# Lambda defaults capture loop variable values
fns = [lambda i=i: i * 10 for i in range(3)]
print("lambda default capture", [fn() for fn in fns])
