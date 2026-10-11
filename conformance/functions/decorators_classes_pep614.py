"""Decorators implemented as classes with __call__, and PEP 614 arbitrary decorator expressions."""


class CountCalls:
    def __init__(self, fn):
        self.fn = fn
        self.count = 0

    def __call__(self, *args, **kwargs):
        self.count += 1
        return self.fn(*args, **kwargs)


@CountCalls
def square(x):
    return x * x


print("class decorator calls", square(3), square(4), square.count)
print("is instance", type(square).__name__)


class Prefix:
    """Decorator with arguments implemented as a class."""

    def __init__(self, prefix):
        self.prefix = prefix

    def __call__(self, fn):
        def wrapper(*a):
            return self.prefix + fn(*a)

        return wrapper


@Prefix(">> ")
def shout(s):
    return s.upper()


print("class decorator with args", shout("hey"))


class Button:
    def __init__(self, name):
        self.name = name
        self.handlers = []

    def clicked(self, fn):
        self.handlers.append(fn)
        return fn

    def click(self):
        return [h(self.name) for h in self.handlers]


buttons = [Button("ok"), Button("cancel")]


@buttons[0].clicked
def on_ok(name):
    return "clicked " + name


@buttons[1].clicked
def on_cancel(name):
    return "cancelled via " + name


print("subscript attribute decorator", buttons[0].click(), buttons[1].click())


@(lambda f: f)
def identity_decorated():
    return "identity"


print("lambda decorator", identity_decorated())

decorators = {"double": lambda f: lambda *a: f(*a) * 2}


@decorators["double"]
def five():
    return 5


print("dict lookup decorator", five())


def maybe(flag):
    return flag


@(lambda f: lambda: "A" + f()) if maybe(True) else (lambda f: lambda: "B" + f())
def conditional():
    return "!"


print("conditional expression decorator", conditional())


@(deco := (lambda f: (lambda: f().title())))
def walrus_decorated():
    return "walrus decorated"


print("walrus decorator", walrus_decorated(), deco.__name__)


class Ns:
    class Inner:
        @staticmethod
        def wrap(f):
            return lambda: "[" + f() + "]"


@Ns.Inner.wrap
def nested_attr():
    return "deep"


print("nested attribute decorator", nested_attr())


@[lambda f: lambda: f() + 1, None][0]
def list_index_decorator():
    return 41


print("list literal decorator", list_index_decorator())


def compose(*decos):
    def apply(f):
        for d in reversed(decos):
            f = d(f)
        return f

    return apply


@compose(lambda f: lambda: f() + "1", lambda f: lambda: f() + "2")
def composed():
    return "0"


print("composed decorators", composed())


class Descriptorish:
    """A class-based decorator used on a method does not bind self by default."""

    def __init__(self, fn):
        self.fn = fn

    def __call__(self, *a):
        return ("called with", len(a))


class UsesIt:
    @Descriptorish
    def method(self):
        return "unreachable"


print("class decorator on method no binding", UsesIt().method())


class BindingDeco:
    def __init__(self, fn):
        self.fn = fn

    def __get__(self, obj, objtype=None):
        if obj is None:
            return self
        return lambda *a: self.fn(obj, *a)


class UsesBinding:
    def __init__(self, v):
        self.v = v

    @BindingDeco
    def get(self, extra):
        return self.v + extra


print("class decorator with __get__", UsesBinding(10).get(5))
