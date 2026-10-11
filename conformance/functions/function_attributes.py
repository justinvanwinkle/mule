"""Function introspection: __name__, __qualname__ with <locals>, __doc__, defaults, __code__, custom attributes, __closure__."""


def documented(a, b=1, *args, c, d=4, **kw):
    """The docstring."""
    return a


def undocumented():
    pass


print("name", documented.__name__)
print("qualname", documented.__qualname__)
print("doc", documented.__doc__)
print("no doc", undocumented.__doc__)
print("defaults", documented.__defaults__)
print("kwdefaults", documented.__kwdefaults__)
print("no defaults", undocumented.__defaults__, undocumented.__kwdefaults__)
print("argcount", documented.__code__.co_argcount)
print("kwonlyargcount", documented.__code__.co_kwonlyargcount)
print("posonlyargcount", documented.__code__.co_posonlyargcount)
print("varnames prefix", documented.__code__.co_varnames[:6])
print("co_name", documented.__code__.co_name)
print("module", documented.__module__)


def outer():
    def inner():
        def innermost():
            pass

        return innermost

    return inner


print("nested qualname", outer().__qualname__)
print("deep qualname", outer()().__qualname__)
print("lambda qualname", (lambda: 0).__qualname__)


def lambda_in_func():
    return lambda: 0


print("lambda in func qualname", lambda_in_func().__qualname__)


class C:
    def method(self):
        def helper():
            pass

        return helper

    class Nested:
        def deep(self):
            pass


print("method qualname", C.method.__qualname__)
print("method local qualname", C().method().__qualname__)
print("nested class method qualname", C.Nested.deep.__qualname__)


def pos_only_example(a, b, /, c):
    pass


print("posonly argcount", pos_only_example.__code__.co_argcount, pos_only_example.__code__.co_posonlyargcount)


def custom():
    return custom.state


custom.state = "custom attr"
custom.tags = ["a", "b"]
print("custom attribute", custom(), custom.tags)
print("custom attrs in __dict__", sorted(custom.__dict__))


def make_closure():
    x = 10
    y = "cell"

    def f():
        return x, y

    return f


f = make_closure()
print("closure cells", [c.cell_contents for c in f.__closure__])
print("freevars", f.__code__.co_freevars)
print("no closure", undocumented.__closure__)


def mutable_cell():
    n = 0

    def bump():
        nonlocal n
        n += 1
        return n

    return bump


bump = mutable_cell()
bump()
bump()
print("cell after mutation", bump.__closure__[0].cell_contents)
bump.__closure__[0].cell_contents = 100
print("cell write", bump())

renamed = documented
renamed.__name__ = "renamed_fn"
print("renaming __name__", documented.__name__, renamed.__qualname__)
documented.__qualname__ = "Some.Path"
print("renaming __qualname__", documented.__qualname__)
documented.__doc__ = "new doc"
print("replace doc", documented.__doc__)


def gen():
    yield


async def coro():
    pass


print("callable", callable(documented), callable(C), callable(C()), callable(3))
print("type name", type(documented).__name__, type(lambda: 0).__name__)
print("generator function type", type(gen).__name__, type(gen()).__name__)
print("code flags differ", bool(gen.__code__.co_flags & 0x20), bool(documented.__code__.co_flags & 0x20))


def docstring_not_first():
    x = 1
    "not a docstring"
    return x


print("docstring must be first", docstring_not_first.__doc__)


class WithMethodDoc:
    def m(self):
        """Method doc."""


print("method doc", WithMethodDoc.m.__doc__, WithMethodDoc().m.__doc__)
print("bound method name", WithMethodDoc().m.__name__, WithMethodDoc().m.__func__ is WithMethodDoc.m)
