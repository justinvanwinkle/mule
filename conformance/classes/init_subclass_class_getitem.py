"""__init_subclass__ with keyword arguments and __class_getitem__ on user classes."""

registry = {}


class Plugin:
    def __init_subclass__(cls, /, key=None, priority=0, **kwargs):
        super().__init_subclass__(**kwargs)
        cls.key = key or cls.__name__.lower()
        cls.priority = priority
        registry[cls.key] = cls


class Alpha(Plugin):
    pass


class Beta(Plugin, key="b", priority=5):
    pass


class Gamma(Beta, priority=9):
    pass


print("registry", sorted((k, v.__name__, v.priority) for k, v in registry.items()))
print("inherited key not reused", Gamma.key)
print("plugin base in registry", "plugin" in registry)

try:
    class Bad(Plugin, unknown=1):
        pass
except TypeError as e:
    print("unknown kwarg", type(e).__name__)


# Chained __init_subclass__ across mixins with cooperative super
order = []


class MixA:
    def __init_subclass__(cls, a=None, **kw):
        order.append(("MixA", cls.__name__, a))
        super().__init_subclass__(**kw)


class MixB:
    def __init_subclass__(cls, b=None, **kw):
        order.append(("MixB", cls.__name__, b))
        super().__init_subclass__(**kw)


class Combined(MixA, MixB, a=1, b=2):
    pass


print("chained", order)


# __init_subclass__ is an implicit classmethod
class Validator:
    required = ()

    def __init_subclass__(cls):
        missing = [r for r in cls.required if not hasattr(cls, r)]
        if missing:
            raise TypeError(f"{cls.__name__} missing {missing}")


class GoodV(Validator):
    required = ("run",)

    def run(self):
        pass


try:
    class BadV(Validator):
        required = ("run", "stop")

        def run(self):
            pass
except TypeError as e:
    print("validator", e)
print("good validator ok", GoodV.__name__)


# __class_getitem__
class Box:
    def __class_getitem__(cls, item):
        return f"{cls.__name__}[{item!r}]"


class SubBox(Box):
    pass


print("class_getitem", Box[int], Box["x"], Box[1, 2], SubBox[None])
print("instance not subscriptable", end=" ")
try:
    Box()[0]
except TypeError as e:
    print(type(e).__name__)


# __class_getitem__ returning a GenericAlias-like object
import types


class Seq:
    def __class_getitem__(cls, params):
        return types.GenericAlias(cls, params)


alias = Seq[int]
print("generic alias", alias.__origin__ is Seq, alias.__args__ == (int,))
print("alias call creates instance", type(alias()).__name__)


# __getitem__ on instance and __class_getitem__ on class coexist
class Both:
    def __class_getitem__(cls, item):
        return ("class", item)

    def __getitem__(self, item):
        return ("instance", item)


print("both", Both[1], Both()[1])


# PEP 695 generic class provides __class_getitem__ automatically
class Pair[T, U]:
    def __init__(self, a: T, b: U):
        self.a, self.b = a, b


alias2 = Pair[int, str]
print("pep695 params", [p.__name__ for p in Pair.__type_params__])
print("pep695 alias", alias2.__origin__ is Pair, [a.__name__ for a in alias2.__args__])
pr = alias2(1, "x")
print("pep695 instance", type(pr).__name__, pr.a, pr.b)
