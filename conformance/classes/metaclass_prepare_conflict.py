"""Metaclass __prepare__ returning a custom logging mapping, and metaclass conflict resolution/TypeError."""


class LoggingDict(dict):
    def __init__(self):
        super().__init__()
        self.order = []

    def __setitem__(self, key, value):
        if not key.startswith("__"):
            self.order.append(key)
        super().__setitem__(key, value)


class OrderMeta(type):
    @classmethod
    def __prepare__(mcls, name, bases, **kwargs):
        ns = LoggingDict()
        ns["injected"] = f"prepared for {name}"
        return ns

    def __new__(mcls, name, bases, ns, **kwargs):
        cls = super().__new__(mcls, name, bases, dict(ns))
        cls.definition_order = tuple(ns.order)
        cls.ns_type = type(ns).__name__
        return cls


class Form(metaclass=OrderMeta):
    zeta = 1
    alpha = 2

    def middle(self):
        pass

    zeta = 3  # reassignment logged again
    beta: int = 4
    print("inside body sees injected:", injected)


print("order", Form.definition_order)
print("ns type", Form.ns_type, "injected", Form.injected)
print("final zeta", Form.zeta)


# __prepare__ returning a mapping that auto-creates entries (dangerous but legal)
class AutoNS(dict):
    def __missing__(self, key):
        if key.startswith("auto_"):
            return key[5:].upper()
        raise KeyError(key)


class AutoMeta(type):
    @classmethod
    def __prepare__(mcls, name, bases):
        return AutoNS()

    def __new__(mcls, name, bases, ns):
        return super().__new__(mcls, name, bases, dict(ns))


class UsesAuto(metaclass=AutoMeta):
    value = auto_hello
    other = auto_x + "!"


print("auto namespace", UsesAuto.value, UsesAuto.other)


# __prepare__ receives class keyword arguments
class KwMeta(type):
    @classmethod
    def __prepare__(mcls, name, bases, **kw):
        return {"seen_kw": sorted(kw.items())}

    def __new__(mcls, name, bases, ns, **kw):
        return super().__new__(mcls, name, bases, ns)

    def __init__(cls, name, bases, ns, **kw):
        super().__init__(name, bases, ns)


class WithKw(metaclass=KwMeta, a=1, b="two"):
    pass


print("prepare kwargs", WithKw.seen_kw)


# Metaclass conflict
class MetaA(type):
    pass


class MetaB(type):
    pass


class UsesA(metaclass=MetaA):
    pass


class UsesB(metaclass=MetaB):
    pass


try:
    class Both(UsesA, UsesB):
        pass
except TypeError as e:
    print("metaclass conflict", type(e).__name__)


# Resolved by a derived metaclass
class MetaAB(MetaA, MetaB):
    pass


class Both(UsesA, UsesB, metaclass=MetaAB):
    pass


print("resolved", type(Both).__name__)


# Most-derived metaclass is chosen automatically
class SubMetaA(MetaA):
    pass


class UsesSub(metaclass=SubMetaA):
    pass


class Mixed(UsesA, UsesSub):
    pass


print("auto most-derived", type(Mixed).__name__)


# Explicit metaclass less derived than a base's is upgraded
class Upgraded(UsesSub, metaclass=MetaA):
    pass


print("upgraded", type(Upgraded).__name__)


# Non-type callable as metaclass
def func_meta(name, bases, ns):
    ns = dict(ns)
    ns["made_by"] = "function"
    return type(name, bases, ns)


class ViaFunc(metaclass=func_meta):
    x = 1


print("function metaclass", ViaFunc.made_by, type(ViaFunc).__name__)


def returns_other(name, bases, ns):
    return f"not a class: {name} {sorted(k for k in ns if not k.startswith('__'))}"


class NotAClass(metaclass=returns_other):
    a = 1
    b = 2


print("metaclass returning str", NotAClass)
