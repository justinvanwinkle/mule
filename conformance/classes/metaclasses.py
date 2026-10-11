"""Metaclasses: __new__, __init__, __call__ overriding instance creation, inheritance of metaclass, methods on metaclass."""

events = []


class Meta(type):
    def __new__(mcls, name, bases, ns, **kwargs):
        events.append(f"Meta.__new__ {name} kw={sorted(kwargs.items())}")
        ns["added_by_meta"] = name.upper()
        return super().__new__(mcls, name, bases, ns)

    def __init__(cls, name, bases, ns, **kwargs):
        events.append(f"Meta.__init__ {name}")
        super().__init__(name, bases, ns)

    def __call__(cls, *args, **kwargs):
        events.append(f"Meta.__call__ {cls.__name__} {args}")
        inst = super().__call__(*args, **kwargs)
        inst.created_via_meta = True
        return inst

    def describe(cls):
        return f"class {cls.__name__} via {type(cls).__name__}"

    @property
    def meta_prop(cls):
        return f"meta_prop of {cls.__name__}"


class Thing(metaclass=Meta, flavor="x"):
    def __init__(self, v):
        events.append(f"Thing.__init__ {v}")
        self.v = v


class SubThing(Thing):
    pass


t = Thing(1)
st = SubThing(2)
for e in events:
    print("event", e)
print("added", Thing.added_by_meta, SubThing.added_by_meta)
print("meta attrs", t.created_via_meta, st.v)
print("type of class", type(Thing).__name__, type(SubThing).__name__)
print("meta method", Thing.describe(), SubThing.meta_prop)
print("meta method not on instance", hasattr(t, "describe"))


# __call__ returning something else entirely: __init__ skipped.
class Cached(type):
    def __call__(cls, key):
        cache = cls.__dict__.get("_cache")
        if cache is None:
            cache = {}
            cls._cache = cache
        if key not in cache:
            cache[key] = super().__call__(key)
        return cache[key]


class Interned(metaclass=Cached):
    inits = 0

    def __init__(self, key):
        type(self).inits += 1
        self.key = key


a1 = Interned("a")
a2 = Interned("a")
b1 = Interned("b")
print("interned", a1 is a2, a1 is b1, Interned.inits)


class Factory(type):
    def __call__(cls, *args):
        return ("not an instance", cls.__name__, args)


class Weird(metaclass=Factory):
    def __init__(self):
        raise AssertionError("never runs")


print("call override", Weird(1, 2))


# Singleton metaclass
class Singleton(type):
    _instances = {}

    def __call__(cls, *a, **k):
        if cls not in cls._instances:
            cls._instances[cls] = super().__call__(*a, **k)
        return cls._instances[cls]


class Config(metaclass=Singleton):
    def __init__(self, n=0):
        self.n = n


c1 = Config(5)
c2 = Config(99)
print("singleton", c1 is c2, c2.n)


# Metaclass __new__ that rewrites methods
class Upper(type):
    def __new__(mcls, name, bases, ns):
        new_ns = {}
        for k, v in ns.items():
            if callable(v) and not k.startswith("__"):
                new_ns[k.upper()] = v
            else:
                new_ns[k] = v
        return super().__new__(mcls, name, bases, new_ns)


class Shouty(metaclass=Upper):
    def hello(self):
        return "hi"


print("renamed", hasattr(Shouty, "hello"), Shouty().HELLO())

# Metaclass via type() call with a metaclass
Dyn = Meta("Dyn", (), {"z": 1})
print("dynamic with meta", Dyn.added_by_meta, Dyn.z, type(Dyn).__name__)

# isinstance relationships
print("isinstance class/meta", isinstance(Thing, Meta), isinstance(Thing, type), isinstance(t, Meta))
print("type(type)", type(type).__name__, type(Meta).__name__)


# Metaclass __repr__ for classes
class ReprMeta(type):
    def __repr__(cls):
        return f"<class-ish {cls.__name__}>"


class R(metaclass=ReprMeta):
    pass


print("class repr", repr(R), str(R), [R])


# Metaclass operator overloading on classes
class OpMeta(type):
    def __or__(cls, other):
        return f"{cls.__name__}|{other.__name__}"

    def __len__(cls):
        return 42

    def __iter__(cls):
        return iter(sorted(k for k in cls.__dict__ if not k.startswith("_")))


class X(metaclass=OpMeta):
    b = 1
    a = 2


print("class ops", X | X, len(X), list(X))
