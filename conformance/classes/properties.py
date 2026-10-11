"""Properties: getter/setter/deleter, read-only errors, inheritance overriding, property objects."""


class Temperature:
    def __init__(self, c):
        self._c = c
        self.log = []

    @property
    def celsius(self):
        self.log.append("get")
        return self._c

    @celsius.setter
    def celsius(self, v):
        self.log.append(f"set {v}")
        if v < -273.15:
            raise ValueError("below absolute zero")
        self._c = v

    @celsius.deleter
    def celsius(self):
        self.log.append("del")
        self._c = None

    @property
    def fahrenheit(self):
        return self._c * 9 / 5 + 32


t = Temperature(100)
print("get", t.celsius, t.fahrenheit)
t.celsius = 0
print("after set", t.celsius, t.fahrenheit)
try:
    t.celsius = -500
except ValueError as e:
    print("setter raised", e)
del t.celsius
print("after del", t._c)
print("log", t.log)

try:
    t.fahrenheit = 5
except AttributeError as e:
    print("read-only set", type(e).__name__)
try:
    del t.fahrenheit
except AttributeError as e:
    print("read-only del", type(e).__name__)

# Property takes precedence over instance __dict__ (data descriptor).
t.__dict__["celsius"] = "shadow?"
t._c = 7
print("property beats dict", t.celsius)

# property() call form and introspection
class Box:
    def _get(self):
        return self._v

    def _set(self, v):
        self._v = v * 2

    v = property(_get, _set, None, "the v doc")


bx = Box()
bx.v = 5
print("property() form", bx.v, Box.v.__doc__)
print("property parts", Box.v.fget is Box._get, Box.v.fset is Box._set, Box.v.fdel)
print("class access returns property", type(Box.v).__name__)


# Inheritance: override only the getter, keep base setter.
class Base:
    def __init__(self):
        self._x = 0

    @property
    def x(self):
        return self._x

    @x.setter
    def x(self, v):
        self._x = v


class Doubler(Base):
    @Base.x.getter
    def x(self):
        return self._x * 2


d = Doubler()
d.x = 21
print("override getter keep setter", d.x, d._x)


# Override property with a plain class attribute and with a new read-only property.
class Plain(Base):
    x = "plain"


class ReadOnly(Base):
    @property
    def x(self):
        return "ro"


print("plain override", Plain.x, Plain().x)
pl = Plain()
pl.x = "instance now"
print("plain override is shadowable", pl.x, Plain.x)
ro = ReadOnly()
print("ro get", ro.x)
try:
    ro.x = 5
except AttributeError as e:
    print("ro set in subclass", type(e).__name__)


# Property that computes lazily and caches in a different attribute
class Lazy:
    calls = 0

    @property
    def value(self):
        if "_cache" not in self.__dict__:
            type(self).calls += 1
            self._cache = sum(range(10))
        return self._cache


lz = Lazy()
print("lazy", lz.value, lz.value, Lazy.calls)

# Property raising AttributeError triggers __getattr__ fallback.
class Fallback:
    @property
    def broken(self):
        raise AttributeError("inner")

    def __getattr__(self, name):
        return f"fallback:{name}"


print("property AttributeError -> __getattr__", Fallback().broken)

# deleter missing
class NoDel:
    @property
    def p(self):
        return 1

    @p.setter
    def p(self, v):
        pass


try:
    del NoDel().p
except AttributeError as e:
    print("no deleter", type(e).__name__)

# property chain identity: setter returns a new property object
p1 = property(lambda s: 1)
p2 = p1.setter(lambda s, v: None)
print("setter makes new", p1 is p2, p2.fget is p1.fget)
