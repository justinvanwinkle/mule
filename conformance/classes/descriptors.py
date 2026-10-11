"""Descriptors: __get__/__set__/__delete__, data vs non-data precedence, __set_name__, functions as descriptors."""

log = []


class Typed:
    def __set_name__(self, owner, name):
        self.owner_name = owner.__name__
        self.name = name
        self.private = "_" + name
        log.append(f"set_name {owner.__name__}.{name}")

    def __init__(self, kind):
        self.kind = kind

    def __get__(self, obj, objtype=None):
        if obj is None:
            return f"<Typed {self.name} on {objtype.__name__}>"
        return getattr(obj, self.private)

    def __set__(self, obj, value):
        if not isinstance(value, self.kind):
            raise TypeError(f"{self.name} must be {self.kind.__name__}")
        setattr(obj, self.private, value)

    def __delete__(self, obj):
        log.append(f"delete {self.name}")
        delattr(obj, self.private)


class Person:
    name = Typed(str)
    age = Typed(int)

    def __init__(self, name, age):
        self.name = name
        self.age = age


print("set_name log", log)
pp = Person("Ann", 30)
print("get", pp.name, pp.age)
print("class access", Person.name)
try:
    pp.age = "old"
except TypeError as e:
    print("type check", e)
del pp.age
print("after delete", hasattr(pp, "_age"), log[-1])


# Data descriptor beats instance dict; non-data loses to instance dict.
class NonData:
    def __get__(self, obj, objtype=None):
        return "non-data value"


class Data:
    def __get__(self, obj, objtype=None):
        return "data value"

    def __set__(self, obj, value):
        obj.__dict__["d_store"] = value


class Holder:
    nd = NonData()
    d = Data()


h = Holder()
print("before shadow", h.nd, h.d)
h.__dict__["nd"] = "instance nd"
h.__dict__["d"] = "instance d"
print("after shadow", h.nd, h.d)
h.nd = "assigned nd"
h.d = "assigned d"
print("after assign", h.nd, h.d, h.__dict__["d_store"])


# Descriptor with only __set__ (no __get__) is still a data descriptor; read returns descriptor itself
class SetOnly:
    def __set__(self, obj, value):
        obj.__dict__["so"] = value * 2

    def __repr__(self):
        return "<SetOnly>"


class S:
    so = SetOnly()


s = S()
s.so = 4
print("set-only get", s.so, s.__dict__)


# Descriptor with only __delete__ is also a data descriptor.
class DelOnly:
    def __delete__(self, obj):
        log.append("delonly")


class DD:
    x = DelOnly()


dd = DD()
del dd.x
print("delete-only", log[-1])


# Functions are non-data descriptors.
def method(self):
    return f"bound to {type(self).__name__}"


class F:
    pass


F.m = method
fobj = F()
bm = method.__get__(fobj, F)
print("function __get__", bm(), F.m(fobj), fobj.m())
print("function get None", method.__get__(None, F) is method)


# Descriptor counting accesses with per-instance storage via __set_name__
class Counted:
    def __set_name__(self, owner, name):
        self.name = name

    def __get__(self, obj, objtype=None):
        if obj is None:
            return self
        obj.__dict__.setdefault("_counts", {}).setdefault(self.name, 0)
        obj._counts[self.name] += 1
        return obj._counts[self.name]


class Widget:
    a = Counted()
    b = Counted()


w = Widget()
print("counted", w.a, w.a, w.b, w.a)
print("class returns descriptor", type(Widget.a).__name__)

# __set_name__ is not called for descriptors attached after class creation
late = Typed(int)
Widget.late = late
print("late set_name", hasattr(late, "name"))
late.__set_name__(Widget, "late")
print("manual set_name", late.name)


# Descriptor on metaclass-free class used via type: __get__ receives obj=None
class ClassLevel:
    def __get__(self, obj, objtype=None):
        return ("obj" if obj is not None else "none", objtype.__name__)


class K:
    c = ClassLevel()


class K2(K):
    pass


print("objtype", K.c, K2.c, K2().c)

# Error inside __set_name__ propagates (3.12+: no RuntimeError wrapper)
class BadName:
    def __set_name__(self, owner, name):
        raise ValueError("bad set_name")


try:
    class UsesBad:
        attr = BadName()
except Exception as e:
    print("set_name error", type(e).__name__)
