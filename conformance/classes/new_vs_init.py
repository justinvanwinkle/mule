"""__new__ vs __init__: call order, singletons, skipped __init__, immutable subclasses of int/str/tuple/float."""

calls = []


class Traced:
    def __new__(cls, *args, **kwargs):
        calls.append(f"__new__ {cls.__name__} {args}")
        return super().__new__(cls)

    def __init__(self, x):
        calls.append(f"__init__ {x}")
        self.x = x


t = Traced(5)
print("order", calls, t.x)


# __new__ returning an instance of another class: __init__ not called
class Other:
    def __init__(self):
        calls.append("Other.__init__")


class Redirect:
    def __new__(cls):
        return Other.__new__(Other)

    def __init__(self):
        calls.append("Redirect.__init__ should not run")


calls.clear()
r = Redirect()
print("redirect", type(r).__name__, calls)


# __new__ returning a subclass instance: __init__ IS called
class Parent:
    def __new__(cls, kind):
        if kind == "child" and cls is Parent:
            return super().__new__(Child)
        return super().__new__(cls)

    def __init__(self, kind):
        self.kind = kind
        self.init_by = type(self).__name__


class Child(Parent):
    pass


c = Parent("child")
print("subclass from __new__", type(c).__name__, c.kind, c.init_by)


# Singleton via __new__; note __init__ re-runs each call
class Single:
    _inst = None

    def __new__(cls, value):
        if cls._inst is None:
            cls._inst = super().__new__(cls)
        return cls._inst

    def __init__(self, value):
        self.value = value


s1 = Single(1)
s2 = Single(2)
print("singleton", s1 is s2, s1.value)


# Immutable subclasses: values must be set in __new__
class Celsius(float):
    def __new__(cls, value):
        return super().__new__(cls, round(value, 1))

    def __repr__(self):
        return f"{float(self)}C"


print("float subclass", Celsius(21.456), Celsius(1) + 1, type(Celsius(1) + 1).__name__)


class Even(int):
    def __new__(cls, value):
        if value % 2:
            raise ValueError(f"{value} is odd")
        return super().__new__(cls, value)

    def half(self):
        return Even(self // 2 * 2) if self // 2 % 2 == 0 else self // 2


e = Even(8)
print("int subclass", e, e + 1, type(e + 1).__name__, e.half(), type(e).__name__)
try:
    Even(3)
except ValueError as err:
    print("int subclass validation", err)


class Upper(str):
    def __new__(cls, s):
        return super().__new__(cls, s.upper())

    def __init__(self, s):
        # __init__ cannot change the str value
        self.original = s


u = Upper("hello")
print("str subclass", u, u.original, u.lower(), type(u.lower()).__name__, isinstance(u, str))


class Point(tuple):
    __slots__ = ()

    def __new__(cls, x, y):
        return super().__new__(cls, (x, y))

    x = property(lambda self: self[0])
    y = property(lambda self: self[1])

    def __repr__(self):
        return f"Point({self.x}, {self.y})"


pt = Point(3, 4)
print("tuple subclass", pt, pt.x, pt.y, len(pt), pt + (5,), type(pt + (5,)).__name__)
print("tuple unpack", *pt)
print("tuple hash equal", hash(pt) == hash((3, 4)), pt == (3, 4))

# Changing int value in __init__ has no effect
class Stubborn(int):
    def __init__(self, v):
        super().__init__()


print("stubborn", Stubborn(7))

# __new__ with extra args and object.__new__ argument checks
class NoInit:
    pass


try:
    NoInit(1)
except TypeError as err:
    print("object rejects extra args", type(err).__name__)


class OnlyNew:
    def __new__(cls, a, b):
        inst = super().__new__(cls)
        inst.total = a + b
        return inst


print("only __new__", OnlyNew(2, 3).total)

# __new__ is a static method implicitly
print("__new__ is staticmethod", type(Traced.__dict__["__new__"]).__name__)

# Calling __init__ again manually
t.__init__(99)
print("manual reinit", t.x)

# __new__ returning None
class ReturnsNone:
    def __new__(cls):
        return None

    def __init__(self):
        raise AssertionError("not called")


print("new returns None", ReturnsNone())
