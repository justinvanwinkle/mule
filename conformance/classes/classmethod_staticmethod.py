"""classmethod alternative constructors returning subclasses, staticmethods, and calling forms."""


class Point:
    dims = 2

    def __init__(self, x, y):
        self.x = x
        self.y = y

    @classmethod
    def origin(cls):
        return cls(0, 0)

    @classmethod
    def from_string(cls, s):
        x, y = (int(p) for p in s.split(","))
        return cls(x, y)

    @staticmethod
    def parse_pair(s):
        return tuple(int(p) for p in s.split(","))

    @classmethod
    def from_pair(cls, s):
        return cls(*cls.parse_pair(s))

    def __repr__(self):
        return f"{type(self).__name__}({self.x}, {self.y})"


class Point3(Point):
    dims = 3

    def __init__(self, x, y, z=0):
        super().__init__(x, y)
        self.z = z

    def __repr__(self):
        return f"Point3({self.x}, {self.y}, {self.z})"


print("origin", Point.origin(), Point3.origin())
print("from_string", Point.from_string("3,4"), Point3.from_string("5,6"))
print("from_pair", Point3.from_pair("7,8"))
p = Point3(1, 2, 3)
print("classmethod via instance", p.origin(), type(p.origin()).__name__)
print("staticmethod via class/instance", Point.parse_pair("9,10"), p.parse_pair("1,1"))


# Staticmethod calling other static/class methods by name
class MathUtil:
    @staticmethod
    def square(x):
        return x * x

    @staticmethod
    def sum_squares(xs):
        return sum(MathUtil.square(x) for x in xs)

    @classmethod
    def describe(cls):
        return f"{cls.__name__}: {cls.sum_squares([1, 2, 3])}"


class SubUtil(MathUtil):
    @staticmethod
    def square(x):
        return x * x * x


print("static calls", MathUtil.sum_squares([1, 2, 3]), SubUtil.sum_squares([1, 2, 3]))
print("classmethod->static", MathUtil.describe(), SubUtil.describe())

# staticmethod objects are callable directly (3.10+) and wrap the function
sm = MathUtil.__dict__["square"]
print("raw staticmethod callable", type(sm).__name__, sm(4), sm.__func__(5))
cm = Point.__dict__["origin"]
print("raw classmethod", type(cm).__name__, cm.__func__(Point))

# bound classmethod identity of __self__
bound = Point3.origin
print("bound cls", bound.__self__.__name__, bound.__func__ is Point.__dict__["origin"].__func__)


# Classmethod counting instances per subclass
class Tracked:
    _counts = {}

    def __init__(self):
        type(self)._register()

    @classmethod
    def _register(cls):
        cls._counts[cls.__name__] = cls._counts.get(cls.__name__, 0) + 1

    @classmethod
    def count(cls):
        return cls._counts.get(cls.__name__, 0)


class T1(Tracked):
    pass


class T2(Tracked):
    pass


T1(); T1(); T2(); Tracked()
print("counts", Tracked.count(), T1.count(), T2.count())

# Classmethod used as factory registry via __init_subclass__-free decorator
class Shape:
    registry = {}

    @classmethod
    def register(cls, name):
        def deco(sub):
            cls.registry[name] = sub
            return sub

        return deco

    @classmethod
    def create(cls, name, *args):
        return cls.registry[name](*args)


@Shape.register("sq")
class Square(Shape):
    def __init__(self, s):
        self.s = s

    def area(self):
        return self.s ** 2


@Shape.register("rect")
class Rect(Shape):
    def __init__(self, w, h):
        self.w, self.h = w, h

    def area(self):
        return self.w * self.h


print("factory", Shape.create("sq", 3).area(), Shape.create("rect", 2, 5).area())
print("registry keys", sorted(Shape.registry))

# Stacking classmethod over property is no longer supported in 3.13+: check plain call.
class Weird:
    @staticmethod
    def no_args():
        return "no args"


print("static no args", Weird.no_args(), Weird().no_args())
try:
    Point.origin(1)
except TypeError as e:
    print("classmethod extra arg", type(e).__name__)
