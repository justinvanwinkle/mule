"""super() zero-arg and two-arg forms, in classmethods, nested functions, and the __class__ cell."""


class A:
    def m(self):
        return "A.m"

    @classmethod
    def make(cls):
        return f"A.make({cls.__name__})"

    @staticmethod
    def st():
        return "A.st"

    def __repr__(self):
        return f"<{type(self).__name__}>"


class B(A):
    def m(self):
        return "B.m>" + super().m()

    @classmethod
    def make(cls):
        return "B.make>" + super().make()

    def two_arg(self):
        return super(B, self).m()

    def nested(self):
        def inner():
            # __class__ cell is visible to nested functions but zero-arg super
            # needs the first argument, so use the explicit form.
            return super(__class__, self).m()

        return inner()

    def lam(self):
        f = lambda: __class__.__name__
        return f()

    def gen(self):
        yield super().m()
        yield __class__.__name__


class C(B):
    def m(self):
        return "C.m>" + super().m()

    @classmethod
    def make(cls):
        return "C.make>" + super().make()


c = C()
print("chain", c.m())
print("classmethod chain", C.make(), c.make())
print("two-arg", c.two_arg())
print("two-arg from C", super(C, c).m(), super(B, c).m())
print("nested __class__", c.nested())
print("lambda __class__", c.lam())
print("generator super", list(c.gen()))
print("staticmethod via super", super(C, c).st())
print("classmethod via two-arg super", super(B, C).make())


# Unbound super object and super in __init_subclass__-like classmethods
class Base:
    def hello(self):
        return f"hello from Base for {type(self).__name__}"


class Derived(Base):
    def hello(self):
        return "Derived+" + super().hello()


d = Derived()
sup = super(Derived, d)
print("super object method", sup.hello())
print("super type attr", super(Derived, Derived).hello(d))

# super() in a method of a class with a metaclass-free __class__ check
class Probe:
    def which(self):
        return __class__ is Probe, type(self) is Probe


class SubProbe(Probe):
    pass


print("__class__ vs type(self)", SubProbe().which())


# super() errors
def outside():
    try:
        super()
    except RuntimeError as e:
        return type(e).__name__


print("super outside class", outside())

try:
    super(B, 5)
except TypeError as e:
    print("super bad obj", type(e).__name__)

# super() with property access
class P:
    @property
    def val(self):
        return 1


class Q(P):
    @property
    def val(self):
        return super().val + 10


print("super property", Q().val)


# super() and data attributes are not found on super (only class attrs)
class R:
    cls_attr = "R.cls_attr"

    def __init__(self):
        self.inst = "inst"


class S(R):
    def look(self):
        try:
            return super().inst
        except AttributeError as e:
            return type(e).__name__

    def look_cls(self):
        return super().cls_attr


s = S()
print("super instance attr", s.look(), s.look_cls())


# Method defined outside, assigned to class: no __class__ cell available.
class T(A):
    pass


def ext(self):
    return super(T, self).m()


T.ext = ext
print("external two-arg super", T().ext())
