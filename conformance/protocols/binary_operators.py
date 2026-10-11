"""Every binary operator dunder including @ (matmul), divmod, pow with modulo, and operand evaluation order."""


class V:
    def __init__(self, *xs):
        self.xs = list(xs)

    def __repr__(self):
        return f"V{tuple(self.xs)}"

    def _zip(self, other, op):
        if isinstance(other, V):
            return V(*(op(a, b) for a, b in zip(self.xs, other.xs)))
        if isinstance(other, (int, float)):
            return V(*(op(a, other) for a in self.xs))
        return NotImplemented

    def __add__(self, o): return self._zip(o, lambda a, b: a + b)
    def __sub__(self, o): return self._zip(o, lambda a, b: a - b)
    def __mul__(self, o): return self._zip(o, lambda a, b: a * b)
    def __truediv__(self, o): return self._zip(o, lambda a, b: a / b)
    def __floordiv__(self, o): return self._zip(o, lambda a, b: a // b)
    def __mod__(self, o): return self._zip(o, lambda a, b: a % b)
    def __pow__(self, o, mod=None):
        if mod is None:
            return self._zip(o, lambda a, b: a ** b)
        return self._zip(o, lambda a, b: pow(a, b, mod))
    def __lshift__(self, o): return self._zip(o, lambda a, b: a << b)
    def __rshift__(self, o): return self._zip(o, lambda a, b: a >> b)
    def __and__(self, o): return self._zip(o, lambda a, b: a & b)
    def __or__(self, o): return self._zip(o, lambda a, b: a | b)
    def __xor__(self, o): return self._zip(o, lambda a, b: a ^ b)

    def __divmod__(self, o):
        return (self // o, self % o)

    def __matmul__(self, o):
        if not isinstance(o, V):
            return NotImplemented
        return sum(a * b for a, b in zip(self.xs, o.xs))


a = V(7, 12, -9)
b = V(2, 5, 4)
print("add", a + b, a + 1)
print("sub", a - b, a - 1.5)
print("mul", a * b, a * 2)
print("truediv", a / b)
print("floordiv", a // b, a // 2)
print("mod", a % b, a % 5)
print("pow", b ** V(2, 2, 2), b ** 3)
print("pow 3-arg", pow(b, 3, 7), pow(a, V(2, 2, 2), 5))
print("lshift/rshift", b << 2, a >> 1)
print("and/or/xor", a & b, a | b, a ^ b)
print("divmod", divmod(a, b), divmod(a, 4))
print("matmul", a @ b, V(1, 2) @ V(3, 4))
try:
    a @ 3
except TypeError as e:
    print("matmul int", type(e).__name__)
try:
    "x" + a
except TypeError as e:
    print("no radd", type(e).__name__)


# Builtin matmul is unsupported for ints/lists
try:
    [1] @ [2]
except TypeError as e:
    print("list matmul", type(e).__name__)


# Operand evaluation order: left operand evaluated first, then right, then the op.
trace = []


class T:
    def __init__(self, n):
        self.n = n

    def __add__(self, o):
        trace.append(f"add {self.n}+{o.n}")
        return T(self.n + o.n)

    def __mul__(self, o):
        trace.append(f"mul {self.n}*{o.n}")
        return T(self.n * o.n)

    def __pow__(self, o):
        trace.append(f"pow {self.n}**{o.n}")
        return T(self.n ** o.n)


def mk(n):
    trace.append(f"eval {n}")
    return T(n)


r = mk(1) + mk(2) * mk(3)
print("precedence result", r.n)
print("evaluation trace", trace)
trace.clear()
r = mk(2) ** mk(3) ** mk(2)
print("right-assoc pow", r.n, trace)
trace.clear()
r = mk(1) + mk(2) + mk(3)
print("left-assoc add", r.n, trace)


# Operators on a class that returns non-standard types
class Weird:
    def __add__(self, o):
        return "added"

    def __sub__(self, o):
        return None

    def __mul__(self, o):
        return [o] * 2

    def __mod__(self, o):
        return f"mod:{o!r}"


w = Weird()
print("weird returns", w + 1, w - 1, w * "x", w % (1, 2))

# Operators are looked up on the type, not instance
w.__add__ = lambda o: "instance add"
print("instance dunder ignored", w + 1, w.__add__(1))

# Dunder assigned on class after creation takes effect
Weird.__truediv__ = lambda self, o: f"late div {o}"
print("late class dunder", w / 9)
