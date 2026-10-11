"""Reflected operators: __radd__ etc., the subclass-reflected-first rule, and when reflection is skipped."""

log = []


class Num:
    def __init__(self, v):
        self.v = v

    def __repr__(self):
        return f"{type(self).__name__}({self.v})"

    def __add__(self, o):
        log.append(f"{self!r}.__add__({o!r})")
        if isinstance(o, Num):
            return Num(self.v + o.v)
        if isinstance(o, int):
            return Num(self.v + o)
        return NotImplemented

    def __radd__(self, o):
        log.append(f"{self!r}.__radd__({o!r})")
        if isinstance(o, int):
            return Num(o + self.v)
        return NotImplemented

    def __rsub__(self, o):
        return Num(o - self.v)

    def __rmul__(self, o):
        return Num(o * self.v)

    def __rtruediv__(self, o):
        return Num(o / self.v)

    def __rfloordiv__(self, o):
        return Num(o // self.v)

    def __rmod__(self, o):
        return Num(o % self.v)

    def __rpow__(self, o, mod=None):
        return Num(o ** self.v)

    def __rlshift__(self, o):
        return Num(o << self.v)

    def __rrshift__(self, o):
        return Num(o >> self.v)

    def __rand__(self, o):
        return Num(o & self.v)

    def __ror__(self, o):
        return Num(o | self.v)

    def __rxor__(self, o):
        return Num(o ^ self.v)

    def __rmatmul__(self, o):
        return f"rmatmul {o!r} @ {self.v}"

    def __rdivmod__(self, o):
        return ("rdivmod", divmod(o, self.v))


n = Num(3)
print("radd via int", 10 + n, log)
log.clear()
print("radd via sum", sum([Num(1), Num(2)]), len(log))
log.clear()
print("rsub rmul", 10 - n, 10 * n)
print("rtruediv rfloordiv rmod", 9 / n, 10 // n, 10 % n)
print("rpow", 2 ** n)
print("rshifts", 1 << n, 64 >> n)
print("rbitwise", 6 & n, 4 | n, 5 ^ n)
print("rmatmul", [1, 2] @ n)
print("rdivmod", divmod(17, n))


# Subclass rule: if right operand's type is a subclass of the left's type and
# overrides the reflected method, the reflected method is tried first.
class Sub(Num):
    def __radd__(self, o):
        log.append(f"{self!r}.__radd__({o!r})")
        return Sub(o.v * 100 + self.v)


log.clear()
print("subclass reflected first", Num(1) + Sub(2), log)


# Subclass that does NOT override __radd__: normal order.
class SubNoOverride(Num):
    pass


log.clear()
print("subclass no override", Num(1) + SubNoOverride(2), log)


# Same type: reflected not tried even if __add__ returns NotImplemented.
class Same:
    def __add__(self, o):
        log.append("Same.__add__")
        return NotImplemented

    def __radd__(self, o):
        log.append("Same.__radd__")
        return "radd"


log.clear()
try:
    Same() + Same()
except TypeError as e:
    print("same type no reflection", type(e).__name__, log)


# Different unrelated types: left first, then right reflected.
class L:
    def __add__(self, o):
        log.append("L.__add__")
        return NotImplemented


class R:
    def __radd__(self, o):
        log.append("R.__radd__")
        return "R handled"


log.clear()
print("unrelated", L() + R(), log)

# Left operand without __add__ at all goes straight to right's __radd__
class OnlyR:
    def __radd__(self, o):
        return f"OnlyR got {o!r}"


print("builtin left falls to radd", "s" + OnlyR(), [1] + OnlyR(), (1,) + OnlyR())

# int subclass with __radd__ is preferred over int.__add__
class MyInt(int):
    def __radd__(self, o):
        return f"MyInt.__radd__({o})"

    def __rsub__(self, o):
        return f"MyInt.__rsub__({o})"


print("int subclass reflected first", 1 + MyInt(2), 5 - MyInt(1))
print("bool+int subclass", True + MyInt(2))


class MyStr(str):
    def __radd__(self, o):
        return f"<{o}|{self}>"


print("str subclass radd", "a" + MyStr("b"))

# Reflected comparison of pow with three args is never reflected
try:
    pow(2, Num(3), 5)
except TypeError as e:
    print("3-arg pow no reflection", type(e).__name__)
