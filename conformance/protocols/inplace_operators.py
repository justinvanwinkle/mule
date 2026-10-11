"""In-place operators: __iadd__ etc., fallback to binary and reflected, aliasing, and augmented targets."""

log = []


class Acc:
    def __init__(self, v):
        self.v = v

    def __repr__(self):
        return f"Acc({self.v})"

    def __iadd__(self, o):
        log.append("iadd")
        self.v += o
        return self

    def __isub__(self, o):
        log.append("isub")
        self.v -= o
        return self

    def __imul__(self, o):
        self.v *= o
        return self

    def __itruediv__(self, o):
        self.v /= o
        return self

    def __ifloordiv__(self, o):
        self.v //= o
        return self

    def __imod__(self, o):
        self.v %= o
        return self

    def __ipow__(self, o):
        self.v **= o
        return self

    def __ilshift__(self, o):
        self.v <<= o
        return self

    def __irshift__(self, o):
        self.v >>= o
        return self

    def __iand__(self, o):
        self.v &= o
        return self

    def __ior__(self, o):
        self.v |= o
        return self

    def __ixor__(self, o):
        self.v ^= o
        return self

    def __imatmul__(self, o):
        self.v = self.v * 1000 + o
        return self


a = Acc(10)
alias = a
a += 5
a -= 2
print("iadd/isub", a, alias is a, log)
a *= 3; print("imul", a)
a //= 4; print("ifloordiv", a)
a %= 5; print("imod", a)
a **= 3; print("ipow", a)
a <<= 2; print("ilshift", a)
a >>= 1; print("irshift", a)
a &= 0b1110; print("iand", a)
a |= 1; print("ior", a)
a ^= 0b11; print("ixor", a)
a @= 7; print("imatmul", a)
a /= 2; print("itruediv", a)
print("alias still same", alias is a, alias)


# Fallback to __add__ when no __iadd__: rebinding, aliasing broken
class Imm:
    def __init__(self, v):
        self.v = v

    def __add__(self, o):
        log.append("add")
        return Imm(self.v + o)

    def __repr__(self):
        return f"Imm({self.v})"


log.clear()
i = Imm(1)
j = i
i += 1
print("fallback to add", i, j, i is j, log)


# __iadd__ returning NotImplemented falls back to __add__ then __radd__
class Picky:
    def __init__(self, v):
        self.v = v

    def __iadd__(self, o):
        log.append("Picky.iadd")
        if isinstance(o, int):
            self.v += o
            return self
        return NotImplemented

    def __add__(self, o):
        log.append("Picky.add")
        return NotImplemented


class Other:
    def __radd__(self, o):
        log.append("Other.radd")
        return "other handled"


log.clear()
p = Picky(1)
p += Other()
print("iadd->add->radd chain", p, log)

log.clear()
q = Picky(1)
try:
    q += "str"
except TypeError as e:
    print("all NotImplemented", type(e).__name__, log)


# __iadd__ returning something other than self rebinds the name
class Returner:
    def __iadd__(self, o):
        return f"replaced by {o}"


rt = Returner()
rt += 3
print("iadd return value bound", rt)

# Builtins: list += mutates (extend semantics, any iterable), tuple += rebinds
lst = [1]
lst_alias = lst
lst += (2, 3)
lst += "ab"
print("list iadd", lst, lst is lst_alias)
try:
    lst = lst + (4,)
except TypeError as e:
    print("list add tuple", type(e).__name__)

tup = (1,)
tup_alias = tup
tup += (2,)
print("tuple iadd rebinds", tup, tup_alias, tup is tup_alias)

# The famous tuple-containing-list case: mutation happens, then assignment fails.
t = ([1],)
try:
    t[0] += [2]
except TypeError as e:
    print("tuple item iadd", type(e).__name__, t)

# Augmented assignment evaluates target subexpressions once.
calls = []


def idx():
    calls.append("idx")
    return 0


class Holder:
    def __init__(self):
        self.items = [10]
        self._attr = 1

    @property
    def attr(self):
        calls.append("get attr")
        return self._attr

    @attr.setter
    def attr(self, v):
        calls.append(f"set attr {v}")
        self._attr = v


def get_holder():
    calls.append("holder")
    return h


h = Holder()
get_holder().items[idx()] += 5
get_holder().attr += 2
print("single evaluation", h.items, h.attr, calls)

# dict augmented
d = {"k": 1}
d["k"] += 1
d["k"] *= 10
print("dict aug", d)

# set in-place ops mutate
s = {1, 2}
s_alias = s
s |= {3}
s &= {1, 3}
s -= {1}
s ^= {4}
print("set inplace", sorted(s), s is s_alias)
