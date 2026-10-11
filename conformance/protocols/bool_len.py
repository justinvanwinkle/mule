"""Truthiness: __bool__ vs __len__ precedence, invalid return types, and truth testing in control flow."""

log = []


class Both:
    def __bool__(self):
        log.append("bool")
        return False

    def __len__(self):
        log.append("len")
        return 5


class LenOnly:
    def __init__(self, n):
        self.n = n

    def __len__(self):
        log.append(f"len {self.n}")
        return self.n


class Neither:
    pass


log.clear()
print("bool beats len", bool(Both()), log)
log.clear()
print("len fallback", bool(LenOnly(0)), bool(LenOnly(3)), log)
print("neither is truthy", bool(Neither()))
print("len() still works with __bool__", len(Both()))

# Truthiness in control flow contexts
log.clear()
x = LenOnly(0)
if x:
    print("unreachable")
while x:
    break
r1 = x or "fallback"
r2 = x and "never"
r3 = not x
r4 = [i for i in [LenOnly(2), LenOnly(0)] if i]
r5 = "yes" if LenOnly(1) else "no"
print("contexts", r1, type(r2).__name__, r3, len(r4), r5)
print("context log", log)

# `and`/`or` return operands, evaluating truthiness once per operand
log.clear()
a = LenOnly(0)
b = LenOnly(4)
res = a or b
print("or returns operand", res is b, log)
log.clear()
res = b and a
print("and returns operand", res is a, log)


# Invalid __bool__ return
class BadBool:
    def __bool__(self):
        return 1


try:
    bool(BadBool())
except TypeError as e:
    print("bool returns int", type(e).__name__)

try:
    if BadBool():
        pass
except TypeError as e:
    print("if with bad bool", type(e).__name__)


# Invalid __len__ returns
class NegLen:
    def __len__(self):
        return -1


class FloatLen:
    def __len__(self):
        return 2.0


class HugeLen:
    def __len__(self):
        return 2 ** 100


for cls in (NegLen, FloatLen, HugeLen):
    for fn_name, fn in (("len", len), ("bool", bool)):
        try:
            fn(cls())
            print(cls.__name__, fn_name, "ok")
        except (ValueError, TypeError, OverflowError) as e:
            print(cls.__name__, fn_name, type(e).__name__)


# __len__ returning an int subclass / bool
class BoolLen:
    def __len__(self):
        return True


print("bool as len", len(BoolLen()), bool(BoolLen()))


class IndexLike:
    def __index__(self):
        return 3


class IndexLen:
    def __len__(self):
        return IndexLike()


try:
    print("len via __index__", len(IndexLen()))
except TypeError as e:
    print("len via __index__", type(e).__name__)


# __bool__ = None makes it raise
class NoBool:
    __bool__ = None


try:
    bool(NoBool())
except TypeError as e:
    print("bool None", type(e).__name__)


# Inheritance: subclass defining __len__ while parent defines __bool__
class P:
    def __bool__(self):
        return True


class C(P):
    def __len__(self):
        return 0


print("inherited bool wins", bool(C()))

# Builtins
print("builtin truthiness", bool(0), bool(0.0), bool(0j), bool(""), bool(b""), bool([]), bool(()), bool({}), bool(set()), bool(range(0)), bool(None))
print("builtin truthy", bool(-1), bool(" "), bool([0]), bool((None,)), bool(range(1, 2)), bool(float("nan")))

# all()/any() use truthiness and short-circuit
log.clear()
print("any", any([LenOnly(0), LenOnly(1), LenOnly(2)]), log)
log.clear()
print("all", all([LenOnly(1), LenOnly(0), LenOnly(2)]), log)

# filter(None, ...) uses truthiness
print("filter None", [o.n for o in filter(None, [LenOnly(0), LenOnly(7), LenOnly(0), LenOnly(1)])])
