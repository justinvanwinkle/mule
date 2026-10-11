"""len() and bool() of built-in containers and custom __len__/__bool__ classes."""

values = [[], [0], (), (None,), {}, {"": 0}, set(), {0}, frozenset(), "", " ", range(0), range(1), b"", bytearray(b"x")]
for v in values:
    print("len bool", type(v).__name__, len(v), bool(v))

print("dict views", len({}.keys()), bool({1: 2}.items()))
print("nested empty truthy", bool([[]]), bool(([],)), bool({(): ()}))
print("not empty", not [], not [0])
print("or default", [] or "fallback", [1] or "fallback")
print("and short", {} and 1, {1: 1} and "second")


class Sized:
    def __init__(self, n):
        self.n = n

    def __len__(self):
        return self.n


print("custom len", len(Sized(3)), bool(Sized(0)), bool(Sized(2)))
try:
    len(Sized(-1))
except ValueError as e:
    print("negative len", type(e).__name__)
try:
    bool(Sized(-1))
except ValueError as e:
    print("negative len bool", type(e).__name__)


class FloatLen:
    def __len__(self):
        return 2.0


try:
    len(FloatLen())
except TypeError as e:
    print("float len", type(e).__name__)


class IndexLen:
    def __len__(self):
        return True


print("bool as len", len(IndexLen()))


class Both:
    def __bool__(self):
        return False

    def __len__(self):
        return 5


print("bool beats len", bool(Both()), len(Both()))


class BadBool:
    def __bool__(self):
        return 1


try:
    bool(BadBool())
except TypeError as e:
    print("bool returns int", type(e).__name__)


class Plain:
    pass


print("default truthy", bool(Plain()))
try:
    len(Plain())
except TypeError as e:
    print("no len", type(e).__name__)
try:
    len(5)
except TypeError as e:
    print("len int", type(e).__name__)
print("len big range", len(range(10**6)), len(range(0, 10**6, 7)))
if [0]:
    print("if list with zero is true")
while []:
    print("never")
print("if empty dict", "yes" if {} else "no")
