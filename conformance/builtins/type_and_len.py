"""type() with one argument on builtins and user classes, and len() via __len__ on custom objects."""

samples = [0, True, 1.5, 2j, "s", b"b", bytearray(), [], (), {}, set(), frozenset(), None, range(1), slice(1), ..., NotImplemented, type, object(), len, print, iter([]), iter(()), (x for x in ()), {}.keys(), {}.values(), {}.items(), memoryview(b"")]
for s in samples:
    print("type", type(s).__name__)
print("type is", int is int, bool is bool, bool is int, type(type) is type)


class Base:
    pass


class Child(Base):
    pass


print("user", type(Child()).__name__, type(Child()) is Child, type(Child()) is Base, type(Child).__name__)
print("type of function", type(lambda: 0).__name__, type(Base.__init__).__name__)


def gen():
    yield 1


async def coro_fn():
    return 1


print("generator function", type(gen).__name__, type(gen()).__name__, type(coro_fn).__name__)
print("module type", type(type).__module__, int.__module__)
print("qualname", type(Child()).__qualname__, int.__qualname__)
print("call type to construct", int("12"), type([])("ab"), str(3.5))


class Length:
    def __init__(self, n):
        self.n = n

    def __len__(self):
        return self.n


print("custom len", len(Length(0)), len(Length(10**6)))
big = Length(2**63)
try:
    len(big)
except OverflowError as e:
    print("len overflow", type(e).__name__)


class IndexLen:
    def __len__(self):
        class I:
            def __index__(self):
                return 4
        return I()


try:
    print("len via __index__", len(IndexLen()))
except TypeError as e:
    print("len via __index__", type(e).__name__)


class LenBool:
    def __len__(self):
        return False


print("len returns bool", len(LenBool()))


class InstanceLen:
    pass


obj = InstanceLen()
obj.__len__ = lambda: 3
try:
    len(obj)
except TypeError as e:
    print("instance __len__ ignored", type(e).__name__)
print("explicit call instance attr", obj.__len__())
print("len builtin name", len.__name__)
