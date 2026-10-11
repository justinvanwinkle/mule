"""collections.abc: isinstance/issubclass of builtins, custom Mapping/Sequence/Set subclasses with mixin methods, structural __subclasshook__ checks."""
from collections import abc
from collections import OrderedDict, deque


def gen():
    yield 1


builtins_cases = [
    ("list", []), ("tuple", ()), ("str", ""), ("bytes", b""), ("dict", {}),
    ("set", set()), ("frozenset", frozenset()), ("range", range(1)),
    ("deque", deque()), ("generator", gen()), ("int", 0), ("dict_keys", {}.keys()),
    ("dict_items", {}.items()), ("dict_values", {}.values()), ("memoryview", memoryview(b"")),
]
abcs = [abc.Iterable, abc.Iterator, abc.Sized, abc.Container, abc.Hashable,
        abc.Sequence, abc.MutableSequence, abc.Mapping, abc.MutableMapping,
        abc.Set, abc.MutableSet, abc.Generator, abc.Collection, abc.Reversible,
        abc.Buffer]
for name, obj in builtins_cases:
    flags = "".join("1" if isinstance(obj, a) else "0" for a in abcs)
    print("isinstance", name.ljust(11), flags)
print("callable", isinstance(len, abc.Callable), isinstance(1, abc.Callable))
print("odict mutable mapping", issubclass(OrderedDict, abc.MutableMapping))
print("list hashable", isinstance([], abc.Hashable))


class Squares(abc.Sequence):
    def __init__(self, n):
        self.n = n

    def __len__(self):
        return self.n

    def __getitem__(self, i):
        if isinstance(i, slice):
            return [self[j] for j in range(*i.indices(self.n))]
        if i < 0:
            i += self.n
        if not 0 <= i < self.n:
            raise IndexError(i)
        return i * i


sq = Squares(6)
print("seq iter", list(sq))
print("seq contains", 16 in sq, 17 in sq)
print("seq reversed", list(reversed(sq)))
print("seq index/count", sq.index(9), sq.count(4))
print("seq slice", sq[1:5:2])
print("seq isinstance", isinstance(sq, abc.Sequence), isinstance(sq, abc.MutableSequence))
try:
    sq.index(7)
except ValueError as e:
    print("seq index missing", type(e).__name__)


class FrozenDict(abc.Mapping):
    def __init__(self, *args, **kw):
        self._d = dict(*args, **kw)

    def __getitem__(self, k):
        return self._d[k]

    def __iter__(self):
        return iter(self._d)

    def __len__(self):
        return len(self._d)


fd = FrozenDict(a=1, b=2)
print("map get", fd.get("a"), fd.get("z", "dflt"))
print("map contains", "a" in fd, "z" in fd)
print("map views", list(fd.keys()), list(fd.values()), list(fd.items()))
print("map eq", fd == {"a": 1, "b": 2}, fd == {"a": 1})
print("map dict()", dict(fd))
print("map ** unpack", {**fd, "c": 3})
print("map not mutable", isinstance(fd, abc.MutableMapping), hasattr(fd, "pop"))
print("map keys view is Set", isinstance(fd.keys(), abc.Set), fd.keys() & {"a", "q"})


class Registry(abc.MutableMapping):
    def __init__(self):
        self.store = {}

    def __getitem__(self, k):
        return self.store[k.lower()]

    def __setitem__(self, k, v):
        self.store[k.lower()] = v

    def __delitem__(self, k):
        del self.store[k.lower()]

    def __iter__(self):
        return iter(self.store)

    def __len__(self):
        return len(self.store)


r = Registry()
r["Foo"] = 1
r.update(BAR=2, baz=3)
print("mutmap", sorted(r.items()))
print("mutmap setdefault", r.setdefault("FOO", 99), r.setdefault("new", 5))
print("mutmap pop", r.pop("BAZ"), r.pop("missing", "none"))
print("mutmap popitem", r.popitem())
r.clear()
print("mutmap clear", len(r))


class Bits(abc.Set):
    def __init__(self, it=()):
        self.items = sorted(set(it))

    def __contains__(self, x):
        return x in self.items

    def __iter__(self):
        return iter(self.items)

    def __len__(self):
        return len(self.items)

    def __repr__(self):
        return f"Bits({self.items})"


x, y = Bits([1, 2, 3]), Bits([2, 3, 4])
print("set ops", x & y, x | y, x - y, x ^ y)
print("set compare", Bits([2]) <= x, x < x, x == Bits([3, 2, 1]), x.isdisjoint(Bits([9])))


class Duck:
    def __iter__(self):
        return iter([])

    def __len__(self):
        return 0


print("structural", isinstance(Duck(), abc.Iterable), isinstance(Duck(), abc.Sized), isinstance(Duck(), abc.Sequence))


class Incomplete(abc.Sequence):
    def __len__(self):
        return 0


try:
    Incomplete()
except TypeError as e:
    print("abstract instantiation", type(e).__name__)
abc.Sequence.register(Duck)
print("registered", isinstance(Duck(), abc.Sequence), issubclass(Duck, abc.Reversible))
