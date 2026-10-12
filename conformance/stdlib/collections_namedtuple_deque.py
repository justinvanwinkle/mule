"""collections.namedtuple (defaults, _replace, _asdict, _fields, _make, subclassing) and deque (maxlen, rotate, appendleft, extendleft)."""
from collections import deque, namedtuple

Point = namedtuple("Point", "x y")
p = Point(1, y=2)
print("namedtuple repr", p)
print("namedtuple index/attr", p[0], p.y, p == (1, 2), isinstance(p, tuple))
print("namedtuple unpack", *p)
print("namedtuple _fields", Point._fields)
print("namedtuple _asdict", p._asdict())
print("namedtuple _replace", p._replace(y=20), p)
print("namedtuple _make", Point._make([5, 6]), Point._make(iter("ab")))
try:
    p.x = 9
except AttributeError as e:
    print("namedtuple immutable", type(e).__name__)
try:
    p._replace(z=1)
except (TypeError, ValueError) as e:
    print("namedtuple bad replace", type(e).__name__)

Config = namedtuple("Config", ["host", "port", "debug"], defaults=[8080, False])
print("defaults", Config("localhost"), Config("h", 1, True))
print("_field_defaults", Config._field_defaults)
Renamed = namedtuple("Renamed", ["ok", "def", "ok", "_x"], rename=True)
print("rename", Renamed._fields)


class Vec(namedtuple("Vec", "x y")):
    __slots__ = ()

    def __add__(self, other):
        return Vec(self.x + other.x, self.y + other.y)

    @property
    def norm1(self):
        return abs(self.x) + abs(self.y)


v = Vec(1, -2) + Vec(3, 4)
print("subclass", v, v.norm1, type(v).__name__)
print("sorted namedtuples", sorted([Point(2, 1), Point(1, 5), Point(1, 2)]))
print("namedtuple as dict key", {Point(0, 0): "origin"}[(0, 0)])

d = deque([1, 2, 3])
d.append(4)
d.appendleft(0)
print("deque", d)
print("deque pop", d.pop(), d.popleft(), d)
d.extend([4, 5])
d.extendleft([-1, -2])
print("extendleft reverses", d)
d.rotate(2)
print("rotate 2", d)
d.rotate(-3)
print("rotate -3", d)
d.rotate(len(d) * 5 + 1)
print("rotate big", d)
print("deque index/count", d[0], d[-1], d.index(3), d.count(4))
d.remove(3)
d.insert(1, "ins")
print("remove/insert", d)
d.reverse()
print("reverse", d)
print("deque in", "ins" in d, 99 in d)
del d[2]
print("del item", d)

bounded = deque(maxlen=3)
for i in range(6):
    bounded.append(i)
    print("bounded append", i, list(bounded))
bounded.appendleft("L")
print("maxlen appendleft drops right", bounded, bounded.maxlen)
bounded.extend("xyz1")
print("maxlen extend", bounded)
print("maxlen zero", deque([1, 2, 3], maxlen=0))
print("tail of iterable", deque(range(100), maxlen=2))
try:
    deque().pop()
except IndexError as e:
    print("empty pop", type(e).__name__)
dd = deque("ab")
print("deque mul/add", dd * 2, dd + deque("c"))
print("deque eq", deque([1, 2]) == deque([1, 2]), deque([1]) == [1])
print("deque copy", dd.copy(), dd.copy() is not dd)
try:
    for x in dd:
        dd.append(x)
except RuntimeError as e:
    print("mutate during iter", type(e).__name__)
