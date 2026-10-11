"""Container item protocol: __getitem__ with ints, slices and tuples, __setitem__, __delitem__, __len__, __missing__."""

log = []


class Recorder:
    def __getitem__(self, key):
        log.append(("get", key))
        return key

    def __setitem__(self, key, value):
        log.append(("set", key, value))

    def __delitem__(self, key):
        log.append(("del", key))


r = Recorder()
print("int key", r[3], r[-1])
print("slice", r[1:5], r[::2], r[:], r[1:2:3], r[None:None])
print("ext slice tuple", r[1:2, 3], r[..., 0], r[:, :], r[()], r[1,])
print("slice attrs", r[1:10:2].start, r[1:10:2].stop, r[1:10:2].step)
print("arbitrary keys", r["k"], r[None], r[(1, 2)])
r[0] = "a"
r[1:3] = [9, 9]
del r[2]
del r[::2]
print("log tail", log[-4:])


class Seq:
    def __init__(self, *items):
        self.items = list(items)

    def __len__(self):
        return len(self.items)

    def __getitem__(self, i):
        if isinstance(i, slice):
            return Seq(*self.items[i])
        if not isinstance(i, int):
            raise TypeError(f"bad index type {type(i).__name__}")
        if i < 0:
            i += len(self)
        if not 0 <= i < len(self):
            raise IndexError("Seq index out of range")
        return self.items[i]

    def __setitem__(self, i, v):
        if isinstance(i, slice):
            self.items[i] = v
        else:
            self.items[i] = v

    def __delitem__(self, i):
        del self.items[i]

    def __repr__(self):
        return f"Seq{self.items}"


s = Seq(10, 20, 30, 40, 50)
print("index", s[0], s[-1], s[2])
print("slice", s[1:3], s[::-1], s[::2], s[-2:])
try:
    s[10]
except IndexError as e:
    print("out of range", e)
try:
    s["x"]
except TypeError as e:
    print("bad type", e)
s[0] = 11
s[1:3] = [21, 22, 23]
print("after set", s, len(s))
del s[0]
del s[-2:]
print("after del", s)

# slice.indices
sl = slice(None, None, -2)
print("slice.indices", sl.indices(7), slice(-3, None).indices(5), slice(1, 100, 3).indices(10))
print("slice eq/hash", slice(1, 2) == slice(1, 2), hash(slice(1, 2)) == hash(slice(1, 2)))
print("slice repr", repr(slice(1, None, -1)))


class Mapping:
    def __init__(self):
        self.d = {}

    def __getitem__(self, k):
        return self.d[k]

    def __setitem__(self, k, v):
        self.d[k] = v

    def __len__(self):
        return len(self.d)


m = Mapping()
m["a"] = 1
m[("t", 1)] = 2
print("mapping", m["a"], m["t", 1], len(m))
try:
    m["missing"]
except KeyError as e:
    print("missing key", type(e).__name__, e.args)
try:
    del m["a"]
except (TypeError, AttributeError) as e:
    print("no __delitem__", type(e).__name__)


class DefaultingDict(dict):
    def __missing__(self, key):
        log.append(("missing", key))
        return key * 2


log.clear()
dd = DefaultingDict(a="A")
print("__missing__", dd["a"], dd["bb"], dd.get("bb", "get-default"), "bb" in dd, log)


class CountingMissing(dict):
    def __missing__(self, key):
        self[key] = 0
        return 0


cm = CountingMissing()
for w in "abracadabra":
    cm[w] += 1
print("missing+augassign", sorted(cm.items()))

# __missing__ only applies to dict subclasses, not to plain classes
class NotDict:
    def __getitem__(self, k):
        raise KeyError(k)

    def __missing__(self, k):
        return "never"


try:
    NotDict()["x"]
except KeyError as e:
    print("missing ignored outside dict", type(e).__name__)

# Item access with bool, negative zero and big ints on builtins
lst = [10, 20, 30]
print("bool index", lst[True], lst[False], lst[-0])
try:
    lst[2 ** 70]
except IndexError as e:
    print("huge index", type(e).__name__)
try:
    lst[1.0]
except TypeError as e:
    print("float index", type(e).__name__)
print("slice beyond bounds", lst[5:10], lst[-100:2], lst[::-100])

# Assignment to extended slice requires matching length
try:
    lst[::2] = [1]
except ValueError as e:
    print("extended slice length", type(e).__name__)
lst[::2] = [1, 3]
print("extended slice assign", lst)
