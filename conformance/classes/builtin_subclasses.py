"""Subclassing list, dict, str, int: which builtin methods route through overrides and which bypass them."""


class LoudList(list):
    def __init__(self, *a):
        super().__init__(*a)
        self.log = []

    def append(self, x):
        self.log.append(f"append {x}")
        super().append(x)

    def __setitem__(self, i, v):
        self.log.append(f"setitem {i}")
        super().__setitem__(i, v)

    def __getitem__(self, i):
        self.log.append(f"getitem {i}")
        return super().__getitem__(i)

    def __len__(self):
        self.log.append("len")
        return super().__len__()


ll = LoudList([1, 2])
ll.append(3)
ll.extend([4])  # extend does NOT call append
ll[0] = 10
v = ll[1]
n = len(ll)
s = sum(ll)  # iteration does not call __getitem__
print("loud list", ll, v, n, s)
print("loud list log", ll.log)
ll.log.clear()
sliced = ll[1:3]
print("slice type", type(sliced).__name__, sliced, ll.log)
print("concat type", type(ll + [1]).__name__, type(ll * 2).__name__)
print("isinstance list", isinstance(ll, list), ll == [10, 2, 3, 4])


class DefaultDict(dict):
    def __missing__(self, key):
        val = f"<{key}>"
        self[key] = val
        return val


dd = DefaultDict(a=1)
print("missing via []", dd["a"], dd["b"])
print("get bypasses missing", dd.get("c"), "c" in dd)
print("after", sorted(dd.items()))


class LowerKeys(dict):
    def __setitem__(self, k, v):
        super().__setitem__(k.lower(), v)

    def __getitem__(self, k):
        return super().__getitem__(k.lower())


lk = LowerKeys({"MixedInit": 1})  # constructor bypasses __setitem__
lk["UPPER"] = 2
lk.update({"Updated": 3})  # update bypasses __setitem__
lk.setdefault("SetDef", 4)  # bypasses too
print("dict keys", sorted(lk))
print("getitem lowered", lk["UPPER"], lk["upper"])
try:
    lk["MixedInit"]
except KeyError as e:
    print("bypassed key not found", type(e).__name__)
print("dict copy type", type(lk.copy()).__name__, type(lk | {}).__name__)


class Shout(str):
    def __str__(self):
        return self.upper() + "!"

    def __len__(self):
        return 1000


sh = Shout("hey")
print("str()", str(sh), "format", f"{sh}", "repr", repr(sh))
print("len override", len(sh), "concat", sh + "x", type(sh + "x").__name__)
print("join", "-".join([sh, sh]), "upper type", type(sh.upper()).__name__)
print("print direct", sh)


class Money(int):
    def __add__(self, other):
        return Money(int(self) + int(other))

    __radd__ = __add__

    def __repr__(self):
        return f"${int(self)}"

    def __str__(self):
        return repr(self)


m = Money(5)
print("int subclass ops", m + 3, 3 + m, sum([m, m, m]), m * 2, type(m * 2).__name__)
print("format int sub", f"{m:04d}", f"{m!r}", str(m))
print("bool / hash", bool(Money(0)), hash(m) == hash(5), m == 5)


class Truthy(int):
    def __bool__(self):
        return True


print("bool override", bool(Truthy(0)), Truthy(0) == 0, "yes" if Truthy(0) else "no")


class CountingDict(dict):
    def __init__(self, *a, **k):
        super().__init__(*a, **k)
        self.lookups = 0

    def __getitem__(self, k):
        self.lookups += 1
        return super().__getitem__(k)


cd = CountingDict(x=1, y=2)
_ = cd["x"]
_ = [cd[k] for k in cd]
fmt = "{x}-{y}".format_map(cd)
print("format_map", fmt, "lookups", cd.lookups)
_ = cd.get("x")
_ = dict(cd)
print("lookups after get/dict()", cd.lookups)


class OrderedSet(list):
    def __contains__(self, item):
        return any(x == item for x in self)

    def add(self, x):
        if x not in self:
            self.append(x)


os_ = OrderedSet()
for x in [3, 1, 3, 2, 1]:
    os_.add(x)
print("custom contains", os_, 2 in os_, 5 in os_)

# reversed / sorted keep base types
print("sorted returns list", type(sorted(LoudList([3, 1]))).__name__)
