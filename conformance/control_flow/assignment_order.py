"""Assignment evaluation order: chained targets, tuple targets, and augmented assignment."""

log = []


class Tracer:
    """Records every item access and assignment."""

    def __init__(self, name, data=None):
        self.name = name
        self.data = dict(data or {})

    def __getitem__(self, key):
        log.append(f"get {self.name}[{key!r}]")
        return self.data[key]

    def __setitem__(self, key, value):
        log.append(f"set {self.name}[{key!r}]={value!r}")
        self.data[key] = value

    def __repr__(self):
        return f"{self.name}{self.data}"


def v(name, value):
    log.append(name)
    return value


def flush(label):
    print(label, log)
    log.clear()


# chained assignment assigns left to right after evaluating the RHS once
a = [0, 0, 0]
i = 0
a[i] = i = 2
print("a[i] = i = 2", a, i)

a = [0, 0, 0]
i = 0
i = a[i] = 2
print("i = a[i] = 2", a, i)

# tuple target: assignments happen left to right, later targets see earlier ones
a = [0, 0, 0]
i = 0
i, a[i] = 1, 9
print("i, a[i] = 1, 9", a, i)

a = [0, 0, 0]
i = 0
a[i], i = 9, 2
print("a[i], i = 9, 2", a, i)

# self-referential list
x = x[0] = [None]
print("x = x[0] = [None] is self-referential:", x[0] is x, len(x))

# chained assignment to tracer targets
t = Tracer("t")
u = Tracer("u")
t[v("k1", "a")] = u[v("k2", "b")] = v("rhs", 1)
flush("chained subscript order")

# tuple targets order of key evaluation and assignment
t[v("k1", "x")], u[v("k2", "y")] = v("r1", 1), v("r2", 2)
flush("tuple subscript order")

# augmented assignment evaluates the target container and key once
t = Tracer("t", {"n": 10})


def key():
    log.append("key()")
    return "n"


def container():
    log.append("container()")
    return t


container()[key()] += v("rhs", 5)
flush("augmented subscript")
print("t now", t)


# augmented attribute assignment
class Obj:
    def __init__(self):
        object.__setattr__(self, "val", 1)

    def __getattribute__(self, name):
        log.append("getattr " + name)
        return object.__getattribute__(self, name)

    def __setattr__(self, name, value):
        log.append(f"setattr {name}={value}")
        object.__setattr__(self, name, value)


o = Obj()
o.val += 1
flush("augmented attribute")


# in-place operator mutating vs rebinding
class InPlace:
    def __init__(self, n):
        self.n = n

    def __iadd__(self, other):
        log.append("__iadd__")
        self.n += other
        return self

    def __repr__(self):
        return f"InPlace({self.n})"


holder = Tracer("h", {"x": InPlace(1)})
holder["x"] += 4
flush("iadd still calls __setitem__")
print("holder", holder)

# tuple item augmented assignment: mutation happens, then TypeError
tup = ([1],)
try:
    tup[0] += [2]
except TypeError as e:
    print("tuple iadd", type(e).__name__, tup)

# RHS fully evaluated before any target assignment
a, b = 1, 2
a, b = b, a + b
print("swap fib", a, b)
xs = [1, 2, 3]
xs[0], xs[xs[0]] = 2, 0
print("index depends on earlier target", xs)

# starred assignment target ordering
first, *rest, t[v("k", "last")] = v("rhs", range(4))
flush("starred with subscript")
print("starred", first, rest)

# nested tuple assignment
(p, (q, r)), s = (1, (2, 3)), 4
print("nested", p, q, r, s)

# multiple chained targets with unpacking
m, n = o2 = (5, 6)
print("chained with unpack", m, n, o2)

# assignment expression in subscript evaluated before the RHS? (RHS first)
arr = [0, 0, 0]
arr[v("idx", 1)] = v("value", 7)
flush("plain subscript: rhs before index")

# dict key order in literal with duplicate key side effects
d = {v("k1", "a"): v("v1", 1), v("k2", "a"): v("v2", 2)}
flush("dict literal order")
print("dict", d)
