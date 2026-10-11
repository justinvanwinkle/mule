"""Evaluation order of augmented assignment on attributes/subscripts with side-effecting getters, and of assignment targets."""

log = []


def t(label, value):
    log.append(label)
    return value


class Obj:
    def __init__(self, name):
        object.__setattr__(self, "name", name)
        object.__setattr__(self, "store", {})

    def __getattr__(self, attr):
        log.append(f"get {self.name}.{attr}")
        return self.store[attr]

    def __setattr__(self, attr, value):
        log.append(f"set {self.name}.{attr}={value!r}")
        self.store[attr] = value


class Box:
    def __init__(self, name, data):
        self.name = name
        self.data = data

    def __getitem__(self, k):
        log.append(f"getitem {self.name}[{k!r}]")
        return self.data[k]

    def __setitem__(self, k, v):
        log.append(f"setitem {self.name}[{k!r}]={v!r}")
        self.data[k] = v


class Num:
    def __init__(self, v):
        self.v = v

    def __iadd__(self, other):
        log.append(f"iadd {self.v}+{other}")
        return Num(self.v + other)

    def __repr__(self):
        return f"Num({self.v})"


a = Obj("a")
inner = Obj("inner")
inner.d = Num(1)
a.b = Box("b", {"k": inner})
log.clear()

# a.b[c()].d += e()
t("a", a).b[t("c()", "k")].d += t("e()", 5)
print("a.b[c()].d += e()")
for line in log:
    print("  ", line)
log.clear()

# Plain subscript augmented assignment evaluates container and key once
box = Box("box", {0: 10})
t("container", box)[t("key", 0)] += t("rhs", 1)
print("subscript aug", log, box.data)
log.clear()

# Normal assignment: RHS first, then targets left to right
box2 = Box("x", {})
t("target-obj1", box2)[t("key1", 1)] = t("target-obj2", box2)[t("key2", 2)] = t("rhs", "V")
print("chained assignment order", log)
log.clear()

# Dict display: key then value, left to right
d = {t("k1", 1): t("v1", "a"), t("k2", 2): t("v2", "b")}
print("dict display order", log)
log.clear()

# Call: function expression, then args left to right, then kwargs
def fn(*a, **k):
    log.append("call body")


t("func", fn)(t("arg1", 1), *t("star", [2]), kw=t("kw", 3), **t("dstar", {}))
print("call order", log)
log.clear()

# Binary operators: left operand fully before right
r = t("L", 2) ** t("R", 3) + t("X", 1) * t("Y", 4)
print("binary order", log, r)
log.clear()

# Conditional expression evaluates condition first
r = t("then", 1) if t("cond", False) else t("else", 2)
print("ternary order", log, r)
log.clear()

# Slices: the container, then start, stop, step left to right
xs = list(range(10))
t("list", xs)[t("start", 1):t("stop", 8):t("step", 2)]
print("slice order", log)
log.clear()

# Augmented assignment on a slice
box3 = Box("s", list(range(5)))
t("obj", box3)[t("lo", 1):t("hi", 3)] += t("ext", [9])
print("slice aug", log, box3.data)
log.clear()

# Augmented assignment on a global via a function that changes it in between
counter = 10


def bump():
    global counter
    counter = 1000
    return 1


counter += bump()
print("aug global read before rhs", counter)

# Attribute aug where getter result object is the same
class Holder:
    pass


h = Holder()
h.items = [1]
alias = h.items
h.items += [2]
print("list iadd keeps identity", alias is h.items, alias)
h.tup = (1,)
alias_t = h.tup
h.tup += (2,)
print("tuple iadd rebinds", alias_t is h.tup, alias_t, h.tup)

# Comparison chain evaluates middle operand once
r = t("p", 1) < t("q", 2) < t("r", 3)
print("chain middle once", log, r)
log.clear()

# Assignment expression order in a tuple
pair = (t("first", 1), (w := t("walrus", 2)), t("third", w + 1))
print("tuple display order", log, pair)
log.clear()

# Simple subscript assignment: value, then container and key
dd = {}
dd[t("key", "k")] = t("value", "v")
print("subscript assign: value before key", log)
