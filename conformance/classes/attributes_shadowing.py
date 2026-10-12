"""Class and instance attributes: lookup, shadowing, deletion, and shared mutable class state."""


class Counter:
    count = 0
    tags = []  # deliberately shared across all instances
    label = "class-label"

    def __init__(self, name):
        self.name = name

    def bump(self):
        # Reads the class attribute, then creates an instance attribute.
        self.count += 1
        return self.count

    def tag(self, t):
        # Mutates the shared class-level list in place.
        self.tags.append(t)


a = Counter("a")
b = Counter("b")
print("initial counts", a.count, b.count, Counter.count)
print("bump a", a.bump(), a.bump())
print("after bump", a.count, b.count, Counter.count)
print("a has own count", "count" in vars(a), "count" in vars(b))

Counter.count = 10
print("class reassigned", a.count, b.count, Counter.count)

del a.count
print("after del instance attr", a.count)

a.tag("x")
b.tag("y")
print("shared tags", a.tags, b.tags, Counter.tags, a.tags is b.tags)

a.tags = ["own"]
a.tag("z")
print("after rebinding a.tags", a.tags, b.tags, Counter.tags)

print("label lookup", a.label, b.label)
b.label = "instance-label"
print("label shadowed", a.label, b.label, Counter.label)
del b.label
print("label unshadowed", b.label)

try:
    del b.label
except AttributeError as e:
    print("del missing instance attr", type(e).__name__)

try:
    print(a.nonexistent)
except AttributeError as e:
    print("missing attr", type(e).__name__)

# Class attribute that refers to another class attribute at class-body time.
class Config:
    base = 2
    doubled = base * 2
    # The outermost iterable is evaluated in class scope, so `base` works there.
    firsts = [i for i in range(base)]


print("config", Config.base, Config.doubled, Config.firsts)
try:
    class BadConfig:
        base = 2
        items = [base * i for i in range(3)]  # class scope is invisible to the body
except NameError as e:
    print("comprehension body class scope", type(e).__name__)


# Functions stored as class attributes become methods; stored on instances they do not.
def free(*args):
    return len(args)


class Holder:
    method = free


h = Holder()
h.attr_func = free
print("bound via class", h.method(), "unbound via instance", h.attr_func())
print("class access", Holder.method(), Holder.method(1, 2))

# Augmented assignment on class attribute through the class.
Holder.n = 1
Holder.n += 5
print("holder n", Holder.n, h.n)

# vars() / __dict__ contents
class P:
    x = 1

    def __init__(self):
        self.y = 2
        self.z = 3


p = P()
print("instance dict", sorted(vars(p).items()))
print("class dict has x", "x" in P.__dict__, "y" in P.__dict__)
p.__dict__["w"] = 4
print("via __dict__", p.w)
p.dyn = 5
print("getattr default", p.dyn, getattr(p, "nope", "default"))
print("hasattr", hasattr(p, "x"), hasattr(p, "nope"))
delattr(p, "dyn")
print("after delattr", hasattr(p, "dyn"))
