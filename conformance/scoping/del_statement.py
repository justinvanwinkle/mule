"""del of names, attributes, subscripts, slices, tuples of targets, and errors."""

a = 1
b = 2
c = 3
del a
try:
    print(a)
except NameError as e:
    print("deleted global", type(e).__name__)

del b, c
print("tuple del", "b" in globals(), "c" in globals())

try:
    del a
except NameError as e:
    print("double del", type(e).__name__)


def local_del():
    x = 1
    del x
    try:
        return x
    except UnboundLocalError as e:
        return type(e).__name__


print("local del", local_del())


def del_then_rebind():
    x = 1
    del x
    x = 2
    return x


print("del then rebind", del_then_rebind())


class Obj:
    cls_attr = "class"

    def __init__(self):
        self.p = 1
        self.q = 2

    def __repr__(self):
        return f"Obj({sorted(vars(self).items())})"


o = Obj()
del o.p
print("del attribute", o, hasattr(o, "p"))
try:
    del o.p
except AttributeError as e:
    print("del missing attribute", type(e).__name__)
try:
    del o.cls_attr
except AttributeError as e:
    print("del class attribute through instance", type(e).__name__)
del Obj.cls_attr
print("del class attribute", hasattr(Obj, "cls_attr"))


class Tracking:
    def __init__(self):
        self.log = []

    def __delattr__(self, name):
        self.log.append(("delattr", name))

    def __delitem__(self, key):
        self.log.append(("delitem", key))


t = Tracking()
del t.anything
del t[1]
del t[1:3]
del t[::2]
del t[1, 2]
del t[...]
print("custom delattr/delitem", t.log)

xs = list(range(10))
del xs[0]
print("del index", xs)
del xs[-1]
print("del negative index", xs)
del xs[2:4]
print("del slice", xs)
del xs[::2]
print("del extended slice", xs)
del xs[:]
print("del all", xs)
try:
    del xs[0]
except IndexError as e:
    print("del out of range", type(e).__name__)

d = {"a": 1, "b": 2, "c": 3}
del d["a"]
print("del dict key", d)
try:
    del d["zz"]
except KeyError as e:
    print("del missing key", type(e).__name__)

# tuple/list target forms
m, n, p = 1, 2, 3
del (m, n), [p]
print("del nested targets", [k in globals() for k in ("m", "n", "p")])

# del of mixed targets evaluated left to right
arr = [0, 1, 2, 3]
holder = {"k": 1}
del arr[0], holder["k"], arr[0]
print("mixed del left to right", arr, holder)

# del in a loop target position
nums = [1, 2, 3]
for i in range(len(nums)):
    pass
del i
print("loop var deleted", "i" in globals())

# del of immutable containers fails
try:
    tup = (1, 2)
    del tup[0]
except TypeError as e:
    print("del tuple item", type(e).__name__)
try:
    s = "abc"
    del s[0]
except TypeError as e:
    print("del str item", type(e).__name__)

# del of a closure variable from nested function requires nonlocal
def outer():
    v = "value"

    def remove():
        nonlocal v
        del v

    remove()
    try:
        return v
    except NameError as e:
        return type(e).__name__


print("nonlocal del", outer())

# del through global declaration
gv = "exists"


def remove_global():
    global gv
    del gv


remove_global()
print("global del", "gv" in globals())

# del bytearray slices and dict view stability
ba = bytearray(b"hello")
del ba[1:3]
print("bytearray del", bytes(ba))

# del with __getitem__-only object fails
class ReadOnly:
    def __getitem__(self, k):
        return k


try:
    del ReadOnly()[0]
except (TypeError, AttributeError) as e:
    print("del unsupported", type(e).__name__)
