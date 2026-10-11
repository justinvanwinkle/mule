"""List slicing: reads, negative/out-of-range bounds, slice assignment, extended slices, del, slice objects."""

xs = list(range(10))
print("basic", xs[2:5], xs[:3], xs[7:], xs[:])
print("negative", xs[-3:], xs[:-7], xs[-5:-2])
print("step", xs[::2], xs[1::3], xs[::-1], xs[::-3])
print("neg step bounds", xs[7:2:-2], xs[2:7:-1], xs[-1:-4:-1])
print("out of range", xs[5:100], xs[100:], xs[-100:2], xs[8:2])
print("huge bounds", xs[-10**30:10**30:3])
print("slice copy is new", xs[:] is xs, xs[:] == xs)
try:
    xs[::0]
except ValueError as e:
    print("zero step", type(e).__name__)
try:
    xs[1.5:3]
except TypeError as e:
    print("float bound", type(e).__name__)


class Idx:
    def __index__(self):
        return 3


print("__index__ bounds", xs[Idx():], xs[:Idx()])
print("none bounds", xs[None:None:None] == xs, xs[None:3])
print("bool bounds", xs[True:False], xs[False:True])

a = list(range(6))
a[1:3] = ["x", "y", "z", "w"]
print("assign grow", a)
a[1:5] = []
print("assign shrink", a)
a[2:2] = [7, 8]
print("assign insert", a)
a[:0] = "ab"
print("assign prepend string", a)
a[len(a):] = (99,)
print("assign append tuple", a)
a[100:200] = [1000]
print("assign past end", a)
a[-2:] = range(3)
print("assign range", a)
a[5:2] = ["mid"]
print("assign reversed bounds inserts", a)
try:
    a[0:1] = 5
except TypeError as e:
    print("assign non iterable", type(e).__name__)

self_assign = [1, 2, 3]
self_assign[1:] = self_assign
print("assign self", self_assign)

b = list(range(8))
b[::2] = ["e0", "e1", "e2", "e3"]
print("extended assign", b)
b[::-3] = [None] * 3
print("extended reverse assign", b)
try:
    b[::2] = [1, 2]
except ValueError as e:
    print("extended length mismatch", type(e).__name__)
b[1:2:1] = ["step one any length", "ok"]
print("step 1 not extended", b)
b[::2] = []  if False else b[::2]
print("extended same", len(b))
empty_ext = [1, 2]
empty_ext[5:10:2] = []
print("empty extended assign", empty_ext)

c = list(range(10))
del c[2:4]
print("del slice", c)
del c[::2]
print("del step", c)
del c[::-2]
print("del neg step", c)
del c[100:]
print("del out of range", c)
del c[:]
print("del all", c)

s = slice(1, 8, 2)
print("slice object", s, s.start, s.stop, s.step)
xs2 = list("abcdefghij")
print("use slice obj", xs2[s])
print("indices", s.indices(5), slice(None, None, -1).indices(4), slice(-3, None).indices(10))
print("indices big", slice(-100, 100, 3).indices(7))
print("range from indices", list(range(*slice(None, None, -2).indices(7))))
print("slice eq", slice(1, 2) == slice(1, 2), slice(1, 2) == slice(1, 2, None), slice(1, 2) == slice(1, 3))
print("slice of string tuple", "abcdef"[s], tuple(range(9))[s])
try:
    slice(0, 1, 0).indices(5)
except ValueError as e:
    print("indices zero step", type(e).__name__)


class Spy:
    def __getitem__(self, key):
        return key


spy = Spy()
print("spy simple", spy[1:2])
print("spy ext", spy[::3], spy[1:2, 3], spy[..., 1:])
print("spy empty parts", spy[:])
