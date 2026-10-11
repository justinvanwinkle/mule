"""sorted, min, max with key, default, reverse, stability, multiple arguments, and empty errors."""

data = [("b", 2), ("a", 2), ("c", 1)]
print("sorted key stable", sorted(data, key=lambda p: p[1]))
print("sorted reverse stable", sorted(data, key=lambda p: p[1], reverse=True))
print("sorted returns new list", type(sorted((3, 1))).__name__, sorted("cab"))
print("sorted generator", sorted(x % 4 for x in range(8)))
src = [3, 1, 2]
out = sorted(src)
print("sorted leaves source", src, out)

print("min max", min([3, 1, 2]), max([3, 1, 2]))
print("min max args", min(3, 1, 2), max(3, 1, 2), max("apple", "pear"))
print("min key", min(["aaa", "b", "cc"], key=len), max(["aaa", "b", "cc"], key=len))
print("first of ties min", min([("x", 1), ("y", 1)], key=lambda p: p[1]))
print("first of ties max", max([("x", 1), ("y", 1)], key=lambda p: p[1]))
print("max of equal values keeps first", max([1, 1.0, True]), min([True, 1, 1.0]))
print("default", min([], default="none"), max((), default=0))
print("default unused", max([5], default=0))
print("key and default", min([], key=abs, default=-1))
print("negative key", max([-5, 3, -1], key=abs), min([-5, 3, -1], key=abs))
print("dict max key", max({"a": 3, "b": 9, "c": 1}.items(), key=lambda kv: kv[1]))
print("max of dict", max({"x": 0, "z": 0, "y": 0}))
print("min strings", min("hello"), max("hello"))
print("min tuples", min([(2, "a"), (1, "z"), (1, "b")]))
print("mixed numeric", max(1, 2.5, True), min(-1, -1.5, False))

for label, call in [
    ("min empty", lambda: min([])),
    ("max empty", lambda: max(iter(()))),
    ("min no args", lambda: min()),
    ("max default with args", lambda: max(1, 2, default=0)),
    ("min mixed types", lambda: min(1, "a")),
    ("max int", lambda: max(5)),
]:
    try:
        call()
        print(label, "no error")
    except (ValueError, TypeError) as e:
        print(label, type(e).__name__)

nan = float("nan")
print("nan order dependent", max([nan, 1, 2]), max([1, 2, nan]))
print("single arg iterable", max([7]), min("z"))
calls = []
max([3, 1, 2], key=lambda v: calls.append(v) or v)
print("key call order", calls)
