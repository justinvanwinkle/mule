"""list.sort and sorted with key, reverse, stability, and comparison errors."""

xs = [5, 2, 9, 1, 5, 6]
r = xs.sort()
print("sort returns", r, xs)
xs.sort(reverse=True)
print("sort reverse", xs)

words = ["banana", "Apple", "cherry", "apple", "Banana", "fig"]
print("default", sorted(words))
print("key lower", sorted(words, key=str.lower))
print("key len", sorted(words, key=len))
print("key len reverse", sorted(words, key=len, reverse=True))

pairs = [("b", 2), ("a", 2), ("c", 1), ("d", 1), ("e", 3), ("f", 2)]
print("stable by second", sorted(pairs, key=lambda p: p[1]))
print("stable reverse keeps order", sorted(pairs, key=lambda p: p[1], reverse=True))

records = [("x", 3, 1), ("y", 1, 2), ("z", 3, 0), ("w", 1, 1)]
step1 = sorted(records, key=lambda r: r[2])
step2 = sorted(step1, key=lambda r: r[1])
print("multi pass", step2)
print("tuple key", sorted(records, key=lambda r: (r[1], -r[2])))

calls = []


def key(v):
    calls.append(v)
    return -v


data = [3, 1, 2]
data.sort(key=key)
print("key once each", data, sorted(calls))

print("mixed numeric", sorted([3, 1.5, True, 0, -2.25, 2]))
print("bools", sorted([True, False, True, False]))
print("tuples lexicographic", sorted([(1, 2), (1,), (0, 9, 9), (1, 1, 1), ()]))
print("lists", sorted([[2], [1, 5], [1], []]))
print("strings by code point", sorted(["b", "B", "a", "_", "1", "é", "Z"]))

try:
    sorted([1, "a", 2])
except TypeError as e:
    print("mixed types", type(e).__name__)

try:
    [3, 1].sort(lambda v: v)
except TypeError as e:
    print("positional key", type(e).__name__)


class Box:
    def __init__(self, v):
        self.v = v

    def __lt__(self, other):
        return self.v < other.v

    def __repr__(self):
        return f"Box({self.v})"


print("custom lt", sorted([Box(3), Box(1), Box(2)]))
print("custom reverse", sorted([Box(3), Box(1), Box(2)], reverse=True))

lst = [3, 2, 1]
try:
    def bad_key(v):
        lst.append(v)
        return v
    lst.sort(key=bad_key)
except ValueError as e:
    print("mutated during sort", type(e).__name__)

print("sorted dict", sorted({"b": 1, "a": 2}))
print("sorted string", sorted("hello"))
print("sorted none key", sorted([2, 1], key=None))
print("empty", sorted([]), sorted([], reverse=True))
