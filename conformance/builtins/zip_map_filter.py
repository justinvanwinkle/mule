"""zip (uneven, strict, unzip), map with several iterables, filter with None and predicates."""

print("zip", list(zip([1, 2, 3], "ab")))
print("zip one", list(zip("ab")))
print("zip none", list(zip()))
print("zip three", list(zip(range(3), "xyz", [True, False, None])))
print("unzip", list(zip(*[(1, "a"), (2, "b")])))
print("zip strict ok", list(zip([1, 2], "ab", strict=True)))
for left, right in (([1, 2, 3], [1]), ([1], [1, 2]), ([], [0])):
    try:
        list(zip(left, right, strict=True))
    except ValueError as e:
        print("zip strict", len(left), len(right), type(e).__name__)
partial = []
try:
    for pair in zip("abc", [1, 2], strict=True):
        partial.append(pair)
except ValueError as e:
    print("zip strict yields before error", partial, type(e).__name__)

pulled = []


def tracked(name, n):
    for i in range(n):
        pulled.append(f"{name}{i}")
        yield i


list(zip(tracked("a", 3), tracked("b", 1)))
print("zip pull order", pulled)
it = iter(range(5))
print("zip same iterator", list(zip(it, it)))
print("zip type", type(zip()).__name__)
try:
    zip(1)
except TypeError as e:
    print("zip non iterable", type(e).__name__)

print("map one", list(map(str, [1, 2])))
print("map two", list(map(pow, [2, 3, 4], [5, 2])))
print("map three", list(map(lambda a, b, c: a + b + c, "ab", "cd", "ef")))
print("map lazy", type(map(abs, [])).__name__)
map_calls = []
m = map(lambda v: map_calls.append(v) or v, [1, 2, 3])
print("map not called yet", map_calls)
print("map next", next(m), map_calls)
print("map strict ok", list(map(max, [1, 2], [3, 0], strict=True)))
try:
    list(map(max, [1, 2], [3], strict=True))
except ValueError as e:
    print("map strict error", type(e).__name__)
try:
    list(map(len, [1]))
except TypeError as e:
    print("map func error", type(e).__name__)

print("filter none", list(filter(None, [0, 1, "", "a", None, [], [0], False, 0.0, 2])))
print("filter pred", list(filter(lambda v: v % 3 == 0, range(10))))
print("filter bool", list(filter(bool, ["", "x"])))
print("filter str method", list(filter(str.isdigit, ["1", "a", "22"])))
print("filter type", type(filter(None, [])).__name__)


class Weird:
    def __init__(self, truth):
        self.truth = truth

    def __bool__(self):
        return self.truth

    def __repr__(self):
        return f"Weird({self.truth})"


print("filter custom bool", list(filter(None, [Weird(True), Weird(False)])))
print("chain", list(map(lambda p: p[0] * p[1], filter(lambda p: p[0] > 1, zip(range(5), range(5, 10))))))
