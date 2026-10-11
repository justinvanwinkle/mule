"""enumerate with start, reversed on lists/ranges/strings/dicts and custom __reversed__/sequence protocol."""

print("enumerate", list(enumerate("abc")))
print("enumerate start", list(enumerate("abc", start=1)))
print("enumerate negative start", list(enumerate("ab", -5)))
print("enumerate big start", list(enumerate("ab", 2**64)))
print("enumerate bool start", list(enumerate("a", True)))
e = enumerate(iter([10, 20]))
print("enumerate next", next(e), next(e), next(e, "end"))
for i, (k, v) in enumerate({"x": 1, "y": 2}.items(), 1):
    print("enumerate items", i, k, v)
try:
    enumerate([], start=1.5)
except TypeError as e:
    print("enumerate float start", type(e).__name__)

print("reversed list", list(reversed([1, 2, 3])))
print("reversed tuple", tuple(reversed((1, 2))))
print("reversed str", "".join(reversed("hello")))
print("reversed range", list(reversed(range(0, 10, 3))), list(reversed(range(5, 0, -2))))
print("reversed empty range", list(reversed(range(0))))
print("reversed dict", list(reversed({"a": 1, "b": 2})))
print("reversed type range", type(reversed(range(3))).__name__)
print("reversed type list", type(reversed([])).__name__)
lst = [1, 2, 3]
r = reversed(lst)
lst.append(4)
print("reversed sees length at creation", list(r))
lst2 = [1, 2, 3]
r2 = reversed(lst2)
lst2.pop()
print("reversed after shrink", list(r2))
try:
    reversed({1, 2})
except TypeError as e:
    print("reversed set", type(e).__name__)
try:
    reversed(x for x in [])
except TypeError as e:
    print("reversed gen", type(e).__name__)


class Custom:
    def __reversed__(self):
        return iter(["custom", "reversed"])


class SeqOnly:
    def __len__(self):
        return 3

    def __getitem__(self, i):
        return "abc"[i]


class NoReverse:
    __reversed__ = None

    def __len__(self):
        return 1

    def __getitem__(self, i):
        return i


print("custom reversed", list(reversed(Custom())))
print("sequence protocol", list(reversed(SeqOnly())))
try:
    reversed(NoReverse())
except TypeError as e:
    print("reversed None", type(e).__name__)
print("enumerate reversed", list(enumerate(reversed("ab"))))
print("reversed enumerate list", list(reversed(list(enumerate("ab")))))
try:
    reversed(enumerate("ab"))
except TypeError as e:
    print("reversed enumerate", type(e).__name__)
