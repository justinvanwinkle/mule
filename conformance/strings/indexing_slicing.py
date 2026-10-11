"""String indexing and slicing with negative, out-of-range, and stepped bounds."""

s = "abcdefghij"
print("index", s[0], s[9], s[-1], s[-10], s[3])
for i in [10, -11, 100]:
    try:
        print("index", i, s[i])
    except IndexError as e:
        print("index", i, type(e).__name__)

starts = [None, 0, 3, -3, 12, -12]
stops = [None, 0, 7, -2, 15, -15]
steps = [None, 1, 2, 3, -1, -2, -4]
for step in steps:
    rows = []
    for a in starts:
        rows.append(" ".join(repr(s[a:b:step]) for b in stops))
    print("step", step)
    for a, row in zip(starts, rows):
        print("  start", a, row)

try:
    s[::0]
except ValueError as e:
    print("zero step", type(e).__name__)
print("slice object", s[slice(2, 8, 3)], s[slice(None, None, -3)], s[slice(-4, None)])
print("slice indices", slice(-3, None, None).indices(len(s)), slice(None, None, -1).indices(len(s)), slice(100, -100, -2).indices(10))
print("reverse", s[::-1], "".join(reversed(s)), s[-1::-1] == s[::-1])
print("big bounds", s[2**70:], s[-(2**70):3], s[:2**64:2**63])
print("empty string slices", ""[:], ""[::-1], ""[5:10], repr(""[0:0]))
print("slice copy identity-free", s[:] == s, s[0:len(s)])


class Idx:
    def __index__(self):
        return 2


print("__index__ in slice", s[Idx()], s[Idx():6], s[::Idx()])
try:
    s[1.0]
except TypeError as e:
    print("float index", type(e).__name__)
try:
    s[1:2.0]
except TypeError as e:
    print("float slice", type(e).__name__)
try:
    s[0] = "z"
except TypeError as e:
    print("str assign", type(e).__name__)

u = "a😀é\u0301z"
print("unicode slicing", repr(u[1]), repr(u[2:4]), repr(u[::-1]), len(u))
print("index in loop", [s[i] + s[-i - 1] for i in range(len(s) // 2)])
print("nested slice", s[2:9][1:-1][::2])
