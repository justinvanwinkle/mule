"""Sequence repetition aliasing: [[]]*3, nested multiplication, and *= in place."""

grid = [[]] * 3
grid[0].append(1)
print("shared inner", grid)
print("same object", grid[0] is grid[1] is grid[2])

proper = [[] for _ in range(3)]
proper[0].append(1)
print("comprehension distinct", proper)

row = [0] * 3
board = [row] * 2
board[0][0] = 9
print("board aliased", board, row)

board2 = [[0] * 3 for _ in range(2)]
board2[0][0] = 9
print("board distinct", board2)

ints = [0] * 4
ints[0] = 5
print("immutable elements fine", ints)

print("zero repeat", [1, 2] * 0, [1] * -3, "ab" * 0)
print("rmul", 3 * [1, 2], 2 * (0,), 2 * "xy")
print("bool repeat", [1] * True, [1] * False)

xs = [1, 2]
ys = xs
xs *= 2
print("imul in place", ys, xs is ys)
t = (1, 2)
u = t
t *= 2
print("tuple imul rebinds", u, t)

nested = [[1, 2]] * 2
nested[1] = [3]
print("rebind element breaks alias", nested)
nested2 = [[1, 2]] * 2
nested2[1] += [3]
print("iadd element shares", nested2)
nested3 = [[1, 2]] * 2
nested3[1] = nested3[1] + [3]
print("add rebinds element", nested3)

deep = [[[0]] * 2] * 2
deep[0][0][0] = 7
print("three level alias", deep)

d = dict.fromkeys(range(2), {})
d[0]["k"] = "v"
print("fromkeys dict alias", d)

try:
    [1] * 1.5
except TypeError as e:
    print("float repeat", type(e).__name__)
try:
    [1] * [2]
except TypeError as e:
    print("list times list", type(e).__name__)


class Three:
    def __index__(self):
        return 3


print("index repeat", [0] * Three(), "a" * Three())
