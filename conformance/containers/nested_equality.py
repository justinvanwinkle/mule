"""Nested containers, deep equality, identity shortcut in comparison, and mixed numeric equality."""

a = {"k": [1, (2, 3), {"n": {4, 5}}]}
b = {"k": [1, (2, 3), {"n": {5, 4}}]}
print("deep eq", a == b, a is b)
b["k"][2]["n"].add(6)
print("deep ne after change", a == b)
print("numeric across nesting", [1, (2.0, [3])] == [1.0, (2, [3.0])])
print("True as 1 nested", [[True]] == [[1]])
print("list vs tuple nested", [(1, 2)] == [[1, 2]])
print("dict vs list", {} == [], [] == ())
print("frozenset vs set nested", [frozenset({1})] == [{1}])

nan = float("nan")
lst = [nan]
print("nan identity shortcut", lst == lst, [nan] == [nan], [float("nan")] == [float("nan")])
print("nan in tuple", (nan,) == (nan,))
print("nan dict value", {"a": nan} == {"a": nan})


class Never:
    def __eq__(self, other):
        return False

    def __repr__(self):
        return "Never()"


n = Never()
print("identity before eq", [n] == [n], n == n, n in [n])
print("distinct never", [Never()] == [Never()])

calls = []


class Loud:
    def __init__(self, v):
        self.v = v

    def __eq__(self, other):
        calls.append(self.v)
        return isinstance(other, Loud) and self.v == other.v

    __hash__ = None

    def __repr__(self):
        return f"Loud({self.v})"


print("stop at first diff", [Loud(1), Loud(2), Loud(3)] == [Loud(1), Loud(9), Loud(3)])
print("calls", calls)
calls.clear()
print("length diff no eq", [Loud(1)] == [Loud(1), Loud(2)], calls)

print("ordering nested", [[1, 2], [3]] < [[1, 2], [4]], [[1]] < [[1], []])
print("dict no ordering")
try:
    {} < {}
except TypeError as e:
    print("dict lt", type(e).__name__)

tree = {"name": "root", "kids": [{"name": "a", "kids": []}, {"name": "b", "kids": [{"name": "c", "kids": []}]}]}


def depth(node):
    return 1 + max((depth(k) for k in node["kids"]), default=0)


def names(node):
    yield node["name"]
    for k in node["kids"]:
        yield from names(k)


print("tree depth", depth(tree))
print("tree names", list(names(tree)))
tree["kids"][1]["kids"][0]["kids"].append({"name": "d", "kids": []})
print("deep mutation", depth(tree), tree["kids"][1])
matrix = [[r * 3 + c for c in range(3)] for r in range(3)]
print("matrix", matrix)
print("transpose", [list(col) for col in zip(*matrix)])
print("flatten", [x for row in matrix for x in row])
print("nested index", matrix[1][2], matrix[-1][-1], matrix[::2][1][::-1])
