"""copy: copy vs deepcopy with shared references and cycles, memo behavior, __copy__/__deepcopy__ hooks, copy.replace, immutables."""
import copy
from collections import namedtuple
from dataclasses import dataclass

inner = [1, 2]
outer = [inner, inner, {"k": inner}]
shallow = copy.copy(outer)
deep = copy.deepcopy(outer)
print("shallow new list", shallow is not outer, shallow[0] is inner)
print("deep new inner", deep[0] is not inner)
print("deep preserves sharing", deep[0] is deep[1], deep[2]["k"] is deep[0])
inner.append(3)
print("after mutate", shallow[0], deep[0])

cyc = [1]
cyc.append(cyc)
dc = copy.deepcopy(cyc)
print("cycle", dc[1] is dc, dc is not cyc, dc[0])
d = {"self": None, "n": 1}
d["self"] = d
dd = copy.deepcopy(d)
print("dict cycle", dd["self"] is dd, dd is not d)

t = (1, 2, "s")
print("immutable tuple copy identity", copy.copy(t) is t, copy.deepcopy(t) is t)
tm = (1, [2])
tmd = copy.deepcopy(tm)
print("tuple with mutable deep", tmd is not tm, tmd[1] is not tm[1], tmd == tm)
print("str/int/None identity", copy.deepcopy("abc") == "abc", copy.copy(None) is None)
fs = frozenset({1, 2})
print("frozenset", copy.copy(fs) is fs)
s = {1, 2}
print("set copy", copy.copy(s) is not s, copy.copy(s) == s)
src_d = {"a": [1]}
print("dict copy shares values", copy.copy(src_d)["a"] is src_d["a"], copy.deepcopy(src_d)["a"] is src_d["a"])


def f():
    return 1


print("functions/classes identity", copy.deepcopy(f) is f, copy.copy(int) is int)


class Node:
    def __init__(self, val, children=None):
        self.val = val
        self.children = children or []

    def __repr__(self):
        return f"Node({self.val}, {self.children})"


leaf = Node("leaf")
root = Node("root", [leaf, Node("mid", [leaf])])
rc = copy.copy(root)
rd = copy.deepcopy(root)
print("obj shallow shares children", rc.children is root.children)
print("obj deep", rd, rd.children[0] is rd.children[1].children[0], rd.children[0] is not leaf)


class Tracked:
    def __init__(self, items, tag):
        self.items = items
        self.tag = tag
        self.history = []

    def __copy__(self):
        new = Tracked(self.items, self.tag + "-copy")
        new.history = ["copied"]
        return new

    def __deepcopy__(self, memo):
        new = Tracked(copy.deepcopy(self.items, memo), self.tag + "-deep")
        memo[id(self)] = new
        new.history = ["deep-copied"]
        return new

    def __repr__(self):
        return f"Tracked({self.items}, {self.tag!r}, {self.history})"


tr = Tracked([1, [2]], "orig")
tc = copy.copy(tr)
td = copy.deepcopy(tr)
print("__copy__", tc, tc.items is tr.items)
print("__deepcopy__", td, td.items is not tr.items)
pair = [tr, tr]
pd = copy.deepcopy(pair)
print("hook memo sharing", pd[0] is pd[1], pd[0].tag)


class Slotted:
    __slots__ = ("a", "b")

    def __init__(self, a, b):
        self.a, self.b = a, b


sl = Slotted([1], 2)
slc = copy.deepcopy(sl)
print("slots deepcopy", slc.a, slc.b, slc.a is not sl.a)


class WithReduce:
    def __init__(self, x):
        self.x = x
        self.calls = 0

    def __reduce__(self):
        return (WithReduce, (self.x * 10,))


wr = copy.copy(WithReduce(4))
print("__reduce__ copy", wr.x, wr.calls)


class Settings:
    def __init__(self, data):
        self.data = data

    def __getstate__(self):
        return {"data": self.data, "note": "from getstate"}

    def __setstate__(self, state):
        self.__dict__.update(state)


st = copy.deepcopy(Settings({"a": 1}))
print("getstate/setstate", st.data, st.note)
@dataclass(frozen=True)
class Pt:
    x: int
    y: int


print("copy.replace dataclass", copy.replace(Pt(1, 2), y=5))
NT = namedtuple("NT", "a b")
print("copy.replace namedtuple", copy.replace(NT(1, 2), a=9))
lst_copy = [[0] * 2] * 2
lst_copy[0][0] = 1
print("aliasing via *", lst_copy)
dl = copy.deepcopy(lst_copy)
print("deepcopy keeps row alias", dl[0] is dl[1], dl[0] is not lst_copy[0])
memo = {}
shared = [1]
first = copy.deepcopy([shared], memo)
second = copy.deepcopy([shared], memo)
print("explicit memo reuse", first[0] is second[0], first[0] is not shared)
