"""Recursive containers: repr shows [...] and {...}, and equality/length on self-referencing structures."""

a = [1, 2]
a.append(a)
print("self list", a)
print("str self list", str(a))
print("len", len(a), a[2] is a, a[2][2][2] is a)

d = {"k": 1}
d["self"] = d
print("self dict", d)

b = [1]
c = [b]
b.append(c)
print("mutual lists", b, c)

e = {}
f = [e]
e["list"] = f
print("dict in list", e, f)

t = ([],)
t[0].append(t)
print("tuple through list", t)

g = {"a": []}
g["a"].append(g)
g["b"] = g["a"]
print("shared and recursive", g)


class Node:
    def __init__(self):
        self.children = []

    def __repr__(self):
        return f"Node({self.children!r})"


n = Node()
n.children.append([1, 2])
print("custom repr ok", n)

x = [1]
y = [x, x]
print("shared not recursive", y)

z = []
z.append(z)
z.append(z)
print("double self", z)
print("contains self", z in z)
print("compare by identity", z == [z, z])
print("identity compare", z == z)
