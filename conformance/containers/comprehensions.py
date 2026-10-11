"""List, set, dict, and generator comprehensions with multiple for/if clauses and nesting."""

print("list", [n * n for n in range(6)])
print("filter", [n for n in range(20) if n % 2 if n % 3])
print("multi for", [(i, j) for i in range(3) for j in range(i)])
print("for if for if", [(i, j) for i in range(4) if i for j in "ab" if j != "b" or i == 3])
print("dependent", [c for word in ["hi", "yo"] for c in word])
print("conditional expr", ["even" if n % 2 == 0 else "odd" for n in range(4)])
print("cond in filter", [n for n in range(10) if (n > 5 if n % 2 else n < 3)])
print("nested comp", [[i * j for j in range(1, 4)] for i in range(1, 4)])
print("comp in iterable", [x + 1 for x in [y * 2 for y in range(3)]])
print("comp in condition", [x for x in range(5) if x in [y for y in range(x, 3)]])

print("set", sorted({n % 3 for n in range(10)}))
print("set strs", sorted({w.lower() for w in ["A", "a", "B"]}))
print("dict", {n: n * n for n in range(4)})
print("dict dup keys last wins", {n % 2: n for n in range(5)})
print("dict swap", {v: k for k, v in {"a": 1, "b": 2}.items()})
print("dict nested", {i: {j: i * j for j in range(2)} for i in range(2)})
print("dict cond value", {k: (v if v else None) for k, v in [("a", 0), ("b", 2)]})

g = (n * 2 for n in range(4))
print("gen type", type(g).__name__)
print("gen consume", list(g), list(g))
print("gen as arg", sum(n for n in range(5)))
print("gen with kw", sorted((n for n in range(3)), reverse=True))
print("gen nested", list(list(c for c in w) for w in ["ab", "c"]))

order = []


def src(name, items):
    order.append(name)
    return items


lazy = (x for x in src("outer", [1, 2]) for y in src("inner", [0]))
print("first iterable eager", order)
list(lazy)
print("after consume", order)

try:
    bad = (x for x in 5)
except TypeError as e:
    print("first iterable error at creation", type(e).__name__)

bad2 = (x for y in [1] for x in 5)
try:
    next(bad2)
except TypeError as e:
    print("inner iterable error at iteration", type(e).__name__)

matrix = [[1, 2, 3], [4, 5, 6]]
print("flatten", [v for row in matrix for v in row])
print("transpose", [[row[i] for row in matrix] for i in range(3)])
print("tuple of comps", tuple(x for x in "ab"), tuple([x for x in "ab"]))
print("empty", [x for x in []], {x: x for x in ()}, set(x for x in ""))
print("unpack target", [a + b for a, b in [(1, 2), (3, 4)]])
print("star target", [rest for _, *rest in ["abc", "de"]])
print("nested unpack target", [c for (a, (b, c)) in [(1, (2, 3))]])
print("lambda capture late", [f() for f in [lambda: i for i in range(3)]])
print("lambda capture default", [f() for f in [lambda i=i: i for i in range(3)]])
