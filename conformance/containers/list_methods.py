"""List methods: append, extend, insert, remove, pop, index, count, clear, reverse, copy and their errors."""

xs = [3, 1, 4]
xs.append(1)
print("append", xs)
xs.append([5, 9])
print("append list", xs, len(xs))
xs.pop()
xs.extend([2, 6])
print("extend list", xs)
xs.extend((5, 3))
xs.extend(range(2))
xs.extend("ab")
print("extend mixed", xs)
gen = (n * n for n in range(3))
xs.extend(gen)
print("extend gen", xs)
ys = [1, 2]
ys.extend(ys)
print("extend self", ys)
ys += ys
print("iadd self", ys)

zs = [10, 20, 30]
zs.insert(0, "a")
zs.insert(2, "b")
zs.insert(-1, "c")
print("insert", zs)
zs.insert(100, "end")
zs.insert(-100, "start")
print("insert out of range", zs)
zs.insert(len(zs), "len")
print("insert at len", zs)

ws = [1, 2, 3, 2, 1]
ws.remove(2)
print("remove first match", ws)
try:
    ws.remove(99)
except ValueError as e:
    print("remove missing", type(e).__name__)
ws.remove(True)
print("remove True removes 1", ws)

ps = [1, 2, 3, 4, 5]
print("pop", ps.pop(), ps)
print("pop 0", ps.pop(0), ps)
print("pop -2", ps.pop(-2), ps)
try:
    ps.pop(10)
except IndexError as e:
    print("pop out of range", type(e).__name__)
try:
    [].pop()
except IndexError as e:
    print("pop empty", type(e).__name__)

ix = ["a", "b", "c", "b", "a"]
print("index", ix.index("b"))
print("index start", ix.index("b", 2))
print("index start stop", ix.index("a", 1, 5))
print("index negative start", ix.index("a", -2))
try:
    ix.index("a", 1, 4)
except ValueError as e:
    print("index window miss", type(e).__name__)
try:
    ix.index("z")
except ValueError as e:
    print("index missing", type(e).__name__)

cs = [1, 1.0, True, 2, "1", [1], 1 + 0j]
print("count 1", cs.count(1))
print("count [1]", cs.count([1]))
print("count missing", cs.count(3))

rv = [1, 2, 3, 4]
result = rv.reverse()
print("reverse returns", result, rv)
cp = rv.copy()
cp.append(0)
print("copy independent", rv, cp)
nested = [[1], [2]]
shallow = nested.copy()
shallow[0].append(99)
print("copy shallow", nested)
rv.clear()
print("clear", rv, len(rv))

alias = [1, 2]
other = alias
alias.clear()
print("clear aliased", other)
alias2 = [1, 2]
other2 = alias2
alias2 = []
print("rebind not clear", other2)

nan = float("nan")
nl = [nan]
print("nan identity in", nan in nl, nl.count(nan), nl.index(nan))
print("fresh nan in", float("nan") in nl)

try:
    [].extend(5)
except TypeError as e:
    print("extend non iterable", type(e).__name__)
try:
    [1, 2].insert("x", 1)
except TypeError as e:
    print("insert bad index", type(e).__name__)
