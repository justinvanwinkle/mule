"""Mutating lists, dicts, sets and deques while iterating them, in the ways whose results are specified."""

from collections import deque

# Appending to a list while iterating it: the iterator sees new items; bound the loop with a break.
xs = [1, 2, 3]
for x in xs:
    if len(xs) < 8:
        xs.append(x * 10)
    if x > 100:
        break
print("append during iteration", xs)

# Removing while iterating forwards skips the element after each removal.
ys = [1, 2, 2, 3, 4, 4, 5]
for y in ys:
    if y % 2 == 0:
        ys.remove(y)
print("forward remove skips", ys)

# Deleting by index while iterating backwards is safe.
zs = list(range(12))
for i in range(len(zs) - 1, -1, -1):
    if zs[i] % 3 == 0:
        del zs[i]
print("backward delete", zs)

# Iterating a copy while mutating the original
ws = [1, 2, 3, 4]
for w in ws[:]:
    if w % 2:
        ws.remove(w)
print("copy iteration", ws)

# Shrinking a list: the iterator stops early when its index passes the end.
vs = list(range(6))
seen = []
for v in vs:
    seen.append(v)
    del vs[0]
print("shrink while iterating", seen, vs)

# Slice assignment replacing the list contents mid-iteration
us = [0, 1, 2]
out = []
for u in us:
    out.append(u)
    if u == 0:
        us[:] = ["a", "b", "c", "d"]
print("slice replace mid iteration", out)

# dict size change raises RuntimeError
d = {"a": 1, "b": 2}
try:
    for k in d:
        d[k + k] = 0
except RuntimeError as e:
    print("dict size change", type(e).__name__)
print("dict after", sorted(d))

# Changing values (not keys) during iteration is fine
d2 = {"a": 1, "b": 2, "c": 3}
for k in d2:
    d2[k] *= 10
print("value update ok", d2)

# Delete and re-add keeping the size constant: CPython detects the key change
d3 = {1: "x", 2: "y"}
try:
    for k in d3:
        del d3[k]
        d3[k + 10] = "z"
except RuntimeError as e:
    print("dict keys changed", type(e).__name__)

# Iterating over list(d) to delete safely
d4 = {i: i * i for i in range(6)}
for k in list(d4):
    if k % 2:
        del d4[k]
print("delete via list copy", d4)

# set size change
s = {1, 2, 3}
try:
    for item in s:
        s.add(item + 100)
except RuntimeError as e:
    print("set size change", type(e).__name__)

# deque mutation during iteration
dq = deque([1, 2, 3])
try:
    for item in dq:
        dq.append(item)
except RuntimeError as e:
    print("deque mutated", type(e).__name__)

# dict views reflect later mutation
d5 = {"x": 1}
keys = d5.keys()
items = d5.items()
d5["y"] = 2
print("live views", sorted(keys), sorted(items))

# Iterator created then list extended before first next()
src = [1, 2]
it = iter(src)
src.extend([3, 4])
print("iterator sees extension", list(it))
# Exhausted list iterator stays exhausted even after append
src2 = [1]
it2 = iter(src2)
print("drain", list(it2))
src2.append(2)
print("after append", list(it2))

# enumerate + insert at front
es = ["a", "b", "c"]
log = []
for i, e in enumerate(es):
    log.append((i, e))
    if i == 0:
        es.insert(0, "Z")
    if i > 4:
        break
print("insert at front", log)

# Generator over a list being mutated by the consumer
nums = [1, 2, 3]
gen = (n * 2 for n in nums)
first = next(gen)
nums.append(4)
nums[1] = 20
print("genexpr over mutated list", first, list(gen))

# Comprehension's first iterable is evaluated once
base = [1, 2, 3]
res = [base.append(b) or b for b in base if len(base) < 6]
print("comprehension over growing list", res, base)
