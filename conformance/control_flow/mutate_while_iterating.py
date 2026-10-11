"""Specified behaviour of mutating sequences and dicts while iterating over them."""

# Removing from a list while iterating skips the element after each removal.
xs = [1, 2, 3, 4, 5, 6]
for x in xs:
    if x % 2 == 0:
        xs.remove(x)
print("remove evens in loop", xs)

xs = [2, 2, 2, 3]
for x in xs:
    if x == 2:
        xs.remove(x)
print("remove repeated", xs)

# Appending while iterating extends the iteration.
xs = [1]
for x in xs:
    if x < 5:
        xs.append(x + 1)
print("append during loop", xs)

# Inserting at front makes the iterator see the same element again.
xs = ["a", "b"]
seen = []
for x in xs:
    seen.append(x)
    if len(xs) < 4:
        xs.insert(0, "new")
print("insert front", seen, xs)

# Clearing the list stops iteration.
xs = [1, 2, 3]
seen = []
for x in xs:
    seen.append(x)
    xs.clear()
print("clear during loop", seen, xs)

# Replacing elements ahead is visible.
xs = [1, 2, 3]
seen = []
for i, x in enumerate(xs):
    seen.append(x)
    if i + 1 < len(xs):
        xs[i + 1] *= 10
print("modify ahead", seen)

# Rebinding the name does not affect the running iterator.
xs = [1, 2, 3]
seen = []
for x in xs:
    seen.append(x)
    xs = [99]
print("rebind name", seen, xs)

# Iterating over a copy is safe.
xs = [1, 2, 3, 4]
for x in xs[:]:
    if x % 2:
        xs.remove(x)
print("copy iteration", xs)

# Reverse index deletion pattern
xs = list(range(10))
for i in range(len(xs) - 1, -1, -1):
    if xs[i] % 3 == 0:
        del xs[i]
print("reverse delete", xs)

# dict size change during iteration raises RuntimeError
d = {"a": 1, "b": 2}
try:
    for k in d:
        d[k + k] = 0
except RuntimeError as e:
    print("dict grow", type(e).__name__)

d = {"a": 1, "b": 2}
try:
    for k in d:
        del d[k]
except RuntimeError as e:
    print("dict shrink", type(e).__name__, sorted(d))

# Value update without size change is allowed.
d = {"a": 1, "b": 2}
for k in d:
    d[k] *= 100
print("dict value update", d)

# set size change raises too
s = {1, 2, 3}
try:
    for v in s:
        s.add(v + 10)
except RuntimeError as e:
    print("set grow", type(e).__name__)

# bytearray and deque mutation
ba = bytearray(b"abc")
seen = []
for b in ba:
    seen.append(b)
    if len(ba) < 5:
        ba.append(ord("z"))
print("bytearray append", seen, bytes(ba))

import collections

dq = collections.deque([1, 2, 3])
try:
    for v in dq:
        dq.append(v)
except RuntimeError as e:
    print("deque mutate", type(e).__name__)

# list iterator after exhaustion stays exhausted even if list grows
xs = [1]
it = iter(xs)
print("first next", next(it))
print("exhausted", next(it, "done"))
xs.append(2)
print("still exhausted", next(it, "done"))

# list iterator not yet exhausted sees appended items
xs = [1, 2]
it = iter(xs)
next(it)
xs.append(3)
print("rest", list(it))
