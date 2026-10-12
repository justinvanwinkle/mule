"""itertools combinatoric iterators: product with repeat, permutations, combinations, combinations_with_replacement."""
import itertools as it
from math import comb, factorial, perm

print("product", list(it.product("ab", [1, 2])))
print("product repeat", ["".join(p) for p in it.product("01", repeat=3)])
print("product repeat 2 iters", list(it.product("ab", "c", repeat=2)))
print("product empty iterable", list(it.product("ab", [])))
print("product no args", list(it.product()))
print("product repeat 0", list(it.product("ab", repeat=0)))
gen_calls = []


def gen():
    gen_calls.append("start")
    yield 1
    yield 2


p = it.product(gen(), "xy")
print("product eager consume", gen_calls)
print("product gen", list(p))

print("permutations", ["".join(p) for p in it.permutations("abc")])
print("permutations r", list(it.permutations(range(4), 2)))
print("permutations r>n", list(it.permutations("ab", 3)))
print("permutations r=0", list(it.permutations("ab", 0)))
print("permutations dupes", list(it.permutations("aab", 2)))
print("permutations count", sum(1 for _ in it.permutations(range(6), 3)), perm(6, 3))

print("combinations", list(it.combinations("abcd", 2)))
print("combinations r=n", list(it.combinations("abc", 3)))
print("combinations r>n", list(it.combinations("abc", 4)))
print("combinations order follows input", list(it.combinations([3, 1, 2], 2)))
print("combinations count", sum(1 for _ in it.combinations(range(10), 4)), comb(10, 4))

print("cwr", list(it.combinations_with_replacement("abc", 2)))
print("cwr r>n", len(list(it.combinations_with_replacement("ab", 4))))
print("cwr count", sum(1 for _ in it.combinations_with_replacement(range(5), 3)), comb(5 + 3 - 1, 3))

# Nested combinatorics: subsets of all sizes, sums.
items = [1, 2, 3, 4]
subsets = [c for r in range(len(items) + 1) for c in it.combinations(items, r)]
print("powerset size", len(subsets))
print("subsets summing to 5", [s for s in subsets if sum(s) == 5])
print("perm == fact", sum(1 for _ in it.permutations(range(5))) == factorial(5))

# Grid coordinates via product and filtering diagonals.
grid = [(x, y) for x, y in it.product(range(3), repeat=2) if x != y]
print("off-diagonal", grid)

try:
    it.permutations("abc", -1)
except ValueError as e:
    print("permutations negative", type(e).__name__)
try:
    it.combinations("abc", -1)
except ValueError as e:
    print("combinations negative", type(e).__name__)
try:
    it.product("ab", repeat=-1)
except ValueError as e:
    print("product negative", type(e).__name__)
