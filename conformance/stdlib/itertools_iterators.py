"""itertools: count, cycle, repeat, accumulate, chain, compress, dropwhile/takewhile, filterfalse, groupby, islice, starmap, tee, zip_longest, pairwise, batched."""
import itertools as it
import operator

print("count", list(it.islice(it.count(), 5)))
print("count start step", list(it.islice(it.count(10, -3), 4)))
print("count float", list(it.islice(it.count(0.5, 0.25), 3)))
print("count zip", list(zip("abc", it.count(1))))
print("cycle", list(it.islice(it.cycle("ab"), 5)))
print("cycle empty", list(it.cycle([])))
print("repeat", list(it.repeat("x", 3)))
print("repeat zero", list(it.repeat("x", 0)), list(it.repeat("x", -1)))
print("repeat map", list(map(pow, range(4), it.repeat(2))))

print("accumulate", list(it.accumulate([1, 2, 3, 4])))
print("accumulate mul", list(it.accumulate([1, 2, 3, 4], operator.mul)))
print("accumulate max", list(it.accumulate([3, 1, 4, 1, 5, 9, 2], max)))
print("accumulate initial", list(it.accumulate([1, 2, 3], initial=100)))
print("accumulate empty initial", list(it.accumulate([], initial=7)))
print("accumulate strings", list(it.accumulate("abc")))
print("accumulate func+initial", list(it.accumulate([2, 3], lambda a, b: a * 10 + b, initial=1)))

print("chain", list(it.chain("ab", [1, 2], (), range(2))))
print("chain.from_iterable", list(it.chain.from_iterable(["xy", "z", ""])))
print("chain lazy", list(it.islice(it.chain.from_iterable(it.repeat([1, 2])), 5)))

print("compress", list(it.compress("abcdef", [1, 0, 1, 0, 1, 1])))
print("compress short selectors", list(it.compress("abcdef", [True, True])))
print("dropwhile", list(it.dropwhile(lambda x: x < 3, [1, 2, 3, 1, 4])))
print("takewhile", list(it.takewhile(lambda x: x < 3, [1, 2, 3, 1, 4])))
print("filterfalse", list(it.filterfalse(lambda x: x % 2, range(8))))
print("filterfalse None", list(it.filterfalse(None, [0, 1, "", "a", None, [], [0]])))

data = sorted(["apple", "avocado", "banana", "blueberry", "cherry", "apricot"])
for key, group in it.groupby(data, key=lambda s: s[0]):
    print("groupby", key, list(group))
print("groupby no key", [(k, len(list(g))) for k, g in it.groupby("aaabccdaa")])
groups = it.groupby("aabbb")
k1, g1 = next(groups)
k2, g2 = next(groups)
print("groupby advanced invalidates", k1, list(g1), k2, list(g2))

print("islice stop", list(it.islice(range(10), 3)))
print("islice start stop", list(it.islice(range(10), 2, 6)))
print("islice step", list(it.islice(range(20), 1, 15, 4)))
print("islice None", list(it.islice("abcdefg", None, None, 3)))
print("islice past end", list(it.islice("abc", 2, 100)))
try:
    it.islice("abc", -1)
except ValueError as e:
    print("islice negative", type(e).__name__)
src = iter(range(10))
print("islice consumes", list(it.islice(src, 2, 4)), next(src))

print("starmap", list(it.starmap(pow, [(2, 5), (3, 2), (10, 3)])))
print("starmap kw-less", list(it.starmap(lambda *a: sum(a), [(), (1,), (1, 2, 3)])))

a, b = it.tee(iter([1, 2, 3]))
print("tee a", next(a), next(a))
print("tee b", list(b))
print("tee a rest", list(a))
t1, t2, t3 = it.tee("xy", 3)
print("tee three", list(t1), list(t2), list(t3))

print("zip_longest", list(it.zip_longest("abc", [1])))
print("zip_longest fill", list(it.zip_longest("ab", "wxyz", fillvalue="-")))
print("zip_longest empty", list(it.zip_longest()))

print("pairwise", list(it.pairwise("abcd")))
print("pairwise short", list(it.pairwise("a")))
print("batched", list(it.batched(range(7), 3)))
print("batched exact", list(it.batched("abcdef", 2)))
print("batched strict ok", list(it.batched("abcd", 2, strict=True)))
try:
    list(it.batched("abcde", 2, strict=True))
except ValueError as e:
    print("batched strict", type(e).__name__)
try:
    it.batched("abc", 0)
except ValueError as e:
    print("batched zero", type(e).__name__)
print("batched tuples", type(next(it.batched([1], 1))).__name__)
