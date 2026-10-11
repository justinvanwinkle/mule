"""heapq (heappush/heappop, heapify, heappushpop/heapreplace, nlargest/nsmallest with key, merge) and bisect (bisect_left/right, insort, key=)."""
import heapq
import bisect

h = []
for x in [5, 1, 8, 3, 9, 2]:
    heapq.heappush(h, x)
print("heap root", h[0], len(h))
print("heappop order", [heapq.heappop(h) for _ in range(len(h))])
data = [9, 4, 7, 1, 8, 2, 2]
heapq.heapify(data)
print("heapify invariant", all(data[i] <= data[c] for i in range(len(data)) for c in (2 * i + 1, 2 * i + 2) if c < len(data)))
print("heappushpop small", heapq.heappushpop(data, 0), data[0])
print("heapreplace", heapq.heapreplace(data, 100), data[0])
try:
    heapq.heappop([])
except IndexError as e:
    print("pop empty", type(e).__name__)

tasks = []
heapq.heappush(tasks, (2, "write"))
heapq.heappush(tasks, (1, "plan"))
heapq.heappush(tasks, (2, "test"))
heapq.heappush(tasks, (0, "wake"))
print("tuple priorities", [heapq.heappop(tasks) for _ in range(4)])

maxheap = [3, 1, 4]
heapq.heapify_max(maxheap)
for x in [1, 5]:
    heapq.heappush_max(maxheap, x)
print("max-heap", [heapq.heappop_max(maxheap) for _ in range(5)])
neg = []
for x in [3, 1, 4]:
    heapq.heappush(neg, -x)
print("negated max-heap", -heapq.heappop(neg))

nums = [7, -3, 10, 0, 10, -8, 4]
print("nlargest", heapq.nlargest(3, nums))
print("nsmallest", heapq.nsmallest(3, nums))
print("nlargest key abs", heapq.nlargest(2, nums, key=abs))
print("nsmallest key stable", heapq.nsmallest(3, ["bb", "a", "cc", "d", "eee"], key=len))
print("nlargest ties stable", heapq.nlargest(3, ["bb", "a", "cc", "d", "dd"], key=len))
print("n > len", heapq.nsmallest(10, [3, 1, 2]))
print("n zero", heapq.nlargest(0, nums))
people = [{"name": "ann", "age": 31}, {"name": "bo", "age": 25}, {"name": "cy", "age": 40}]
print("nsmallest dict key", [p["name"] for p in heapq.nsmallest(2, people, key=lambda p: p["age"])])

print("merge", list(heapq.merge([1, 4, 7], [2, 5, 8], [0, 3, 9])))
print("merge key", list(heapq.merge(["a", "ccc"], ["bb", "dddd"], key=len)))
print("merge reverse", list(heapq.merge([9, 5, 1], [8, 2], reverse=True)))
print("merge stable", list(heapq.merge([(1, "a"), (2, "a")], [(1, "b")], key=lambda t: t[0])))
print("merge empty", list(heapq.merge([], [1], [])))

xs = [1, 2, 4, 4, 4, 7, 9]
for v in [0, 4, 5, 10]:
    print("bisect", v, bisect.bisect_left(xs, v), bisect.bisect_right(xs, v), bisect.bisect(xs, v))
print("bisect lo/hi", bisect.bisect_left(xs, 4, lo=4), bisect.bisect_right(xs, 4, hi=3))
ys = []
for v in [5, 1, 4, 1, 3]:
    bisect.insort(ys, v)
print("insort", ys)
bisect.insort_left(ys, 4)
bisect.insort_right(ys, 4.0)
print("insort left/right", ys, [type(v).__name__ for v in ys])


def grade(score, breakpoints=(60, 70, 80, 90), grades="FDCBA"):
    return grades[bisect.bisect(breakpoints, score)]


print("grades", [grade(s) for s in [33, 60, 77, 89, 90, 100]])
records = [("ann", 3), ("bo", 5), ("cy", 8)]
print("bisect key", bisect.bisect_left(records, 5, key=lambda r: r[1]))
bisect.insort(records, ("dee", 6), key=lambda r: r[1])
print("insort key", records)
try:
    bisect.bisect_left(xs, 1, lo=-1)
except ValueError as e:
    print("bisect negative lo", type(e).__name__)
