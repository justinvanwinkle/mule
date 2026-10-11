"""Generator idioms: pipelines, windowing, generators as methods and __iter__, closures over generator state."""

import itertools


def numbers():
    n = 0
    while True:
        yield n
        n += 1


def evens(src):
    for v in src:
        if v % 2 == 0:
            yield v


def squares(src):
    for v in src:
        yield v * v


def take(n, src):
    for _, v in zip(range(n), src):
        yield v


print("pipeline", list(take(5, squares(evens(numbers())))))


def window(it, size):
    buf = []
    for v in it:
        buf.append(v)
        if len(buf) > size:
            buf.pop(0)
        if len(buf) == size:
            yield tuple(buf)


print("sliding window", list(window(range(6), 3)))


def chunks(seq, n):
    it = iter(seq)
    while chunk := list(itertools.islice(it, n)):
        yield chunk


print("chunks", list(chunks("abcdefg", 3)))


class Tree:
    def __init__(self, value, *children):
        self.value = value
        self.children = children

    def __iter__(self):
        yield self.value
        for c in self.children:
            yield from c

    def leaves(self):
        if not self.children:
            yield self.value
        for c in self.children:
            yield from c.leaves()

    def depth_first_with_level(self, level=0):
        yield level, self.value
        for c in self.children:
            yield from c.depth_first_with_level(level + 1)


t = Tree(1, Tree(2, Tree(4)), Tree(3, Tree(5), Tree(6)))
print("__iter__ generator", list(t))
print("method generator", list(t.leaves()))
print("levels", list(t.depth_first_with_level()))
print("iterate twice", list(t) == list(t))


def interleave(*iterables):
    iters = [iter(i) for i in iterables]
    while iters:
        for it in list(iters):
            try:
                yield next(it)
            except StopIteration:
                iters.remove(it)


print("interleave", list(interleave("abc", [1, 2], "XYZW")))


def unique(it):
    seen = set()
    for v in it:
        if v not in seen:
            seen.add(v)
            yield v


print("unique order preserved", list(unique([3, 1, 3, 2, 1, 4])))


def running_total(it):
    total = 0
    for v in it:
        total += v
        yield total


print("running total", list(running_total(range(1, 6))))
print("accumulate equivalent", list(itertools.accumulate(range(1, 6))))


def tee_like():
    a, b = itertools.tee(x * 3 for x in range(4))
    return list(a), list(b)


print("tee", tee_like())


def collatz(n):
    while n != 1:
        yield n
        n = n // 2 if n % 2 == 0 else 3 * n + 1
    yield 1


print("collatz 27 length", sum(1 for _ in collatz(27)), max(collatz(27)))


def generator_with_closure():
    count = 0

    def bump():
        nonlocal count
        count += 1

    for _ in range(3):
        bump()
        yield count


print("generator with nested closure", list(generator_with_closure()))


def gen_of_gens():
    for i in range(3):
        yield (i * 10 + j for j in range(2))


print("generator of generators", [list(g) for g in gen_of_gens()])


def zip_longest_manual(a, b, fill=None):
    ia, ib = iter(a), iter(b)
    while True:
        va = next(ia, fill)
        vb = next(ib, fill)
        if va is fill and vb is fill:
            return
        yield va, vb


print("manual zip_longest", list(zip_longest_manual([1, 2, 3], "ab", fill="-")))

print("groupby", [(k, len(list(g))) for k, g in itertools.groupby("aaabccdd")])
print("product generator", list(itertools.product("ab", repeat=2)))


def lines():
    data = "alpha\n\nbeta\n# comment\ngamma\n"
    for line in data.splitlines():
        yield line


def non_blank(src):
    return (line for line in src if line and not line.startswith("#"))


print("text pipeline", [s.upper() for s in non_blank(lines())])
