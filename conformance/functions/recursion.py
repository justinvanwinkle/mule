"""Recursion: direct, mutual, through closures, accumulator-style, memoized, and bounded-depth."""


def fact(n):
    return 1 if n <= 1 else n * fact(n - 1)


print("fact", fact(0), fact(5), fact(25))


def is_even(n):
    return True if n == 0 else is_odd(n - 1)


def is_odd(n):
    return False if n == 0 else is_even(n - 1)


print("mutual", [is_even(i) for i in range(6)], is_odd(201))


def make_fib():
    cache = {}

    def fib(n):
        if n in cache:
            return cache[n]
        r = n if n < 2 else fib(n - 1) + fib(n - 2)
        cache[n] = r
        return r

    return fib


fib = make_fib()
print("memo closure fib", fib(90))


def walk(tree, depth=0):
    if isinstance(tree, list):
        out = []
        for sub in tree:
            out.extend(walk(sub, depth + 1))
        return out
    return [(tree, depth)]


print("tree walk", walk([1, [2, [3, [4]], 5], [[6]]]))


def flatten(x):
    if not isinstance(x, (list, tuple)):
        return [x]
    return [y for item in x for y in flatten(item)]


print("flatten", flatten([1, (2, [3, (4,)]), [], [[5]]]))


def hanoi(n, src="A", dst="C", via="B", moves=None):
    if moves is None:
        moves = []
    if n:
        hanoi(n - 1, src, via, dst, moves)
        moves.append(src + dst)
        hanoi(n - 1, via, dst, src, moves)
    return moves


print("hanoi 3", hanoi(3))
print("hanoi 10 count", len(hanoi(10)))


def ackermann(m, n):
    if m == 0:
        return n + 1
    if n == 0:
        return ackermann(m - 1, 1)
    return ackermann(m - 1, ackermann(m, n - 1))


print("ackermann", ackermann(2, 3))


def depth(n):
    return 0 if n == 0 else 1 + depth(n - 1)


print("deep but bounded", depth(500))


def permutations(xs):
    if len(xs) <= 1:
        return [list(xs)]
    out = []
    for i, x in enumerate(xs):
        for rest in permutations(xs[:i] + xs[i + 1:]):
            out.append([x] + rest)
    return out


print("permutations", ["".join(p) for p in permutations("abc")])


# Recursion through a rebinding: the function name is looked up at call time
def countdown(n):
    return [n] + countdown(n - 1) if n else [0]


original = countdown


def countdown(n):
    return ["replaced", n]


print("name looked up at call time", original(3))


# Recursion via a lambda stored in a dict
ops = {}
ops["sum"] = lambda xs: 0 if not xs else xs[0] + ops["sum"](xs[1:])
print("dict lambda recursion", ops["sum"](list(range(11))))


# Recursion inside a nested function referencing itself through closure
def outer_sum(n):
    def go(k):
        return 0 if k == 0 else k + go(k - 1)

    return go(n)


print("closure self-recursion", outer_sum(100))


# Recursion with exceptions unwinding through frames
def find(tree, target, path=()):
    if tree == target:
        raise LookupError("/".join(path) or "<root>")
    if isinstance(tree, dict):
        for k, v in tree.items():
            find(v, target, path + (k,))


try:
    find({"a": {"b": 1, "c": {"d": 42}}}, 42)
except LookupError as e:
    print("found via exception", e)


# Mutually recursive closures
def parity_pair():
    def ev(n):
        return n == 0 or od(n - 1)

    def od(n):
        return n != 0 and ev(n - 1)

    return ev, od


ev, od = parity_pair()
print("mutual closures", ev(10), od(7), ev(7))
