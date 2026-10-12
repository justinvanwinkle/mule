"""functools: reduce, partial (nested, keyword overrides), wraps, total_ordering, cmp_to_key."""
import operator
from functools import cmp_to_key, partial, reduce, total_ordering, wraps

print("reduce add", reduce(operator.add, [1, 2, 3, 4]))
print("reduce initial", reduce(operator.add, [1, 2, 3], 100))
print("reduce single", reduce(operator.mul, [7]))
print("reduce empty initial", reduce(operator.mul, [], 1))
try:
    reduce(operator.add, [])
except TypeError as e:
    print("reduce empty", type(e).__name__)
print("reduce order", reduce(lambda acc, x: f"({acc}{x})", "abcd"))
print("reduce order init", reduce(lambda acc, x: f"({acc}{x})", "abc", "_"))
print("reduce generator", reduce(lambda a, b: a * 10 + b, (d for d in [3, 1, 4])))


def power(base, exp, *, mod=None):
    r = base ** exp
    return r % mod if mod is not None else r


square = partial(power, exp=2)
print("partial kw", square(9))
print("partial kw override", square(9, exp=3))
cube_mod = partial(power, exp=3, mod=10)
print("partial mod", cube_mod(7))
two_to = partial(power, 2)
print("partial positional", two_to(10))
nested = partial(partial(power, 3), mod=5)
print("nested call", nested(4))
print("nested flattened func", nested.func is power)
print("nested args", nested.args, sorted(nested.keywords.items()))
p2 = partial(cube_mod, mod=1000)
print("nested kw override", p2(7), sorted(p2.keywords.items()))
join3 = partial("{}-{}-{}".format, "a")
print("partial bound method", join3("b", "c"))
adder = partial(sum, start=10)
print("partial sum start", adder([1, 2]))


def trace(fn):
    @wraps(fn)
    def wrapper(*args, **kwargs):
        result = fn(*args, **kwargs)
        print(f"  {fn.__name__}{args} -> {result}")
        return result
    return wrapper


@trace
def add(a, b):
    """Add two things."""
    return a + b


add(2, 3)
print("wraps name", add.__name__)
print("wraps doc", add.__doc__)
print("wraps wrapped", add.__wrapped__(10, 20))
print("wraps qualname", add.__qualname__)


@total_ordering
class Version:
    def __init__(self, *parts):
        self.parts = parts

    def __eq__(self, other):
        if not isinstance(other, Version):
            return NotImplemented
        return self.parts == other.parts

    def __lt__(self, other):
        if not isinstance(other, Version):
            return NotImplemented
        return self.parts < other.parts

    def __repr__(self):
        return "v" + ".".join(map(str, self.parts))


a, b = Version(1, 2), Version(1, 10)
print("total_ordering", a < b, a <= b, a > b, a >= b, a == b, a != b)
print("total_ordering same", a <= Version(1, 2), a >= Version(1, 2))
print("sorted versions", sorted([Version(2), Version(1, 10), Version(1, 2, 3), Version(1, 2)]))
print("max version", max(a, b))
try:
    a < 3
except TypeError as e:
    print("total_ordering mixed", type(e).__name__)


def compare_len_then_alpha(x, y):
    if len(x) != len(y):
        return len(x) - len(y)
    return (x > y) - (x < y)


words = ["pear", "fig", "apple", "kiwi", "date", "banana"]
print("cmp_to_key", sorted(words, key=cmp_to_key(compare_len_then_alpha)))
print("cmp_to_key reverse", sorted(words, key=cmp_to_key(compare_len_then_alpha), reverse=True))
K = cmp_to_key(lambda x, y: y - x)
print("cmp_to_key objects", K(1) < K(2), K(2) == K(2))
print("cmp_to_key min", min([5, 3, 9], key=K))
