"""Call-site unpacking: *a, **k, multiple of each, mixing with explicit args, and evaluation order."""

trace = []


def t(label, value):
    trace.append(label)
    return value


def show(*args, **kwargs):
    return (args, sorted(kwargs.items()))


def fixed(a, b, c, d=0):
    return (a, b, c, d)


xs = [1, 2]
ys = (3, 4)
print("single star", show(*xs))
print("double star", show(*xs, *ys))
print("star mixed", show(0, *xs, 5, *ys, 6))
print("kw unpack", show(a=1))
print("two kw unpacks", show(a=1, b=2))
print("kw mixed", show(x=0, a=1, y=2, b=3))
print("all mixed", show(0, *xs, k=1, *ys, m=2))
print("fixed via star", fixed(*range(3)))
print("fixed via star and kw", fixed(*[1, 2], c=3, d=4))
print("star string", show(*"abc"))
print("star dict keys", show(*{"p": 1, "q": 2}))
print("star generator", show(*(i * i for i in range(4))))
print("star empty", show(*[]))
print("star set sorted", sorted(show(*{3, 1, 2})[0]))


class Mapping:
    """Minimal mapping: keys() and __getitem__ are enough for ** unpacking."""

    def keys(self):
        trace.append("keys")
        return ["u", "v"]

    def __getitem__(self, k):
        trace.append("get " + k)
        return k.upper()


trace.clear()
print("custom mapping", show(**Mapping()))
print("custom mapping trace", trace)


class Iter:
    def __iter__(self):
        trace.append("iter")
        return iter([7, 8])


trace.clear()
print("custom iterable", show(*Iter()))
print("custom iterable trace", trace)

# Evaluation order: function expression, then arguments left to right
trace.clear()
t("func", show)(t("a", 1), *t("star", [2]), t("b", 3), k=t("kw", 4), **t("dstar", {"z": 5}))
print("eval order", trace)

trace.clear()
t("func", show)(*t("s1", [1]), *t("s2", [2]), x=t("x", 0), *t("s3", [3]))
print("eval order stars after kw", trace)

trace.clear()
t("func", show)(**t("d1", {"a": 1}), b=t("b", 2), **t("d2", {"c": 3}))
print("eval order kw", trace)

# Unpacked containers are not aliased by the callee's *args
src = [1, 2, 3]


def mutate_args(*args):
    return args + (4,)


print("no alias", mutate_args(*src), src)

# Keyword unpacking into a function with a parameter named like the dict method
print("kw named keys", show(keys=1, items=2))

# Nested call unpacking
print("nested", show(*show(1, 2)[0], **dict(show(a=1)[1])))

# Star of a tuple of tuples
pairs = [(1, 2), (3, 4)]
print("zip star", list(zip(*pairs)))
print("dict from star", dict(*[[("k", "v")]]))
print("max star", max(*[3, 9, 4]))
