"""collections.Counter (arithmetic, most_common ties, elements, total) and defaultdict (factories, nested defaultdicts)."""
from collections import Counter, defaultdict

c = Counter("abracadabra")
print("counter", sorted(c.items()))
print("most_common 2", c.most_common(2))
print("most_common ties insertion order", Counter("bbaacc").most_common())
print("most_common all", c.most_common())
print("missing key", c["z"], "z" in c)
print("elements", "".join(c.elements()))
print("total", c.total())
c2 = Counter(a=1, b=-2, z=3)
print("add", sorted((c + c2).items()))
print("sub drops nonpositive", sorted((c - c2).items()))
print("or max", sorted((c | c2).items()))
print("and min", sorted((c & c2).items()))
print("unary +", sorted((+c2).items()))
print("unary -", sorted((-c2).items()))
c3 = Counter(c)
c3.subtract(a=10, q=1)
print("subtract keeps negatives", sorted(c3.items()))
c3.update("aaa")
c3.update({"b": 5})
print("update", c3["a"], c3["b"])
print("counter eq ignores zero", Counter(a=1, b=0) == Counter(a=1))
print("counter compare", Counter(a=1) <= Counter(a=2, b=1), Counter(a=3) < Counter(a=2))
words = "the cat and the hat and the bat".split()
wc = Counter(words)
print("word counts", wc.most_common(3))
del wc["the"]
print("after del", wc.most_common(1))
print("from mapping", Counter({"x": 3}).most_common())
try:
    Counter.fromkeys("abc")
except NotImplementedError as e:
    print("fromkeys raises", type(e).__name__)
print("most_common 0", c.most_common(0))
print("counter repr", Counter(b=2, a=3))

dd = defaultdict(list)
for word in ["apple", "bob", "avocado", "cat", "banana"]:
    dd[word[0]].append(word)
print("defaultdict list", sorted(dd.items()))
print("default_factory", dd.default_factory is list)
print("get does not create", dd.get("zz"), "zz" in dd)
_ = dd["zz"]
print("getitem creates", "zz" in dd, dd["zz"])

counts = defaultdict(int)
for ch in "mississippi":
    counts[ch] += 1
print("defaultdict int", sorted(counts.items()))


def tree():
    return defaultdict(tree)


t = tree()
t["a"]["b"]["c"] = 1
t["a"]["x"] = 2
t["q"]


def to_plain(node):
    if isinstance(node, defaultdict):
        return {k: to_plain(v) for k, v in sorted(node.items())}
    return node


print("autovivify", to_plain(t))
nested = defaultdict(lambda: defaultdict(set))
nested["u1"]["tags"].add("x")
nested["u1"]["tags"].add("y")
nested["u2"]["roles"].add("admin")
print("nested sets", {u: {k: sorted(v) for k, v in m.items()} for u, m in sorted(nested.items())})

nofactory = defaultdict()
try:
    nofactory["missing"]
except KeyError as e:
    print("no factory", type(e).__name__)
print("repr", defaultdict(int, {"k": 1}))
merged = defaultdict(int, a=1) | {"b": 2}
print("or keeps type", type(merged).__name__, sorted(merged.items()))
