"""operator: itemgetter, attrgetter (dotted), methodcaller, arithmetic/comparison functions with reduce and map, in-place ops, index/length_hint."""
import operator as op
from functools import reduce

print("itemgetter single", op.itemgetter(1)("abc"))
print("itemgetter multi", op.itemgetter(0, 2)("abc"))
print("itemgetter dict", op.itemgetter("b", "a")({"a": 1, "b": 2}))
print("itemgetter slice", op.itemgetter(slice(1, None))([1, 2, 3]))
rows = [("bo", 30, "nyc"), ("ann", 25, "sf"), ("cy", 30, "la")]
print("sort by itemgetter", sorted(rows, key=op.itemgetter(1, 0)))
print("max by itemgetter", max(rows, key=op.itemgetter(1)))
try:
    op.itemgetter(5)([1])
except IndexError as e:
    print("itemgetter missing", type(e).__name__)


class Addr:
    def __init__(self, city, zipcode):
        self.city = city
        self.zipcode = zipcode


class Person:
    def __init__(self, name, addr):
        self.name = name
        self.addr = addr

    def greet(self, greeting, punct="!"):
        return f"{greeting}, {self.name}{punct}"


people = [Person("ann", Addr("sf", "94110")), Person("bo", Addr("la", "90001")), Person("cy", Addr("nyc", "10001"))]
print("attrgetter", op.attrgetter("name")(people[0]))
print("attrgetter dotted", op.attrgetter("addr.city")(people[1]))
print("attrgetter multi", op.attrgetter("name", "addr.zipcode")(people[2]))
print("sort dotted", [p.name for p in sorted(people, key=op.attrgetter("addr.zipcode"))])
try:
    op.attrgetter("addr.state")(people[0])
except AttributeError as e:
    print("attrgetter missing", type(e).__name__)

print("methodcaller", op.methodcaller("greet", "hi")(people[0]))
print("methodcaller kw", op.methodcaller("greet", "yo", punct="?")(people[1]))
print("methodcaller str", list(map(op.methodcaller("upper"), ["a", "b"])))
print("methodcaller split", op.methodcaller("split", ",", 1)("a,b,c"))

nums = [1, 2, 3, 4, 5]
print("reduce add", reduce(op.add, nums))
print("reduce mul", reduce(op.mul, nums))
print("reduce sub", reduce(op.sub, nums))
print("reduce or", reduce(op.or_, [1, 2, 4, 8]))
print("reduce and", reduce(op.and_, [{1, 2, 3}, {2, 3}, {3, 9}]))
print("reduce concat", reduce(op.concat, [[1], [2, 3], []]))
print("map pow", list(map(op.pow, [2, 3], [3, 2])))
print("map truediv/floordiv/mod", op.truediv(7, 2), op.floordiv(-7, 2), op.mod(-7, 2))
print("neg/pos/abs/inv", op.neg(3), op.pos(-3), op.abs(-3), op.invert(5))
print("shifts xor", op.lshift(1, 4), op.rshift(256, 3), op.xor(0b1100, 0b1010))
print("comparisons", op.lt(1, 2), op.le(2, 2), op.eq(1, 1.0), op.ne("a", "a"), op.ge(1, 2), op.gt(3, 2))
print("logic", op.not_([]), op.truth([0]), op.is_(None, None), op.is_not(1, None))
print("contains/countOf/indexOf", op.contains("abc", "b"), op.countOf([1, 2, 1], 1), op.indexOf("xyz", "z"))
print("getitem/setitem/delitem", op.getitem({"k": 1}, "k"))
lst = [1, 2, 3]
op.setitem(lst, 0, 9)
op.delitem(lst, 1)
print("after set/del", lst)
print("index", op.index(7), op.index(True))
try:
    op.index(1.5)
except TypeError as e:
    print("index float", type(e).__name__)
print("length_hint", op.length_hint([1, 2]), op.length_hint(iter(range(5))), op.length_hint(x for x in []), op.length_hint(object(), 42))
a = [1]
b = op.iadd(a, [2])
print("iadd mutates list", a, b is a)
t = (1,)
u = op.iadd(t, (2,))
print("iadd tuple new", t, u)
s = "x"
print("imul str", op.imul(s, 3), s)
print("matmul", op.matmul.__name__)
print("call", op.call(max, 3, 9, 4))
print("dot product", sum(map(op.mul, [1, 2, 3], [4, 5, 6])))
print("is_none", op.is_none(None), op.is_not_none(0))
