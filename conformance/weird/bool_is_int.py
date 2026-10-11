"""bool as an int subclass: indexing, dict/set key collapse, arithmetic, counting, and formatting."""

xs = ["zero", "one", "two"]
print("index with True/False", xs[True], xs[False], xs[-True])
print("slice with bools", xs[False:True], xs[True::True])
d = {True: "bool", 1: "int", 1.0: "float"}
print("dict key collapse", d)
d2 = {1: "int", True: "bool"}
print("first key kept, last value wins", d2)
d3 = {0: "a", False: "b", 0.0: "c", 0j: "d"}
print("zero keys", d3)
print("set collapse", {True, 1, 1.0}, {False, 0, 0.0, 0j})
print("len set mixed", len({True, 1, 2, False, 0, ""}))
print("sum of comparisons", sum(x > 1 for x in [0, 1, 2, 3, 4]))
print("True + True", True + True, True * 3, True / 2, True // 2, True - False)
print("True ** False", True ** False, False ** False)
print("bitwise on bools stays bool", True & False, True | False, True ^ True, type(True & True).__name__)
print("bitwise mixed becomes int", True & 3, True | 2, type(True | 2).__name__)
print("neg", -True, +True, -False, abs(-True))
print("type of arithmetic", type(True + True).__name__, type(-True).__name__)
print("isinstance", isinstance(True, int), issubclass(bool, int), type(True) is int)
print("equality with int", True == 1, False == 0, True is not 1 + 0)
print("hash equal", hash(True) == hash(1), hash(False) == hash(0) == hash(0.0))
print("count", [1, True, 1.0, 2].count(True), [0, False, None, ""].count(False))
print("index", [0, 1, 2].index(True), (False, 0).index(0))
print("in", True in [1], 1 in [True], 0.0 in [False])
print("str/repr", str(True), repr(False), f"{True}", f"{True:d}", f"{True:>5}", "%d" % True, "%s" % True)
print("format numeric", format(True, "b"), format(True, ".2f"), format(False, "x"))
print("int()", int(True), float(False), complex(True))
print("max/min", max(True, 1), max(1, True), min(False, 0), min(0, False))
print("sorted mixed", sorted([2, True, 0, False, 1]))
print("divmod", divmod(True, 2), divmod(7, True))
try:
    1 / False
except ZeroDivisionError as e:
    print("divide by False", type(e).__name__)
print("range with bools", list(range(True, 4, True)))
print("string mult", "ab" * True, "ab" * False + "|")
print("list mult", [0] * True, [0] * False)
print("shift", True << 4, 16 >> True)
print("round", round(True), round(True, 2), type(round(True)).__name__)
print("bit_length", True.bit_length(), False.bit_length(), True.real, True.imag, True.numerator)
print("int methods", True.to_bytes(1, "big"), (True).conjugate())
print("bool of strings", bool("False"), bool(""), bool(" "))
print("bool() of numbers", bool(0.0), bool(-0.0), bool(0j), bool(float("nan")))
print("comparisons", True > False, False < 0.5, True >= 1.0)
print("counter of bools", {k: v for k, v in __import__("collections").Counter([True, 1, False, 0, 2]).items()})
print("dict get with int on bool key", {True: "x"}.get(1), {1: "y"}.get(True))
print("enumerate start bool", list(enumerate("ab", start=True)))


class Flag(int):
    pass


print("int subclass equal to bool", Flag(1) == True, {Flag(1): "f", True: "t"})
try:
    class Sub(bool):
        pass
except TypeError as e:
    print("cannot subclass bool", type(e).__name__)
print("bool singletons", bool(1) is True, bool([]) is False)
print("not returns bool", not 5, type(not 5).__name__)
print("and/or return operands", 0 and True, 5 or False, True and 7)
print("bool sum of bools", sum([True, True, False]), type(sum([True, True])).__name__)
print("any/all on bools", any([False, False]), all([]), any([]))
