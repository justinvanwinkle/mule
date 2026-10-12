"""bool as an int subclass: arithmetic, sums, indexing, and dict key collapsing."""

print("arith", True + True, True * 10, True - False, -True, +True, True / 2, True ** 3)
print("types", type(True + True).__name__, type(-True).__name__, type(True & True).__name__, type(True | False).__name__)
print("isinstance", isinstance(True, int), issubclass(bool, int), bool is bool)
print("sum of bools", sum([True, False, True, True]), sum(x > 2 for x in range(10)))
print("count trues", [1, True, 1.0, 2].count(True), [0, False, 0.0].count(False))
print("index with bool", ["a", "b"][True], ["a", "b"][False], "xyz"[True:])
print("repr", repr(True), str(False), f"{True}", f"{True:d}", f"{True:>5}", format(False, "05"))
print("int/float", int(True), float(False), True.real, True.numerator, True.denominator)

d = {}
d[1] = "int"
d[True] = "bool"
d[1.0] = "float"
d[1 + 0j] = "complex"
print("collapsed dict", d, len(d))
d2 = {True: "a", 1: "b", 1.0: "c"}
print("literal collapse", d2)
d3 = {0: "zero", False: "false", 0.0: "fz", -0.0: "nz"}
print("zero collapse", d3)
s = {1}
print("set collapse", s, len(s))
s2 = {True}
print("set first wins", s2)
print("membership", True in {1: 0}, 1 in {True}, 0.0 in {False: 1})

print("comparisons", True == 1, False == 0, True == 1.0, True > False, True < 2)
print("bool ops return operands", 0 or 5, 3 and 0, True and 7, False or False, [] or {})
print("not", not 0, not 1, not True, not [], type(not 5).__name__)
print("bool of bool math", bool(True - True), bool(True + True))
print("max/min bools", max(True, 1), max(1, True), min(False, 0), sorted([True, 0, False, 1, -1]))
print("bitwise with int", True & 3, True | 4, True ^ 1, type(True ^ 1).__name__, ~int(False))
print("shift", True << 10, True >> 1)
print("divmod", divmod(True, 2), True // True, True % True)
print("bool hash", hash(True) == hash(1), hash(False) == hash(0))
print("bool(5) is True", bool(5) is True, bool(-0.0) is False)
try:
    class Sub(bool):
        pass
except TypeError as e:
    print("subclass bool", type(e).__name__)
print("bool() of objects", bool(None), bool(()), bool((0,)), bool(" "), bool(0.0j))
