"""Operator precedence and chained-comparison corner cases: not, unary minus vs **, in/is chains, lambda in ternaries."""

log = []


def s(label, v):
    log.append(label)
    return v


a, b = 1, 1
print("not a == b", not a == b)
print("a == (not b)", a == (not b))
print("not not", not not 0, not not [0])
print("-x ** y", -2 ** 2, (-2) ** 2, 2 ** -1, -2 ** -2)
print("power right assoc", 2 ** 3 ** 2, (2 ** 3) ** 2)
print("unary chain", - - 3, -+-+3, ~-1, -~1, ~~5)
print("not vs and/or", not 1 or 2, not (1 or 2), 0 or not 0 and 3)
print("and binds tighter than or", 1 or 0 and 0, (1 or 0) and 0)
print("comparison before not", not 1 < 0)
print("bitwise vs comparison", 1 & 3 == 1, (1 & 3) == 1)
print("shift vs add", 1 << 2 + 1, (1 << 2) + 1)
print("bitwise order", 1 | 2 ^ 3 & 4, ((1 | 2) ^ 3) & 4)
print("modulo with negatives", -7 % 3, 7 % -3, -7 // 3, divmod(-7, 3))
print("string repetition precedence", "ab" * 2 + "c", "x" + "y" * 3)
print("unary minus after power on call", -abs(-3) ** 2)

# Chained comparisons mixing in, is not, <
xs = [1, 2, 3]
print("in chain", 2 in xs != None, (2 in xs) != None)
zero = 0
print("is not chain", None is not zero < 1)
print("mixed chain", 1 < 2 in [2] is not None)
r = s("x", 0) < s("y", 1) in s("z", [1]) is not s("w", None)
print("side effect chain", r, log)
log.clear()
r = s("x", 5) < s("y", 1) in s("z", [1])
print("short-circuit chain", r, log)
log.clear()
print("equality chains", 1 == 1.0 == True, 0 == False == [] )
print("not in vs not (in)", 4 not in xs, not 4 in xs)
print("is vs ==", [] == [], (lambda l: l is l)([]))
print("chained != is not transitive", 1 != 2 != 1)

# Lambda and conditional expressions
f = lambda x: x if x else -1
print("lambda body includes ternary", f(0), f(5))
g = (lambda: 1) if False else (lambda: 2)
print("ternary of lambdas", g())
h = lambda: (yield_ := 3)
print("lambda walrus", h())
k = lambda x=1 if True else 2: x
print("ternary default in lambda", k())
nested = 1 if 0 else 2 if 0 else 3
print("ternary right assoc", nested)
print("ternary lower than or", (0 or 5 if 0 else 7))
print("lambda captures loop var late", [fn() for fn in [lambda: i for i in range(3)]])

# Starred and tuple precedence
t = 1, 2 + 3, (4,)
print("tuple without parens", t)
u = *xs, 4
print("starred display", u)
print("comma in subscript", {(1, 2): "tuple key"}[1, 2])
print("trailing comma one-tuple", (1,), 1,)

# Attribute and call precedence on literals
print("int literal attribute", (1).bit_length(), 1 .bit_length(), 1.0.is_integer())
print("negative literal attribute", -1 .bit_length(), (-1).bit_length())
print("string concat adjacency", "a" "b" 'c', ("x"
      "y"))
print("unary minus inside right operand of power", 2 ** -1 ** 2)

# Conditional expression in comprehension vs filter
print("ternary in element", [x if x % 2 else -x for x in range(4)])
print("filter clause", [x for x in range(4) if x % 2 if x > 1])

# in with strings and dict
print("substring", "" in "abc", "ab" in "abc", "ac" in "abc")
print("dict in checks keys", 1 in {1: 2}, 2 in {1: 2})
print("is with small literal", (lambda: None)() is None)

# not with comparison chains and parenthesized walrus
print("not chain", not 1 < 2 < 3)
print("walrus precedence", (n := 2 + 3) * 2, n)
