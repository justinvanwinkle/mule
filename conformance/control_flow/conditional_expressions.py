"""Conditional expressions: nesting, chaining, laziness, and precedence."""

log = []


def v(name, value):
    log.append(name)
    return value


def show(label, value):
    print(label, value, log)
    log.clear()


show("simple true", v("a", 1) if v("c", True) else v("b", 2))
show("simple false", v("a", 1) if v("c", 0) else v("b", 2))

# right-associative chaining
def sign(n):
    return "neg" if n < 0 else "zero" if n == 0 else "pos"


print("sign", [sign(n) for n in (-3, 0, 4)])

# explicit nesting in the condition position
show("nested condition", v("x", "X") if (v("i", 1) if v("o", False) else v("e", 0)) else v("y", "Y"))

# nested in the true branch
show("nested true branch", (v("p", "P") if v("c2", False) else v("q", "Q")) if v("c1", True) else v("r", "R"))

# precedence: lambda body, or/and inside
f = lambda n: n * 2 if n > 2 else -n
print("lambda body", f(1), f(5))
print("or inside", (0 or 5) if 0 or 1 else 9)
print("and binds tighter", 1 if 0 and 1 / 0 else 2)
print("arith in branches", 1 + 2 if False else 3 * 4)
print("tuple in branch", (1, 2) if True else (3,))
print("not in condition", "yes" if not [] else "no")

# conditional expression producing callables
op = max if len("ab") > 1 else min
print("callable choice", op(3, 7))

# in comprehensions and generator expressions
print("comp", [x if x % 2 else -x for x in range(6)])
print("comp filter and ternary", [("e" if x % 2 == 0 else "o") for x in range(5) if x])
print("dict comp", {k: ("big" if k > 2 else "small") for k in range(4)})

# as argument and in f-strings
print("arg", abs(-5 if True else 5))
n = 3
print(f"fstring {'many' if n > 1 else 'one'} items")

# laziness of untaken branch: errors not raised
print("lazy", "ok" if True else 1 / 0)
print("lazy2", {}["missing"] if False else "skipped")

# conditional expression returning different types
for val in (0, "", None, [1]):
    print("typed", type(val).__name__, (val if val else "fallback"))

# conditional with walrus
data = [5, 0, 7]
print("walrus", [(y if (y := x * 2) else "zero") for x in data])

# chained ternary with side effects only evaluating the necessary parts
def bucket(n):
    return v("lt10", "small") if v("t1", n < 10) else v("lt100", "medium") if v("t2", n < 100) else v("big", "large")


for n in (5, 50, 500):
    show(f"bucket {n}", bucket(n))

# ternary as statement-level expression with side effects
log.append("start")
v("side-a", None) if len(log) == 1 else v("side-b", None)
show("statement ternary", "done")
