"""Assignment expressions nested in comprehensions, lambdas, default arguments, conditions and module-level code."""

# Walrus in a comprehension binds in the enclosing (module) scope
total = 0
partials = [total := total + n for n in range(5)]
print("running totals", partials, "total", total)

# Nested comprehension at module level rebinding a global
last = None
grid = [[(last := (i, j)) for j in range(2)] for i in range(3)]
print("nested comp walrus", grid[-1], "last", last)

# Walrus inside a comprehension inside a function binds the function local
def f():
    hits = [y for x in range(10) if (y := x * x) % 3 == 0]
    return hits, y


print("function local from comprehension", f())

# Walrus inside a lambda inside a default argument (evaluated at def time)
def g(a=(lambda: [w := 5, w * 2])(), b=(seed := 7)):
    return a, b


print("lambda default", g(), "seed leaked to module", seed)

# Lambda containing a comprehension with walrus binds in the lambda's scope
h = lambda xs: ([m := x for x in xs], m)
print("lambda comprehension", h([3, 1, 4]))
print("module m untouched", "m" in globals())

# Walrus in while condition with side-effecting iterator
it = iter([3, 1, 0, 5])
collected = []
while (v := next(it)) != 0:
    collected.append(v)
print("while walrus", collected, v)

# Walrus in if/elif chains
def classify(s):
    if (n := len(s)) > 5:
        return "long", n
    elif (first := s[:1]) == "a":
        return "a-word", first
    return "other", n


print("elif walrus", [classify(s) for s in ["abracadabra", "ant", "bee"]])

# Walrus in generator expression: binding visible after consumption
def gen_bind():
    g = (z := i for i in range(3))
    before = "z" in locals()
    list(g)
    return before, z


print("genexpr binding", gen_bind())

# Walrus in nested function via nonlocal
def outer():
    count = 0

    def inc():
        nonlocal count
        return [count := count + k for k in (1, 2)]
    inc()
    return count


print("nonlocal through comprehension", outer())

# Walrus in a class body comprehension binds in the class? No: it is a SyntaxError,
# so instead bind inside a method's comprehension and inspect the instance.
class K:
    def run(self):
        self.vals = [(q := c.upper()) for c in "ab"]
        return q


print("method comprehension walrus", K().run())

# Walrus as function argument and in subscripts / slices
data = list(range(10))
print("in call", max(a := 3, a * 2), a)
print("in subscript", data[(i := 4)], i)
print("in slice", data[(lo := 2):(hi := lo + 3)], lo, hi)

# Walrus in f-string requires parentheses
print(f"fstring {(fw := 10) + 1} then {fw}")

# Walrus with any() short-circuit captures witness
nums = [2, 4, 7, 8, 9]
if any((witness := n) % 2 for n in nums):
    print("first odd witness", witness)
if not all((counter := n) < 5 for n in nums):
    print("first failing", counter)

# Walrus in comprehension inside a comprehension condition referencing outer walrus
pairs = [(x, y) for x in range(4) if (sq := x * x) > 1 for y in range(sq) if y == sq - 1]
print("chained conditions", pairs, sq)

# Walrus in a dict comprehension key and value
dc = {(k := c): (k * 2) for c in "xy"}
print("dict comp", dc, k)

# Walrus in ternary and in a tuple
r = (t1 := 1, t2 := t1 + 1) if (flag := True) else None
print("tuple and ternary", r, t1, t2, flag)
