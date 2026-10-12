"""Comprehension scopes: iteration variables do not leak, walrus binds outward, nesting rules."""

x = "global x"
squares = [x * x for x in range(4)]
print("list comp var does not leak", squares, x)

gens = list(x for x in "ab")
sets = sorted({x for x in (3, 1, 2)})
dicts = {x: x * 2 for x in range(2)}
print("all comprehension kinds isolate", gens, sets, dicts, x)


def in_function():
    y = "local y"
    vals = [y for y in range(3)]
    return vals, y


print("function local not clobbered", in_function())


# comprehension reads enclosing function locals
def enclosing_read():
    factor = 3
    return [n * factor for n in range(3)]


print("enclosing read", enclosing_read())


# comprehension iteration variable shadowing an enclosing name used in the iterable
def shadow_in_iterable():
    n = [10, 20]
    return [n for n in n]


print("iterable uses outer n", shadow_in_iterable())

# nested comprehensions: inner can see outer variable
matrix = [[r * 3 + c for c in range(3)] for r in range(2)]
print("nested", matrix)
flat = [v for row in matrix for v in row if v % 2]
print("flatten with condition", flat)

# later for-clauses can reference earlier ones
pairs = [(a, b) for a in range(3) for b in range(a)]
print("dependent clauses", pairs)

# walrus binds in containing scope, not comprehension scope
total = 0
partial_sums = [total := total + v for v in [1, 2, 3]]
print("walrus outward", partial_sums, total)


def walrus_in_function():
    found = [(last := v) for v in "xyz"]
    return found, last


print("walrus in function", walrus_in_function())


# walrus in nested comprehension binds outermost function scope
def walrus_nested():
    grid = [[(cell := (r, c)) for c in range(2)] for r in range(2)]
    return cell


print("walrus nested", walrus_nested())

# walrus may not rebind an iteration variable
for src in ("[i := 0 for i in range(2)]", "[[(j := 1) for i in range(2)] for j in range(2)]",
            "[x for x in (y := [1])]"):
    try:
        compile(src, "<t>", "eval")
        print("compiled", src)
    except SyntaxError:
        print("syntax error for", src)


# global/nonlocal declarations apply to walrus targets in comprehensions
def walrus_nonlocal():
    acc = "unset"

    def inner():
        nonlocal acc
        [acc := v for v in range(3)]

    inner()
    return acc


print("walrus with nonlocal", walrus_nonlocal())

g_target = "unset"


def walrus_global():
    global g_target
    [g_target := v * 10 for v in range(3)]


walrus_global()
print("walrus with global", g_target)

# generator expression variables isolated; lazy evaluation sees updated enclosing variables
offset = 1
gen = (v + offset for v in range(3))
offset = 100
print("generator late binding", list(gen))

# first iterable is evaluated eagerly; errors surface at creation
try:
    bad = (v for v in undefined_name)
except NameError as e:
    print("first iterable eager", type(e).__name__)
lazy = (undefined_name for v in range(1))
try:
    list(lazy)
except NameError as e:
    print("body lazy", type(e).__name__)

# comprehension variable is not visible after an exception inside the comprehension
try:
    [1 / z for z in (1, 0)]
except ZeroDivisionError:
    print("z leaked?", "z" in globals())

# locals() inside an inlined comprehension (PEP 709) includes the function's locals and the iteration variable
def comp_locals():
    outer_name = 1
    return [sorted(k for k in locals() if not k.startswith(".")) for _ in range(1)]


print("locals in comprehension", comp_locals())


# lambda inside comprehension captures comprehension variable per iteration? (no: late binding)
fns = [lambda: k for k in range(3)]
print("lambda in comprehension", [f() for f in fns])

# async-free nested generator expression inside list comprehension
print("genexp in listcomp", [sum(v for v in range(n)) for n in range(5)])

# dict comprehension key/value evaluation order
order = []
d = {order.append(("k", n)) or n: order.append(("v", n)) or n for n in range(2)}
print("dict comp order", order, d)
