"""Closures capturing loop variables (late binding) versus default-argument capture."""

import functools

late = [lambda: i for i in range(3)]
print("comprehension lambdas late-bound", [f() for f in late])

early = [lambda i=i: i for i in range(3)]
print("default-argument capture", [f() for f in early])

partials = [functools.partial(lambda n: n * 10, i) for i in range(3)]
print("functools.partial capture", [p() for p in partials])

# for loop at function scope: all closures share one cell
def loop_closures():
    funcs = []
    for i in range(3):
        def f():
            return i
        funcs.append(f)
    i = "rebound after loop"
    return [f() for f in funcs]


print("function loop closures", loop_closures())


# factory function creates a new cell per call
def make_adder(n):
    def add(x):
        return x + n
    return add


adders = [make_adder(i) for i in range(3)]
print("factory capture", [a(100) for a in adders])

# while loop with closures
def while_closures():
    funcs = []
    k = 0
    while k < 3:
        funcs.append(lambda: k)
        k += 1
    return [f() for f in funcs]


print("while closures", while_closures())

# module-level loop: closures read the global
mods = []
for j in range(3):
    mods.append(lambda: j)
print("module loop closures", [f() for f in mods])
j = "changed global"
print("after changing global", [f() for f in mods])

# default argument evaluated once at definition time (mutable default)
def append_to(item, bucket=[]):
    bucket.append(item)
    return bucket


print("mutable default shared", append_to(1), append_to(2))

# default evaluated at definition time with expression side effects
log = []


def make_default():
    log.append("default evaluated")
    return "dflt"


def uses_default(x=make_default()):
    return x


print("default evaluation", uses_default(), uses_default(), log)

# closures over the same variable see each other's changes
def shared_cell():
    value = 0

    def get():
        return value

    def set_(v):
        nonlocal value
        value = v

    return get, set_


get, set_ = shared_cell()
set_(42)
print("shared cell", get())

# __closure__ cell contents
def outer():
    a = "cell a"
    b = "cell b"

    def inner():
        return a, b

    return inner


inner = outer()
print("closure cells", [c.cell_contents for c in inner.__closure__], inner.__code__.co_freevars)

# generator expression captures loop variable lazily for later clauses, eagerly for the first iterable
x = [1, 2]
gen = (v for v in x)
x = [9, 9]
print("genexp first iterable bound early", list(gen))

y = 1
gen2 = (v + y for v in range(2))
y = 100
print("genexp body bound late", list(gen2))

# nested comprehensions capturing outer comprehension variable
table = [[lambda: (r, c) for c in range(2)] for r in range(2)]
print("nested comprehension lambdas", [[f() for f in row] for row in table])

# keyword-only default capture
funcs = [lambda *, i=i: i * i for i in range(4)]
print("kw-only default capture", [f() for f in funcs])

# closures in class body loops: class scope is not a closure scope
class Holder:
    items = [lambda: n for n in range(2)]


n = "global n"
print("class body comprehension closures", [f() for f in Holder.items])
