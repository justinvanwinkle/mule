"""Closures: counters, closures over closures, loop late binding, shared cells between siblings."""


def make_counter(start=0):
    count = start

    def inc(step=1):
        nonlocal count
        count += step
        return count

    return inc


c1 = make_counter()
c2 = make_counter(100)
print("counter", c1(), c1(), c1(5), c2(), c1())


def outer(a):
    def middle(b):
        def inner(c):
            return a + b + c

        return inner

    return middle


print("closure over closure", outer(1)(10)(100), outer("a")("b")("c"))


def make_pair():
    shared = []

    def push(x):
        shared.append(x)
        return len(shared)

    def snapshot():
        return list(shared)

    return push, snapshot


push, snap = make_pair()
push(1)
push(2)
print("shared cell (mutable)", snap())


def make_get_set():
    value = "init"

    def get():
        return value

    def set_(v):
        nonlocal value
        value = v

    return get, set_


get, set_ = make_get_set()
before = get()
set_("changed")
print("shared cell rebinding", before, get())

# Late binding in loops: all closures see the final value
funcs = []
for i in range(3):
    funcs.append(lambda: i)
print("loop late binding", [f() for f in funcs])

# Fix with a factory
def bind(v):
    return lambda: v


print("factory binding", [bind(i)() for i in range(3)])

# Comprehension variable closure: each comprehension has its own scope
cfuncs = [lambda: j for j in range(3)]
print("comprehension late binding", [f() for f in cfuncs])

# Closure sees later assignment in enclosing scope
def later_assignment():
    def show():
        return x

    x = "assigned after def"
    return show()


print("later assignment", later_assignment())

# Closure cell updated after closure creation
def updates():
    x = 1
    f = lambda: x  # noqa: E731
    x = 2
    r1 = f()
    x = 3
    return r1, f()


print("cell updates", updates())

# Closure retains environment after outer returns, independent per call
def accumulator():
    total = 0

    def add(n):
        nonlocal total
        total += n
        return total

    return add


acc_a, acc_b = accumulator(), accumulator()
print("independent envs", acc_a(1), acc_a(2), acc_b(10), acc_a(3))

# Closure over loop variable inside function body, cell rebinding per iteration
def loop_cells():
    out = []
    for k in "abc":
        def f():
            return k
        out.append(f)
    return [f() for f in out]


print("loop cells in function", loop_cells())

# Closure reading a variable deleted later raises NameError
def deleted():
    x = 1

    def f():
        return x

    del x
    try:
        return f()
    except NameError as e:
        return type(e).__name__


print("deleted free var", deleted())

# Closure over a parameter
def param_closure(p):
    return lambda: p * 2


print("param closure", param_closure(21)())

# Class body does not create a closure scope for methods
class Scope:
    hidden = "class attr"

    def method(self):
        try:
            return hidden  # noqa: F821
        except NameError as e:
            return type(e).__name__


print("class scope not enclosing", Scope().method())
