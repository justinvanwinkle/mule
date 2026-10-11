"""Class body scope: invisible to methods and comprehensions except the first iterable."""

x = "global x"


class A:
    x = "class x"
    y = x + " reused"           # later class attribute sees earlier one
    z = [x]                     # list display sees class scope

    def method(self):
        return x                # methods skip class scope

    def method_via_self(self):
        return self.x

    first_iterable = [c for c in x]           # first iterable evaluated in class scope
    try:
        inner_ref = [x for _ in range(1)]     # x resolved globally inside comprehension
    except NameError as e:
        inner_ref = type(e).__name__


a = A()
print("later attribute", A.y)
print("list display", A.z)
print("method sees global", a.method())
print("method via self", a.method_via_self())
print("first iterable from class scope", "".join(A.first_iterable))
print("comprehension body uses global", A.inner_ref)


class B:
    values = [1, 2, 3]
    factor = 10
    try:
        scaled = [v * factor for v in values]
    except NameError as e:
        scaled = "NameError (factor invisible in comprehension body)"
    doubled = [v * 2 for v in values]
    try:
        nested = [[v for v in values] for _ in range(1)]
    except NameError as e:
        nested = "NameError (values invisible in nested comprehension)"
    lam = lambda: factor


print("comprehension body cannot see class name", B.scaled)
print("first iterable ok", B.doubled)
print("nested iterable", B.nested)
factor = "global factor"
print("lambda in class body uses global", B.lam())


# class body names evaluated top-to-bottom; reassigning global during class body
counter = 0


class C:
    global counter
    counter += 1
    snapshot = counter
    local_counter = 5
    local_counter += 1


print("class body global", counter, C.snapshot, C.local_counter)


# class body can read enclosing function locals (free variables)
def factory(prefix):
    class Made:
        label = prefix + "-label"

        def get(self):
            return prefix

    return Made


M = factory("pre")
print("enclosing function visible", M.label, M().get())


# class body variable shadowing enclosing function variable: methods see enclosing
def shadowing():
    name = "function name"

    class K:
        name = "class name"

        def get(self):
            return name

    return K.name, K().get()


print("class shadow vs method", shadowing())


# a name assigned in the class body is class-local; reading it earlier skips the
# enclosing function and falls back to globals
v = "global v"


def tricky():
    v = "enclosing v"

    class K:
        before = v
        v = "class v"
        after = v

    return K.before, K.after


print("class body read-before-assign", tricky())


# names in class body are not visible to nested classes
class Outer:
    secret = "outer secret"

    class Inner:
        try:
            seen = secret
        except NameError as e:
            seen = type(e).__name__


print("nested class cannot see outer class body", Outer.Inner.seen)

# __qualname__ and __module__ assigned in class body
class D:
    q = __qualname__
    m = __module__


print("class body dunders", D.q, D.m)

# class body executed once; locals become attributes
class E:
    for i in range(3):
        pass
    del i
    temp = 1
    del temp


print("class body loop var deleted", hasattr(E, "i"), hasattr(E, "temp"))

# comprehension in class body that uses walrus is a SyntaxError
try:
    compile("class F:\n    [(y := 1) for _ in range(1)]\n", "<t>", "exec")
except SyntaxError as e:
    print("walrus in class-body comprehension", type(e).__name__)
