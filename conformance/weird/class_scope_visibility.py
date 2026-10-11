"""Class-body names seen (or not) from lambdas, comprehensions and generator expressions; the first-iterable rule."""


class A:
    xs = [1, 2, 3]
    factor = 10
    # first iterable is evaluated in class scope: OK
    copied = [x for x in xs]
    try:
        scaled = [x * factor for x in xs]
    except NameError as e:
        scaled_error = type(e).__name__
    # first iterable may reference class names, conditions may not
    try:
        filtered = [x for x in xs if x > factor]
    except NameError as e:
        filtered_error = type(e).__name__
    # nested loop's second iterable is evaluated inside the comprehension scope
    try:
        product = [(x, y) for x in xs for y in xs]
    except NameError as e:
        product_error = type(e).__name__
    gen = (x + 1 for x in xs)
    lazy_gen = (factor for _ in range(1))
    lam = lambda: factor
    lam_default = lambda f=factor: f


print("first iterable", A.copied)
print("scaled", getattr(A, "scaled", None), getattr(A, "scaled_error", None))
print("filtered", getattr(A, "filtered", None), getattr(A, "filtered_error", None))
print("product", getattr(A, "product", None), getattr(A, "product_error", None))
print("genexpr first iterable", list(A.gen))
try:
    list(A.lazy_gen)
except NameError as e:
    print("lazy genexpr body", type(e).__name__)
try:
    A.lam()
except NameError as e:
    print("lambda body", type(e).__name__)
print("lambda default captured", A.lam_default())

# A module-level global of the same name is what the comprehension body finds
factor = "global factor"


class B:
    factor = "class factor"
    seen = [factor for _ in range(1)]
    lam = staticmethod(lambda: factor)


print("comprehension body sees global", B.seen)
print("lambda sees global", B.lam())


# Inside a function, class body comprehension sees function locals but not class names
def make():
    factor = "function factor"

    class C:
        factor = "class factor"
        seen = [factor for _ in range(1)]
        direct = factor

    return C


C = make()
print("function local beats class", C.seen, C.direct)


# The first-iterable exception: errors in the first iterable are raised at creation time,
# errors in the rest of a generator expression are deferred until iteration.
def explode():
    raise ValueError("first iterable")


try:
    g = (x for x in explode())
except ValueError as e:
    print("genexpr first iterable evaluated eagerly", e)

g2 = (x for x in [1] for y in explode())
print("second iterable deferred: created ok")
try:
    next(g2)
except ValueError as e:
    print("raised on next", e)

try:
    g3 = (x for x in 5)
except TypeError as e:
    print("iter() on first iterable eager", type(e).__name__)


# Class body nonlocal / global interplay
counter = 0


class D:
    global counter
    counter += 1
    local_in_class = counter


print("global stmt in class body", counter, D.local_in_class)


def enclosing():
    v = "enclosing"

    class E:
        nonlocal v
        v = "rebound by class"

    return v


print("nonlocal in class body", enclosing())


# __class__ and super() inside a comprehension inside a method
class Base:
    def name(self):
        return "base"


class Derived(Base):
    def name(self):
        return [super().name() + str(i) for i in range(2)]


print("super in comprehension", Derived().name())


# Name lookup in class body falls back to globals, not to enclosing class
class Outer:
    tag = "outer"

    class Inner:
        try:
            got = tag
        except NameError as e:
            got = type(e).__name__


print("nested class cannot see outer class names", Outer.Inner.got)


# Class body sees a local defined later only after assignment
class Order:
    try:
        early = later
    except NameError as e:
        early = type(e).__name__
    later = 1


print("forward reference in class body", Order.early)
