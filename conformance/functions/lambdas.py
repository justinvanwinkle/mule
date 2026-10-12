"""Lambdas: immediate invocation, defaults, nesting, currying, *args/**kw, and lambdas in data structures."""

print("immediate", (lambda x, y: x * y)(6, 7))
print("immediate no args", (lambda: "hi")())
print("conditional body", (lambda n: "neg" if n < 0 else "nonneg")(-1))

add = lambda a, b=10: a + b
print("default", add(1), add(1, 2))

curried = lambda a: lambda b: lambda c: a + b + c
print("curried", curried(1)(2)(3))
step = curried("x")
print("partial curried", step("y")("z"), step("Y")("Z"))

compose = lambda f, g: lambda x: f(g(x))
inc = lambda x: x + 1
dbl = lambda x: x * 2
print("compose", compose(inc, dbl)(5), compose(dbl, inc)(5))

varargs = lambda *a, **k: (a, sorted(k.items()))
print("varargs", varargs(1, 2, z=3, y=4))
kwonly = lambda *, k: k
print("kwonly", kwonly(k="K"))
posonly = lambda a, /, b: (a, b)
print("posonly", posonly(1, b=2))


def with_lambda_default(key=lambda v: -v):
    return sorted([3, 1, 2], key=key)


print("lambda as default", with_lambda_default(), with_lambda_default(lambda v: v))

table = {
    "+": lambda a, b: a + b,
    "-": lambda a, b: a - b,
    "*": lambda a, b: a * b,
}
print("dispatch", [table[op](7, 3) for op in "+-*"])

print("sort key", sorted(["bb", "a", "ccc"], key=lambda s: (-len(s), s)))
print("map", list(map(lambda x: x ** 2, range(5))))
print("filter", list(filter(lambda x: x % 2, range(10))))

print("lambda name", (lambda: 0).__name__)
print("lambda returns tuple", (lambda: (1, 2))())

# Lambda closures over loop variable: late binding
late = [lambda: i for i in range(3)]
print("late binding", [fn() for fn in late])

# Lambda containing a walrus and comprehension
print("walrus in lambda", (lambda xs: [y for x in xs if (y := x * 3) > 3])([1, 2, 3]))

# Recursive lambda via name lookup at call time
fact = lambda n: 1 if n <= 1 else n * fact(n - 1)
print("recursive lambda", fact(10))

# Y-combinator style recursion without names
Y = lambda f: (lambda x: f(lambda v: x(x)(v)))(lambda x: f(lambda v: x(x)(v)))
fib = Y(lambda self: lambda n: n if n < 2 else self(n - 1) + self(n - 2))
print("Y combinator fib", [fib(i) for i in range(10)])

# Lambda in class body sees class name only through module scope
class K:
    scale = 3
    f = lambda self, x: x * self.scale


print("lambda method", K().f(4))

# Lambda default evaluated at lambda creation time
n = 5
capture = lambda n=n: n
n = 6
print("lambda default captured", capture(), n)

# Nested lambdas share outer parameters
make_pair = lambda a: (lambda: a, lambda: a * 2)
p, q = make_pair(21)
print("sibling lambdas", p(), q())

# Lambda with yield is a generator function
gen_lambda = lambda: (yield 1)
print("lambda generator", list(gen_lambda()))

# Lambda with unpacked call
print("lambda star call", (lambda *a: sum(a))(*range(5)))
