"""A small symbolic expression DSL built entirely from operator overloading, printed, simplified and evaluated."""


class Expr:
    precedence = 100

    # Binary operators build trees; constants are wrapped automatically.
    def __add__(self, o): return BinOp("+", self, wrap(o))
    def __radd__(self, o): return BinOp("+", wrap(o), self)
    def __sub__(self, o): return BinOp("-", self, wrap(o))
    def __rsub__(self, o): return BinOp("-", wrap(o), self)
    def __mul__(self, o): return BinOp("*", self, wrap(o))
    def __rmul__(self, o): return BinOp("*", wrap(o), self)
    def __truediv__(self, o): return BinOp("/", self, wrap(o))
    def __rtruediv__(self, o): return BinOp("/", wrap(o), self)
    def __pow__(self, o): return BinOp("**", self, wrap(o))
    def __rpow__(self, o): return BinOp("**", wrap(o), self)
    def __matmul__(self, o): return BinOp("@", self, wrap(o))
    def __neg__(self): return Neg(self)
    def __pos__(self): return self

    # Comparisons build predicate nodes instead of returning bools.
    def __lt__(self, o): return Cmp("<", self, wrap(o))
    def __gt__(self, o): return Cmp(">", self, wrap(o))
    def __eq__(self, o): return Cmp("==", self, wrap(o))
    def __ne__(self, o): return Cmp("!=", self, wrap(o))
    __hash__ = object.__hash__

    def __call__(self, **env):
        return self.eval(env)

    def __bool__(self):
        raise TypeError("symbolic expression has no truth value")


def wrap(v):
    return v if isinstance(v, Expr) else Const(v)


class Const(Expr):
    def __init__(self, v):
        self.v = v

    def __str__(self):
        return repr(self.v)

    def eval(self, env):
        return self.v

    def diff(self, var):
        return Const(0)


class Var(Expr):
    def __init__(self, name):
        self.name = name

    def __str__(self):
        return self.name

    def eval(self, env):
        return env[self.name]

    def diff(self, var):
        return Const(1 if var == self.name else 0)


OPS = {
    "+": (1, lambda a, b: a + b),
    "-": (1, lambda a, b: a - b),
    "*": (2, lambda a, b: a * b),
    "/": (2, lambda a, b: a / b),
    "@": (2, lambda a, b: sum(x * y for x, y in zip(a, b))),
    "**": (4, lambda a, b: a ** b),
}


class BinOp(Expr):
    def __init__(self, op, left, right):
        self.op, self.left, self.right = op, left, right
        self.precedence = OPS[op][0]

    def __str__(self):
        def part(e, right_side):
            s = str(e)
            p = e.precedence
            # Equal precedence needs parentheses on the right of - and /,
            # and on the left of the right-associative **.
            tight_side = not right_side if self.op == "**" else right_side
            needs = p < self.precedence or (p == self.precedence and tight_side and self.op in ("-", "/", "**"))
            return f"({s})" if needs else s

        return f"{part(self.left, False)} {self.op} {part(self.right, True)}"

    def eval(self, env):
        return OPS[self.op][1](self.left.eval(env), self.right.eval(env))

    def diff(self, v):
        l, r = self.left, self.right
        if self.op in "+-":
            return BinOp(self.op, l.diff(v), r.diff(v))
        if self.op == "*":
            return l.diff(v) * r + l * r.diff(v)
        if self.op == "**" and isinstance(r, Const):
            return r * l ** (r.v - 1) * l.diff(v)
        raise NotImplementedError(self.op)


class Neg(Expr):
    precedence = 3

    def __init__(self, e):
        self.e = e

    def __str__(self):
        s = str(self.e)
        return f"-({s})" if self.e.precedence < self.precedence else f"-{s}"

    def eval(self, env):
        return -self.e.eval(env)

    def diff(self, v):
        return Neg(self.e.diff(v))


class Cmp(Expr):
    precedence = 0

    def __init__(self, op, l, r):
        self.op, self.l, self.r = op, l, r

    def __str__(self):
        return f"{self.l} {self.op} {self.r}"

    def eval(self, env):
        a, b = self.l.eval(env), self.r.eval(env)
        return {"<": a < b, ">": a > b, "==": a == b, "!=": a != b}[self.op]


def simplify(e):
    if isinstance(e, BinOp):
        l, r = simplify(e.left), simplify(e.right)
        if isinstance(l, Const) and isinstance(r, Const):
            return Const(OPS[e.op][1](l.v, r.v))
        if e.op == "*":
            for a, b in ((l, r), (r, l)):
                if isinstance(a, Const) and a.v == 0:
                    return Const(0)
                if isinstance(a, Const) and a.v == 1:
                    return b
        if e.op in "+-" and isinstance(r, Const) and r.v == 0:
            return l
        if e.op == "+" and isinstance(l, Const) and l.v == 0:
            return r
        if e.op == "**" and isinstance(r, Const) and r.v == 1:
            return l
        return BinOp(e.op, l, r)
    if isinstance(e, Neg):
        inner = simplify(e.e)
        return Const(-inner.v) if isinstance(inner, Const) else Neg(inner)
    return e


x, y = Var("x"), Var("y")
exprs = {
    "linear": 3 * x + 2,
    "reflected": 10 - x / 2,
    "precedence": (x + 1) * (y - 2),
    "nested sub": x - (y - 1),
    "power": 2 ** x ** 2,
    "power left": (x ** 2) ** 3,
    "negation": -(x + y) * -x,
    "mixed": 1 / (x * y) + -x ** 2,
}
for label, e in exprs.items():
    print(f"{label:>10}: {e}  => {e(x=3, y=4)}")

dot = Var("u") @ Var("v")
print("matmul node", dot, dot(u=[1, 2, 3], v=[4, 5, 6]))

pred = x * 2 > y + 1
print("predicate", pred, pred(x=3, y=4), (x == y)(x=1, y=1), (x != 3)(x=3))
try:
    if x < y:
        pass
except TypeError as e:
    print("no truth value", e)

# Chained comparison calls __bool__ on the first result -> error
try:
    x < y < 3
except TypeError as e:
    print("chained comparison", type(e).__name__)

poly = 3 * x ** 2 + 2 * x + 1
d = poly.diff("x")
print("derivative raw", d)
print("derivative simplified", simplify(d), simplify(d)(x=5))
print("simplify consts", simplify((Const(2) + 3) * x * 1 + 0), simplify(-(Const(4) - 6)))

# Expressions are hashable by identity so they can key dicts even with symbolic __eq__
cache = {poly: "poly", d: "deriv"}
print("hash identity keys", cache[poly], cache[d], len(cache))
# `in` on a list uses __eq__ -> returns Cmp -> truthiness -> TypeError, except identity short-circuit
print("identity membership", poly in [poly])
try:
    poly in [d]
except TypeError as e:
    print("membership needs bool", type(e).__name__)

# Evaluation with float and big ints
big = x ** 40 + 1
print("big eval", big(x=3), (x / 3)(x=1.0))
print("env missing", end=" ")
try:
    (x + y)(x=1)
except KeyError as e:
    print(type(e).__name__, e.args)
