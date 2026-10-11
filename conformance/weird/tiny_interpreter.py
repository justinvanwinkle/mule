"""A tiny arithmetic interpreter: generator tokenizer, recursive-descent parser over dataclasses, match-based evaluator, custom errors."""

from __future__ import annotations

from dataclasses import dataclass, field


class CalcError(Exception):
    pass


class LexError(CalcError):
    pass


class ParseError(CalcError):
    pass


class EvalError(CalcError):
    pass


@dataclass(frozen=True)
class Tok:
    kind: str
    text: str
    pos: int


def tokenize(src):
    i = 0
    while i < len(src):
        ch = src[i]
        if ch.isspace():
            i += 1
            continue
        if ch.isdigit() or ch == ".":
            j = i
            while j < len(src) and (src[j].isdigit() or src[j] == "."):
                j += 1
            yield Tok("num", src[i:j], i)
            i = j
        elif ch.isalpha() or ch == "_":
            j = i
            while j < len(src) and (src[j].isalnum() or src[j] == "_"):
                j += 1
            yield Tok("name", src[i:j], i)
            i = j
        elif src.startswith("**", i):
            yield Tok("op", "**", i)
            i += 2
        elif ch in "+-*/%(),=":
            yield Tok("op", ch, i)
            i += 1
        else:
            raise LexError(f"bad char {ch!r} at {i}")
    yield Tok("eof", "", len(src))


@dataclass
class Num:
    value: float


@dataclass
class Var:
    name: str


@dataclass
class Unary:
    op: str
    operand: object


@dataclass
class Bin:
    op: str
    left: object
    right: object


@dataclass
class Call:
    func: str
    args: list = field(default_factory=list)


@dataclass
class Assign:
    name: str
    value: object


class Parser:
    def __init__(self, src):
        self.toks = list(tokenize(src))
        self.i = 0

    @property
    def cur(self):
        return self.toks[self.i]

    def eat(self, kind, text=None):
        t = self.cur
        if t.kind != kind or (text is not None and t.text != text):
            raise ParseError(f"expected {text or kind} at {t.pos}, got {t.text or 'EOF'!r}")
        self.i += 1
        return t

    def parse(self):
        node = self.statement()
        self.eat("eof")
        return node

    def statement(self):
        if self.cur.kind == "name" and self.toks[self.i + 1].text == "=":
            name = self.eat("name").text
            self.eat("op", "=")
            return Assign(name, self.statement())
        return self.expr()

    def expr(self):
        node = self.term()
        while self.cur.text in ("+", "-"):
            op = self.eat("op").text
            node = Bin(op, node, self.term())
        return node

    def term(self):
        node = self.unary()
        while self.cur.text in ("*", "/", "%"):
            op = self.eat("op").text
            node = Bin(op, node, self.unary())
        return node

    def unary(self):
        if self.cur.text in ("+", "-"):
            op = self.eat("op").text
            return Unary(op, self.unary())
        return self.power()

    def power(self):
        base = self.atom()
        if self.cur.text == "**":
            self.eat("op")
            return Bin("**", base, self.unary())
        return base

    def atom(self):
        t = self.cur
        match t:
            case Tok(kind="num", text=text):
                self.i += 1
                try:
                    return Num(float(text) if "." in text else int(text))
                except ValueError:
                    raise ParseError(f"bad number {text!r}") from None
            case Tok(kind="name", text=name) if self.toks[self.i + 1].text == "(":
                self.i += 2
                args = []
                if self.cur.text != ")":
                    args.append(self.expr())
                    while self.cur.text == ",":
                        self.eat("op")
                        args.append(self.expr())
                self.eat("op", ")")
                return Call(name, args)
            case Tok(kind="name", text=name):
                self.i += 1
                return Var(name)
            case Tok(text="("):
                self.i += 1
                node = self.expr()
                self.eat("op", ")")
                return node
            case _:
                raise ParseError(f"unexpected {t.text or 'EOF'!r} at {t.pos}")


FUNCS = {
    "max": max,
    "min": min,
    "abs": abs,
    "sum": lambda *a: sum(a),
}


def evaluate(node, env):
    match node:
        case Num(value=v):
            return v
        case Var(name=n):
            try:
                return env[n]
            except KeyError:
                raise EvalError(f"undefined {n}") from None
        case Unary("-", operand):
            return -evaluate(operand, env)
        case Unary("+", operand):
            return +evaluate(operand, env)
        case Bin("/" | "%" as op, left, right):
            l, r = evaluate(left, env), evaluate(right, env)
            if r == 0:
                raise EvalError(f"{op} by zero")
            return l / r if op == "/" else l % r
        case Bin(op, left, right):
            l, r = evaluate(left, env), evaluate(right, env)
            return {"+": l + r, "-": l - r, "*": l * r}.get(op) if op != "**" else l ** r
        case Call(func=f, args=args) if f in FUNCS:
            vals = [evaluate(a, env) for a in args]
            if not vals and f in ("max", "min"):
                raise EvalError(f"{f}() needs arguments")
            return FUNCS[f](*vals)
        case Call(func=f):
            raise EvalError(f"unknown function {f}")
        case Assign(name, value):
            env[name] = result = evaluate(value, env)
            return result
        case other:
            raise TypeError(f"bad node {other!r}")


programs = [
    "1 + 2 * 3",
    "(1 + 2) * 3",
    "-2 ** 2",
    "2 ** -1",
    "2 ** 3 ** 2",
    "x = y = 4",
    "x * y - 1",
    "max(1, x, 3) + min(7, -y)",
    "sum() + sum(1, 2, 3)",
    "10 / 4",
    "10 % 0",
    "7 % 3 + 1.5",
    "z + 1",
    "abs(-3",
    "1 + * 2",
    "3 $ 4",
    "1..2",
    "foo(1)",
    "max()",
    "",
    "--+-5",
]
env = {}
for src in programs:
    try:
        tree = Parser(src).parse()
        print(f"{src!r:32} => {evaluate(tree, env)!r}")
    except CalcError as e:
        print(f"{src!r:32} !! {type(e).__name__}: {e}")
print("final env", sorted(env.items()))
print("tree", Parser("a = -(1 + f(2, b))").parse())
