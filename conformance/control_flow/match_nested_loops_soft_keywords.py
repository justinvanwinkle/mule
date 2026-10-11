"""Nested match statements, match inside loops with break/continue, custom subjects, soft keywords."""


class Event:
    __match_args__ = ("kind", "payload")

    def __init__(self, kind, payload=None):
        self.kind = kind
        self.payload = payload

    def __repr__(self):
        return f"Event({self.kind!r})"


events = [
    Event("key", "a"),
    Event("noise"),
    Event("key", "b"),
    Event("mouse", (3, 4)),
    Event("mouse", (0, 0)),
    Event("key", "q"),
    Event("key", "never-processed"),
]

typed = []
for ev in events:
    match ev:
        case Event("key", "q"):
            print("quit seen, breaking")
            break
        case Event("key", ch):
            typed.append(ch)
        case Event("mouse", (x, y)):
            match (x, y):
                case (0, 0):
                    print("mouse at origin, continue")
                    continue
                case (x, y) if x > y:
                    print("mouse below diagonal")
                case _:
                    print("mouse elsewhere", x, y)
        case Event(kind):
            print("ignored event", kind)
            continue
    print("processed", ev)
else:
    print("never: loop broke")
print("typed", typed)


# match inside while with break in a nested match
def tokenize(text):
    tokens = []
    i = 0
    while i < len(text):
        ch = text[i]
        match ch:
            case " ":
                i += 1
                continue
            case "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9":
                j = i
                while j < len(text):
                    match text[j]:
                        case d if d.isdigit():
                            j += 1
                        case _:
                            break
                tokens.append(("num", int(text[i:j])))
                i = j
                continue
            case "+" | "-" | "*" as op:
                tokens.append(("op", op))
            case "#":
                break
            case other:
                tokens.append(("unknown", other))
        i += 1
    return tokens


print("tokenize", tokenize("12 + 3*45 ?# ignored"))


# match in a function returning from inside cases
def evaluate(expr):
    match expr:
        case ("+", left, right):
            return evaluate(left) + evaluate(right)
        case ("*", left, right):
            return evaluate(left) * evaluate(right)
        case ("neg", inner):
            return -evaluate(inner)
        case int() | float() as number:
            return number
        case _:
            raise ValueError(f"bad expression {expr!r}")


print("evaluate", evaluate(("+", 1, ("*", 2, ("neg", 3)))))
try:
    evaluate(("/", 1, 2))
except ValueError as e:
    print("bad expr", e)

# match and case as ordinary identifiers
match = "match is a name"
case = ["case", "is", "a", "name"]
print(match, case)
match match:
    case "match is a name":
        print("matched on variable named match")
match case:
    case [*words]:
        print("matched on variable named case", len(words))


def type(x):  # noqa: shadowing a builtin name is also legal; restored below
    return "shadowed"


print("shadowed type", type(1))
del type
print("restored type", type(1).__name__)


class match_holder:
    match = 1
    case = 2


print("attributes named match/case", match_holder.match, match_holder.case)
_ = [match for match in range(3)]
print("comprehension var named match", _)


def f(match, case=None):
    return match, case


print("parameters named match/case", f(case=1, match=2))

# soft keyword `_` in expression context is an ordinary name
_ = "underscore"
match 0:
    case _:
        print("case _ wildcard; _ still", _)

# match statement subject is a call named match
def match_(x):
    return x


match match_(1),:
    case (1,):
        print("tuple subject with trailing comma")
