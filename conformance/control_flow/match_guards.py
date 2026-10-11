"""match guards with side effects and evaluation order across cases."""

log = []


def g(name, result):
    log.append(name)
    return result


def run(label, subject):
    log.clear()
    match subject:
        case int(n) if g(f"guard1 n={n}", n > 10):
            out = "big int"
        case int(n) if g(f"guard2 n={n}", n % 2 == 0):
            out = "even int"
        case str() if g("guard str", False):
            out = "never"
        case [x, *_] if g(f"guard list x={x}", x):
            out = "truthy-first list"
        case _ if g("guard wildcard", True):
            out = "wildcard"
    print(label, out, log)


run("big", 50)
run("even small", 4)
run("odd small", 3)
run("str", "s")
run("list truthy", [1, 2])
run("list falsy", [0, 2])
run("tuple empty", ())

# guards are only evaluated when the pattern matches
log.clear()
match "text":
    case int() if g("int guard", True):
        pass
    case str() if g("str guard", True):
        pass
print("guard only on structural match", log)


# guard sees bindings and can mutate state visible in later cases
state = {"tries": 0}


def bump():
    state["tries"] += 1
    return state["tries"]


match 7:
    case x if bump() > 5:
        print("never")
    case x if bump() > 5:
        print("never")
    case x if bump() == 3:
        print("third guard true, tries", state["tries"])

# binding happens before the guard; failed guard still leaves bindings set
match (1, 2):
    case (a, b) if a > b:
        print("never")
    case _:
        print("after failed guard a, b =", a, b)

# OR pattern with guard
for v in (1, 2, 3, 4):
    match v:
        case 1 | 2 | 3 as small if small % 2:
            print("odd small", small)
        case 1 | 2 | 3 | 4 as n:
            print("other", n)

# guard raising propagates out of match
try:
    match 0:
        case n if 1 / n:
            pass
except ZeroDivisionError as e:
    print("guard raised", type(e).__name__)

# guard with walrus binding visible after match
match [3, 4]:
    case [a, b] if (total := a + b) > 5:
        print("walrus in guard", total)
print("walrus total after match", total)


# guard returning non-bool truthy values
class Truthy:
    def __bool__(self):
        log.append("Truthy.__bool__")
        return True


log.clear()
match "x":
    case _ if Truthy():
        print("non-bool guard", log)

# guard order across many cases with the same pattern
log.clear()
match 5:
    case n if g("a", n < 0):
        pass
    case n if g("b", n == 0):
        pass
    case n if g("c", n < 10):
        print("third case", log)
    case n if g("d", True):
        pass

# guard referencing names bound in a nested pattern
match {"user": {"name": "ann", "age": 17}}:
    case {"user": {"age": age}} if age >= 18:
        print("adult")
    case {"user": {"name": name, "age": age}} if name.startswith("a"):
        print("minor with a-name", name, age)
