"""Soft keywords match, case, type and _ used as ordinary names alongside real match statements and type aliases."""

match = [1, 2, 3]
case = {"k": "v"}
type = "shadowed type"
_ = "underscore"
print("as variables", match, case, type, _)
del type  # restore builtins.type lookup


def match(case, type=None):
    return ("match()", case, type)


print("as function and params", match(1, type=2))
print("call with keyword case", match(case="c"))

match (1, 2):
    case (a, b):
        print("match statement after function named match", a, b)


class Obj:
    match = "attr match"
    case = "attr case"
    type = "attr type"

    def _(self):
        return "method named _"


o = Obj()
print("attributes", o.match, o.case, o.type, o._())

# A match statement whose subject is a variable named match
match = {"case": "inner", "type": 3}
match match:
    case {"case": case, "type": int(type)}:
        print("captured case and type", case, type)
del type

# _ is a wildcard in patterns, not a binding
_ = "outer underscore"
match 99:
    case _:
        print("wildcard does not bind", _)
match [5, 6]:
    case [first, *_]:
        print("star wildcard", first, _)
match 7:
    case _ if _ == "outer underscore":
        print("guard sees the variable _", _)

# Expressions that start with the word match are not match statements
match = [10, 20]
match[0] = 11
print("subscript assignment", match)
try:
    match (0)
except TypeError as e:
    print("`match (0)` without a colon is a call expression", type(e).__name__)
match = lambda *a: a
print("call", match(1, 2))
match = 3
print("arithmetic", match * 2, -match)

# type statement next to a variable named type
type Pair[T] = tuple[T, T]
type = "variable again"
print("alias and variable coexist", Pair.__name__, type)
del type
type Alias = list[int]
print("alias value", Alias.__value__)
type = 5
print("type as int", type + 1)
del type
print("builtin type works", type(1).__name__)

# case as a name used inside a case block
match ("x", "y"):
    case (case, _):
        print("case as capture name", case)

# match inside a function whose locals are named match/case
def weird(match, case):
    match match:
        case "a":
            return "matched a with case=" + case
        case _:
            return f"other {match} {case}"


print(weird("a", "C"), weird("b", "D"))

# lambda parameter names
f = lambda match, case, _: (match, case, _)
print("lambda params", f(1, 2, 3))

# Keyword argument names
print("kwargs", dict(match=1, case=2, type=3, _=4))

# Comprehension variables
print("comprehension", [match for match in range(3)], [case * 2 for case in "ab"])
