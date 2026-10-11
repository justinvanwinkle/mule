"""match capture, wildcard, dotted value patterns, group and OR patterns, AS patterns."""

import enum


class Color:
    RED = "red"
    GREEN = "green"
    BLUE = "blue"


class Shade(enum.Enum):
    LIGHT = 1
    DARK = 2


class Namespace:
    class Inner:
        VALUE = 99


def color_name(c):
    match c:
        case Color.RED:
            return "matched RED"
        case Color.GREEN | Color.BLUE:
            return "matched GREEN or BLUE"
        case Shade.LIGHT:
            return "light shade"
        case Shade.DARK:
            return "dark shade"
        case Namespace.Inner.VALUE:
            return "deep dotted value"
        case other:
            return f"captured {other!r}"


for c in ("red", "green", "blue", Shade.LIGHT, Shade.DARK, 99, 1, "purple"):
    print("color", repr(c) if not isinstance(c, Shade) else c.name, "->", color_name(c))

# Capture pattern always binds, even at module level
match [1, 2]:
    case whole:
        print("capture whole", whole)
print("binding survives match", whole)

# Wildcard does not bind
_ = "untouched"
match 5:
    case _:
        pass
print("wildcard did not bind", _)


# Value patterns are evaluated at match time
class Config:
    LIMIT = 10


def check(n):
    match n:
        case Config.LIMIT:
            return "at limit"
        case _:
            return "not at limit"


print("limit 10", check(10))
Config.LIMIT = 20
print("limit changed", check(10), check(20))


# OR patterns with bindings: every alternative binds the same names
def shape(point):
    match point:
        case (x, 0) | (0, x):
            return f"on an axis at {x}"
        case [x, y] | {"x": x, "y": y}:
            return f"point {x},{y}"
        case (1 | 2 | 3) as small:
            return f"small {small}"
        case _:
            return "unknown"


for p in ((5, 0), (0, 7), (0, 0), [3, 4], {"x": 1, "y": 2, "z": 3}, 2, 4):
    print("shape", p, "->", shape(p))


# Group patterns (parentheses) and nested OR
def grouped(v):
    match v:
        case (("a" | "b") | ("c")):
            return "abc"
        case ((1 | 2) as n):
            return f"num {n}"
        case [("x" | "y") as letter, *_]:
            return f"starts with {letter}"
        case _:
            return "none"


print("grouped", [grouped(v) for v in ("a", "c", 2, ["y", 0], ["z"], 5)])


# AS patterns binding sub-parts and the whole
def as_patterns(v):
    match v:
        case [int() as a, [str() as b, *rest] as inner] as outer:
            return a, b, rest, inner, outer
        case {"k": (1 | 2) as k} as mapping:
            return k, mapping
        case _ as anything:
            return "fallback", anything


print("as nested", as_patterns([1, ["s", 2, 3]]))
print("as mapping", as_patterns({"k": 2, "other": 0}))
print("as fallback", as_patterns(3.5))


# Cases are tried in order; the first structural match wins.
def first_match(v):
    match v:
        case [a, b, c]:
            return ("three", a, b, c)
        case [a, b]:
            return ("two", a, b)
        case [a]:
            return ("one", a)
    return ("none",)


print("first match", first_match([1]), first_match([1, 2]), first_match((1, 2, 3)), first_match([]))


# match subject is evaluated once
calls = []


def subject():
    calls.append("subject")
    return 3


match subject():
    case 1:
        pass
    case 2:
        pass
    case 3:
        print("matched 3, subject calls", calls)


# match with tuple subject without parentheses
a, b = 1, "x"
match a, b:
    case 1, "x":
        print("tuple subject matched")
