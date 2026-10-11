"""Short-circuit and/or returning operands, side effects, and not."""

log = []


def v(name, value):
    log.append(name)
    return value


def show(label, value):
    print(label, repr(value), log)
    log.clear()


show("and both truthy", v("a", 1) and v("b", 2))
show("and first falsy", v("a", 0) and v("b", 2))
show("and returns falsy operand", v("a", []) and v("b", 2))
show("or first truthy", v("a", "x") or v("b", "y"))
show("or first falsy", v("a", "") or v("b", "y"))
show("or all falsy returns last", v("a", 0) or v("b", None) or v("c", ()))
show("and all truthy returns last", v("a", 1) and v("b", "s") and v("c", [0]))
show("mixed precedence", v("a", 0) or v("b", 1) and v("c", 2))
show("mixed precedence 2", v("a", 1) and v("b", 0) or v("c", 3))
show("parenthesized", (v("a", 0) or v("b", 1)) and v("c", 2))
show("not", not v("a", []))
show("not binds looser than ==", not v("a", 1) == v("b", 2))
show("not and", not v("a", 0) and v("b", 5))


class Truth:
    def __init__(self, name, value):
        self.name = name
        self.value = value

    def __bool__(self):
        log.append("bool " + self.name)
        return self.value

    def __repr__(self):
        return "Truth(" + self.name + ")"


# __bool__ called once per operand that needs testing; final operand never tested
show("bool calls and", Truth("a", True) and Truth("b", False))
show("bool calls or", Truth("a", False) or Truth("b", False))
show("bool in if", "taken" if (Truth("a", True) and Truth("b", True)) else "not")
show("not calls bool", not Truth("a", True))


# short circuit protects from errors
d = {}
show("guard missing key", "k" in d and d["k"] > 0)
xs = []
show("guard index", len(xs) > 0 and xs[0])
show("default via or", None or "default")
show("zero lost with or", 0 or 42)

# and/or in while conditions with side effects
items = [3, 2, 0, 5]
i = 0
while i < len(items) and v(f"check {i}", items[i]):
    i += 1
show("while stopped at", i)

# in comprehension conditions
show("comp", [x for x in range(6) if x % 2 and x > 1 or x == 0])

# operators returning non-bool operands of mixed types
print("types", type(0 or 0.0).__name__, type("" and 1).__name__, type([] or {}).__name__)

# chained with comparison results
show("comparisons", v("a", 1) < 2 and v("b", "z") or "never")


def raising():
    raise RuntimeError("should not run")


print("skip raising or", True or raising())
print("skip raising and", False and raising())
try:
    print(True and raising())
except RuntimeError as e:
    print("raised", e)

# augmented boolean idiom
x = None
x = x or []
x.append(1)
print("x or []", x)
