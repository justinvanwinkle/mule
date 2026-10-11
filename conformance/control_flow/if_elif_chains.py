"""if/elif/else chains with side-effecting conditions and evaluation order."""

log = []


def cond(name, value):
    log.append(name)
    return value


def classify(n):
    log.clear()
    if cond("neg", n < 0):
        result = "negative"
    elif cond("zero", n == 0):
        result = "zero"
    elif cond("small", n < 10):
        result = "small"
    elif cond("medium", n < 100):
        result = "medium"
    else:
        result = "large"
    return result, list(log)


for n in (-5, 0, 3, 42, 1000):
    print("classify", n, classify(n))

# elif conditions that are only evaluated when earlier ones fail
counter = [0]


def bump():
    counter[0] += 1
    return counter[0]


if bump() > 5:
    print("unreachable first")
elif bump() == 2:
    print("second branch, counter", counter[0])
elif bump():
    print("unreachable third")
print("counter after chain", counter[0])

# if without else, nested if inside elif
for x in range(6):
    if x % 2 == 0:
        if x == 0:
            print("x", x, "zero even")
        elif x == 4:
            print("x", x, "four")
        else:
            print("x", x, "other even")
    elif x == 3:
        pass
    else:
        print("x", x, "odd not three")

# conditions returning non-bool objects
for value in ([], [0], "", "a", 0.0, -0.0, 1j, None, (), ((),)):
    if value:
        print("truthy", repr(value))
    else:
        print("falsy", repr(value))

# conditions with assignment and mutation inside
items = [3, 1, 2]
if items.sort() is None and items:
    print("sorted in condition", items)

# deeply nested chain with fallthrough
def grade(score):
    if score >= 90:
        if score >= 97:
            return "A+"
        elif score >= 93:
            return "A"
        return "A-"
    elif score >= 80:
        return "B"
    elif score >= 70:
        return "C"
    return "F"


print("grades", [grade(s) for s in (99, 95, 91, 85, 75, 10)])

# single-line if bodies and semicolon statements
if True: a = 1; b = 2
print("single line", a, b)
if False: c = 1
else: c = 2
print("single line else", c)

# if on a call that raises in a later elif is not reached
def boom():
    raise RuntimeError("boom")


if cond("ok", True):
    print("first taken, boom never evaluated")
elif boom():
    pass

try:
    if cond("f", False):
        pass
    elif boom():
        pass
except RuntimeError as e:
    print("raised in elif", e)
