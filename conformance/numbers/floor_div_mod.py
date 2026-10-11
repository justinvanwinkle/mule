"""Floor division, modulo, and divmod with negative and float operands."""

pairs = [(7, 2), (-7, 2), (7, -2), (-7, -2), (0, 3), (0, -3), (6, 3), (-6, 3), (1, 5), (-1, 5)]
for a, b in pairs:
    print("int", a, b, a // b, a % b, divmod(a, b))
    q, r = divmod(a, b)
    print("  identity", q * b + r == a, "sign ok", r == 0 or (r > 0) == (b > 0))

fpairs = [(7.5, 2), (-7.5, 2), (7.5, -2), (-7.5, -2.0), (1, 0.3), (-1, 0.3), (0.0, 5), (-0.0, 5)]
for a, b in fpairs:
    print("float", a, b, a // b, a % b, divmod(a, b))

print("neg zero mod", -0.0 % 5, 0.0 % -5, -0.0 // 5)
print("float mod sign", 5.0 % -3, -5.0 % 3)
print("tiny mod", -1e-20 % 1.0)
print("inf mod", 5 % float("inf"), -5 % float("inf"), 5 // float("inf"), -5 // float("inf"))

for label, thunk in [
    ("int // 0", lambda: 1 // 0),
    ("int % 0", lambda: 1 % 0),
    ("divmod 0", lambda: divmod(1, 0)),
    ("float // 0", lambda: 1.0 // 0),
    ("float % 0.0", lambda: 1.0 % 0.0),
    ("int / 0", lambda: 1 / 0),
    ("big // 0", lambda: 2**100 // 0),
]:
    try:
        print(label, thunk())
    except ZeroDivisionError as e:
        print(label, "raised", type(e).__name__)

print("truediv", 7 / 2, -7 / 2, 6 / 3, type(6 / 3).__name__, 1 / 3)
print("truediv big exact", (10**30 + 1) / 10**30)
print("bool operands", True // 2, True % 2, divmod(True, True), 5 // True)

x = 17
x //= -5
print("augmented //=", x)
x %= 3
print("augmented %=", x)
y = -17.0
y //= 5
print("augmented float //=", y)
