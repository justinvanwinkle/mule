"""Complex arithmetic, attributes, repr, and comparisons."""

a = 3 + 4j
b = 1 - 2j
print("repr", a, b, 1j, -1j, 0j, complex(0, -0.0), complex(-0.0, 0), 1 + 0j, 2.5j)
print("attrs", a.real, a.imag, type(a.real).__name__, a.conjugate(), abs(a))
print("int attrs", (5).real, (5).imag, (5).conjugate(), (2.5).imag, (2.5).conjugate(), True.imag)
print("add/sub", a + b, a - b, a + 1, 1 - a, a + 0.5)
print("mul/div", a * b, a / b, a * 2, 2 / a, a / 2)
print("pow", a ** 2, 1j ** 2, (1j) ** 0, a ** 0.5, pow(1j, 3))
print("neg/pos", -a, +a, -(0j))
print("eq", a == complex(3, 4), 1 + 0j == 1, 1j == 1, (2 + 0j) == 2.0)
print("hash eq", hash(1 + 0j) == hash(1), hash(2.0 + 0j) == hash(2))
try:
    print(a < b)
except TypeError as e:
    print("ordering", type(e).__name__)
try:
    print(a // b)
except TypeError as e:
    print("floordiv", type(e).__name__)
try:
    print(a % 2)
except TypeError as e:
    print("mod", type(e).__name__)
try:
    print(a / 0)
except ZeroDivisionError as e:
    print("div zero", type(e).__name__)
print("bool", bool(0j), bool(1j), bool(complex(0, 0.0)))
print("infinite parts", complex(float("inf"), 1), complex(1, float("nan")))
print("abs", abs(-3j), abs(1 + 1j))
print("str vs repr", str(1 + 2j), repr(1.5 - 0.5j), f"{a}", f"{a!r}")
print("format spec", format(a, ".2f"), format(1.5 - 2.25j, "^20"), f"{b:+.1e}")
z = 0j
for k in range(4):
    z += 1j ** k
print("sum of powers", z)
print("in containers", [1j, 2 + 0j], {1j: "i"}[1j], 2 + 0j in [2])
print("sum complex", sum([1j, 2, 3.5]))
print("complex mixing", type(1j * 1).__name__, type(True * 1j).__name__, True + 1j)
