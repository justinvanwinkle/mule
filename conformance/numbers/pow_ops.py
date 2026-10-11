"""Exponentiation: **, pow() two- and three-arg, negative and float exponents."""

print("int pow", 2**10, (-2)**3, (-2)**4, 0**0, 1**1000, (-1)**1001)
print("neg exponent", 2**-1, 2**-3, (-2)**-2, 10**-1)
print("type of neg exp", type(2**-1).__name__, type(2**0).__name__)
print("float pow", 2.0**10, 4**0.5, 2**0.5, 8**(1 / 3))
print("big pow", 3**80, 7**-0 == 1)
print("pow builtin", pow(2, 10), pow(2, -2), pow(2.5, 2))
print("pow 3-arg", pow(3, 4, 5), pow(2, 100, 1000), pow(10**20, 10**5, 97))
print("pow 3-arg negative base", pow(-3, 3, 7), pow(-2, 5, 10))
print("pow 3-arg negative mod", pow(3, 3, -7), pow(2, 5, -3))
print("pow modular inverse", pow(3, -1, 7), pow(38, -1, 97), pow(2, -2, 9))
print("pow mod 1", pow(5, 3, 1), pow(0, 0, 7))
print("zero to neg float", end=" ")
try:
    print(0.0 ** -1)
except ZeroDivisionError as e:
    print(type(e).__name__)
try:
    print("0 ** -1", 0 ** -1)
except ZeroDivisionError as e:
    print("0 ** -1", type(e).__name__)
try:
    print(pow(2, -1, 4))
except ValueError as e:
    print("no inverse", type(e).__name__)
try:
    print(pow(2.0, 3, 5))
except TypeError as e:
    print("float 3-arg", type(e).__name__)
try:
    print(pow(2, 3, 0))
except ValueError as e:
    print("mod zero", type(e).__name__)

r = (-8) ** (1 / 3)
print("neg base frac exp is complex", type(r).__name__, round(r.real, 6), round(r.imag, 6))
print("pow(-8.0, 2.0)", pow(-8.0, 2.0))
print("pow float underflow", 0.5 ** 2000, (-0.5) ** 2001)
try:
    print(10.0 ** 400)
except OverflowError as e:
    print("float overflow", type(e).__name__)
print("int pow huge", len(str(2**10000)))
print("1.0 ** nan", 1.0 ** float("nan"), float("nan") ** 0)
print("inf pow", float("inf") ** -1, float("-inf") ** 3, float("-inf") ** 2)

x = 3
x **= 4
print("augmented **=", x)
x **= -1
print("augmented **= neg", x)

print("pow bool", True ** 5, 2 ** True, False ** 0)
print("pow keyword", pow(base=2, exp=5), pow(base=2, exp=5, mod=3))
