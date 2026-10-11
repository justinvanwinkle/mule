"""Bitwise operators and shifts on negatives and big integers."""

vals = [0, 1, -1, 5, -5, 255, -256, 2**64, -(2**64), 2**100 + 12345]
for a in vals:
    print("invert", a, ~a)

for a, b in [(12, 10), (-12, 10), (12, -10), (-12, -10), (2**70 - 1, -(2**35)), (-(2**70), 2**69 + 7)]:
    print("and/or/xor", a, b, a & b, a | b, a ^ b)

print("shift left", 1 << 0, 1 << 63, 1 << 64, 1 << 200, -1 << 70, 3 << 2)
print("shift right", 1024 >> 3, -1024 >> 3, -1 >> 100, 1 >> 100, -7 >> 1, 7 >> 1)
print("big shift right", (2**100 + 5) >> 98, -(2**100) >> 99)
print("shift zero", 0 << 1000, 0 >> 1000)

try:
    print(1 << -1)
except ValueError as e:
    print("neg shift", type(e).__name__)
try:
    print(1 >> -1)
except ValueError as e:
    print("neg rshift", type(e).__name__)
try:
    print(1.0 << 1)
except TypeError as e:
    print("float shift", type(e).__name__)
try:
    print(1.5 & 1)
except TypeError as e:
    print("float and", type(e).__name__)

print("bool bitwise", True & False, True | False, True ^ True, type(True & True).__name__, True << 3)
print("mask low bits", (-1) & 0xFF, (-200) & 0xFF, (2**100 - 1) & 0xFFFF)
print("two's complement 8bit", [(x & 0xFF) for x in (-1, -128, 127, 128, -129)])
print("precedence", 1 | 2 ^ 3 & 4, (1 | 2) ^ 3, 1 + 2 << 3, 1 << 2 + 3, ~0 & 5)

x = 0b1100
x &= 0b1010
print("aug &=", bin(x))
x |= 0b0001
print("aug |=", bin(x))
x ^= 0b1111
print("aug ^=", bin(x))
x <<= 70
print("aug <<=", x)
x >>= 69
print("aug >>=", x)

popcount = 0
n = 2**100 - 1
while n:
    n &= n - 1
    popcount += 1
print("kernighan popcount", popcount)
