"""int methods: bit_length, bit_count, to_bytes, from_bytes, as_integer_ratio, is_integer, and attributes."""

for n in [0, 1, -1, 255, 256, -256, 2**64, -(2**100) - 1]:
    print("bits", n, n.bit_length(), n.bit_count())

print("to_bytes big", (1024).to_bytes(2, "big"), (1024).to_bytes(4, "little"))
print("to_bytes default", (65).to_bytes(), (0).to_bytes(3))
print("to_bytes signed", (-1).to_bytes(2, "big", signed=True), (-129).to_bytes(2, "little", signed=True))
print("to_bytes big int", (2**70 + 1).to_bytes(9, "big"))
try:
    (256).to_bytes(1, "big")
except OverflowError as e:
    print("to_bytes overflow", type(e).__name__)
try:
    (-1).to_bytes(1, "big")
except OverflowError as e:
    print("to_bytes neg unsigned", type(e).__name__)
print("to_bytes 128 signed", end=" ")
try:
    print((128).to_bytes(1, "big", signed=True))
except OverflowError as e:
    print(type(e).__name__)

print("from_bytes", int.from_bytes(b"\x01\x00", "big"), int.from_bytes(b"\x01\x00", "little"))
print("from_bytes signed", int.from_bytes(b"\xff\xff", "big", signed=True), int.from_bytes(b"\x80", signed=True))
print("from_bytes default", int.from_bytes(b"\x00\x10"), int.from_bytes(b""), int.from_bytes([1, 2, 3], "big"))
print("from_bytes bytearray", int.from_bytes(bytearray(b"\xde\xad\xbe\xef"), "big"))
n = 2**100 - 12345
print("roundtrip", int.from_bytes(n.to_bytes(13, "little"), "little") == n)

print("as_integer_ratio", (7).as_integer_ratio(), (-3).as_integer_ratio(), True.as_integer_ratio())
print("is_integer", (5).is_integer(), (0).is_integer())
print("numerator/denominator", (12).numerator, (12).denominator, (-4).real, (-4).imag)
print("conjugate", (9).conjugate(), type(True.conjugate()).__name__)
print("int.__index__", (42).__index__(), True.__index__())
print("abs/pos/neg methods", (-5).__abs__(), (5).__neg__(), (5).__pos__())
print("dunder arith", (5).__add__(3), (5).__radd__(3), (5).__sub__(1.5), (5).__truediv__(2), (7).__floordiv__(-2))
print("bit ops methods", (12).__and__(10), (12).__or__(10), (12).__xor__(10), (1).__lshift__(65), (5).__invert__())


class MyInt(int):
    def __repr__(self):
        return f"MyInt({int(self)})"


m = MyInt(10)
print("subclass", m, m + 1, type(m + 1).__name__, m.bit_length(), m.to_bytes(1, "big"))
print("from_bytes on subclass", MyInt.from_bytes(b"\x05", "big"), type(MyInt.from_bytes(b"\x05", "big")).__name__)
print("int() of subclass", int(m), type(int(m)).__name__)
