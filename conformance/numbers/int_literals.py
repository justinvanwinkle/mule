"""Integer literals: decimal, hex, octal, binary, underscores, and huge literals."""

print("decimal", 0, 7, 1234567890)
print("hex", 0x1F, 0XfF, 0x_dead_beef, 0x0)
print("octal", 0o17, 0O777, 0o_1_2)
print("binary", 0b1010, 0B1111_0000, 0b_1)
print("underscores", 1_000_000, 1_2_3_4, 0_0)
print("zeros", 0, 00, 0_0_0, 000)
print("negative literals", -0x10, -0o10, -0b10, -10)

big = 123_456_789_012_345_678_901_234_567_890
print("big decimal", big)
print("big hex", 0xFFFF_FFFF_FFFF_FFFF_FFFF_FFFF)
print("big binary", 0b1 << 100 == 0b1_0000000000_0000000000_0000000000_0000000000_0000000000_0000000000_0000000000_0000000000_0000000000_0000000000)
print("big octal", 0o7777777777777777777777777777)

print("type of big", type(big).__name__)
print("2**63", 9223372036854775808, 2**63)
print("2**64-1", 18446744073709551615 == 2**64 - 1)
print("-2**63", -9223372036854775808, -(2**63))

# int attribute access on literal requires a space or parens
print("attr access", 5 .bit_length(), (255).bit_length(), 0x10 .real)

# float literals adjacent to int forms
print("float literals", 1_0.5_0, 1e1_0, .5, 5., 0e0, 1_000.000_1)
print("float exp", 1E3, 1e-3, 2.5e+2)
print("imag literals", 3j, 1_0j, 1.5e2j, 0j)

values = [0x1F, 0o37, 0b11111, 31]
print("all equal", all(v == 31 for v in values), values)

print("bin/oct/hex builtins", bin(10), oct(10), hex(255), bin(-10), oct(-8), hex(-255))
print("bin big", bin(2**70))
print("hex 0", hex(0), bin(0), oct(0))
