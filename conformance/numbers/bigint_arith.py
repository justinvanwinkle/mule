"""Arbitrary-precision integers and their mixing with machine-sized ints."""

i64_max = 2**63 - 1
i64_min = -(2**63)

print("overflow add", i64_max + 1)
print("overflow sub", i64_min - 1)
print("overflow mul", i64_max * 2)
print("overflow neg", -i64_min)
print("abs min", abs(i64_min))
print("mul min by -1", i64_min * -1)
print("floordiv min by -1", i64_min // -1)
print("mod min by -1", i64_min % -1)
print("back to small", (i64_max + 1) - 1 == i64_max, type((i64_max + 1) - 1).__name__)

f = 1
for n in range(1, 31):
    f *= n
print("30!", f)
print("30! // 29!", f // (f // 30))
print("digits of 2**200", len(str(2**200)))
print("2**200", 2**200)
print("big minus big", 10**30 - (10**30 - 1))
print("big mixed", 10**25 + 7, 7 + 10**25, 10**25 * -3)
print("big floordiv", 10**30 // 7, -(10**30) // 7)
print("big mod", 10**30 % 7, -(10**30) % 7, 10**30 % -7)
print("big divmod", divmod(10**40, 12345678901234567))
print("big truediv", 10**20 / 4, (2**64) / 2)
print("big eq", 2**64 == 18446744073709551616, 2**64 != 2**64 + 1)
print("big order", 2**64 > 2**63, -(2**64) < -(2**63), sorted([2**65, -5, 2**64, 0]))
print("big neg", -(2**100), abs(-(2**100)))
print("big bool", bool(2**100), bool(2**100 - 2**100))
print("big str roundtrip", int(str(3**100)) == 3**100)

acc = 0
for k in range(100):
    acc += k * 2**60
print("accumulate", acc)

fib = [0, 1]
while len(fib) < 120:
    fib.append(fib[-1] + fib[-2])
print("fib 119", fib[119])

print("big in containers", [2**70, 2**70 + 1].index(2**70 + 1), {2**70: "a"}[2**70])
print("big max/min", max(2**80, 3, -(2**90)), min(2**80, 3, -(2**90)))
print("sum big", sum([2**62, 2**62, 2**62]))
print("big gcd-ish", (2**100) % (2**50 + 1))
