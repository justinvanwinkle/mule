"""format() builtin with numeric/string specs and custom __format__."""

print("default", format(42), format(3.5), format("s"), format(None), format([1, 2]))
print("width align", "[" + format("ab", "5") + "]", "[" + format("ab", ">5") + "]", "[" + format("ab", "^6") + "]")
print("fill", format("ab", "*^7"), format(5, "0>4"), format(-5, "05"), format(5, "+05"))
print("int types", format(255, "x"), format(255, "X"), format(255, "#x"), format(255, "o"), format(255, "#b"), format(65, "c"))
print("grouping", format(1234567, ","), format(1234567, "_"), format(0xFFFFFFFF, "_x"), format(1234567.891, ",.2f"))
print("sign", format(5, "+"), format(5, " "), format(-5, " "), format(5, "-"))
print("float", format(3.14159, ".2f"), format(3.14159, ".3e"), format(1234.5, "g"), format(0.00001234, "g"))
print("percent", format(0.256, ".1%"), format(1, "%"))
print("float special", format(float("inf"), "f"), format(float("-inf"), "+.2f"), format(float("nan"), "E"))
print("negative zero", format(-0.0, ".1f"), format(-0.0, "z.1f"), format(-0.001, "z.1f"))
print("repr g", format(1e16), format(1e-5), format(123456789.0))
print("precision string", format("abcdef", ".3"), format("abcdef", "^9.3"))
print("equals align", format(-42, "=8"), format(-42, "0=8"))
print("int as float", format(7, ".2f"), format(10**20, "e"))
print("bool", format(True), format(True, "d"), format(False, "^7"))
print("complex", format(1 + 2j), format(1.5 - 2j, ".1f"))
print("nested width in fstring", f"{3.14159:{'.'}{3}}", f"{'x':{'>'}{4}}")

for spec_value, spec in [("abc", "d"), (1.5, "d"), (5, "s"), (object, "x")]:
    try:
        format(spec_value, spec)
    except (ValueError, TypeError) as e:
        print("bad spec", spec, type(e).__name__)


class Temp:
    def __init__(self, c):
        self.c = c

    def __format__(self, spec):
        if spec == "F":
            return f"{self.c * 9 / 5 + 32:.1f}F"
        return f"{self.c:{spec}}C"


t = Temp(21.5)
print("custom", format(t), format(t, "F"), format(t, ">8"), f"{t:F}", f"[{t:<7}]")


class BadFormat:
    def __format__(self, spec):
        return 5


try:
    format(BadFormat())
except TypeError as e:
    print("format returns non str", type(e).__name__)


class NoFormat:
    def __str__(self):
        return "str-used"


print("object format empty spec", format(NoFormat()))
try:
    format(NoFormat(), "10")
except TypeError as e:
    print("object non empty spec", type(e).__name__)
