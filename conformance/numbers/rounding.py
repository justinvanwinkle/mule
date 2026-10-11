"""round() with banker's rounding, ndigits, negative ndigits, and math rounding helpers."""

import math

for x in [0.5, 1.5, 2.5, 3.5, -0.5, -1.5, -2.5, 0.4999999, 2.675, 1.005]:
    print("round", x, round(x), type(round(x)).__name__)

print("round ndigits", round(2.675, 2), round(1.005, 2), round(0.125, 2), round(0.375, 2), round(3.14159, 3))
print("round ndigits 0", round(2.5, 0), type(round(2.5, 0)).__name__, round(-0.4, 0))
print("round negative ndigits float", round(1234.5, -1), round(1250.0, -2), round(1350.0, -2), round(-15.0, -1))
print("round int", round(7), round(-7), type(round(7, 2)).__name__)
print("round int negative ndigits", round(1234, -2), round(1250, -2), round(1350, -2), round(-25, -1), round(15, -1))
print("round int big", round(10**30 + 5 * 10**10, -11), round(5, -100))
print("round bool", round(True), type(round(True)).__name__)
print("round None ndigits", round(2.5, None), type(round(2.5, None)).__name__)
print("round huge ndigits", round(1.23456, 400), round(1.5, -400))
print("round neg zero", round(-0.4), round(-0.4, 1))

print("trunc/floor/ceil", math.trunc(-3.7), math.floor(-3.7), math.ceil(-3.2), math.floor(3.0), math.ceil(-0.5))
print("floor/ceil types", type(math.floor(2.5)).__name__, type(math.ceil(2.5)).__name__)
print("floor big float", math.floor(1e20))


class Money:
    def __init__(self, cents):
        self.cents = cents

    def __round__(self, ndigits=None):
        return ("rounded", self.cents, ndigits)


print("custom __round__", round(Money(150)), round(Money(150), 1))
try:
    round("1.5")
except TypeError as e:
    print("round str", type(e).__name__)
