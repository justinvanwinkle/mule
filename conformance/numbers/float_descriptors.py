"""Float constructors, methods, descriptors, subclassing, and receiver checks."""

print("float_construct:", float(), float(7), float("-3.25"), float(" 1.5e2 "), float(b"-0.5"))
print("float_ops:", 1.5 + 2.25, 5.0 - 1.5, 2.5 * 4.0, 7.0 / 2.0, 7.5 // 2.0, 7.5 % 2.0)
print(
    "float_methods:",
    (2.0).is_integer(),
    (2.5).is_integer(),
    (-3.0).is_integer(),
    (0.75).as_integer_ratio(),
    (-2.5).as_integer_ratio(),
    (1.5).hex(),
    (0.0).hex(),
    (3.5).real,
    (3.5).imag,
    (3.5).conjugate(),
)
print(
    "float_descriptors:",
    float.__int__(3.9),
    float.__int__(-3.9),
    float.__float__(2.5),
    float.__round__(3.6),
    float.__round__(3.14159, 2),
    float.__divmod__(7.5, 2.0),
    float.is_integer(4.0),
    float.is_integer(4.5),
    float.as_integer_ratio(1.25),
    float.hex(1.5),
    float.__repr__(1.5),
    float.__str__(1.5),
)


class MyFloat(float):
    def doubled(self):
        return float(self) * 2.0


mf = MyFloat(1.25)
print(
    "float_subclass:",
    isinstance(mf, float),
    mf.doubled(),
    mf.is_integer(),
    float.is_integer(mf),
    float.as_integer_ratio(mf),
    float.hex(mf),
)

try:
    float("not_a_float")
except ValueError as e:
    print("value_error:", type(e).__name__)

for bad_receiver in (
    lambda: float.hex(1),
    lambda: float.is_integer(1),
    lambda: float.as_integer_ratio(1),
    lambda: float.__int__("1.0"),
):
    try:
        bad_receiver()
    except TypeError as e:
        print("receiver_error:", type(e).__name__)
