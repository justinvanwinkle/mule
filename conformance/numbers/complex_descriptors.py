"""Complex constructors, properties, descriptors, subclassing, and receiver checks."""

c1 = complex()
c2 = complex(3.5, -4.25)
c3 = complex("2.5+5.5j")
print("complex_construct:", c1.real, c1.imag, c2, c3)
print(
    "complex_ops:",
    c2 + c3,
    c2 - c3,
    c2 == complex(3.5, -4.25),
    c2 != c3,
    abs(complex(3.0, -4.0)),
)
print(
    "complex_props:",
    c2.real,
    c2.imag,
    c2.conjugate(),
    complex.conjugate(c2),
)


class MyComplex(complex):
    def norm_sq(self):
        return self.real * self.real + self.imag * self.imag


mc = MyComplex(1.5, 2.5)
print(
    "complex_subclass:",
    isinstance(mc, complex),
    mc.real,
    mc.imag,
    mc.conjugate(),
    complex.conjugate(mc),
    mc.norm_sq(),
)

try:
    complex("not_a_complex")
except ValueError as e:
    print("value_error:", type(e).__name__)

try:
    complex.conjugate(1.0)
except TypeError as e:
    print("receiver_error:", type(e).__name__)
