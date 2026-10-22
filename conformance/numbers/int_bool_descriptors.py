"""Int and bool constructors, descriptors, subclassing, and receiver checks."""

print("constructors:", int(), int(42), int(-3.9), int(" -123 "), int(b"456"))
print("bases:", int("ff", 16), int("0b1010", 0), int("1_000", 10), int("z", 36))
print(
    "bool_construct:",
    bool(),
    bool(0),
    bool(1),
    bool(""),
    bool("x"),
    bool([]),
    bool([0]),
)
print(
    "bool_is_int:",
    issubclass(bool, int),
    isinstance(True, int),
    isinstance(False, int),
    True == 1,
    False == 0,
    True + True,
    int(True),
    int(False),
)

for n in (0, 1, -1, 7, -8, 255, 1 << 100):
    print("bit_length:", n.bit_length(), int.bit_length(n))

print("bool_bit_length:", True.bit_length(), False.bit_length(), int.bit_length(True))
print(
    "properties:",
    (42).real,
    (42).imag,
    (42).numerator,
    (42).denominator,
    (42).conjugate(),
)

raw = (1024).to_bytes(2, "big")
print("to_from_bytes:", list(raw), int.from_bytes(raw, "big"))
neg_raw = (-5).to_bytes(2, "little", signed=True)
print(
    "signed_bytes:",
    list(neg_raw),
    int.from_bytes(neg_raw, "little", signed=True),
)

print(
    "int_descriptors:",
    int.__int__(19),
    int.__index__(19),
    int.__float__(4),
    int.__round__(19),
    int.__divmod__(17, 5),
    int.__repr__(19),
    int.__str__(19),
)
print(
    "bool_descriptors:",
    bool.__repr__(True),
    bool.__repr__(False),
    int.__repr__(True),
    int.__str__(True),
)


class TaggedInt(int):
    def label(self):
        return f"int:{int.__repr__(self)}"

    def __repr__(self):
        return f"TaggedInt({int.__repr__(self)})"


ti = TaggedInt(29)
print(
    "subclass:",
    isinstance(ti, int),
    ti + 1,
    ti.bit_length(),
    int.bit_length(ti),
    ti.label(),
    repr(ti),
    int.__repr__(ti),
)

for bad_call in (
    lambda: int("not_an_int"),
    lambda: int("10", 1),
    lambda: int("10", 37),
):
    try:
        bad_call()
    except ValueError as e:
        print("int_value_error:", type(e).__name__)

for bad_receiver in (
    lambda: int.bit_length("7"),
    lambda: int.__repr__("7"),
    lambda: int.__int__(3.5),
    lambda: bool.__repr__(1),
    lambda: bool.__repr__(0),
):
    try:
        bad_receiver()
    except TypeError as e:
        print("receiver_error:", type(e).__name__)
