"""Exact hexadecimal rendering of binary64 storage, compared with Python 3.14."""

import struct

from test.mule.compiler.test_math_native import assert_matches_python


def test_float_hex_stored_values_cover_exponents_and_fraction_edges(tmp_path):
    words = []
    for exponent in range(0, 2048, 17):
        for fraction in (0, 1, (1 << 51), (1 << 52) - 1):
            word = (exponent << 52) | fraction
            words.extend((word, word | (1 << 63)))
    words.extend(
        (
            1,
            (1 << 52) - 1,
            1 << 52,
            0x7FEFFFFFFFFFFFFF,
            0x7FF0000000000000,
            0xFFF0000000000000,
            0x7FF0000000000001,
            0xFFF8000000000001,
        )
    )
    values = [struct.unpack(">d", word.to_bytes(8, "big"))[0] for word in words]
    # Build decimal inputs externally; float.hex itself must read stored bits.
    texts = "|".join(repr(value) for value in values)
    assert_matches_python(
        tmp_path,
        "for text in " + repr(texts) + ".split('|'):\n print(float(text).hex())\n",
    )


def test_float_hex_descriptor_uses_storage_without_subclass_hooks(tmp_path):
    assert_matches_python(
        tmp_path,
        """
class Trapped(float):
    def __float__(self): raise AssertionError('float conversion')
    def __int__(self): raise AssertionError('integer conversion')
    def __lt__(self, other): raise AssertionError('comparison')
    def __eq__(self, other): raise AssertionError('equality')
    def __str__(self): raise AssertionError('string conversion')
    def __repr__(self): raise AssertionError('representation')
    def __abs__(self): raise AssertionError('absolute value')
for text in ('0.0', '-0.0', '0.1', '-1.0', '5e-324', 'inf', '-inf', 'nan', '-nan'):
    value = Trapped(text)
    print(value.hex(), float.hex(value))
class Convertible:
    def __float__(self): raise AssertionError('unexpected conversion')
for value in (None, 1, True, '1', Convertible()):
    try:
        float.hex(value)
    except TypeError as error:
        print(str(error))
""",
    )


def test_float_integer_ratio_and_integrality_match_python(tmp_path):
    assert_matches_python(
        tmp_path,
        """
class Trapped(float):
    def __float__(self): raise AssertionError('float conversion')
    def __int__(self): raise AssertionError('integer conversion')
    def __eq__(self, other): raise AssertionError('equality')
    def __lt__(self, other): raise AssertionError('comparison')
for make in (float, Trapped):
    for text in ('0.0', '-0.0', '0.875', '-0.875', '2.1', '-2100', '5e-324',
                 '2.2250738585072014e-308', '1.7976931348623157e308',
                 '4503599627370495.5', '4503599627370496', 'inf', '-inf', 'nan'):
        value = make(text)
        print(value.is_integer())
        try:
            print(value.as_integer_ratio())
        except (ValueError, OverflowError) as error:
            print(type(error).__name__, str(error))
for method in (float.is_integer, float.as_integer_ratio):
    for value in (None, True, 1, '1'):
        try:
            method(value)
        except TypeError as error:
            print(str(error))
""",
    )
