"""Numeric hashing preserves inherited values and explicit overrides."""

from test.mule.compiler.test_exception_groups import compare


def test_numeric_subclass_hash_matches_equal_builtin_keys_and_respects_overrides(
    tmp_path,
):
    compare(
        tmp_path,
        """
class Integer(int):
    pass
class Float(float):
    pass
class CustomInteger(int):
    def __hash__(self):
        return 12345
class CustomFloat(float):
    def __hash__(self):
        return 23456
for value in (Integer(1), Integer(-1), Integer(2 ** 100), Float(1.0), Float(1.25)):
    print(hash(value) == hash(int(value) if isinstance(value, int) else float(value)))
    print({value: 'first', int(value) if isinstance(value, int) else float(value): 'last'})
for value in (CustomInteger(1), CustomFloat(1.0)):
    print(hash(value), {value: 'present'}[value])
print(int.__hash__(Integer(1)), float.__hash__(Float(1.25)))
print({Integer(1): 'flag', True: 'bool'})
class ComparisonTrap(float):
    def __ne__(self, other):
        raise RuntimeError('comparison override must not run')
    def __float__(self):
        raise RuntimeError('conversion override must not run')
finite = ComparisonTrap(1.25)
print(float.__hash__(finite) == hash(1.25), hash(finite) == hash(1.25))
nan = ComparisonTrap(float('nan'))
print(float.__hash__(nan) == hash(nan))
""",
    )


def test_real_complex_multiplication_preserves_component_zero_signs(tmp_path):
    compare(
        tmp_path,
        """
for scalar in (0.0, -0.0, 2.0, -2.0, 3, -3):
    for value in (complex(-0.0, -2.0), complex(0.0, 2.0), complex(-3.0, -0.0)):
        left = scalar * value
        right = value * scalar
        print(left.real.hex(), left.imag.hex(), right.real.hex(), right.imag.hex())
""",
    )
