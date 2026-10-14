"""Public numeric conversion and mapping formatting use class protocols."""

from test.mule.compiler.test_exception_groups import compare


def test_float_int_conversion_preserves_storage_and_special_errors(tmp_path):
    compare(
        tmp_path,
        """
class Float(float):
    def __float__(self):
        return 999.0
for value in (0.0, -0.0, 2.75, -2.75, 1e100, Float(2.75), float('nan'), float('inf'), float('-inf')):
    try:
        print(int(value), float.__int__(value))
    except (ValueError, OverflowError) as error:
        print(type(error).__name__, str(error))
try:
    float.__int__(42)
except TypeError as error:
    print(type(error).__name__, str(error))
""",
    )


def test_percent_mapping_uses_view_and_custom_getitem_protocols(tmp_path):
    compare(
        tmp_path,
        """
class Object:
    value = 'class'
instance = Object()
instance.value = 'instance'
print('%(value)s' % instance.__dict__)
class Mapping:
    def __getitem__(self, key):
        print('lookup', key)
        return 'selected'
    def __str__(self):
        return 'mapping'
mapping = Mapping()
print('plain' % mapping)
print('%s' % mapping)
print('%(value)s' % mapping)
print('%s %(value)s' % mapping)
try:
    print('%(value)s %s' % mapping)
except TypeError as error:
    print(type(error).__name__, str(error))
""",
    )
