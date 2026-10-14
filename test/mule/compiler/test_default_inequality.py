"""Default inequality delegates to the class's equality slot and descriptor."""

from test.mule.compiler.test_exception_groups import compare


def test_default_ne_uses_equality_slot_and_ignores_instance_hooks(tmp_path):
    compare(
        tmp_path,
        """
class Equal:
    def __eq__(self, other):
        return True
first, second = Equal(), Equal()
print(first == second, first != second, object.__ne__(first, second))
print(object.__ne__(None, None), object.__ne__(None, object()) is NotImplemented)
for first, second in [(None, None), (None, ''), ('', None), (None, 0), (0, None),
                      (1, 1), (1, 2), (False, 0), (b'a', b'a'), ('a', 'b')]:
    print(first == second, first != second)
value = float('nan')
print(value == value, value != value)
class Static:
    __eq__ = staticmethod(lambda other: True)
print(Static() != Static())
class Declines:
    def __eq__(self, other):
        return NotImplemented
first, second = Declines(), Declines()
print(object.__ne__(first, second) is NotImplemented, first != second, first != first)
class Hook(Equal):
    def __getattribute__(self, name):
        if name == '__eq__':
            raise AssertionError('ordinary lookup')
        return object.__getattribute__(self, name)
print(Hook() != Hook())
from pathlib import PurePosixPath
first, second = PurePosixPath('/tmp/same'), PurePosixPath('/tmp/same')
print(first == second, first != second)
""",
    )
