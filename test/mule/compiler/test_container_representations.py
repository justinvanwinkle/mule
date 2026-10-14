"""Compiled container representations use element protocols and recursion guards."""

from test.mule.compiler.test_exception_groups import compare


def test_container_representations_dispatch_and_clean_recursion_guards(tmp_path):
    compare(
        tmp_path,
        """
class Number(int):
    def __repr__(self):
        return 'number'
class Items(list):
    def __iter__(self):
        raise AssertionError('iteration override')
    def __getitem__(self, key):
        raise AssertionError('index override')
print([Number(3)], (Number(4),), Items([Number(5)]))
items = []
items.append(items)
print(items)
pair = (items,)
items.append(pair)
print(pair)
class Broken:
    def __repr__(self):
        raise ValueError('broken')
items = [Broken()]
try:
    repr(items)
except ValueError:
    print('caught')
items.clear()
print(items)
""",
    )
