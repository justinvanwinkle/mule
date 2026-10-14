"""Mutable list allocation does not consume subclass initializer arguments."""

from test.mule.compiler.test_exception_groups import compare


def test_allocator_and_initializer_have_separate_iterable_responsibilities(tmp_path):
    compare(
        tmp_path,
        """
print(list.__new__(list, 42, 43, keyword=7))
class Custom(list):
    def __init__(self, value, *, name):
        self.value = value
        self.name = name
custom = Custom(42, name='hello')
print(custom, custom.value, custom.name)
class Initialized(list):
    def __init__(self, count):
        super().__init__(range(count))
print(Initialized(3))
values = [1, 2]
values.__init__(values)
print(values)
values = [1, 2]
values.__init__(iter(values))
print(values)
values.__init__([3, 4])
print(values)
values.__init__()
print(values)
class Broken:
    def __iter__(self):
        yield 7
        raise ValueError('iteration failed')
values = [1, 2]
try:
    values.__init__(Broken())
except ValueError:
    print(values)
try:
    values.__init__(1, 2)
except TypeError:
    print('argument count', values)
""",
    )
