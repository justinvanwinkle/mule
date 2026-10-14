"""Membership follows special methods, iteration and identity before equality."""

from test.mule.compiler.test_exception_groups import compare


def test_membership_in_views_generators_and_indexed_sequences(tmp_path):
    compare(
        tmp_path,
        """
mapping = {'first': 1, 'second': 2}
print(1 in mapping.values(), 3 not in mapping.values())
print('first' in mapping.keys(), ('first', 1) in mapping.items())
print(2 in (item for item in (1, 2, 3)))
class Indexed:
    def __getitem__(self, index):
        if index < 2:
            return index + 1
        raise IndexError
print(2 in Indexed(), 3 not in Indexed())
class Identity:
    def __eq__(self, other):
        raise ValueError('equality')
value = Identity()
print(value in (item for item in (value,)))
try:
    object() in (item for item in (value,))
except ValueError as error:
    print(str(error))
class Equality:
    def __eq__(self, other):
        raise StopIteration('comparison exhaustion')
try:
    object() in (item for item in (Equality(),))
except StopIteration as error:
    print(str(error))
""",
    )


def test_membership_methods_descriptors_and_disabled_protocols(tmp_path):
    compare(
        tmp_path,
        """
class Contains:
    @staticmethod
    def __contains__(item):
        return item == 2
    def __iter__(self):
        raise RuntimeError('must not iterate')
print(2 in Contains(), 3 not in Contains())
calls = []
class Iterator:
    def __init__(self):
        self.index = 0
    def __iter__(self):
        calls.append('iterator iter')
        return self
    def __next__(self):
        if self.index == 1:
            raise StopIteration
        self.index += 1
        return 2
class Descriptor:
    def __get__(self, instance, owner):
        calls.append('descriptor')
        return lambda: Iterator()
class Iterable:
    __iter__ = Descriptor()
print(2 in Iterable(), calls)
class DisabledContains:
    __contains__ = None
    def __iter__(self):
        return iter((2,))
class DisabledIter:
    __iter__ = None
    def __getitem__(self, index):
        return 2
for value in (DisabledContains(), DisabledIter(), 1, None):
    try:
        2 in value
    except TypeError as error:
        print(str(error))
class BrokenDescriptor:
    def __get__(self, instance, owner):
        raise AttributeError('descriptor failure')
class Broken:
    __contains__ = BrokenDescriptor()
    def __iter__(self):
        return iter((2,))
print(2 in Broken())
class MethodFailure:
    def __contains__(self, item):
        raise AttributeError('method failure')
    def __iter__(self):
        return iter((2,))
try:
    2 in MethodFailure()
except AttributeError as error:
    print(str(error))
""",
    )
