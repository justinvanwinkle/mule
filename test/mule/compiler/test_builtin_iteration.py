"""Builtin sequence iterators bypass subclass indexing overrides."""

from test.mule.compiler.test_exception_groups import compare


def test_list_and_tuple_iterators_use_storage_and_stay_exhausted(tmp_path):
    compare(
        tmp_path,
        """
class Tuple(tuple):
    def __getitem__(self, key):
        raise AssertionError('index override called')
class List(list):
    def __getitem__(self, key):
        raise AssertionError('index override called')
for sequence in (Tuple((1, 2, 3)), List((4, 5, 6))):
    print(list(sequence), tuple(sequence))
    iterator = iter(sequence)
    print(next(iterator), next(iterator), next(iterator))
    print(next(iterator, 'finished'), next(iterator, 'still finished'))
sequence = List([1])
iterator = iter(sequence)
print(next(iterator))
sequence.append(2)
print(next(iterator))
print(next(iterator, 'finished'))
sequence.append(3)
print(next(iterator, 'still finished'))
""",
    )
