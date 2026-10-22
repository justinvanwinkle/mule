"""List length descriptors and the sequence patterns that call them."""

import pytest

from test.mule.compiler.test_exception_groups import compare


def test_list_length_descriptor_reads_storage_and_validates_receiver(tmp_path):
    compare(
        tmp_path,
        """
class CustomList(list):
    def __len__(self):
        return 99
values = [10, 20]
print(values.__len__(), list.__len__(values))
values.append(30)
print(values.__len__(), list.__len__(values))
derived = CustomList([1, 2, 3, 4])
print(derived.__len__(), list.__len__(derived))
for wrong in ((), 'abc', object()):
    try:
        list.__len__(wrong)
    except TypeError as error:
        print(type(error).__name__, str(error))
""",
    )


@pytest.mark.integration
def test_sequence_patterns_read_materialized_lists_with_nested_class_patterns(tmp_path):
    compare(
        tmp_path,
        """
class Lit:
    __match_args__ = ('value',)
    def __init__(self, value):
        self.value = value
def choose(value):
    match value:
        case (1, Lit(left), Lit(right)):
            return left + right
        case [1, *middle, 9]:
            return middle
        case _:
            return None
print(choose((1, Lit(4), Lit(7))))
print(choose([1, Lit(6), Lit(8)]))
print(choose((1, 2, 3, 9)))
print(choose([1, 9]))
print(choose((1,)))
""",
    )
