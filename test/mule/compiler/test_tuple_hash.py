"""The tuple class exposes the hash operation for immutable physical storage."""

from test.mule.compiler.test_exception_groups import compare


def test_tuple_hash_method_is_visible_and_uses_physical_elements(tmp_path):
    compare(
        tmp_path,
        """
print(tuple.__hash__ is None, list.__hash__ is None)
for value in ((), (1,), (1, True, 2.5), ((1, 2), (3,)), (2 ** 100, -1)):
    print(tuple.__hash__(value), hash(value), tuple.__hash__(value) == hash(value))
class Tuple(tuple):
    def __iter__(self):
        raise AssertionError('iteration override')
    def __getitem__(self, index):
        raise AssertionError('index override')
    def __len__(self):
        raise AssertionError('length override')
print(tuple.__hash__(Tuple((1, 2, 3))))
try:
    tuple.__hash__(([],))
except TypeError as error:
    print(str(error))
""",
    )
