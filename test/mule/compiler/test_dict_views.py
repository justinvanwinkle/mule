"""Set protocols on live dictionary keys and items."""

from test.mule.compiler.test_exception_groups import compare


def test_dict_view_set_operators_reflection_and_membership(tmp_path):
    compare(
        tmp_path,
        """
mapping = {'a': 1, 'b': 2}
keys = mapping.keys()
other = ['b', 'c']
print(sorted(keys | other), sorted(other | keys))
print(sorted(keys & other), sorted(other & keys))
print(sorted(keys - other), sorted(other - keys))
print(sorted(keys ^ other), sorted(other ^ keys))
print(sorted({'c'} | keys), sorted({'c'} - keys))
print(keys == {'a', 'b'}, {'a', 'b'} == keys, keys != {'a'})
print(keys <= {'a', 'b', 'c'}, {'a'} < keys, keys >= {'a'}, {'a'} <= keys)
print(keys.isdisjoint(['c']), keys.isdisjoint(['b']))
mapping['c'] = 3
print(sorted(keys - ['b']))
items = mapping.items()
for candidate in (('a', 1), ['a', 1], ('a',), 'ab', 42):
    print(candidate in items)
unhashable = {'key': [1]}.items()
print(unhashable == {'key': [1]}.items(), unhashable & [])
""",
    )
