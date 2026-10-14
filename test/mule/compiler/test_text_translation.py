"""Translation table construction preserves Python's strings and validation."""

from test.mule.compiler.test_exception_groups import compare


def test_maketrans_arbitrary_values_deletion_unicode_and_validation(tmp_path):
    result = compare(
        tmp_path,
        """
table = str.maketrans({'a': '', 'b': 'several', 'c': None, 100: 0x1f600})
print(table)
print('abcdxyz'.translate(table))
print(str.maketrans('abc', 'XYZ', 'a!'))
print('a!bc'.translate(str.maketrans('abc', 'XYZ', 'a!')))
print(str.maketrans({'a': 1.0}))
class Key(str):
    def __len__(self): raise AssertionError('length')
    def __getitem__(self, key): raise AssertionError('index')
print(str.maketrans({Key('a'): Key('replacement')}))
print(str.maketrans(Key('ab'), Key('XY'), Key('a')))
class Table(dict): pass
for args in ((), (1, 2, 3, 4), ([],), (Table(a='x'),),
             ({'ab': 'x'},), ({1.0: 'x'},), (b'a', 'b'),
             ('a', b'b'), ('a', 'b', None), ('ab', 'x')):
    try:
        str.maketrans(*args)
    except (TypeError, ValueError) as error:
        print(type(error).__name__, str(error))
for operation in (lambda: 'a'.find(None), lambda: 'a'.encode(None),
                  lambda: format('a', None)):
    try:
        operation()
    except TypeError as error:
        print(str(error))
""",
    )
    assert result.stderr == ""
