"""Incremental format syntax and lazy traversal match Python 3.14."""

import pytest

from test.mule.compiler.test_exception_groups import compare


def test_format_and_field_iterators_match_python(tmp_path):
    compare(
        tmp_path,
        """
import _string
for text in ('', 'abc', '{{abc}}', '{{}}{}', 'a{x!r:>{width}}z',
             '{x[a:b!c]}', '{x[y{z]}', '{x!xx}', '{x!}', '{x:{{}}}',
             '{x:{y:{z}}}', '{a{b}}', '{a[b}', '{a[b]c}', '{a:foo!r}',
             '}}}}', '{0!z}', '{', '{x', '{x!', '{x!:}', '{x!r',
             '{x!r}', '{x:', '{x:{', '{x:{}}', '{x[}', '{x[]}',
             '{x]y}', '{x[a]z}', '{x[[]}', '{x!{a}', '{x!}}'):
    iterator = _string.formatter_parser(text)
    print('created', repr(text), iter(iterator) is iterator)
    try:
        print(list(iterator))
    except ValueError as error:
        print(type(error).__name__, str(error))
for field in ('', '0.a[2]', 'a[]', 'a..b', 'a[x]foo', 'a.[x]',
              'a[0', '٠.a', '-1[x]', 'a[٣]', 'a[b[c]', 'a]z',
              '18446744073709551616.x', 'a[18446744073709551616]'):
    try:
        first, iterator = _string.formatter_field_name_split(field)
        print('first', repr(field), repr(first), iter(iterator) is iterator)
    except ValueError as error:
        print('first error', str(error))
        continue
    for index in range(3):
        try:
            print(next(iterator))
        except (ValueError, StopIteration) as error:
            print(type(error).__name__, str(error))
for value in (None, 7, b'text'):
    for operation in (_string.formatter_parser, _string.formatter_field_name_split):
        try:
            operation(value)
        except TypeError:
            print('requires text')
""",
    )


@pytest.mark.integration
def test_formatter_uses_attributes_conversions_and_nested_specifications(tmp_path):
    compare(
        tmp_path,
        """
from string import Formatter
class Value:
    name = 'Jill'
    def __str__(self):
        return 'object'
    def __format__(self, specification):
        return 'value:' + specification
formatter = Formatter()
print(formatter.format('{0.name} {data[٣]} {0!s}', Value(), data={3: 'three'}))
print(formatter.format('{0:{width}}', Value(), width='nested'))
print(formatter.format('{} {} {{literal}}', 'first', 'second'))
print(formatter.format('{v!a}', v='é'))
for text in ('{} {0}', '{0} {}', '{0!z}', '{0:{1:{2}}}'):
    try:
        print(formatter.format(text, Value(), 'x', 'y'))
    except ValueError as error:
        print(str(error))
""",
    )
