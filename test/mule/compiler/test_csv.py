"""CSV streaming and dialect behavior compared with Python 3.14."""

import pytest

from test.mule.compiler.test_exception_groups import compare


def test_csv_reader_streams_quotes_escapes_and_record_boundaries(tmp_path):
    compare(
        tmp_path,
        r"""
import _csv
cases = [
    [], [''], ['\n'], ['a,b,\r\n'], ['"a,b",c\n'],
    ['"a\n', 'b",c\r\n'], ['"a', 'b"'], ['"a""b",c'],
    ['a\\\n', 'b\n'], ['a\\'], ['a\\', 'b'], ['"a\\'],
    ['a\nb'], ['a\r\nb\r\n'], ['"a"x,b'], ['"unterminated'],
    ['a\x00b,c'], ['a\\,b,c'], [b'a,b'], [None],
]
for strict in (False, True):
    for chunks in cases:
        reader = _csv.reader(chunks, escapechar='\\', strict=strict)
        try:
            print(repr(list(reader)), reader.line_num)
        except (_csv.Error, TypeError, ValueError) as error:
            print(type(error).__name__, str(error), reader.line_num)
for mode in range(6):
    for text in ('1,2.5,"word",', ',"",', ' 1 ,2', '"1",2', '\\1,2'):
        try:
            print(mode, repr(list(_csv.reader([text], quoting=mode, escapechar='\\'))))
        except (_csv.Error, ValueError) as error:
            print(mode, type(error).__name__, str(error))
""",
    )


def test_csv_writer_quoting_modes_escaping_and_write_result(tmp_path):
    compare(
        tmp_path,
        r"""
import _csv
class Sink:
    def __init__(self):
        self.parts = []
    def write(self, text):
        self.parts.append(text)
        return len(text)
rows = [[], [''], [None], ['a,b'], ['a"b'], ['a\\b'], ['a\nb', 'c'],
        ['word', 42, 2.5, None, ''], (item for item in ('first', 'second'))]
for mode in range(6):
    for doublequote in (False, True):
        for row in rows[:-1]:
            sink = Sink()
            writer = _csv.writer(sink, quoting=mode, escapechar='\\', doublequote=doublequote)
            try:
                print(mode, doublequote, writer.writerow(row), repr(''.join(sink.parts)))
            except _csv.Error as error:
                print(mode, doublequote, type(error).__name__, str(error))
for row in (None, 42, ['a,b'], ['a"b']):
    try:
        print(_csv.writer(Sink(), quoting=_csv.QUOTE_NONE).writerow(row))
    except _csv.Error as error:
        print(type(error).__name__, str(error))
sink = Sink()
print(_csv.writer(sink, lineterminator='END').writerows([['E'], ['D'], ['x']]), repr(''.join(sink.parts)))
""",
    )


def test_csv_dialects_registry_and_field_limit(tmp_path):
    compare(
        tmp_path,
        r"""
import _csv
_csv.register_dialect('custom', delimiter=';', quotechar="'", strict=True)
dialect = _csv.get_dialect('custom')
print(_csv.list_dialects(), dialect.delimiter, dialect.quotechar, dialect.strict)
print(_csv.reader([], 'custom').dialect is dialect)
print(_csv.reader(['a;b'], 'custom', delimiter='|').dialect.delimiter)
for params in ({'delimiter': None}, {'delimiter': ''}, {'delimiter': 'ab'},
               {'quotechar': ''}, {'quotechar': None, 'quoting': 0},
               {'escapechar': ''}, {'quoting': 7}, {'quoting': True},
               {'lineterminator': None}, {'delimiter': '\n'},
               {'delimiter': '"'}, {'escapechar': ','}, {'escapechar': '"'},
               {'unknown': 1}):
    try:
        _csv.Dialect(**params)
    except (TypeError, ValueError) as error:
        print(type(error).__name__, str(error))
_csv.unregister_dialect('custom')
for name in ('custom', 42):
    try:
        _csv.get_dialect(name)
    except _csv.Error as error:
        print(type(error).__name__, str(error))
saved = _csv.field_size_limit(3)
for text in ('abc', 'abcd', '"abc"', 'ééé', 'éééé'):
    try:
        print(list(_csv.reader([text])))
    except _csv.Error as error:
        print(type(error).__name__, str(error))
for limit in (None, 1.5, 1 << 70):
    try:
        _csv.field_size_limit(limit)
    except (TypeError, OverflowError) as error:
        print(type(error).__name__, str(error))
print(_csv.field_size_limit(saved))
""",
    )


@pytest.mark.integration
def test_csv_module_dict_reader_and_writer(tmp_path):
    compare(
        tmp_path,
        r"""
import csv
from io import StringIO
source = StringIO('name,value\n"a,b",42\nlast,7,extra\nshort\n\n')
reader = csv.DictReader(source, restkey='extra', restval='missing')
print(reader.fieldnames, list(reader), reader.line_num)
target = StringIO()
writer = csv.DictWriter(target, fieldnames=['name', 'value'])
print(writer.writeheader())
writer.writerows([{'name': 'a,b', 'value': 42}, {'name': 'short'}])
print(repr(target.getvalue()))
""",
    )
