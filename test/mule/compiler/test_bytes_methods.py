"""Bytes-like joins and hexadecimal grouping compared with Python 3.14."""

from test.mule.compiler.test_exception_groups import compare


def test_byte_padding_directions_center_rounding_and_fill_validation(tmp_path):
    result = compare(
        tmp_path,
        """
for factory in (bytes, bytearray):
    for data in (b'', b'a', b'ab'):
        for width in (-1, 0, 1, 2, 3, 4, 5):
            value = factory(data)
            for name in ('ljust', 'rjust', 'center'):
                print(getattr(value, name)(width, bytearray(b'_')))
    for fill in (b'', b'xx', 'x', None, memoryview(b'_')):
        for name in ('ljust', 'rjust', 'center'):
            try:
                getattr(factory(b'ab'), name)(1, fill)
            except TypeError as error:
                print(str(error))
class Raw(bytes):
    def __len__(self): raise AssertionError('length')
    def __getitem__(self, key): raise AssertionError('index')
    def __iter__(self): raise AssertionError('iteration')
for value in (Raw(b'ab'), b'ab'):
    result = value.ljust(1)
    print(type(result).__name__, result, result is value)
    print(value.center(5))
value = bytearray(b'ab')
result = value.ljust(1)
print(result is value, result)
""",
    )
    assert result.stderr == ""


def test_bytes_join_accepts_contiguous_buffers_and_preserves_iterators(tmp_path):
    compare(
        tmp_path,
        """
print(b'|'.join(item for item in (b'a', bytearray(b'b'), memoryview(b'c'))))
print(b'|'.join([]), b'|'.join([b'only']))
for items in ([b'a', 'text'], [b'a', [98]], [memoryview(b'abc')[::2]]):
    try:
        b'|'.join(items)
    except TypeError as error:
        print(type(error).__name__, str(error))
""",
    )


def test_byte_hex_separator_grouping_and_released_views(tmp_path):
    compare(
        tmp_path,
        """
for value in (b'abcde', bytearray(b'abcde'), memoryview(b'abcde')):
    print(value.hex())
    for group in (0, 1, 2, -2, 3, -3, 99, -99):
        print(value.hex(':', group), value.hex(b'|', group))
    for separator in (None, '', 'ab', 'é', b''):
        try:
            value.hex(separator)
        except (TypeError, ValueError) as error:
            print(type(error).__name__, str(error))
view = memoryview(b'abc')
view.release()
try:
    view.hex()
except ValueError as error:
    print(type(error).__name__, str(error))
""",
    )
