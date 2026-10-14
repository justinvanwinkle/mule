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
for sep in (b'|', bytearray(b'|')):
    print(sep.join(item for item in (b'a', bytearray(b'b'), memoryview(b'c'))))
    print(sep.join([]), sep.join([b'only']))
    for items in ([b'a', 'text'], [b'a', [98]], [memoryview(b'abc')[::2]]):
        try:
            sep.join(items)
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


def test_bytes_and_bytearray_strip_split_partition_replace_and_contains(tmp_path):
    compare(
        tmp_path,
        """
class SubBytes(bytes):
    pass

for factory in (bytes, bytearray):
    raw = factory(b' \\t\\n\\r\\x0b\\x0cab//cd//ef \\n')
    print(raw.strip(), raw.lstrip(), raw.rstrip())
    path = factory(b'/usr/local/bin///')
    print(path.rstrip(b'/'), path.lstrip(bytearray(b'/')), path.strip(memoryview(b'/')))
    print(factory(b'foobar').removeprefix(b'foo'), factory(b'foobar').removeprefix(b'bar'))
    print(factory(b'foobar').removesuffix(b'bar'), factory(b'foobar').removesuffix(b'foo'))
    print(factory(b'  a   b\\tc\\n ').split(), factory(b'  a   b\\tc\\n ').split(None, 1))
    print(factory(b'  a   b\\tc\\n ').rsplit(), factory(b'  a   b\\tc\\n ').rsplit(None, 1))
    print(factory(b'   ').split(), factory(b'   ').rsplit())
    print(factory(b'a::b::c').split(b'::'), factory(b'a::b::c').split(b'::', 1))
    print(factory(b'a::b::c').rsplit(b'::'), factory(b'a::b::c').rsplit(b'::', 1))
    print(factory(b'a:b:c').split(bytearray(b':'), 2))
    print(factory(b'a\\nb\\r\\nc\\rd').splitlines(), factory(b'a\\nb\\r\\nc\\rd').splitlines(True))
    print(factory(b'a:b:c').partition(b':'), factory(b'a:b:c').partition(b'/'))
    print(factory(b'a:b:c').rpartition(bytearray(b':')), factory(b'a:b:c').rpartition(b'/'))
    print(factory(b'ababa').replace(b'aba', b'X'), factory(b'ababa').replace(b'a', b'Z', 2))
    print(factory(b'abc').replace(b'', b'-'), factory(b'abc').replace(b'', b'-', 2))
    print(b'bc' in factory(b'abcd'), 98 in factory(b'abcd'), 999 not in [1, 2])
    for bad in (b'',):
        for method in ('split', 'rsplit', 'partition', 'rpartition'):
            try:
                getattr(factory(b'abc'), method)(bad)
            except ValueError as error:
                print(method, type(error).__name__, str(error))

sub = SubBytes(b'abc')
print(type(sub.strip()).__name__, type(sub.removeprefix(b'z')).__name__, type(sub.replace(b'x', b'y')).__name__)
""",
    )


def test_subprocess_run_handles_executable_dirname_and_child_error_pipe(tmp_path):
    compare(
        tmp_path,
        """
import os
import shutil
import subprocess

echo = shutil.which('echo') or '/bin/echo'
result = subprocess.run(
    [echo, 'hello-from-subprocess'],
    capture_output=True,
    text=True,
    env={'PATH': os.environ.get('PATH', '/usr/bin:/bin'), 'CUSTOM_VAR': 'ok'},
    check=True,
)
print(result.returncode, result.stdout.strip())
try:
    subprocess.run(['/nonexistent/mule/missing_binary'], capture_output=True, check=False)
except FileNotFoundError as error:
    print(type(error).__name__, error.filename)
""",
    )
