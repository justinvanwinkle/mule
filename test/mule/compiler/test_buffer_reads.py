"""Writable reads and buffer export lifetime agree with Python 3.14."""

from test.mule.compiler.test_exception_groups import compare
from test.mule.compiler.test_modules import execute


def test_physical_word_reads_are_unsigned_unaligned_and_bounded(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
from _mule_native import read_u64_le
class Raw(bytes):
    def __len__(self): raise AssertionError('length override')
    def __getitem__(self, index): raise AssertionError('index override')
    def __iter__(self): raise AssertionError('iteration override')
for data in (bytes(range(17)), b'\\xff' * 17, b'\\x00' * 17):
    for start in (0, 1, 9):
        print(read_u64_le(Raw(data), start))
for data, start in ((b'', 0), (b'1234567', 0), (b'12345678', -1),
                    (b'12345678', 1), (b'12345678', 1 << 100)):
    try:
        read_u64_le(data, start)
    except IndexError:
        print('bounds')
for data, start in ((None, 0), ('12345678', 0), (bytearray(8), 0),
                    (b'12345678', None), (b'12345678', 1.0)):
    try:
        read_u64_le(data, start)
    except TypeError:
        print('type')
""",
    )
    assert result.returncode == 0, result.stderr
    expected = [
        str(int.from_bytes(data[start : start + 8], "little"))
        for data in (bytes(range(17)), b"\xff" * 17, b"\x00" * 17)
        for start in (0, 1, 9)
    ]
    assert result.stdout.splitlines() == expected + ["bounds"] * 5 + ["type"] * 5


def test_memoryview_copies_slices_casts_and_strides_without_buffer_hooks(tmp_path):
    compare(
        tmp_path,
        """
class Raw(bytes):
    def __len__(self): raise AssertionError('length')
    def __getitem__(self, key): raise AssertionError('index')
    def __iter__(self): raise AssertionError('iteration')
    def __bytes__(self): raise AssertionError('conversion')
original = bytes(range(32))
view = memoryview(Raw(original))
for selected in (view, view[:], view[5:13], view[32:], view[::2], view[::-1],
                 view.cast('H')[2:7], view.cast('I')[1:4], view.cast('Q')[:2],
                 view.cast('f')[2:6], view.cast('d')[1:3]):
    print(selected.tobytes().hex())
    selected.release()
try:
    view.tobytes()
except ValueError as error:
    print(type(error).__name__, str(error))
buffer = bytearray(original)
mutable = memoryview(buffer)[3:11]
copied = mutable.tobytes()
buffer[3] = 255
print(copied.hex(), mutable.tobytes().hex())
mutable.release()
""",
    )


def test_bytes_slices_normalize_each_integer_bound_once(tmp_path):
    compare(
        tmp_path,
        """
class Bound:
    def __init__(self, name, value): self.name, self.value = name, value
    def __index__(self):
        print('bound', self.name)
        return self.value
class Raw(bytes):
    def __len__(self): raise AssertionError('length')
    def __getitem__(self, key): raise AssertionError('index')
for data in (b'abcdefgh', Raw(b'abcdefgh')):
    for start, stop, step in ((1, 7, 1), (1, 7, 2), (7, 1, -2), (5, 1, 1),
                              (-100, 100, 1), (100, 200, 1), (0, 8, 0)):
        bounds = slice(Bound('start', start), Bound('stop', stop), Bound('step', step))
        try:
            result = bytes.__getitem__(data, bounds)
            print(type(result).__name__, result)
        except ValueError as error:
            print(type(error).__name__, str(error))
print(b''[:], bytes.__getitem__(Raw(b''), slice(None)))
""",
    )


def test_bytearray_resize_and_exported_views_preserve_storage(tmp_path):
    compare(
        tmp_path,
        """
class Size:
    def __index__(self):
        return 5
buffer = bytearray(b'ab')
buffer.resize(Size())
print(bytes(buffer))
buffer.resize(3)
print(bytes(buffer))
view = memoryview(buffer)
derived = view[1:]
view.release()
for operation in (lambda: buffer.resize(4), lambda: buffer.append(7), lambda: buffer.pop(),
                  lambda: buffer.clear(), lambda: buffer.extend(b'x'), lambda: buffer.insert(0, 8),
                  lambda: buffer.remove(97), lambda: buffer.__setitem__(slice(0, 1), b'xy'),
                  lambda: buffer.__delitem__(0)):
    try:
        operation()
    except BufferError as error:
        print(str(error))
buffer.resize(3)
buffer[1] = 99
buffer[:1] = b'z'
print(bytes(buffer), derived.tobytes())
derived.release()
buffer.resize(4)
print(bytes(buffer))
temporary = memoryview(buffer)
del temporary
buffer.resize(2)
print(bytes(buffer))
buffer.resize(0)
print(bytes(buffer))
for size in (-1, 1 << 80, -(1 << 80), 1.5):
    try:
        buffer.resize(size)
    except (ValueError, OverflowError, TypeError) as error:
        print(type(error).__name__)
for attribute in ('obj', 'format', 'readonly', 'nbytes', 'shape', 'strides', 'c_contiguous'):
    try:
        getattr(view, attribute)
    except ValueError:
        print('released', attribute)
""",
    )


def test_readinto_fills_slices_and_casts_and_rejects_invalid_buffers(tmp_path):
    compare(
        tmp_path,
        """
import os
read, write = os.pipe()
os.write(write, b'abcdef')
os.close(write)
buffer = bytearray(b'XXXXXXXX')
view = memoryview(buffer)[2:6]
print(os.readinto(read, view), bytes(buffer))
view.release()
words = memoryview(buffer).cast('I')
print(os.readinto(read, words), bytes(buffer))
words.release()
print(os.readinto(read, buffer), bytes(buffer))
os.close(read)
buffer.resize(5)
print(len(buffer))
for value in (b'readonly', memoryview(b'readonly'), memoryview(buffer)[::2], object()):
    try:
        os.readinto(-1, value)
    except (TypeError, BufferError) as error:
        print(type(error).__name__, str(error))
try:
    os.readinto(-1, bytearray())
except OSError:
    print('bad descriptor')
""",
    )
