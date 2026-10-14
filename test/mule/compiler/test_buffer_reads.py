"""Writable reads and buffer export lifetime agree with Python 3.14."""

from test.mule.compiler.test_exception_groups import compare


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
