"""POSIX writes consume contiguous byte buffers independently of iteration."""

from test.mule.compiler.test_exception_groups import compare


def test_write_buffer_types_views_indices_and_errors(tmp_path):
    result = compare(
        tmp_path,
        """
import os
class Index:
    def __init__(self, value): self.value = value
    def __index__(self): return self.value
class Raw(bytes):
    def __len__(self): raise AssertionError('length')
    def __getitem__(self, index): raise AssertionError('index')
    def __iter__(self): raise AssertionError('iteration')
class Mutable(bytearray):
    def __len__(self): raise AssertionError('length')
    def __getitem__(self, index): raise AssertionError('index')
    def __iter__(self): raise AssertionError('iteration')
read, write = os.pipe()
for data in (b'bytes', bytearray(b'array'), memoryview(b'view'),
             Raw(b'subclass'), Mutable(b'mutable'), memoryview(b'abcd').cast('H')):
    count = os.write(Index(write), data)
    print(count, os.read(read, count))
for data in ('text', [1, 2], 2, memoryview(b'abcd')[::2]):
    try:
        os.write(write, data)
    except (TypeError, BufferError) as error:
        print(type(error).__name__, str(error))
os.close(write)
os.close(read)
""",
    )
    assert result.stderr == ""
