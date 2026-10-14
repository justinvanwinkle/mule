"""Integer byte conversions use Python's iterator and special-method protocols."""

from test.mule.compiler.test_exception_groups import compare


def test_integer_byte_iterators_orders_signs_hooks_and_subclasses(tmp_path):
    result = compare(
        tmp_path,
        """
class Index:
    def __init__(self, value): self.value = value
    def __index__(self): return self.value
class Number(int): pass
class Octets:
    def __bytes__(self):
        print('bytes hook')
        return b'\\x01\\x80'
class Raw(bytes):
    def __len__(self): raise AssertionError('length')
    def __getitem__(self, key): raise AssertionError('index')
    def __iter__(self): raise AssertionError('iteration')
for order in ('little', 'big'):
    for data in (b'', b'\\x01\\x80', bytearray(b'\\x01\\x80'),
                 memoryview(b'\\x01\\x80'), Raw(b'\\x01\\x80')):
        print(int.from_bytes(data, order), int.from_bytes(data, order, signed=True))
    print(int.from_bytes(map(Index, [1, 128]), order))
    print(int.from_bytes(iter([1, 128]), order, signed=True))
    print(int.from_bytes(Octets(), order))
    for value in (0, 1, 127, 128, 255, -1, -128, -129, 1 << 80):
        for signed in (False, True):
            try:
                data = value.to_bytes(Index(12), order, signed=signed)
                print(int.from_bytes(iter(data), order, signed=signed))
            except OverflowError as error:
                print(type(error).__name__)
print((1).to_bytes(), int.from_bytes(b'\\x01\\x00'))
print(type(Number.from_bytes(b'\\x01')).__name__)
for data in (0, 'text', [-1], [256], [1.5]):
    try:
        int.from_bytes(data)
    except (TypeError, ValueError) as error:
        print(type(error).__name__, str(error))
for order in (None, 0, 'invalid'):
    try:
        int.from_bytes(b'\\x01', order)
    except (TypeError, ValueError) as error:
        print(type(error).__name__, str(error))
""",
    )
    assert result.stderr == ""


def test_integer_byte_buffers_cross_word_boundaries_and_preserve_signs(tmp_path):
    compare(
        tmp_path,
        """
class Raw(bytes):
    def __len__(self): raise AssertionError('length')
    def __getitem__(self, key): raise AssertionError('index')
    def __iter__(self): raise AssertionError('iteration')
for size in (0, 1, 7, 8, 9, 31, 128):
    data = bytes((index * 37 + 129) & 255 for index in range(size))
    for buffer in (data, bytes([0]) + data, bytes([255]) + data):
        for order in ('little', 'big'):
            for signed in (False, True):
                value = int.from_bytes(Raw(buffer), order, signed=signed)
                print(size, order, signed, value)
                assert value.to_bytes(len(buffer), order, signed=signed) == buffer
""",
    )
