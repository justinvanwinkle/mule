"""Streaming hash state over compiled compression functions and bytes storage."""


def buffer_bytes(data):
    if isinstance(data, str):
        raise TypeError("Strings must be encoded before hashing")
    try:
        view = memoryview(data)
    except TypeError:
        raise TypeError("object supporting the buffer API required") from None
    with view:
        if not view.c_contiguous:
            raise BufferError("memoryview: underlying buffer is not C-contiguous")
        return view.tobytes()


def rotate_left(value, count, width):
    mask = (1 << width) - 1
    value &= mask
    return ((value << count) | (value >> (width - count))) & mask


def rotate_right(value, count, width):
    return rotate_left(value, width - count, width)


class BlockHash:
    """A digest whose subclass supplies a block compression function."""

    def __init__(self, data=b"", *, usedforsecurity=True):
        bool(usedforsecurity)
        self._state = list(self._initial)
        self._buffer = b""
        self._count = 0
        self.update(data)

    @property
    def name(self):
        return self._name

    @property
    def digest_size(self):
        return self._digest_size

    @property
    def block_size(self):
        return self._block_size

    def _compress(self, block):
        raise NotImplementedError("BlockHash subclasses must provide compression")

    def _ingest(self, data):
        self._count += len(data)
        data = self._buffer + data
        offset = 0
        while offset + self.block_size <= len(data):
            self._compress(data[offset : offset + self.block_size])
            offset += self.block_size
        self._buffer = data[offset:]

    def update(self, data, /):
        self._ingest(buffer_bytes(data))

    def copy(self):
        result = object.__new__(type(self))
        result._state = self._state.copy()
        result._buffer = self._buffer
        result._count = self._count
        return result

    def digest(self):
        result = self.copy()
        length_bits = (self._count * 8) % (1 << (self._length_size * 8))
        padding_size = (
            self.block_size - self._length_size - 1 - self._count
        ) % self.block_size
        result._ingest(
            b"\x80"
            + b"\x00" * padding_size
            + length_bits.to_bytes(self._length_size, self._byteorder)
        )
        return b"".join(
            word.to_bytes(self._word_size, self._byteorder) for word in result._state
        )[: self.digest_size]

    def hexdigest(self):
        return self.digest().hex()
