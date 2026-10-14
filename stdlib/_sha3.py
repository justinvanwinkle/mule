"""SHA-3 and SHAKE, following FIPS 202's byte-oriented Keccak sponge."""

from _hash_common import buffer_bytes, rotate_left
from math.integer import _index

_ROTATIONS = (
    0,
    1,
    62,
    28,
    27,
    36,
    44,
    6,
    55,
    20,
    3,
    10,
    43,
    25,
    39,
    41,
    45,
    15,
    21,
    8,
    18,
    2,
    61,
    56,
    14,
)
_CONSTANTS = (
    0x0000000000000001,
    0x0000000000008082,
    0x800000000000808A,
    0x8000000080008000,
    0x000000000000808B,
    0x0000000080000001,
    0x8000000080008081,
    0x8000000000008009,
    0x000000000000008A,
    0x0000000000000088,
    0x0000000080008009,
    0x000000008000000A,
    0x000000008000808B,
    0x800000000000008B,
    0x8000000000008089,
    0x8000000000008003,
    0x8000000000008002,
    0x8000000000000080,
    0x000000000000800A,
    0x800000008000000A,
    0x8000000080008081,
    0x8000000000008080,
    0x0000000080000001,
    0x8000000080008008,
)


class _Sponge:
    def __init__(self, data=b"", *, usedforsecurity=True):
        bool(usedforsecurity)
        self._state = [0] * 25
        self._buffer = b""
        self.update(data)

    @property
    def name(self):
        return self._name

    @property
    def digest_size(self):
        return self._digest_size

    @property
    def block_size(self):
        return self._rate

    def _permute(self):
        state = self._state
        for constant in _CONSTANTS:
            columns = [
                state[x] ^ state[x + 5] ^ state[x + 10] ^ state[x + 15] ^ state[x + 20]
                for x in range(5)
            ]
            differences = [
                columns[(x - 1) % 5] ^ rotate_left(columns[(x + 1) % 5], 1, 64)
                for x in range(5)
            ]
            rotated = [0] * 25
            for y in range(5):
                for x in range(5):
                    index = x + 5 * y
                    rotated[y + 5 * ((2 * x + 3 * y) % 5)] = rotate_left(
                        state[index] ^ differences[x], _ROTATIONS[index], 64
                    )
            for y in range(5):
                for x in range(5):
                    state[x + 5 * y] = rotated[x + 5 * y] ^ (
                        ~rotated[(x + 1) % 5 + 5 * y] & rotated[(x + 2) % 5 + 5 * y]
                    )
            state[0] ^= constant

    def _absorb(self, block):
        for index in range(self._rate // 8):
            self._state[index] ^= int.from_bytes(
                block[index * 8 : index * 8 + 8], "little"
            )
        self._permute()

    def update(self, data, /):
        data = self._buffer + buffer_bytes(data)
        offset = 0
        while offset + self._rate <= len(data):
            self._absorb(data[offset : offset + self._rate])
            offset += self._rate
        self._buffer = data[offset:]

    def copy(self):
        result = object.__new__(type(self))
        result._state = self._state.copy()
        result._buffer = self._buffer
        return result

    def _squeeze(self, length):
        result = self.copy()
        padding = bytearray(self._buffer)
        padding.append(self._domain)
        padding.extend(bytes(self._rate - len(padding)))
        padding[-1] |= 0x80
        result._absorb(padding)
        pieces = []
        while length:
            data = b"".join(
                word.to_bytes(8, "little") for word in result._state[: self._rate // 8]
            )
            pieces.append(data[:length])
            length -= min(length, self._rate)
            if length:
                result._permute()
        return b"".join(pieces)


class _SHA3(_Sponge):
    _domain = 0x06

    def digest(self):
        return self._squeeze(self._digest_size)

    def hexdigest(self):
        return self.digest().hex()


class SHA3_224(_SHA3):
    _name = "sha3_224"
    _digest_size = 28
    _rate = 144


class SHA3_256(_SHA3):
    _name = "sha3_256"
    _digest_size = 32
    _rate = 136


class SHA3_384(_SHA3):
    _name = "sha3_384"
    _digest_size = 48
    _rate = 104


class SHA3_512(_SHA3):
    _name = "sha3_512"
    _digest_size = 64
    _rate = 72


class _SHAKE(_Sponge):
    _domain = 0x1F
    _digest_size = 0

    def digest(self, length):
        length = _index(length)
        if length < 0:
            raise ValueError("Cannot convert negative int")
        if length >= 1 << 64:
            raise OverflowError("Python int too large for C unsigned long")
        return self._squeeze(length)

    def hexdigest(self, length):
        return self.digest(length).hex()


class SHAKE128(_SHAKE):
    _name = "shake_128"
    _rate = 168


class SHAKE256(_SHAKE):
    _name = "shake_256"
    _rate = 136


def sha3_224(data=b"", *, usedforsecurity=True):
    return SHA3_224(data, usedforsecurity=usedforsecurity)


def sha3_256(data=b"", *, usedforsecurity=True):
    return SHA3_256(data, usedforsecurity=usedforsecurity)


def sha3_384(data=b"", *, usedforsecurity=True):
    return SHA3_384(data, usedforsecurity=usedforsecurity)


def sha3_512(data=b"", *, usedforsecurity=True):
    return SHA3_512(data, usedforsecurity=usedforsecurity)


def shake_128(data=b"", *, usedforsecurity=True):
    return SHAKE128(data, usedforsecurity=usedforsecurity)


def shake_256(data=b"", *, usedforsecurity=True):
    return SHAKE256(data, usedforsecurity=usedforsecurity)
