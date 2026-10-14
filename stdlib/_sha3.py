"""SHA-3 and SHAKE, following FIPS 202's byte-oriented Keccak sponge."""

from _hash_common import buffer_bytes, rotate_left
from _mule_native import read_u64_le as _read_u64_le
from math.integer import _index

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
        # Lane i is x + 5*y. Keep lanes local across the 24 Keccak rounds.
        state = self._state
        a0, a1, a2, a3, a4 = state[0:5]
        a5, a6, a7, a8, a9 = state[5:10]
        a10, a11, a12, a13, a14 = state[10:15]
        a15, a16, a17, a18, a19 = state[15:20]
        a20, a21, a22, a23, a24 = state[20:25]
        for constant in _CONSTANTS:
            c0 = a0 ^ a5 ^ a10 ^ a15 ^ a20
            c1 = a1 ^ a6 ^ a11 ^ a16 ^ a21
            c2 = a2 ^ a7 ^ a12 ^ a17 ^ a22
            c3 = a3 ^ a8 ^ a13 ^ a18 ^ a23
            c4 = a4 ^ a9 ^ a14 ^ a19 ^ a24
            d0 = c4 ^ rotate_left(c1, 1, 64)
            d1 = c0 ^ rotate_left(c2, 1, 64)
            d2 = c1 ^ rotate_left(c3, 1, 64)
            d3 = c2 ^ rotate_left(c4, 1, 64)
            d4 = c3 ^ rotate_left(c0, 1, 64)
            b0 = a0 ^ d0
            b10 = rotate_left(a1 ^ d1, 1, 64)
            b20 = rotate_left(a2 ^ d2, 62, 64)
            b5 = rotate_left(a3 ^ d3, 28, 64)
            b15 = rotate_left(a4 ^ d4, 27, 64)
            b16 = rotate_left(a5 ^ d0, 36, 64)
            b1 = rotate_left(a6 ^ d1, 44, 64)
            b11 = rotate_left(a7 ^ d2, 6, 64)
            b21 = rotate_left(a8 ^ d3, 55, 64)
            b6 = rotate_left(a9 ^ d4, 20, 64)
            b7 = rotate_left(a10 ^ d0, 3, 64)
            b17 = rotate_left(a11 ^ d1, 10, 64)
            b2 = rotate_left(a12 ^ d2, 43, 64)
            b12 = rotate_left(a13 ^ d3, 25, 64)
            b22 = rotate_left(a14 ^ d4, 39, 64)
            b23 = rotate_left(a15 ^ d0, 41, 64)
            b8 = rotate_left(a16 ^ d1, 45, 64)
            b18 = rotate_left(a17 ^ d2, 15, 64)
            b3 = rotate_left(a18 ^ d3, 21, 64)
            b13 = rotate_left(a19 ^ d4, 8, 64)
            b14 = rotate_left(a20 ^ d0, 18, 64)
            b24 = rotate_left(a21 ^ d1, 2, 64)
            b9 = rotate_left(a22 ^ d2, 61, 64)
            b19 = rotate_left(a23 ^ d3, 56, 64)
            b4 = rotate_left(a24 ^ d4, 14, 64)
            a0 = b0 ^ (~b1 & b2)
            a1 = b1 ^ (~b2 & b3)
            a2 = b2 ^ (~b3 & b4)
            a3 = b3 ^ (~b4 & b0)
            a4 = b4 ^ (~b0 & b1)
            a5 = b5 ^ (~b6 & b7)
            a6 = b6 ^ (~b7 & b8)
            a7 = b7 ^ (~b8 & b9)
            a8 = b8 ^ (~b9 & b5)
            a9 = b9 ^ (~b5 & b6)
            a10 = b10 ^ (~b11 & b12)
            a11 = b11 ^ (~b12 & b13)
            a12 = b12 ^ (~b13 & b14)
            a13 = b13 ^ (~b14 & b10)
            a14 = b14 ^ (~b10 & b11)
            a15 = b15 ^ (~b16 & b17)
            a16 = b16 ^ (~b17 & b18)
            a17 = b17 ^ (~b18 & b19)
            a18 = b18 ^ (~b19 & b15)
            a19 = b19 ^ (~b15 & b16)
            a20 = b20 ^ (~b21 & b22)
            a21 = b21 ^ (~b22 & b23)
            a22 = b22 ^ (~b23 & b24)
            a23 = b23 ^ (~b24 & b20)
            a24 = b24 ^ (~b20 & b21)
            a0 ^= constant
        state[:] = [
            a0,
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11,
            a12,
            a13,
            a14,
            a15,
            a16,
            a17,
            a18,
            a19,
            a20,
            a21,
            a22,
            a23,
            a24,
        ]

    def _absorb(self, block):
        for index in range(self._rate // 8):
            self._state[index] ^= _read_u64_le(block, index * 8)
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
        result._absorb(bytes(padding))
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
