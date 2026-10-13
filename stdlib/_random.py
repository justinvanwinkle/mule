"""Compiled MT19937 core, following SBCL's target-random.lisp."""

from math.integer import _index
from os import urandom

_MASK = 0xFFFFFFFF
_N = 624
_M = 397


class _Words:
    """The MT state is packed unsigned 32-bit storage, not boxed values."""

    def __init__(self):
        self._data = bytearray(4 * _N)

    def __getitem__(self, index):
        if index < 0:
            index += _N
        if index < 0 or index >= _N:
            raise IndexError("state word index out of range")
        offset = 4 * index
        return (
            self._data[offset]
            | (self._data[offset + 1] << 8)
            | (self._data[offset + 2] << 16)
            | (self._data[offset + 3] << 24)
        )

    def __setitem__(self, index, value):
        if index < 0:
            index += _N
        if index < 0 or index >= _N:
            raise IndexError("state word index out of range")
        offset = 4 * index
        for byte in range(4):
            self._data[offset + byte] = (value >> (8 * byte)) & 255


class Random:
    def __init__(self, seed=None):
        self.seed(seed)

    def seed(self, seed=None, /):
        if seed is None:
            data = urandom(4 * _N)
            words = [
                int.from_bytes(data[i : i + 4], "little")
                for i in range(0, len(data), 4)
            ]
        else:
            value = abs(_index(seed))
            words = []
            while value:
                words.append(value & _MASK)
                value >>= 32
            if not words:
                words.append(0)
        self._seed_words(words)

    def _seed_words(self, words):
        state = _Words()
        state[0] = 19650218
        for i in range(1, _N):
            previous = state[i - 1]
            state[i] = (1812433253 * (previous ^ (previous >> 30)) + i) & _MASK
        i, j = 1, 0
        for _ in range(max(_N, len(words))):
            previous = state[i - 1]
            state[i] = (
                (state[i] ^ ((previous ^ (previous >> 30)) * 1664525)) + words[j] + j
            ) & _MASK
            i += 1
            if i >= _N:
                state[0] = state[-1]
                i = 1
            j = (j + 1) % len(words)
        for _ in range(_N - 1):
            previous = state[i - 1]
            state[i] = (
                (state[i] ^ ((previous ^ (previous >> 30)) * 1566083941)) - i
            ) & _MASK
            i += 1
            if i >= _N:
                state[0] = state[-1]
                i = 1
        state[0] = 0x80000000
        self._state = state
        self._position = _N

    def _twist(self):
        state = self._state
        for i in range(_N):
            value = (state[i] & 0x80000000) | (state[(i + 1) % _N] & 0x7FFFFFFF)
            state[i] = (
                state[(i + _M) % _N] ^ (value >> 1) ^ (0x9908B0DF if value & 1 else 0)
            )
        self._position = 0

    def _word(self):
        if self._position >= _N:
            self._twist()
        value = self._state[self._position]
        self._position += 1
        value ^= value >> 11
        value ^= (value << 7) & 0x9D2C5680
        value ^= (value << 15) & 0xEFC60000
        return value ^ (value >> 18)

    def random(self, /):
        high = self._word() >> 5
        low = self._word() >> 6
        return (high * 67108864 + low) / 9007199254740992

    def getrandbits(self, k, /):
        k = _index(k)
        if k < 0:
            raise ValueError("number of bits must be non-negative")
        result = 0
        offset = 0
        while k:
            width = min(k, 32)
            result |= (self._word() >> (32 - width)) << offset
            offset += width
            k -= width
        return result

    def getstate(self, /):
        return (*[self._state[i] for i in range(_N)], self._position)

    def setstate(self, state, /):
        if len(state) != _N + 1:
            raise ValueError("state vector is the wrong size")
        words = _Words()
        for index, value in enumerate(state[:-1]):
            value = _index(value)
            if value < 0 or value > _MASK:
                raise OverflowError("state word is outside unsigned 32-bit storage")
            words[index] = value
        position = _index(state[-1])
        if position < 0 or position > _N:
            raise ValueError("invalid state")
        self._state = words
        self._position = position
