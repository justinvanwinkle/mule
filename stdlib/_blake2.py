"""RFC 7693 BLAKE2 compression with Python's parameter-block interface."""

from _hash_common import buffer_bytes, rotate_right
from math.integer import _index
from _sha2 import SHA256Type, SHA512Type

BLAKE2B_SALT_SIZE = 16
BLAKE2B_PERSON_SIZE = 16
BLAKE2B_MAX_KEY_SIZE = 64
BLAKE2B_MAX_DIGEST_SIZE = 64
BLAKE2S_SALT_SIZE = 8
BLAKE2S_PERSON_SIZE = 8
BLAKE2S_MAX_KEY_SIZE = 32
BLAKE2S_MAX_DIGEST_SIZE = 32
_MISSING = object()
_SCHEDULE = (
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
    (14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3),
    (11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4),
    (7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8),
    (9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13),
    (2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9),
    (12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11),
    (13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10),
    (6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5),
    (10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0),
)
_MIXES = (
    (0, 4, 8, 12),
    (1, 5, 9, 13),
    (2, 6, 10, 14),
    (3, 7, 11, 15),
    (0, 5, 10, 15),
    (1, 6, 11, 12),
    (2, 7, 8, 13),
    (3, 4, 9, 14),
)


def _bounded(value, name, minimum, maximum):
    value = _index(value)
    if not minimum <= value <= maximum:
        raise ValueError(f"{name} must be between {minimum} and {maximum}")
    return value


class _Blake2:
    def __init__(
        self,
        data=b"",
        /,
        *,
        digest_size=_MISSING,
        key=b"",
        salt=b"",
        person=b"",
        fanout=1,
        depth=1,
        leaf_size=0,
        node_offset=0,
        node_depth=0,
        inner_size=0,
        last_node=False,
        usedforsecurity=True,
    ):
        bool(usedforsecurity)
        if digest_size is _MISSING:
            digest_size = self.MAX_DIGEST_SIZE
        self._size = _bounded(digest_size, "digest_size", 1, self.MAX_DIGEST_SIZE)
        key, salt, person = buffer_bytes(key), buffer_bytes(salt), buffer_bytes(person)
        if len(key) > self.MAX_KEY_SIZE:
            raise ValueError(f"maximum key length is {self.MAX_KEY_SIZE} bytes")
        if len(salt) > self.SALT_SIZE:
            raise ValueError(f"maximum salt length is {self.SALT_SIZE} bytes")
        if len(person) > self.PERSON_SIZE:
            raise ValueError(f"maximum person length is {self.PERSON_SIZE} bytes")
        fanout = _bounded(fanout, "fanout", 0, 255)
        depth = _bounded(depth, "depth", 1, 255)
        leaf_size = _bounded(leaf_size, "leaf_size", 0, (1 << 32) - 1)
        node_offset = _bounded(
            node_offset, "node_offset", 0, (1 << (8 * self._offset_size)) - 1
        )
        node_depth = _bounded(node_depth, "node_depth", 0, 255)
        inner_size = _bounded(inner_size, "inner_size", 0, self.MAX_DIGEST_SIZE)
        parameters = (
            bytes((self._size, len(key), fanout, depth))
            + leaf_size.to_bytes(4, "little")
            + node_offset.to_bytes(self._offset_size, "little")
            + bytes((node_depth, inner_size))
            + bytes(self._reserved)
            + salt.ljust(self.SALT_SIZE, b"\0")
            + person.ljust(self.PERSON_SIZE, b"\0")
        )
        self._state = [
            word
            ^ int.from_bytes(
                parameters[index * self._word_size : (index + 1) * self._word_size],
                "little",
            )
            for index, word in enumerate(self._initial)
        ]
        self._processed = 0
        self._last_node = bool(last_node)
        self._buffer = key.ljust(self.block_size, b"\0") if key else b""
        self.update(data)

    @property
    def name(self):
        return self._name

    @property
    def digest_size(self):
        return self._size

    @property
    def block_size(self):
        return self._word_size * 16

    def _mix(self, state, a, b, c, d, x, y):
        width = self._word_size * 8
        mask = (1 << width) - 1
        first, second, third, fourth = self._rotations
        state[a] = (state[a] + state[b] + x) & mask
        state[d] = rotate_right(state[d] ^ state[a], first, width)
        state[c] = (state[c] + state[d]) & mask
        state[b] = rotate_right(state[b] ^ state[c], second, width)
        state[a] = (state[a] + state[b] + y) & mask
        state[d] = rotate_right(state[d] ^ state[a], third, width)
        state[c] = (state[c] + state[d]) & mask
        state[b] = rotate_right(state[b] ^ state[c], fourth, width)

    def _compress(self, block, final):
        words = [
            int.from_bytes(block[index : index + self._word_size], "little")
            for index in range(0, self.block_size, self._word_size)
        ]
        state = self._state + list(self._initial)
        width = self._word_size * 8
        mask = (1 << width) - 1
        state[12] ^= self._processed & mask
        state[13] ^= (self._processed >> width) & mask
        if final:
            state[14] ^= mask
            if self._last_node:
                state[15] ^= mask
        for round in range(self._rounds):
            schedule = _SCHEDULE[round % 10]
            for index, (a, b, c, d) in enumerate(_MIXES):
                self._mix(
                    state,
                    a,
                    b,
                    c,
                    d,
                    words[schedule[2 * index]],
                    words[schedule[2 * index + 1]],
                )
        self._state = [
            word ^ state[index] ^ state[index + 8]
            for index, word in enumerate(self._state)
        ]

    def update(self, data, /):
        data = self._buffer + buffer_bytes(data)
        offset = 0
        while offset + self.block_size < len(data):
            self._processed += self.block_size
            self._compress(data[offset : offset + self.block_size], False)
            offset += self.block_size
        self._buffer = data[offset:]

    def copy(self):
        result = object.__new__(type(self))
        result._state = self._state.copy()
        result._processed = self._processed
        result._buffer = self._buffer
        result._last_node = self._last_node
        result._size = self._size
        return result

    def digest(self):
        result = self.copy()
        result._processed += len(result._buffer)
        result._compress(result._buffer.ljust(self.block_size, b"\0"), True)
        return b"".join(
            word.to_bytes(self._word_size, "little") for word in result._state
        )[: self._size]

    def hexdigest(self):
        return self.digest().hex()


class blake2b(_Blake2):
    _name = "blake2b"
    _initial = SHA512Type._initial
    _word_size = 8
    _offset_size = 8
    _reserved = 14
    _rounds = 12
    _rotations = (32, 24, 16, 63)
    SALT_SIZE = BLAKE2B_SALT_SIZE
    PERSON_SIZE = BLAKE2B_PERSON_SIZE
    MAX_KEY_SIZE = BLAKE2B_MAX_KEY_SIZE
    MAX_DIGEST_SIZE = BLAKE2B_MAX_DIGEST_SIZE


class blake2s(_Blake2):
    _name = "blake2s"
    _initial = SHA256Type._initial
    _word_size = 4
    _offset_size = 6
    _reserved = 0
    _rounds = 10
    _rotations = (16, 12, 8, 7)
    SALT_SIZE = BLAKE2S_SALT_SIZE
    PERSON_SIZE = BLAKE2S_PERSON_SIZE
    MAX_KEY_SIZE = BLAKE2S_MAX_KEY_SIZE
    MAX_DIGEST_SIZE = BLAKE2S_MAX_DIGEST_SIZE
