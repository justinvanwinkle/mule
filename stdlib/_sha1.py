"""FIPS 180-4 SHA-1, using the compiled streaming hash boundary."""

from _hash_common import BlockHash, rotate_left

_MASK = (1 << 32) - 1


class SHA1Type(BlockHash):
    _name = "sha1"
    _digest_size = 20
    _block_size = 64
    _length_size = 8
    _word_size = 4
    _byteorder = "big"
    _initial = (0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476, 0xC3D2E1F0)

    def _compress(self, block):
        words = [
            int.from_bytes(block[offset : offset + 4], "big")
            for offset in range(0, 64, 4)
        ]
        for index in range(16, 80):
            words.append(
                rotate_left(
                    words[index - 3]
                    ^ words[index - 8]
                    ^ words[index - 14]
                    ^ words[index - 16],
                    1,
                    32,
                )
            )
        a, b, c, d, e = self._state
        for index in range(80):
            if index < 20:
                function, constant = (b & c) | (~b & d), 0x5A827999
            elif index < 40:
                function, constant = b ^ c ^ d, 0x6ED9EBA1
            elif index < 60:
                function, constant = (b & c) | (b & d) | (c & d), 0x8F1BBCDC
            else:
                function, constant = b ^ c ^ d, 0xCA62C1D6
            temporary = (
                rotate_left(a, 5, 32) + function + e + constant + words[index]
            ) & _MASK
            a, b, c, d, e = temporary, a, rotate_left(b, 30, 32), c, d
        self._state = [
            (before + after) & _MASK
            for before, after in zip(self._state, (a, b, c, d, e))
        ]


def sha1(data=b"", *, usedforsecurity=True):
    return SHA1Type(data, usedforsecurity=usedforsecurity)
