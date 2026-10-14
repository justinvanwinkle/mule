"""MD5's RFC 1321 compression, following SBCL's compiled sb-md5 boundary."""

from _hash_common import BlockHash, rotate_left

_CONSTANTS = (
    0xD76AA478,
    0xE8C7B756,
    0x242070DB,
    0xC1BDCEEE,
    0xF57C0FAF,
    0x4787C62A,
    0xA8304613,
    0xFD469501,
    0x698098D8,
    0x8B44F7AF,
    0xFFFF5BB1,
    0x895CD7BE,
    0x6B901122,
    0xFD987193,
    0xA679438E,
    0x49B40821,
    0xF61E2562,
    0xC040B340,
    0x265E5A51,
    0xE9B6C7AA,
    0xD62F105D,
    0x02441453,
    0xD8A1E681,
    0xE7D3FBC8,
    0x21E1CDE6,
    0xC33707D6,
    0xF4D50D87,
    0x455A14ED,
    0xA9E3E905,
    0xFCEFA3F8,
    0x676F02D9,
    0x8D2A4C8A,
    0xFFFA3942,
    0x8771F681,
    0x6D9D6122,
    0xFDE5380C,
    0xA4BEEA44,
    0x4BDECFA9,
    0xF6BB4B60,
    0xBEBFBC70,
    0x289B7EC6,
    0xEAA127FA,
    0xD4EF3085,
    0x04881D05,
    0xD9D4D039,
    0xE6DB99E5,
    0x1FA27CF8,
    0xC4AC5665,
    0xF4292244,
    0x432AFF97,
    0xAB9423A7,
    0xFC93A039,
    0x655B59C3,
    0x8F0CCC92,
    0xFFEFF47D,
    0x85845DD1,
    0x6FA87E4F,
    0xFE2CE6E0,
    0xA3014314,
    0x4E0811A1,
    0xF7537E82,
    0xBD3AF235,
    0x2AD7D2BB,
    0xEB86D391,
)
_SHIFTS = (7, 12, 17, 22, 5, 9, 14, 20, 4, 11, 16, 23, 6, 10, 15, 21)
_MASK = (1 << 32) - 1


class MD5Type(BlockHash):
    _name = "md5"
    _digest_size = 16
    _block_size = 64
    _length_size = 8
    _word_size = 4
    _byteorder = "little"
    _initial = (0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476)

    def _compress(self, block):
        words = [
            int.from_bytes(block[offset : offset + 4], "little")
            for offset in range(0, 64, 4)
        ]
        a, b, c, d = self._state
        for index in range(64):
            if index < 16:
                function = (b & c) | (~b & d)
                word = index
            elif index < 32:
                function = (d & b) | (~d & c)
                word = (5 * index + 1) % 16
            elif index < 48:
                function = b ^ c ^ d
                word = (3 * index + 5) % 16
            else:
                function = c ^ (b | ~d)
                word = (7 * index) % 16
            mixed = (a + function + _CONSTANTS[index] + words[word]) & _MASK
            shift = _SHIFTS[(index // 16) * 4 + index % 4]
            a, b, c, d = d, (b + rotate_left(mixed, shift, 32)) & _MASK, b, c
        self._state = [
            (old + new) & _MASK for old, new in zip(self._state, (a, b, c, d))
        ]


def md5(data=b"", *, usedforsecurity=True):
    return MD5Type(data, usedforsecurity=usedforsecurity)
