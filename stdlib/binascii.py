"""Conversion between binary data and ASCII text encodings.

This is a pure-Python implementation of the interface CPython provides
in its ``binascii`` C module, written for Mule's compiled standard
library. It supplies the helpers used by ``base64``, ``zipfile``,
``quopri`` and ``uu``: CRC checksums, hexadecimal conversion, and the
base64, quoted-printable and uuencode line codecs.
"""

__all__ = [
    "Error",
    "Incomplete",
    "a2b_base64",
    "a2b_hex",
    "a2b_qp",
    "a2b_uu",
    "b2a_base64",
    "b2a_hex",
    "b2a_qp",
    "b2a_uu",
    "crc32",
    "crc_hqx",
    "hexlify",
    "unhexlify",
]


class Error(ValueError):
    pass


class Incomplete(Error):
    pass


def _as_bytes(data):
    if isinstance(data, str):
        raise TypeError("a bytes-like object is required, not 'str'")
    return bytes(data)


# ---------------------------------------------------------------------------
# CRC-32 (as in zip and PNG), reflected polynomial 0xEDB88320.
def _make_crc_table():
    table = []
    for value in range(256):
        crc = value
        for _ in range(8):
            if crc & 1:
                crc = (crc >> 1) ^ 0xEDB88320
            else:
                crc >>= 1
        table.append(crc)
    return table


_CRC_TABLE = _make_crc_table()


def crc32(data, value=0):
    crc = (value ^ 0xFFFFFFFF) & 0xFFFFFFFF
    for byte in _as_bytes(data):
        crc = _CRC_TABLE[(crc ^ byte) & 0xFF] ^ (crc >> 8)
    return (crc ^ 0xFFFFFFFF) & 0xFFFFFFFF


# ---------------------------------------------------------------------------
# CRC-CCITT (as used by HQX), polynomial 0x1021.


def _make_hqx_table():
    table = []
    for value in range(256):
        crc = value << 8
        for _ in range(8):
            if crc & 0x8000:
                crc = ((crc << 1) ^ 0x1021) & 0xFFFF
            else:
                crc = (crc << 1) & 0xFFFF
        table.append(crc)
    return table


_HQX_TABLE = _make_hqx_table()


def crc_hqx(data, value):
    crc = value & 0xFFFF
    for byte in _as_bytes(data):
        crc = _HQX_TABLE[((crc >> 8) ^ byte) & 0xFF] ^ ((crc << 8) & 0xFFFF)
    return crc & 0xFFFF


# ---------------------------------------------------------------------------
# Hexadecimal conversion.

_HEX_DIGITS = b"0123456789abcdef"


def b2a_hex(data, sep=None, bytes_per_sep=1):
    data = _as_bytes(data)
    result = bytearray()
    for byte in data:
        result.append(_HEX_DIGITS[byte >> 4])
        result.append(_HEX_DIGITS[byte & 0x0F])
    if sep is not None:
        if isinstance(sep, str):
            if len(sep) != 1:
                raise ValueError("sep must be length 1")
            separator = ord(sep)
        else:
            sep = _as_bytes(sep)
            if len(sep) != 1:
                raise ValueError("sep must be length 1")
            separator = sep[0]
        if bytes_per_sep == 0:
            raise ValueError("bytes_per_sep must be nonzero")
        group = 2 * (bytes_per_sep if bytes_per_sep > 0 else -bytes_per_sep)
        grouped = bytearray()
        if bytes_per_sep > 0:
            # Groups are counted from the right-hand end.
            index = len(result) % group
            if index:
                grouped += result[:index]
                if index < len(result):
                    grouped.append(separator)
            while index < len(result):
                grouped += result[index : index + group]
                index += group
                if index < len(result):
                    grouped.append(separator)
        else:
            index = 0
            while index < len(result):
                grouped += result[index : index + group]
                index += group
                if index < len(result):
                    grouped.append(separator)
        result = grouped
    return bytes(result)


hexlify = b2a_hex


def _hex_value(byte):
    if 48 <= byte <= 57:
        return byte - 48
    if 97 <= byte <= 102:
        return byte - 87
    if 65 <= byte <= 70:
        return byte - 55
    return -1


def a2b_hex(data):
    if isinstance(data, str):
        try:
            data = data.encode("ascii")
        except UnicodeEncodeError:
            raise Error("Non-hexadecimal digit found") from None
    digits = bytearray()
    for byte in _as_bytes(data):
        if byte in (9, 10, 13, 32):
            continue
        value = _hex_value(byte)
        if value < 0:
            raise Error("Non-hexadecimal digit found")
        digits.append(value)
    if len(digits) % 2:
        raise Error("Odd-length string")
    result = bytearray()
    index = 0
    while index < len(digits):
        result.append(digits[index] * 16 + digits[index + 1])
        index += 2
    return bytes(result)


unhexlify = a2b_hex

# ---------------------------------------------------------------------------
# Base64 line codec.

_BASE64_ALPHABET = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"


def b2a_base64(data, *, newline=True):
    data = _as_bytes(data)
    result = bytearray()
    index = 0
    while index + 3 <= len(data):
        value = data[index] * 65536 + data[index + 1] * 256 + data[index + 2]
        result.append(_BASE64_ALPHABET[(value >> 18) & 63])
        result.append(_BASE64_ALPHABET[(value >> 12) & 63])
        result.append(_BASE64_ALPHABET[(value >> 6) & 63])
        result.append(_BASE64_ALPHABET[value & 63])
        index += 3
    remaining = len(data) - index
    if remaining == 1:
        value = data[index] << 16
        result.append(_BASE64_ALPHABET[(value >> 18) & 63])
        result.append(_BASE64_ALPHABET[(value >> 12) & 63])
        result.append(61)
        result.append(61)
    elif remaining == 2:
        value = data[index] * 65536 + data[index + 1] * 256
        result.append(_BASE64_ALPHABET[(value >> 18) & 63])
        result.append(_BASE64_ALPHABET[(value >> 12) & 63])
        result.append(_BASE64_ALPHABET[(value >> 6) & 63])
        result.append(61)
    if newline:
        result.append(10)
    return bytes(result)


def _base64_value(byte):
    if 65 <= byte <= 90:
        return byte - 65
    if 97 <= byte <= 122:
        return byte - 71
    if 48 <= byte <= 57:
        return byte + 4
    if byte == 43:
        return 62
    if byte == 47:
        return 63
    return -1


def a2b_base64(data, *, strict_mode=False):
    data = _as_bytes(data)
    digits = bytearray()
    padding = 0
    for byte in data:
        value = _base64_value(byte)
        if value >= 0:
            if padding:
                raise Error("Excess data after padding")
            digits.append(value)
        elif byte == 61:
            padding += 1
            digits.append(0)
        elif strict_mode:
            raise Error("Only base64 data is allowed")
    if padding > 2:
        raise Error("Excess data after padding")
    if len(digits) % 4:
        raise Error("Incorrect padding")
    result = bytearray()
    index = 0
    while index < len(digits):
        value = (
            (digits[index] << 18)
            + (digits[index + 1] << 12)
            + (digits[index + 2] << 6)
            + digits[index + 3]
        )
        result.append((value >> 16) & 0xFF)
        result.append((value >> 8) & 0xFF)
        result.append(value & 0xFF)
        index += 4
    if padding:
        del result[len(result) - padding :]
    return bytes(result)


# ---------------------------------------------------------------------------
# Quoted-printable line codec.


def b2a_qp(data, quotetabs=False, istext=True, header=False):
    data = _as_bytes(data)
    result = bytearray()
    linelen = 0
    index = 0
    while index < len(data):
        byte = data[index]
        following = data[index + 1] if index + 1 < len(data) else 0
        if istext and byte == 10:
            result.append(byte)
            linelen = 0
            index += 1
            continue
        token = bytearray()
        if header and byte == 32:
            token.append(95)
        elif byte == 61 or (header and byte == 95):
            token.append(61)
            token.append(_upper_hex(_HEX_DIGITS[byte >> 4]))
            token.append(_upper_hex(_HEX_DIGITS[byte & 0x0F]))
        elif byte in (9, 32):
            if quotetabs or following in (10, 13) or index + 1 == len(data):
                token.append(61)
                token.append(_upper_hex(_HEX_DIGITS[byte >> 4]))
                token.append(_upper_hex(_HEX_DIGITS[byte & 0x0F]))
            else:
                token.append(byte)
        elif 33 <= byte <= 126:
            token.append(byte)
        elif istext and byte == 13:
            token.append(byte)
        else:
            token.append(61)
            token.append(_upper_hex(_HEX_DIGITS[byte >> 4]))
            token.append(_upper_hex(_HEX_DIGITS[byte & 0x0F]))
        if linelen + len(token) > 75 and index + 1 < len(data):
            result.append(61)
            result.append(10)
            linelen = 0
        result += token
        linelen += len(token)
        index += 1
    return bytes(result)


def _upper_hex(byte):
    return byte - 32 if 97 <= byte <= 102 else byte


def a2b_qp(data, header=False):
    data = _as_bytes(data)
    result = bytearray()
    index = 0
    while index < len(data):
        byte = data[index]
        if byte == 61 and index + 1 < len(data):
            following = data[index + 1]
            if following in (10, 13):
                index += 2
                if following == 13 and index < len(data) and data[index] == 10:
                    index += 1
                continue
            if index + 2 < len(data) or index + 2 == len(data):
                high = _hex_value(data[index + 1]) if index + 1 < len(data) else -1
                low = _hex_value(data[index + 2]) if index + 2 < len(data) else -1
                if high >= 0 and low >= 0:
                    result.append(high * 16 + low)
                    index += 3
                    continue
            result.append(byte)
            index += 1
        elif header and byte == 95:
            result.append(32)
            index += 1
        else:
            result.append(byte)
            index += 1
    return bytes(result)


# ---------------------------------------------------------------------------
# Uuencode line codec: at most 45 source bytes per encoded line.


def b2a_uu(data, /, *, backtick=False):
    data = _as_bytes(data)
    if len(data) > 45:
        raise Error("At most 45 bytes can be encoded in a UU line")
    zero = 96 if backtick else 32
    result = bytearray()
    result.append(zero if backtick and not data else len(data) + 32)
    index = 0
    while index < len(data):
        first = data[index]
        second = data[index + 1] if index + 1 < len(data) else 0
        third = data[index + 2] if index + 2 < len(data) else 0
        for value in (
            first >> 2,
            ((first & 3) << 4) | (second >> 4),
            ((second & 15) << 2) | (third >> 6),
            third & 63,
        ):
            result.append(zero if value == 0 and backtick else value + 32)
        index += 3
    result.append(10)
    return bytes(result)


def a2b_uu(data):
    data = _as_bytes(data)
    if not data:
        raise Incomplete("Truncated input data")
    length = (data[0] - 32) & 63
    result = bytearray()
    index = 1
    while len(result) < length:
        values = []
        for offset in range(4):
            position = index + offset
            if position < len(data) and data[position] not in (10, 13):
                values.append((data[position] - 32) & 63)
            else:
                values.append(0)
        result.append((values[0] << 2) | (values[1] >> 4))
        result.append(((values[1] & 15) << 4) | (values[2] >> 2))
        result.append(((values[2] & 3) << 6) | values[3])
        index += 4
    return bytes(result[:length])
