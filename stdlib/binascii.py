"""Conversion between binary data and ASCII text encodings.

This is a pure-Python implementation of the interface CPython provides
in its ``binascii`` C module, written for Mule's compiled standard
library. It supplies the helpers used by ``base64``, ``zipfile``,
``quopri`` and ``uu``: CRC checksums, hexadecimal conversion, and the
base64, quoted-printable and uuencode line codecs.
"""

from _mule_native import bytes_from_values

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


def _as_bytes(data) -> bytes:
    if isinstance(data, bytes):
        return data
    if isinstance(data, str):
        raise TypeError("a bytes-like object is required, not 'str'")
    return bytes(data)


# ---------------------------------------------------------------------------
# CRC-32 (as in zip and PNG), reflected polynomial 0xEDB88320.
def _make_crc_table() -> list[int]:
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


_CRC_TABLE: list[int] = _make_crc_table()


def _crc32_bytes(raw: bytes, crc: int) -> int:
    table: list[int] = _CRC_TABLE
    c = (crc ^ 0xFFFFFFFF) & 0xFFFFFFFF
    n = len(raw)
    i = 0
    while i < n:
        c = table[(c ^ raw[i]) & 0xFF] ^ (c >> 8)
        i += 1
    return (c ^ 0xFFFFFFFF) & 0xFFFFFFFF


def crc32(data, value: int = 0) -> int:
    return _crc32_bytes(_as_bytes(data), value)


# ---------------------------------------------------------------------------
# CRC-CCITT (as used by HQX), polynomial 0x1021.


def _make_hqx_table() -> list[int]:
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


_HQX_TABLE: list[int] = _make_hqx_table()


def _crc_hqx_bytes(raw: bytes, value: int) -> int:
    table: list[int] = _HQX_TABLE
    crc = value & 0xFFFF
    n = len(raw)
    i = 0
    while i < n:
        crc = table[((crc >> 8) ^ raw[i]) & 0xFF] ^ ((crc << 8) & 0xFFFF)
        i += 1
    return crc & 0xFFFF


def crc_hqx(data, value: int) -> int:
    return _crc_hqx_bytes(_as_bytes(data), value)


# ---------------------------------------------------------------------------
# Hexadecimal conversion.

_HEX_DIGITS: bytes = b"0123456789abcdef"

_HEX_DECODE: bytes = bytes(
    b - 48
    if 48 <= b <= 57
    else b - 87
    if 97 <= b <= 102
    else b - 55
    if 65 <= b <= 70
    else 254
    if b in (9, 10, 13, 32)
    else 255
    for b in range(256)
)


def _b2a_hex_bytes(raw: bytes, sep_byte: int, bytes_per_sep: int) -> bytes:
    n = len(raw)
    if n == 0:
        return b""
    digits: bytes = _HEX_DIGITS
    if sep_byte < 0:
        out = [0] * (n * 2)
        i = 0
        w = 0
        while i < n and w + 1 < len(out):
            byte = raw[i]
            out[w] = digits[byte >> 4]
            out[w + 1] = digits[byte & 0x0F]
            i += 1
            w += 2
        return bytes_from_values(out)

    if bytes_per_sep == 0:
        raise ValueError("bytes_per_sep must be nonzero")
    step = bytes_per_sep if bytes_per_sep > 0 else -bytes_per_sep
    num_seps = (n - 1) // step
    out_len = n * 2 + num_seps
    out = [0] * out_len
    if bytes_per_sep > 0:
        first_group = n % step
        if first_group == 0:
            first_group = step
    else:
        first_group = step
    i = 0
    w = 0
    in_group = 0
    current_limit = first_group
    while i < n:
        if in_group == current_limit:
            out[w] = sep_byte
            w += 1
            in_group = 0
            current_limit = step
        byte = raw[i]
        out[w] = digits[byte >> 4]
        out[w + 1] = digits[byte & 0x0F]
        i += 1
        w += 2
        in_group += 1
    return bytes_from_values(out)


def b2a_hex(data, sep=None, bytes_per_sep=1) -> bytes:
    raw = _as_bytes(data)
    if sep is None:
        return _b2a_hex_bytes(raw, -1, 1)
    if isinstance(sep, str):
        if len(sep) != 1:
            raise ValueError("sep must be length 1")
        separator = ord(sep)
        if separator > 127:
            raise ValueError("sep must be ASCII")
    else:
        sep_bytes = _as_bytes(sep)
        if len(sep_bytes) != 1:
            raise ValueError("sep must be length 1")
        separator = sep_bytes[0]
        if separator > 127:
            raise ValueError("sep must be ASCII")
    return _b2a_hex_bytes(raw, separator, int(bytes_per_sep))


hexlify = b2a_hex


def _hex_value(byte: int) -> int:
    val = _HEX_DECODE[byte]
    return val if val < 16 else -1


def _a2b_hex_bytes(raw: bytes) -> bytes:
    n = len(raw)
    if n == 0:
        return b""
    table: bytes = _HEX_DECODE
    if (n & 1) == 0:
        out = [0] * (n >> 1)
        i = 0
        w = 0
        ws_found = False
        while i < n and w < len(out):
            hi = table[raw[i]]
            lo = table[raw[i + 1]]
            if hi < 16 and lo < 16:
                out[w] = (hi << 4) | lo
                w += 1
                i += 2
            else:
                ws_found = True
                break
        if not ws_found:
            return bytes_from_values(out)

    digits = [0] * n
    d_len = 0
    i = 0
    while i < n:
        val = table[raw[i]]
        if val < 16:
            digits[d_len] = val
            d_len += 1
        elif val != 254:
            raise Error("Non-hexadecimal digit found")
        i += 1
    if d_len & 1:
        raise Error("Odd-length string")
    out_len = d_len >> 1
    out2 = [0] * out_len
    j = 0
    w2 = 0
    while w2 < out_len:
        out2[w2] = (digits[j] << 4) | digits[j + 1]
        j += 2
        w2 += 1
    return bytes_from_values(out2)


def a2b_hex(data) -> bytes:
    if isinstance(data, str):
        try:
            raw = data.encode("ascii")
        except UnicodeEncodeError:
            raise Error("Non-hexadecimal digit found") from None
    else:
        raw = _as_bytes(data)
    return _a2b_hex_bytes(raw)


unhexlify = a2b_hex

# ---------------------------------------------------------------------------
# Base64 line codec.

_BASE64_ALPHABET: bytes = (
    b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
)

_BASE64_DECODE: bytes = bytes(
    b - 65
    if 65 <= b <= 90
    else b - 71
    if 97 <= b <= 122
    else b + 4
    if 48 <= b <= 57
    else 62
    if b == 43
    else 63
    if b == 47
    else 64
    if b == 61
    else 255
    for b in range(256)
)


def _b2a_base64_bytes(raw: bytes, newline: bool) -> bytes:
    n = len(raw)
    full = (n // 3) * 3
    rem = n - full
    out_len = (full // 3) * 4 + (4 if rem else 0) + (1 if newline else 0)
    out = [0] * out_len
    r = 0
    w = 0
    table: bytes = _BASE64_ALPHABET
    while r + 2 < n and w + 3 < len(out):
        value = (raw[r] << 16) | (raw[r + 1] << 8) | raw[r + 2]
        out[w] = table[(value >> 18) & 63]
        out[w + 1] = table[(value >> 12) & 63]
        out[w + 2] = table[(value >> 6) & 63]
        out[w + 3] = table[value & 63]
        r += 3
        w += 4
    if rem == 1:
        value = raw[r] << 16
        out[w] = table[(value >> 18) & 63]
        out[w + 1] = table[(value >> 12) & 63]
        out[w + 2] = 61
        out[w + 3] = 61
        w += 4
    elif rem == 2:
        value = (raw[r] << 16) | (raw[r + 1] << 8)
        out[w] = table[(value >> 18) & 63]
        out[w + 1] = table[(value >> 12) & 63]
        out[w + 2] = table[(value >> 6) & 63]
        out[w + 3] = 61
        w += 4
    if newline:
        out[w] = 10
    return bytes_from_values(out)


def b2a_base64(data, *, newline: bool = True) -> bytes:
    return _b2a_base64_bytes(_as_bytes(data), bool(newline))


def _base64_value(byte: int) -> int:
    val = _BASE64_DECODE[byte]
    return val if val < 64 else -1


def _a2b_base64_bytes(raw: bytes, strict_mode: bool) -> bytes:
    n = len(raw)
    if n == 0:
        return b""
    table: bytes = _BASE64_DECODE
    out = [0] * ((n // 4) * 3 + 3)
    w = 0
    padding = 0
    q_count = 0
    acc = 0
    i = 0
    while i < n:
        code = table[raw[i]]
        i += 1
        if code < 64:
            if padding:
                raise Error("Excess data after padding")
            acc = (acc << 6) | code
            q_count += 1
            if q_count == 4:
                out[w] = (acc >> 16) & 0xFF
                out[w + 1] = (acc >> 8) & 0xFF
                out[w + 2] = acc & 0xFF
                w += 3
                acc = 0
                q_count = 0
        elif code == 64:
            padding += 1
            acc <<= 6
            q_count += 1
            if q_count == 4:
                out[w] = (acc >> 16) & 0xFF
                out[w + 1] = (acc >> 8) & 0xFF
                out[w + 2] = acc & 0xFF
                w += 3
                acc = 0
                q_count = 0
        elif strict_mode:
            raise Error("Only base64 data is allowed")
    if padding > 2:
        raise Error("Excess data after padding")
    if q_count != 0:
        raise Error("Incorrect padding")
    if padding:
        del out[w - padding :]
    elif w < len(out):
        del out[w:]
    return bytes_from_values(out)


def a2b_base64(data, *, strict_mode: bool = False) -> bytes:
    if isinstance(data, str):
        try:
            raw = data.encode("ascii")
        except UnicodeEncodeError:
            raise ValueError(
                "string argument should contain only ASCII characters"
            ) from None
    else:
        raw = _as_bytes(data)
    return _a2b_base64_bytes(raw, bool(strict_mode))


# ---------------------------------------------------------------------------
# Quoted-printable line codec.


def b2a_qp(data, quotetabs=False, istext=True, header=False) -> bytes:
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


def _upper_hex(byte: int) -> int:
    return byte - 32 if 97 <= byte <= 102 else byte


def a2b_qp(data, header=False) -> bytes:
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


def b2a_uu(data, /, *, backtick=False) -> bytes:
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


def a2b_uu(data) -> bytes:
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
