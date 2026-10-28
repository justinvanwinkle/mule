"""Base16, Base32, Base64 (RFC 4648), Base85 and Ascii85 data encodings"""

# Modified 04-Oct-1995 by Jack Jansen to use binascii module
# Modified 30-Dec-2003 by Barry Warsaw to add full RFC 3548 support
# Modified 22-May-2007 by Guido van Rossum to use bytes everywhere

import binascii
from _mule_native import bytes_from_values


__all__ = [
    # Legacy interface exports traditional RFC 2045 Base64 encodings
    'encode', 'decode', 'encodebytes', 'decodebytes',
    # Generalized interface for other encodings
    'b64encode', 'b64decode', 'b32encode', 'b32decode',
    'b32hexencode', 'b32hexdecode', 'b16encode', 'b16decode',
    # Base85 and Ascii85 encodings
    'b85encode', 'b85decode', 'a85encode', 'a85decode', 'z85encode', 'z85decode',
    # Standard Base64 encoding
    'standard_b64encode', 'standard_b64decode',
    # Some common Base64 alternatives.  As referenced by RFC 3458, see thread
    # starting at:
    #
    # http://zgp.org/pipermail/p2p-hackers/2001-September/000316.html
    'urlsafe_b64encode', 'urlsafe_b64decode',
    ]


bytes_types = (bytes, bytearray)  # Types acceptable as binary data

def _bytes_from_decode_data(s) -> bytes:
    if isinstance(s, bytes):
        return s
    if isinstance(s, str):
        try:
            return s.encode('ascii')
        except UnicodeEncodeError:
            raise ValueError('string argument should contain only ASCII characters')
    if isinstance(s, bytearray):
        return bytes(s)
    try:
        return memoryview(s).tobytes()
    except TypeError:
        raise TypeError("argument should be a bytes-like object or ASCII "
                        "string, not %r" % s.__class__.__name__) from None


def _as_encode_bytes(s) -> bytes:
    if isinstance(s, bytes):
        return s
    if isinstance(s, bytearray):
        return bytes(s)
    return memoryview(s).tobytes()


# Base64 encoding/decoding uses binascii

def _translate_2chars_bytes(raw: bytes, from0: int, from1: int, to0: int, to1: int) -> bytes:
    if from0 not in raw and from1 not in raw:
        return raw
    n = len(raw)
    out = [0] * n
    i = 0
    while i < n:
        c = raw[i]
        if c == from0:
            out[i] = to0
        elif c == from1:
            out[i] = to1
        else:
            out[i] = c
        i += 1
    return bytes_from_values(out)


def b64encode(s, altchars=None) -> bytes:
    """Encode the bytes-like object s using Base64 and return a bytes object.

    Optional altchars should be a byte string of length 2 which specifies an
    alternative alphabet for the '+' and '/' characters.  This allows an
    application to e.g. generate url or filesystem safe Base64 strings.
    """
    encoded = binascii.b2a_base64(s, newline=False)
    if altchars is not None:
        assert len(altchars) == 2, repr(altchars)
        alt = _as_encode_bytes(altchars)
        return _translate_2chars_bytes(encoded, 43, 47, alt[0], alt[1])
    return encoded


def b64decode(s, altchars=None, validate=False) -> bytes:
    """Decode the Base64 encoded bytes-like object or ASCII string s.

    Optional altchars must be a bytes-like object or ASCII string of length 2
    which specifies the alternative alphabet used instead of the '+' and '/'
    characters.

    The result is returned as a bytes object.  A binascii.Error is raised if
    s is incorrectly padded.

    If validate is False (the default), characters that are neither in the
    normal base-64 alphabet nor the alternative alphabet are discarded prior
    to the padding check.  If validate is True, these non-alphabet characters
    in the input result in a binascii.Error.
    For more information about the strict base64 check, see:

    https://docs.python.org/3.11/library/binascii.html#binascii.a2b_base64
    """
    raw = _bytes_from_decode_data(s)
    if altchars is not None:
        alt = _bytes_from_decode_data(altchars)
        assert len(alt) == 2, repr(alt)
        raw = raw.translate(bytes.maketrans(alt, b'+/'))
    return binascii.a2b_base64(raw, strict_mode=validate)


def standard_b64encode(s) -> bytes:
    """Encode bytes-like object s using the standard Base64 alphabet.

    The result is returned as a bytes object.
    """
    return b64encode(s)

def standard_b64decode(s) -> bytes:
    """Decode bytes encoded with the standard Base64 alphabet.

    Argument s is a bytes-like object or ASCII string to decode.  The result
    is returned as a bytes object.  A binascii.Error is raised if the input
    is incorrectly padded.  Characters that are not in the standard alphabet
    are discarded prior to the padding check.
    """
    return b64decode(s)


_urlsafe_encode_translation = bytes.maketrans(b'+/', b'-_')
_urlsafe_decode_translation = bytes.maketrans(b'-_', b'+/')

_URLSAFE_BASE64_ALPHABET: bytes = (
    b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
)

def _urlsafe_b64encode_bytes(raw: bytes) -> bytes:
    n = len(raw)
    full = (n // 3) * 3
    rem = n - full
    out_len = (full // 3) * 4 + (4 if rem else 0)
    out = [0] * out_len
    r = 0
    w = 0
    table: bytes = _URLSAFE_BASE64_ALPHABET
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
    elif rem == 2:
        value = (raw[r] << 16) | (raw[r + 1] << 8)
        out[w] = table[(value >> 18) & 63]
        out[w + 1] = table[(value >> 12) & 63]
        out[w + 2] = table[(value >> 6) & 63]
        out[w + 3] = 61
    return bytes_from_values(out)


def urlsafe_b64encode(s) -> bytes:
    """Encode bytes using the URL- and filesystem-safe Base64 alphabet.

    Argument s is a bytes-like object to encode.  The result is returned as a
    bytes object.  The alphabet uses '-' instead of '+' and '_' instead of
    '/'.
    """
    return _urlsafe_b64encode_bytes(binascii._as_bytes(s))


_URLSAFE_BASE64_DECODE: bytes = bytes(
    b - 65
    if 65 <= b <= 90
    else b - 71
    if 97 <= b <= 122
    else b + 4
    if 48 <= b <= 57
    else 62
    if b in (43, 45)
    else 63
    if b in (47, 95)
    else 64
    if b == 61
    else 255
    for b in range(256)
)


def _urlsafe_b64decode_bytes(raw: bytes) -> bytes:
    n = len(raw)
    if n == 0:
        return b""
    table: bytes = _URLSAFE_BASE64_DECODE
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
                raise binascii.Error("Excess data after padding")
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
    if padding > 2:
        raise binascii.Error("Excess data after padding")
    if q_count != 0:
        raise binascii.Error("Incorrect padding")
    if padding:
        del out[w - padding :]
    elif w < len(out):
        del out[w:]
    return bytes_from_values(out)


def urlsafe_b64decode(s) -> bytes:
    """Decode bytes using the URL- and filesystem-safe Base64 alphabet.

    Argument s is a bytes-like object or ASCII string to decode.  The result
    is returned as a bytes object.  A binascii.Error is raised if the input
    is incorrectly padded.  Characters that are not in the URL-safe base-64
    alphabet, and are not a plus '+' or slash '/', are discarded prior to the
    padding check.

    The alphabet uses '-' instead of '+' and '_' instead of '/'.
    """
    return _urlsafe_b64decode_bytes(_bytes_from_decode_data(s))


# Base32 encoding/decoding must be done in Python
_B32_ENCODE_DOCSTRING = '''
Encode the bytes-like objects using {encoding} and return a bytes object.
'''
_B32_DECODE_DOCSTRING = '''
Decode the {encoding} encoded bytes-like object or ASCII string s.

Optional casefold is a flag specifying whether a lowercase alphabet is
acceptable as input.  For security purposes, the default is False.
{extra_args}
The result is returned as a bytes object.  A binascii.Error is raised if
the input is incorrectly padded or if there are non-alphabet
characters present in the input.
'''
_B32_DECODE_MAP01_DOCSTRING = '''
RFC 4648 allows for optional mapping of the digit 0 (zero) to the
letter O (oh), and for optional mapping of the digit 1 (one) to
either the letter I (eye) or letter L (el).  The optional argument
map01 when not None, specifies which letter the digit 1 should be
mapped to (when map01 is not None, the digit 0 is always mapped to
the letter O).  For security purposes the default is None, so that
0 and 1 are not allowed in the input.
'''
_b32alphabet: bytes = b'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567'
_b32hexalphabet: bytes = b'0123456789ABCDEFGHIJKLMNOPQRSTUV'
_b32tab2 = {}
_b32rev = {}

_B32_REV_STD: bytes = bytes(
    b - 65
    if 65 <= b <= 90
    else b - 24
    if 50 <= b <= 55
    else 255
    for b in range(256)
)

_B32_REV_STD_CASEFOLD: bytes = bytes(
    b - 65
    if 65 <= b <= 90
    else b - 97
    if 97 <= b <= 122
    else b - 24
    if 50 <= b <= 55
    else 255
    for b in range(256)
)

_B32_REV_HEX: bytes = bytes(
    b - 48
    if 48 <= b <= 57
    else b - 55
    if 65 <= b <= 86
    else 255
    for b in range(256)
)

_B32_REV_HEX_CASEFOLD: bytes = bytes(
    b - 48
    if 48 <= b <= 57
    else b - 55
    if 65 <= b <= 86
    else b - 87
    if 97 <= b <= 118
    else 255
    for b in range(256)
)


def _b32encode_bytes(alphabet: bytes, raw: bytes) -> bytes:
    n = len(raw)
    if n == 0:
        return b""
    full = (n // 5) * 5
    leftover = n - full
    out_len = (full // 5) * 8 + (8 if leftover else 0)
    out = [0] * out_len
    r = 0
    w = 0
    while r + 4 < n and w + 7 < len(out):
        c = (
            (raw[r] << 32)
            | (raw[r + 1] << 24)
            | (raw[r + 2] << 16)
            | (raw[r + 3] << 8)
            | raw[r + 4]
        )
        out[w] = alphabet[(c >> 35) & 31]
        out[w + 1] = alphabet[(c >> 30) & 31]
        out[w + 2] = alphabet[(c >> 25) & 31]
        out[w + 3] = alphabet[(c >> 20) & 31]
        out[w + 4] = alphabet[(c >> 15) & 31]
        out[w + 5] = alphabet[(c >> 10) & 31]
        out[w + 6] = alphabet[(c >> 5) & 31]
        out[w + 7] = alphabet[c & 31]
        r += 5
        w += 8
    if leftover:
        b0 = raw[r]
        b1 = raw[r + 1] if leftover > 1 else 0
        b2 = raw[r + 2] if leftover > 2 else 0
        b3 = raw[r + 3] if leftover > 3 else 0
        c = (b0 << 32) | (b1 << 24) | (b2 << 16) | (b3 << 8)
        out[w] = alphabet[(c >> 35) & 31]
        out[w + 1] = alphabet[(c >> 30) & 31]
        if leftover == 1:
            out[w + 2] = 61
            out[w + 3] = 61
            out[w + 4] = 61
            out[w + 5] = 61
            out[w + 6] = 61
            out[w + 7] = 61
        elif leftover == 2:
            out[w + 2] = alphabet[(c >> 25) & 31]
            out[w + 3] = alphabet[(c >> 20) & 31]
            out[w + 4] = 61
            out[w + 5] = 61
            out[w + 6] = 61
            out[w + 7] = 61
        elif leftover == 3:
            out[w + 2] = alphabet[(c >> 25) & 31]
            out[w + 3] = alphabet[(c >> 20) & 31]
            out[w + 4] = alphabet[(c >> 15) & 31]
            out[w + 5] = 61
            out[w + 6] = 61
            out[w + 7] = 61
        else:
            out[w + 2] = alphabet[(c >> 25) & 31]
            out[w + 3] = alphabet[(c >> 20) & 31]
            out[w + 4] = alphabet[(c >> 15) & 31]
            out[w + 5] = alphabet[(c >> 10) & 31]
            out[w + 6] = alphabet[(c >> 5) & 31]
            out[w + 7] = 61
    return bytes_from_values(out)


def _b32encode(alphabet: bytes, s) -> bytes:
    return _b32encode_bytes(alphabet, _as_encode_bytes(s))


def _b32decode_bytes(rev: bytes, raw: bytes) -> bytes:
    l = len(raw)
    if l % 8:
        raise binascii.Error('Incorrect padding')
    if l == 0:
        return b""
    end = l
    while end > 0 and raw[end - 1] == 61:
        end -= 1
    padchars = l - end
    if padchars not in (0, 1, 3, 4, 6):
        # Still validate characters before raising Incorrect padding, matching CPython order!
        i = 0
        while i < end:
            if rev[raw[i]] > 31:
                raise binascii.Error('Non-base32 digit found')
            i += 1
        raise binascii.Error('Incorrect padding')

    full_end = end if padchars == 0 else l - 8
    num_full = full_end >> 3
    leftover = (43 - 5 * padchars) // 8 if padchars and end > 0 else 0
    out_len = num_full * 5 + leftover
    out = [0] * out_len
    i = 0
    w = 0
    while i < full_end and w + 4 < len(out):
        d0 = rev[raw[i]]
        d1 = rev[raw[i + 1]]
        d2 = rev[raw[i + 2]]
        d3 = rev[raw[i + 3]]
        d4 = rev[raw[i + 4]]
        d5 = rev[raw[i + 5]]
        d6 = rev[raw[i + 6]]
        d7 = rev[raw[i + 7]]
        if (d0 | d1 | d2 | d3 | d4 | d5 | d6 | d7) > 31:
            raise binascii.Error('Non-base32 digit found')
        acc = (
            (d0 << 35)
            | (d1 << 30)
            | (d2 << 25)
            | (d3 << 20)
            | (d4 << 15)
            | (d5 << 10)
            | (d6 << 5)
            | d7
        )
        out[w] = (acc >> 32) & 0xFF
        out[w + 1] = (acc >> 24) & 0xFF
        out[w + 2] = (acc >> 16) & 0xFF
        out[w + 3] = (acc >> 8) & 0xFF
        out[w + 4] = acc & 0xFF
        i += 8
        w += 5

    if padchars and end > 0:
        acc = 0
        while i < end:
            d = rev[raw[i]]
            if d > 31:
                raise binascii.Error('Non-base32 digit found')
            acc = (acc << 5) | d
            i += 1
        acc <<= 5 * padchars
        if leftover >= 1:
            out[w] = (acc >> 32) & 0xFF
        if leftover >= 2:
            out[w + 1] = (acc >> 24) & 0xFF
        if leftover >= 3:
            out[w + 2] = (acc >> 16) & 0xFF
        if leftover >= 4:
            out[w + 3] = (acc >> 8) & 0xFF
    return bytes_from_values(out)


def _b32decode(alphabet: bytes, s, casefold=False, map01=None) -> bytes:
    raw = _bytes_from_decode_data(s)
    if len(raw) % 8:
        raise binascii.Error('Incorrect padding')
    if map01 is not None:
        map01_bytes = _bytes_from_decode_data(map01)
        assert len(map01_bytes) == 1, repr(map01_bytes)
        raw = _translate_2chars_bytes(raw, 48, 49, 79, map01_bytes[0])
    if alphabet == _b32alphabet:
        rev = _B32_REV_STD_CASEFOLD if casefold else _B32_REV_STD
    elif alphabet == _b32hexalphabet:
        rev = _B32_REV_HEX_CASEFOLD if casefold else _B32_REV_HEX
    else:
        if casefold:
            raw = raw.upper()
        rev_list = [255] * 256
        for k, v in enumerate(alphabet):
            rev_list[v] = k
        rev = bytes_from_values(rev_list)
    return _b32decode_bytes(rev, raw)


def b32encode(s) -> bytes:
    return _b32encode(_b32alphabet, s)
b32encode.__doc__ = _B32_ENCODE_DOCSTRING.format(encoding='base32')

def b32decode(s, casefold=False, map01=None) -> bytes:
    return _b32decode(_b32alphabet, s, casefold, map01)
b32decode.__doc__ = _B32_DECODE_DOCSTRING.format(encoding='base32',
                                        extra_args=_B32_DECODE_MAP01_DOCSTRING)

def b32hexencode(s) -> bytes:
    return _b32encode(_b32hexalphabet, s)
b32hexencode.__doc__ = _B32_ENCODE_DOCSTRING.format(encoding='base32hex')

def b32hexdecode(s, casefold=False) -> bytes:
    # base32hex does not have the 01 mapping
    return _b32decode(_b32hexalphabet, s, casefold)
b32hexdecode.__doc__ = _B32_DECODE_DOCSTRING.format(encoding='base32hex',
                                                    extra_args='')


# RFC 4648, Base 16 Alphabet specifies uppercase, but hexlify() returns
# lowercase.  The RFC also recommends against accepting input case
# insensitively.

_B16_DIGITS: bytes = b"0123456789ABCDEF"

_B16_DECODE: bytes = bytes(
    b - 48
    if 48 <= b <= 57
    else b - 55
    if 65 <= b <= 70
    else 255
    for b in range(256)
)

_B16_DECODE_CASEFOLD: bytes = bytes(
    b - 48
    if 48 <= b <= 57
    else b - 55
    if 65 <= b <= 70
    else b - 87
    if 97 <= b <= 102
    else 255
    for b in range(256)
)


def _b16encode_bytes(raw: bytes) -> bytes:
    n = len(raw)
    if n == 0:
        return b""
    digits: bytes = _B16_DIGITS
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


def b16encode(s) -> bytes:
    """Encode the bytes-like object s using Base16 and return a bytes object.
    """
    return _b16encode_bytes(binascii._as_bytes(s))


def _b16decode_bytes(raw: bytes, casefold: bool) -> bytes:
    n = len(raw)
    if n == 0:
        return b""
    table: bytes = _B16_DECODE_CASEFOLD if casefold else _B16_DECODE
    out = [0] * (n >> 1)
    i = 0
    w = 0
    even_len = n & ~1
    while i < even_len and w < len(out):
        hi = table[raw[i]]
        lo = table[raw[i + 1]]
        if (hi | lo) > 15:
            raise binascii.Error('Non-base16 digit found')
        out[w] = (hi << 4) | lo
        i += 2
        w += 1
    if n & 1:
        if table[raw[n - 1]] > 15:
            raise binascii.Error('Non-base16 digit found')
        raise binascii.Error('Odd-length string')
    return bytes_from_values(out)


def b16decode(s, casefold=False) -> bytes:
    """Decode the Base16 encoded bytes-like object or ASCII string s.

    Optional casefold is a flag specifying whether a lowercase alphabet is
    acceptable as input.  For security purposes, the default is False.

    The result is returned as a bytes object.  A binascii.Error is raised if
    s is incorrectly padded or if there are non-alphabet characters present
    in the input.
    """
    return _b16decode_bytes(_bytes_from_decode_data(s), bool(casefold))


#
# Ascii85 encoding/decoding
#

_a85chars = None
_a85chars2 = None
_A85START = b"<~"
_A85END = b"~>"
_A85_ALPHABET: bytes = bytes(range(33, 118))


def _85encode_bytes(
    raw: bytes,
    alphabet: bytes,
    pad: bool,
    foldnuls: bool,
    foldspaces: bool,
) -> bytes:
    n = len(raw)
    if n == 0:
        return b""
    rem = n & 3
    padding = (4 - rem) & 3
    full = n - rem
    max_len = ((n + 3) >> 2) * 5
    out = [0] * max_len
    r = 0
    w = 0
    while r + 3 < n and w + 4 < len(out):
        word = (
            (raw[r] << 24)
            | (raw[r + 1] << 16)
            | (raw[r + 2] << 8)
            | raw[r + 3]
        )
        r += 4
        if foldnuls and word == 0:
            out[w] = 122  # 'z'
            w += 1
        elif foldspaces and word == 0x20202020:
            out[w] = 121  # 'y'
            w += 1
        else:
            q0 = word // 85
            r0 = word - q0 * 85
            q1 = q0 // 85
            r1 = q0 - q1 * 85
            q2 = q1 // 85
            r2 = q1 - q2 * 85
            q3 = q2 // 85
            r3 = q2 - q3 * 85
            out[w] = alphabet[q3]
            out[w + 1] = alphabet[r3]
            out[w + 2] = alphabet[r2]
            out[w + 3] = alphabet[r1]
            out[w + 4] = alphabet[r0]
            w += 5

    if rem:
        b0 = raw[r]
        b1 = raw[r + 1] if rem > 1 else 0
        b2 = raw[r + 2] if rem > 2 else 0
        word = (b0 << 24) | (b1 << 16) | (b2 << 8)
        if foldnuls and word == 0 and pad:
            out[w] = 122  # 'z'
            w += 1
        elif foldspaces and word == 0x20202020 and pad:
            out[w] = 121  # 'y'
            w += 1
        else:
            q0 = word // 85
            r0 = word - q0 * 85
            q1 = q0 // 85
            r1 = q0 - q1 * 85
            q2 = q1 // 85
            r2 = q1 - q2 * 85
            q3 = q2 // 85
            r3 = q2 - q3 * 85
            if pad:
                out[w] = alphabet[q3]
                out[w + 1] = alphabet[r3]
                out[w + 2] = alphabet[r2]
                out[w + 3] = alphabet[r1]
                out[w + 4] = alphabet[r0]
                w += 5
            else:
                keep = 5 - padding
                out[w] = alphabet[q3]
                out[w + 1] = alphabet[r3]
                if keep >= 3:
                    out[w + 2] = alphabet[r2]
                if keep >= 4:
                    out[w + 3] = alphabet[r1]
                w += keep

    if w < len(out):
        del out[w:]
    return bytes_from_values(out)


def _85encode(b, chars, chars2, pad=False, foldnuls=False, foldspaces=False) -> bytes:
    raw = _as_encode_bytes(b)
    alphabet = bytes(c[0] for c in chars)
    return _85encode_bytes(raw, alphabet, bool(pad), bool(foldnuls), bool(foldspaces))


def _wrap_a85_bytes(raw: bytes, wrapcol: int, adobe: bool) -> bytes:
    if adobe:
        raw = _A85START + raw
    if wrapcol:
        wrapcol = max(2 if adobe else 1, wrapcol)
        n = len(raw)
        if n == 0:
            if adobe:
                return _A85END
            return b""
        num_chunks = (n + wrapcol - 1) // wrapcol
        last_chunk_len = n - (num_chunks - 1) * wrapcol
        extra_nl = 1 if (adobe and last_chunk_len + 2 > wrapcol) else 0
        out_len = n + (num_chunks - 1) + extra_nl + (2 if adobe else 0)
        out = [0] * out_len
        r = 0
        w = 0
        col = 0
        while r < n:
            if col == wrapcol:
                out[w] = 10
                w += 1
                col = 0
            out[w] = raw[r]
            w += 1
            r += 1
            col += 1
        if extra_nl:
            out[w] = 10
            w += 1
        if adobe:
            out[w] = 126  # '~'
            out[w + 1] = 62  # '>'
        return bytes_from_values(out)
    if adobe:
        return raw + _A85END
    return raw


def a85encode(b, *, foldspaces=False, wrapcol=0, pad=False, adobe=False) -> bytes:
    """Encode bytes-like object b using Ascii85 and return a bytes object.

    foldspaces is an optional flag that uses the special short sequence 'y'
    instead of 4 consecutive spaces (ASCII 0x20) as supported by 'btoa'. This
    feature is not supported by the standard encoding used in PDF.

    If wrapcol is non-zero, insert a newline (b'\\n') character after at most
    every wrapcol characters.

    pad controls whether zero-padding applied to the end of the input
    is fully retained in the output encoding, as done by btoa,
    producing an exact multiple of 5 bytes of output.

    adobe controls whether the encoded byte sequence is framed with <~
    and ~>, as in a PostScript base-85 string literal.  Note that
    while ASCII85Decode streams in PDF documents must be terminated
    with ~>, they must not use a leading <~.

    """
    result = _85encode_bytes(
        _as_encode_bytes(b), _A85_ALPHABET, bool(pad), True, bool(foldspaces)
    )
    if adobe or wrapcol:
        return _wrap_a85_bytes(result, int(wrapcol), bool(adobe))
    return result


def _a85decode_bytes(raw: bytes, foldspaces: bool, adobe: bool, ignorechars: bytes) -> bytes:
    if adobe:
        if not raw.endswith(_A85END):
            raise ValueError(
                "Ascii85 encoded byte sequences must end "
                "with {!r}".format(_A85END)
            )
        if raw.startswith(_A85START):
            raw = raw[2:-2]
        else:
            raw = raw[:-2]

    n = len(raw)
    if n == 0:
        return b""
    # In the worst case where every byte is 'z' or 'y', 1 input byte -> 4 output bytes.
    # Usually 5 input bytes -> 4 output bytes. Let's count or size conservatively if no z/y,
    # or allocate n * 4 if small, else count z/y first!
    zy_count = 0
    i = 0
    while i < n:
        c = raw[i]
        if c == 122 or c == 121:
            zy_count += 1
        i += 1
    max_out = ((n - zy_count + 4) // 5) * 4 + zy_count * 4 + 4
    out = [0] * max_out
    w = 0
    curr_len = 0
    acc = 0
    i = 0
    while i < n:
        x = raw[i]
        i += 1
        if 33 <= x <= 117:
            acc = acc * 85 + (x - 33)
            curr_len += 1
            if curr_len == 5:
                if acc > 0xFFFFFFFF:
                    raise ValueError('Ascii85 overflow')
                out[w] = (acc >> 24) & 0xFF
                out[w + 1] = (acc >> 16) & 0xFF
                out[w + 2] = (acc >> 8) & 0xFF
                out[w + 3] = acc & 0xFF
                w += 4
                acc = 0
                curr_len = 0
        elif x == 122:  # 'z'
            if curr_len != 0:
                raise ValueError('z inside Ascii85 5-tuple')
            out[w] = 0
            out[w + 1] = 0
            out[w + 2] = 0
            out[w + 3] = 0
            w += 4
        elif foldspaces and x == 121:  # 'y'
            if curr_len != 0:
                raise ValueError('y inside Ascii85 5-tuple')
            out[w] = 0x20
            out[w + 1] = 0x20
            out[w + 2] = 0x20
            out[w + 3] = 0x20
            w += 4
        elif x in ignorechars:
            continue
        else:
            raise ValueError('Non-Ascii85 digit found: %c' % x)

    if curr_len != 0:
        if curr_len == 1:
            raise ValueError('Ascii85 5-tuple-incomplete (1 byte)')
        padding = 5 - curr_len
        j = 0
        while j < padding:
            acc = acc * 85 + 84  # 'u' - 33 == 84
            j += 1
        if acc > 0xFFFFFFFF:
            raise ValueError('Ascii85 overflow')
        keep = 4 - padding
        if keep >= 1:
            out[w] = (acc >> 24) & 0xFF
        if keep >= 2:
            out[w + 1] = (acc >> 16) & 0xFF
        if keep >= 3:
            out[w + 2] = (acc >> 8) & 0xFF
        w += keep

    if w < len(out):
        del out[w:]
    return bytes_from_values(out)


def a85decode(b, *, foldspaces=False, adobe=False, ignorechars=b' \t\n\r\v') -> bytes:
    """Decode the Ascii85 encoded bytes-like object or ASCII string b.

    foldspaces is a flag that specifies whether the 'y' short sequence
    should be accepted as shorthand for 4 consecutive spaces (ASCII
    0x20).  This feature is not supported by the standard Ascii85
    encoding used in PDF and PostScript.

    adobe controls whether the <~ and ~> markers are present. While
    the leading <~ is not required, the input must end with ~>, or a
    ValueError is raised.

    ignorechars should be a byte string containing characters to ignore from the
    input. This should only contain whitespace characters, and by default
    contains all whitespace characters in ASCII.

    The result is returned as a bytes object.
    """
    raw = _bytes_from_decode_data(b)
    ignore = _as_encode_bytes(ignorechars)
    return _a85decode_bytes(raw, bool(foldspaces), bool(adobe), ignore)


# The following code is originally taken (with permission) from Mercurial

_b85alphabet: bytes = (
    b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    b"abcdefghijklmnopqrstuvwxyz!#$%&()*+-;<=>?@^_`{|}~"
)
_b85chars = None
_b85chars2 = None
_b85dec = None


def _make_b85dec_bytes() -> bytes:
    tbl = [255] * 256
    for i, c in enumerate(_b85alphabet):
        tbl[c] = i
    return bytes(tbl)


_B85_DEC_BYTES: bytes = _make_b85dec_bytes()


def b85encode(b, pad=False) -> bytes:
    """Encode bytes-like object b in base85 format and return a bytes object.

    The input is padded with b'\\0' so its length is a multiple of 4
    bytes before encoding.  If pad is true, all the resulting
    characters are retained in the output, which will always be a
    multiple of 5 bytes.
    """
    return _85encode_bytes(_as_encode_bytes(b), _b85alphabet, bool(pad), False, False)


def _b85decode_bytes(raw: bytes) -> bytes:
    n = len(raw)
    if n == 0:
        return b""
    rem = n % 5
    padding = (5 - rem) % 5
    full_end = n - rem
    num_full = full_end // 5
    keep = 4 - padding if rem else 0
    if rem == 1:
        # 1 byte input padded with 4 '~' (84) always overflows or leaves 0 bytes,
        # check character validity first then overflow!
        pass
    out_len = num_full * 4 + (keep if keep > 0 else 0)
    out = [0] * out_len
    dec: bytes = _B85_DEC_BYTES
    i = 0
    w = 0
    while i < full_end and w + 3 < len(out):
        d0 = dec[raw[i]]
        d1 = dec[raw[i + 1]]
        d2 = dec[raw[i + 2]]
        d3 = dec[raw[i + 3]]
        d4 = dec[raw[i + 4]]
        if (d0 | d1 | d2 | d3 | d4) == 255:
            for j in range(5):
                if dec[raw[i + j]] == 255:
                    raise ValueError('bad base85 character at position %d' % (i + j))
        acc = (((d0 * 85 + d1) * 85 + d2) * 85 + d3) * 85 + d4
        if acc > 0xFFFFFFFF:
            raise ValueError('base85 overflow in hunk starting at byte %d' % i)
        out[w] = (acc >> 24) & 0xFF
        out[w + 1] = (acc >> 16) & 0xFF
        out[w + 2] = (acc >> 8) & 0xFF
        out[w + 3] = acc & 0xFF
        i += 5
        w += 4

    if rem:
        acc = 0
        j = 0
        while j < rem:
            d = dec[raw[i + j]]
            if d == 255:
                raise ValueError('bad base85 character at position %d' % (i + j))
            acc = acc * 85 + d
            j += 1
        while j < 5:
            acc = acc * 85 + 84  # '~' is index 84
            j += 1
        if acc > 0xFFFFFFFF:
            raise ValueError('base85 overflow in hunk starting at byte %d' % i)
        if keep >= 1:
            out[w] = (acc >> 24) & 0xFF
        if keep >= 2:
            out[w + 1] = (acc >> 16) & 0xFF
        if keep >= 3:
            out[w + 2] = (acc >> 8) & 0xFF

    return bytes_from_values(out)


def b85decode(b) -> bytes:
    """Decode the base85-encoded bytes-like object or ASCII string b

    The result is returned as a bytes object.
    """
    return _b85decode_bytes(_bytes_from_decode_data(b))


_z85alphabet = (b'0123456789abcdefghijklmnopqrstuvwxyz'
                b'ABCDEFGHIJKLMNOPQRSTUVWXYZ.-:+=^!/*?&<>()[]{}@%$#')
# Translating b85 valid but z85 invalid chars to b'\x00' is required
# to prevent them from being decoded as b85 valid chars.
_z85_b85_decode_diff = b';_`|~'
_z85_decode_translation = bytes.maketrans(
    _z85alphabet + _z85_b85_decode_diff,
    _b85alphabet + b'\x00' * len(_z85_b85_decode_diff)
)
_z85_encode_translation = bytes.maketrans(_b85alphabet, _z85alphabet)

def z85encode(s) -> bytes:
    """Encode bytes-like object b in z85 format and return a bytes object."""
    return _85encode_bytes(_as_encode_bytes(s), _z85alphabet, False, False, False)

def z85decode(s) -> bytes:
    """Decode the z85-encoded bytes-like object or ASCII string b

    The result is returned as a bytes object.
    """
    s = _bytes_from_decode_data(s)
    s = s.translate(_z85_decode_translation)
    try:
        return b85decode(s)
    except ValueError as e:
        raise ValueError(e.args[0].replace('base85', 'z85')) from None

# Legacy interface.  This code could be cleaned up since I don't believe
# binascii has any line length limitations.  It just doesn't seem worth it
# though.  The files should be opened in binary mode.

MAXLINESIZE = 76 # Excluding the CRLF
MAXBINSIZE = (MAXLINESIZE//4)*3

def encode(input, output):
    """Encode a file; input and output are binary files."""
    while s := input.read(MAXBINSIZE):
        while len(s) < MAXBINSIZE and (ns := input.read(MAXBINSIZE-len(s))):
            s += ns
        line = binascii.b2a_base64(s)
        output.write(line)


def decode(input, output):
    """Decode a file; input and output are binary files."""
    while line := input.readline():
        s = binascii.a2b_base64(line)
        output.write(s)

def _input_type_check(s):
    try:
        m = memoryview(s)
    except TypeError as err:
        msg = "expected bytes-like object, not %s" % s.__class__.__name__
        raise TypeError(msg) from err
    if m.format not in ('c', 'b', 'B'):
        msg = ("expected single byte elements, not %r from %s" %
                                          (m.format, s.__class__.__name__))
        raise TypeError(msg)
    if m.ndim != 1:
        msg = ("expected 1-D data, not %d-D data from %s" %
                                          (m.ndim, s.__class__.__name__))
        raise TypeError(msg)


def encodebytes(s) -> bytes:
    """Encode a bytestring into a bytes object containing multiple lines
    of base-64 data."""
    _input_type_check(s)
    pieces = []
    for i in range(0, len(s), MAXBINSIZE):
        chunk = s[i : i + MAXBINSIZE]
        pieces.append(binascii.b2a_base64(chunk))
    return b"".join(pieces)


def decodebytes(s) -> bytes:
    """Decode a bytestring of base-64 data into a bytes object."""
    _input_type_check(s)
    return binascii.a2b_base64(s)


# Usable as a script...
def main():
    """Small main program"""
    import sys, getopt
    usage = f"""usage: {sys.argv[0]} [-h|-d|-e|-u] [file|-]
        -h: print this help message and exit
        -d, -u: decode
        -e: encode (default)"""
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'hdeu')
    except getopt.error as msg:
        sys.stdout = sys.stderr
        print(msg)
        print(usage)
        sys.exit(2)
    func = encode
    for o, a in opts:
        if o == '-e': func = encode
        if o == '-d': func = decode
        if o == '-u': func = decode
        if o == '-h': print(usage); return
    if args and args[0] != '-':
        with open(args[0], 'rb') as f:
            func(f, sys.stdout.buffer)
    else:
        if sys.stdin.isatty():
            # gh-138775: read terminal input data all at once to detect EOF
            import io
            data = sys.stdin.buffer.read()
            buffer = io.BytesIO(data)
        else:
            buffer = sys.stdin.buffer
        func(buffer, sys.stdout.buffer)


if __name__ == '__main__':
    main()
