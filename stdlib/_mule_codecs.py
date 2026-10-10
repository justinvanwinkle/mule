"""Compiled Unicode decoding over explicitly imported physical byte storage."""

from _mule_native import byte_length, read_byte, text_from_codepoints


def utf8_decode(data):
    """Decode byte storage strictly, without a host codec or argument-specific API."""
    length = byte_length(data)
    codepoints = []
    offset = 0
    while offset < length:
        first = read_byte(data, offset)
        if first < 0x80:
            codepoints.append(first)
            offset += 1
            continue
        if 0xC2 <= first <= 0xDF:
            width = 2
            codepoint = first & 0x1F
            minimum = 0x80
        elif 0xE0 <= first <= 0xEF:
            width = 3
            codepoint = first & 0x0F
            minimum = 0x800
        elif 0xF0 <= first <= 0xF4:
            width = 4
            codepoint = first & 0x07
            minimum = 0x10000
        else:
            raise UnicodeDecodeError(
                "utf-8", data, offset, offset + 1, "invalid UTF-8 leading byte"
            )
        if offset + width > length:
            raise UnicodeDecodeError(
                "utf-8", data, offset, length, "incomplete UTF-8 sequence"
            )
        continuation = 1
        while continuation < width:
            byte = read_byte(data, offset + continuation)
            if byte < 0x80 or byte > 0xBF:
                raise UnicodeDecodeError(
                    "utf-8",
                    data,
                    offset,
                    offset + continuation,
                    "invalid UTF-8 continuation byte",
                )
            codepoint = (codepoint << 6) | (byte & 0x3F)
            continuation += 1
        if codepoint < minimum or codepoint > 0x10FFFF:
            raise UnicodeDecodeError(
                "utf-8",
                data,
                offset,
                offset + width,
                "UTF-8 codepoint outside permitted range",
            )
        if 0xD800 <= codepoint <= 0xDFFF:
            raise UnicodeDecodeError(
                "utf-8",
                data,
                offset,
                offset + width,
                "UTF-8 encodes a surrogate codepoint",
            )
        codepoints.append(codepoint)
        offset += width
    return text_from_codepoints(codepoints)
