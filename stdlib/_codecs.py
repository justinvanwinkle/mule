"""Stateless codec functions, as CPython's builtin ``_codecs`` module.

The conversions delegate to the str and bytes codec implementations;
the registry functions are provided for ``codecs`` and encoding modules.
"""

__all__ = [
    "ascii_decode", "ascii_encode", "charmap_build", "charmap_decode",
    "charmap_encode", "decode", "encode", "escape_decode", "escape_encode",
    "latin_1_decode", "latin_1_encode", "lookup", "lookup_error",
    "raw_unicode_escape_decode", "raw_unicode_escape_encode",
    "readbuffer_encode", "register", "register_error", "unicode_escape_decode",
    "unicode_escape_encode", "utf_16_be_decode", "utf_16_be_encode",
    "utf_16_decode", "utf_16_encode", "utf_16_ex_decode", "utf_16_le_decode",
    "utf_16_le_encode", "utf_32_be_decode", "utf_32_be_encode",
    "utf_32_decode", "utf_32_encode", "utf_32_ex_decode", "utf_32_le_decode",
    "utf_32_le_encode", "utf_7_decode", "utf_7_encode", "utf_8_decode",
    "utf_8_encode", "unregister",
]

def _strict_errors(exc):
    raise exc


def _ignore_errors(exc):
    return "", exc.end


def _replace_errors(exc):
    if isinstance(exc, UnicodeEncodeError):
        return "?", exc.end
    return "\ufffd", exc.end


def _backslashreplace_errors(exc):
    from _str import _escaped_character

    if isinstance(exc, UnicodeEncodeError):
        return "".join(_escaped_character(ord(c)) for c in exc.object[exc.start:exc.end]), exc.end
    return "".join("\\x%02x" % byte for byte in exc.object[exc.start:exc.end]), exc.end


def _xmlcharrefreplace_errors(exc):
    return "".join("&#%d;" % ord(c) for c in exc.object[exc.start:exc.end]), exc.end


def _namereplace_errors(exc):
    from unicodedata import name as _ucd_name
    from _str import _escaped_character

    pieces = []
    for c in exc.object[exc.start:exc.end]:
        char_name = _ucd_name(c, "")
        if char_name:
            pieces.append("\\N{" + char_name + "}")
        else:
            pieces.append(_escaped_character(ord(c)))
    return "".join(pieces), exc.end


def _surrogateescape_errors(exc):
    if isinstance(exc, UnicodeDecodeError):
        values = exc.object[exc.start:exc.end]
        if all(byte >= 128 for byte in values):
            return ''.join(chr(byte + 0xDC00) for byte in values), exc.end
    raise exc


def _surrogatepass_errors(exc):
    raise exc


_search_functions = []
_codec_cache = {}
_initialized = False
_initializing = False
_error_handlers = {
    "strict": _strict_errors,
    "ignore": _ignore_errors,
    "replace": _replace_errors,
    "backslashreplace": _backslashreplace_errors,
    "xmlcharrefreplace": _xmlcharrefreplace_errors,
    "namereplace": _namereplace_errors,
    "surrogateescape": _surrogateescape_errors,
    "surrogatepass": _surrogatepass_errors,
}


def _initialize():
    global _initialized, _initializing
    if _initialized or _initializing:
        return
    _initializing = True
    try:
        import encodings
        _initialized = True
    finally:
        _initializing = False


def register(search_function, /):
    if not callable(search_function):
        raise TypeError('argument must be callable')
    _initialize()
    _search_functions.append(search_function)


def unregister(search_function, /):
    if search_function in _search_functions:
        _search_functions.remove(search_function)
        _codec_cache.clear()


def _normalize_encoding(encoding):
    pieces = []
    separator = False
    for character in encoding:
        if character == '\x00':
            raise ValueError('embedded null character')
        if character == '.' or character.isascii() and character.isalnum():
            if separator and pieces:
                pieces.append('_')
            pieces.append(character.lower())
            separator = False
        else:
            separator = True
    return ''.join(pieces)


def lookup(encoding, /):
    if not isinstance(encoding, str):
        raise TypeError('lookup() argument must be str, not ' + type(encoding).__name__)
    normalized = _normalize_encoding(encoding)
    _initialize()
    cached = _codec_cache.get(normalized)
    if cached is not None:
        return cached
    for search in tuple(_search_functions):
        result = search(normalized)
        if result is not None:
            if not isinstance(result, tuple) or len(result) != 4:
                raise TypeError('codec search functions must return 4-tuples')
            _codec_cache[normalized] = result
            return result
    raise LookupError(f"unknown encoding: {encoding}")


def register_error(name, handler):
    _error_handlers[name] = handler


def lookup_error(name):
    if name in _error_handlers:
        return _error_handlers[name]
    raise LookupError(f"unknown error handler name '{name}'")


def encode(value, encoding="utf-8", errors="strict"):
    result = lookup(encoding)[0](value, errors)
    if not isinstance(result, tuple) or len(result) != 2:
        raise TypeError('encoder must return a tuple (object, integer)')
    return result[0]


def decode(value, encoding="utf-8", errors="strict"):
    result = lookup(encoding)[1](value, errors)
    if not isinstance(result, tuple) or len(result) != 2:
        raise TypeError('decoder must return a tuple (object, integer)')
    return result[0]


def _text_codec(encoding, operation):
    info = lookup(encoding)
    if not getattr(info, '_is_text_encoding', True):
        raise LookupError(
            repr(encoding) + ' is not a text encoding; use codecs.' + operation
            + '() to handle arbitrary codecs'
        )
    return tuple.__getitem__(info, 0 if operation == 'encode' else 1)


def _encode_text(value, encoding, errors, supplied):
    encoder = _text_codec(encoding, 'encode')
    result = encoder(value, errors) if supplied else encoder(value)
    if not isinstance(result, tuple) or tuple.__len__(result) != 2:
        raise TypeError('encoder must return a tuple (object, integer)')
    output = tuple.__getitem__(result, 0)
    if isinstance(output, bytes):
        return output
    if isinstance(output, bytearray):
        import warnings
        from _mule_native import instance_field, bytes_from_values
        warnings.warn(
            'encoder ' + encoding + ' returned bytearray instead of bytes; '
            'use codecs.encode() to encode to arbitrary types',
            RuntimeWarning,
        )
        return bytes_from_values(instance_field(output, 'values'))
    raise TypeError(
        repr(encoding) + ' encoder returned ' + repr(type(output).__name__)
        + " instead of 'bytes'; use codecs.encode() to encode to arbitrary types"
    )


def _decode_text(value, encoding, errors, supplied):
    decoder = _text_codec(encoding, 'decode')
    view = memoryview(value)
    result = decoder(view, errors) if supplied else decoder(view)
    if not isinstance(result, tuple) or tuple.__len__(result) != 2:
        raise TypeError('decoder must return a tuple (object,integer)')
    output = tuple.__getitem__(result, 0)
    if not isinstance(output, str):
        raise TypeError(
            repr(encoding) + ' decoder returned ' + repr(type(output).__name__)
            + " instead of 'str'; use codecs.decode() to decode to arbitrary types"
        )
    return output


def _encode_as(value, encoding, errors="strict"):
    from _str import _codec_name
    if _codec_name(encoding) is None:
        raise LookupError(f"unknown encoding: {encoding}")
    if not isinstance(value, str):
        value = str(value)
    return value.encode(encoding, errors), len(value)


def _decode_as(value, encoding, errors="strict"):
    from _str import _codec_name
    if _codec_name(encoding) is None:
        raise LookupError(f"unknown encoding: {encoding}")
    data = bytes(value)
    return data.decode(encoding, errors), len(data)


def utf_8_encode(value, errors="strict"):
    return _encode_as(value, "utf-8", errors)


def utf_8_decode(value, errors="strict", final=False):
    from _str import _utf8_sequence
    data = bytes(value)
    size = len(data)
    consumed = size
    if not final and size:
        start = size - 1
        while start > 0 and size - start < 4 and 0x80 <= data[start] <= 0xBF:
            start -= 1
        # An incomplete surrogate sequence is buffered until it is complete;
        # decoding below still applies the requested error policy.
        code, length = _utf8_sequence(data, start, size, True)
        if code == -2:
            consumed = start
    return data[:consumed].decode('utf-8', errors), consumed


def ascii_encode(value, errors="strict"):
    return _encode_as(value, "ascii", errors)


def ascii_decode(value, errors="strict", final=False):
    return _decode_as(value, "ascii", errors)


def latin_1_encode(value, errors="strict"):
    return _encode_as(value, "latin-1", errors)


def latin_1_decode(value, errors="strict", final=False):
    return _decode_as(value, "latin-1", errors)


def _utf_16_encode_bytes(value, errors, little_endian, bom):
    if not isinstance(value, str):
        value = str(value)
    result = bytearray()
    if bom:
        result.extend(b"\xff\xfe" if little_endian else b"\xfe\xff")
    index = 0
    size = len(value)
    while index < size:
        code = ord(value[index])
        if 0xD800 <= code <= 0xDFFF and errors != "surrogatepass":
            exc = UnicodeEncodeError("utf-16", value, index, index + 1, "surrogates not allowed")
            replacement, index = lookup_error(errors)(exc)
            if isinstance(replacement, str):
                result.extend(_utf_16_encode_bytes(replacement, "strict", little_endian, False)[0])
            else:
                result.extend(replacement)
            continue
        if code < 0x10000:
            if little_endian:
                result.append(code & 0xFF)
                result.append((code >> 8) & 0xFF)
            else:
                result.append((code >> 8) & 0xFF)
                result.append(code & 0xFF)
        else:
            part = code - 0x10000
            high = 0xD800 | (part >> 10)
            low = 0xDC00 | (part & 0x3FF)
            if little_endian:
                result.extend((high & 0xFF, (high >> 8) & 0xFF, low & 0xFF, (low >> 8) & 0xFF))
            else:
                result.extend(((high >> 8) & 0xFF, high & 0xFF, (low >> 8) & 0xFF, low & 0xFF))
        index += 1
    return bytes(result), size


def _utf_16_decode_bytes(data, errors, little_endian, final):
    data = bytes(data)
    size = len(data)
    pieces = []
    index = 0
    while index < size:
        if index + 1 >= size:
            if not final:
                break
            exc = UnicodeDecodeError("utf-16", data, index, size, "truncated data")
            replacement, index = lookup_error(errors)(exc)
            pieces.append(replacement)
            continue
        b0 = data[index]
        b1 = data[index + 1]
        word = (b0 | (b1 << 8)) if little_endian else ((b0 << 8) | b1)
        if 0xD800 <= word <= 0xDBFF:
            if index + 3 >= size:
                if not final:
                    break
                exc = UnicodeDecodeError("utf-16", data, index, index + 2, "unexpected end of data")
                replacement, index = lookup_error(errors)(exc)
                pieces.append(replacement)
                continue
            b2 = data[index + 2]
            b3 = data[index + 3]
            word2 = (b2 | (b3 << 8)) if little_endian else ((b2 << 8) | b3)
            if 0xDC00 <= word2 <= 0xDFFF:
                pieces.append(chr(0x10000 + ((word - 0xD800) << 10) + (word2 - 0xDC00)))
                index += 4
                continue
            if errors == "surrogatepass":
                pieces.append(chr(word))
                index += 2
                continue
            exc = UnicodeDecodeError("utf-16", data, index, index + 2, "illegal UTF-16 surrogate")
            replacement, index = lookup_error(errors)(exc)
            pieces.append(replacement)
            continue
        if 0xDC00 <= word <= 0xDFFF and errors != "surrogatepass":
            exc = UnicodeDecodeError("utf-16", data, index, index + 2, "illegal encoding")
            replacement, index = lookup_error(errors)(exc)
            pieces.append(replacement)
            continue
        pieces.append(chr(word))
        index += 2
    return "".join(pieces), index


def utf_16_encode(value, errors="strict", byteorder=0):
    if byteorder == 0:
        return _utf_16_encode_bytes(value, errors, True, True)
    return _utf_16_encode_bytes(value, errors, byteorder < 0, False)


def utf_16_decode(value, errors="strict", final=False):
    text, consumed, _ = utf_16_ex_decode(value, errors, 0, final)
    return text, consumed


def utf_16_le_encode(value, errors="strict"):
    return _utf_16_encode_bytes(value, errors, True, False)


def utf_16_le_decode(value, errors="strict", final=False):
    return _utf_16_decode_bytes(value, errors, True, final)


def utf_16_be_encode(value, errors="strict"):
    return _utf_16_encode_bytes(value, errors, False, False)


def utf_16_be_decode(value, errors="strict", final=False):
    return _utf_16_decode_bytes(value, errors, False, final)


def utf_16_ex_decode(value, errors="strict", byteorder=0, final=False):
    data = bytes(value)
    order = byteorder
    offset = 0
    if order == 0:
        if len(data) < 2 and not final:
            return "", 0, 0
        if data[:2] == b"\xff\xfe":
            order = -1
            offset = 2
        elif data[:2] == b"\xfe\xff":
            order = 1
            offset = 2
    little = order <= 0
    text, consumed = _utf_16_decode_bytes(data[offset:], errors, little, final)
    return text, offset + consumed, order


def _utf_32_encode_bytes(value, errors, little_endian, bom):
    if not isinstance(value, str):
        value = str(value)
    result = bytearray()
    if bom:
        result.extend(b"\xff\xfe\x00\x00" if little_endian else b"\x00\x00\xfe\xff")
    index = 0
    size = len(value)
    while index < size:
        code = ord(value[index])
        if 0xD800 <= code <= 0xDFFF and errors != "surrogatepass":
            exc = UnicodeEncodeError("utf-32", value, index, index + 1, "surrogates not allowed")
            replacement, index = lookup_error(errors)(exc)
            if isinstance(replacement, str):
                result.extend(_utf_32_encode_bytes(replacement, "strict", little_endian, False)[0])
            else:
                result.extend(replacement)
            continue
        if little_endian:
            result.extend((code & 0xFF, (code >> 8) & 0xFF, (code >> 16) & 0xFF, (code >> 24) & 0xFF))
        else:
            result.extend(((code >> 24) & 0xFF, (code >> 16) & 0xFF, (code >> 8) & 0xFF, code & 0xFF))
        index += 1
    return bytes(result), size


def _utf_32_decode_bytes(data, errors, little_endian, final):
    data = bytes(data)
    size = len(data)
    pieces = []
    index = 0
    while index < size:
        if index + 3 >= size:
            if not final:
                break
            exc = UnicodeDecodeError("utf-32", data, index, size, "truncated data")
            replacement, index = lookup_error(errors)(exc)
            pieces.append(replacement)
            continue
        b0, b1, b2, b3 = data[index], data[index + 1], data[index + 2], data[index + 3]
        if little_endian:
            code = b0 | (b1 << 8) | (b2 << 16) | (b3 << 24)
        else:
            code = (b0 << 24) | (b1 << 16) | (b2 << 8) | b3
        if code > 0x10FFFF or (0xD800 <= code <= 0xDFFF and errors != "surrogatepass"):
            exc = UnicodeDecodeError("utf-32", data, index, index + 4, "code point out of range")
            replacement, index = lookup_error(errors)(exc)
            pieces.append(replacement)
            continue
        pieces.append(chr(code))
        index += 4
    return "".join(pieces), index


def utf_32_encode(value, errors="strict", byteorder=0):
    if byteorder == 0:
        return _utf_32_encode_bytes(value, errors, True, True)
    return _utf_32_encode_bytes(value, errors, byteorder < 0, False)


def utf_32_decode(value, errors="strict", final=False):
    text, consumed, _ = utf_32_ex_decode(value, errors, 0, final)
    return text, consumed


def utf_32_le_encode(value, errors="strict"):
    return _utf_32_encode_bytes(value, errors, True, False)


def utf_32_le_decode(value, errors="strict", final=False):
    return _utf_32_decode_bytes(value, errors, True, final)


def utf_32_be_encode(value, errors="strict"):
    return _utf_32_encode_bytes(value, errors, False, False)


def utf_32_be_decode(value, errors="strict", final=False):
    return _utf_32_decode_bytes(value, errors, False, final)


def utf_32_ex_decode(value, errors="strict", byteorder=0, final=False):
    data = bytes(value)
    order = byteorder
    offset = 0
    if order == 0:
        if len(data) < 4 and not final:
            return "", 0, 0
        if data[:4] == b"\xff\xfe\x00\x00":
            order = -1
            offset = 4
        elif data[:4] == b"\x00\x00\xfe\xff":
            order = 1
            offset = 4
    little = order <= 0
    text, consumed = _utf_32_decode_bytes(data[offset:], errors, little, final)
    return text, offset + consumed, order


def utf_7_encode(value, errors="strict"):
    return _encode_as(value, "utf-7", errors)


def utf_7_decode(value, errors="strict", final=False):
    return _decode_as(value, "utf-7", errors)


def charmap_encode(value, errors="strict", mapping=None):
    result = bytearray()
    index = 0
    size = len(value)
    while index < size:
        char = value[index]
        code = ord(char)
        mapped = None
        if mapping is not None:
            if isinstance(mapping, (str, bytes)):
                mapped = mapping[code] if code < len(mapping) else None
                if isinstance(mapped, str):
                    mapped = None if mapped == "\ufffe" else ord(mapped)
            else:
                mapped = mapping.get(code)
        else:
            mapped = code if code < 256 else None
        if mapped is None:
            if errors == "ignore":
                index += 1
                continue
            if errors == "replace":
                result.append(63)
                index += 1
                continue
            exc = UnicodeEncodeError("charmap", value, index, index + 1, "character maps to <undefined>")
            replacement, index = lookup_error(errors)(exc)
            if isinstance(replacement, str):
                result.extend(charmap_encode(replacement, "strict", mapping)[0])
            else:
                result.extend(replacement)
            continue
        result.append(mapped)
        index += 1
    return bytes(result), size


def charmap_decode(value, errors="strict", mapping=None):
    data = bytes(value)
    result = []
    index = 0
    size = len(data)
    while index < size:
        byte = data[index]
        if mapping is None:
            result.append(chr(byte))
            index += 1
            continue
        if isinstance(mapping, (str, bytes)):
            char = mapping[byte] if byte < len(mapping) else "\ufffe"
            mapped = char if isinstance(char, str) else chr(char)
            if mapped == "\ufffe":
                mapped = None
        else:
            mapped = mapping.get(byte)
            if isinstance(mapped, int):
                mapped = None if mapped == 0xFFFE else chr(mapped)
        if mapped is None:
            if errors == "ignore":
                index += 1
                continue
            if errors == "replace":
                result.append("\ufffd")
                index += 1
                continue
            exc = UnicodeDecodeError("charmap", data, index, index + 1, "character maps to <undefined>")
            replacement, index = lookup_error(errors)(exc)
            result.append(replacement)
            continue
        result.append(mapped)
        index += 1
    return "".join(result), size


def charmap_build(mapping):
    return {ord(ch): i for i, ch in enumerate(mapping) if ch != "\ufffe"}


_ESCAPES = {
    "n": "\n", "t": "\t", "r": "\r", "\\": "\\", "'": "'", '"': '"',
    "a": "\a", "b": "\b", "f": "\f", "v": "\v", "0": "\0",
}


def unicode_escape_encode(value, errors="strict"):
    return value.encode("unicode_escape", errors), len(value)


def unicode_escape_decode(value, errors="strict", final=False):
    from _str import _unicode_escape_decode

    data = bytes(value)
    return _unicode_escape_decode(data, errors, final)


def raw_unicode_escape_encode(value, errors="strict"):
    return value.encode("raw_unicode_escape", errors), len(value)


def raw_unicode_escape_decode(value, errors="strict", final=False):
    from _str import _unicode_escape_decode

    data = bytes(value)
    return _unicode_escape_decode(data, errors, final, True)


def escape_encode(value, errors="strict"):
    data = bytes(value)
    return data.decode("unicode_escape", errors).encode("latin-1", errors), len(data)


def escape_decode(value, errors="strict", final=False):
    data = bytes(value)
    return data.decode("unicode_escape", errors).encode("latin-1", errors), len(data)


def readbuffer_encode(value, errors="strict"):
    data = bytes(value)
    return data, len(data)


# These public codec services emulate builtin functions: storing one on a
# codec class must not add an instance argument. Their algorithms remain Python.
for _builtin_name in __all__:
    globals()[_builtin_name] = staticmethod(globals()[_builtin_name])
del _builtin_name
