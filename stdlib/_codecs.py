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
    "utf_8_encode",
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
    if isinstance(exc, UnicodeEncodeError):
        return "".join("\\x%02x" % ord(c) for c in exc.object[exc.start:exc.end]), exc.end
    return "\ufffd", exc.end


def _xmlcharrefreplace_errors(exc):
    return "".join("&#%d;" % ord(c) for c in exc.object[exc.start:exc.end]), exc.end


def _namereplace_errors(exc):
    return "".join("\\N{%s}" % (getattr(c, "name", "UNKNOWN")) for c in exc.object[exc.start:exc.end]), exc.end


def _surrogateescape_errors(exc):
    raise exc


def _surrogatepass_errors(exc):
    raise exc


_search_functions = []
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


def register(search_function):
    _search_functions.append(search_function)


def unregister(search_function):
    if search_function in _search_functions:
        _search_functions.remove(search_function)


def lookup(encoding):
    raise LookupError(f"unknown encoding: {encoding}")


def register_error(name, handler):
    _error_handlers[name] = handler


def lookup_error(name):
    if name in _error_handlers:
        return _error_handlers[name]
    raise LookupError(f"unknown error handler name '{name}'")


def encode(value, encoding="utf-8", errors="strict"):
    if isinstance(value, str):
        result = value.encode(encoding, errors)
        return result, len(value)
    if isinstance(value, (bytes, bytearray)):
        if encoding in ("utf-8", "utf8", "u8", "utf", "utf_8"):
            return bytes(value), len(value)
        result = bytes(value).decode(encoding, errors).encode(encoding, errors)
        return result, len(value)
    raise TypeError("encode() argument must be str or bytes")


def decode(value, encoding="utf-8", errors="strict"):
    if isinstance(value, str):
        return value, len(value)
    result = bytes(value).decode(encoding, errors)
    return result, len(value)


def _encode_as(value, encoding, errors="strict"):
    if not isinstance(value, str):
        value = str(value)
    return value.encode(encoding, errors), len(value)


def _decode_as(value, encoding, errors="strict"):
    data = bytes(value)
    return data.decode(encoding, errors), len(data)


def utf_8_encode(value, errors="strict"):
    return _encode_as(value, "utf-8", errors)


def utf_8_decode(value, errors="strict", final=False):
    return _decode_as(value, "utf-8", errors)


def ascii_encode(value, errors="strict"):
    return _encode_as(value, "ascii", errors)


def ascii_decode(value, errors="strict", final=False):
    return _decode_as(value, "ascii", errors)


def latin_1_encode(value, errors="strict"):
    return _encode_as(value, "latin-1", errors)


def latin_1_decode(value, errors="strict", final=False):
    return _decode_as(value, "latin-1", errors)


def utf_16_encode(value, errors="strict"):
    return _encode_as(value, "utf-16", errors)


def utf_16_decode(value, errors="strict", final=False):
    return _decode_as(value, "utf-16", errors)


def utf_16_le_encode(value, errors="strict"):
    return _encode_as(value, "utf-16-le", errors)


def utf_16_le_decode(value, errors="strict", final=False):
    return _decode_as(value, "utf-16-le", errors)


def utf_16_be_encode(value, errors="strict"):
    return _encode_as(value, "utf-16-be", errors)


def utf_16_be_decode(value, errors="strict", final=False):
    return _decode_as(value, "utf-16-be", errors)


def utf_16_ex_decode(value, errors="strict", final=False):
    data = bytes(value)
    if data[:2] == b"\xff\xfe":
        return data[2:].decode("utf-16-le", errors), len(data)
    if data[:2] == b"\xfe\xff":
        return data[2:].decode("utf-16-be", errors), len(data)
    return _decode_as(value, "utf-16-le", errors)


def utf_32_encode(value, errors="strict"):
    return _encode_as(value, "utf-32", errors)


def utf_32_decode(value, errors="strict", final=False):
    return _decode_as(value, "utf-32", errors)


def utf_32_le_encode(value, errors="strict"):
    return _encode_as(value, "utf-32-le", errors)


def utf_32_le_decode(value, errors="strict", final=False):
    return _decode_as(value, "utf-32-le", errors)


def utf_32_be_encode(value, errors="strict"):
    return _encode_as(value, "utf-32-be", errors)


def utf_32_be_decode(value, errors="strict", final=False):
    return _decode_as(value, "utf-32-be", errors)


def utf_32_ex_decode(value, errors="strict", final=False):
    data = bytes(value)
    if data[:4] == b"\xff\xfe\x00\x00":
        return data[4:].decode("utf-32-le", errors), len(data)
    if data[:4] == b"\x00\x00\xfe\xff":
        return data[4:].decode("utf-32-be", errors), len(data)
    return _decode_as(value, "utf-32-le", errors)


def utf_7_encode(value, errors="strict"):
    return _encode_as(value, "utf-7", errors)


def utf_7_decode(value, errors="strict", final=False):
    return _decode_as(value, "utf-7", errors)


def charmap_encode(value, errors="strict", mapping=None):
    result = bytearray()
    for char in value:
        code = ord(char)
        mapped = None
        if mapping is not None:
            if isinstance(mapping, (str, bytes)):
                mapped = mapping[code] if code < len(mapping) else None
                if isinstance(mapped, str):
                    mapped = ord(mapped)
            else:
                mapped = mapping.get(code)
        else:
            mapped = code if code < 256 else None
        if mapped is None:
            if errors == "ignore":
                continue
            if errors == "replace":
                result.append(63)
                continue
            raise UnicodeEncodeError("charmap", value, 0, 1, "character maps to <undefined>")
        result.append(mapped)
    return bytes(result), len(value)


def charmap_decode(value, errors="strict", mapping=None):
    data = bytes(value)
    result = []
    for byte in data:
        if mapping is None:
            result.append(chr(byte))
            continue
        if isinstance(mapping, (str, bytes)):
            char = mapping[byte]
            result.append(char if isinstance(char, str) else chr(char))
        else:
            mapped = mapping.get(byte)
            if mapped is None:
                if errors == "ignore":
                    continue
                if errors == "replace":
                    result.append("\ufffd")
                    continue
                raise UnicodeDecodeError("charmap", data, 0, 1, "character maps to <undefined>")
            result.append(chr(mapped) if isinstance(mapped, int) else mapped)
    return "".join(result), len(data)


def charmap_build(mapping):
    return mapping


_ESCAPES = {
    "n": "\n", "t": "\t", "r": "\r", "\\": "\\", "'": "'", '"': '"',
    "a": "\a", "b": "\b", "f": "\f", "v": "\v", "0": "\0",
}


def unicode_escape_encode(value, errors="strict"):
    return value.encode("unicode_escape", errors), len(value)


def unicode_escape_decode(value, errors="strict", final=False):
    data = bytes(value)
    return data.decode("unicode_escape", errors), len(data)


def raw_unicode_escape_encode(value, errors="strict"):
    return value.encode("raw_unicode_escape", errors), len(value)


def raw_unicode_escape_decode(value, errors="strict", final=False):
    data = bytes(value)
    return data.decode("raw_unicode_escape", errors), len(data)


def escape_encode(value, errors="strict"):
    data = bytes(value)
    return data.decode("unicode_escape", errors).encode("latin-1", errors), len(data)


def escape_decode(value, errors="strict", final=False):
    data = bytes(value)
    return data.decode("unicode_escape", errors).encode("latin-1", errors), len(data)


def readbuffer_encode(value, errors="strict"):
    data = bytes(value)
    return data, len(data)
