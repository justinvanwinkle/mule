"""The ``str`` type's methods and operators, compiled by Mule.

``TextStorage`` is the method table of the runtime's physical ``str`` storage.
Its algorithms follow CPython's ``Objects/unicodeobject.c`` and run as compiled
Python over constant-time codepoint access. The underscored names are physical
primitives bound by the compiler: storage length, codepoint reads, substring
copies, and concatenation of finished pieces.
"""

from _mule_native import bytes_from_values as _bytes_from_values
from _mule_native import byte_length as _byte_length
from _mule_native import read_byte as _read_byte
from _mule_native import text_from_codepoints as _from_codepoints
from _slice import is_slice, positions
from _unicode_data import (
    fold_full,
    is_alpha,
    is_case_ignorable,
    is_cased,
    is_decimal,
    is_digit,
    is_linebreak,
    is_lower,
    is_numeric,
    is_printable,
    is_space,
    is_titlecase,
    is_upper,
    is_xid_continue,
    is_xid_start,
    lower_full,
    title_full,
    upper_full,
)

# Compiler-provided physical primitives are not host Python globals.
# ruff: noqa: F821

_HEX_DIGITS = "0123456789abcdef"


def _index(value):
    method = _index_method(value)
    if method is not None:
        value = method()
    try:
        return _integer_storage(value)
    except TypeError:
        raise TypeError(
            f"'{_type_name(value)}' object cannot be interpreted as an integer"
        )


def _require_text(value, message):
    if not _text_storage(value):
        raise TypeError(f"{message}, not {_type_name(value)}")


def _bounds(length, start, end):
    """Clamp optional ``start``/``end`` like slice indices, as CPython's
    ``ADJUST_INDICES`` does; ``start`` may still exceed ``length``."""
    if start is None:
        start = 0
    else:
        start = _index(start)
        if start < 0:
            start += length
            if start < 0:
                start = 0
    if end is None:
        end = length
    else:
        end = _index(end)
        if end > length:
            end = length
        elif end < 0:
            end += length
            if end < 0:
                end = 0
    return start, end


def _matches_at(text, sub, position, size):
    index = 0
    while index < size:
        if _text_codepoint(text, position + index) != _text_codepoint(sub, index):
            return False
        index += 1
    return True


def find_in(text, sub, start, end):
    """The first index of ``sub`` within clamped ``start:end``, or -1."""
    size = _text_length(sub)
    if end - start < size:
        return -1
    if size == 0:
        return start
    first = _text_codepoint(sub, 0)
    position = start
    last = end - size
    while position <= last:
        if _text_codepoint(text, position) == first and _matches_at(
            text, sub, position, size
        ):
            return position
        position += 1
    return -1


def rfind_in(text, sub, start, end):
    size = _text_length(sub)
    if end - start < size:
        return -1
    if size == 0:
        return end
    first = _text_codepoint(sub, 0)
    position = end - size
    while position >= start:
        if _text_codepoint(text, position) == first and _matches_at(
            text, sub, position, size
        ):
            return position
        position -= 1
    return -1


def _search(text, sub, start, end, method):
    _require_text(sub, f"{method}() argument 1 must be str")
    start, end = _bounds(_text_length(text), start, end)
    if method == "find" or method == "index":
        return find_in(text, sub, start, end)
    return rfind_in(text, sub, start, end)


def _contains_codepoint(text, code):
    index = 0
    size = _text_length(text)
    while index < size:
        if _text_codepoint(text, index) == code:
            return True
        index += 1
    return False


def _extend_mapping(codes, mapped):
    if _tuple_storage(mapped):
        for code in mapped:
            codes.append(code)
    else:
        codes.append(mapped)


def _final_sigma(text, index, length):
    """Lowercase U+03A3 by Unicode's Final_Sigma context, as CPython's
    ``handle_capital_sigma`` does."""
    position = index - 1
    while position >= 0 and is_case_ignorable(_text_codepoint(text, position)):
        position -= 1
    final = position >= 0 and is_cased(_text_codepoint(text, position))
    if final and index + 1 < length:
        position = index + 1
        while position < length and is_case_ignorable(
            _text_codepoint(text, position)
        ):
            position += 1
        final = position == length or not is_cased(_text_codepoint(text, position))
    if final:
        return 0x3C2
    return 0x3C3


def _lower_into(codes, text, index, length):
    code = _text_codepoint(text, index)
    if code == 0x3A3:
        codes.append(_final_sigma(text, index, length))
    else:
        _extend_mapping(codes, lower_full(code))


def _all_codepoints(text, predicate, empty):
    size = _text_length(text)
    if size == 0:
        return empty
    index = 0
    while index < size:
        if not predicate(_text_codepoint(text, index)):
            return False
        index += 1
    return True


def _is_alnum(code):
    return is_alpha(code) or is_decimal(code) or is_digit(code) or is_numeric(code)


def _is_ascii(code):
    return code < 128


def _strip_member(chars):
    if chars is None:
        return None
    _require_text(chars, "strip arg must be None or str")
    return chars


def _stripped(code, chars):
    if chars is None:
        return is_space(code)
    return _contains_codepoint(chars, code)


def _strip(text, chars, left, right):
    chars = _strip_member(chars)
    start = 0
    end = _text_length(text)
    if left:
        while start < end and _stripped(_text_codepoint(text, start), chars):
            start += 1
    if right:
        while end > start and _stripped(_text_codepoint(text, end - 1), chars):
            end -= 1
    if start == 0 and end == _text_length(text):
        return text
    return _text_slice(text, start, end)


def _fill_character(fillchar):
    if not _text_storage(fillchar):
        raise TypeError(
            f"The fill character must be a unicode character, not {_type_name(fillchar)}"
        )
    if _text_length(fillchar) != 1:
        raise TypeError("The fill character must be exactly one character long")
    return fillchar


def _repeat(text, count):
    """Concatenate ``count`` copies by doubling, so the work is linear in
    the result length."""
    if count <= 0 or _text_length(text) == 0:
        return ""
    pieces = []
    power = text
    while count > 0:
        if count & 1:
            pieces.append(power)
        count >>= 1
        if count > 0:
            power = _text_join((power, power))
    return _text_join(pieces)


def _pad(text, left, right, fill):
    if left <= 0 and right <= 0:
        return text
    return _text_join((_repeat(fill, left), text, _repeat(fill, right)))


def _reversed(values):
    result = []
    index = _storage_length(values) - 1
    while index >= 0:
        result.append(_storage_read(values, index))
        index -= 1
    return result


def _split_whitespace(text, maxsplit):
    result = []
    length = _text_length(text)
    index = 0
    while maxsplit != 0:
        while index < length and is_space(_text_codepoint(text, index)):
            index += 1
        if index == length:
            return result
        start = index
        index += 1
        while index < length and not is_space(_text_codepoint(text, index)):
            index += 1
        result.append(_text_slice(text, start, index))
        maxsplit -= 1
    while index < length and is_space(_text_codepoint(text, index)):
        index += 1
    if index != length:
        result.append(_text_slice(text, index, length))
    return result


def _rsplit_whitespace(text, maxsplit):
    result = []
    index = _text_length(text) - 1
    while maxsplit != 0:
        while index >= 0 and is_space(_text_codepoint(text, index)):
            index -= 1
        if index < 0:
            return _reversed(result)
        end = index + 1
        index -= 1
        while index >= 0 and not is_space(_text_codepoint(text, index)):
            index -= 1
        result.append(_text_slice(text, index + 1, end))
        maxsplit -= 1
    while index >= 0 and is_space(_text_codepoint(text, index)):
        index -= 1
    if index >= 0:
        result.append(_text_slice(text, 0, index + 1))
    return _reversed(result)


def _split_arguments(sep, maxsplit):
    maxsplit = _index(maxsplit)
    if sep is not None:
        _require_text(sep, "must be str or None")
        if _text_length(sep) == 0:
            raise ValueError("empty separator")
    return maxsplit


def _codec_name(encoding):
    """Normalize an encoding name to one of the built-in codecs."""
    _require_text(encoding, "str() argument 'encoding' must be str")
    codes = []
    index = 0
    size = _text_length(encoding)
    while index < size:
        code = _text_codepoint(encoding, index)
        if code >= 65 and code <= 90:
            code += 32
        elif code == 32 or code == 95:
            code = 45
        codes.append(code)
        index += 1
    name = _from_codepoints(codes)
    if name == "utf-8" or name == "utf8" or name == "u8" or name == "utf":
        return "utf-8"
    if name == "ascii" or name == "us-ascii" or name == "646":
        return "ascii"
    if (
        name == "latin-1"
        or name == "latin1"
        or name == "latin"
        or name == "l1"
        or name == "iso-8859-1"
        or name == "iso8859-1"
        or name == "8859"
        or name == "cp819"
    ):
        return "latin-1"
    raise LookupError(f"unknown encoding: {encoding}")


def _escaped_character(code):
    if code <= 0xFF:
        return "\\x" + _hex(code, 2)
    if code <= 0xFFFF:
        return "\\u" + _hex(code, 4)
    return "\\U" + _hex(code, 8)


def _hex(value, width):
    codes = []
    while width > 0:
        width -= 1
        codes.append(_text_codepoint(_HEX_DIGITS, (value >> (width * 4)) & 15))
    return _from_codepoints(codes)


def _decimal(value):
    if value == 0:
        return "0"
    codes = []
    while value > 0:
        codes.append(48 + value % 10)
        value //= 10
    return _from_codepoints(_reversed(codes))


def _append_ascii(values, text):
    index = 0
    size = _text_length(text)
    while index < size:
        values.append(_text_codepoint(text, index))
        index += 1


def encode(text, encoding, errors):
    codec = _codec_name(encoding)
    _require_text(errors, "encode() argument 'errors' must be str")
    if codec == "utf-8":
        limit = 0x10FFFF
    elif codec == "latin-1":
        limit = 0xFF
    else:
        limit = 0x7F
    values = []
    index = 0
    size = _text_length(text)
    while index < size:
        code = _text_codepoint(text, index)
        surrogate = code >= 0xD800 and code <= 0xDFFF
        if code <= limit and not (codec == "utf-8" and surrogate):
            _encode_one(values, codec, code)
        elif errors == "surrogatepass" and codec == "utf-8" and surrogate:
            _encode_utf8(values, code)
        elif errors == "surrogateescape" and code >= 0xDC80 and code <= 0xDCFF:
            values.append(code - 0xDC00)
        elif errors == "ignore":
            pass
        elif errors == "replace":
            values.append(63)
        elif errors == "backslashreplace":
            _append_ascii(values, _escaped_character(code))
        elif errors == "xmlcharrefreplace":
            _append_ascii(values, "&#" + _decimal(code) + ";")
        elif errors == "strict":
            if codec == "utf-8":
                reason = "surrogates not allowed"
            else:
                reason = f"ordinal not in range({limit + 1})"
            character = _text_character(text, index)
            raise UnicodeEncodeError(
                f"'{codec}' codec can't encode character {character!r} "
                f"in position {index}: {reason}"
            )
        else:
            raise LookupError(f"unknown error handler name '{errors}'")
        index += 1
    return _bytes_from_values(values)


def _encode_one(values, codec, code):
    if codec == "utf-8":
        _encode_utf8(values, code)
    else:
        values.append(code)


def _encode_utf8(values, code):
    if code < 0x80:
        values.append(code)
    elif code < 0x800:
        values.append(0xC0 | (code >> 6))
        values.append(0x80 | (code & 0x3F))
    elif code < 0x10000:
        values.append(0xE0 | (code >> 12))
        values.append(0x80 | ((code >> 6) & 0x3F))
        values.append(0x80 | (code & 0x3F))
    else:
        values.append(0xF0 | (code >> 18))
        values.append(0x80 | ((code >> 12) & 0x3F))
        values.append(0x80 | ((code >> 6) & 0x3F))
        values.append(0x80 | (code & 0x3F))


def _continuation(data, position, size, low, high):
    if position >= size:
        return False
    byte = _read_byte(data, position)
    return byte >= low and byte <= high


def _utf8_sequence(data, index, size, surrogates):
    """Return ``(codepoint, length)`` for the sequence at ``index``, or
    ``(-1, length)`` with the invalid maximal subpart's length, or
    ``(-2, length)`` when the data ends inside a valid prefix."""
    first = _read_byte(data, index)
    if first < 0x80:
        return first, 1
    if first >= 0xC2 and first <= 0xDF:
        count = 1
        low = 0x80
        high = 0xBF
        value = first & 0x1F
    elif first >= 0xE0 and first <= 0xEF:
        count = 2
        low = 0x80
        high = 0xBF
        if first == 0xE0:
            low = 0xA0
        elif first == 0xED and not surrogates:
            high = 0x9F
        value = first & 0x0F
    elif first >= 0xF0 and first <= 0xF4:
        count = 3
        low = 0x80
        high = 0xBF
        if first == 0xF0:
            low = 0x90
        elif first == 0xF4:
            high = 0x8F
        value = first & 0x07
    else:
        return -1, 1
    length = 1
    while length <= count:
        if index + length >= size:
            return -2, length
        if not _continuation(data, index + length, size, low, high):
            return -1, length
        value = (value << 6) | (_read_byte(data, index + length) & 0x3F)
        low = 0x80
        high = 0xBF
        length += 1
    return value, length


def _decode_error(codec, data, start, end, reason):
    if end - start == 1:
        where = f"byte 0x{_hex(_read_byte(data, start), 2)} in position {start}"
    else:
        where = f"bytes in position {start}-{end - 1}"
    return UnicodeDecodeError(f"'{codec}' codec can't decode {where}: {reason}")


def decode(data, encoding, errors):
    """Decode ``bytes`` storage with a built-in codec."""
    codec = _codec_name(encoding)
    _require_text(errors, "decode() argument 'errors' must be str")
    codes = []
    index = 0
    size = _byte_length(data)
    while index < size:
        if codec == "utf-8":
            code, length = _utf8_sequence(
                data, index, size, errors == "surrogatepass"
            )
            if code >= 0:
                codes.append(code)
                index += length
                continue
            if code == -2:
                length = size - index
                reason = "unexpected end of data"
            elif length == 1:
                reason = "invalid start byte"
            else:
                reason = "invalid continuation byte"
                if length > 1:
                    length -= 1
        else:
            byte = _read_byte(data, index)
            if codec == "latin-1" or byte < 128:
                codes.append(byte)
                index += 1
                continue
            length = 1
            reason = "ordinal not in range(128)"
        _decode_failure(codes, codec, data, index, index + length, reason, errors)
        index += length
    return _from_codepoints(codes)


def _decode_failure(codes, codec, data, start, end, reason, errors):
    if errors == "strict":
        raise _decode_error(codec, data, start, end, reason)
    if errors == "ignore":
        return
    if errors == "replace":
        codes.append(0xFFFD)
        return
    position = start
    while position < end:
        byte = _read_byte(data, position)
        if errors == "surrogateescape" and byte >= 128:
            codes.append(0xDC00 + byte)
        elif errors == "backslashreplace":
            _append_ascii(codes, "\\x" + _hex(byte, 2))
        else:
            raise LookupError(f"unknown error handler name '{errors}'")
        position += 1


def parse_specification(specification):
    """Split a standard format specification into its fields.

    Returns ``(fill, align, sign, coerce_zero, alternate, zero, width,
    grouping, precision, kind)``. Absent fields are ``None`` (or 0 for
    ``width``, False for flags). Follows CPython's ``parse_internal_render_format_spec``.
    """
    size = _text_length(specification)
    index = 0
    fill = None
    align = None
    if size >= 2 and _is_alignment(_text_character(specification, 1)):
        fill = _text_character(specification, 0)
        align = _text_character(specification, 1)
        index = 2
    elif size >= 1 and _is_alignment(_text_character(specification, 0)):
        align = _text_character(specification, 0)
        index = 1
    sign = None
    if index < size:
        character = _text_character(specification, index)
        if character == "+" or character == "-" or character == " ":
            sign = character
            index += 1
    coerce_zero = False
    if index < size and _text_character(specification, index) == "z":
        coerce_zero = True
        index += 1
    alternate = False
    if index < size and _text_character(specification, index) == "#":
        alternate = True
        index += 1
    zero = False
    if index < size and _text_character(specification, index) == "0":
        zero = True
        index += 1
    width, index = _digits(specification, index, size)
    grouping = None
    if index < size:
        character = _text_character(specification, index)
        if character == "," or character == "_":
            grouping = character
            index += 1
    precision = None
    if index < size and _text_character(specification, index) == ".":
        precision, end = _digits(specification, index + 1, size)
        if end == index + 1:
            raise ValueError("Format specifier missing precision")
        index = end
    kind = None
    if index < size:
        kind = _text_character(specification, index)
        index += 1
    if index < size:
        raise ValueError("Invalid format specifier")
    return fill, align, sign, coerce_zero, alternate, zero, width, grouping, precision, kind


def _is_alignment(character):
    return character == "<" or character == ">" or character == "^" or character == "="


def _digits(text, index, size):
    value = 0
    while index < size:
        digit = _text_codepoint(text, index) - 48
        if digit < 0 or digit > 9:
            break
        value = value * 10 + digit
        index += 1
    return value, index


def align(text, width, fill, alignment):
    padding = width - _text_length(text)
    if padding <= 0:
        return text
    if alignment == ">":
        return _pad(text, padding, 0, fill)
    if alignment == "^":
        left = padding // 2
        return _pad(text, left, padding - left, fill)
    return _pad(text, 0, padding, fill)


def format_text(text, specification):
    """Render ``format(text, specification)`` for a ``str`` value."""
    if _text_length(specification) == 0:
        return text
    fill, alignment, sign, coerce_zero, alternate, zero, width, grouping, precision, kind = (
        parse_specification(specification)
    )
    if kind is not None and kind != "s":
        raise ValueError(f"Unknown format code '{kind}' for object of type 'str'")
    if sign is not None:
        raise ValueError("Sign not allowed in string format specifier")
    if coerce_zero:
        raise ValueError(
            "Negative zero coercion (z) not allowed in string format specifier"
        )
    if alternate:
        raise ValueError("Alternate form (#) not allowed in string format specifier")
    if grouping is not None:
        raise ValueError(f"Cannot specify '{grouping}' with 's'.")
    if alignment == "=":
        raise ValueError("'=' alignment not allowed in string format specifier")
    if zero and fill is None:
        fill = "0"
    if fill is None:
        fill = " "
    if precision is not None and precision < _text_length(text):
        text = _text_slice(text, 0, precision)
    return align(text, width, fill, alignment)


class _Fields:
    """Positional replacement-field numbering state for ``str.format``."""

    def __init__(self, args):
        self.args = args
        self.next_index = 0
        self.manual = False
        self.automatic = False


def _field_value(name, fields):
    size = _text_length(name)
    index = 0
    while index < size:
        character = _text_character(name, index)
        if character == "." or character == "[":
            break
        index += 1
    first = _text_slice(name, 0, index)
    if _text_length(first) == 0:
        if fields.manual:
            raise ValueError(
                "cannot switch from manual field specification to automatic field numbering"
            )
        fields.automatic = True
        position = fields.next_index
        fields.next_index += 1
    else:
        position, end = _digits(first, 0, _text_length(first))
        if end != _text_length(first):
            raise KeyError(first)
        if fields.automatic:
            raise ValueError(
                "cannot switch from automatic field numbering to manual field specification"
            )
        fields.manual = True
    if position >= _storage_length(fields.args):
        raise IndexError(
            f"Replacement index {position} out of range for positional args tuple"
        )
    value = _storage_read(fields.args, position)
    while index < size:
        character = _text_character(name, index)
        index += 1
        start = index
        if character == ".":
            raise AttributeError("format field attribute access is not supported")
        while index < size and _text_character(name, index) != "]":
            index += 1
        if index == size:
            raise ValueError("Missing ']' in format string")
        key = _text_slice(name, start, index)
        index += 1
        if _text_length(key) == 0:
            raise ValueError("Empty attribute in format string")
        number, end = _digits(key, 0, _text_length(key))
        if end == _text_length(key):
            value = value[number]
        else:
            value = value[key]
        if index < size:
            character = _text_character(name, index)
            if character != "." and character != "[":
                raise ValueError(
                    "Only '.' or '[' may follow ']' in format field specifier"
                )
    return value


def _replacement_field(template, index, size, fields, depth):
    """Format the field after the ``{`` at ``index - 1``. Returns the text and
    the index after its closing ``}``."""
    start = index
    nesting = 1
    while index < size:
        character = _text_character(template, index)
        if character == "{":
            nesting += 1
        elif character == "}":
            nesting -= 1
            if nesting == 0:
                break
        index += 1
    if index == size:
        raise ValueError("expected '}' before end of string")
    field = _text_slice(template, start, index)
    end = index + 1
    field_size = _text_length(field)
    position = 0
    bracket = False
    while position < field_size:
        character = _text_character(field, position)
        if character == "[":
            bracket = True
        elif character == "]":
            bracket = False
        elif not bracket and (character == "!" or character == ":"):
            break
        position += 1
    name = _text_slice(field, 0, position)
    conversion = None
    specification = ""
    if position < field_size and _text_character(field, position) == "!":
        if position + 1 >= field_size:
            raise ValueError("end of string while looking for conversion specifier")
        conversion = _text_character(field, position + 1)
        position += 2
        if position < field_size and _text_character(field, position) != ":":
            raise ValueError("expected ':' after conversion specifier")
    if position < field_size:
        specification = _text_slice(field, position + 1, field_size)
        if depth > 1:
            raise ValueError("Max string recursion exceeded")
        specification = _expand(specification, fields, depth + 1)
    value = _field_value(name, fields)
    if conversion is not None:
        if conversion == "r":
            value = _representation_protocol(value)
        elif conversion == "s":
            value = _string_protocol(value)
        elif conversion == "a":
            value = ascii(value)
        else:
            raise ValueError(f"Unknown conversion specifier {conversion}")
    return _format_value_protocol(value, specification), end


def _expand(template, fields, depth):
    pieces = []
    size = _text_length(template)
    index = 0
    literal = 0
    while index < size:
        character = _text_character(template, index)
        if character == "{":
            pieces.append(_text_slice(template, literal, index))
            if index + 1 < size and _text_character(template, index + 1) == "{":
                pieces.append("{")
                index += 2
            else:
                if index + 1 == size:
                    raise ValueError("Single '{' encountered in format string")
                text, index = _replacement_field(
                    template, index + 1, size, fields, depth
                )
                pieces.append(text)
            literal = index
        elif character == "}":
            pieces.append(_text_slice(template, literal, index))
            if index + 1 < size and _text_character(template, index + 1) == "}":
                pieces.append("}")
                index += 2
                literal = index
            else:
                raise ValueError("Single '}' encountered in format string")
        else:
            index += 1
    pieces.append(_text_slice(template, literal, size))
    return _text_join(pieces)


def representation(text):
    """``repr(text)``: CPython's ``unicode_repr`` quoting and escapes."""
    single = _contains_codepoint(text, 39)
    double = _contains_codepoint(text, 34)
    quote = 39
    if single and not double:
        quote = 34
    codes = [quote]
    index = 0
    size = _text_length(text)
    while index < size:
        code = _text_codepoint(text, index)
        index += 1
        if code == quote or code == 92:
            codes.append(92)
            codes.append(code)
        elif code == 9:
            codes.append(92)
            codes.append(116)
        elif code == 10:
            codes.append(92)
            codes.append(110)
        elif code == 13:
            codes.append(92)
            codes.append(114)
        elif code < 32 or code == 127:
            _append_ascii(codes, _escaped_character(code))
        elif code < 127 or is_printable(code):
            codes.append(code)
        else:
            _append_ascii(codes, _escaped_character(code))
    codes.append(quote)
    return _from_codepoints(codes)


class _Missing:
    pass


_MISSING = _Missing()


def str(object=_MISSING, encoding=_MISSING, errors=_MISSING):
    if encoding is _MISSING and errors is _MISSING:
        if object is _MISSING:
            return ""
        return _string_protocol(object)
    if encoding is _MISSING:
        encoding = "utf-8"
    if errors is _MISSING:
        errors = "strict"
    if object is _MISSING:
        return ""
    if _text_storage(object):
        raise TypeError("decoding str is not supported")
    try:
        _byte_length(object)
    except TypeError:
        raise TypeError(
            f"decoding to str: need a bytes-like object, {_type_name(object)} found"
        )
    return decode(object, encoding, errors)


def repr(value, /):
    return _representation_protocol(value)


def ascii(value, /):
    text = _representation_protocol(value)
    codes = []
    index = 0
    size = _text_length(text)
    while index < size:
        code = _text_codepoint(text, index)
        if code < 128:
            codes.append(code)
        else:
            _append_ascii(codes, _escaped_character(code))
        index += 1
    return _from_codepoints(codes)


def format(value, format_spec="", /):
    _require_text(format_spec, "format() argument 2 must be str")
    return _format_value_protocol(value, format_spec)


def ord(value, /):
    if not _text_storage(value):
        raise TypeError(
            f"ord() expected string of length 1, but {_type_name(value)} found"
        )
    size = _text_length(value)
    if size != 1:
        raise TypeError(
            f"ord() expected a character, but string of length {size} found"
        )
    return _text_codepoint(value, 0)


class TextStorage:
    def __getitem__(self, index):
        if is_slice(index):
            start, stop, step, count = positions(index, _text_length(self))
            if step == 1:
                return _text_slice(self, start, start + count)
            codes = []
            offset = 0
            while offset < count:
                codes.append(_text_codepoint(self, start + offset * step))
                offset += 1
            return _from_codepoints(codes)
        size = _text_length(self)
        index = _index(index)
        if index < 0:
            index += size
        if index < 0 or index >= size:
            raise IndexError("string index out of range")
        return _text_character(self, index)

    def __iter__(self):
        return TextIterator(self)

    def __len__(self):
        return _text_length(self)

    def __contains__(self, item):
        if not _text_storage(item):
            raise TypeError(
                f"'in <string>' requires string as left operand, not {_type_name(item)}"
            )
        return find_in(self, item, 0, _text_length(self)) >= 0

    def __add__(self, other):
        if not _text_storage(other):
            raise TypeError(
                f'can only concatenate str (not "{_type_name(other)}") to str'
            )
        if _text_length(other) == 0:
            return self
        if _text_length(self) == 0:
            return other
        return _text_join((self, other))

    def __mul__(self, count):
        if _index_method(count) is None:
            try:
                count = _integer_storage(count)
            except TypeError:
                raise TypeError(
                    f"can't multiply sequence by non-int of type '{_type_name(count)}'"
                )
        else:
            count = _index(count)
        return _repeat(self, count)

    def __rmul__(self, count):
        return self.__mul__(count)

    def __eq__(self, other):
        if not _text_storage(other):
            return False
        return _compare(self, other) == 0

    def __ne__(self, other):
        if not _text_storage(other):
            return True
        return _compare(self, other) != 0

    def __lt__(self, other):
        return _compare(self, _ordered_operand(self, other, "<")) < 0

    def __le__(self, other):
        return _compare(self, _ordered_operand(self, other, "<=")) <= 0

    def __gt__(self, other):
        return _compare(self, _ordered_operand(self, other, ">")) > 0

    def __ge__(self, other):
        return _compare(self, _ordered_operand(self, other, ">=")) >= 0

    def __str__(self):
        return self

    def __repr__(self):
        return representation(self)

    def __format__(self, format_spec):
        _require_text(format_spec, "format() argument 2 must be str")
        return format_text(self, format_spec)

    def __hash__(self):
        return hash_text(self)

    def __mod__(self, values):
        return _percent_decimal_protocol(self, values)

    def encode(self, encoding="utf-8", errors="strict"):
        return encode(self, encoding, errors)

    def find(self, sub, start=None, end=None, /):
        return _search(self, sub, start, end, "find")

    def rfind(self, sub, start=None, end=None, /):
        return _search(self, sub, start, end, "rfind")

    def index(self, sub, start=None, end=None, /):
        result = _search(self, sub, start, end, "index")
        if result < 0:
            raise ValueError("substring not found")
        return result

    def rindex(self, sub, start=None, end=None, /):
        result = _search(self, sub, start, end, "rindex")
        if result < 0:
            raise ValueError("substring not found")
        return result

    def count(self, sub, start=None, end=None, /):
        _require_text(sub, "count() argument 1 must be str")
        start, end = _bounds(_text_length(self), start, end)
        size = _text_length(sub)
        if end - start < size:
            return 0
        if size == 0:
            return end - start + 1
        result = 0
        position = find_in(self, sub, start, end)
        while position >= 0:
            result += 1
            position = find_in(self, sub, position + size, end)
        return result

    def startswith(self, prefix, start=None, end=None, /):
        return _affix_match(self, prefix, start, end, False, "startswith")

    def endswith(self, suffix, start=None, end=None, /):
        return _affix_match(self, suffix, start, end, True, "endswith")

    def removeprefix(self, prefix, /):
        _require_text(prefix, "removeprefix() argument must be str")
        size = _text_length(prefix)
        if size <= _text_length(self) and _matches_at(self, prefix, 0, size):
            return _text_slice(self, size, _text_length(self))
        return self

    def removesuffix(self, suffix, /):
        _require_text(suffix, "removesuffix() argument must be str")
        size = _text_length(suffix)
        length = _text_length(self)
        if size == 0:
            return self
        if size <= length and _matches_at(self, suffix, length - size, size):
            return _text_slice(self, 0, length - size)
        return self

    def replace(self, old, new, /, count=-1):
        _require_text(old, "replace() argument 1 must be str")
        _require_text(new, "replace() argument 2 must be str")
        count = _index(count)
        if count < 0:
            count = _text_length(self) + 1
        if count == 0:
            return self
        old_size = _text_length(old)
        length = _text_length(self)
        pieces = []
        if old_size == 0:
            index = 0
            while index < length and count > 0:
                pieces.append(new)
                pieces.append(_text_character(self, index))
                count -= 1
                index += 1
            if count > 0:
                pieces.append(new)
            pieces.append(_text_slice(self, index, length))
            return _text_join(pieces)
        start = 0
        position = find_in(self, old, 0, length)
        if position < 0:
            return self
        while position >= 0 and count > 0:
            pieces.append(_text_slice(self, start, position))
            pieces.append(new)
            start = position + old_size
            count -= 1
            position = find_in(self, old, start, length)
        pieces.append(_text_slice(self, start, length))
        return _text_join(pieces)

    def split(self, sep=None, maxsplit=-1):
        maxsplit = _split_arguments(sep, maxsplit)
        if sep is None:
            return _split_whitespace(self, maxsplit)
        result = []
        length = _text_length(self)
        size = _text_length(sep)
        start = 0
        position = find_in(self, sep, 0, length)
        while position >= 0 and maxsplit != 0:
            result.append(_text_slice(self, start, position))
            start = position + size
            maxsplit -= 1
            position = find_in(self, sep, start, length)
        result.append(_text_slice(self, start, length))
        return result

    def rsplit(self, sep=None, maxsplit=-1):
        maxsplit = _split_arguments(sep, maxsplit)
        if sep is None:
            return _rsplit_whitespace(self, maxsplit)
        result = []
        size = _text_length(sep)
        end = _text_length(self)
        position = rfind_in(self, sep, 0, end)
        while position >= 0 and maxsplit != 0:
            result.append(_text_slice(self, position + size, end))
            end = position
            maxsplit -= 1
            position = rfind_in(self, sep, 0, end)
        result.append(_text_slice(self, 0, end))
        return _reversed(result)

    def splitlines(self, keepends=False):
        result = []
        length = _text_length(self)
        index = 0
        while index < length:
            start = index
            while index < length and not is_linebreak(_text_codepoint(self, index)):
                index += 1
            line_end = index
            if index < length:
                if (
                    _text_codepoint(self, index) == 13
                    and index + 1 < length
                    and _text_codepoint(self, index + 1) == 10
                ):
                    index += 2
                else:
                    index += 1
                if keepends:
                    line_end = index
            result.append(_text_slice(self, start, line_end))
        return result

    def partition(self, sep, /):
        _require_text(sep, "must be str")
        size = _text_length(sep)
        if size == 0:
            raise ValueError("empty separator")
        length = _text_length(self)
        position = find_in(self, sep, 0, length)
        if position < 0:
            return self, "", ""
        return (
            _text_slice(self, 0, position),
            sep,
            _text_slice(self, position + size, length),
        )

    def rpartition(self, sep, /):
        _require_text(sep, "must be str")
        size = _text_length(sep)
        if size == 0:
            raise ValueError("empty separator")
        length = _text_length(self)
        position = rfind_in(self, sep, 0, length)
        if position < 0:
            return "", "", self
        return (
            _text_slice(self, 0, position),
            sep,
            _text_slice(self, position + size, length),
        )

    def join(self, iterable, /):
        pieces = []
        for item in iterable:
            if not _text_storage(item):
                raise TypeError(
                    f"sequence item {_storage_length(pieces) // 2}: expected str "
                    f"instance, {_type_name(item)} found"
                )
            if _storage_length(pieces) > 0:
                pieces.append(self)
            pieces.append(item)
        if _storage_length(pieces) == 1:
            return _storage_read(pieces, 0)
        return _text_join(pieces)

    def strip(self, chars=None, /):
        return _strip(self, chars, True, True)

    def lstrip(self, chars=None, /):
        return _strip(self, chars, True, False)

    def rstrip(self, chars=None, /):
        return _strip(self, chars, False, True)

    def lower(self):
        codes = []
        index = 0
        length = _text_length(self)
        while index < length:
            _lower_into(codes, self, index, length)
            index += 1
        return _from_codepoints(codes)

    def upper(self):
        codes = []
        index = 0
        length = _text_length(self)
        while index < length:
            _extend_mapping(codes, upper_full(_text_codepoint(self, index)))
            index += 1
        return _from_codepoints(codes)

    def casefold(self):
        codes = []
        index = 0
        length = _text_length(self)
        while index < length:
            _extend_mapping(codes, fold_full(_text_codepoint(self, index)))
            index += 1
        return _from_codepoints(codes)

    def capitalize(self):
        length = _text_length(self)
        if length == 0:
            return self
        codes = []
        _extend_mapping(codes, title_full(_text_codepoint(self, 0)))
        index = 1
        while index < length:
            _lower_into(codes, self, index, length)
            index += 1
        return _from_codepoints(codes)

    def title(self):
        codes = []
        previous_cased = False
        index = 0
        length = _text_length(self)
        while index < length:
            code = _text_codepoint(self, index)
            if previous_cased:
                _lower_into(codes, self, index, length)
            else:
                _extend_mapping(codes, title_full(code))
            previous_cased = is_cased(code)
            index += 1
        return _from_codepoints(codes)

    def swapcase(self):
        codes = []
        index = 0
        length = _text_length(self)
        while index < length:
            code = _text_codepoint(self, index)
            if is_upper(code):
                _lower_into(codes, self, index, length)
            elif is_lower(code):
                _extend_mapping(codes, upper_full(code))
            else:
                codes.append(code)
            index += 1
        return _from_codepoints(codes)

    def isalpha(self):
        return _all_codepoints(self, is_alpha, False)

    def isdecimal(self):
        return _all_codepoints(self, is_decimal, False)

    def isdigit(self):
        return _all_codepoints(self, is_digit, False)

    def isnumeric(self):
        return _all_codepoints(self, is_numeric, False)

    def isalnum(self):
        return _all_codepoints(self, _is_alnum, False)

    def isspace(self):
        return _all_codepoints(self, is_space, False)

    def isprintable(self):
        return _all_codepoints(self, is_printable, True)

    def isascii(self):
        return _all_codepoints(self, _is_ascii, True)

    def isidentifier(self):
        length = _text_length(self)
        if length == 0:
            return False
        first = _text_codepoint(self, 0)
        if not is_xid_start(first) and first != 95:
            return False
        index = 1
        while index < length:
            if not is_xid_continue(_text_codepoint(self, index)):
                return False
            index += 1
        return True

    def islower(self):
        cased = False
        index = 0
        length = _text_length(self)
        while index < length:
            code = _text_codepoint(self, index)
            if is_upper(code) or is_titlecase(code):
                return False
            if not cased and is_lower(code):
                cased = True
            index += 1
        return cased

    def isupper(self):
        cased = False
        index = 0
        length = _text_length(self)
        while index < length:
            code = _text_codepoint(self, index)
            if is_lower(code) or is_titlecase(code):
                return False
            if not cased and is_upper(code):
                cased = True
            index += 1
        return cased

    def istitle(self):
        cased = False
        previous_cased = False
        index = 0
        length = _text_length(self)
        while index < length:
            code = _text_codepoint(self, index)
            if is_upper(code) or is_titlecase(code):
                if previous_cased:
                    return False
                previous_cased = True
                cased = True
            elif is_lower(code):
                if not previous_cased:
                    return False
                previous_cased = True
                cased = True
            else:
                previous_cased = False
            index += 1
        return cased

    def center(self, width, fillchar=" ", /):
        width = _index(width)
        fill = _fill_character(fillchar)
        padding = width - _text_length(self)
        if padding <= 0:
            return self
        # CPython rounds an odd margin toward the left when the width is odd.
        left = padding // 2 + (padding & width & 1)
        return _pad(self, left, padding - left, fill)

    def ljust(self, width, fillchar=" ", /):
        width = _index(width)
        return _pad(self, 0, width - _text_length(self), _fill_character(fillchar))

    def rjust(self, width, fillchar=" ", /):
        width = _index(width)
        return _pad(self, width - _text_length(self), 0, _fill_character(fillchar))

    def zfill(self, width, /):
        width = _index(width)
        length = _text_length(self)
        padding = width - length
        if padding <= 0:
            return self
        if length > 0:
            first = _text_codepoint(self, 0)
            if first == 43 or first == 45:
                return _text_join(
                    (
                        _text_character(self, 0),
                        _repeat("0", padding),
                        _text_slice(self, 1, length),
                    )
                )
        return _text_join((_repeat("0", padding), self))

    def expandtabs(self, tabsize=8):
        tabsize = _index(tabsize)
        codes = []
        column = 0
        index = 0
        length = _text_length(self)
        while index < length:
            code = _text_codepoint(self, index)
            index += 1
            if code == 9:
                if tabsize > 0:
                    spaces = tabsize - column % tabsize
                    column += spaces
                    while spaces > 0:
                        codes.append(32)
                        spaces -= 1
            else:
                codes.append(code)
                column += 1
                if code == 10 or code == 13:
                    column = 0
        return _from_codepoints(codes)

    def translate(self, table, /):
        codes = []
        index = 0
        length = _text_length(self)
        while index < length:
            code = _text_codepoint(self, index)
            index += 1
            try:
                mapped = table[code]
            except LookupError:
                codes.append(code)
                continue
            if mapped is None:
                continue
            if _text_storage(mapped):
                position = 0
                while position < _text_length(mapped):
                    codes.append(_text_codepoint(mapped, position))
                    position += 1
                continue
            try:
                mapped = _integer_storage(mapped)
            except TypeError:
                raise TypeError("character mapping must return integer, None or str")
            if mapped < 0 or mapped > 0x10FFFF:
                raise ValueError("character mapping must be in range(0x110000)")
            codes.append(mapped)
        return _from_codepoints(codes)

    def format(self, *args):
        return _expand(self, _Fields(args), 1)


class TextIterator:
    def __init__(self, text):
        self.text = text
        self.index = 0

    def __iter__(self):
        return self

    def __next__(self):
        index = self.index
        if index >= _text_length(self.text):
            raise StopIteration
        self.index = index + 1
        return _text_character(self.text, index)


def _ordered_operand(left, right, operator):
    if not _text_storage(right):
        raise TypeError(
            f"'{operator}' not supported between instances of 'str' and "
            f"'{_type_name(right)}'"
        )
    return right


def _compare(left, right):
    """Lexicographic codepoint order: negative, zero, or positive."""
    left_size = _text_length(left)
    right_size = _text_length(right)
    size = left_size
    if right_size < size:
        size = right_size
    index = 0
    while index < size:
        difference = _text_codepoint(left, index) - _text_codepoint(right, index)
        if difference != 0:
            return difference
        index += 1
    return left_size - right_size


def _affix_match(text, affix, start, end, suffix, method):
    if _tuple_storage(affix):
        for candidate in affix:
            if not _text_storage(candidate):
                raise TypeError(
                    f"tuple for {method} must only contain str, not "
                    f"{_type_name(candidate)}"
                )
            if _affix_match(text, candidate, start, end, suffix, method):
                return True
        return False
    if not _text_storage(affix):
        raise TypeError(
            f"{method} first arg must be str or a tuple of str, not {_type_name(affix)}"
        )
    start, end = _bounds(_text_length(text), start, end)
    size = _text_length(affix)
    if end - start < size:
        return False
    if suffix:
        return _matches_at(text, affix, end - size, size)
    return _matches_at(text, affix, start, size)


def hash_text(text):
    """FNV-1a over codepoints, reduced to a nonnegative 61-bit value. CPython
    randomizes ``str`` hashes per process, so no particular values are part
    of the contract; equal strings hash equally."""
    value = 0xCBF29CE484222325
    index = 0
    length = _text_length(text)
    while index < length:
        value = ((value ^ _text_codepoint(text, index)) * 0x100000001B3) & (
            (1 << 64) - 1
        )
        index += 1
    value = value % ((1 << 61) - 1)
    if value == 0:
        return 1
    return value

