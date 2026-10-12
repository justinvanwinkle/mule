"""Compiler-private formatting protocols, compiled by Mule itself.

The underscored calls below are physical primitive bindings supplied by the
compiler. Method selection and Python fallback rules execute as compiled Python.
"""

# Compiler-provided physical primitives are not host Python globals.
# ruff: noqa: F821


def string(value):
    method = _str_method(value)
    if method is None:
        method = _repr_method(value)
    if method is None:
        return _render_string(value)
    return _text_result(method())


def representation(value):
    method = _repr_method(value)
    if method is None:
        return _render_representation(value)
    return _text_result(method())


def format_value(value, specification):
    method = _format_method(value)
    if method is None:
        if specification == "":
            return _string_protocol(value)
        raise TypeError(
            f"unsupported format string passed to {_type_name(value)}.__format__"
        )
    return _text_result(method(specification))


def decimal_integer(value):
    value = _integer_storage(value)
    if value == 0:
        return "0"
    negative = value < 0
    if negative:
        value = -value
    result = ""
    while value > 0:
        result = _text_character("0123456789", value % 10) + result
        value = value // 10
    if negative:
        result = "-" + result
    return result


def percent_decimal(pattern, values):
    if not _storage_compatible(values, ()):
        values = (values,)
    count = _storage_length(values)
    consumed = 0
    index = 0
    size = _text_length(pattern)
    result = ""
    while index < size:
        character = _text_character(pattern, index)
        index += 1
        if character != "%":
            result += character
            continue
        if index == size:
            raise ValueError("incomplete format")
        conversion = _text_character(pattern, index)
        index += 1
        if conversion == "%":
            result += "%"
            continue
        if conversion != "d":
            raise ValueError("unsupported percent format conversion")
        if consumed == count:
            raise TypeError("not enough arguments for format string")
        result += _decimal_protocol(_storage_read(values, consumed))
        consumed += 1
    if consumed != count:
        raise TypeError("not all arguments converted during string formatting")
    return result


def modulo(left, right):
    if _text_storage(left):
        return _percent_decimal_protocol(left, right)
    return _numeric_modulo(left, right)


def ascii_representation(value):
    """``ascii(value)``: the representation with non-ASCII escaped."""
    text = representation(value)
    pieces = []
    index = 0
    while index < _text_length(text):
        code = _text_codepoint(text, index)
        if code < 0x80:
            pieces.append(_text_character(text, index))
        elif code < 0x100:
            pieces.append("\\x" + _hex(code, 2))
        elif code < 0x10000:
            pieces.append("\\u" + _hex(code, 4))
        else:
            pieces.append("\\U" + _hex(code, 8))
        index += 1
    return "".join(pieces)


def _hex(code, width):
    digits = []
    while width > 0:
        digits.append("0123456789abcdef"[code & 15])
        code >>= 4
        width -= 1
    digits.reverse()
    return "".join(digits)
