"""Compiler-private formatting protocols, compiled by Mule itself.

The underscored calls below are physical primitive bindings supplied by the
compiler. Method selection and Python fallback rules execute as compiled Python.
"""

# Compiler-provided physical primitives are not host Python globals.
# ruff: noqa: F821

from _mule_native import bind_instance_attribute
from _mule_native import class_lookup_default
from _mule_native import type_of

_MAPPING_MISSING = object()


def _index_integer(value):
    method = _index_method(value)
    if method is not None:
        return _integer_storage(method())
    return _integer_storage(value)


def _int_or_index(value):
    owner = type_of(value)
    for hook in ("__int__", "__index__"):
        method = class_lookup_default(owner, hook, _MAPPING_MISSING)
        if method is not _MAPPING_MISSING:
            return _integer_storage(bind_instance_attribute(method, value, owner)())
    return _integer_storage(value)


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
    original = values
    mapping = None
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
        if _text_character(pattern, index) == "%":
            index += 1
            result += "%"
            continue
        key = None
        if _text_character(pattern, index) == "(":
            close = index + 1
            while close < size and _text_character(pattern, close) != ")":
                close += 1
            if close == size:
                raise ValueError("incomplete format key")
            key = ""
            while index + 1 < close:
                index += 1
                key += _text_character(pattern, index)
            index = close + 1
            if index == size:
                raise ValueError("incomplete format")
        left = False
        zero = False
        plus = False
        space = False
        alternate = False
        while index < size and _text_character(pattern, index) in "-0+ #":
            flag = _text_character(pattern, index)
            if flag == "-":
                left = True
            elif flag == "0":
                zero = True
            elif flag == "+":
                plus = True
            elif flag == " ":
                space = True
            else:
                alternate = True
            index += 1
        width = 0
        if index < size and _text_character(pattern, index) == "*":
            index += 1
            if mapping is not None or consumed == count:
                raise TypeError("not enough arguments for format string")
            width = _index_integer(_storage_read(values, consumed))
            consumed += 1
            if width < 0:
                left = True
                width = -width
        else:
            while index < size and "0" <= _text_character(pattern, index) <= "9":
                width = width * 10 + "0123456789".index(_text_character(pattern, index))
                index += 1
        precision = -1
        if index < size and _text_character(pattern, index) == ".":
            index += 1
            precision = 0
            if index < size and _text_character(pattern, index) == "*":
                index += 1
                if mapping is not None or consumed == count:
                    raise TypeError("not enough arguments for format string")
                precision = _index_integer(_storage_read(values, consumed))
                consumed += 1
                if precision < 0:
                    precision = -1
            else:
                while index < size and "0" <= _text_character(pattern, index) <= "9":
                    precision = precision * 10 + "0123456789".index(
                        _text_character(pattern, index)
                    )
                    index += 1
        if index < size and _text_character(pattern, index) in "hlL":
            index += 1
        if index == size:
            raise ValueError("incomplete format")
        conversion = _text_character(pattern, index)
        index += 1
        if conversion not in "diouxXeEfFgGcrsa":
            raise ValueError("unsupported percent format conversion")
        if key is not None:
            if not _mapping_operand(original):
                raise TypeError("format requires a mapping")
            mapping = original
            value = mapping[key]
            consumed = count
        else:
            if consumed == count:
                raise TypeError("not enough arguments for format string")
            value = _storage_read(values, consumed)
            consumed += 1
        sign = ""
        prefix = ""
        body = ""
        numeric = False
        zero_pad = zero
        if conversion in "sra":
            if conversion == "s":
                body = _string_protocol(value)
            elif conversion == "r":
                body = _representation_protocol(value)
            else:
                body = ascii_representation(value)
            zero_pad = False
            if precision >= 0:
                body = body[:precision]
        elif conversion == "c":
            if _text_storage(value):
                if _text_length(value) != 1:
                    raise TypeError("%c requires int or char")
                body = value
            else:
                code = _index_integer(value)
                if code < 0 or code >= 0x110000:
                    raise OverflowError("%c arg not in range(0x110000)")
                body = chr(code)
            zero_pad = False
        elif conversion in "diu":
            numeric = True
            number = _int_or_index(value)
            if number < 0:
                sign = "-"
                number = -number
            elif plus:
                sign = "+"
            elif space:
                sign = " "
            body = _decimal_protocol(number)
            if precision == 0 and number == 0:
                body = ""
            while _text_length(body) < precision:
                body = "0" + body
            if precision >= 0:
                zero_pad = False
        elif conversion in "xXo":
            numeric = True
            number = _index_integer(value)
            if number < 0:
                sign = "-"
                number = -number
            elif plus:
                sign = "+"
            elif space:
                sign = " "
            digits = "0123456789abcdef" if conversion != "X" else "0123456789ABCDEF"
            base = 8 if conversion == "o" else 16
            if number == 0:
                body = "0"
            while number > 0:
                body = _text_character(digits, number % base) + body
                number = number // base
            if precision == 0 and body == "0":
                body = ""
            while _text_length(body) < precision:
                body = "0" + body
            if precision >= 0:
                zero_pad = False
            if alternate:
                if conversion == "o":
                    prefix = "0o"
                elif conversion == "x":
                    prefix = "0x"
                else:
                    prefix = "0X"
        else:
            numeric = True
            specification = ""
            if alternate:
                specification += "#"
            if precision >= 0:
                specification += "." + _decimal_protocol(precision)
            specification += conversion
            body = _format_value_protocol(float(value), specification)
            if body.startswith("-"):
                sign = "-"
                body = body[1:]
            elif plus:
                sign = "+"
            elif space:
                sign = " "
        text = sign + prefix + body
        if _text_length(text) < width:
            pad = width - _text_length(text)
            padding = ""
            while pad > 0:
                padding += "0" if zero_pad and numeric and not left else " "
                pad -= 1
            if left:
                text = text + padding
            elif zero_pad and numeric:
                text = sign + prefix + padding + body
            else:
                text = padding + text
        result += text
    if consumed != count and not _mapping_operand(original):
        raise TypeError("not all arguments converted during string formatting")
    return result


def _mapping_operand(value):
    return (
        not _text_storage(value)
        and not _storage_compatible(value, ())
        and class_lookup_default(type_of(value), "__getitem__", _MAPPING_MISSING)
        is not _MAPPING_MISSING
    )


def modulo(left, right):
    if _text_storage(left):
        return _percent_decimal_protocol(left, right)
    return _numeric_modulo(left, right)


def ascii_representation(value):
    """``ascii(value)``: the representation with non-ASCII escaped."""
    text = _representation_protocol(value)
    pieces = []
    index = 0
    while index < _text_length(text):
        code = _text_codepoint(text, index)
        if code < 0x80:
            pieces.append(_text_character(text, index))
        elif code < 0x100:
            pieces.append("\\x" + _hex_protocol(code, 2))
        elif code < 0x10000:
            pieces.append("\\u" + _hex_protocol(code, 4))
        else:
            pieces.append("\\U" + _hex_protocol(code, 8))
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
