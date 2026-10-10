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


def align_string(value, specification):
    size = _text_length(specification)
    if size == 0:
        return value
    alignment = "<"
    fill = " "
    index = 0
    first = _text_character(specification, 0)
    if size > 1:
        second = _text_character(specification, 1)
        if second == "<" or second == ">" or second == "^":
            fill = first
            alignment = second
            index = 2
    if index == 0 and (first == "<" or first == ">" or first == "^"):
        alignment = first
        index = 1
    if index < size and _text_character(specification, index) == "0":
        return _invalid_specification()
    width = 0
    while index < size:
        digit = _text_codepoint(specification, index) - 48
        if digit < 0 or digit > 9:
            return _invalid_specification()
        width = width * 10 + digit
        index += 1
    padding = width - _text_length(value)
    if padding <= 0:
        return value
    if alignment == "<":
        return value + fill * padding
    if alignment == ">":
        return fill * padding + value
    left = padding // 2
    return fill * left + value + fill * (padding - left)


def format_value(value, specification):
    method = _format_method(value)
    if method is None:
        if specification == "":
            return _string_protocol(value)
        return _alignment_protocol(_text_result(value), specification)
    return _text_result(method(specification))
