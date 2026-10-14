"""Incremental replacement-field parsers, compiled as ordinary Mule Python."""

from _unicode_data import decimal_value


def _text(value):
    if not isinstance(value, str):
        raise TypeError("expected str, got " + type(value).__name__)
    return value


def _field_index(text):
    if not text:
        return text
    value = 0
    for character in text:
        digit = decimal_value(ord(character))
        if digit < 0:
            return text
        value = value * 10 + digit
        if value > (1 << 63) - 1:
            raise ValueError("Too many decimal digits in format string")
    return value


class _FieldIterator:
    def __init__(self, text, index):
        self.text = text
        self.index = index

    def __iter__(self):
        return self

    def __next__(self):
        text = self.text
        index = self.index
        size = len(text)
        if index == size:
            raise StopIteration
        character = text[index]
        index += 1
        start = index
        if character == '.':
            while index < size and text[index] not in '.[':
                index += 1
            name = text[start:index]
            attribute = True
        elif character == '[':
            while index < size and text[index] != ']':
                index += 1
            if index == size:
                self.index = index
                raise ValueError("Missing ']' in format string")
            name = text[start:index]
            index += 1
            attribute = False
        else:
            raise ValueError("Only '.' or '[' may follow ']' in format field specifier")
        self.index = index
        if not name:
            raise ValueError("Empty attribute in format string")
        return attribute, name if attribute else _field_index(name)


def formatter_field_name_split(field_name, /):
    field_name = _text(field_name)
    index = 0
    while index < len(field_name) and field_name[index] not in '.[':
        index += 1
    return _field_index(field_name[:index]), _FieldIterator(field_name, index)


def _field(text, index):
    start = index
    size = len(text)
    while index < size and text[index] not in '!}:':
        if text[index] == '{':
            raise ValueError("unexpected '{' in field name")
        if text[index] == '[':
            index += 1
            while index < size and text[index] != ']':
                index += 1
        index += 1
    if index >= size:
        raise ValueError("expected '}' before end of string")
    name = text[start:index]
    conversion = None
    specification = ''
    if text[index] == '!':
        index += 1
        if index == size:
            raise ValueError("end of string while looking for conversion specifier")
        conversion = text[index]
        index += 1
        if index == size:
            raise ValueError("unmatched '{' in format spec")
        if text[index] not in ':}':
            raise ValueError("expected ':' after conversion specifier")
    if text[index] == ':':
        index += 1
        start = index
        depth = 1
        while index < size:
            if text[index] == '{':
                depth += 1
            elif text[index] == '}':
                depth -= 1
                if depth == 0:
                    break
            index += 1
        if index == size:
            raise ValueError("unmatched '{' in format spec")
        specification = text[start:index]
    return name, specification, conversion, index + 1


class _FormatIterator:
    def __init__(self, text):
        self.text = text
        self.index = 0

    def __iter__(self):
        return self

    def __next__(self):
        text = self.text
        start = self.index
        index = start
        size = len(text)
        if index == size:
            raise StopIteration
        while index < size:
            character = text[index]
            if character in '{}':
                index += 1
                if index == size:
                    raise ValueError("Single '" + character + "' encountered in format string")
                if text[index] == character:
                    self.index = index + 1
                    return text[start:index], None, None, None
                if character == '}':
                    raise ValueError("Single '}' encountered in format string")
                name, specification, conversion, end = _field(text, index)
                self.index = end
                return text[start:index - 1], name, specification, conversion
            index += 1
        self.index = size
        return text[start:size], None, None, None


def formatter_parser(format_string, /):
    return _FormatIterator(_text(format_string))
