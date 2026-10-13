"""Interpret bytes as packed binary data.

This is a pure-Python implementation of the interface CPython provides
in its ``_struct`` C module, written for Mule's compiled standard
library. It supports the standard format characters and byte orders
used by ``struct`` consumers such as ``zipfile``, ``base64`` and the
``importlib`` machinery.
"""

__all__ = [
    "Struct",
    "calcsize",
    "error",
    "iter_unpack",
    "pack",
    "pack_into",
    "unpack",
    "unpack_from",
]


class error(Exception):
    pass


def _clearcache():
    pass


# Standard sizes and native alignments for each format character.
# ``l``/``L`` are 8 bytes in native mode on this platform and 4 in
# standard mode, so they are handled specially where sizes are chosen.
_SIZES = {
    "x": 1,
    "c": 1,
    "b": 1,
    "B": 1,
    "?": 1,
    "h": 2,
    "H": 2,
    "i": 4,
    "I": 4,
    "q": 8,
    "Q": 8,
    "e": 2,
    "f": 4,
    "d": 8,
    "P": 8,
}

_SIGNED = {
    "b": (-128, 127, "byte format requires -128 <= number <= 127"),
    "h": (-32768, 32767, "short format requires -32768 <= number <= 32767"),
    "i": (
        -2147483648,
        2147483647,
        "'i' format requires -2147483648 <= number <= 2147483647",
    ),
    "l": (
        -9223372036854775808,
        9223372036854775807,
        "'l' format requires -9223372036854775808 <= number <= 9223372036854775807",
    ),
    "q": (
        -9223372036854775808,
        9223372036854775807,
        "'q' format requires -9223372036854775808 <= number <= 9223372036854775807",
    ),
}

_UNSIGNED = {
    "B": (255, "ubyte format requires 0 <= number <= 255"),
    "H": (65535, "ushort format requires 0 <= number <= 65535"),
    "I": (4294967295, "'I' format requires 0 <= number <= 4294967295"),
    "L": (
        18446744073709551615,
        "'L' format requires 0 <= number <= 18446744073709551615",
    ),
    "Q": (
        18446744073709551615,
        "'Q' format requires 0 <= number <= 18446744073709551615",
    ),
    "P": (18446744073709551615, "cannot fit 'int' into an offset-sized integer"),
}


def _parse(format):
    if isinstance(format, bytes):
        format = format.decode("ascii")
    if not isinstance(format, str):
        raise TypeError("struct format must be a string")
    order = "@"
    index = 0
    if index < len(format) and format[index] in "@=<>!":
        order = format[index]
        index += 1
    items = []
    while index < len(format):
        char = format[index]
        if char in " \t\n\r":
            index += 1
            continue
        count = 0
        has_count = False
        while index < len(format) and "0" <= format[index] <= "9":
            count = count * 10 + (ord(format[index]) - 48)
            has_count = True
            index += 1
        if index >= len(format):
            raise error("bad char in struct format")
        char = format[index]
        index += 1
        if char not in _SIZES and char not in "lLspP":
            raise error("bad char in struct format")
        if order != "@" and char == "P":
            raise error("bad char in struct format")
        items.append((char, count if has_count else 1, has_count))
    return order, items


def _item_size(order, char, count):
    if char in "sp":
        return count
    if char in "lL":
        return 8 if order == "@" else 4
    return _SIZES[char] * count


def _alignment(order, char, count):
    if order != "@" or char in "xcs p":
        return 1
    if char in "lL":
        return 8
    return _SIZES[char]


def _layout(order, items):
    # Return a list of (char, count, offset, size) with native padding.
    layout = []
    offset = 0
    for char, count, _has_count in items:
        if char in "sp":
            entry_size = count
            repeats = 1
        else:
            entry_size = _item_size(order, char, 1)
            repeats = count
        for _ in range(repeats):
            alignment = _alignment(order, char, count)
            if alignment > 1:
                offset += -offset % alignment
            size = _item_size(order, char, count) if char in "sp" else entry_size
            layout.append((char, count, offset, size))
            offset += size
    return layout, offset


def _integer_value(value):
    if isinstance(value, bool):
        return 1 if value else 0
    if isinstance(value, int):
        return value
    index_method = getattr(value, "__index__", None)
    if index_method is not None:
        return index_method()
    raise TypeError("required argument is not an integer")


def _int_bits(char, order, value):
    value = _integer_value(value)
    size = _item_size(order, char, 1)
    if char in _SIGNED:
        low, high, message = _SIGNED[char]
        if size == 4:
            low, high = -2147483648, 2147483647
            message = (
                "'" + char + "' format requires -2147483648 <= number <= 2147483647"
            )
        if value < low or value > high:
            raise error(message)
        return value & ((1 << (size * 8)) - 1), size
    limit, message = _UNSIGNED[char]
    if size == 4:
        limit = 4294967295
        message = "'" + char + "' format requires 0 <= number <= 4294967295"
    if value < 0 or value > limit:
        raise error(message)
    return value, size


def _write_bits(result, offset, bits, size, order):
    for position in range(size):
        if order == ">" or order == "!":
            shift = (size - 1 - position) * 8
        else:
            shift = position * 8
        result[offset + position] = (bits >> shift) & 0xFF


def _read_bits(buffer, offset, size, order):
    bits = 0
    for position in range(size):
        if order == ">" or order == "!":
            bits = (bits << 8) | buffer[offset + position]
        else:
            bits |= buffer[offset + position] << (position * 8)
    return bits


def _scale_pow2(value, exponent):
    while exponent > 0:
        value *= 2.0
        exponent -= 1
    while exponent < 0:
        value /= 2.0
        exponent += 1
    return value


def _round_half_even(value):
    floor = int(value)
    difference = value - floor
    if difference > 0.5:
        return floor + 1
    if difference < 0.5:
        return floor
    return floor + 1 if floor % 2 else floor


def _float_bits(value, mantissa_bits, exponent_bits):
    if isinstance(value, (int, float)):
        pass
    elif getattr(value, "__float__", None) is not None:
        value = value.__float__()
    else:
        raise TypeError("required argument is not a float")
    value = float(value)
    bias = (1 << (exponent_bits - 1)) - 1
    max_exponent = (1 << exponent_bits) - 1
    negative = value < 0.0 or (value == 0.0 and str(value).startswith("-"))
    magnitude = -value if value < 0.0 else value
    sign = (1 << (mantissa_bits + exponent_bits)) if negative else 0
    if value != value:
        return sign | (max_exponent << mantissa_bits) | (1 << (mantissa_bits - 1))
    if magnitude == float("inf"):
        return sign | (max_exponent << mantissa_bits)
    if magnitude == 0.0:
        return sign
    exponent = 0
    while magnitude >= 2.0:
        magnitude /= 2.0
        exponent += 1
    while magnitude < 1.0:
        magnitude *= 2.0
        exponent -= 1
    biased = exponent + bias
    if biased >= max_exponent:
        raise OverflowError("float too large to pack with this format")
    if biased <= 0:
        fraction = _round_half_even(_scale_pow2(magnitude, mantissa_bits + biased - 1))
        if fraction >= (1 << mantissa_bits):
            # Rounding carried into the smallest normal exponent.
            return sign | (1 << mantissa_bits)
        return sign | fraction
    fraction = _round_half_even((magnitude - 1.0) * (2.0**mantissa_bits))
    if fraction >= (1 << mantissa_bits):
        fraction = 0
        biased += 1
        if biased >= max_exponent:
            raise OverflowError("float too large to pack with this format")
    return sign | (biased << mantissa_bits) | fraction


def _bits_float(bits, mantissa_bits, exponent_bits):
    bias = (1 << (exponent_bits - 1)) - 1
    max_exponent = (1 << exponent_bits) - 1
    negative = bits >> (mantissa_bits + exponent_bits)
    exponent = (bits >> mantissa_bits) & max_exponent
    fraction = bits & ((1 << mantissa_bits) - 1)
    if exponent == max_exponent:
        value = float("inf") if fraction == 0 else float("nan")
    elif exponent == 0:
        value = _scale_pow2(float(fraction), 1 - bias - mantissa_bits)
    else:
        value = _scale_pow2(1.0 + fraction / (2.0**mantissa_bits), exponent - bias)
    return -value if negative else value


def _bytes_value(value, char):
    if isinstance(value, str):
        raise TypeError("argument for 's' must be a bytes object")
    return bytes(value)


class Struct:
    def __init__(self, format):
        self.format = format
        self._order, self._items = _parse(format)
        self._layout, self.size = _layout(self._order, self._items)

    def __repr__(self):
        return "Struct(" + repr(self.format) + ")"

    def pack(self, *values):
        result = bytearray(self.size)
        self._pack_into(result, 0, values)
        return bytes(result)

    def pack_into(self, buffer, offset, *values):
        if offset < 0:
            offset += len(buffer)
        if offset < 0 or offset + self.size > len(buffer):
            raise error(
                "pack_into requires a buffer of at least " + str(self.size) + " bytes"
            )
        scratch = bytearray(self.size)
        self._pack_into(scratch, 0, values)
        index = 0
        while index < self.size:
            buffer[offset + index] = scratch[index]
            index += 1

    def _pack_into(self, result, base, values):
        order = self._order
        position = 0
        needed = 0
        for char, count, _offset, _size in self._layout:
            if char != "x":
                needed += 1
        if len(values) != needed:
            raise error(
                "pack expected "
                + str(needed)
                + " items for packing (got "
                + str(len(values))
                + ")"
            )
        for char, count, offset, size in self._layout:
            offset += base
            if char == "x":
                continue
            value = values[position]
            position += 1
            if char in "bBhHiIlLqQBP":
                effective = char
                bits, written = _int_bits(effective, order, value)
                _write_bits(result, offset, bits, written, order)
            elif char == "?":
                result[offset] = 1 if value else 0
            elif char == "c":
                data = _bytes_value(value, char)
                if len(data) != 1:
                    raise TypeError("char format requires a bytes object of length 1")
                result[offset] = data[0]
            elif char == "s":
                data = _bytes_value(value, char)[:size]
                index = 0
                while index < len(data):
                    result[offset + index] = data[index]
                    index += 1
            elif char == "p":
                data = _bytes_value(value, char)
                length = min(len(data), size - 1, 255) if size else 0
                if size:
                    result[offset] = length
                    index = 0
                    while index < length:
                        result[offset + 1 + index] = data[index]
                        index += 1
            elif char == "e":
                _write_bits(result, offset, _float_bits(value, 10, 5), 2, order)
            elif char == "f":
                _write_bits(result, offset, _float_bits(value, 23, 8), 4, order)
            elif char == "d":
                _write_bits(result, offset, _float_bits(value, 52, 11), 8, order)

    def unpack(self, buffer):
        buffer = bytes(buffer)
        if len(buffer) != self.size:
            raise error("unpack requires a buffer of " + str(self.size) + " bytes")
        return self._unpack_from(buffer, 0)

    def unpack_from(self, buffer, offset=0):
        if offset < 0:
            offset += len(buffer)
        if offset < 0 or offset + self.size > len(buffer):
            raise error(
                "unpack_from requires a buffer of at least "
                + str(self.size + offset)
                + " bytes"
            )
        return self._unpack_from(bytes(buffer), offset)

    def _unpack_from(self, buffer, base):
        order = self._order
        values = []
        for char, count, offset, size in self._layout:
            offset += base
            if char == "x":
                continue
            if char in "bBhHiIlLqQBP":
                bits = _read_bits(buffer, offset, size, order)
                if char in _SIGNED or (char == "l"):
                    if char == "L" or char == "P":
                        values.append(bits)
                    else:
                        sign_bit = 1 << (size * 8 - 1)
                        values.append(
                            bits - (1 << (size * 8)) if bits & sign_bit else bits
                        )
                else:
                    values.append(bits)
            elif char == "?":
                values.append(buffer[offset] != 0)
            elif char == "c":
                values.append(bytes(buffer[offset : offset + 1]))
            elif char == "s":
                values.append(bytes(buffer[offset : offset + size]))
            elif char == "p":
                if size == 0:
                    values.append(b"")
                else:
                    length = min(buffer[offset], size - 1)
                    values.append(bytes(buffer[offset + 1 : offset + 1 + length]))
            elif char == "e":
                values.append(_bits_float(_read_bits(buffer, offset, 2, order), 10, 5))
            elif char == "f":
                values.append(_bits_float(_read_bits(buffer, offset, 4, order), 23, 8))
            elif char == "d":
                values.append(_bits_float(_read_bits(buffer, offset, 8, order), 52, 11))
        return tuple(values)

    def iter_unpack(self, buffer):
        buffer = bytes(buffer)
        if self.size == 0:
            raise ValueError("cannot iteratively unpack with a struct of length 0")
        if len(buffer) % self.size:
            raise error(
                "iterative unpacking requires a buffer of a multiple of "
                + str(self.size)
                + " bytes"
            )
        offset = 0
        while offset < len(buffer):
            yield self._unpack_from(buffer, offset)
            offset += self.size


def calcsize(format):
    return Struct(format).size


def pack(format, *values):
    return Struct(format).pack(*values)


def pack_into(format, buffer, offset, *values):
    Struct(format).pack_into(buffer, offset, *values)


def unpack(format, buffer):
    return Struct(format).unpack(buffer)


def unpack_from(format, buffer, offset=0):
    return Struct(format).unpack_from(buffer, offset)


def iter_unpack(format, buffer):
    return Struct(format).iter_unpack(buffer)
