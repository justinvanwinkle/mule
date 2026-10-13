"""Marshal serialization, as CPython's builtin ``marshal`` module.

This implements version 4 data for the types marshal supports without
code objects: scalars, strings, bytes, and containers, with references.
"""

version = 4

_FLAG_REF = 0x80


class _Writer:
    def __init__(self):
        self.data = bytearray()
        self.refs = []

    def write(self, value):
        for index, existing in enumerate(self.refs):
            if existing is value and isinstance(value, (str, bytes, tuple, list, dict, set)):
                self.data.append(ord('r'))
                self._int32(index)
                return
        kind = type(value)
        if value is None:
            self.data.append(ord('N'))
        elif value is True:
            self.data.append(ord('T'))
        elif value is False:
            self.data.append(ord('F'))
        elif kind is int:
            self.refs.append(value)
            if -2147483648 <= value <= 2147483647:
                self.data.append(ord('i') | _FLAG_REF)
                self._int32(value)
            elif -9223372036854775808 <= value <= 9223372036854775807:
                self.data.append(ord('I') | _FLAG_REF)
                self.data += value.to_bytes(8, 'little', signed=True)
            else:
                self.data.append(ord('l') | _FLAG_REF)
                negative = value < 0
                magnitude = -value if negative else value
                digits = []
                while magnitude:
                    digits.append(magnitude & 0x7FFF)
                    magnitude >>= 15
                self._int32(-len(digits) if negative else len(digits))
                for digit in digits:
                    self.data += digit.to_bytes(2, 'little')
        elif kind is float:
            self.refs.append(value)
            self.data.append(ord('g') | _FLAG_REF)
            import struct as _struct
            self.data += _struct.pack('<d', value)
        elif kind is complex:
            self.refs.append(value)
            self.data.append(ord('y') | _FLAG_REF)
            import struct as _struct
            self.data += _struct.pack('<dd', value.real, value.imag)
        elif kind is bytes or kind is bytearray:
            self.refs.append(value)
            data = bytes(value)
            self.data.append(ord('s') | _FLAG_REF)
            self._int32(len(data))
            self.data += data
        elif kind is str:
            self.refs.append(value)
            data = value.encode('utf-8')
            self.data.append(ord('u') | _FLAG_REF)
            self._int32(len(data))
            self.data += data
        elif kind is tuple or kind is list or kind is set or kind is frozenset:
            self.refs.append(value)
            marker = {tuple: '(', list: '[', set: '<', frozenset: '>'}[kind]
            self.data.append(ord(marker) | _FLAG_REF)
            self._int32(len(value))
            for item in value:
                self.write(item)
        elif kind is dict:
            self.refs.append(value)
            self.data.append(ord('{') | _FLAG_REF)
            for key, item in value.items():
                self.write(key)
                self.write(item)
            self.data.append(ord('0'))
        else:
            raise ValueError('unmarshallable object')

    def _int32(self, value):
        self.data += value.to_bytes(4, 'little', signed=True)


class _Reader:
    def __init__(self, data):
        self.data = bytes(data)
        self.index = 0
        self.refs = []

    def read(self):
        code = self.data[self.index]
        self.index += 1
        flagged = code & _FLAG_REF
        code &= ~_FLAG_REF
        char = chr(code)
        if char == 'N':
            return None
        if char == 'T':
            return True
        if char == 'F':
            return False
        if char == 'r':
            return self.refs[self._int32()]
        if char == 'i':
            value = self._int32()
            return self._ref(value, flagged)
        if char == 'I':
            value = int.from_bytes(self._take(8), 'little', signed=True)
            return self._ref(value, flagged)
        if char == 'l':
            count = self._int32()
            negative = count < 0
            count = -count if negative else count
            value = 0
            for position in range(count):
                digit = int.from_bytes(self._take(2), 'little')
                value |= digit << (15 * position)
            return self._ref(-value if negative else value, flagged)
        if char == 'g':
            import struct as _struct
            value = _struct.unpack('<d', self._take(8))[0]
            return self._ref(value, flagged)
        if char == 'y':
            import struct as _struct
            real, imag = _struct.unpack('<dd', self._take(16))
            return self._ref(complex(real, imag), flagged)
        if char == 's':
            value = self._take(self._int32())
            return self._ref(value, flagged)
        if char in ('u', 'Z', 'a', 'A', 'z'):
            if char in ('z', 'Z'):
                size = self.data[self.index]
                self.index += 1
            else:
                size = self._int32()
            raw = self._take(size)
            value = raw.decode('utf-8') if char in ('u', 'Z') else raw.decode('ascii')
            return self._ref(value, flagged)
        if char in ('(', '[', '<', '>'):
            size = self._int32()
            slot = len(self.refs)
            if flagged:
                self.refs.append(None)
            items = [self.read() for _ in range(size)]
            if char == '(':
                value = tuple(items)
            elif char == '[':
                value = items
            elif char == '<':
                value = set(items)
            else:
                value = frozenset(items)
            if flagged:
                self.refs[slot] = value
            return value
        if char == '{':
            slot = len(self.refs)
            if flagged:
                self.refs.append(None)
            value = {}
            while True:
                key_code = self.data[self.index]
                if chr(key_code & ~_FLAG_REF) == '0':
                    self.index += 1
                    break
                key = self.read()
                value[key] = self.read()
            if flagged:
                self.refs[slot] = value
            return value
        if char == '0':
            return None
        raise ValueError(f'bad marshal data (unknown type code {char!r})')

    def _ref(self, value, flagged):
        if flagged:
            self.refs.append(value)
        return value

    def _int32(self):
        return int.from_bytes(self._take(4), 'little', signed=True)

    def _take(self, count):
        result = self.data[self.index:self.index + count]
        self.index += count
        return result


def dumps(value, version=4):
    writer = _Writer()
    writer.write(value)
    return bytes(writer.data)


def loads(data):
    reader = _Reader(data)
    return reader.read()


def dump(value, file, version=4):
    file.write(dumps(value))


def load(file):
    return loads(file.read())
