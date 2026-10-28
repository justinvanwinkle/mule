"""Implementation of JSONDecoder
"""
import re

from json import scanner
c_scanstring = None

__all__ = ['JSONDecoder', 'JSONDecodeError']

FLAGS = re.VERBOSE | re.MULTILINE | re.DOTALL

NaN = float('nan')
PosInf = float('inf')
NegInf = float('-inf')


class JSONDecodeError(ValueError):
    """Subclass of ValueError with the following additional properties:

    msg: The unformatted error message
    doc: The JSON document being parsed
    pos: The start index of doc where parsing failed
    lineno: The line corresponding to pos
    colno: The column corresponding to pos

    """
    # Note that this exception is used from _json
    def __init__(self, msg, doc, pos):
        lineno = doc.count('\n', 0, pos) + 1
        colno = pos - doc.rfind('\n', 0, pos)
        errmsg = '%s: line %d column %d (char %d)' % (msg, lineno, colno, pos)
        ValueError.__init__(self, errmsg)
        self.msg = msg
        self.doc = doc
        self.pos = pos
        self.lineno = lineno
        self.colno = colno

    def __reduce__(self):
        return self.__class__, (self.msg, self.doc, self.pos)


_CONSTANTS = {
    '-Infinity': NegInf,
    'Infinity': PosInf,
    'NaN': NaN,
}


HEXDIGITS = re.compile(r'[0-9A-Fa-f]{4}', FLAGS)
STRINGCHUNK = re.compile(r'(.*?)(["\\\x00-\x1f])', FLAGS)
BACKSLASH = {
    '"': '"', '\\': '\\', '/': '/',
    'b': '\b', 'f': '\f', 'n': '\n', 'r': '\r', 't': '\t',
}

def _hex_digit(code: int) -> int:
    if 48 <= code <= 57:
        return code - 48
    if 97 <= code <= 102:
        return code - 87
    if 65 <= code <= 70:
        return code - 55
    return -1


def _decode_uXXXX(s: str, pos: int, _m=HEXDIGITS.match) -> int:
    if _m is not HEXDIGITS.match:
        esc = _m(s, pos + 1)
        if esc is not None:
            try:
                return int(esc.group(), 16)
            except ValueError:
                pass
        msg = "Invalid \\uXXXX escape"
        raise JSONDecodeError(msg, s, pos)
    if pos + 5 <= len(s):
        d0 = _hex_digit(ord(s[pos + 1]))
        d1 = _hex_digit(ord(s[pos + 2]))
        d2 = _hex_digit(ord(s[pos + 3]))
        d3 = _hex_digit(ord(s[pos + 4]))
        if (d0 | d1 | d2 | d3) >= 0:
            return (d0 << 12) | (d1 << 8) | (d2 << 4) | d3
    msg = "Invalid \\uXXXX escape"
    raise JSONDecodeError(msg, s, pos)


def _scan_clean_string(s: str, i: int, n: int, strict: bool) -> int:
    if strict:
        while i < n:
            ch = s[i]
            if ch == '"':
                return i
            if ch == '\\' or ch < ' ':
                return -1 - i
            i += 1
    else:
        while i < n:
            ch = s[i]
            if ch == '"':
                return i
            if ch == '\\':
                return -1 - i
            i += 1
    return -1 - n


def py_scanstring(s: str, end: int, strict: bool = True,
        _b=BACKSLASH, _m=STRINGCHUNK.match):
    """Scan the string s for a JSON string. End is the index of the
    character in s after the quote that started the JSON string.
    Unescapes all valid JSON string escape sequences and raises ValueError
    on attempt to decode an invalid string. If strict is False then literal
    control characters are allowed in the string.

    Returns a tuple of the decoded string and the index of the character in s
    after the end quote."""
    if end > 9223372036854775807 or end < -9223372036854775808:
        raise OverflowError("Python int too large to convert to C ssize_t")
    is_strict: bool = True if strict else False
    n: int = len(s)
    begin: int = end - 1
    if end < 0:
        raise JSONDecodeError("Unterminated string starting at", s, begin)
    first_pos: int = _scan_clean_string(s, end, n, is_strict)
    if first_pos >= 0:
        return s[end:first_pos], first_pos + 1

    chunks: list[str] = []
    _append = chunks.append
    stop: int = -1 - first_pos
    while True:
        if stop >= n:
            raise JSONDecodeError("Unterminated string starting at", s, begin)
        if stop > end:
            _append(s[end:stop])
        end = stop + 1
        term_ch = s[stop]
        if term_ch == '"':
            break
        if term_ch != '\\':
            if is_strict:
                terminator = s[stop]
                msg = f"Invalid control character {terminator!r} at"
                raise JSONDecodeError(msg, s, stop)
            _append(s[stop])
            next_pos: int = _scan_clean_string(s, end, n, is_strict)
            if next_pos >= 0:
                if next_pos > end:
                    _append(s[end:next_pos])
                end = next_pos + 1
                break
            stop = -1 - next_pos
            continue
        if end >= n:
            raise JSONDecodeError("Unterminated string starting at", s, begin)
        esc = s[end]
        if esc != 'u':
            try:
                char = _b[esc]
            except KeyError:
                msg = f"Invalid \\escape: {esc!r}"
                raise JSONDecodeError(msg, s, end) from None
            end += 1
        else:
            uni = _decode_uXXXX(s, end)
            end += 5
            if 0xd800 <= uni <= 0xdbff and end + 1 < n and s[end] == '\\' and s[end + 1] == 'u':
                uni2 = _decode_uXXXX(s, end + 1)
                if 0xdc00 <= uni2 <= 0xdfff:
                    uni = 0x10000 + (((uni - 0xd800) << 10) | (uni2 - 0xdc00))
                    end += 6
            char = chr(uni)
        _append(char)
        next_pos = _scan_clean_string(s, end, n, is_strict)
        if next_pos >= 0:
            if next_pos > end:
                _append(s[end:next_pos])
            end = next_pos + 1
            break
        stop = -1 - next_pos
    return ''.join(chunks), end


# Use speedup if available
scanstring = c_scanstring or py_scanstring

WHITESPACE = re.compile(r'[ \t\n\r]*', FLAGS)
WHITESPACE_STR = ' \t\n\r'


def _skip_whitespace(s: str, idx: int, n: int) -> int:
    while idx < n:
        ch = s[idx]
        if ch == ' ' or ch == '\t' or ch == '\n' or ch == '\r':
            idx += 1
        else:
            break
    return idx


def _scan_number_fast(string: str, idx: int, n: int) -> int:
    i: int = idx
    if i >= n:
        return 0
    ch = string[i]
    if ch == '-':
        i += 1
        if i >= n:
            return 0
        ch = string[i]
    if ch == '0':
        i += 1
    elif '1' <= ch <= '9':
        i += 1
        while i < n and '0' <= string[i] <= '9':
            i += 1
    else:
        return 0

    is_float: bool = False
    if i < n and string[i] == '.':
        frac_start: int = i + 1
        if frac_start < n and '0' <= string[frac_start] <= '9':
            is_float = True
            i = frac_start + 1
            while i < n and '0' <= string[i] <= '9':
                i += 1

    if i < n:
        exp_ch = string[i]
        if exp_ch == 'e' or exp_ch == 'E':
            j: int = i + 1
            if j < n:
                sign_ch = string[j]
                if sign_ch == '+' or sign_ch == '-':
                    j += 1
            if j < n and '0' <= string[j] <= '9':
                is_float = True
                i = j + 1
                while i < n and '0' <= string[i] <= '9':
                    i += 1

    if is_float:
        return -i
    return i


def _scan_once_fast(
    string: str,
    idx: int,
    strict: bool,
    object_hook,
    object_pairs_hook,
    memo: dict,
):
    if idx < 0:
        raise StopIteration(idx)
    n: int = len(string)
    if idx >= n:
        raise StopIteration(idx)
    ch = string[idx]

    if ch == '"':
        return py_scanstring(string, idx + 1, strict)
    if ch == '{':
        return _json_object_fast(
            string, idx + 1, strict, object_hook, object_pairs_hook, memo
        )
    if ch == '[':
        return _json_array_fast(
            string, idx + 1, strict, object_hook, object_pairs_hook, memo
        )
    if ch == 'n' and idx + 4 <= n and string[idx:idx + 4] == 'null':
        return None, idx + 4
    if ch == 't' and idx + 4 <= n and string[idx:idx + 4] == 'true':
        return True, idx + 4
    if ch == 'f' and idx + 5 <= n and string[idx:idx + 5] == 'false':
        return False, idx + 5

    if ch == '-' or ('0' <= ch <= '9'):
        num_end: int = _scan_number_fast(string, idx, n)
        if num_end != 0:
            if num_end > 0:
                return int(string[idx:num_end]), num_end
            end_pos: int = -num_end
            return float(string[idx:end_pos]), end_pos
        if ch == '-' and idx + 9 <= n and string[idx:idx + 9] == '-Infinity':
            return NegInf, idx + 9
        raise StopIteration(idx)
    if ch == 'N' and idx + 3 <= n and string[idx:idx + 3] == 'NaN':
        return NaN, idx + 3
    if ch == 'I' and idx + 8 <= n and string[idx:idx + 8] == 'Infinity':
        return PosInf, idx + 8
    raise StopIteration(idx)


def _json_object_fast(
    s: str,
    end: int,
    strict: bool,
    object_hook,
    object_pairs_hook,
    memo: dict,
):
    pairs = []
    pairs_append = pairs.append
    memo_get = memo.setdefault
    n: int = len(s)
    if end < n:
        ch = s[end]
        if ch != '"':
            if ch == ' ' or ch == '\t' or ch == '\n' or ch == '\r':
                end = _skip_whitespace(s, end + 1, n)
            if end < n:
                ch2 = s[end]
                if ch2 == '}':
                    if object_pairs_hook is not None:
                        result = object_pairs_hook(pairs)
                        return result, end + 1
                    pairs_dict = {}
                    if object_hook is not None:
                        pairs_dict = object_hook(pairs_dict)
                    return pairs_dict, end + 1
                if ch2 != '"':
                    raise JSONDecodeError(
                        "Expecting property name enclosed in double quotes", s, end)
            else:
                raise JSONDecodeError(
                    "Expecting property name enclosed in double quotes", s, end)
    else:
        raise JSONDecodeError(
            "Expecting property name enclosed in double quotes", s, end)
    end += 1
    while True:
        key, end = py_scanstring(s, end, strict)
        key = memo_get(key, key)
        if end >= n or s[end] != ':':
            end = _skip_whitespace(s, end, n)
            if end >= n or s[end] != ':':
                raise JSONDecodeError("Expecting ':' delimiter", s, end)
        end += 1

        if end < n:
            ws1 = s[end]
            if ws1 == ' ' or ws1 == '\t' or ws1 == '\n' or ws1 == '\r':
                end += 1
                if end < n:
                    ws2 = s[end]
                    if ws2 == ' ' or ws2 == '\t' or ws2 == '\n' or ws2 == '\r':
                        end = _skip_whitespace(s, end + 1, n)

        try:
            value, end = _scan_once_fast(
                s, end, strict, object_hook, object_pairs_hook, memo
            )
        except StopIteration as err:
            raise JSONDecodeError("Expecting value", s, err.value) from None
        pairs_append((key, value))

        if end < n:
            ws3 = s[end]
            if ws3 == ' ' or ws3 == '\t' or ws3 == '\n' or ws3 == '\r':
                end += 1
                if end < n:
                    ws4 = s[end]
                    if ws4 == ' ' or ws4 == '\t' or ws4 == '\n' or ws4 == '\r':
                        end = _skip_whitespace(s, end + 1, n)
        if end >= n:
            raise JSONDecodeError("Expecting ',' delimiter", s, end)
        delim = s[end]
        end += 1
        if delim == '}':
            break
        if delim != ',':
            raise JSONDecodeError("Expecting ',' delimiter", s, end - 1)
        comma_idx: int = end - 1

        if end < n:
            ws5 = s[end]
            if ws5 == ' ' or ws5 == '\t' or ws5 == '\n' or ws5 == '\r':
                end += 1
                if end < n:
                    ws6 = s[end]
                    if ws6 == ' ' or ws6 == '\t' or ws6 == '\n' or ws6 == '\r':
                        end = _skip_whitespace(s, end + 1, n)
        if end >= n:
            raise JSONDecodeError(
                "Expecting property name enclosed in double quotes", s, end)
        quote_ch = s[end]
        end += 1
        if quote_ch != '"':
            if quote_ch == '}':
                raise JSONDecodeError(
                    "Illegal trailing comma before end of object", s, comma_idx)
            raise JSONDecodeError(
                "Expecting property name enclosed in double quotes", s, end - 1)
    if object_pairs_hook is not None:
        result = object_pairs_hook(pairs)
        return result, end
    pairs_dict = dict(pairs)
    if object_hook is not None:
        pairs_dict = object_hook(pairs_dict)
    return pairs_dict, end


def _json_array_fast(
    s: str,
    end: int,
    strict: bool,
    object_hook,
    object_pairs_hook,
    memo: dict,
):
    values = []
    n: int = len(s)
    if end < n:
        ws0 = s[end]
        if ws0 == ' ' or ws0 == '\t' or ws0 == '\n' or ws0 == '\r':
            end = _skip_whitespace(s, end + 1, n)
    if end < n and s[end] == ']':
        return values, end + 1
    _append = values.append
    while True:
        try:
            value, end = _scan_once_fast(
                s, end, strict, object_hook, object_pairs_hook, memo
            )
        except StopIteration as err:
            raise JSONDecodeError("Expecting value", s, err.value) from None
        _append(value)
        if end < n:
            ws1 = s[end]
            if ws1 == ' ' or ws1 == '\t' or ws1 == '\n' or ws1 == '\r':
                end = _skip_whitespace(s, end + 1, n)
        if end >= n:
            raise JSONDecodeError("Expecting ',' delimiter", s, end)
        delim = s[end]
        end += 1
        if delim == ']':
            break
        if delim != ',':
            raise JSONDecodeError("Expecting ',' delimiter", s, end - 1)
        comma_idx: int = end - 1
        if end < n:
            ws2 = s[end]
            if ws2 == ' ' or ws2 == '\t' or ws2 == '\n' or ws2 == '\r':
                end = _skip_whitespace(s, end + 1, n)
        if end < n and s[end] == ']':
            raise JSONDecodeError(
                "Illegal trailing comma before end of array", s, comma_idx)

    return values, end


def _json_object(s: str, end: int, strict, scan_once, object_hook,
                 object_pairs_hook, memo):
    pairs = []
    pairs_append = pairs.append
    if memo is None:
        memo = {}
    memo_get = memo.setdefault
    n: int = len(s)
    if end < n:
        ch = s[end]
        if ch != '"':
            if ch == ' ' or ch == '\t' or ch == '\n' or ch == '\r':
                end = _skip_whitespace(s, end + 1, n)
            if end < n:
                ch2 = s[end]
                if ch2 == '}':
                    if object_pairs_hook is not None:
                        result = object_pairs_hook(pairs)
                        return result, end + 1
                    pairs_dict = {}
                    if object_hook is not None:
                        pairs_dict = object_hook(pairs_dict)
                    return pairs_dict, end + 1
                if ch2 != '"':
                    raise JSONDecodeError(
                        "Expecting property name enclosed in double quotes", s, end)
            else:
                raise JSONDecodeError(
                    "Expecting property name enclosed in double quotes", s, end)
    else:
        raise JSONDecodeError(
            "Expecting property name enclosed in double quotes", s, end)
    end += 1
    while True:
        key, end = scanstring(s, end, strict)
        key = memo_get(key, key)
        if end >= n or s[end] != ':':
            end = _skip_whitespace(s, end, n)
            if end >= n or s[end] != ':':
                raise JSONDecodeError("Expecting ':' delimiter", s, end)
        end += 1

        if end < n:
            ws1 = s[end]
            if ws1 == ' ' or ws1 == '\t' or ws1 == '\n' or ws1 == '\r':
                end += 1
                if end < n:
                    ws2 = s[end]
                    if ws2 == ' ' or ws2 == '\t' or ws2 == '\n' or ws2 == '\r':
                        end = _skip_whitespace(s, end + 1, n)

        try:
            value, end = scan_once(s, end)
        except StopIteration as err:
            raise JSONDecodeError("Expecting value", s, err.value) from None
        pairs_append((key, value))

        if end < n:
            ws3 = s[end]
            if ws3 == ' ' or ws3 == '\t' or ws3 == '\n' or ws3 == '\r':
                end += 1
                if end < n:
                    ws4 = s[end]
                    if ws4 == ' ' or ws4 == '\t' or ws4 == '\n' or ws4 == '\r':
                        end = _skip_whitespace(s, end + 1, n)
        if end >= n:
            raise JSONDecodeError("Expecting ',' delimiter", s, end)
        delim = s[end]
        end += 1
        if delim == '}':
            break
        if delim != ',':
            raise JSONDecodeError("Expecting ',' delimiter", s, end - 1)
        comma_idx: int = end - 1

        if end < n:
            ws5 = s[end]
            if ws5 == ' ' or ws5 == '\t' or ws5 == '\n' or ws5 == '\r':
                end += 1
                if end < n:
                    ws6 = s[end]
                    if ws6 == ' ' or ws6 == '\t' or ws6 == '\n' or ws6 == '\r':
                        end = _skip_whitespace(s, end + 1, n)
        if end >= n:
            raise JSONDecodeError(
                "Expecting property name enclosed in double quotes", s, end)
        quote_ch = s[end]
        end += 1
        if quote_ch != '"':
            if quote_ch == '}':
                raise JSONDecodeError(
                    "Illegal trailing comma before end of object", s, comma_idx)
            raise JSONDecodeError(
                "Expecting property name enclosed in double quotes", s, end - 1)
    if object_pairs_hook is not None:
        result = object_pairs_hook(pairs)
        return result, end
    pairs_dict = dict(pairs)
    if object_hook is not None:
        pairs_dict = object_hook(pairs_dict)
    return pairs_dict, end


def JSONObject(s_and_end, strict, scan_once, object_hook, object_pairs_hook,
               memo=None, _w=WHITESPACE.match, _ws=WHITESPACE_STR):
    s, end = s_and_end
    return _json_object(s, end, strict, scan_once, object_hook, object_pairs_hook, memo)


def _json_array(s: str, end: int, scan_once):
    values = []
    n: int = len(s)
    if end < n:
        ws0 = s[end]
        if ws0 == ' ' or ws0 == '\t' or ws0 == '\n' or ws0 == '\r':
            end = _skip_whitespace(s, end + 1, n)
    if end < n and s[end] == ']':
        return values, end + 1
    _append = values.append
    while True:
        try:
            value, end = scan_once(s, end)
        except StopIteration as err:
            raise JSONDecodeError("Expecting value", s, err.value) from None
        _append(value)
        if end < n:
            ws1 = s[end]
            if ws1 == ' ' or ws1 == '\t' or ws1 == '\n' or ws1 == '\r':
                end = _skip_whitespace(s, end + 1, n)
        if end >= n:
            raise JSONDecodeError("Expecting ',' delimiter", s, end)
        delim = s[end]
        end += 1
        if delim == ']':
            break
        if delim != ',':
            raise JSONDecodeError("Expecting ',' delimiter", s, end - 1)
        comma_idx: int = end - 1
        if end < n:
            ws2 = s[end]
            if ws2 == ' ' or ws2 == '\t' or ws2 == '\n' or ws2 == '\r':
                end = _skip_whitespace(s, end + 1, n)
        if end < n and s[end] == ']':
            raise JSONDecodeError(
                "Illegal trailing comma before end of array", s, comma_idx)

    return values, end


def JSONArray(s_and_end, scan_once, _w=WHITESPACE.match, _ws=WHITESPACE_STR):
    s, end = s_and_end
    return _json_array(s, end, scan_once)


class JSONDecoder(object):
    """Simple JSON <https://json.org> decoder

    Performs the following translations in decoding by default:

    +---------------+-------------------+
    | JSON          | Python            |
    +===============+===================+
    | object        | dict              |
    +---------------+-------------------+
    | array         | list              |
    +---------------+-------------------+
    | string        | str               |
    +---------------+-------------------+
    | number (int)  | int               |
    +---------------+-------------------+
    | number (real) | float             |
    +---------------+-------------------+
    | true          | True              |
    +---------------+-------------------+
    | false         | False             |
    +---------------+-------------------+
    | null          | None              |
    +---------------+-------------------+

    It also understands ``NaN``, ``Infinity``, and ``-Infinity`` as
    their corresponding ``float`` values, which is outside the JSON spec.

    """

    def __init__(self, *, object_hook=None, parse_float=None,
            parse_int=None, parse_constant=None, strict=True,
            object_pairs_hook=None):
        """``object_hook``, if specified, will be called with the result
        of every JSON object decoded and its return value will be used in
        place of the given ``dict``.  This can be used to provide custom
        deserializations (e.g. to support JSON-RPC class hinting).

        ``object_pairs_hook``, if specified will be called with the result
        of every JSON object decoded with an ordered list of pairs.  The
        return value of ``object_pairs_hook`` will be used instead of the
        ``dict``.  This feature can be used to implement custom decoders.
        If ``object_hook`` is also defined, the ``object_pairs_hook`` takes
        priority.

        ``parse_float``, if specified, will be called with the string
        of every JSON float to be decoded. By default this is equivalent to
        float(num_str). This can be used to use another datatype or parser
        for JSON floats (e.g. decimal.Decimal).

        ``parse_int``, if specified, will be called with the string
        of every JSON int to be decoded. By default this is equivalent to
        int(num_str). This can be used to use another datatype or parser
        for JSON integers (e.g. float).

        ``parse_constant``, if specified, will be called with one of the
        following strings: -Infinity, Infinity, NaN.
        This can be used to raise an exception if invalid JSON numbers
        are encountered.

        If ``strict`` is false (true is the default), then control
        characters will be allowed inside strings.  Control characters in
        this context are those with character codes in the 0-31 range,
        including ``'\\t'`` (tab), ``'\\n'``, ``'\\r'`` and ``'\\0'``.
        """
        self.object_hook = object_hook
        self.parse_float = parse_float or float
        self.parse_int = parse_int or int
        self.parse_constant = parse_constant or _CONSTANTS.__getitem__
        self.strict = strict
        self.object_pairs_hook = object_pairs_hook
        self.parse_object = JSONObject
        self.parse_array = JSONArray
        self.parse_string = scanstring
        self.memo = {}
        self.scan_once = scanner.make_scanner(self)
        self._default_scan_once = self.scan_once


    def decode(self, s: str, _w=WHITESPACE.match):
        """Return the Python representation of ``s`` (a ``str`` instance
        containing a JSON document).

        """
        if _w is WHITESPACE.match and type(s) is str:
            n: int = len(s)
            idx: int = _skip_whitespace(s, 0, n)
            obj, end = self.raw_decode(s, idx=idx)
            end = _skip_whitespace(s, end, n)
            if end != n:
                raise JSONDecodeError("Extra data", s, end)
            return obj
        obj, end = self.raw_decode(s, idx=_w(s, 0).end())
        end = _w(s, end).end()
        if end != len(s):
            raise JSONDecodeError("Extra data", s, end)
        return obj

    def raw_decode(self, s: str, idx: int = 0):
        """Decode a JSON document from ``s`` (a ``str`` beginning with
        a JSON document) and return a 2-tuple of the Python
        representation and the index in ``s`` where the document ended.

        This can be used to decode a JSON document from a string that may
        have extraneous data at the end.

        """
        if (
            type(s) is str
            and type(idx) is int
            and self.scan_once is self._default_scan_once
            and self.parse_object is JSONObject
            and self.parse_array is JSONArray
            and self.parse_string is scanstring
            and self.parse_int is int
            and self.parse_float is float
            and self.parse_constant is _CONSTANTS.__getitem__
        ):
            memo = self.memo
            is_strict: bool = True if self.strict else False
            try:
                obj, end = _scan_once_fast(
                    s,
                    idx,
                    is_strict,
                    self.object_hook,
                    self.object_pairs_hook,
                    memo,
                )
            except StopIteration as err:
                raise JSONDecodeError("Expecting value", s, err.value) from None
            finally:
                memo.clear()
            return obj, end
        try:
            obj, end = self.scan_once(s, idx)
        except StopIteration as err:
            raise JSONDecodeError("Expecting value", s, err.value) from None
        return obj, end
