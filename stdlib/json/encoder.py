"""Implementation of JSONEncoder
"""
import re

c_encode_basestring_ascii = None
c_encode_basestring = None
c_make_encoder = None

ESCAPE = re.compile(r'[\x00-\x1f\\"\b\f\n\r\t]')
ESCAPE_ASCII = re.compile(r'([\\"]|[^\ -~])')
HAS_UTF8 = re.compile(b'[\x80-\xff]')
ESCAPE_DCT = {
    '\\': '\\\\',
    '"': '\\"',
    '\b': '\\b',
    '\f': '\\f',
    '\n': '\\n',
    '\r': '\\r',
    '\t': '\\t',
}
for i in range(0x20):
    ESCAPE_DCT.setdefault(chr(i), '\\u{0:04x}'.format(i))
    #ESCAPE_DCT.setdefault(chr(i), '\\u%04x' % (i,))
del i

INFINITY = float('inf')

def _find_escape(s: str, i: int, n: int) -> int:
    while i < n:
        ch = s[i]
        if ch < ' ' or ch == '"' or ch == '\\':
            return i
        i += 1
    return n


def py_encode_basestring(s: str) -> str:
    """Return a JSON representation of a Python string

    """
    n: int = len(s)
    first: int = _find_escape(s, 0, n)
    if first == n:
        return '"' + s + '"'
    chunks: list[str] = ['"']
    _append = chunks.append
    start: int = 0
    i: int = first
    while i < n:
        if i > start:
            _append(s[start:i])
        _append(ESCAPE_DCT[s[i]])
        i += 1
        start = i
        i = _find_escape(s, i, n)
    if start < n:
        _append(s[start:n])
    _append('"')
    return ''.join(chunks)


encode_basestring = (c_encode_basestring or py_encode_basestring)


_HEX_BYTE = tuple(f"{i:02x}" for i in range(256))


def _find_escape_ascii(s: str, i: int, n: int) -> int:
    while i < n:
        ch = s[i]
        if ch < ' ' or ch > '~' or ch == '"' or ch == '\\':
            return i
        i += 1
    return n


def py_encode_basestring_ascii(s: str) -> str:
    """Return an ASCII-only JSON representation of a Python string

    """
    n: int = len(s)
    first: int = _find_escape_ascii(s, 0, n)
    if first == n:
        return '"' + s + '"'
    chunks: list[str] = ['"']
    _append = chunks.append
    start: int = 0
    i: int = first
    while i < n:
        if i > start:
            _append(s[start:i])
        ch_str = s[i]
        esc = ESCAPE_DCT.get(ch_str)
        if esc is not None:
            _append(esc)
        else:
            code: int = ord(ch_str)
            if code < 0x10000:
                _append('\\u' + _HEX_BYTE[code >> 8] + _HEX_BYTE[code & 0xff])
            else:
                code -= 0x10000
                s1: int = 0xd800 | ((code >> 10) & 0x3ff)
                s2: int = 0xdc00 | (code & 0x3ff)
                _append(
                    '\\u'
                    + _HEX_BYTE[s1 >> 8]
                    + _HEX_BYTE[s1 & 0xff]
                    + '\\u'
                    + _HEX_BYTE[s2 >> 8]
                    + _HEX_BYTE[s2 & 0xff]
                )
        i += 1
        start = i
        i = _find_escape_ascii(s, i, n)
    if start < n:
        _append(s[start:n])
    _append('"')
    return ''.join(chunks)


encode_basestring_ascii = (
    c_encode_basestring_ascii or py_encode_basestring_ascii)

class JSONEncoder(object):
    """Extensible JSON <https://json.org> encoder for Python data structures.

    Supports the following objects and types by default:

    +-------------------+---------------+
    | Python            | JSON          |
    +===================+===============+
    | dict              | object        |
    +-------------------+---------------+
    | list, tuple       | array         |
    +-------------------+---------------+
    | str               | string        |
    +-------------------+---------------+
    | int, float        | number        |
    +-------------------+---------------+
    | True              | true          |
    +-------------------+---------------+
    | False             | false         |
    +-------------------+---------------+
    | None              | null          |
    +-------------------+---------------+

    To extend this to recognize other objects, subclass and implement a
    ``.default()`` method with another method that returns a serializable
    object for ``o`` if possible, otherwise it should call the superclass
    implementation (to raise ``TypeError``).

    """
    item_separator = ', '
    key_separator = ': '
    def __init__(self, *, skipkeys=False, ensure_ascii=True,
            check_circular=True, allow_nan=True, sort_keys=False,
            indent=None, separators=None, default=None):
        """Constructor for JSONEncoder, with sensible defaults.

        If skipkeys is false, then it is a TypeError to attempt
        encoding of keys that are not str, int, float, bool or None.
        If skipkeys is True, such items are simply skipped.

        If ensure_ascii is true, the output is guaranteed to be str objects
        with all incoming non-ASCII and non-printable characters escaped.
        If ensure_ascii is false, the output can contain non-ASCII and
        non-printable characters.

        If check_circular is true, then lists, dicts, and custom encoded
        objects will be checked for circular references during encoding to
        prevent an infinite recursion (which would cause an RecursionError).
        Otherwise, no such check takes place.

        If allow_nan is true, then NaN, Infinity, and -Infinity will be
        encoded as such.  This behavior is not JSON specification compliant,
        but is consistent with most JavaScript based encoders and decoders.
        Otherwise, it will be a ValueError to encode such floats.

        If sort_keys is true, then the output of dictionaries will be
        sorted by key; this is useful for regression tests to ensure
        that JSON serializations can be compared on a day-to-day basis.

        If indent is a non-negative integer, then JSON array
        elements and object members will be pretty-printed with that
        indent level.  An indent level of 0 will only insert newlines.
        None is the most compact representation.

        If specified, separators should be an (item_separator,
        key_separator) tuple.  The default is (', ', ': ') if *indent* is
        ``None`` and (',', ': ') otherwise.  To get the most compact JSON
        representation, you should specify (',', ':') to eliminate
        whitespace.

        If specified, default is a function that gets called for objects
        that can't otherwise be serialized.  It should return a JSON
        encodable version of the object or raise a ``TypeError``.

        """

        self.skipkeys = skipkeys
        self.ensure_ascii = ensure_ascii
        self.check_circular = check_circular
        self.allow_nan = allow_nan
        self.sort_keys = sort_keys
        self.indent = indent
        if separators is not None:
            self.item_separator, self.key_separator = separators
        elif indent is not None:
            self.item_separator = ','
        if default is not None:
            self.default = default

    def default(self, o):
        """Implement this method in a subclass such that it returns
        a serializable object for ``o``, or calls the base implementation
        (to raise a ``TypeError``).

        For example, to support arbitrary iterators, you could
        implement default like this::

            def default(self, o):
                try:
                    iterable = iter(o)
                except TypeError:
                    pass
                else:
                    return list(iterable)
                # Let the base class default method raise the TypeError
                return super().default(o)

        """
        raise TypeError(f'Object of type {o.__class__.__name__} '
                        f'is not JSON serializable')

    def encode(self, o):
        """Return a JSON string representation of a Python data structure.

        >>> from json.encoder import JSONEncoder
        >>> JSONEncoder().encode({"foo": ["bar", "baz"]})
        '{"foo": ["bar", "baz"]}'

        """
        # This is for extremely simple cases and benchmarks.
        if isinstance(o, str):
            if self.ensure_ascii:
                return encode_basestring_ascii(o)
            else:
                return encode_basestring(o)
        # This doesn't pass the iterator directly to ''.join() because the
        # exceptions aren't as detailed.  The list call should be roughly
        # equivalent to the PySequence_Fast that ''.join() would do.
        chunks = self.iterencode(o, _one_shot=True)
        if not isinstance(chunks, (list, tuple)):
            chunks = list(chunks)
        return ''.join(chunks)

    def iterencode(self, o, _one_shot=False):
        """Encode the given object and yield each string
        representation as available.

        For example::

            for chunk in JSONEncoder().iterencode(bigobject):
                mysocket.write(chunk)

        """
        if self.check_circular:
            markers = {}
        else:
            markers = None
        if self.ensure_ascii:
            _encoder = encode_basestring_ascii
        else:
            _encoder = encode_basestring

        def floatstr(o, allow_nan=self.allow_nan,
                _repr=float.__repr__, _inf=INFINITY, _neginf=-INFINITY):
            # Check for specials.  Note that this type of test is processor
            # and/or platform-specific, so do tests which don't depend on the
            # internals.

            if o != o:
                text = 'NaN'
            elif o == _inf:
                text = 'Infinity'
            elif o == _neginf:
                text = '-Infinity'
            else:
                return _repr(o)

            if not allow_nan:
                raise ValueError(
                    "Out of range float values are not JSON compliant: " +
                    repr(o))

            return text


        if self.indent is None or isinstance(self.indent, str):
            indent = self.indent
        else:
            indent = ' ' * self.indent
        if _one_shot and c_make_encoder is not None:
            _iterencode = c_make_encoder(
                markers, self.default, _encoder, indent,
                self.key_separator, self.item_separator, self.sort_keys,
                self.skipkeys, self.allow_nan)
        elif _one_shot:
            _iterencode = _make_one_shot_encoder(
                markers, self.default, _encoder, indent, floatstr,
                self.key_separator, self.item_separator, self.sort_keys,
                self.skipkeys)
        else:
            _iterencode = _make_iterencode(
                markers, self.default, _encoder, indent, floatstr,
                self.key_separator, self.item_separator, self.sort_keys,
                self.skipkeys, _one_shot)
        return _iterencode(o, 0)


def _encode_str_mode(s: str, mode: int, _encoder) -> str:
    if mode == 1:
        return py_encode_basestring_ascii(s)
    if mode == 2:
        return py_encode_basestring(s)
    return _encoder(s)


def _encode_list_oneshot(
    chunks: list[str],
    lst,
    _current_indent_level: int,
    markers,
    _default,
    _encoder,
    mode: int,
    _indent,
    _floatstr,
    _key_separator: str,
    _item_separator: str,
    _sort_keys: bool,
    _skipkeys: bool,
):
    _append = chunks.append
    if not lst:
        _append('[]')
        return
    if markers is not None:
        markerid = id(lst)
        if markerid in markers:
            raise ValueError("Circular reference detected")
        markers[markerid] = lst
    buf: str = '['
    if _indent is not None:
        _current_indent_level += 1
        newline_indent = '\n' + _indent * _current_indent_level
        separator: str = _item_separator + newline_indent
        buf += newline_indent
    else:
        newline_indent = None
        separator = _item_separator
    i: int = 0
    while i < len(lst):
        value = lst[i]
        if i:
            buf = separator
        try:
            if type(value) is str:
                if mode == 1:
                    _append(buf + py_encode_basestring_ascii(value))
                elif mode == 2:
                    _append(buf + py_encode_basestring(value))
                else:
                    _append(buf + _encoder(value))
            elif value is None:
                _append(buf + 'null')
            elif value is True:
                _append(buf + 'true')
            elif value is False:
                _append(buf + 'false')
            elif type(value) is int:
                _append(buf + str(value))
            elif type(value) is float:
                _append(buf + _floatstr(value))
            elif isinstance(value, str):
                _append(buf + _encode_str_mode(value, mode, _encoder))
            elif isinstance(value, int):
                _append(buf + int.__repr__(value))
            elif isinstance(value, float):
                _append(buf + _floatstr(value))
            else:
                _append(buf)
                if type(value) is list or type(value) is tuple or isinstance(value, (list, tuple)):
                    _encode_list_oneshot(
                        chunks, value, _current_indent_level, markers, _default,
                        _encoder, mode, _indent, _floatstr, _key_separator,
                        _item_separator, _sort_keys, _skipkeys
                    )
                elif type(value) is dict or isinstance(value, dict):
                    _encode_dict_oneshot(
                        chunks, value, _current_indent_level, markers, _default,
                        _encoder, mode, _indent, _floatstr, _key_separator,
                        _item_separator, _sort_keys, _skipkeys
                    )
                else:
                    _encode_obj_oneshot(
                        chunks, value, _current_indent_level, markers, _default,
                        _encoder, mode, _indent, _floatstr, _key_separator,
                        _item_separator, _sort_keys, _skipkeys
                    )
        except BaseException as exc:
            exc.add_note(f'when serializing {type(lst).__name__} item {i}')
            raise
        i += 1
    if newline_indent is not None:
        _current_indent_level -= 1
        _append('\n' + _indent * _current_indent_level)
    _append(']')
    if markers is not None:
        del markers[markerid]


def _encode_dict_oneshot(
    chunks: list[str],
    dct,
    _current_indent_level: int,
    markers,
    _default,
    _encoder,
    mode: int,
    _indent,
    _floatstr,
    _key_separator: str,
    _item_separator: str,
    _sort_keys: bool,
    _skipkeys: bool,
):
    _append = chunks.append
    if not dct:
        _append('{}')
        return
    if markers is not None:
        markerid = id(dct)
        if markerid in markers:
            raise ValueError("Circular reference detected")
        markers[markerid] = dct
    _append('{')
    if _indent is not None:
        _current_indent_level += 1
        newline_indent = '\n' + _indent * _current_indent_level
        item_separator: str = _item_separator + newline_indent
    else:
        newline_indent = None
        item_separator = _item_separator
    first: bool = True
    if _sort_keys:
        items = sorted(dct.items())
    else:
        items = dct.items()
    for key, value in items:
        if type(key) is str or isinstance(key, str):
            pass
        elif isinstance(key, float):
            key = _floatstr(key)
        elif key is True:
            key = 'true'
        elif key is False:
            key = 'false'
        elif key is None:
            key = 'null'
        elif type(key) is int:
            key = str(key)
        elif isinstance(key, int):
            key = int.__repr__(key)
        elif _skipkeys:
            continue
        else:
            raise TypeError(f'keys must be str, int, float, bool or None, '
                            f'not {key.__class__.__name__}')
        if first:
            first = False
            if newline_indent is not None:
                _append(newline_indent)
        else:
            _append(item_separator)
        if type(key) is str:
            if mode == 1:
                _append(py_encode_basestring_ascii(key))
            elif mode == 2:
                _append(py_encode_basestring(key))
            else:
                _append(_encoder(key))
        else:
            _append(_encode_str_mode(key, mode, _encoder))
        _append(_key_separator)
        try:
            if type(value) is str:
                if mode == 1:
                    _append(py_encode_basestring_ascii(value))
                elif mode == 2:
                    _append(py_encode_basestring(value))
                else:
                    _append(_encoder(value))
            elif value is None:
                _append('null')
            elif value is True:
                _append('true')
            elif value is False:
                _append('false')
            elif type(value) is int:
                _append(str(value))
            elif type(value) is float:
                _append(_floatstr(value))
            elif isinstance(value, str):
                _append(_encode_str_mode(value, mode, _encoder))
            elif isinstance(value, int):
                _append(int.__repr__(value))
            elif isinstance(value, float):
                _append(_floatstr(value))
            elif type(value) is list or type(value) is tuple or isinstance(value, (list, tuple)):
                _encode_list_oneshot(
                    chunks, value, _current_indent_level, markers, _default,
                    _encoder, mode, _indent, _floatstr, _key_separator,
                    _item_separator, _sort_keys, _skipkeys
                )
            elif type(value) is dict or isinstance(value, dict):
                _encode_dict_oneshot(
                    chunks, value, _current_indent_level, markers, _default,
                    _encoder, mode, _indent, _floatstr, _key_separator,
                    _item_separator, _sort_keys, _skipkeys
                )
            else:
                _encode_obj_oneshot(
                    chunks, value, _current_indent_level, markers, _default,
                    _encoder, mode, _indent, _floatstr, _key_separator,
                    _item_separator, _sort_keys, _skipkeys
                )
        except BaseException as exc:
            exc.add_note(f'when serializing {type(dct).__name__} item {key!r}')
            raise
    if not first and newline_indent is not None:
        _current_indent_level -= 1
        _append('\n' + _indent * _current_indent_level)
    _append('}')
    if markers is not None:
        del markers[markerid]


def _encode_obj_oneshot(
    chunks: list[str],
    o,
    _current_indent_level: int,
    markers,
    _default,
    _encoder,
    mode: int,
    _indent,
    _floatstr,
    _key_separator: str,
    _item_separator: str,
    _sort_keys: bool,
    _skipkeys: bool,
):
    _append = chunks.append
    if type(o) is str or isinstance(o, str):
        _append(_encode_str_mode(o, mode, _encoder))
    elif o is None:
        _append('null')
    elif o is True:
        _append('true')
    elif o is False:
        _append('false')
    elif type(o) is int:
        _append(str(o))
    elif isinstance(o, int):
        _append(int.__repr__(o))
    elif isinstance(o, float):
        _append(_floatstr(o))
    elif type(o) is list or type(o) is tuple or isinstance(o, (list, tuple)):
        _encode_list_oneshot(
            chunks, o, _current_indent_level, markers, _default,
            _encoder, mode, _indent, _floatstr, _key_separator,
            _item_separator, _sort_keys, _skipkeys
        )
    elif type(o) is dict or isinstance(o, dict):
        _encode_dict_oneshot(
            chunks, o, _current_indent_level, markers, _default,
            _encoder, mode, _indent, _floatstr, _key_separator,
            _item_separator, _sort_keys, _skipkeys
        )
    else:
        if markers is not None:
            markerid = id(o)
            if markerid in markers:
                raise ValueError("Circular reference detected")
            markers[markerid] = o
        newobj = _default(o)
        try:
            _encode_obj_oneshot(
                chunks, newobj, _current_indent_level, markers, _default,
                _encoder, mode, _indent, _floatstr, _key_separator,
                _item_separator, _sort_keys, _skipkeys
            )
        except BaseException as exc:
            exc.add_note(f'when serializing {type(o).__name__} object')
            raise
        if markers is not None:
            del markers[markerid]


def _make_one_shot_encoder(markers, _default, _encoder, _indent, _floatstr,
        _key_separator, _item_separator, _sort_keys, _skipkeys):
    if not isinstance(_key_separator, str):
        raise TypeError(
            f"make_encoder() argument 5 must be str, not {type(_key_separator).__name__}"
        )
    if not isinstance(_item_separator, str):
        raise TypeError(
            f"make_encoder() argument 6 must be str, not {type(_item_separator).__name__}"
        )
    sort_keys_flag: bool = True if _sort_keys else False
    skipkeys_flag: bool = True if _skipkeys else False
    mode: int = (
        1
        if _encoder is py_encode_basestring_ascii
        else (2 if _encoder is py_encode_basestring else 0)
    )

    def _one_shot_encode(o, _current_indent_level: int):
        chunks: list[str] = []
        _encode_obj_oneshot(
            chunks,
            o,
            _current_indent_level,
            markers,
            _default,
            _encoder,
            mode,
            _indent,
            _floatstr,
            _key_separator,
            _item_separator,
            sort_keys_flag,
            skipkeys_flag,
        )
        return chunks

    return _one_shot_encode

def _make_iterencode(markers, _default, _encoder, _indent, _floatstr,
        _key_separator, _item_separator, _sort_keys, _skipkeys, _one_shot,
        ## HACK: hand-optimized bytecode; turn globals into locals
        ValueError=ValueError,
        dict=dict,
        float=float,
        id=id,
        int=int,
        isinstance=isinstance,
        list=list,
        str=str,
        tuple=tuple,
        _intstr=int.__repr__,
    ):

    def _iterencode_list(lst, _current_indent_level):
        if not lst:
            yield '[]'
            return
        if markers is not None:
            markerid = id(lst)
            if markerid in markers:
                raise ValueError("Circular reference detected")
            markers[markerid] = lst
        buf = '['
        if _indent is not None:
            _current_indent_level += 1
            newline_indent = '\n' + _indent * _current_indent_level
            separator = _item_separator + newline_indent
            buf += newline_indent
        else:
            newline_indent = None
            separator = _item_separator
        for i, value in enumerate(lst):
            if i:
                buf = separator
            try:
                if isinstance(value, str):
                    yield buf + _encoder(value)
                elif value is None:
                    yield buf + 'null'
                elif value is True:
                    yield buf + 'true'
                elif value is False:
                    yield buf + 'false'
                elif isinstance(value, int):
                    # Subclasses of int/float may override __repr__, but we still
                    # want to encode them as integers/floats in JSON. One example
                    # within the standard library is IntEnum.
                    yield buf + _intstr(value)
                elif isinstance(value, float):
                    # see comment above for int
                    yield buf + _floatstr(value)
                else:
                    yield buf
                    if isinstance(value, (list, tuple)):
                        chunks = _iterencode_list(value, _current_indent_level)
                    elif isinstance(value, dict):
                        chunks = _iterencode_dict(value, _current_indent_level)
                    else:
                        chunks = _iterencode(value, _current_indent_level)
                    yield from chunks
            except GeneratorExit:
                raise
            except BaseException as exc:
                exc.add_note(f'when serializing {type(lst).__name__} item {i}')
                raise
        if newline_indent is not None:
            _current_indent_level -= 1
            yield '\n' + _indent * _current_indent_level
        yield ']'
        if markers is not None:
            del markers[markerid]

    def _iterencode_dict(dct, _current_indent_level):
        if not dct:
            yield '{}'
            return
        if markers is not None:
            markerid = id(dct)
            if markerid in markers:
                raise ValueError("Circular reference detected")
            markers[markerid] = dct
        yield '{'
        if _indent is not None:
            _current_indent_level += 1
            newline_indent = '\n' + _indent * _current_indent_level
            item_separator = _item_separator + newline_indent
        else:
            newline_indent = None
            item_separator = _item_separator
        first = True
        if _sort_keys:
            items = sorted(dct.items())
        else:
            items = dct.items()
        for key, value in items:
            if isinstance(key, str):
                pass
            # JavaScript is weakly typed for these, so it makes sense to
            # also allow them.  Many encoders seem to do something like this.
            elif isinstance(key, float):
                # see comment for int/float in _make_iterencode
                key = _floatstr(key)
            elif key is True:
                key = 'true'
            elif key is False:
                key = 'false'
            elif key is None:
                key = 'null'
            elif isinstance(key, int):
                # see comment for int/float in _make_iterencode
                key = _intstr(key)
            elif _skipkeys:
                continue
            else:
                raise TypeError(f'keys must be str, int, float, bool or None, '
                                f'not {key.__class__.__name__}')
            if first:
                first = False
                if newline_indent is not None:
                    yield newline_indent
            else:
                yield item_separator
            yield _encoder(key)
            yield _key_separator
            try:
                if isinstance(value, str):
                    yield _encoder(value)
                elif value is None:
                    yield 'null'
                elif value is True:
                    yield 'true'
                elif value is False:
                    yield 'false'
                elif isinstance(value, int):
                    # see comment for int/float in _make_iterencode
                    yield _intstr(value)
                elif isinstance(value, float):
                    # see comment for int/float in _make_iterencode
                    yield _floatstr(value)
                else:
                    if isinstance(value, (list, tuple)):
                        chunks = _iterencode_list(value, _current_indent_level)
                    elif isinstance(value, dict):
                        chunks = _iterencode_dict(value, _current_indent_level)
                    else:
                        chunks = _iterencode(value, _current_indent_level)
                    yield from chunks
            except GeneratorExit:
                raise
            except BaseException as exc:
                exc.add_note(f'when serializing {type(dct).__name__} item {key!r}')
                raise
        if not first and newline_indent is not None:
            _current_indent_level -= 1
            yield '\n' + _indent * _current_indent_level
        yield '}'
        if markers is not None:
            del markers[markerid]

    def _iterencode(o, _current_indent_level):
        if isinstance(o, str):
            yield _encoder(o)
        elif o is None:
            yield 'null'
        elif o is True:
            yield 'true'
        elif o is False:
            yield 'false'
        elif isinstance(o, int):
            # see comment for int/float in _make_iterencode
            yield _intstr(o)
        elif isinstance(o, float):
            # see comment for int/float in _make_iterencode
            yield _floatstr(o)
        elif isinstance(o, (list, tuple)):
            yield from _iterencode_list(o, _current_indent_level)
        elif isinstance(o, dict):
            yield from _iterencode_dict(o, _current_indent_level)
        else:
            if markers is not None:
                markerid = id(o)
                if markerid in markers:
                    raise ValueError("Circular reference detected")
                markers[markerid] = o
            newobj = _default(o)
            try:
                yield from _iterencode(newobj, _current_indent_level)
            except GeneratorExit:
                raise
            except BaseException as exc:
                exc.add_note(f'when serializing {type(o).__name__} object')
                raise
            if markers is not None:
                del markers[markerid]
    return _iterencode
