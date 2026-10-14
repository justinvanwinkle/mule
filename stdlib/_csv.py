"""CSV dialects and streaming text protocols, compiled by Mule."""

__mule_builtin_implementation__ = True


QUOTE_MINIMAL = 0
QUOTE_ALL = 1
QUOTE_NONNUMERIC = 2
QUOTE_NONE = 3
QUOTE_STRINGS = 4
QUOTE_NOTNULL = 5
__version__ = "1.0"


class Error(Exception):
    pass


_registry = {}
_field_limit = 131072
_missing = object()
_defaults = {
    "delimiter": ",",
    "quotechar": '"',
    "escapechar": None,
    "doublequote": True,
    "skipinitialspace": False,
    "lineterminator": "\r\n",
    "quoting": QUOTE_MINIMAL,
    "strict": False,
}


def _character(name, value, nullable):
    if nullable and value is None:
        return
    expected = "a unicode character or None" if nullable else "a unicode character"
    if not isinstance(value, str):
        detail = type(value).__name__
    elif len(value) != 1:
        detail = "a string of length " + str(len(value))
    else:
        if value in "\r\n":
            raise ValueError("bad " + name + " value")
        return
    raise TypeError('"' + name + '" must be ' + expected + ", not " + detail)


class Dialect:
    __slots__ = ("_settings",)

    def __init__(self, dialect=None, **fmtparams):
        if isinstance(dialect, str):
            dialect = get_dialect(dialect)
        settings = {}
        quoting_supplied = "quoting" in fmtparams
        for name, default in _defaults.items():
            value = getattr(dialect, name, _missing)
            settings[name] = default if value is _missing else value
            if name == "quoting" and value is not _missing:
                quoting_supplied = True
        for name, value in fmtparams.items():
            if name not in _defaults:
                raise TypeError(
                    "this function got an unexpected keyword argument '" + name + "'"
                )
            settings[name] = value
        if settings["quotechar"] is None and not quoting_supplied:
            settings["quoting"] = QUOTE_NONE
        _character("delimiter", settings["delimiter"], False)
        _character("quotechar", settings["quotechar"], True)
        _character("escapechar", settings["escapechar"], True)
        quoting = settings["quoting"]
        if isinstance(quoting, bool) or not isinstance(quoting, int):
            raise TypeError(
                '"quoting" must be an integer, not ' + type(quoting).__name__
            )
        if quoting not in (0, 1, 2, 3, 4, 5):
            raise TypeError('bad "quoting" value')
        if settings["quotechar"] is None and quoting != QUOTE_NONE:
            raise TypeError("quotechar must be set if quoting enabled")
        if not isinstance(settings["lineterminator"], str):
            raise TypeError(
                '"lineterminator" must be a string, not '
                + type(settings["lineterminator"]).__name__
            )
        for left, right in (
            ("delimiter", "quotechar"),
            ("delimiter", "escapechar"),
            ("escapechar", "quotechar"),
        ):
            if settings[left] is not None and settings[left] == settings[right]:
                raise ValueError("bad " + left + " or " + right + " value")
        for name in ("doublequote", "skipinitialspace", "strict"):
            settings[name] = bool(settings[name])
        self._settings = settings

    @property
    def delimiter(self):
        return self._settings["delimiter"]

    @property
    def quotechar(self):
        return self._settings["quotechar"]

    @property
    def escapechar(self):
        return self._settings["escapechar"]

    @property
    def doublequote(self):
        return self._settings["doublequote"]

    @property
    def skipinitialspace(self):
        return self._settings["skipinitialspace"]

    @property
    def lineterminator(self):
        return self._settings["lineterminator"]

    @property
    def quoting(self):
        return self._settings["quoting"]

    @property
    def strict(self):
        return self._settings["strict"]


def _dialect(dialect, fmtparams):
    if isinstance(dialect, str):
        dialect = get_dialect(dialect)
    if isinstance(dialect, Dialect) and not fmtparams:
        return dialect
    return Dialect(dialect, **fmtparams)


def register_dialect(name, dialect=None, **fmtparams):
    if not isinstance(name, str):
        raise TypeError("dialect name must be a string")
    _registry[name] = _dialect(dialect, fmtparams)


def get_dialect(name):
    try:
        return _registry[name]
    except KeyError:
        raise Error("unknown dialect") from None


def unregister_dialect(name):
    try:
        del _registry[name]
    except KeyError:
        raise Error("unknown dialect") from None


def list_dialects():
    return list(_registry)


def field_size_limit(new_limit=_missing):
    global _field_limit
    previous = _field_limit
    if new_limit is not _missing:
        if isinstance(new_limit, bool) or not isinstance(new_limit, int):
            raise TypeError("limit must be an integer")
        new_limit = int.__index__(new_limit)
        if new_limit < -(1 << 63) or new_limit >= (1 << 63):
            raise OverflowError("Python int too large to convert to C ssize_t")
        _field_limit = new_limit
    return previous


class _StartRecord:
    def feed(self, reader, char):
        if char is None:
            reader._complete = True
        elif char in "\r\n":
            reader._state = _EAT_NEWLINE
        else:
            reader._state = _START_FIELD
            _START_FIELD.feed(reader, char)


class _StartField:
    def feed(self, reader, char):
        dialect = reader.dialect
        if char is None:
            reader._save()
            reader._complete = True
        elif char in "\r\n":
            reader._save()
            reader._state = _EAT_NEWLINE
        elif char == dialect.quotechar and dialect.quoting != QUOTE_NONE:
            reader._quoted = True
            reader._state = _QUOTED
        elif char == dialect.escapechar:
            reader._state = _ESCAPE_FIELD
        elif char == " " and dialect.skipinitialspace:
            pass
        elif char == dialect.delimiter:
            reader._save()
        else:
            reader._append(char)
            reader._state = _FIELD


class _Field:
    def feed(self, reader, char):
        if char is None:
            reader._save()
            reader._complete = True
        elif char in "\r\n":
            reader._save()
            reader._state = _EAT_NEWLINE
        elif char == reader.dialect.escapechar:
            reader._state = _ESCAPE_FIELD
        elif char == reader.dialect.delimiter:
            reader._save()
            reader._state = _START_FIELD
        else:
            reader._append(char)


class _Quoted:
    def feed(self, reader, char):
        if char is None:
            return
        if char == reader.dialect.escapechar:
            reader._state = _ESCAPE_QUOTED
        elif char == reader.dialect.quotechar:
            reader._state = _AFTER_QUOTE if reader.dialect.doublequote else _FIELD
        else:
            reader._append(char)


class _EscapeField:
    def feed(self, reader, char):
        reader._append("\n" if char is None else char)
        reader._state = (
            _AFTER_ESCAPED_NEWLINE if char is not None and char in "\r\n" else _FIELD
        )


class _EscapeQuoted:
    def feed(self, reader, char):
        reader._append("\n" if char is None else char)
        reader._state = _QUOTED


class _AfterEscapedNewline:
    def feed(self, reader, char):
        if char is not None:
            reader._state = _FIELD
            _FIELD.feed(reader, char)


class _AfterQuote:
    def feed(self, reader, char):
        if char is not None and char == reader.dialect.quotechar:
            reader._append(char)
            reader._state = _QUOTED
        elif char is None or char in "\r\n" or char == reader.dialect.delimiter:
            _FIELD.feed(reader, char)
        elif reader.dialect.strict:
            raise Error(
                "'"
                + reader.dialect.delimiter
                + "' expected after '"
                + reader.dialect.quotechar
                + "'"
            )
        else:
            reader._append(char)
            reader._state = _FIELD


class _EatNewline:
    def feed(self, reader, char):
        if char is None:
            reader._complete = True
        elif char not in "\r\n":
            raise Error(
                "new-line character seen in unquoted field - do you need to open the file with newline=''?"
            )


_START_RECORD = _StartRecord()
_START_FIELD = _StartField()
_FIELD = _Field()
_QUOTED = _Quoted()
_ESCAPE_FIELD = _EscapeField()
_ESCAPE_QUOTED = _EscapeQuoted()
_AFTER_ESCAPED_NEWLINE = _AfterEscapedNewline()
_AFTER_QUOTE = _AfterQuote()
_EAT_NEWLINE = _EatNewline()


class _Reader:
    def __init__(self, iterable, dialect):
        self._input = iter(iterable)
        self._dialect = dialect
        self._line_num = 0

    @property
    def dialect(self):
        return self._dialect

    @property
    def line_num(self):
        return self._line_num

    def __iter__(self):
        return self

    def _append(self, char):
        if len(self._chars) >= _field_limit:
            raise Error("field larger than field limit (" + str(_field_limit) + ")")
        self._chars.append(char)

    def _save(self):
        value = "".join(self._chars)
        if not self._quoted:
            if not value and self.dialect.quoting in (QUOTE_STRINGS, QUOTE_NOTNULL):
                value = None
            elif value and self.dialect.quoting in (QUOTE_NONNUMERIC, QUOTE_STRINGS):
                value = float(value)
        self._fields.append(value)
        self._chars = []
        self._quoted = False

    def __next__(self):
        self._state = _START_RECORD
        self._fields = []
        self._chars = []
        self._quoted = False
        self._complete = False
        while not self._complete:
            try:
                line = next(self._input)
            except StopIteration:
                if self._state is _START_RECORD:
                    raise
                if self.dialect.strict:
                    raise Error("unexpected end of data") from None
                self._save()
                return self._fields
            if not isinstance(line, str):
                raise Error(
                    "iterator should return strings, not "
                    + type(line).__name__
                    + " (the file should be opened in text mode)"
                )
            self._line_num += 1
            for char in line:
                self._state.feed(self, char)
            self._state.feed(self, None)
        return self._fields


def reader(iterable, dialect=None, **fmtparams):
    return _Reader(iterable, _dialect(dialect, fmtparams))


def _numeric(value):
    owner = type(value)
    return (
        hasattr(owner, "__float__")
        or hasattr(owner, "__index__")
        or hasattr(owner, "__int__")
    )


class _Writer:
    def __init__(self, file, dialect):
        write = getattr(file, "write", None)
        if not callable(write):
            raise TypeError('argument 1 must have a "write" method')
        self._write = write
        self._dialect = dialect

    @property
    def dialect(self):
        return self._dialect

    def _field(self, value, force=False):
        dialect = self.dialect
        mode = dialect.quoting
        quoted = (
            mode == QUOTE_ALL
            or mode == QUOTE_NONNUMERIC
            and not _numeric(value)
            or mode == QUOTE_STRINGS
            and isinstance(value, str)
            or mode == QUOTE_NOTNULL
            and value is not None
        )
        text = "" if value is None else str(value)
        if force or not text and dialect.delimiter == " " and dialect.skipinitialspace:
            if (
                mode == QUOTE_NONE
                or mode in (QUOTE_STRINGS, QUOTE_NOTNULL)
                and value is None
            ):
                if force:
                    raise Error("single empty field record must be quoted")
                raise Error(
                    "empty field must be quoted if delimiter is a space and skipinitialspace is true"
                )
            quoted = True
        parts = []
        for char in text:
            special = (
                char in "\r\n"
                or char in dialect.lineterminator
                or char == dialect.delimiter
                or char == dialect.quotechar
                or char == dialect.escapechar
            )
            if special:
                escape = mode == QUOTE_NONE or char == dialect.escapechar
                if char == dialect.quotechar and mode != QUOTE_NONE:
                    if dialect.doublequote:
                        parts.append(char)
                    else:
                        escape = True
                if escape:
                    if dialect.escapechar is None:
                        raise Error("need to escape, but no escapechar set")
                    parts.append(dialect.escapechar)
                elif mode != QUOTE_NONE:
                    quoted = True
            parts.append(char)
        result = "".join(parts)
        return dialect.quotechar + result + dialect.quotechar if quoted else result

    def writerow(self, row):
        try:
            iterator = iter(row)
        except TypeError:
            raise Error("iterable expected, not " + type(row).__name__) from None
        fields = []
        values = []
        for value in iterator:
            values.append(value)
            fields.append(self._field(value))
        if len(fields) == 1 and fields[0] == "":
            fields[0] = self._field(values[0], True)
        return self._write(
            self.dialect.delimiter.join(fields) + self.dialect.lineterminator
        )

    def writerows(self, rows):
        for row in rows:
            self.writerow(row)


def writer(file, dialect=None, **fmtparams):
    return _Writer(file, _dialect(dialect, fmtparams))
