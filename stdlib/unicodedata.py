"""Unicode 16.0.0 properties and normalization, compiled by Mule.

Property tables and normalization mappings are generated at build time.
Algorithms follow SBCL's target-unicode.lisp and unicode-norm.lisp: canonical
ordering, blocked primary composition, and algorithmic Hangul processing.
"""

from _str import TextStorage
from _mule_native import byte_copy as _byte_copy
from _mule_native import byte_length as _byte_length
from _unicodedata_data import (
    category_code,
    combining_code,
    composition_code,
    decomposition_code,
    decomposition_compatibility,
    decomposition_length,
    decomposition_record,
    unidata_version,
    width_code,
)


def _character(value):
    if not isinstance(value, str):
        raise TypeError("argument must be a unicode character")
    return ord(value)


def category(chr, /):
    return category_code(_character(chr))


def east_asian_width(chr, /):
    return width_code(_character(chr))


def combining(chr, /):
    return combining_code(_character(chr))


def lookup(name, /):
    from _unicode_names import ALGORITHMIC_RANGES, lookup_name

    if isinstance(name, str):
        key = TextStorage.encode(name, "utf-8")
    elif isinstance(name, bytes):
        key = _byte_copy(name, 0, _byte_length(name))
    else:
        kind = type(name).__name__
        if isinstance(name, (bytearray, memoryview)):
            raise TypeError(
                "lookup() argument must be read-only bytes-like object, not " + kind
            )
        raise TypeError("a bytes-like object is required, not '" + kind + "'")
    original = key
    # Unicode character names use ASCII case-insensitive matching, not Unicode
    # case folding (dotless i, for example, must not alias ASCII I).
    key = bytes(c - 32 if 97 <= c <= 122 else c for c in key)
    value = lookup_name(key)
    if value is not None:
        return value
    for prefix, ranges in ALGORITHMIC_RANGES.items():
        if key.startswith(prefix):
            suffix = key[len(prefix) :]
            if not suffix or any(c not in b"0123456789ABCDEF" for c in suffix):
                break
            code = int(suffix, 16)
            if suffix != format(code, "X").encode("ascii"):
                break
            for first, last in ranges:
                if first <= code <= last:
                    return chr(code)
            break
    display = original.split(b"\x00", 1)[0].decode("utf-8", "replace")
    raise KeyError("undefined character name '" + display + "'")


def _ordered_append(codes, cp):
    """Stable insertion into this starter's combining sequence."""
    ccc = combining_code(cp)
    codes.append(cp)
    if ccc == 0:
        return
    at = len(codes) - 1
    while at > 0 and combining_code(codes[at - 1]) > ccc:
        codes[at] = codes[at - 1]
        at -= 1
    codes[at] = cp


def _decompose(codes, cp, compatibility):
    if 0xAC00 <= cp <= 0xD7A3:
        index = cp - 0xAC00
        _ordered_append(codes, 0x1100 + index // 588)
        _ordered_append(codes, 0x1161 + (index % 588) // 28)
        trailing = index % 28
        if trailing:
            _ordered_append(codes, 0x11A7 + trailing)
        return
    record = decomposition_record(cp)
    if record >= 0 and (compatibility or not decomposition_compatibility(record)):
        for index in range(decomposition_length(record)):
            _decompose(codes, decomposition_code(record, index), compatibility)
    else:
        _ordered_append(codes, cp)


def _primary_composition(first, second):
    if 0x1100 <= first <= 0x1112 and 0x1161 <= second <= 0x1175:
        return 0xAC00 + (first - 0x1100) * 588 + (second - 0x1161) * 28
    if (
        0xAC00 <= first <= 0xD7A3
        and (first - 0xAC00) % 28 == 0
        and 0x11A8 <= second <= 0x11C2
    ):
        return first + second - 0x11A7
    return composition_code(first, second)


def _compose(codes):
    result = []
    starter = -1
    previous_class = 0
    for cp in codes:
        ccc = combining_code(cp)
        composed = -1
        if starter >= 0 and (previous_class == 0 or previous_class < ccc):
            composed = _primary_composition(result[starter], cp)
        if composed >= 0:
            result[starter] = composed
        else:
            if ccc == 0:
                starter = len(result)
            result.append(cp)
            previous_class = ccc
    return result


def normalize(form, unistr, /):
    if not isinstance(form, str):
        raise TypeError("normalize() argument 1 must be str")
    if not isinstance(unistr, str):
        raise TypeError("normalize() argument 2 must be str")
    # UCD reads physical text, ignoring overrides on str subclasses. CPython
    # validates both argument types before its empty-text fast path.
    size = TextStorage.__len__(unistr)
    if size == 0:
        return ""
    canonical_composed = TextStorage.__eq__(form, "NFC")
    compatibility_composed = TextStorage.__eq__(form, "NFKC")
    compatibility = compatibility_composed or TextStorage.__eq__(form, "NFKD")
    if not (canonical_composed or compatibility or TextStorage.__eq__(form, "NFD")):
        raise ValueError("invalid normalization form")
    codes = []
    for index in range(size):
        _decompose(codes, ord(TextStorage.__getitem__(unistr, index)), compatibility)
    if canonical_composed or compatibility_composed:
        codes = _compose(codes)
    return "".join(chr(cp) for cp in codes)
