#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.15"
# dependencies = []
# ///
"""Generate Unicode name lookup data; no host APIs are used at execution.

Canonical names come from the running CPython's Unicode database. Formal
aliases and named sequences come from the adjacent, versioned Unicode
Consortium data files, whose headers retain their copyright and license
references and whose version must match the running Python's.
"""

import unicodedata
from pathlib import Path

from tools.generate_unicode_data import (
    ascii_ranges,
    render_bytes,
    require_matching_unicode_version,
)


def render_names():
    require_matching_unicode_version()
    names = {}
    algorithmic = {}
    for cp in range(0x110000):
        name = unicodedata.name(chr(cp), "")
        if not name:
            continue
        prefix, _, suffix = name.rpartition("-")
        if suffix == f"{cp:X}":
            algorithmic.setdefault(prefix + "-", []).append(cp)
        else:
            names[name] = chr(cp)
    # Tangut names are accepted by lookup but not exposed by name in Python.
    tangut = []
    for cp in range(0x17000, 0x18E00):
        try:
            value = unicodedata.lookup(f"TANGUT IDEOGRAPH-{cp:X}")
        except KeyError:
            continue
        assert value == chr(cp)
        tangut.append(cp)
    algorithmic["TANGUT IDEOGRAPH-"] = tangut
    directory = Path(__file__).with_name("unicode")
    for filename in ("NameAliases.txt", "NamedSequences.txt"):
        for line in (directory / filename).read_text().splitlines():
            line = line.partition("#")[0].strip()
            if not line:
                continue
            fields = line.split(";")
            if filename == "NameAliases.txt":
                name, value = fields[1], chr(int(fields[0], 16))
            else:
                name = fields[0]
                value = "".join(chr(int(cp, 16)) for cp in fields[1].split())
            assert unicodedata.lookup(name) == value
            names[name] = value
    keys = bytearray()
    values = bytearray()
    records = bytearray()
    for name, value in sorted(names.items()):
        encoded = name.encode("ascii")
        records += len(keys).to_bytes(3, "little") + bytes((len(encoded),))
        records += len(values).to_bytes(3, "little") + bytes((len(value),))
        keys += encoded
        for character in value:
            values += ord(character).to_bytes(3, "little")
    ranges = {
        prefix.encode("ascii"): tuple(map(tuple, ascii_ranges(codes)))
        for prefix, codes in algorithmic.items()
    }
    version = unicodedata.unidata_version
    source = f'''"""Generated Unicode {version} name data. Do not edit.

Regenerate with: uv run tools/generate_unicode_names.py
Aliases and sequences originate in https://www.unicode.org/Public/{version}/ucd/.
Sorted records: name offset u24, name length u8, value offset u24,
value length u8. Values are little-endian u24 codepoints.
"""

'''
    source += f"ALGORITHMIC_RANGES = {ranges!r}\n\n"
    for name, data in (("_KEYS", keys), ("_VALUES", values), ("_RECORDS", records)):
        source += render_bytes(name, bytes(data)) + "\n"
    source += """
def _u24(data, at):
    return data[at] | (data[at + 1] << 8) | (data[at + 2] << 16)


def lookup_name(key):
    low = 0
    high = len(_RECORDS) // 8
    while low < high:
        middle = (low + high) // 2
        at = middle * 8
        start = _u24(_RECORDS, at)
        candidate = _KEYS[start:start + _RECORDS[at + 3]]
        if candidate == key:
            start = _u24(_RECORDS, at + 4)
            return "".join(chr(_u24(_VALUES, start + index * 3))
                           for index in range(_RECORDS[at + 7]))
        if candidate < key:
            low = middle + 1
        else:
            high = middle
    return None
"""
    return source


def main():
    target = Path(__file__).resolve().parents[1] / "stdlib" / "_unicode_names.py"
    source = render_names()
    target.write_text(source)
    print(f"wrote {len(source.encode())} bytes to {target}")


if __name__ == "__main__":
    main()
