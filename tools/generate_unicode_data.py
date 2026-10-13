#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.14"
# dependencies = []
# ///
"""Generate both compiled Unicode data modules from CPython's Unicode database.

Every value is derived from CPython's own per-character behavior: `str`
predicates and case methods, and `unicodedata` for categories and digit values.
The generated module therefore matches the `str` methods of the CPython that
ran this script, including its `unicodedata.unidata_version`.

Unicode 16.0.0 is required. _unicode_data.py retains the text-protocol record
contract; _unicodedata_data.py adds properties and normalization mappings.
Host APIs are used only during this explicit generation, never by Mule programs.

Case_Ignorable is not exposed by CPython's API; it is recovered from the
Final_Sigma rule of `str.lower`, which consults exactly that property.
"""

import argparse
import sys
import unicodedata
from pathlib import Path

CODEPOINTS = 0x110000
ASCII = 128
OUTPUT = Path(__file__).resolve().parents[1] / "stdlib" / "_unicode_data.py"
UNICODEDATA_OUTPUT = OUTPUT.with_name("_unicodedata_data.py")
UNICODE_VERSION = "16.0.0"
COMMAND = "uv run tools/generate_unicode_data.py"

# Flag bits in the first two bytes of each record, in bit order.
FLAGS = (
    "alpha",
    "numeric",
    "space",
    "printable",
    "lower",
    "upper",
    "cased",
    "case_ignorable",
    "titlecase",
    "xid_start",
    "xid_continue",
    "linebreak",
)

# Record layout: 2 flag bytes, 1 digit byte (low nibble decimal, high nibble
# digit, 15 for none), then four 3-byte little-endian case fields.
CASE_FIELDS = ("lower", "upper", "title", "fold")
CASE_OFFSET = 3
RECORD_SIZE = CASE_OFFSET + 3 * len(CASE_FIELDS)
NO_DIGIT = 15
# A case field below EXPANDED holds DELTA_BIAS plus the single-codepoint delta;
# at or above it, the field holds EXPANDED plus an offset into _EXPANSIONS.
DELTA_BIAS = 0x400000
EXPANDED = 0x800000

BYTES_LINE_WIDTH = 88


def is_case_ignorable(character):
    """CPython's Case_Ignorable property, observed through Final_Sigma.

    `str.lower` maps U+03A3 to final sigma when a cased letter precedes it
    across case-ignorable characters and no cased letter follows it across
    case-ignorable characters. Placing the character after a cased letter and
    sigma, then before another cased letter, shows whether it is skipped; for
    a cased character, placing it directly before sigma shows whether it is
    skipped there instead of ending the scan.
    """
    if is_cased(character):
        return (character + "Σ").lower()[-1] == "σ"
    return ("AΣ" + character + "A").lower()[1] == "σ"


def is_cased(character):
    """Unicode's derived Cased property: Lowercase, Uppercase, or Lt."""
    return (
        character.islower()
        or character.isupper()
        or unicodedata.category(character) == "Lt"
    )


def character_flags(character):
    values = {
        "alpha": character.isalpha(),
        "numeric": character.isnumeric(),
        "space": character.isspace(),
        "printable": character.isprintable(),
        "lower": character.islower(),
        "upper": character.isupper(),
        "cased": is_cased(character),
        "case_ignorable": is_case_ignorable(character),
        "titlecase": unicodedata.category(character) == "Lt",
        "xid_start": character != "_" and character.isidentifier(),
        "xid_continue": ("a" + character).isidentifier(),
        "linebreak": len(("a" + character + "b").splitlines()) == 2,
    }
    return sum(1 << bit for bit, name in enumerate(FLAGS) if values[name])


def digit_byte(character):
    decimal = unicodedata.decimal(character, NO_DIGIT)
    digit = unicodedata.digit(character, NO_DIGIT)
    if not (0 <= decimal <= NO_DIGIT and 0 <= digit <= NO_DIGIT):
        raise ValueError(f"U+{ord(character):04X} has a digit value above 9")
    return decimal | (digit << 4)


def case_mappings(character):
    """Each full case mapping as a codepoint tuple, in CASE_FIELDS order."""
    return tuple(
        tuple(map(ord, mapped))
        for mapped in (
            character.lower(),
            character.upper(),
            character.title(),
            character.casefold(),
        )
    )


class Expansions:
    """Shared encoding of multi-codepoint mappings: a count, then 3-byte codepoints."""

    def __init__(self):
        self.data = bytearray()
        self.offsets = {}

    def offset(self, codepoints):
        if codepoints not in self.offsets:
            if len(codepoints) not in (2, 3):
                raise ValueError(f"unsupported expansion length: {codepoints}")
            self.offsets[codepoints] = len(self.data)
            self.data.append(len(codepoints))
            for codepoint in codepoints:
                self.data += codepoint.to_bytes(3, "little")
        return self.offsets[codepoints]


def encode_record(codepoint, expansions):
    character = chr(codepoint)
    record = bytearray(character_flags(character).to_bytes(2, "little"))
    record.append(digit_byte(character))
    for mapped in case_mappings(character):
        if len(mapped) == 1:
            field = DELTA_BIAS + mapped[0] - codepoint
        else:
            field = EXPANDED + expansions.offset(mapped)
        if not 0 <= field < 1 << 24:
            raise ValueError(f"U+{codepoint:04X} case field does not fit 24 bits")
        record += field.to_bytes(3, "little")
    return bytes(record)


def build_records():
    """Return each codepoint's record index, the record table, and expansions."""
    expansions = Expansions()
    records = {}
    indices = []
    for codepoint in range(CODEPOINTS):
        record = encode_record(codepoint, expansions)
        indices.append(records.setdefault(record, len(records)))
    return indices, b"".join(records), bytes(expansions.data)


def split_blocks(values, block_size):
    """Deduplicate fixed-size blocks; return block numbers and the unique blocks."""
    blocks = {}
    numbers = [
        blocks.setdefault(tuple(values[start : start + block_size]), len(blocks))
        for start in range(0, len(values), block_size)
    ]
    return numbers, [value for block in blocks for value in block]


def build_index(record_indices):
    """Choose the three-level split with the smallest encoding.

    The top level holds 1-byte middle block numbers; the middle and leaf
    levels hold 2-byte little-endian leaf block numbers and record indices.
    """
    best = None
    for leaf_bits in range(2, 9):
        middle_values, leaves = split_blocks(record_indices, 1 << leaf_bits)
        for middle_bits in range(2, 9):
            top, middles = split_blocks(middle_values, 1 << middle_bits)
            if max(top) >= 1 << 8:
                continue
            size = len(top) + 2 * len(middles) + 2 * len(leaves)
            if best is None or size < best[0]:
                best = (size, leaf_bits, middle_bits, top, middles, leaves)
    if best is None:
        raise ValueError("no split keeps the top level within 1-byte entries")
    _, leaf_bits, middle_bits, top, middles, leaves = best
    if max(middles) >= 1 << 16 or max(leaves) >= 1 << 16:
        raise ValueError("index entries do not fit 16 bits")
    return leaf_bits, middle_bits, bytes(top), pack16(middles), pack16(leaves)


def pack16(values):
    return b"".join(value.to_bytes(2, "little") for value in values)


def bytes_literal_lines(data):
    """Split a bytes literal into implicitly concatenated lines of at most
    BYTES_LINE_WIDTH columns, writing printable ASCII bytes directly."""
    limit = BYTES_LINE_WIDTH - len('    b""')
    lines = []
    current = []
    width = 0
    for byte in data:
        if 0x20 <= byte < 0x7F and byte not in b'"\\':
            piece = chr(byte)
        else:
            piece = f"\\x{byte:02x}"
        if width + len(piece) > limit:
            lines.append('    b"' + "".join(current) + '"')
            current, width = [], 0
        current.append(piece)
        width += len(piece)
    lines.append('    b"' + "".join(current) + '"')
    return lines


def render_bytes(name, data):
    return f"{name} = (\n" + "\n".join(bytes_literal_lines(data)) + "\n)\n"


def ascii_ranges(members):
    """Coalesce sorted ASCII codepoints into inclusive (first, last) ranges."""
    ranges = []
    for codepoint in members:
        if ranges and ranges[-1][1] == codepoint - 1:
            ranges[-1][1] = codepoint
        else:
            ranges.append([codepoint, codepoint])
    return ranges


def ascii_condition(members):
    """A Python expression on `cp` true exactly for the given ASCII codepoints."""
    terms = [
        f"cp == {first}" if first == last else f"{first} <= cp <= {last}"
        for first, last in ascii_ranges(members)
    ]
    return " or ".join(terms) if terms else "False"


def ascii_members(predicate):
    return [codepoint for codepoint in range(ASCII) if predicate(chr(codepoint))]


def check_ascii_case_rules():
    """The emitted ASCII case paths assume CPython's plain A-Z/a-z mapping."""
    for codepoint in range(ASCII):
        character = chr(codepoint)
        upper = codepoint - 32 if 97 <= codepoint <= 122 else codepoint
        lower = codepoint + 32 if 65 <= codepoint <= 90 else codepoint
        expected = (
            (lower,),
            (upper,),
            (upper,),
            (lower,),
        )
        if case_mappings(character) != expected:
            raise ValueError(f"unexpected ASCII case mapping for U+{codepoint:04X}")
        if unicodedata.decimal(character, None) != unicodedata.digit(character, None):
            raise ValueError(f"unexpected ASCII digit value for U+{codepoint:04X}")
        if (unicodedata.decimal(character, None) is None) != (
            not 48 <= codepoint <= 57
        ):
            raise ValueError(f"unexpected ASCII decimal digit U+{codepoint:04X}")


PREDICATES = (
    ("is_alpha", "alpha", "str.isalpha: general category Lm, Lt, Lu, Ll, or Lo."),
    ("is_numeric", "numeric", "str.isnumeric: the character has a numeric value."),
    ("is_space", "space", "str.isspace: bidirectional WS, B, or S, or category Zs."),
    (
        "is_printable",
        "printable",
        "str.isprintable: not in categories C* or Z*, except U+0020 SPACE.",
    ),
    ("is_lower", "lower", "str.islower for one character: derived Lowercase."),
    ("is_upper", "upper", "str.isupper for one character: derived Uppercase."),
    ("is_cased", "cased", "Derived Cased: Lowercase, Uppercase, or category Lt."),
    (
        "is_case_ignorable",
        "case_ignorable",
        "Derived Case_Ignorable, as used by str.lower's Final_Sigma rule.",
    ),
    ("is_titlecase", "titlecase", "General category Lt."),
    (
        "is_xid_start",
        "xid_start",
        "Derived XID_Start; U+005F LOW LINE is not XID_Start.",
    ),
    ("is_xid_continue", "xid_continue", "Derived XID_Continue."),
    (
        "is_linebreak",
        "linebreak",
        "A boundary for str.splitlines: bidirectional B, or category Zl or Zp.",
    ),
)

ASCII_PREDICATES = {
    "alpha": str.isalpha,
    "numeric": str.isnumeric,
    "space": str.isspace,
    "printable": str.isprintable,
    "lower": str.islower,
    "upper": str.isupper,
    "cased": is_cased,
    "case_ignorable": is_case_ignorable,
    "titlecase": lambda character: unicodedata.category(character) == "Lt",
    "xid_start": lambda character: character != "_" and character.isidentifier(),
    "xid_continue": lambda character: ("a" + character).isidentifier(),
    "linebreak": lambda character: len(("a" + character + "b").splitlines()) == 2,
}

MODULE_DOCSTRING = '''\
"""Unicode character database for Mule's compiled text protocols.

Generated by tools/generate_unicode_data.py from CPython {python_version}
(Unicode {unidata_version}). Do not edit; regenerate with:

    {command}

Every function takes an int codepoint in 0..0x10FFFF and matches what CPython's
str methods do for that one character. Arguments are not range checked.

- is_* predicates return bool.
- decimal_value and digit_value return the value 0..9, or -1 when there is none.
- lower_full, upper_full, title_full, and fold_full return the full case
  mapping (UnicodeData plus unconditional SpecialCasing, as str.lower, upper,
  title, and casefold use it): an int codepoint when the mapping is a single
  codepoint (the argument itself when unchanged), or a tuple of 2 or 3 int
  codepoints when it expands. Context-sensitive rules are not applied:
  lower_full(0x3A3) is 0x3C3, never final sigma.

Each codepoint selects a record of {record_size} bytes in _RECORDS through a
three-level index. _TOP[cp >> {top_shift}] selects a block of {middle_size} entries in
_MIDDLE; that 2-byte entry selects a block of {leaf_size} entries in _LEAVES; that
2-byte entry is the record number. A record holds 2 flag bytes, a digit byte
(low nibble decimal, high nibble digit, 15 for none), and four 3-byte case
fields (lower, upper, title, fold). A case field below 0x{expanded:X} is the
mapping's delta plus 0x{delta_bias:X}. Otherwise it is 0x{expanded:X} plus the
offset in _EXPANSIONS of a count byte followed by 3-byte codepoints. Multi-byte
values are little-endian. The tables are bytes literals so that they compile to
static data instead of tuples constructed at startup.
"""
'''

HELPERS = """

def _record(cp):
    \"\"\"Return the offset of the codepoint's record in _RECORDS.\"\"\"
    block = _TOP[cp >> {top_shift}]
    middle = ((block << {middle_bits}) | ((cp >> {leaf_bits}) & {middle_mask})) << 1
    block = _MIDDLE[middle] | (_MIDDLE[middle + 1] << 8)
    leaf = ((block << {leaf_bits}) | (cp & {leaf_mask})) << 1
    return (_LEAVES[leaf] | (_LEAVES[leaf + 1] << 8)) * {record_size}


def _u24(data, at):
    return data[at] | (data[at + 1] << 8) | (data[at + 2] << 16)


def _case(cp, field):
    mapping = _u24(_RECORDS, _record(cp) + field)
    if mapping < 0x{expanded:X}:
        return cp + mapping - 0x{delta_bias:X}
    at = mapping - 0x{expanded:X}
    first = _u24(_EXPANSIONS, at + 1)
    second = _u24(_EXPANSIONS, at + 4)
    if _EXPANSIONS[at] == 2:
        return (first, second)
    return (first, second, _u24(_EXPANSIONS, at + 7))
"""

PREDICATE = """

def {function}(cp):
    \"\"\"{doc}\"\"\"
    if cp < 128:
        return {ascii}
    return (_RECORDS[{record}] & {mask}) != 0
"""

VALUES = """

def decimal_value(cp):
    \"\"\"The decimal digit value used by int() and str.isdecimal, or -1.\"\"\"
    if cp < 128:
        if 48 <= cp <= 57:
            return cp - 48
        return -1
    value = _RECORDS[_record(cp) + 2] & 15
    if value == 15:
        return -1
    return value


def digit_value(cp):
    \"\"\"The digit value used by str.isdigit, or -1.\"\"\"
    if cp < 128:
        if 48 <= cp <= 57:
            return cp - 48
        return -1
    value = _RECORDS[_record(cp) + 2] >> 4
    if value == 15:
        return -1
    return value


def is_decimal(cp):
    \"\"\"str.isdecimal: the character has a decimal digit value.\"\"\"
    return decimal_value(cp) >= 0


def is_digit(cp):
    \"\"\"str.isdigit: the character has a digit value.\"\"\"
    return digit_value(cp) >= 0
"""

CASES = """

def lower_full(cp):
    \"\"\"Full lowercase mapping, as str.lower applies it without Final_Sigma.\"\"\"
    if cp < 128:
        if 65 <= cp <= 90:
            return cp + 32
        return cp
    return _case(cp, {lower})


def upper_full(cp):
    \"\"\"Full uppercase mapping, as str.upper applies it.\"\"\"
    if cp < 128:
        if 97 <= cp <= 122:
            return cp - 32
        return cp
    return _case(cp, {upper})


def title_full(cp):
    \"\"\"Full titlecase mapping, as str.title applies it to a word's first character.\"\"\"
    if cp < 128:
        if 97 <= cp <= 122:
            return cp - 32
        return cp
    return _case(cp, {title})


def fold_full(cp):
    \"\"\"Full case folding, as str.casefold applies it.\"\"\"
    if cp < 128:
        if 65 <= cp <= 90:
            return cp + 32
        return cp
    return _case(cp, {fold})
"""


def render_module():
    check_ascii_case_rules()
    record_indices, records, expansions = build_records()
    leaf_bits, middle_bits, top, middles, leaves = build_index(record_indices)
    top_shift = leaf_bits + middle_bits
    layout = {
        "record_size": RECORD_SIZE,
        "top_shift": top_shift,
        "middle_bits": middle_bits,
        "middle_mask": (1 << middle_bits) - 1,
        "middle_size": 1 << middle_bits,
        "leaf_bits": leaf_bits,
        "leaf_mask": (1 << leaf_bits) - 1,
        "leaf_size": 1 << leaf_bits,
        "expanded": EXPANDED,
        "delta_bias": DELTA_BIAS,
    }
    parts = [
        MODULE_DOCSTRING.format(
            python_version=sys.version.split()[0],
            unidata_version=unicodedata.unidata_version,
            command=COMMAND,
            **layout,
        ),
        "\n",
        render_bytes("_TOP", top),
        render_bytes("_MIDDLE", middles),
        render_bytes("_LEAVES", leaves),
        render_bytes("_RECORDS", records),
        render_bytes("_EXPANSIONS", expansions),
        HELPERS.format(**layout),
    ]
    for function, flag, doc in PREDICATES:
        bit = FLAGS.index(flag)
        members = ascii_members(ASCII_PREDICATES[flag])
        parts.append(
            PREDICATE.format(
                function=function,
                doc=doc,
                ascii=ascii_condition(members),
                record="_record(cp)" + (" + 1" if bit >= 8 else ""),
                mask=1 << (bit & 7),
            )
        )
    parts.append(VALUES)
    parts.append(
        CASES.format(
            **{
                name: CASE_OFFSET + 3 * position
                for position, name in enumerate(CASE_FIELDS)
            }
        )
    )
    return "".join(parts)


def render_unicodedata_module():
    """Properties and sparse normalization maps; Hangul is algorithmic.

    Composition exclusions are observed at generation time through NFC, not
    guessed from the decomposition mapping. Execution needs only these bytes.
    """
    categories = {}
    widths = {}
    records = {}
    indices = []
    decompositions = bytearray()
    mappings = bytearray()
    compositions = []
    for cp in range(CODEPOINTS):
        char = chr(cp)
        category = categories.setdefault(unicodedata.category(char), len(categories))
        width = widths.setdefault(unicodedata.east_asian_width(char), len(widths))
        record = bytes((category, width, unicodedata.combining(char)))
        indices.append(records.setdefault(record, len(records)))
        if 0xAC00 <= cp <= 0xD7A3:
            continue
        decomposition = unicodedata.decomposition(char)
        if not decomposition:
            continue
        parts = decomposition.split()
        compatibility = parts[0].startswith("<")
        mapped = tuple(
            int(value, 16) for value in (parts[1:] if compatibility else parts)
        )
        decompositions += cp.to_bytes(3, "little")
        decompositions += len(mappings).to_bytes(3, "little")
        decompositions += bytes((len(mapped), compatibility))
        for value in mapped:
            mappings += value.to_bytes(3, "little")
        if (
            not compatibility
            and len(mapped) == 2
            and unicodedata.normalize("NFC", "".join(map(chr, mapped))) == char
        ):
            compositions.append((*mapped, cp))
    leaf_bits, middle_bits, top, middle, leaves = build_index(indices)
    composition_bytes = b"".join(
        value.to_bytes(3, "little")
        for triple in sorted(compositions)
        for value in triple
    )
    payload = {
        "_TOP": top,
        "_MIDDLE": middle,
        "_LEAVES": leaves,
        "_RECORDS": b"".join(records),
        "_DECOMPOSITIONS": bytes(decompositions),
        "_MAPPINGS": bytes(mappings),
        "_COMPOSITIONS": composition_bytes,
    }
    source = (
        '"""Unicode properties and normalization maps for compiled Mule Python.\n\n'
        f"Generated by tools/generate_unicode_data.py from CPython {sys.version.split()[0]}\n"
        f"(Unicode {unicodedata.unidata_version}). Regenerate with:\n\n"
        f"    {COMMAND}\n\n"
        "Properties use a deduplicated three-level index and 3-byte records:\n"
        "category index, East Asian width index, canonical combining class.\n"
        "Sorted decomposition records are 8 bytes: codepoint u24, mapping byte\n"
        "offset u24, count u8, compatibility u8. Mapping codepoints are u24.\n"
        "Sorted composition records are 9 bytes: first, second, result u24.\n"
        "All multibyte fields are little-endian. Hangul has no table entries.\n"
        f"Total byte payload: {sum(map(len, payload.values()))}.\n"
        "Internal functions accept validated codepoints in 0..0x10FFFF.\n"
        '"""\n\n'
        f"unidata_version = {unicodedata.unidata_version!r}\n"
        f"_CATEGORIES = {tuple(categories)!r}\n"
        f"_WIDTHS = {tuple(widths)!r}\n\n"
    )
    source += "\n".join(render_bytes(name, value) for name, value in payload.items())
    source += f"""

def _record(cp):
    block = _TOP[cp >> {leaf_bits + middle_bits}]
    at = ((block << {middle_bits}) | ((cp >> {leaf_bits}) & {(1 << middle_bits) - 1})) << 1
    block = _MIDDLE[at] | (_MIDDLE[at + 1] << 8)
    at = ((block << {leaf_bits}) | (cp & {(1 << leaf_bits) - 1})) << 1
    return (_LEAVES[at] | (_LEAVES[at + 1] << 8)) * 3


def _u24(data, at):
    return data[at] | (data[at + 1] << 8) | (data[at + 2] << 16)


def category_code(cp):
    return _CATEGORIES[_RECORDS[_record(cp)]]


def width_code(cp):
    return _WIDTHS[_RECORDS[_record(cp) + 1]]


def combining_code(cp):
    return _RECORDS[_record(cp) + 2]


def decomposition_record(cp):
    low = 0
    high = len(_DECOMPOSITIONS) // 8
    while low < high:
        middle = (low + high) // 2
        code = _u24(_DECOMPOSITIONS, middle * 8)
        if code < cp:
            low = middle + 1
        else:
            high = middle
    at = low * 8
    if at < len(_DECOMPOSITIONS) and _u24(_DECOMPOSITIONS, at) == cp:
        return at
    return -1


def decomposition_compatibility(at):
    return _DECOMPOSITIONS[at + 7] != 0


def decomposition_length(at):
    return _DECOMPOSITIONS[at + 6]


def decomposition_code(at, index):
    return _u24(_MAPPINGS, _u24(_DECOMPOSITIONS, at + 3) + index * 3)


def composition_code(first, second):
    low = 0
    high = len(_COMPOSITIONS) // 9
    while low < high:
        middle = (low + high) // 2
        at = middle * 9
        left = _u24(_COMPOSITIONS, at)
        right = _u24(_COMPOSITIONS, at + 3)
        if left < first or (left == first and right < second):
            low = middle + 1
        else:
            high = middle
    at = low * 9
    if (at < len(_COMPOSITIONS)
        and _u24(_COMPOSITIONS, at) == first
        and _u24(_COMPOSITIONS, at + 3) == second):
        return _u24(_COMPOSITIONS, at + 6)
    return -1
"""
    return source


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=OUTPUT)
    parser.add_argument("--unicodedata-output", type=Path, default=UNICODEDATA_OUTPUT)
    args = parser.parse_args()
    if unicodedata.unidata_version != UNICODE_VERSION:
        raise RuntimeError(
            f"expected Unicode {UNICODE_VERSION}, got {unicodedata.unidata_version}"
        )
    output = args.output
    source = render_module()
    properties = render_unicodedata_module()
    output.write_text(source, encoding="utf-8")
    print(f"wrote {len(source.encode())} bytes to {output}")
    args.unicodedata_output.write_text(properties, encoding="utf-8")
    print(f"wrote {len(properties.encode())} bytes to {args.unicodedata_output}")


if __name__ == "__main__":
    main()
