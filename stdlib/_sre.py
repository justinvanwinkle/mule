"""The SRE engine, compiled by Mule.

``re._compiler`` turns a parsed pattern into a code list; this module
executes that list with a flat iterative bytecode virtual machine and
provides the compiled Pattern, Match, Scanner, and Template objects,
following CPython's ``Modules/_sre/sre_lib.h`` and ``Modules/_sre/sre.c``.
"""

from __future__ import annotations

import mule_types
import types
import _unicode_data

MAGIC: Final[int] = 20230612
CODESIZE: Final[int] = 4
MAXREPEAT: Final[int] = 4294967295
MAXGROUPS: Final[int] = 1073741823
copyright: str = " SRE 2.2.2 Copyright (c) 1997-2002 by Secret Labs AB "

_MASK32: Final[int] = 0xFFFFFFFF
_UNSET: Final[int] = 0xFFFFFFFF

# Opcode values, in re._constants.OPCODES order.
_FAILURE: Final[int] = 0
_SUCCESS: Final[int] = 1
_ANY: Final[int] = 2
_ANY_ALL: Final[int] = 3
_ASSERT: Final[int] = 4
_ASSERT_NOT: Final[int] = 5
_AT: Final[int] = 6
_BRANCH: Final[int] = 7
_CATEGORY: Final[int] = 8
_CHARSET: Final[int] = 9
_BIGCHARSET: Final[int] = 10
_GROUPREF: Final[int] = 11
_GROUPREF_EXISTS: Final[int] = 12
_IN: Final[int] = 13
_INFO: Final[int] = 14
_JUMP: Final[int] = 15
_LITERAL: Final[int] = 16
_MARK: Final[int] = 17
_MAX_UNTIL: Final[int] = 18
_MIN_UNTIL: Final[int] = 19
_NOT_LITERAL: Final[int] = 20
_NEGATE: Final[int] = 21
_RANGE: Final[int] = 22
_REPEAT: Final[int] = 23
_REPEAT_ONE: Final[int] = 24
_SUBPATTERN: Final[int] = 25
_MIN_REPEAT_ONE: Final[int] = 26
_ATOMIC_GROUP: Final[int] = 27
_POSSESSIVE_REPEAT: Final[int] = 28
_POSSESSIVE_REPEAT_ONE: Final[int] = 29
_GROUPREF_IGNORE: Final[int] = 30
_IN_IGNORE: Final[int] = 31
_LITERAL_IGNORE: Final[int] = 32
_NOT_LITERAL_IGNORE: Final[int] = 33
_GROUPREF_LOC_IGNORE: Final[int] = 34
_IN_LOC_IGNORE: Final[int] = 35
_LITERAL_LOC_IGNORE: Final[int] = 36
_NOT_LITERAL_LOC_IGNORE: Final[int] = 37
_GROUPREF_UNI_IGNORE: Final[int] = 38
_IN_UNI_IGNORE: Final[int] = 39
_LITERAL_UNI_IGNORE: Final[int] = 40
_NOT_LITERAL_UNI_IGNORE: Final[int] = 41
_RANGE_UNI_IGNORE: Final[int] = 42

# AT codes, in re._constants.ATCODES order.
_AT_BEGINNING: Final[int] = 0
_AT_BEGINNING_LINE: Final[int] = 1
_AT_BEGINNING_STRING: Final[int] = 2
_AT_BOUNDARY: Final[int] = 3
_AT_NON_BOUNDARY: Final[int] = 4
_AT_END: Final[int] = 5
_AT_END_LINE: Final[int] = 6
_AT_END_STRING: Final[int] = 7
_AT_LOC_BOUNDARY: Final[int] = 8
_AT_LOC_NON_BOUNDARY: Final[int] = 9
_AT_UNI_BOUNDARY: Final[int] = 10
_AT_UNI_NON_BOUNDARY: Final[int] = 11

# CATEGORY codes, in re._constants.CHCODES order.
_CATEGORY_DIGIT: Final[int] = 0
_CATEGORY_NOT_DIGIT: Final[int] = 1
_CATEGORY_SPACE: Final[int] = 2
_CATEGORY_NOT_SPACE: Final[int] = 3
_CATEGORY_WORD: Final[int] = 4
_CATEGORY_NOT_WORD: Final[int] = 5
_CATEGORY_LINEBREAK: Final[int] = 6
_CATEGORY_NOT_LINEBREAK: Final[int] = 7
_CATEGORY_LOC_WORD: Final[int] = 8
_CATEGORY_LOC_NOT_WORD: Final[int] = 9
_CATEGORY_UNI_DIGIT: Final[int] = 10
_CATEGORY_UNI_NOT_DIGIT: Final[int] = 11
_CATEGORY_UNI_SPACE: Final[int] = 12
_CATEGORY_UNI_NOT_SPACE: Final[int] = 13
_CATEGORY_UNI_WORD: Final[int] = 14
_CATEGORY_UNI_NOT_WORD: Final[int] = 15
_CATEGORY_UNI_LINEBREAK: Final[int] = 16
_CATEGORY_UNI_NOT_LINEBREAK: Final[int] = 17

# INFO flags.
_SRE_INFO_PREFIX: Final[int] = 1
_SRE_INFO_LITERAL: Final[int] = 2
_SRE_INFO_CHARSET: Final[int] = 4

# Regex flags.
_SRE_FLAG_TEMPLATE: Final[int] = 1
_SRE_FLAG_IGNORECASE: Final[int] = 2
_SRE_FLAG_LOCALE: Final[int] = 4
_SRE_FLAG_MULTILINE: Final[int] = 8
_SRE_FLAG_DOTALL: Final[int] = 16
_SRE_FLAG_UNICODE: Final[int] = 32
_SRE_FLAG_VERBOSE: Final[int] = 64
_SRE_FLAG_DEBUG: Final[int] = 128
_SRE_FLAG_ASCII: Final[int] = 256

# VM jump return states (matching JUMP_* in Modules/_sre/sre_lib.h).
_JUMP_NONE: Final[int] = 0
_JUMP_MAX_UNTIL_1: Final[int] = 1
_JUMP_MAX_UNTIL_2: Final[int] = 2
_JUMP_MAX_UNTIL_3: Final[int] = 3
_JUMP_MIN_UNTIL_1: Final[int] = 4
_JUMP_MIN_UNTIL_2: Final[int] = 5
_JUMP_MIN_UNTIL_3: Final[int] = 6
_JUMP_REPEAT: Final[int] = 7
_JUMP_REPEAT_ONE_1: Final[int] = 8
_JUMP_REPEAT_ONE_2: Final[int] = 9
_JUMP_MIN_REPEAT_ONE: Final[int] = 10
_JUMP_BRANCH: Final[int] = 11
_JUMP_ASSERT: Final[int] = 12
_JUMP_ASSERT_NOT: Final[int] = 13
_JUMP_POSS_REPEAT_1: Final[int] = 14
_JUMP_POSS_REPEAT_2: Final[int] = 15
_JUMP_ATOMIC_GROUP: Final[int] = 16

# State array indices on _Engine.state.
_ST_PTR: Final[int] = 0
_ST_END: Final[int] = 1
_ST_START: Final[int] = 2
_ST_NUM_MARKS: Final[int] = 3
_ST_LASTINDEX: Final[int] = 4
_ST_SAVE_MARKS: Final[int] = 5
_ST_REPEAT_HEAD: Final[int] = 6
_ST_MATCH_ALL: Final[int] = 7
_ST_MUST_ADVANCE: Final[int] = 8

# Pattern info array indices on Pattern._p_info.
_PI_PATTERN_PC: Final[int] = 0
_PI_INFO_FLAGS: Final[int] = 1
_PI_MIN_WIDTH: Final[int] = 2
_PI_PREFIX_LEN: Final[int] = 3
_PI_PREFIX_SKIP: Final[int] = 4
_PI_PREFIX_FIRST: Final[int] = 5
_PI_CHARSET_PC: Final[int] = 6
_PI_ANCHORED: Final[int] = 7
_PI_PREFIX_BYTES_VALID: Final[int] = 8


def getcodesize() -> int:
    return CODESIZE


def ascii_iscased(character: int) -> bool:
    if character < 0:
        return False
    c: int = character & _MASK32
    return (65 <= c <= 90) or (97 <= c <= 122)


def ascii_tolower(character: int) -> int:
    if character < 0:
        return character
    c: int = character & _MASK32
    return (c + 32) if (65 <= c <= 90) else c


@mule_types.optimize(safety=0)
def _ascii_toupper(character: int) -> int:
    c: int = character & _MASK32
    return (c - 32) if (97 <= c <= 122) else c


def unicode_tolower(character: int) -> int:
    if character < 0:
        return character
    c: int = character & _MASK32
    if c < 128:
        return (c + 32) if (65 <= c <= 90) else c
    if c > 0x10FFFF:
        return c
    rec: int = _unicode_data._record(c)
    lo_map: int = _unicode_data._u24(_unicode_data._RECORDS, rec + 3)
    if lo_map < 0x800000:
        return c + lo_map - 0x400000
    if c == 0x0130:
        return 105
    return c


@mule_types.optimize(safety=0)
def _unicode_toupper(character: int) -> int:
    c: int = character & _MASK32
    if c < 128:
        return (c - 32) if (97 <= c <= 122) else c
    if c > 0x10FFFF:
        return c
    rec: int = _unicode_data._record(c)
    up_map: int = _unicode_data._u24(_unicode_data._RECORDS, rec + 6)
    if up_map < 0x800000:
        return c + up_map - 0x400000
    return c


def unicode_iscased(character: int) -> bool:
    if character < 0:
        return False
    c: int = character & _MASK32
    if c < 128:
        return (65 <= c <= 90) or (97 <= c <= 122)
    if c > 0x10FFFF:
        return False
    rec: int = _unicode_data._record(c)
    if _unicode_data._u24(_unicode_data._RECORDS, rec + 3) != 0x400000:
        return True
    return _unicode_data._u24(_unicode_data._RECORDS, rec + 6) != 0x400000


@mule_types.optimize(safety=0)
def _is_ascii_word(ch: int) -> bool:
    c: int = ch & _MASK32
    if c >= 128:
        return False
    return (97 <= c <= 122) or (65 <= c <= 90) or (48 <= c <= 57) or c == 95


@mule_types.optimize(safety=0)
def _is_uni_word(ch: int) -> bool:
    c: int = ch & _MASK32
    if c < 128:
        return (97 <= c <= 122) or (65 <= c <= 90) or (48 <= c <= 57) or c == 95
    if c > 0x10FFFF:
        return False
    return (_unicode_data._RECORDS[_unicode_data._record(c)] & 3) != 0


@mule_types.optimize(safety=0)
def _is_ascii_space(ch: int) -> bool:
    c: int = ch & _MASK32
    return c == 32 or (9 <= c <= 13)


@mule_types.optimize(safety=0)
def _is_uni_space(ch: int) -> bool:
    c: int = ch & _MASK32
    if c < 128:
        return c == 32 or (9 <= c <= 13) or (28 <= c <= 31)
    if c == 0x85 or c == 0xA0:
        return True
    if c < 256 or c > 0x10FFFF:
        return False
    return (_unicode_data._RECORDS[_unicode_data._record(c)] & 4) != 0


@mule_types.optimize(safety=0)
def _is_uni_digit(ch: int) -> bool:
    c: int = ch & _MASK32
    if c < 256:
        return 48 <= c <= 57
    if c > 0x10FFFF:
        return False
    return (_unicode_data._RECORDS[_unicode_data._record(c) + 2] & 15) != 15


@mule_types.optimize(safety=0)
def _is_uni_linebreak(ch: int) -> bool:
    c: int = ch & _MASK32
    if c < 128:
        return (10 <= c <= 13) or (28 <= c <= 30)
    return c == 0x85 or c == 0x2028 or c == 0x2029


@mule_types.optimize(safety=0)
def _char_loc_ignore(pattern_ch: int, ch: int) -> bool:
    p: int = pattern_ch & _MASK32
    c: int = ch & _MASK32
    if c == p:
        return True
    if c < 256:
        if ascii_tolower(c) == p:
            return True
        if _ascii_toupper(c) == p:
            return True
    return False


@mule_types.optimize(safety=0)
def _category(cat: int, ch: int) -> bool:
    cat_v: int = cat & _MASK32
    c: int = ch & _MASK32
    if cat_v == _CATEGORY_DIGIT:
        return 48 <= c <= 57
    if cat_v == _CATEGORY_NOT_DIGIT:
        return c < 48 or c > 57
    if cat_v == _CATEGORY_SPACE:
        return _is_ascii_space(c)
    if cat_v == _CATEGORY_NOT_SPACE:
        return not _is_ascii_space(c)
    if cat_v == _CATEGORY_WORD or cat_v == _CATEGORY_LOC_WORD:
        return _is_ascii_word(c)
    if cat_v == _CATEGORY_NOT_WORD or cat_v == _CATEGORY_LOC_NOT_WORD:
        return not _is_ascii_word(c)
    if cat_v == _CATEGORY_LINEBREAK:
        return c == 10
    if cat_v == _CATEGORY_NOT_LINEBREAK:
        return c != 10
    if cat_v == _CATEGORY_UNI_DIGIT:
        return _is_uni_digit(c)
    if cat_v == _CATEGORY_UNI_NOT_DIGIT:
        return not _is_uni_digit(c)
    if cat_v == _CATEGORY_UNI_SPACE:
        return _is_uni_space(c)
    if cat_v == _CATEGORY_UNI_NOT_SPACE:
        return not _is_uni_space(c)
    if cat_v == _CATEGORY_UNI_WORD:
        return _is_uni_word(c)
    if cat_v == _CATEGORY_UNI_NOT_WORD:
        return not _is_uni_word(c)
    if cat_v == _CATEGORY_UNI_LINEBREAK:
        return _is_uni_linebreak(c)
    if cat_v == _CATEGORY_UNI_NOT_LINEBREAK:
        return not _is_uni_linebreak(c)
    return False


@mule_types.optimize(safety=0)
def _sre_charset(code: list[mule_types.uint32], set_pc: int, ch: int) -> bool:
    """Port of SRE(charset) in Modules/_sre/sre_lib.h."""
    ok: bool = True
    pc: int = set_pc & _MASK32
    c: int = ch & _MASK32
    while True:
        op: int = code[pc]
        pc = (pc + 1) & _MASK32
        if op == _FAILURE:
            return not ok
        if op == _LITERAL:
            if c == code[pc]:
                return ok
            pc = (pc + 1) & _MASK32
        elif op == _CATEGORY:
            if _category(code[pc], c):
                return ok
            pc = (pc + 1) & _MASK32
        elif op == _CHARSET:
            if c < 256 and ((code[pc + (c >> 5)] >> (c & 31)) & 1) != 0:
                return ok
            pc = (pc + 8) & _MASK32
        elif op == _RANGE:
            if code[pc] <= c <= code[pc + 1]:
                return ok
            pc = (pc + 2) & _MASK32
        elif op == _RANGE_UNI_IGNORE:
            rlo: int = code[pc]
            rhi: int = code[pc + 1]
            if rlo <= c <= rhi:
                return ok
            uch: int = _unicode_toupper(c) & _MASK32
            if rlo <= uch <= rhi:
                return ok
            pc = (pc + 2) & _MASK32
        elif op == _NEGATE:
            ok = not ok
        elif op == _BIGCHARSET:
            count: int = code[pc]
            pc = (pc + 1) & _MASK32
            if c < 0x10000:
                hi_byte: int = c >> 8
                word: int = code[pc + (hi_byte >> 2)]
                block: int = (word >> ((hi_byte & 3) << 3)) & 255
                pc = (pc + 64) & _MASK32
                bit_pos: int = (block << 8) + (c & 255)
                if ((code[pc + (bit_pos >> 5)] >> (c & 31)) & 1) != 0:
                    return ok
            else:
                pc = (pc + 64) & _MASK32
            pc = (pc + (count << 3)) & _MASK32
        else:
            return False


@mule_types.optimize(safety=0)
def _sre_charset_loc_ignore(
    code: list[mule_types.uint32], set_pc: int, ch: int
) -> bool:
    """Port of SRE(charset_loc_ignore) in Modules/_sre/sre_lib.h."""
    c: int = ch & _MASK32
    lo: int = (ascii_tolower(c) & _MASK32) if c < 256 else c
    up: int = (_ascii_toupper(c) & _MASK32) if c < 256 else c
    if up == lo:
        return _sre_charset(code, set_pc, lo)
    ok: bool = True
    pc: int = set_pc & _MASK32
    while True:
        op: int = code[pc]
        pc = (pc + 1) & _MASK32
        if op == _FAILURE:
            return not ok
        if op == _LITERAL:
            w: int = code[pc]
            if lo == w or up == w:
                return ok
            pc = (pc + 1) & _MASK32
        elif op == _CATEGORY:
            cat: int = code[pc]
            if _category(cat, lo) or _category(cat, up):
                return ok
            pc = (pc + 1) & _MASK32
        elif op == _CHARSET:
            if (lo < 256 and ((code[pc + (lo >> 5)] >> (lo & 31)) & 1) != 0) or (
                up < 256 and ((code[pc + (up >> 5)] >> (up & 31)) & 1) != 0
            ):
                return ok
            pc = (pc + 8) & _MASK32
        elif op == _RANGE:
            rlo: int = code[pc]
            rhi: int = code[pc + 1]
            if (rlo <= lo <= rhi) or (rlo <= up <= rhi):
                return ok
            pc = (pc + 2) & _MASK32
        elif op == _NEGATE:
            ok = not ok
        else:
            return False


@mule_types.optimize(safety=0)
def _sre_in_charset(
    code: list[mule_types.uint32],
    charset_flat: list[mule_types.uint32],
    charset_base: list[mule_types.uint32],
    in_op: int,
    set_pc: int,
    ch: int,
) -> bool:
    op: int = in_op & _MASK32
    spc: int = set_pc & _MASK32
    c: int = ch & _MASK32
    if c < 256:
        base: int = charset_base[spc]
        if base < _UNSET:
            return charset_flat[base + c] != 0
    if op == _IN:
        return _sre_charset(code, spc, c)
    if op == _IN_IGNORE:
        return _sre_charset(code, spc, ascii_tolower(c) & _MASK32)
    if op == _IN_UNI_IGNORE:
        return _sre_charset(code, spc, unicode_tolower(c) & _MASK32)
    return _sre_charset_loc_ignore(code, spc, c)


@mule_types.optimize(safety=0)
def _sre_at(
    is_bytes: bool, text: str, data: bytes, end_in: int, ptr_in: int, atcode_in: int
) -> bool:
    """Port of SRE(at) in Modules/_sre/sre_lib.h."""
    end: int = end_in & _MASK32
    ptr: int = ptr_in & _MASK32
    atcode: int = atcode_in & _MASK32
    if atcode == _AT_BEGINNING or atcode == _AT_BEGINNING_STRING:
        return ptr == 0
    if atcode == _AT_BEGINNING_LINE:
        if ptr == 0:
            return True
        prev_ch: int = data[ptr - 1] if is_bytes else ord(text[ptr - 1])
        return prev_ch == 10
    if atcode == _AT_END:
        if ptr == end:
            return True
        if end - ptr == 1:
            cur_ch: int = data[ptr] if is_bytes else ord(text[ptr])
            return cur_ch == 10
        return False
    if atcode == _AT_END_LINE:
        if ptr == end:
            return True
        cur_ch2: int = data[ptr] if is_bytes else ord(text[ptr])
        return cur_ch2 == 10
    if atcode == _AT_END_STRING:
        return ptr == end
    if atcode == _AT_BOUNDARY or atcode == _AT_LOC_BOUNDARY:
        if end == 0:
            return False
        thatp: bool = (
            _is_ascii_word(data[ptr - 1] if is_bytes else ord(text[ptr - 1]))
            if ptr > 0
            else False
        )
        thisp: bool = (
            _is_ascii_word(data[ptr] if is_bytes else ord(text[ptr]))
            if ptr < end
            else False
        )
        return thisp != thatp
    if atcode == _AT_NON_BOUNDARY or atcode == _AT_LOC_NON_BOUNDARY:
        if end == 0:
            return True
        thatp2: bool = (
            _is_ascii_word(data[ptr - 1] if is_bytes else ord(text[ptr - 1]))
            if ptr > 0
            else False
        )
        thisp2: bool = (
            _is_ascii_word(data[ptr] if is_bytes else ord(text[ptr]))
            if ptr < end
            else False
        )
        return thisp2 == thatp2
    if atcode == _AT_UNI_BOUNDARY:
        if end == 0:
            return False
        thatpu: bool = (
            _is_uni_word(data[ptr - 1] if is_bytes else ord(text[ptr - 1]))
            if ptr > 0
            else False
        )
        thispu: bool = (
            _is_uni_word(data[ptr] if is_bytes else ord(text[ptr]))
            if ptr < end
            else False
        )
        return thispu != thatpu
    if atcode == _AT_UNI_NON_BOUNDARY:
        if end == 0:
            return True
        thatpu2: bool = (
            _is_uni_word(data[ptr - 1] if is_bytes else ord(text[ptr - 1]))
            if ptr > 0
            else False
        )
        thispu2: bool = (
            _is_uni_word(data[ptr] if is_bytes else ord(text[ptr]))
            if ptr < end
            else False
        )
        return thispu2 == thatpu2
    return False


@mule_types.optimize(safety=0)
def _sre_count(
    code: list[mule_types.uint32],
    charset_flat: list[mule_types.uint32],
    charset_base: list[mule_types.uint32],
    is_bytes: bool,
    text: str,
    data: bytes,
    ptr_in: int,
    end_in: int,
    pattern_pc_in: int,
    maxcount_in: int,
) -> int:
    """Port of SRE(count) in Modules/_sre/sre_lib.h."""
    orig_ptr: int = ptr_in & _MASK32
    ptr: int = orig_ptr
    end: int = end_in & _MASK32
    pattern_pc: int = pattern_pc_in & _MASK32
    maxcount: int = maxcount_in & _MASK32
    if maxcount < end - ptr and maxcount != MAXREPEAT:
        end = (ptr + maxcount) & _MASK32
    op: int = code[pattern_pc]

    if op == _ANY_ALL:
        return (end - orig_ptr) & _MASK32

    if not is_bytes:
        if (
            op == _IN
            or op == _IN_IGNORE
            or op == _IN_UNI_IGNORE
            or op == _IN_LOC_IGNORE
        ):
            set_pc: int = (pattern_pc + 2) & _MASK32
            base: int = charset_base[set_pc]
            if base < _UNSET:
                while ptr < end:
                    ch: int = ord(text[ptr]) & _MASK32
                    if ch < 256:
                        if charset_flat[base + ch] == 0:
                            break
                    elif not _sre_in_charset(
                        code, charset_flat, charset_base, op, set_pc, ch
                    ):
                        break
                    ptr = (ptr + 1) & _MASK32
                return (ptr - orig_ptr) & _MASK32
            while ptr < end and _sre_in_charset(
                code, charset_flat, charset_base, op, set_pc, ord(text[ptr]) & _MASK32
            ):
                ptr = (ptr + 1) & _MASK32
            return (ptr - orig_ptr) & _MASK32
        if op == _ANY:
            while ptr < end and ord(text[ptr]) != 10:
                ptr = (ptr + 1) & _MASK32
            return (ptr - orig_ptr) & _MASK32
        if op == _LITERAL:
            wanted: int = code[pattern_pc + 1]
            while ptr < end and ord(text[ptr]) == wanted:
                ptr = (ptr + 1) & _MASK32
            return (ptr - orig_ptr) & _MASK32
        if op == _NOT_LITERAL:
            unwanted: int = code[pattern_pc + 1]
            while ptr < end and ord(text[ptr]) != unwanted:
                ptr = (ptr + 1) & _MASK32
            return (ptr - orig_ptr) & _MASK32
        if op == _LITERAL_IGNORE or op == _LITERAL_LOC_IGNORE:
            wanted_lo: int = code[pattern_pc + 1]
            while ptr < end and ascii_tolower(ord(text[ptr])) == wanted_lo:
                ptr = (ptr + 1) & _MASK32
            return (ptr - orig_ptr) & _MASK32
        if op == _LITERAL_UNI_IGNORE:
            wanted_ulo: int = code[pattern_pc + 1]
            while ptr < end and unicode_tolower(ord(text[ptr])) == wanted_ulo:
                ptr = (ptr + 1) & _MASK32
            return (ptr - orig_ptr) & _MASK32
        if op == _NOT_LITERAL_IGNORE or op == _NOT_LITERAL_LOC_IGNORE:
            unwanted_lo: int = code[pattern_pc + 1]
            while ptr < end and ascii_tolower(ord(text[ptr])) != unwanted_lo:
                ptr = (ptr + 1) & _MASK32
            return (ptr - orig_ptr) & _MASK32
        if op == _NOT_LITERAL_UNI_IGNORE:
            unwanted_ulo: int = code[pattern_pc + 1]
            while ptr < end and unicode_tolower(ord(text[ptr])) != unwanted_ulo:
                ptr = (ptr + 1) & _MASK32
            return (ptr - orig_ptr) & _MASK32
        if op == _CATEGORY:
            cat: int = code[pattern_pc + 1]
            while ptr < end and _category(cat, ord(text[ptr]) & _MASK32):
                ptr = (ptr + 1) & _MASK32
            return (ptr - orig_ptr) & _MASK32
        return 0
    else:
        if (
            op == _IN
            or op == _IN_IGNORE
            or op == _IN_UNI_IGNORE
            or op == _IN_LOC_IGNORE
        ):
            set_pc_b: int = (pattern_pc + 2) & _MASK32
            base_b: int = charset_base[set_pc_b]
            if base_b < _UNSET:
                while ptr < end:
                    ch_b: int = data[ptr] & 255
                    if charset_flat[base_b + ch_b] == 0:
                        break
                    ptr = (ptr + 1) & _MASK32
                return (ptr - orig_ptr) & _MASK32
            while ptr < end and _sre_in_charset(
                code, charset_flat, charset_base, op, set_pc_b, data[ptr] & 255
            ):
                ptr = (ptr + 1) & _MASK32
            return (ptr - orig_ptr) & _MASK32
        if op == _ANY:
            while ptr < end and data[ptr] != 10:
                ptr = (ptr + 1) & _MASK32
            return (ptr - orig_ptr) & _MASK32
        if op == _LITERAL:
            wanted_b: int = code[pattern_pc + 1]
            if wanted_b > 255:
                return 0
            while ptr < end and data[ptr] == wanted_b:
                ptr = (ptr + 1) & _MASK32
            return (ptr - orig_ptr) & _MASK32
        if op == _NOT_LITERAL:
            unwanted_b: int = code[pattern_pc + 1]
            if unwanted_b > 255:
                return (end - orig_ptr) & _MASK32
            while ptr < end and data[ptr] != unwanted_b:
                ptr = (ptr + 1) & _MASK32
            return (ptr - orig_ptr) & _MASK32
        if op == _LITERAL_IGNORE:
            wanted_blo: int = code[pattern_pc + 1]
            while ptr < end and ascii_tolower(data[ptr]) == wanted_blo:
                ptr = (ptr + 1) & _MASK32
            return (ptr - orig_ptr) & _MASK32
        if op == _LITERAL_LOC_IGNORE:
            wanted_l: int = code[pattern_pc + 1]
            while ptr < end and _char_loc_ignore(wanted_l, data[ptr]):
                ptr = (ptr + 1) & _MASK32
            return (ptr - orig_ptr) & _MASK32
        if op == _LITERAL_UNI_IGNORE:
            wanted_bulo: int = code[pattern_pc + 1]
            while ptr < end and unicode_tolower(data[ptr]) == wanted_bulo:
                ptr = (ptr + 1) & _MASK32
            return (ptr - orig_ptr) & _MASK32
        if op == _NOT_LITERAL_IGNORE:
            unwanted_blo: int = code[pattern_pc + 1]
            while ptr < end and ascii_tolower(data[ptr]) != unwanted_blo:
                ptr = (ptr + 1) & _MASK32
            return (ptr - orig_ptr) & _MASK32
        if op == _NOT_LITERAL_LOC_IGNORE:
            unwanted_l: int = code[pattern_pc + 1]
            while ptr < end and not _char_loc_ignore(unwanted_l, data[ptr]):
                ptr = (ptr + 1) & _MASK32
            return (ptr - orig_ptr) & _MASK32
        if op == _NOT_LITERAL_UNI_IGNORE:
            unwanted_bulo: int = code[pattern_pc + 1]
            while ptr < end and unicode_tolower(data[ptr]) != unwanted_bulo:
                ptr = (ptr + 1) & _MASK32
            return (ptr - orig_ptr) & _MASK32
        if op == _CATEGORY:
            cat_b: int = code[pattern_pc + 1]
            while ptr < end and _category(cat_b, data[ptr] & 255):
                ptr = (ptr + 1) & _MASK32
            return (ptr - orig_ptr) & _MASK32
        return 0


@mule_types.optimize(safety=0)
def _sre_mark_push(
    mark_stack: list[mule_types.uint32],
    marks: list[mule_types.uint32],
    mark_sp_in: int,
    num_marks_in: int,
) -> int:
    mark_sp: int = mark_sp_in & _MASK32
    num_marks: int = num_marks_in & _MASK32
    if num_marks == 0:
        return mark_sp
    needed: int = (mark_sp + num_marks) & _MASK32
    while len(mark_stack) < needed:
        mark_stack.append(0)
    j: int = 0
    while j < num_marks:
        mark_stack[mark_sp + j] = marks[j]
        j = (j + 1) & _MASK32
    return needed


@mule_types.optimize(safety=0)
def _sre_mark_pop(
    mark_stack: list[mule_types.uint32],
    marks: list[mule_types.uint32],
    mark_sp_in: int,
    num_marks_in: int,
) -> int:
    mark_sp: int = mark_sp_in & _MASK32
    num_marks: int = num_marks_in & _MASK32
    if num_marks == 0:
        return mark_sp
    new_sp: int = (mark_sp - num_marks) & _MASK32
    j: int = 0
    while j < num_marks:
        marks[j] = mark_stack[new_sp + j]
        j = (j + 1) & _MASK32
    return new_sp


@mule_types.optimize(safety=0)
def _sre_mark_pop_keep(
    mark_stack: list[mule_types.uint32],
    marks: list[mule_types.uint32],
    mark_sp_in: int,
    num_marks_in: int,
) -> None:
    mark_sp: int = mark_sp_in & _MASK32
    num_marks: int = num_marks_in & _MASK32
    if num_marks == 0:
        return
    base: int = (mark_sp - num_marks) & _MASK32
    j: int = 0
    while j < num_marks:
        marks[j] = mark_stack[base + j]
        j = (j + 1) & _MASK32


@mule_types.optimize(safety=0)
def _sre_match(
    code: list[mule_types.uint32],
    charset_flat: list[mule_types.uint32],
    charset_base: list[mule_types.uint32],
    is_bytes: bool,
    text: str,
    data: bytes,
    marks: list[mule_types.uint32],
    mark_stack: list[mule_types.uint32],
    ctx_stack: list[mule_types.uint32],
    rep_stack: list[mule_types.uint32],
    state: list[mule_types.uint32],
    init_pattern_in: int,
    init_toplevel_in: int,
) -> int:
    """Flat iterative port of SRE(match) in Modules/_sre/sre_lib.h."""
    curr_ptr: int = state[_ST_PTR]
    end: int = state[_ST_END]
    start: int = state[_ST_START]
    num_marks: int = state[_ST_NUM_MARKS]
    lastindex: int = state[_ST_LASTINDEX]
    save_marks: int = state[_ST_SAVE_MARKS]
    repeat_head: int = state[_ST_REPEAT_HEAD]
    match_all: bool = state[_ST_MATCH_ALL] != 0
    must_advance: bool = state[_ST_MUST_ADVANCE] != 0

    ctx_sp: int = 0
    rep_sp: int = 0
    mark_sp: int = 0

    pattern: int = init_pattern_in & _MASK32
    ptr: int = curr_ptr
    ctx_count: int = 0
    ctx_u: int = 0
    ctx_num_marks: int = 0
    ctx_lastindex: int = 0
    ctx_toplevel: int = init_toplevel_in & _MASK32
    ctx_jump: int = _JUMP_NONE
    ctx_saved_last_ptr: int = _UNSET

    ret: int = 0
    # action: 0 = entrance, 1 = dispatch, 2 = exit, 3 = push_jump
    action: int = 0
    next_pattern: int = 0
    next_toplevel: int = 0
    next_jump: int = 0

    while True:
        if action == 0:
            ptr = curr_ptr
            if code[pattern] == _INFO:
                min_len: int = code[pattern + 3]
                if min_len > 0 and (end - ptr) < min_len:
                    ret = 0
                    action = 2
                    continue
                pattern = (pattern + code[pattern + 1] + 1) & _MASK32
            action = 1

        if action == 1:
            op: int = code[pattern]
            pattern = (pattern + 1) & _MASK32

            if op == _MARK:
                mark_id: int = code[pattern]
                if (mark_id & 1) != 0:
                    lastindex = ((mark_id >> 1) + 1) & _MASK32
                if mark_id >= num_marks:
                    jm: int = num_marks
                    while jm < mark_id:
                        marks[jm] = _UNSET
                        jm = (jm + 1) & _MASK32
                    num_marks = (mark_id + 1) & _MASK32
                marks[mark_id] = ptr
                pattern = (pattern + 1) & _MASK32
                continue

            if op == _LITERAL:
                if ptr >= end:
                    ret = 0
                    action = 2
                    continue
                ch_lit: int = (
                    (data[ptr] & 255) if is_bytes else (ord(text[ptr]) & _MASK32)
                )
                if ch_lit != code[pattern]:
                    ret = 0
                    action = 2
                    continue
                pattern = (pattern + 1) & _MASK32
                ptr = (ptr + 1) & _MASK32
                continue

            if op == _NOT_LITERAL:
                if ptr >= end:
                    ret = 0
                    action = 2
                    continue
                ch_nlit: int = (
                    (data[ptr] & 255) if is_bytes else (ord(text[ptr]) & _MASK32)
                )
                if ch_nlit == code[pattern]:
                    ret = 0
                    action = 2
                    continue
                pattern = (pattern + 1) & _MASK32
                ptr = (ptr + 1) & _MASK32
                continue

            if op == _SUCCESS:
                if ctx_toplevel != 0 and (
                    (match_all and ptr != end) or (must_advance and ptr == start)
                ):
                    ret = 0
                    action = 2
                    continue
                curr_ptr = ptr
                ret = 1
                action = 2
                continue

            if op == _AT:
                if not _sre_at(is_bytes, text, data, end, ptr, code[pattern]):
                    ret = 0
                    action = 2
                    continue
                pattern = (pattern + 1) & _MASK32
                continue

            if op == _CATEGORY:
                if ptr >= end:
                    ret = 0
                    action = 2
                    continue
                ch_cat: int = (
                    (data[ptr] & 255) if is_bytes else (ord(text[ptr]) & _MASK32)
                )
                if not _category(code[pattern], ch_cat):
                    ret = 0
                    action = 2
                    continue
                pattern = (pattern + 1) & _MASK32
                ptr = (ptr + 1) & _MASK32
                continue

            if op == _ANY:
                if ptr >= end:
                    ret = 0
                    action = 2
                    continue
                ch_any: int = (
                    (data[ptr] & 255) if is_bytes else (ord(text[ptr]) & _MASK32)
                )
                if ch_any == 10:
                    ret = 0
                    action = 2
                    continue
                ptr = (ptr + 1) & _MASK32
                continue

            if op == _ANY_ALL:
                if ptr >= end:
                    ret = 0
                    action = 2
                    continue
                ptr = (ptr + 1) & _MASK32
                continue

            if (
                op == _IN
                or op == _IN_IGNORE
                or op == _IN_UNI_IGNORE
                or op == _IN_LOC_IGNORE
            ):
                if ptr >= end:
                    ret = 0
                    action = 2
                    continue
                ch_in: int = (
                    (data[ptr] & 255) if is_bytes else (ord(text[ptr]) & _MASK32)
                )
                set_pc_in: int = (pattern + 1) & _MASK32
                if ch_in < 256:
                    base_in: int = charset_base[set_pc_in]
                    if base_in < _UNSET:
                        if charset_flat[base_in + ch_in] == 0:
                            ret = 0
                            action = 2
                            continue
                        pattern = (pattern + code[pattern]) & _MASK32
                        ptr = (ptr + 1) & _MASK32
                        continue
                if not _sre_in_charset(
                    code, charset_flat, charset_base, op, set_pc_in, ch_in
                ):
                    ret = 0
                    action = 2
                    continue
                pattern = (pattern + code[pattern]) & _MASK32
                ptr = (ptr + 1) & _MASK32
                continue

            if op == _LITERAL_IGNORE:
                if ptr >= end:
                    ret = 0
                    action = 2
                    continue
                ch_li: int = (
                    (data[ptr] & 255) if is_bytes else (ord(text[ptr]) & _MASK32)
                )
                if (ascii_tolower(ch_li) & _MASK32) != code[pattern]:
                    ret = 0
                    action = 2
                    continue
                pattern = (pattern + 1) & _MASK32
                ptr = (ptr + 1) & _MASK32
                continue

            if op == _LITERAL_UNI_IGNORE:
                if ptr >= end:
                    ret = 0
                    action = 2
                    continue
                ch_lui: int = (
                    (data[ptr] & 255) if is_bytes else (ord(text[ptr]) & _MASK32)
                )
                if (unicode_tolower(ch_lui) & _MASK32) != code[pattern]:
                    ret = 0
                    action = 2
                    continue
                pattern = (pattern + 1) & _MASK32
                ptr = (ptr + 1) & _MASK32
                continue

            if op == _LITERAL_LOC_IGNORE:
                if ptr >= end:
                    ret = 0
                    action = 2
                    continue
                ch_lli: int = (
                    (data[ptr] & 255) if is_bytes else (ord(text[ptr]) & _MASK32)
                )
                if not _char_loc_ignore(code[pattern], ch_lli):
                    ret = 0
                    action = 2
                    continue
                pattern = (pattern + 1) & _MASK32
                ptr = (ptr + 1) & _MASK32
                continue

            if op == _NOT_LITERAL_IGNORE:
                if ptr >= end:
                    ret = 0
                    action = 2
                    continue
                ch_nli: int = (
                    (data[ptr] & 255) if is_bytes else (ord(text[ptr]) & _MASK32)
                )
                if (ascii_tolower(ch_nli) & _MASK32) == code[pattern]:
                    ret = 0
                    action = 2
                    continue
                pattern = (pattern + 1) & _MASK32
                ptr = (ptr + 1) & _MASK32
                continue

            if op == _NOT_LITERAL_UNI_IGNORE:
                if ptr >= end:
                    ret = 0
                    action = 2
                    continue
                ch_nlui: int = (
                    (data[ptr] & 255) if is_bytes else (ord(text[ptr]) & _MASK32)
                )
                if (unicode_tolower(ch_nlui) & _MASK32) == code[pattern]:
                    ret = 0
                    action = 2
                    continue
                pattern = (pattern + 1) & _MASK32
                ptr = (ptr + 1) & _MASK32
                continue

            if op == _NOT_LITERAL_LOC_IGNORE:
                if ptr >= end:
                    ret = 0
                    action = 2
                    continue
                ch_nlli: int = (
                    (data[ptr] & 255) if is_bytes else (ord(text[ptr]) & _MASK32)
                )
                if _char_loc_ignore(code[pattern], ch_nlli):
                    ret = 0
                    action = 2
                    continue
                pattern = (pattern + 1) & _MASK32
                ptr = (ptr + 1) & _MASK32
                continue

            if op == _JUMP or op == _INFO:
                pattern = (pattern + code[pattern]) & _MASK32
                continue

            if op == _BRANCH:
                while code[pattern] != 0:
                    arm_pc: int = (pattern + 1) & _MASK32
                    arm_ptr: int = ptr
                    arm_ok: bool = True
                    while True:
                        arm_op: int = code[arm_pc]
                        if arm_op == _LITERAL:
                            if arm_ptr >= end:
                                arm_ok = False
                                break
                            ch_br: int = (
                                (data[arm_ptr] & 255)
                                if is_bytes
                                else (ord(text[arm_ptr]) & _MASK32)
                            )
                            if ch_br != code[arm_pc + 1]:
                                arm_ok = False
                                break
                            arm_pc = (arm_pc + 2) & _MASK32
                            arm_ptr = (arm_ptr + 1) & _MASK32
                        elif arm_op == _IN:
                            if arm_ptr >= end:
                                arm_ok = False
                                break
                            ch_br_in: int = (
                                (data[arm_ptr] & 255)
                                if is_bytes
                                else (ord(text[arm_ptr]) & _MASK32)
                            )
                            set_pc_br: int = (arm_pc + 2) & _MASK32
                            if ch_br_in < 256:
                                base_br: int = charset_base[set_pc_br]
                                if base_br < _UNSET:
                                    if charset_flat[base_br + ch_br_in] == 0:
                                        arm_ok = False
                                        break
                                elif not _sre_in_charset(
                                    code,
                                    charset_flat,
                                    charset_base,
                                    _IN,
                                    set_pc_br,
                                    ch_br_in,
                                ):
                                    arm_ok = False
                                    break
                            elif not _sre_in_charset(
                                code,
                                charset_flat,
                                charset_base,
                                _IN,
                                set_pc_br,
                                ch_br_in,
                            ):
                                arm_ok = False
                                break
                            arm_pc = (arm_pc + 1 + code[arm_pc + 1]) & _MASK32
                            arm_ptr = (arm_ptr + 1) & _MASK32
                        else:
                            break
                    if not arm_ok:
                        pattern = (pattern + code[pattern]) & _MASK32
                        continue
                    if code[arm_pc] == _SUCCESS and not (
                        ctx_toplevel != 0
                        and (
                            (match_all and arm_ptr != end)
                            or (must_advance and arm_ptr == start)
                        )
                    ):
                        curr_ptr = arm_ptr
                        ret = 1
                        action = 2
                        break
                    if code[pattern + code[pattern]] == 0:
                        ptr = arm_ptr
                        pattern = arm_pc
                        action = 1
                        break
                    ctx_num_marks = num_marks
                    ctx_lastindex = lastindex
                    if save_marks != 0:
                        mark_sp = (
                            _sre_mark_push(mark_stack, marks, mark_sp, ctx_num_marks)
                            & _MASK32
                        )
                    curr_ptr = arm_ptr
                    next_jump = _JUMP_BRANCH
                    next_pattern = arm_pc
                    next_toplevel = ctx_toplevel
                    action = 3
                    break
                if action == 2 or action == 3:
                    continue
                if code[pattern] != 0:
                    continue
                ret = 0
                action = 2
                continue

            if op == _REPEAT_ONE:
                min_rep: int = code[pattern + 1]
                if min_rep > end - ptr:
                    ret = 0
                    action = 2
                    continue
                curr_ptr = ptr
                ctx_count = (
                    _sre_count(
                        code,
                        charset_flat,
                        charset_base,
                        is_bytes,
                        text,
                        data,
                        ptr,
                        end,
                        (pattern + 3) & _MASK32,
                        code[pattern + 2],
                    )
                    & _MASK32
                )
                ptr = (ptr + ctx_count) & _MASK32
                if ctx_count < min_rep:
                    ret = 0
                    action = 2
                    continue
                following: int = (pattern + code[pattern]) & _MASK32
                if (
                    code[following] == _SUCCESS
                    and ptr == end
                    and not (ctx_toplevel != 0 and must_advance and ptr == start)
                ):
                    curr_ptr = ptr
                    ret = 1
                    action = 2
                    continue
                ctx_num_marks = num_marks
                ctx_lastindex = lastindex
                if save_marks != 0:
                    mark_sp = (
                        _sre_mark_push(mark_stack, marks, mark_sp, ctx_num_marks)
                        & _MASK32
                    )
                if code[following] == _LITERAL:
                    ctx_u = code[following + 1]
                    matched_r0: bool = False
                    while True:
                        if (
                            ptr < end
                            and (
                                (data[ptr] & 255)
                                if is_bytes
                                else (ord(text[ptr]) & _MASK32)
                            )
                            == ctx_u
                        ):
                            matched_r0 = True
                            break
                        if ctx_count <= min_rep:
                            break
                        ptr = (ptr - 1) & _MASK32
                        ctx_count = (ctx_count - 1) & _MASK32
                    if not matched_r0:
                        if save_marks != 0 and ctx_num_marks > 0:
                            mark_sp = (mark_sp - ctx_num_marks) & _MASK32
                        ret = 0
                        action = 2
                        continue
                    curr_ptr = ptr
                    next_jump = _JUMP_REPEAT_ONE_1
                    next_pattern = following
                    next_toplevel = ctx_toplevel
                    action = 3
                    continue
                curr_ptr = ptr
                next_jump = _JUMP_REPEAT_ONE_2
                next_pattern = following
                next_toplevel = ctx_toplevel
                action = 3
                continue

            if op == _MIN_REPEAT_ONE:
                min_mrep: int = code[pattern + 1]
                if min_mrep > end - ptr:
                    ret = 0
                    action = 2
                    continue
                curr_ptr = ptr
                if min_mrep == 0:
                    ctx_count = 0
                else:
                    ctx_count = (
                        _sre_count(
                            code,
                            charset_flat,
                            charset_base,
                            is_bytes,
                            text,
                            data,
                            ptr,
                            end,
                            (pattern + 3) & _MASK32,
                            min_mrep,
                        )
                        & _MASK32
                    )
                    if ctx_count < min_mrep:
                        ret = 0
                        action = 2
                        continue
                    ptr = (ptr + ctx_count) & _MASK32
                mfollowing: int = (pattern + code[pattern]) & _MASK32
                if code[mfollowing] == _SUCCESS and not (
                    ctx_toplevel != 0
                    and ((match_all and ptr != end) or (must_advance and ptr == start))
                ):
                    curr_ptr = ptr
                    ret = 1
                    action = 2
                    continue
                ctx_num_marks = num_marks
                ctx_lastindex = lastindex
                if save_marks != 0:
                    mark_sp = (
                        _sre_mark_push(mark_stack, marks, mark_sp, ctx_num_marks)
                        & _MASK32
                    )
                max_mrep: int = code[pattern + 2]
                if max_mrep == MAXREPEAT or ctx_count <= max_mrep:
                    curr_ptr = ptr
                    next_jump = _JUMP_MIN_REPEAT_ONE
                    next_pattern = mfollowing
                    next_toplevel = ctx_toplevel
                    action = 3
                    continue
                if save_marks != 0 and ctx_num_marks > 0:
                    mark_sp = (mark_sp - ctx_num_marks) & _MASK32
                ret = 0
                action = 2
                continue

            if op == _POSSESSIVE_REPEAT_ONE:
                min_prep: int = code[pattern + 1]
                if ptr + min_prep > end:
                    ret = 0
                    action = 2
                    continue
                curr_ptr = ptr
                ctx_count = (
                    _sre_count(
                        code,
                        charset_flat,
                        charset_base,
                        is_bytes,
                        text,
                        data,
                        ptr,
                        end,
                        (pattern + 3) & _MASK32,
                        code[pattern + 2],
                    )
                    & _MASK32
                )
                ptr = (ptr + ctx_count) & _MASK32
                if ctx_count < min_prep:
                    ret = 0
                    action = 2
                    continue
                pattern = (pattern + code[pattern]) & _MASK32
                if (
                    code[pattern] == _SUCCESS
                    and ptr == end
                    and not (ctx_toplevel != 0 and must_advance and ptr == start)
                ):
                    curr_ptr = ptr
                    ret = 1
                    action = 2
                    continue
                continue

            if op == _REPEAT:
                while len(rep_stack) < rep_sp + 4:
                    rep_stack.append(0)
                rep_idx: int = rep_sp
                rep_sp = (rep_sp + 4) & _MASK32
                rep_stack[rep_idx] = 0
                rep_stack[rep_idx + 1] = pattern
                rep_stack[rep_idx + 2] = repeat_head
                rep_stack[rep_idx + 3] = _UNSET
                ctx_u = rep_idx
                repeat_head = rep_idx
                save_marks = (save_marks + 1) & _MASK32
                curr_ptr = ptr
                next_jump = _JUMP_REPEAT
                next_pattern = (pattern + code[pattern]) & _MASK32
                next_toplevel = ctx_toplevel
                action = 3
                continue

            if op == _MAX_UNTIL:
                ctx_u = repeat_head
                if ctx_u == _UNSET:
                    return -2
                curr_ptr = ptr
                ctx_count = rep_stack[ctx_u]
                rep_pat: int = rep_stack[ctx_u + 1]
                if ctx_count < code[rep_pat + 1]:
                    rep_stack[ctx_u] = (ctx_count + 1) & _MASK32
                    next_jump = _JUMP_MAX_UNTIL_1
                    next_pattern = (rep_pat + 3) & _MASK32
                    next_toplevel = ctx_toplevel
                    action = 3
                    continue
                max_u: int = code[rep_pat + 2]
                if (ctx_count < max_u or max_u == MAXREPEAT) and curr_ptr != rep_stack[
                    ctx_u + 3
                ]:
                    rep_stack[ctx_u] = (ctx_count + 1) & _MASK32
                    ctx_num_marks = num_marks
                    ctx_lastindex = lastindex
                    mark_sp = (
                        _sre_mark_push(mark_stack, marks, mark_sp, ctx_num_marks)
                        & _MASK32
                    )
                    ctx_saved_last_ptr = rep_stack[ctx_u + 3]
                    rep_stack[ctx_u + 3] = curr_ptr
                    next_jump = _JUMP_MAX_UNTIL_2
                    next_pattern = (rep_pat + 3) & _MASK32
                    next_toplevel = ctx_toplevel
                    action = 3
                    continue
                repeat_head = rep_stack[ctx_u + 2]
                save_marks = (save_marks - 1) & _MASK32
                next_jump = _JUMP_MAX_UNTIL_3
                next_pattern = pattern
                next_toplevel = ctx_toplevel
                action = 3
                continue

            if op == _MIN_UNTIL:
                ctx_u = repeat_head
                if ctx_u == _UNSET:
                    return -2
                curr_ptr = ptr
                ctx_count = rep_stack[ctx_u]
                rep_pat_min: int = rep_stack[ctx_u + 1]
                if ctx_count < code[rep_pat_min + 1]:
                    rep_stack[ctx_u] = (ctx_count + 1) & _MASK32
                    next_jump = _JUMP_MIN_UNTIL_1
                    next_pattern = (rep_pat_min + 3) & _MASK32
                    next_toplevel = ctx_toplevel
                    action = 3
                    continue
                repeat_head = rep_stack[ctx_u + 2]
                save_marks = (save_marks - 1) & _MASK32
                ctx_num_marks = num_marks
                ctx_lastindex = lastindex
                if save_marks != 0:
                    mark_sp = (
                        _sre_mark_push(mark_stack, marks, mark_sp, ctx_num_marks)
                        & _MASK32
                    )
                next_jump = _JUMP_MIN_UNTIL_2
                next_pattern = pattern
                next_toplevel = ctx_toplevel
                action = 3
                continue

            if op == _POSSESSIVE_REPEAT:
                curr_ptr = ptr
                save_marks = (save_marks + 1) & _MASK32
                ctx_count = 0
                if ctx_count < code[pattern + 1]:
                    next_jump = _JUMP_POSS_REPEAT_1
                    next_pattern = (pattern + 3) & _MASK32
                    next_toplevel = 0
                    action = 3
                    continue
                ptr = _UNSET
                max_pr: int = code[pattern + 2]
                if (ctx_count < max_pr or max_pr == MAXREPEAT) and curr_ptr != ptr:
                    ctx_num_marks = num_marks
                    ctx_lastindex = lastindex
                    mark_sp = (
                        _sre_mark_push(mark_stack, marks, mark_sp, ctx_num_marks)
                        & _MASK32
                    )
                    ptr = curr_ptr
                    next_jump = _JUMP_POSS_REPEAT_2
                    next_pattern = (pattern + 3) & _MASK32
                    next_toplevel = 0
                    action = 3
                    continue
                save_marks = (save_marks - 1) & _MASK32
                pattern = (pattern + code[pattern] + 1) & _MASK32
                ptr = curr_ptr
                continue

            if op == _ATOMIC_GROUP:
                curr_ptr = ptr
                next_jump = _JUMP_ATOMIC_GROUP
                next_pattern = (pattern + 1) & _MASK32
                next_toplevel = 0
                action = 3
                continue

            if op == _GROUPREF:
                groupref: int = (code[pattern] << 1) & _MASK32
                if groupref + 1 >= num_marks:
                    ret = 0
                    action = 2
                    continue
                gp: int = marks[groupref]
                ge: int = marks[groupref + 1]
                if gp == _UNSET or ge == _UNSET or ge < gp:
                    ret = 0
                    action = 2
                    continue
                while gp < ge:
                    if ptr >= end:
                        ret = 0
                        action = 2
                        break
                    ch_g1: int = (
                        (data[ptr] & 255) if is_bytes else (ord(text[ptr]) & _MASK32)
                    )
                    ch_g2: int = (
                        (data[gp] & 255) if is_bytes else (ord(text[gp]) & _MASK32)
                    )
                    if ch_g1 != ch_g2:
                        ret = 0
                        action = 2
                        break
                    gp = (gp + 1) & _MASK32
                    ptr = (ptr + 1) & _MASK32
                if action == 2:
                    continue
                pattern = (pattern + 1) & _MASK32
                continue

            if op == _GROUPREF_IGNORE or op == _GROUPREF_LOC_IGNORE:
                groupref_i: int = (code[pattern] << 1) & _MASK32
                if groupref_i + 1 >= num_marks:
                    ret = 0
                    action = 2
                    continue
                gpi: int = marks[groupref_i]
                gei: int = marks[groupref_i + 1]
                if gpi == _UNSET or gei == _UNSET or gei < gpi:
                    ret = 0
                    action = 2
                    continue
                while gpi < gei:
                    if ptr >= end:
                        ret = 0
                        action = 2
                        break
                    ch_gi1: int = (
                        (data[ptr] & 255) if is_bytes else (ord(text[ptr]) & _MASK32)
                    )
                    ch_gi2: int = (
                        (data[gpi] & 255) if is_bytes else (ord(text[gpi]) & _MASK32)
                    )
                    if ascii_tolower(ch_gi1) != ascii_tolower(ch_gi2):
                        ret = 0
                        action = 2
                        break
                    gpi = (gpi + 1) & _MASK32
                    ptr = (ptr + 1) & _MASK32
                if action == 2:
                    continue
                pattern = (pattern + 1) & _MASK32
                continue

            if op == _GROUPREF_UNI_IGNORE:
                groupref_u: int = (code[pattern] << 1) & _MASK32
                if groupref_u + 1 >= num_marks:
                    ret = 0
                    action = 2
                    continue
                gpu: int = marks[groupref_u]
                geu: int = marks[groupref_u + 1]
                if gpu == _UNSET or geu == _UNSET or geu < gpu:
                    ret = 0
                    action = 2
                    continue
                while gpu < geu:
                    if ptr >= end:
                        ret = 0
                        action = 2
                        break
                    ch_gu1: int = (
                        (data[ptr] & 255) if is_bytes else (ord(text[ptr]) & _MASK32)
                    )
                    ch_gu2: int = (
                        (data[gpu] & 255) if is_bytes else (ord(text[gpu]) & _MASK32)
                    )
                    if unicode_tolower(ch_gu1) != unicode_tolower(ch_gu2):
                        ret = 0
                        action = 2
                        break
                    gpu = (gpu + 1) & _MASK32
                    ptr = (ptr + 1) & _MASK32
                if action == 2:
                    continue
                pattern = (pattern + 1) & _MASK32
                continue

            if op == _GROUPREF_EXISTS:
                groupref_e: int = (code[pattern] << 1) & _MASK32
                if groupref_e + 1 >= num_marks:
                    pattern = (pattern + code[pattern + 1]) & _MASK32
                    continue
                gpe: int = marks[groupref_e]
                gee: int = marks[groupref_e + 1]
                if gpe == _UNSET or gee == _UNSET or gee < gpe:
                    pattern = (pattern + code[pattern + 1]) & _MASK32
                    continue
                pattern = (pattern + 2) & _MASK32
                continue

            if op == _ASSERT:
                back: int = code[pattern + 1]
                if ptr < back:
                    ret = 0
                    action = 2
                    continue
                curr_ptr = (ptr - back) & _MASK32
                next_jump = _JUMP_ASSERT
                next_pattern = (pattern + 2) & _MASK32
                next_toplevel = 0
                action = 3
                continue

            if op == _ASSERT_NOT:
                back_not: int = code[pattern + 1]
                if ptr >= back_not:
                    curr_ptr = (ptr - back_not) & _MASK32
                    ctx_num_marks = num_marks
                    ctx_lastindex = lastindex
                    if save_marks != 0:
                        mark_sp = (
                            _sre_mark_push(mark_stack, marks, mark_sp, ctx_num_marks)
                            & _MASK32
                        )
                    next_jump = _JUMP_ASSERT_NOT
                    next_pattern = (pattern + 2) & _MASK32
                    next_toplevel = 0
                    action = 3
                    continue
                pattern = (pattern + code[pattern]) & _MASK32
                continue

            if op == _FAILURE:
                ret = 0
                action = 2
                continue

            return -1

        if action == 3:
            # DO_JUMPX: push caller frame onto ctx_stack and enter child frame
            while len(ctx_stack) < ctx_sp + 9:
                ctx_stack.append(0)
            ctx_stack[ctx_sp] = pattern
            ctx_stack[ctx_sp + 1] = ptr
            ctx_stack[ctx_sp + 2] = ctx_count
            ctx_stack[ctx_sp + 3] = ctx_u
            ctx_stack[ctx_sp + 4] = ctx_num_marks
            ctx_stack[ctx_sp + 5] = ctx_lastindex
            ctx_stack[ctx_sp + 6] = ctx_toplevel
            ctx_stack[ctx_sp + 7] = ctx_jump
            ctx_stack[ctx_sp + 8] = ctx_saved_last_ptr
            ctx_sp = (ctx_sp + 9) & _MASK32

            pattern = next_pattern
            ctx_toplevel = next_toplevel
            ctx_jump = next_jump
            ctx_count = 0
            ctx_u = 0
            ctx_num_marks = 0
            ctx_lastindex = 0
            ctx_saved_last_ptr = _UNSET
            action = 0
            continue

        # action == 2: exit (pop frame and resume at ctx_jump)
        jump: int = ctx_jump
        if ctx_sp == 0:
            state[_ST_PTR] = curr_ptr
            state[_ST_NUM_MARKS] = num_marks
            state[_ST_LASTINDEX] = lastindex
            state[_ST_SAVE_MARKS] = save_marks
            state[_ST_REPEAT_HEAD] = repeat_head
            return ret

        ctx_sp = (ctx_sp - 9) & _MASK32
        pattern = ctx_stack[ctx_sp]
        ptr = ctx_stack[ctx_sp + 1]
        ctx_count = ctx_stack[ctx_sp + 2]
        ctx_u = ctx_stack[ctx_sp + 3]
        ctx_num_marks = ctx_stack[ctx_sp + 4]
        ctx_lastindex = ctx_stack[ctx_sp + 5]
        ctx_toplevel = ctx_stack[ctx_sp + 6]
        ctx_jump = ctx_stack[ctx_sp + 7]
        ctx_saved_last_ptr = ctx_stack[ctx_sp + 8]

        if jump == _JUMP_REPEAT_ONE_1:
            if ret != 0:
                if save_marks != 0 and ctx_num_marks > 0:
                    mark_sp = (mark_sp - ctx_num_marks) & _MASK32
                action = 2
                continue
            if save_marks != 0:
                _sre_mark_pop_keep(mark_stack, marks, mark_sp, ctx_num_marks)
            num_marks = ctx_num_marks
            lastindex = ctx_lastindex
            min_r1: int = code[pattern + 1]
            matched_r1: bool = False
            while ctx_count > min_r1:
                ptr = (ptr - 1) & _MASK32
                ctx_count = (ctx_count - 1) & _MASK32
                if (
                    ptr < end
                    and ((data[ptr] & 255) if is_bytes else (ord(text[ptr]) & _MASK32))
                    == ctx_u
                ):
                    matched_r1 = True
                    break
            if matched_r1:
                curr_ptr = ptr
                next_jump = _JUMP_REPEAT_ONE_1
                next_pattern = (pattern + code[pattern]) & _MASK32
                next_toplevel = ctx_toplevel
                action = 3
                continue
            if save_marks != 0 and ctx_num_marks > 0:
                mark_sp = (mark_sp - ctx_num_marks) & _MASK32
            ret = 0
            action = 2
            continue

        if jump == _JUMP_REPEAT_ONE_2:
            if ret != 0:
                if save_marks != 0 and ctx_num_marks > 0:
                    mark_sp = (mark_sp - ctx_num_marks) & _MASK32
                action = 2
                continue
            if save_marks != 0:
                _sre_mark_pop_keep(mark_stack, marks, mark_sp, ctx_num_marks)
            num_marks = ctx_num_marks
            lastindex = ctx_lastindex
            if ctx_count > code[pattern + 1]:
                ptr = (ptr - 1) & _MASK32
                ctx_count = (ctx_count - 1) & _MASK32
                curr_ptr = ptr
                next_jump = _JUMP_REPEAT_ONE_2
                next_pattern = (pattern + code[pattern]) & _MASK32
                next_toplevel = ctx_toplevel
                action = 3
                continue
            if save_marks != 0 and ctx_num_marks > 0:
                mark_sp = (mark_sp - ctx_num_marks) & _MASK32
            ret = 0
            action = 2
            continue

        if jump == _JUMP_MIN_REPEAT_ONE:
            if ret != 0:
                if save_marks != 0 and ctx_num_marks > 0:
                    mark_sp = (mark_sp - ctx_num_marks) & _MASK32
                action = 2
                continue
            if save_marks != 0:
                _sre_mark_pop_keep(mark_stack, marks, mark_sp, ctx_num_marks)
            num_marks = ctx_num_marks
            lastindex = ctx_lastindex
            curr_ptr = ptr
            step_cnt: int = (
                _sre_count(
                    code,
                    charset_flat,
                    charset_base,
                    is_bytes,
                    text,
                    data,
                    ptr,
                    end,
                    (pattern + 3) & _MASK32,
                    1,
                )
                & _MASK32
            )
            if step_cnt != 0:
                ptr = (ptr + 1) & _MASK32
                ctx_count = (ctx_count + 1) & _MASK32
                max_mr: int = code[pattern + 2]
                if max_mr == MAXREPEAT or ctx_count <= max_mr:
                    curr_ptr = ptr
                    next_jump = _JUMP_MIN_REPEAT_ONE
                    next_pattern = (pattern + code[pattern]) & _MASK32
                    next_toplevel = ctx_toplevel
                    action = 3
                    continue
            if save_marks != 0 and ctx_num_marks > 0:
                mark_sp = (mark_sp - ctx_num_marks) & _MASK32
            ret = 0
            action = 2
            continue

        if jump == _JUMP_BRANCH:
            if ret != 0:
                if save_marks != 0 and ctx_num_marks > 0:
                    mark_sp = (mark_sp - ctx_num_marks) & _MASK32
                action = 2
                continue
            if save_marks != 0:
                _sre_mark_pop_keep(mark_stack, marks, mark_sp, ctx_num_marks)
            num_marks = ctx_num_marks
            lastindex = ctx_lastindex
            pattern = (pattern + code[pattern]) & _MASK32
            while code[pattern] != 0:
                arm_pc2: int = (pattern + 1) & _MASK32
                arm_ptr2: int = ptr
                arm_ok2: bool = True
                while True:
                    arm_op2: int = code[arm_pc2]
                    if arm_op2 == _LITERAL:
                        if arm_ptr2 >= end:
                            arm_ok2 = False
                            break
                        ch_br2: int = (
                            (data[arm_ptr2] & 255)
                            if is_bytes
                            else (ord(text[arm_ptr2]) & _MASK32)
                        )
                        if ch_br2 != code[arm_pc2 + 1]:
                            arm_ok2 = False
                            break
                        arm_pc2 = (arm_pc2 + 2) & _MASK32
                        arm_ptr2 = (arm_ptr2 + 1) & _MASK32
                    elif arm_op2 == _IN:
                        if arm_ptr2 >= end:
                            arm_ok2 = False
                            break
                        ch_br2_in: int = (
                            (data[arm_ptr2] & 255)
                            if is_bytes
                            else (ord(text[arm_ptr2]) & _MASK32)
                        )
                        set_pc_br2: int = (arm_pc2 + 2) & _MASK32
                        if ch_br2_in < 256:
                            base_br2: int = charset_base[set_pc_br2]
                            if base_br2 < _UNSET:
                                if charset_flat[base_br2 + ch_br2_in] == 0:
                                    arm_ok2 = False
                                    break
                            elif not _sre_in_charset(
                                code,
                                charset_flat,
                                charset_base,
                                _IN,
                                set_pc_br2,
                                ch_br2_in,
                            ):
                                arm_ok2 = False
                                break
                        elif not _sre_in_charset(
                            code,
                            charset_flat,
                            charset_base,
                            _IN,
                            set_pc_br2,
                            ch_br2_in,
                        ):
                            arm_ok2 = False
                            break
                        arm_pc2 = (arm_pc2 + 1 + code[arm_pc2 + 1]) & _MASK32
                        arm_ptr2 = (arm_ptr2 + 1) & _MASK32
                    else:
                        break
                if not arm_ok2:
                    pattern = (pattern + code[pattern]) & _MASK32
                    continue
                if code[arm_pc2] == _SUCCESS and not (
                    ctx_toplevel != 0
                    and (
                        (match_all and arm_ptr2 != end)
                        or (must_advance and arm_ptr2 == start)
                    )
                ):
                    if save_marks != 0 and ctx_num_marks > 0:
                        mark_sp = (mark_sp - ctx_num_marks) & _MASK32
                    curr_ptr = arm_ptr2
                    ret = 1
                    action = 2
                    break
                if code[pattern + code[pattern]] == 0:
                    if save_marks != 0 and ctx_num_marks > 0:
                        mark_sp = (mark_sp - ctx_num_marks) & _MASK32
                    ptr = arm_ptr2
                    pattern = arm_pc2
                    action = 1
                    break
                curr_ptr = arm_ptr2
                next_jump = _JUMP_BRANCH
                next_pattern = arm_pc2
                next_toplevel = ctx_toplevel
                action = 3
                break
            if action == 1 or action == 2 or action == 3:
                continue
            if save_marks != 0 and ctx_num_marks > 0:
                mark_sp = (mark_sp - ctx_num_marks) & _MASK32
            ret = 0
            action = 2
            continue

        if jump == _JUMP_REPEAT:
            repeat_head = rep_stack[ctx_u + 2]
            save_marks = (save_marks - 1) & _MASK32
            rep_sp = (rep_sp - 4) & _MASK32
            action = 2
            continue

        if jump == _JUMP_MAX_UNTIL_1:
            if ret != 0:
                action = 2
                continue
            rep_stack[ctx_u] = ctx_count
            curr_ptr = ptr
            ret = 0
            action = 2
            continue

        if jump == _JUMP_MAX_UNTIL_2:
            rep_stack[ctx_u + 3] = ctx_saved_last_ptr
            if ret != 0:
                if ctx_num_marks > 0:
                    mark_sp = (mark_sp - ctx_num_marks) & _MASK32
                action = 2
                continue
            mark_sp = _sre_mark_pop(mark_stack, marks, mark_sp, ctx_num_marks) & _MASK32
            num_marks = ctx_num_marks
            lastindex = ctx_lastindex
            rep_stack[ctx_u] = ctx_count
            curr_ptr = ptr
            repeat_head = rep_stack[ctx_u + 2]
            save_marks = (save_marks - 1) & _MASK32
            next_jump = _JUMP_MAX_UNTIL_3
            next_pattern = pattern
            next_toplevel = ctx_toplevel
            action = 3
            continue

        if jump == _JUMP_MAX_UNTIL_3:
            repeat_head = ctx_u
            save_marks = (save_marks + 1) & _MASK32
            if ret == 0:
                curr_ptr = ptr
            action = 2
            continue

        if jump == _JUMP_MIN_UNTIL_1:
            if ret != 0:
                action = 2
                continue
            rep_stack[ctx_u] = ctx_count
            curr_ptr = ptr
            ret = 0
            action = 2
            continue

        if jump == _JUMP_MIN_UNTIL_2:
            pushed_min: bool = save_marks != 0
            repeat_head = ctx_u
            save_marks = (save_marks + 1) & _MASK32
            if ret != 0:
                if pushed_min and ctx_num_marks > 0:
                    mark_sp = (mark_sp - ctx_num_marks) & _MASK32
                action = 2
                continue
            if pushed_min:
                mark_sp = (
                    _sre_mark_pop(mark_stack, marks, mark_sp, ctx_num_marks) & _MASK32
                )
            num_marks = ctx_num_marks
            lastindex = ctx_lastindex
            curr_ptr = ptr
            rep_pat2: int = rep_stack[ctx_u + 1]
            max_mu2: int = code[rep_pat2 + 2]
            if (ctx_count >= max_mu2 and max_mu2 != MAXREPEAT) or curr_ptr == rep_stack[
                ctx_u + 3
            ]:
                ret = 0
                action = 2
                continue
            rep_stack[ctx_u] = (ctx_count + 1) & _MASK32
            ctx_saved_last_ptr = rep_stack[ctx_u + 3]
            rep_stack[ctx_u + 3] = curr_ptr
            next_jump = _JUMP_MIN_UNTIL_3
            next_pattern = (rep_pat2 + 3) & _MASK32
            next_toplevel = ctx_toplevel
            action = 3
            continue

        if jump == _JUMP_MIN_UNTIL_3:
            rep_stack[ctx_u + 3] = ctx_saved_last_ptr
            if ret != 0:
                action = 2
                continue
            rep_stack[ctx_u] = ctx_count
            curr_ptr = ptr
            ret = 0
            action = 2
            continue

        if jump == _JUMP_POSS_REPEAT_1:
            if ret != 0:
                ctx_count = (ctx_count + 1) & _MASK32
                if ctx_count < code[pattern + 1]:
                    next_jump = _JUMP_POSS_REPEAT_1
                    next_pattern = (pattern + 3) & _MASK32
                    next_toplevel = 0
                    action = 3
                    continue
                ptr = _UNSET
                max_pr1: int = code[pattern + 2]
                if (ctx_count < max_pr1 or max_pr1 == MAXREPEAT) and curr_ptr != ptr:
                    ctx_num_marks = num_marks
                    ctx_lastindex = lastindex
                    mark_sp = (
                        _sre_mark_push(mark_stack, marks, mark_sp, ctx_num_marks)
                        & _MASK32
                    )
                    ptr = curr_ptr
                    next_jump = _JUMP_POSS_REPEAT_2
                    next_pattern = (pattern + 3) & _MASK32
                    next_toplevel = 0
                    action = 3
                    continue
                save_marks = (save_marks - 1) & _MASK32
                pattern = (pattern + code[pattern] + 1) & _MASK32
                ptr = curr_ptr
                action = 1
                continue
            curr_ptr = ptr
            save_marks = (save_marks - 1) & _MASK32
            ret = 0
            action = 2
            continue

        if jump == _JUMP_POSS_REPEAT_2:
            if ret != 0:
                if ctx_num_marks > 0:
                    mark_sp = (mark_sp - ctx_num_marks) & _MASK32
                ctx_count = (ctx_count + 1) & _MASK32
                max_pr2: int = code[pattern + 2]
                if (ctx_count < max_pr2 or max_pr2 == MAXREPEAT) and curr_ptr != ptr:
                    ctx_num_marks = num_marks
                    ctx_lastindex = lastindex
                    mark_sp = (
                        _sre_mark_push(mark_stack, marks, mark_sp, ctx_num_marks)
                        & _MASK32
                    )
                    ptr = curr_ptr
                    next_jump = _JUMP_POSS_REPEAT_2
                    next_pattern = (pattern + 3) & _MASK32
                    next_toplevel = 0
                    action = 3
                    continue
            else:
                mark_sp = (
                    _sre_mark_pop(mark_stack, marks, mark_sp, ctx_num_marks) & _MASK32
                )
                num_marks = ctx_num_marks
                lastindex = ctx_lastindex
                curr_ptr = ptr
            save_marks = (save_marks - 1) & _MASK32
            pattern = (pattern + code[pattern] + 1) & _MASK32
            ptr = curr_ptr
            action = 1
            continue

        if jump == _JUMP_ATOMIC_GROUP:
            if ret == 0:
                curr_ptr = ptr
                action = 2
                continue
            pattern = (pattern + code[pattern]) & _MASK32
            ptr = curr_ptr
            action = 1
            continue

        if jump == _JUMP_ASSERT:
            if ret == 0:
                action = 2
                continue
            pattern = (pattern + code[pattern]) & _MASK32
            action = 1
            continue

        if jump == _JUMP_ASSERT_NOT:
            if ret != 0:
                if save_marks != 0 and ctx_num_marks > 0:
                    mark_sp = (mark_sp - ctx_num_marks) & _MASK32
                ret = 0
                action = 2
                continue
            if save_marks != 0:
                mark_sp = (
                    _sre_mark_pop(mark_stack, marks, mark_sp, ctx_num_marks) & _MASK32
                )
            num_marks = ctx_num_marks
            lastindex = ctx_lastindex
            pattern = (pattern + code[pattern]) & _MASK32
            action = 1
            continue


@mule_types.optimize(safety=0)
def _sre_search(
    code: list[mule_types.uint32],
    charset_flat: list[mule_types.uint32],
    charset_base: list[mule_types.uint32],
    p_info: list[mule_types.uint32],
    prefix_str: str,
    prefix_bytes: bytes,
    is_bytes: bool,
    text: str,
    data: bytes,
    marks: list[mule_types.uint32],
    mark_stack: list[mule_types.uint32],
    ctx_stack: list[mule_types.uint32],
    rep_stack: list[mule_types.uint32],
    state: list[mule_types.uint32],
) -> int:
    """Port of SRE(search) in Modules/_sre/sre_lib.h."""
    ptr: int = state[_ST_START]
    end: int = state[_ST_END]
    if ptr > end:
        return 0
    min_width: int = p_info[_PI_MIN_WIDTH]
    if min_width > 0 and (end - ptr) < min_width:
        return 0

    pattern_pc: int = p_info[_PI_PATTERN_PC]
    flags: int = p_info[_PI_INFO_FLAGS]
    prefix_len: int = p_info[_PI_PREFIX_LEN]
    status: int = 0

    if prefix_len >= 1:
        if prefix_len > end - ptr:
            return 0
        prefix_skip: int = p_info[_PI_PREFIX_SKIP]
        is_literal: bool = (flags & _SRE_INFO_LITERAL) != 0
        match_pc: int = (pattern_pc + (prefix_skip << 1)) & _MASK32
        scan_end: int = (
            ((end - (min_width - prefix_len)) & _MASK32)
            if min_width > prefix_len
            else end
        )
        state[_ST_MUST_ADVANCE] = 0
        if not is_bytes:
            while ptr < scan_end:
                found: int = text.find(prefix_str, ptr, scan_end)
                if found < 0:
                    return 0
                pos: int = found & _MASK32
                state[_ST_START] = pos
                state[_ST_PTR] = (pos + prefix_skip) & _MASK32
                if is_literal:
                    return 1
                state[_ST_NUM_MARKS] = 0
                state[_ST_LASTINDEX] = 0
                status = _sre_match(
                    code,
                    charset_flat,
                    charset_base,
                    is_bytes,
                    text,
                    data,
                    marks,
                    mark_stack,
                    ctx_stack,
                    rep_stack,
                    state,
                    match_pc,
                    0,
                )
                if status != 0:
                    return status
                ptr = (pos + 1) & _MASK32
            return 0
        else:
            if p_info[_PI_PREFIX_BYTES_VALID] == 0:
                return 0
            while ptr < scan_end:
                found_b: int = data.find(prefix_bytes, ptr, scan_end)
                if found_b < 0:
                    return 0
                pos_b: int = found_b & _MASK32
                state[_ST_START] = pos_b
                state[_ST_PTR] = (pos_b + prefix_skip) & _MASK32
                if is_literal:
                    return 1
                state[_ST_NUM_MARKS] = 0
                state[_ST_LASTINDEX] = 0
                status = _sre_match(
                    code,
                    charset_flat,
                    charset_base,
                    is_bytes,
                    text,
                    data,
                    marks,
                    mark_stack,
                    ctx_stack,
                    rep_stack,
                    state,
                    match_pc,
                    0,
                )
                if status != 0:
                    return status
                ptr = (pos_b + 1) & _MASK32
            return 0

    charset_pc: int = p_info[_PI_CHARSET_PC]
    if charset_pc > 0:
        info_base: int = charset_base[charset_pc]
        cs_limit: int = ((end - (min_width - 1)) & _MASK32) if min_width > 1 else end
        state[_ST_MUST_ADVANCE] = 0
        if not is_bytes:
            while True:
                while ptr < cs_limit:
                    ch_cs: int = ord(text[ptr]) & _MASK32
                    if ch_cs < 256:
                        if charset_flat[info_base + ch_cs] != 0:
                            break
                    elif _sre_charset(code, charset_pc, ch_cs):
                        break
                    ptr = (ptr + 1) & _MASK32
                if ptr >= cs_limit:
                    return 0
                state[_ST_START] = ptr
                state[_ST_PTR] = ptr
                state[_ST_NUM_MARKS] = 0
                state[_ST_LASTINDEX] = 0
                status = _sre_match(
                    code,
                    charset_flat,
                    charset_base,
                    is_bytes,
                    text,
                    data,
                    marks,
                    mark_stack,
                    ctx_stack,
                    rep_stack,
                    state,
                    pattern_pc,
                    0,
                )
                if status != 0:
                    return status
                ptr = (ptr + 1) & _MASK32
        else:
            while True:
                while ptr < cs_limit:
                    ch_csb: int = data[ptr] & 255
                    if charset_flat[info_base + ch_csb] != 0:
                        break
                    ptr = (ptr + 1) & _MASK32
                if ptr >= cs_limit:
                    return 0
                state[_ST_START] = ptr
                state[_ST_PTR] = ptr
                state[_ST_NUM_MARKS] = 0
                state[_ST_LASTINDEX] = 0
                status = _sre_match(
                    code,
                    charset_flat,
                    charset_base,
                    is_bytes,
                    text,
                    data,
                    marks,
                    mark_stack,
                    ctx_stack,
                    rep_stack,
                    state,
                    pattern_pc,
                    0,
                )
                if status != 0:
                    return status
                ptr = (ptr + 1) & _MASK32

    adj_end: int = end
    if min_width > 1:
        adj_end = (end - (min_width - 1)) & _MASK32
        if adj_end < ptr:
            adj_end = ptr

    state[_ST_START] = ptr
    state[_ST_PTR] = ptr
    status = _sre_match(
        code,
        charset_flat,
        charset_base,
        is_bytes,
        text,
        data,
        marks,
        mark_stack,
        ctx_stack,
        rep_stack,
        state,
        pattern_pc,
        1,
    )
    state[_ST_MUST_ADVANCE] = 0
    if status == 0 and p_info[_PI_ANCHORED] != 0:
        state[_ST_START] = adj_end
        state[_ST_PTR] = adj_end
        return 0
    while status == 0 and ptr < adj_end:
        ptr = (ptr + 1) & _MASK32
        state[_ST_NUM_MARKS] = 0
        state[_ST_LASTINDEX] = 0
        state[_ST_START] = ptr
        state[_ST_PTR] = ptr
        status = _sre_match(
            code,
            charset_flat,
            charset_base,
            is_bytes,
            text,
            data,
            marks,
            mark_stack,
            ctx_stack,
            rep_stack,
            state,
            pattern_pc,
            0,
        )
    return status


@mule_types.optimize(safety=0)
def _append_latin1_charset_table(
    code: list[mule_types.uint32],
    set_pc: int,
    in_op: int,
    charset_flat: list[mule_types.uint32],
) -> None:
    c: int = 0
    if in_op == _IN:
        while c < 256:
            charset_flat.append(1 if _sre_charset(code, set_pc, c) else 0)
            c += 1
    elif in_op == _IN_IGNORE:
        while c < 256:
            charset_flat.append(
                1 if _sre_charset(code, set_pc, ascii_tolower(c)) else 0
            )
            c += 1
    elif in_op == _IN_UNI_IGNORE:
        while c < 256:
            charset_flat.append(
                1 if _sre_charset(code, set_pc, unicode_tolower(c)) else 0
            )
            c += 1
    else:
        while c < 256:
            charset_flat.append(1 if _sre_charset_loc_ignore(code, set_pc, c) else 0)
            c += 1


@mule_types.optimize(safety=0)
def _populate_charset_tables(
    code: list[mule_types.uint32],
    start: int,
    end: int,
    charset_flat: list[mule_types.uint32],
    charset_base: list[mule_types.uint32],
) -> None:
    pc: int = start & _MASK32
    limit: int = end & _MASK32
    while pc < limit:
        op: int = code[pc]
        if op == _INFO:
            skip: int = code[pc + 1]
            flags: int = code[pc + 2]
            if (flags & _SRE_INFO_CHARSET) != 0:
                set_pc: int = (pc + 5) & _MASK32
                if charset_base[set_pc] == _UNSET:
                    charset_base[set_pc] = len(charset_flat)
                    _append_latin1_charset_table(code, set_pc, _IN, charset_flat)
            pc = (pc + 1 + skip) & _MASK32
        elif (
            op == _IN
            or op == _IN_IGNORE
            or op == _IN_UNI_IGNORE
            or op == _IN_LOC_IGNORE
        ):
            skip_in: int = code[pc + 1]
            set_pc_in: int = (pc + 2) & _MASK32
            if charset_base[set_pc_in] == _UNSET:
                charset_base[set_pc_in] = len(charset_flat)
                _append_latin1_charset_table(code, set_pc_in, op, charset_flat)
            pc = (pc + 1 + skip_in) & _MASK32
        elif (
            op == _SUCCESS
            or op == _FAILURE
            or op == _ANY
            or op == _ANY_ALL
            or op == _MAX_UNTIL
            or op == _MIN_UNTIL
        ):
            pc += 1
        elif (
            op == _LITERAL
            or op == _NOT_LITERAL
            or op == _LITERAL_IGNORE
            or op == _NOT_LITERAL_IGNORE
            or op == _LITERAL_UNI_IGNORE
            or op == _NOT_LITERAL_UNI_IGNORE
            or op == _LITERAL_LOC_IGNORE
            or op == _NOT_LITERAL_LOC_IGNORE
            or op == _AT
            or op == _CATEGORY
            or op == _MARK
            or op == _GROUPREF
            or op == _GROUPREF_IGNORE
            or op == _GROUPREF_UNI_IGNORE
            or op == _GROUPREF_LOC_IGNORE
            or op == _JUMP
        ):
            pc += 2
        elif op == _GROUPREF_EXISTS:
            pc += 3
        elif (
            op == _REPEAT
            or op == _REPEAT_ONE
            or op == _MIN_REPEAT_ONE
            or op == _POSSESSIVE_REPEAT
            or op == _POSSESSIVE_REPEAT_ONE
        ):
            skip_rep: int = code[pc + 1]
            _populate_charset_tables(
                code,
                (pc + 4) & _MASK32,
                (pc + 1 + skip_rep) & _MASK32,
                charset_flat,
                charset_base,
            )
            pc = (pc + 1 + skip_rep) & _MASK32
        elif op == _ASSERT or op == _ASSERT_NOT:
            skip_as: int = code[pc + 1]
            _populate_charset_tables(
                code,
                (pc + 3) & _MASK32,
                (pc + 1 + skip_as) & _MASK32,
                charset_flat,
                charset_base,
            )
            pc = (pc + 1 + skip_as) & _MASK32
        elif op == _ATOMIC_GROUP:
            skip_ag: int = code[pc + 1]
            _populate_charset_tables(
                code,
                (pc + 2) & _MASK32,
                (pc + 1 + skip_ag) & _MASK32,
                charset_flat,
                charset_base,
            )
            pc = (pc + 1 + skip_ag) & _MASK32
        elif op == _BRANCH:
            at: int = (pc + 1) & _MASK32
            while at < limit and code[at] != 0:
                skip_br: int = code[at]
                _populate_charset_tables(
                    code,
                    (at + 1) & _MASK32,
                    (at + skip_br) & _MASK32,
                    charset_flat,
                    charset_base,
                )
                at = (at + skip_br) & _MASK32
            pc = (at + 1) & _MASK32
        else:
            break


def _extract_buffer(string) -> tuple[bool, str, bytes, int]:
    """Extract (is_bytes, text, data, length) from a target string or buffer."""
    if isinstance(string, str):
        text: str = str(string)
        return False, text, b"", len(text)
    if isinstance(string, (bytes, bytearray)):
        data: bytes = bytes(string)
        return True, "", data, len(data)
    try:
        mv = memoryview(string).cast("B")
    except TypeError:
        raise TypeError(
            f"expected string or bytes-like object, got '{type(string).__name__}'"
        ) from None
    data2: bytes = bytes(mv)
    return True, "", data2, len(data2)


def _getslice(is_bytes: bool, string, text: str, data: bytes, start: int, end: int):
    if not is_bytes:
        if start <= 0 and end >= len(text) and type(string) is str:
            return string
        if end <= start:
            return ""
        return text[start:end]
    if start <= 0 and end >= len(data) and type(string) is bytes:
        return string
    if end <= start:
        return b""
    return data[start:end]


def _group_slice_from_marks(
    is_bytes: bool,
    string,
    text: str,
    data: bytes,
    marks: list[int],
    state: list[int],
    group_idx: int,
    empty: bool,
):
    if group_idx == 0:
        return _getslice(
            is_bytes,
            string,
            text,
            data,
            state[_ST_START] & _MASK32,
            state[_ST_PTR] & _MASK32,
        )
    j: int = ((group_idx - 1) & _MASK32) << 1
    num_marks: int = state[_ST_NUM_MARKS] & _MASK32
    if j + 1 < num_marks:
        s: int = marks[j] & _MASK32
        e: int = marks[j + 1] & _MASK32
        if s < _UNSET and e < _UNSET:
            if s > e:
                raise SystemError(
                    "The span of capturing group is wrong, please report a bug for the re module."
                )
            return _getslice(is_bytes, string, text, data, s, e)
    if empty:
        return b"" if is_bytes else ""
    return None


class _Engine:
    __mule_slots__ = (
        "pattern",
        "code",
        "charset_flat",
        "charset_base",
        "p_info",
        "prefix_str",
        "prefix_bytes",
        "string",
        "text",
        "data",
        "is_bytes",
        "pos",
        "endpos",
        "marks",
        "mark_stack",
        "ctx_stack",
        "rep_stack",
        "state",
    )

    def __init__(self, pattern: Pattern, string, pos: int, endpos: int):
        is_bytes, text, data, length = _extract_buffer(string)
        if is_bytes and pattern._isbytes == 0:
            raise TypeError("cannot use a string pattern on a bytes-like object")
        if not is_bytes and pattern._isbytes > 0:
            raise TypeError("cannot use a bytes pattern on a string-like object")
        if pos < 0:
            pos = 0
        elif pos > length:
            pos = length
        if endpos < 0:
            endpos = 0
        elif endpos > length:
            endpos = length
        self.pattern: Pattern = pattern
        self.code: list[int] = pattern._code
        self.charset_flat: list[int] = pattern._charset_flat
        self.charset_base: list[int] = pattern._charset_base
        self.p_info: list[int] = pattern._p_info
        self.prefix_str: str = pattern._prefix_str
        self.prefix_bytes: bytes = pattern._prefix_bytes
        self.string = string
        self.text: str = text
        self.data: bytes = data
        self.is_bytes: bool = is_bytes
        self.pos: int = pos
        self.endpos: int = endpos
        ngroups: int = pattern.groups
        self.marks: list[int] = [_UNSET] * (ngroups * 2) if ngroups > 0 else []
        self.mark_stack: list[int] = []
        self.ctx_stack: list[int] = []
        self.rep_stack: list[int] = []
        self.state: list[int] = [pos, endpos, pos, 0, 0, 0, _UNSET, 0, 0]

    def reset(self) -> None:
        state: list[int] = self.state
        state[_ST_NUM_MARKS] = 0
        state[_ST_LASTINDEX] = 0
        state[_ST_SAVE_MARKS] = 0
        state[_ST_REPEAT_HEAD] = _UNSET

    def _match(self, init_pattern: int, init_toplevel: int) -> int:
        return _sre_match(
            self.code,
            self.charset_flat,
            self.charset_base,
            self.is_bytes,
            self.text,
            self.data,
            self.marks,
            self.mark_stack,
            self.ctx_stack,
            self.rep_stack,
            self.state,
            init_pattern,
            init_toplevel,
        )

    def _search(self) -> int:
        return _sre_search(
            self.code,
            self.charset_flat,
            self.charset_base,
            self.p_info,
            self.prefix_str,
            self.prefix_bytes,
            self.is_bytes,
            self.text,
            self.data,
            self.marks,
            self.mark_stack,
            self.ctx_stack,
            self.rep_stack,
            self.state,
        )

    def _new_match(self) -> Match:
        ngroups: int = self.pattern.groups
        out_marks: list[int] = [-1] * ((ngroups + 1) << 1)
        state: list[int] = self.state
        out_marks[0] = state[_ST_START] & _MASK32
        out_marks[1] = state[_ST_PTR] & _MASK32
        marks: list[int] = self.marks
        num_marks: int = state[_ST_NUM_MARKS] & _MASK32
        i: int = 0
        j: int = 0
        while i < ngroups:
            if j + 1 < num_marks:
                m_start: int = marks[j] & _MASK32
                m_end: int = marks[j + 1] & _MASK32
                if m_start < _UNSET and m_end < _UNSET:
                    if m_start > m_end:
                        raise SystemError(
                            "The span of capturing group is wrong, please report a bug for the re module."
                        )
                    out_marks[j + 2] = m_start
                    out_marks[j + 3] = m_end
            i += 1
            j += 2
        raw_lastindex: int = state[_ST_LASTINDEX] & _MASK32
        lastindex: int = raw_lastindex if raw_lastindex > 0 else -1
        return Match(
            self.pattern,
            self.string,
            out_marks,
            self.pos,
            self.endpos,
            lastindex,
        )


class _Template:
    __mule_slots__ = ("literal", "indices", "literals", "is_bytes")

    def __init__(self, literal, indices: list[int], literals: list, is_bytes: bool):
        self.literal = literal
        self.indices: list[int] = indices
        self.literals: list = literals
        self.is_bytes: bool = is_bytes

    def expand_match(self, match: Match):
        indices: list[int] = self.indices
        n: int = len(indices)
        if n == 0:
            return self.literal
        literals: list = self.literals
        ngroups: int = match.re._ngroups
        parts: list = []
        if self.literal:
            parts.append(self.literal)
        i: int = 0
        while i < n:
            idx: int = indices[i] & _MASK32
            if idx >= ngroups:
                raise IndexError("no such group")
            piece = match._group_by_index(idx, None)
            if piece is not None:
                parts.append(piece)
            lit = literals[i]
            if lit is not None:
                parts.append(lit)
            i += 1
        if self.is_bytes:
            return b"".join(parts)
        return "".join(parts)


def template(pattern, template_items):
    if not isinstance(template_items, list):
        raise TypeError("invalid template")
    n: int = len(template_items)
    if (n & 1) == 0 or n < 1:
        raise TypeError("invalid template")
    pairs: int = n >> 1
    first_lit = template_items[0]
    is_bytes: bool = isinstance(first_lit, (bytes, bytearray, memoryview))
    indices: list[int] = []
    literals: list = []
    i: int = 0
    while i < pairs:
        raw_idx = template_items[(i << 1) + 1]
        if not isinstance(raw_idx, int):
            try:
                raw_idx = raw_idx.__index__()
            except AttributeError, TypeError:
                raise TypeError("an integer is required") from None
        idx: int = int(raw_idx)
        if idx < 0:
            raise TypeError("invalid template")
        indices.append(idx)
        lit = template_items[(i << 1) + 2]
        if len(lit) == 0:
            literals.append(None)
        else:
            literals.append(lit)
        i += 1
    return _Template(first_lit, indices, literals, is_bytes)


def compile(pattern, flags, code, ngroups, groupindex, indexgroup):
    if not isinstance(code, list):
        raise TypeError("code must be a list")
    if groupindex is not None and not isinstance(groupindex, dict):
        raise TypeError("groupindex must be a dict")
    if indexgroup is not None and not isinstance(indexgroup, tuple):
        raise TypeError("indexgroup must be a tuple")
    checked_code: list[int] = []
    i: int = 0
    n: int = len(code)
    while i < n:
        item = code[i]
        if not isinstance(item, int):
            try:
                item = item.__index__()
            except AttributeError, TypeError:
                raise TypeError("an integer is required") from None
        val: int = int(item)
        if val < 0 or val > MAXREPEAT:
            raise OverflowError("regular expression code size limit exceeded")
        checked_code.append(val)
        i += 1
    isbytes: int = -1
    if pattern is not None:
        is_b, _, _, _ = _extract_buffer(pattern)
        isbytes = 1 if is_b else 0
    return Pattern(
        pattern,
        int(flags),
        checked_code,
        int(ngroups),
        groupindex,
        indexgroup,
        isbytes,
    )


class Pattern:
    __mule_slots__ = (
        "pattern",
        "flags",
        "_code",
        "groups",
        "_ngroups",
        "_groupindex",
        "_indexgroup",
        "_isbytes",
        "_p_info",
        "_prefix_str",
        "_prefix_bytes",
        "_charset_flat",
        "_charset_base",
    )

    def __init__(
        self,
        pattern,
        flags: int,
        code: list[int],
        ngroups: int,
        groupindex,
        indexgroup,
        isbytes: int = -1,
    ):
        if isbytes == -1 and pattern is not None:
            is_b, _, _, _ = _extract_buffer(pattern)
            isbytes = 1 if is_b else 0
        self.pattern = pattern
        self.flags: int = flags
        self._code: list[int] = code
        self.groups: int = ngroups
        self._ngroups: int = ngroups + 1
        self._groupindex = dict(groupindex) if groupindex else {}
        self._indexgroup = tuple(indexgroup) if indexgroup else ()
        self._isbytes: int = isbytes

        pattern_pc: int = 0
        info_flags: int = 0
        min_width: int = 0
        prefix_len: int = 0
        prefix_skip: int = 0
        prefix_first: int = 0
        charset_pc: int = 0
        prefix_str: str = ""
        prefix_bytes: bytes = b""
        prefix_bytes_valid: int = 1

        if len(code) > 0 and (code[0] & _MASK32) == _INFO:
            info_skip: int = code[1] & _MASK32
            info_flags = code[2] & _MASK32
            min_width = code[3] & _MASK32
            if (info_flags & _SRE_INFO_PREFIX) != 0:
                prefix_len = code[5] & _MASK32
                prefix_skip = code[6] & _MASK32
                if prefix_len > 0:
                    prefix_first = code[7] & _MASK32
                    chars: list[str] = []
                    b_arr = bytearray()
                    pi: int = 0
                    while pi < prefix_len:
                        cp: int = code[7 + pi] & _MASK32
                        if cp <= 0x10FFFF:
                            chars.append(chr(cp))
                        if cp <= 255:
                            b_arr.append(cp)
                        else:
                            prefix_bytes_valid = 0
                        pi += 1
                    prefix_str = "".join(chars)
                    if prefix_bytes_valid != 0:
                        prefix_bytes = bytes(b_arr)
            elif (info_flags & _SRE_INFO_CHARSET) != 0:
                charset_pc = 5
            pattern_pc = (1 + info_skip) & _MASK32

        anchored_start: int = (
            1
            if (
                pattern_pc + 1 < len(code)
                and (code[pattern_pc] & _MASK32) == _AT
                and (
                    (code[pattern_pc + 1] & _MASK32) == _AT_BEGINNING
                    or (code[pattern_pc + 1] & _MASK32) == _AT_BEGINNING_STRING
                )
            )
            else 0
        )

        self._p_info: list[int] = [
            pattern_pc,
            info_flags,
            min_width,
            prefix_len,
            prefix_skip,
            prefix_first,
            charset_pc,
            anchored_start,
            prefix_bytes_valid,
        ]
        self._prefix_str: str = prefix_str
        self._prefix_bytes: bytes = prefix_bytes

        charset_flat: list[int] = []
        charset_base: list[int] = [_UNSET] * len(code)
        _populate_charset_tables(code, 0, len(code), charset_flat, charset_base)
        self._charset_flat: list[int] = charset_flat
        self._charset_base: list[int] = charset_base

    @property
    def groupindex(self):
        return types.MappingProxyType(self._groupindex)

    def _make_engine(self, string, pos, endpos) -> _Engine:
        start: int = 0 if pos is None else int(pos)
        end: int = 0x7FFFFFFF if endpos is None else int(endpos)
        return _Engine(self, string, start, end)

    def match(self, string, pos=0, endpos=None):
        engine: _Engine = self._make_engine(string, pos, endpos)
        state: list[int] = engine.state
        state[_ST_PTR] = state[_ST_START]
        status: int = engine._match(0, 1)
        if status > 0:
            return engine._new_match()
        return None

    def prefixmatch(self, string, pos=0, endpos=None):
        return self.match(string, pos, endpos)

    def fullmatch(self, string, pos=0, endpos=None):
        engine: _Engine = self._make_engine(string, pos, endpos)
        state: list[int] = engine.state
        state[_ST_MATCH_ALL] = 1
        state[_ST_PTR] = state[_ST_START]
        status: int = engine._match(0, 1)
        if status > 0:
            return engine._new_match()
        return None

    def search(self, string, pos=0, endpos=None):
        engine: _Engine = self._make_engine(string, pos, endpos)
        status: int = engine._search()
        if status > 0:
            return engine._new_match()
        return None

    def findall(self, string, pos=0, endpos=None):
        engine: _Engine = self._make_engine(string, pos, endpos)
        result: list = []
        ngroups: int = self.groups
        is_bytes: bool = engine.is_bytes
        orig_string = engine.string
        text: str = engine.text
        data: bytes = engine.data
        code: list[int] = engine.code
        charset_flat: list[int] = engine.charset_flat
        charset_base: list[int] = engine.charset_base
        p_info: list[int] = engine.p_info
        prefix_str: str = engine.prefix_str
        prefix_bytes: bytes = engine.prefix_bytes
        marks: list[int] = engine.marks
        mark_stack: list[int] = engine.mark_stack
        ctx_stack: list[int] = engine.ctx_stack
        rep_stack: list[int] = engine.rep_stack
        state: list[int] = engine.state
        end: int = state[_ST_END] & _MASK32
        while (state[_ST_START] & _MASK32) <= end:
            state[_ST_NUM_MARKS] = 0
            state[_ST_LASTINDEX] = 0
            state[_ST_SAVE_MARKS] = 0
            state[_ST_REPEAT_HEAD] = _UNSET
            state[_ST_PTR] = state[_ST_START]
            status: int = _sre_search(
                code,
                charset_flat,
                charset_base,
                p_info,
                prefix_str,
                prefix_bytes,
                is_bytes,
                text,
                data,
                marks,
                mark_stack,
                ctx_stack,
                rep_stack,
                state,
            )
            if status <= 0:
                break
            m_start: int = state[_ST_START] & _MASK32
            m_end: int = state[_ST_PTR] & _MASK32
            if ngroups == 0:
                item = _getslice(is_bytes, orig_string, text, data, m_start, m_end)
            elif ngroups == 1:
                item = _group_slice_from_marks(
                    is_bytes, orig_string, text, data, marks, state, 1, True
                )
            else:
                tup: list = []
                g: int = 1
                while g <= ngroups:
                    tup.append(
                        _group_slice_from_marks(
                            is_bytes, orig_string, text, data, marks, state, g, True
                        )
                    )
                    g += 1
                item = tuple(tup)
            result.append(item)
            state[_ST_MUST_ADVANCE] = 1 if m_end == m_start else 0
            state[_ST_START] = m_end
        return result

    def finditer(self, string, pos=0, endpos=None):
        scanner: _Scanner = self.scanner(string, pos, endpos)
        while True:
            m = scanner.search()
            if m is None:
                return
            yield m

    def split(self, string, maxsplit=0):
        maxsplit_i: int = int(maxsplit)
        engine: _Engine = self._make_engine(string, 0, None)
        result: list = []
        ngroups: int = self.groups
        is_bytes: bool = engine.is_bytes
        orig_string = engine.string
        text: str = engine.text
        data: bytes = engine.data
        marks: list[int] = engine.marks
        state: list[int] = engine.state
        last: int = state[_ST_START] & _MASK32
        n: int = 0
        while maxsplit_i == 0 or n < maxsplit_i:
            engine.reset()
            state[_ST_PTR] = state[_ST_START]
            status: int = engine._search()
            if status <= 0:
                break
            m_start: int = state[_ST_START] & _MASK32
            m_end: int = state[_ST_PTR] & _MASK32
            result.append(_getslice(is_bytes, orig_string, text, data, last, m_start))
            g: int = 1
            while g <= ngroups:
                result.append(
                    _group_slice_from_marks(
                        is_bytes, orig_string, text, data, marks, state, g, False
                    )
                )
                g += 1
            n += 1
            state[_ST_MUST_ADVANCE] = 1 if m_end == m_start else 0
            last = m_end
            state[_ST_START] = m_end
        result.append(_getslice(is_bytes, orig_string, text, data, last, engine.endpos))
        return result

    def _compile_repl_template(self, repl):
        import re

        try:
            return re._compile_template(self, repl)
        except TypeError:
            is_b, text_r, data_r, _ = _extract_buffer(repl)
            return re._compile_template(self, data_r if is_b else text_r)

    def subn(self, repl, string, count=0):
        count_i: int = int(count)
        is_callable: bool = callable(repl)
        filter_kind: int = 0  # 0 = literal, 1 = template, 2 = callable
        filter_obj = repl
        if is_callable:
            filter_kind = 2
        else:
            try:
                is_repl_bytes, repl_text, repl_data, _ = _extract_buffer(repl)
                if is_repl_bytes:
                    has_backslash: bool = b"\\" in repl_data
                else:
                    has_backslash = "\\" in repl_text
            except TypeError:
                has_backslash = True
                is_repl_bytes = False
                repl_text = ""
                repl_data = b""
            if not has_backslash:
                if is_repl_bytes and self._isbytes == 0:
                    raise TypeError(
                        "cannot use a string pattern on a bytes-like object"
                    )
                if not is_repl_bytes and self._isbytes > 0:
                    raise TypeError(
                        "cannot use a bytes pattern on a string-like object"
                    )
                filter_kind = 0
                filter_obj = repl_data if is_repl_bytes else repl_text
            else:
                compiled_tpl = self._compile_repl_template(repl)
                if len(compiled_tpl.indices) == 0:
                    filter_kind = 0
                    filter_obj = compiled_tpl.literal
                else:
                    filter_kind = 1
                    filter_obj = compiled_tpl

        engine: _Engine = self._make_engine(string, 0, None)
        is_bytes: bool = engine.is_bytes
        orig_string = engine.string
        text: str = engine.text
        data: bytes = engine.data
        code: list[int] = engine.code
        charset_flat: list[int] = engine.charset_flat
        charset_base: list[int] = engine.charset_base
        p_info: list[int] = engine.p_info
        prefix_str: str = engine.prefix_str
        prefix_bytes: bytes = engine.prefix_bytes
        marks: list[int] = engine.marks
        mark_stack: list[int] = engine.mark_stack
        ctx_stack: list[int] = engine.ctx_stack
        rep_stack: list[int] = engine.rep_stack
        state: list[int] = engine.state
        parts: list = []
        n: int = 0
        i: int = 0
        while count_i == 0 or n < count_i:
            state[_ST_NUM_MARKS] = 0
            state[_ST_LASTINDEX] = 0
            state[_ST_SAVE_MARKS] = 0
            state[_ST_REPEAT_HEAD] = _UNSET
            state[_ST_PTR] = state[_ST_START]
            status: int = _sre_search(
                code,
                charset_flat,
                charset_base,
                p_info,
                prefix_str,
                prefix_bytes,
                is_bytes,
                text,
                data,
                marks,
                mark_stack,
                ctx_stack,
                rep_stack,
                state,
            )
            if status <= 0:
                break
            b: int = state[_ST_START] & _MASK32
            e: int = state[_ST_PTR] & _MASK32
            if i < b:
                parts.append(_getslice(is_bytes, orig_string, text, data, i, b))
            if filter_kind == 0:
                parts.append(filter_obj)
            elif filter_kind == 1:
                m_tpl: Match = engine._new_match()
                item_tpl = filter_obj.expand_match(m_tpl)
                if item_tpl is not None:
                    parts.append(item_tpl)
            else:
                m_call: Match = engine._new_match()
                item_call = filter_obj(m_call)
                if item_call is not None:
                    parts.append(item_call)
            i = e
            n += 1
            state[_ST_MUST_ADVANCE] = 1 if e == b else 0
            state[_ST_START] = e

        if i < engine.endpos:
            parts.append(_getslice(is_bytes, orig_string, text, data, i, engine.endpos))

        if len(parts) == 0:
            joined = b"" if is_bytes else ""
        elif is_bytes:
            joined = b"".join(parts)
        else:
            joined = "".join(parts)
        return joined, n

    def sub(self, repl, string, count=0):
        return self.subn(repl, string, count)[0]

    def scanner(self, string, pos=0, endpos=None):
        return _Scanner(self, string, pos, endpos)

    def __copy__(self):
        return self

    def __deepcopy__(self, memo):
        return self

    def __eq__(self, other):
        if self is other:
            return True
        if not isinstance(other, Pattern):
            return NotImplemented
        return (
            self.flags == other.flags
            and self._isbytes == other._isbytes
            and self._code == other._code
            and self.pattern == other.pattern
        )

    def __hash__(self) -> int:
        return hash((self.pattern, self._isbytes, self.flags, tuple(self._code)))

    def __repr__(self) -> str:
        flags: int = self.flags
        if (
            self._isbytes == 0
            and (flags & (_SRE_FLAG_LOCALE | _SRE_FLAG_UNICODE | _SRE_FLAG_ASCII))
            == _SRE_FLAG_UNICODE
        ):
            flags &= ~_SRE_FLAG_UNICODE
        flag_items: list[str] = []
        for name, val in (
            ("re.IGNORECASE", _SRE_FLAG_IGNORECASE),
            ("re.LOCALE", _SRE_FLAG_LOCALE),
            ("re.MULTILINE", _SRE_FLAG_MULTILINE),
            ("re.DOTALL", _SRE_FLAG_DOTALL),
            ("re.UNICODE", _SRE_FLAG_UNICODE),
            ("re.VERBOSE", _SRE_FLAG_VERBOSE),
            ("re.DEBUG", _SRE_FLAG_DEBUG),
            ("re.ASCII", _SRE_FLAG_ASCII),
        ):
            if (flags & val) != 0:
                flag_items.append(name)
                flags &= ~val
        if flags != 0:
            flag_items.append(hex(flags))
        pat_repr: str = repr(self.pattern)
        if len(pat_repr) > 200:
            pat_repr = pat_repr[:200]
        if flag_items:
            return f"re.compile({pat_repr}, {'|'.join(flag_items)})"
        return f"re.compile({pat_repr})"

    def __class_getitem__(cls, item):
        return types.GenericAlias(cls, item)


class _Scanner:
    __mule_slots__ = ("pattern", "_engine", "_active", "_done")

    def __init__(self, pattern: Pattern, string, pos=0, endpos=None):
        self.pattern: Pattern = pattern
        self._engine: _Engine = pattern._make_engine(string, pos, endpos)
        self._active: bool = False
        self._done: bool = False

    def match(self):
        if self._active:
            raise ValueError("regular expression scanner already executing")
        if self._done:
            return None
        self._active = True
        try:
            engine: _Engine = self._engine
            state: list[int] = engine.state
            engine.reset()
            state[_ST_PTR] = state[_ST_START]
            status: int = engine._match(0, 1)
            if status <= 0:
                self._done = True
                return None
            m: Match = engine._new_match()
            m_start: int = state[_ST_START] & _MASK32
            m_end: int = state[_ST_PTR] & _MASK32
            state[_ST_MUST_ADVANCE] = 1 if m_end == m_start else 0
            state[_ST_START] = m_end
            return m
        finally:
            self._active = False

    def prefixmatch(self):
        return self.match()

    def search(self):
        if self._active:
            raise ValueError("regular expression scanner already executing")
        if self._done:
            return None
        self._active = True
        try:
            engine: _Engine = self._engine
            state: list[int] = engine.state
            if (state[_ST_START] & _MASK32) > (state[_ST_END] & _MASK32):
                self._done = True
                return None
            engine.reset()
            state[_ST_PTR] = state[_ST_START]
            status: int = engine._search()
            if status <= 0:
                self._done = True
                return None
            m: Match = engine._new_match()
            m_start: int = state[_ST_START] & _MASK32
            m_end: int = state[_ST_PTR] & _MASK32
            state[_ST_MUST_ADVANCE] = 1 if m_end == m_start else 0
            state[_ST_START] = m_end
            return m
        finally:
            self._active = False


class Match:
    __mule_slots__ = (
        "re",
        "string",
        "_marks",
        "pos",
        "endpos",
        "lastindex",
        "lastgroup",
        "_regs",
    )

    def __init__(
        self,
        pattern: Pattern,
        string,
        marks: list[int],
        pos: int,
        endpos: int,
        lastindex: int = -1,
    ):
        self.re: Pattern = pattern
        self.string = string
        self._marks: list[int] = marks
        self.pos: int = pos
        self.endpos: int = endpos
        self.lastindex = lastindex if lastindex >= 0 else None
        self.lastgroup = None
        if lastindex >= 0:
            indexgroup = pattern._indexgroup
            if indexgroup and lastindex < len(indexgroup):
                self.lastgroup = indexgroup[lastindex]
            elif pattern._groupindex:
                for name, idx in pattern._groupindex.items():
                    if idx == lastindex:
                        self.lastgroup = name
                        break
        self._regs = None

    def _get_index(self, group) -> int:
        idx: int = -1
        if isinstance(group, int):
            idx = int(group)
        elif isinstance(group, str):
            groupindex = self.re._groupindex
            if groupindex and group in groupindex:
                idx = groupindex[group]
        else:
            try:
                idx = int(group.__index__())
            except AttributeError, TypeError:
                groupindex2 = self.re._groupindex
                if groupindex2:
                    try:
                        if group in groupindex2:
                            idx = groupindex2[group]
                    except TypeError:
                        pass
        if idx < 0 or idx >= self.re._ngroups:
            raise IndexError("no such group")
        return idx

    def _group_by_index(self, idx: int, default):
        start: int = self._marks[idx << 1]
        if self.string is None or start < 0:
            return default
        end: int = self._marks[(idx << 1) + 1]
        is_b, text, data, length = _extract_buffer(self.string)
        if start > length:
            start = length
        if end > length:
            end = length
        return _getslice(is_b, self.string, text, data, start, end)

    def span(self, group=0):
        idx: int = self._get_index(group)
        return self._marks[idx << 1], self._marks[(idx << 1) + 1]

    def start(self, group=0) -> int:
        idx: int = self._get_index(group)
        return self._marks[idx << 1]

    def end(self, group=0) -> int:
        idx: int = self._get_index(group)
        return self._marks[(idx << 1) + 1]

    def group(self, *groups):
        n: int = len(groups)
        if n == 0:
            return self._group_by_index(0, None)
        if n == 1:
            return self._group_by_index(self._get_index(groups[0]), None)
        values: list = []
        i: int = 0
        while i < n:
            values.append(self._group_by_index(self._get_index(groups[i]), None))
            i += 1
        return tuple(values)

    def groups(self, default=None):
        values: list = []
        i: int = 1
        limit: int = self.re._ngroups
        while i < limit:
            values.append(self._group_by_index(i, default))
            i += 1
        return tuple(values)

    def groupdict(self, default=None):
        groupindex = self.re._groupindex
        if not groupindex:
            return {}
        out: dict = {}
        for name, idx in groupindex.items():
            out[name] = self._group_by_index(idx, default)
        return out

    def expand(self, template_text):
        compiled_tpl = self.re._compile_repl_template(template_text)
        return compiled_tpl.expand_match(self)

    @property
    def regs(self):
        if self._regs is None:
            out: list = []
            marks: list[int] = self._marks
            i: int = 0
            limit: int = self.re._ngroups
            while i < limit:
                out.append((marks[i << 1], marks[(i << 1) + 1]))
                i += 1
            self._regs = tuple(out)
        return self._regs

    def __getitem__(self, group):
        return self._group_by_index(self._get_index(group), None)

    def __copy__(self):
        return self

    def __deepcopy__(self, memo):
        return self

    def __repr__(self) -> str:
        g0 = repr(self._group_by_index(0, None))
        if len(g0) > 50:
            g0 = g0[:50]
        return (
            f"<re.Match object; span=({self._marks[0]}, {self._marks[1]}), match={g0}>"
        )

    def __class_getitem__(cls, item):
        return types.GenericAlias(cls, item)
