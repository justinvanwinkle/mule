"""JSON token scanner
"""
import re
c_make_scanner = None

__all__ = ['make_scanner']

NUMBER_RE = re.compile(
    r'(-?(?:0|[1-9][0-9]*))(\.[0-9]+)?([eE][-+]?[0-9]+)?',
    (re.VERBOSE | re.MULTILINE | re.DOTALL))

def _scan_number(string: str, idx: int, n: int) -> int:
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


def py_make_scanner(context):
    parse_object = context.parse_object
    parse_array = context.parse_array
    parse_string = context.parse_string
    strict = context.strict
    parse_float = context.parse_float
    parse_int = context.parse_int
    parse_constant = context.parse_constant
    object_hook = context.object_hook
    object_pairs_hook = context.object_pairs_hook
    memo = context.memo
    default_int: bool = parse_int is int
    default_float: bool = parse_float is float

    def _scan_once(string: str, idx: int):
        if idx < 0:
            raise StopIteration(idx)
        n: int = len(string)
        if idx >= n:
            raise StopIteration(idx)
        ch = string[idx]

        if ch == '"':
            return parse_string(string, idx + 1, strict)
        if ch == '{':
            return parse_object((string, idx + 1), strict,
                _scan_once, object_hook, object_pairs_hook, memo)
        if ch == '[':
            return parse_array((string, idx + 1), _scan_once)
        if ch == 'n' and idx + 4 <= n and string[idx:idx + 4] == 'null':
            return None, idx + 4
        if ch == 't' and idx + 4 <= n and string[idx:idx + 4] == 'true':
            return True, idx + 4
        if ch == 'f' and idx + 5 <= n and string[idx:idx + 5] == 'false':
            return False, idx + 5

        if ch == '-' or ('0' <= ch <= '9'):
            num_end: int = _scan_number(string, idx, n)
            if num_end != 0:
                if num_end > 0:
                    if default_int:
                        return int(string[idx:num_end]), num_end
                    return parse_int(string[idx:num_end]), num_end
                end_pos: int = -num_end
                if default_float:
                    return float(string[idx:end_pos]), end_pos
                return parse_float(string[idx:end_pos]), end_pos
            if ch == '-' and idx + 9 <= n and string[idx:idx + 9] == '-Infinity':
                return parse_constant('-Infinity'), idx + 9
            raise StopIteration(idx)
        if ch == 'N' and idx + 3 <= n and string[idx:idx + 3] == 'NaN':
            return parse_constant('NaN'), idx + 3
        if ch == 'I' and idx + 8 <= n and string[idx:idx + 8] == 'Infinity':
            return parse_constant('Infinity'), idx + 8
        raise StopIteration(idx)

    def scan_once(string: str, idx: int):
        try:
            return _scan_once(string, idx)
        finally:
            memo.clear()

    return scan_once

make_scanner = c_make_scanner or py_make_scanner
