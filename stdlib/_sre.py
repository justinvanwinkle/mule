"""The SRE engine, compiled by Mule.

``re._compiler`` turns a parsed pattern into a code list; this module
executes that list with a backtracking interpreter and provides the
compiled Pattern and Match objects, following CPython's ``_sre`` module.
"""

MAGIC = 20230612
CODESIZE = 4
MAXREPEAT = 4294967295
MAXGROUPS = 1073741823

# Opcode and category values, in _constants order.
(_FAILURE, _SUCCESS, _ANY, _ANY_ALL, _ASSERT, _ASSERT_NOT, _AT, _BRANCH,
 _CATEGORY, _CHARSET, _BIGCHARSET, _GROUPREF, _GROUPREF_EXISTS, _IN, _INFO,
 _JUMP, _LITERAL, _MARK, _MAX_UNTIL, _MIN_UNTIL, _NOT_LITERAL, _NEGATE,
 _RANGE, _REPEAT, _REPEAT_ONE, _SUBPATTERN, _MIN_REPEAT_ONE, _ATOMIC_GROUP,
 _POSSESSIVE_REPEAT, _POSSESSIVE_REPEAT_ONE, _GROUPREF_IGNORE, _IN_IGNORE,
 _LITERAL_IGNORE, _NOT_LITERAL_IGNORE, _GROUPREF_LOC_IGNORE,
 _IN_LOC_IGNORE, _LITERAL_LOC_IGNORE, _NOT_LITERAL_LOC_IGNORE,
 _GROUPREF_UNI_IGNORE, _IN_UNI_IGNORE, _LITERAL_UNI_IGNORE,
 _NOT_LITERAL_UNI_IGNORE, _RANGE_UNI_IGNORE) = range(43)

SRE_INFO_PREFIX = 1
SRE_INFO_LITERAL = 2
_CATEGORY_DIGIT = 0
_CATEGORY_SPACE = 2
_CATEGORY_WORD = 4


def ascii_iscased(code: int) -> bool:
    return 65 <= code <= 90 or 97 <= code <= 122


def ascii_tolower(code: int) -> int:
    return code + 32 if 65 <= code <= 90 else code


def unicode_iscased(code: int) -> bool:
    if code < 128:
        return 65 <= code <= 90 or 97 <= code <= 122
    char = chr(code)
    return char.lower() != char or char.upper() != char


def unicode_tolower(code: int) -> int:
    if code < 128:
        return code + 32 if 65 <= code <= 90 else code
    lowered = chr(code).lower()
    return ord(lowered) if len(lowered) == 1 else code


def _is_word(char: str) -> bool:
    return char == "_" or char.isalnum()


def _is_ascii_word(code: int) -> bool:
    return (
        (48 <= code <= 57)
        or (65 <= code <= 90)
        or (97 <= code <= 122)
        or code == 95
    )


def _is_uni_word_code(code: int) -> bool:
    if code < 128:
        return (
            (48 <= code <= 57)
            or (65 <= code <= 90)
            or (97 <= code <= 122)
            or code == 95
        )
    return chr(code).isalnum()


def _is_ascii_space(code: int) -> bool:
    return 9 <= code <= 13 or code == 32


def _is_uni_space_code(code: int) -> bool:
    if code < 128:
        return 9 <= code <= 13 or code == 32 or (28 <= code <= 31)
    return chr(code).isspace()


def _is_uni_digit_code(code: int) -> bool:
    if code < 128:
        return 48 <= code <= 57
    return chr(code).isdecimal()


def _is_uni_linebreak_code(code: int) -> bool:
    return (
        (10 <= code <= 13)
        or (28 <= code <= 30)
        or code == 0x85
        or code == 0x2028
        or code == 0x2029
    )


def _category_matches_code(category: int, code: int) -> bool:
    if category == 0:
        return 48 <= code <= 57
    if category == 1:
        return not (48 <= code <= 57)
    if category == 2:
        return _is_ascii_space(code)
    if category == 3:
        return not _is_ascii_space(code)
    if category == 4 or category == 8:
        return _is_ascii_word(code)
    if category == 5 or category == 9:
        return not _is_ascii_word(code)
    if category == 6:
        return code == 10 or code == 13
    if category == 7:
        return code != 10 and code != 13
    if category == 10:
        return _is_uni_digit_code(code)
    if category == 11:
        return not _is_uni_digit_code(code)
    if category == 12:
        return _is_uni_space_code(code)
    if category == 13:
        return not _is_uni_space_code(code)
    if category == 14:
        return _is_uni_word_code(code)
    if category == 15:
        return not _is_uni_word_code(code)
    if category == 16:
        return _is_uni_linebreak_code(code)
    return not _is_uni_linebreak_code(code)


def _category_matches(category: int, char, code: int) -> bool:
    return _category_matches_code(category, code)


class _Engine:
    def __init__(self, code, string, end: int):
        self.code = code
        self.string = string
        self.end = end
        self.bytes = isinstance(string, (bytes, bytearray, memoryview))
        self.is_str_or_bytes = isinstance(string, (str, bytes, bytearray))
        self._branch_next_cand = None

    def char(self, pos: int) -> int:
        value = self.string[pos]
        return value if self.bytes else ord(value)

    def text_char(self, pos: int) -> str:
        value = self.string[pos]
        return chr(value) if self.bytes else value

    def run(self, pc, pos, marks, halt=-1):
        code = self.code
        string = self.string
        copied = False
        while True:
            if pc == halt:
                yield pos, marks
                return
            op = code[pc]
            if op == _SUCCESS:
                yield pos, marks
                return
            if op == _FAILURE:
                return
            if op == _MAX_UNTIL or op == _MIN_UNTIL:
                pc += 1
            elif op == _INFO:
                pc = pc + 1 + code[pc + 1]
            elif op == _LITERAL or op == _NOT_LITERAL:
                if pos < self.end and (self.char(pos) == code[pc + 1]) == (op == _LITERAL):
                    pc += 2
                    pos += 1
                else:
                    return
            elif (
                op == _LITERAL_IGNORE
                or op == _LITERAL_LOC_IGNORE
                or op == _LITERAL_UNI_IGNORE
                or op == _NOT_LITERAL_IGNORE
                or op == _NOT_LITERAL_LOC_IGNORE
                or op == _NOT_LITERAL_UNI_IGNORE
            ):
                negate = (
                    op == _NOT_LITERAL_IGNORE
                    or op == _NOT_LITERAL_LOC_IGNORE
                    or op == _NOT_LITERAL_UNI_IGNORE
                )
                if pos >= self.end:
                    return
                char = self.char(pos)
                if op == _LITERAL_UNI_IGNORE or op == _NOT_LITERAL_UNI_IGNORE:
                    char = unicode_tolower(char)
                else:
                    char = ascii_tolower(char)
                if (char == code[pc + 1]) == negate:
                    return
                pc += 2
                pos += 1
            elif op == _ANY or op == _ANY_ALL:
                if pos >= self.end or (op == _ANY and self.char(pos) == 10):
                    return
                pc += 1
                pos += 1
            elif op == _MARK:
                if not copied:
                    marks = list(marks)
                    copied = True
                marks[code[pc + 1]] = pos
                pc += 2
            elif op == _JUMP:
                pc = pc + 1 + code[pc + 1]
            elif op == _BRANCH:
                at = pc + 1
                while True:
                    arm = at + 1
                    if not _is_single_char_op(code[arm]) or self._match_one(arm, pos):
                        yield from self.run(arm, pos, marks, halt)
                    at = at + code[at]
                    if code[at] == _FAILURE:
                        break
                return
            elif (
                op == _IN
                or op == _IN_IGNORE
                or op == _IN_UNI_IGNORE
                or op == _IN_LOC_IGNORE
            ):
                if pos >= self.end:
                    return
                if not self._in_charset(pc + 2, pos, op):
                    return
                pc = pc + 1 + code[pc + 1]
                pos += 1
            elif op == _CATEGORY:
                if pos >= self.end:
                    return
                if not _category_matches_code(code[pc + 1], self.char(pos)):
                    return
                pc += 2
                pos += 1
            elif op == _AT:
                if not self._at_matches(code[pc + 1], pos):
                    return
                pc += 2
            elif (
                op == _GROUPREF
                or op == _GROUPREF_IGNORE
                or op == _GROUPREF_LOC_IGNORE
                or op == _GROUPREF_UNI_IGNORE
            ):
                group = code[pc + 1]
                start, stop = marks[2 * group], marks[2 * group + 1]
                if start < 0 or stop < 0:
                    return
                size = stop - start
                if pos + size > self.end:
                    return
                wanted = string[start:stop]
                found = string[pos:pos + size]
                if op != _GROUPREF:
                    if self.bytes:
                        wanted = bytes(ascii_tolower(c) for c in wanted)
                        found = bytes(ascii_tolower(c) for c in found)
                    else:
                        wanted = wanted.lower()
                        found = found.lower()
                if found != wanted:
                    return
                pc += 2
                pos += size
            elif op == _GROUPREF_EXISTS:
                group = code[pc + 1]
                if marks[2 * group] >= 0:
                    pc += 3
                else:
                    pc = pc + 1 + code[pc + 2]
            elif (
                op == _REPEAT
                or op == _REPEAT_ONE
                or op == _MIN_REPEAT_ONE
                or op == _POSSESSIVE_REPEAT
                or op == _POSSESSIVE_REPEAT_ONE
            ):
                yield from self._repeat(pc, pos, marks, halt)
                return
            elif op == _ATOMIC_GROUP:
                stop = pc + 1 + code[pc + 1]
                for state in self.run(pc + 2, pos, marks, stop):
                    yield from self.run(stop, state[0], state[1], halt)
                    break
                return
            elif op == _ASSERT or op == _ASSERT_NOT:
                stop = pc + 1 + code[pc + 1]
                direction = code[pc + 2]
                states = self._assert_states(pc + 3, pos, direction, stop, marks)
                if op == _ASSERT:
                    if not states:
                        return
                    pc, marks = stop, states[0][1]
                    copied = False
                else:
                    if states:
                        return
                    pc = stop
            else:
                raise ValueError("unsupported SRE opcode %r" % (op,))

    def _assert_states(self, pc, pos, direction, stop, marks):
        if direction == 0:
            return list(self.run(pc, pos, marks, stop))
        start = pos - direction
        if start < 0:
            return []
        return [state for state in self.run(pc, start, marks, stop)
                if state[0] == pos]

    def _fresh_marks(self):
        return [-1] * (2 * self.groups)

    groups = 1

    def _max_repeat_one_count(self, body_pc: int, pos: int, max_count: int) -> int:
        if max_count <= 0:
            return 0
        avail = self.end - pos
        if avail <= 0:
            return 0
        if max_count > avail:
            max_count = avail
        code = self.code
        op = code[body_pc]
        if op == _ANY_ALL:
            return max_count
        if op == _ANY:
            if self.is_str_or_bytes:
                nl = self.string.find(b"\n" if self.bytes else "\n", pos, pos + max_count)
                return max_count if nl < 0 else nl - pos
            count = 0
            while count < max_count and self.char(pos + count) != 10:
                count += 1
            return count
        if op == _NOT_LITERAL:
            wanted = code[body_pc + 1]
            if self.is_str_or_bytes:
                target = bytes([wanted]) if self.bytes else chr(wanted)
                idx = self.string.find(target, pos, pos + max_count)
                return max_count if idx < 0 else idx - pos
            count = 0
            while count < max_count and self.char(pos + count) != wanted:
                count += 1
            return count
        if op == _LITERAL:
            wanted = code[body_pc + 1]
            count = 0
            while count < max_count and self.char(pos + count) == wanted:
                count += 1
            return count
        if op == _CATEGORY:
            cat = code[body_pc + 1]
            count = 0
            while count < max_count and _category_matches_code(cat, self.char(pos + count)):
                count += 1
            return count
        if op == _IN:
            charset_pc = body_pc + 2
            count = 0
            while count < max_count and self._in_charset(charset_pc, pos + count, _IN):
                count += 1
            return count
        count = 0
        while count < max_count and self._match_one(body_pc, pos + count):
            count += 1
        return count

    def _repeat(self, pc, pos, marks, halt):
        code = self.code
        op = code[pc]
        stop = pc + 1 + code[pc + 1]
        following = stop + 1 if (op == _REPEAT or op == _POSSESSIVE_REPEAT) else stop
        minimum, maximum = code[pc + 2], code[pc + 3]
        greedy = op != _MIN_REPEAT_ONE
        # For a general REPEAT the tail is MAX_UNTIL or MIN_UNTIL.
        if op == _REPEAT:
            greedy = code[stop] == _MAX_UNTIL
        possessive = op == _POSSESSIVE_REPEAT or op == _POSSESSIVE_REPEAT_ONE

        if (
            op == _REPEAT_ONE
            or op == _MIN_REPEAT_ONE
            or op == _POSSESSIVE_REPEAT_ONE
        ):
            if greedy:
                count = self._max_repeat_one_count(pc + 4, pos, maximum)
                if count < minimum:
                    return
                while count >= minimum:
                    yield from self.run(following, pos + count, marks, halt)
                    if possessive:
                        return
                    count -= 1
            else:
                if minimum > 0 and self._max_repeat_one_count(pc + 4, pos, minimum) < minimum:
                    return
                max_possible = minimum + self._max_repeat_one_count(
                    pc + 4, pos + minimum, maximum - minimum
                )
                count = minimum
                while count <= max_possible:
                    yield from self.run(following, pos + count, marks, halt)
                    count += 1
            return

        if not possessive and maximum == 1 and minimum <= 1:
            if greedy:
                yield from self.run(pc + 4, pos, marks, halt)
                if minimum == 0:
                    yield from self.run(following, pos, marks, halt)
            else:
                if minimum == 0:
                    yield from self.run(following, pos, marks, halt)
                yield from self.run(pc + 4, pos, marks, halt)
            return

        for at, seen_marks in self._repeat_states(pc + 4, stop, pos, marks, minimum, maximum, greedy):
            yield from self.run(following, at, seen_marks, halt)
            if possessive:
                return

    def _match_one(self, pc, pos):
        if pos >= self.end:
            return False
        code, op = self.code, self.code[pc]
        if op == _LITERAL or op == _NOT_LITERAL:
            return (self.char(pos) == code[pc + 1]) == (op == _LITERAL)
        if op == _ANY or op == _ANY_ALL:
            return op == _ANY_ALL or self.char(pos) != 10
        if (
            op == _IN
            or op == _IN_IGNORE
            or op == _IN_UNI_IGNORE
            or op == _IN_LOC_IGNORE
        ):
            return self._in_charset(pc + 2, pos, op)
        if op == _CATEGORY:
            return _category_matches_code(code[pc + 1], self.char(pos))
        if (
            op == _LITERAL_IGNORE
            or op == _LITERAL_LOC_IGNORE
            or op == _LITERAL_UNI_IGNORE
            or op == _NOT_LITERAL_IGNORE
            or op == _NOT_LITERAL_LOC_IGNORE
            or op == _NOT_LITERAL_UNI_IGNORE
        ):
            negate = (
                op == _NOT_LITERAL_IGNORE
                or op == _NOT_LITERAL_LOC_IGNORE
                or op == _NOT_LITERAL_UNI_IGNORE
            )
            char = self.char(pos)
            char = (
                unicode_tolower(char)
                if (op == _LITERAL_UNI_IGNORE or op == _NOT_LITERAL_UNI_IGNORE)
                else ascii_tolower(char)
            )
            return (char == code[pc + 1]) != negate
        raise ValueError("unsupported single-character repeat opcode %r" % (op,))

    def _repeat_states(self, body, stop, pos, marks, minimum, maximum, greedy):
        # Keep depth-first branch order without consuming the Python call stack
        # once per matched character. Greedy exits follow their children; lazy
        # exits precede them. A stalled iteration can still satisfy a minimum.
        unbounded = maximum == MAXREPEAT
        visited = set() if unbounded else None
        stack = [(pos, marks, 0, None, False)]
        try:
            while stack:
                current, seen_marks, count, iterator, stalled = stack[-1]
                if iterator is None:
                    if not greedy and count >= minimum:
                        yield current, seen_marks
                    if count >= maximum or stalled and count >= minimum:
                        stack.pop()
                        if greedy and count >= minimum:
                            yield current, seen_marks
                        continue
                    iterator = iter(self.run(body, current, seen_marks, stop))
                    stack[-1] = current, seen_marks, count, iterator, stalled
                try:
                    nxt, next_marks = next(iterator)
                except StopIteration:
                    stack.pop()
                    if greedy and count >= minimum:
                        yield current, seen_marks
                    continue
                next_count = count + 1
                if visited is not None and next_count >= minimum and nxt != current:
                    state_key = (nxt, tuple(next_marks))
                    if state_key in visited:
                        continue
                    visited.add(state_key)
                stack.append((nxt, next_marks, next_count, None, nxt == current))
        finally:
            for _, _, _, iterator, _ in stack:
                if iterator is not None:
                    iterator.close()

    def _search_state(self, pc, pos, marks, anchored_end=False):
        code = self.code
        string = self.string
        copied = False
        while True:
            op = code[pc]
            if op == _SUCCESS:
                if anchored_end and pos != self.end:
                    return None
                return pos, marks
            if op == _FAILURE:
                return None
            if op == _MAX_UNTIL or op == _MIN_UNTIL:
                pc += 1
            elif op == _INFO:
                pc = pc + 1 + code[pc + 1]
            elif op == _LITERAL or op == _NOT_LITERAL:
                if pos < self.end and (self.char(pos) == code[pc + 1]) == (
                    op == _LITERAL
                ):
                    pc += 2
                    pos += 1
                else:
                    return None
            elif (
                op == _LITERAL_IGNORE
                or op == _LITERAL_LOC_IGNORE
                or op == _LITERAL_UNI_IGNORE
                or op == _NOT_LITERAL_IGNORE
                or op == _NOT_LITERAL_LOC_IGNORE
                or op == _NOT_LITERAL_UNI_IGNORE
            ):
                negate = (
                    op == _NOT_LITERAL_IGNORE
                    or op == _NOT_LITERAL_LOC_IGNORE
                    or op == _NOT_LITERAL_UNI_IGNORE
                )
                if pos >= self.end:
                    return None
                char = self.char(pos)
                if op == _LITERAL_UNI_IGNORE or op == _NOT_LITERAL_UNI_IGNORE:
                    char = unicode_tolower(char)
                else:
                    char = ascii_tolower(char)
                if (char == code[pc + 1]) == negate:
                    return None
                pc += 2
                pos += 1
            elif op == _ANY or op == _ANY_ALL:
                if pos >= self.end or (op == _ANY and self.char(pos) == 10):
                    return None
                pc += 1
                pos += 1
            elif op == _MARK:
                if not copied:
                    marks = list(marks)
                    copied = True
                marks[code[pc + 1]] = pos
                pc += 2
            elif op == _JUMP:
                pc = pc + 1 + code[pc + 1]
            elif op == _BRANCH:
                at = pc + 1
                while True:
                    arm = at + 1
                    if not _is_single_char_op(code[arm]) or self._match_one(
                        arm, pos
                    ):
                        found = self._search_state(arm, pos, marks, anchored_end)
                        if found is not None:
                            return found
                    at = at + code[at]
                    if code[at] == _FAILURE:
                        break
                return None
            elif (
                op == _IN
                or op == _IN_IGNORE
                or op == _IN_UNI_IGNORE
                or op == _IN_LOC_IGNORE
            ):
                if pos >= self.end:
                    return None
                if not self._in_charset(pc + 2, pos, op):
                    return None
                pc = pc + 1 + code[pc + 1]
                pos += 1
            elif op == _CATEGORY:
                if pos >= self.end:
                    return None
                if not _category_matches_code(code[pc + 1], self.char(pos)):
                    return None
                pc += 2
                pos += 1
            elif op == _AT:
                if not self._at_matches(code[pc + 1], pos):
                    return None
                pc += 2
            elif (
                op == _GROUPREF
                or op == _GROUPREF_IGNORE
                or op == _GROUPREF_LOC_IGNORE
                or op == _GROUPREF_UNI_IGNORE
            ):
                group = code[pc + 1]
                start, stop = marks[2 * group], marks[2 * group + 1]
                if start < 0 or stop < 0:
                    return None
                size = stop - start
                if pos + size > self.end:
                    return None
                wanted = string[start:stop]
                found = string[pos : pos + size]
                if op != _GROUPREF:
                    if self.bytes:
                        wanted = bytes(ascii_tolower(c) for c in wanted)
                        found = bytes(ascii_tolower(c) for c in found)
                    else:
                        wanted = wanted.lower()
                        found = found.lower()
                if found != wanted:
                    return None
                pc += 2
                pos += size
            elif op == _GROUPREF_EXISTS:
                group = code[pc + 1]
                if marks[2 * group] >= 0:
                    pc += 3
                else:
                    pc = pc + 1 + code[pc + 2]
            elif (
                op == _REPEAT
                or op == _REPEAT_ONE
                or op == _MIN_REPEAT_ONE
                or op == _POSSESSIVE_REPEAT
                or op == _POSSESSIVE_REPEAT_ONE
            ):
                if op == _REPEAT and code[pc + 2] == 0 and code[pc + 3] == 1:
                    stop = pc + 1 + code[pc + 1]
                    if code[stop] == _MAX_UNTIL:
                        bf = pc + 4
                        while code[bf] == _MARK:
                            bf += 2
                        bf_op = code[bf]
                        if _is_single_char_op(bf_op):
                            if not self._match_one(bf, pos):
                                pc = stop + 1
                                continue
                        elif (
                            (
                                bf_op == _REPEAT_ONE
                                or bf_op == _MIN_REPEAT_ONE
                                or bf_op == _POSSESSIVE_REPEAT_ONE
                            )
                            and code[bf + 2] >= 1
                            and not self._match_one(bf + 4, pos)
                        ):
                            pc = stop + 1
                            continue
                return self._repeat_search(pc, pos, marks, anchored_end)
            elif op == _ATOMIC_GROUP:
                stop = pc + 1 + code[pc + 1]
                states = self.run(pc + 2, pos, marks, stop)
                try:
                    for state in states:
                        return self._search_state(
                            stop, state[0], state[1], anchored_end
                        )
                finally:
                    states.close()
                return None
            elif op == _ASSERT or op == _ASSERT_NOT:
                stop = pc + 1 + code[pc + 1]
                direction = code[pc + 2]
                states = self._assert_states(pc + 3, pos, direction, stop, marks)
                if op == _ASSERT:
                    if not states:
                        return None
                    pc, marks = stop, states[0][1]
                    copied = False
                else:
                    if states:
                        return None
                    pc = stop
            else:
                raise ValueError("unsupported SRE opcode %r" % (op,))

    def _repeat_search(self, pc, pos, marks, anchored_end):
        code = self.code
        op = code[pc]
        stop = pc + 1 + code[pc + 1]
        following = (
            stop + 1
            if (op == _REPEAT or op == _POSSESSIVE_REPEAT)
            else stop
        )
        minimum, maximum = code[pc + 2], code[pc + 3]
        greedy = op != _MIN_REPEAT_ONE
        if op == _REPEAT:
            greedy = code[stop] == _MAX_UNTIL
        possessive = op == _POSSESSIVE_REPEAT or op == _POSSESSIVE_REPEAT_ONE

        if (
            op == _REPEAT_ONE
            or op == _MIN_REPEAT_ONE
            or op == _POSSESSIVE_REPEAT_ONE
        ):
            tail_pc = following
            while code[tail_pc] == _MARK:
                tail_pc += 2
            tail_op = code[tail_pc]
            tail_single = _is_single_char_op(tail_op)
            if greedy:
                count = self._max_repeat_one_count(pc + 4, pos, maximum)
                if count < minimum:
                    return None
                if not possessive and tail_op == _LITERAL:
                    tail_code = code[tail_pc + 1]
                    hi_pos = pos + count
                    if hi_pos < self.end and self.char(hi_pos) == tail_code:
                        found = self._search_state(
                            following, hi_pos, marks, anchored_end
                        )
                        if found is not None:
                            return found
                    if code[pc + 4] == _NOT_LITERAL and code[pc + 5] == tail_code:
                        return None
                    if count - minimum > 16 and self.is_str_or_bytes:
                        tail_lit = (
                            bytes([tail_code]) if self.bytes else chr(tail_code)
                        )
                        hi = hi_pos - 1
                        if hi >= self.end:
                            hi = self.end - 1
                        lo = pos + minimum
                        while hi >= lo:
                            idx = self.string.rfind(tail_lit, lo, hi + 1)
                            if idx < 0:
                                break
                            found = self._search_state(
                                following, idx, marks, anchored_end
                            )
                            if found is not None:
                                return found
                            hi = idx - 1
                        return None
                while count >= minimum:
                    if not tail_single or self._match_one(tail_pc, pos + count):
                        found = self._search_state(
                            following, pos + count, marks, anchored_end
                        )
                        if found is not None:
                            return found
                    if possessive:
                        return None
                    count -= 1
            else:
                if minimum > 0 and self._max_repeat_one_count(pc + 4, pos, minimum) < minimum:
                    return None
                max_possible = minimum + self._max_repeat_one_count(
                    pc + 4, pos + minimum, maximum - minimum
                )
                if (
                    tail_op == _LITERAL
                    and max_possible - minimum > 16
                    and self.is_str_or_bytes
                ):
                    tail_code = code[tail_pc + 1]
                    tail_lit = bytes([tail_code]) if self.bytes else chr(tail_code)
                    lo = pos + minimum
                    hi = pos + max_possible
                    if hi >= self.end:
                        hi = self.end - 1
                    while lo <= hi:
                        idx = self.string.find(tail_lit, lo, hi + 1)
                        if idx < 0:
                            break
                        found = self._search_state(following, idx, marks, anchored_end)
                        if found is not None:
                            return found
                        lo = idx + 1
                    return None
                count = minimum
                while count <= max_possible:
                    if not tail_single or self._match_one(tail_pc, pos + count):
                        found = self._search_state(
                            following, pos + count, marks, anchored_end
                        )
                        if found is not None:
                            return found
                    count += 1
            return None

        if not possessive and maximum == 1 and minimum <= 1:
            if greedy:
                found = self._search_state(pc + 4, pos, marks, anchored_end)
                if found is not None:
                    return found
                if minimum == 0:
                    return self._search_state(following, pos, marks, anchored_end)
                return None
            else:
                if minimum == 0:
                    found = self._search_state(following, pos, marks, anchored_end)
                    if found is not None:
                        return found
                return self._search_state(pc + 4, pos, marks, anchored_end)

        states = self._repeat_states(
            pc + 4, stop, pos, marks, minimum, maximum, greedy
        )
        try:
            for at, seen_marks in states:
                found = self._search_state(following, at, seen_marks, anchored_end)
                if found is not None:
                    return found
                if possessive:
                    return None
        finally:
            states.close()
        return None

    def _in_charset(self, pc, pos, in_op):
        code = self.code
        char = self.char(pos)
        negate = False
        matched = False
        while True:
            op = code[pc]
            if op == _FAILURE:
                break
            if op == _NEGATE:
                negate = True
                pc += 1
            elif op == _LITERAL:
                if self._charset_char(char, code[pc + 1], in_op):
                    matched = True
                pc += 2
            elif op == _RANGE or op == _RANGE_UNI_IGNORE:
                lo, hi = code[pc + 1], code[pc + 2]
                if in_op == _IN_UNI_IGNORE:
                    lowered = unicode_tolower(char)
                    if (lo <= lowered <= hi) or (lo <= char <= hi):
                        matched = True
                elif lo <= char <= hi:
                    matched = True
                pc += 3
            elif op == _CATEGORY:
                if _category_matches_code(code[pc + 1], char):
                    matched = True
                pc += 2
            elif op == _CHARSET:
                if char < 256:
                    word = code[pc + 1 + char // 32]
                    if (word >> (char % 32)) & 1:
                        matched = True
                pc += 1 + 8
            elif op == _BIGCHARSET:
                count = code[pc + 1]
                base = pc + 2
                if char < 65536:
                    block = 0
                    # mapping is 256 code words holding packed bytes
                    index = char // 256
                    word = code[base + index // 4]
                    block = (word >> ((index % 4) * 8)) & 255
                    chunk = base + 64 + block * 8
                    bit = char % 256
                    if (code[chunk + bit // 32] >> (bit % 32)) & 1:
                        matched = True
                pc += 2 + 64 + count * 8
            else:
                raise ValueError("unsupported charset opcode %r" % (op,))
        return matched != negate

    def _charset_char(self, char, wanted, in_op):
        if in_op == _IN_UNI_IGNORE:
            return unicode_tolower(char) == unicode_tolower(wanted) or char == wanted
        if in_op == _IN_IGNORE or in_op == _IN_LOC_IGNORE:
            return ascii_tolower(char) == ascii_tolower(wanted)
        return char == wanted

    def _at_matches(self, at, pos):
        if at == 0 or at == 2:
            return pos == 0
        if at == 1:
            return pos == 0 or self.char(pos - 1) == 10
        if at == 7:
            return pos == self.end
        if at == 5:
            return pos == self.end or (pos == self.end - 1 and self.char(pos) == 10)
        if at == 6:
            return pos == self.end or self.char(pos) == 10
        # word boundary codes 3, 4 and locale/unicode variants 8..11
        if at == 10 or at == 11:
            before = pos > 0 and _is_uni_word_code(self.char(pos - 1))
            after = pos < self.end and _is_uni_word_code(self.char(pos))
        else:
            before = pos > 0 and _is_ascii_word(self.char(pos - 1))
            after = pos < self.end and _is_ascii_word(self.char(pos))
        boundary = before != after
        return not boundary if (at == 4 or at == 9 or at == 11) else boundary


def compile(pattern, flags, code, ngroups, groupindex, indexgroup):
    return Pattern(pattern, flags, code, ngroups, groupindex, indexgroup)


def template(pattern, template_items):
    def apply(match):
        parts = []
        for item in template_items:
            if isinstance(item, int):
                text = match.group(item)
                parts.append(item if False else ("" if text is None else text))
            else:
                parts.append(item)
        if isinstance(pattern.pattern, bytes):
            return b"".join(parts)
        return "".join(parts)

    return apply


_SINGLE_CHAR_OPS = (
    _LITERAL,
    _NOT_LITERAL,
    _ANY,
    _ANY_ALL,
    _IN,
    _IN_IGNORE,
    _IN_UNI_IGNORE,
    _IN_LOC_IGNORE,
    _CATEGORY,
    _LITERAL_IGNORE,
    _LITERAL_LOC_IGNORE,
    _LITERAL_UNI_IGNORE,
    _NOT_LITERAL_IGNORE,
    _NOT_LITERAL_LOC_IGNORE,
    _NOT_LITERAL_UNI_IGNORE,
)


def _is_single_char_op(op):
    return (
        op == _LITERAL
        or op == _NOT_LITERAL
        or op == _ANY
        or op == _ANY_ALL
        or op == _IN
        or op == _IN_IGNORE
        or op == _IN_UNI_IGNORE
        or op == _IN_LOC_IGNORE
        or op == _CATEGORY
        or op == _LITERAL_IGNORE
        or op == _LITERAL_LOC_IGNORE
        or op == _LITERAL_UNI_IGNORE
        or op == _NOT_LITERAL_IGNORE
        or op == _NOT_LITERAL_LOC_IGNORE
        or op == _NOT_LITERAL_UNI_IGNORE
    )


def _advance_single_char_op(code, pc):
    op = code[pc]
    if op == _ANY or op == _ANY_ALL:
        return pc + 1
    if (
        op == _IN
        or op == _IN_IGNORE
        or op == _IN_UNI_IGNORE
        or op == _IN_LOC_IGNORE
    ):
        return pc + 1 + code[pc + 1]
    return pc + 2


def _collect_literals_and_pc(code, pc, is_bytes):
    chars = []
    n = len(code)
    while pc < n:
        op = code[pc]
        if op == _MARK:
            pc += 2
            continue
        if op == _LITERAL:
            chars.append(code[pc + 1])
            pc += 2
            continue
        break
    if not chars:
        return None, pc
    if is_bytes:
        return bytes(chars), pc
    return "".join(chr(c) for c in chars), pc


def _extract_required_literal(code, first_pc, is_bytes):
    n = len(code)
    pc = first_pc
    min_pre = 0
    max_pre = 0
    pred_pc = -1
    fixed_before = 0
    has_anchor = False
    best_fixed_lit = None
    while pc < n:
        op = code[pc]
        if op == _MARK:
            pc += 2
            continue
        if op == _AT:
            has_anchor = True
            pc += 2
            continue
        if op == _ASSERT or op == _ASSERT_NOT:
            has_anchor = True
            pc = pc + 1 + code[pc + 1]
            continue
        if (op == _REPEAT or op == _POSSESSIVE_REPEAT) and code[pc + 2] == 1:
            if min_pre == max_pre:
                pc += 4
                continue
            break
        if op == _LITERAL:
            lit, next_pc = _collect_literals_and_pc(code, pc, is_bytes)
            if lit is None:
                break
            if min_pre == max_pre:
                if best_fixed_lit is None or len(lit) > len(best_fixed_lit[1]):
                    best_fixed_lit = (min_pre, lit)
                min_pre += len(lit)
                max_pre += len(lit)
                pc = next_pc
                continue
            if best_fixed_lit is not None and len(best_fixed_lit[1]) >= 2:
                return (best_fixed_lit[0], best_fixed_lit[0], best_fixed_lit[1], -1, 0)
            return (min_pre, max_pre, lit, pred_pc, fixed_before)
        if _is_single_char_op(op):
            if min_pre != max_pre:
                break
            min_pre += 1
            max_pre += 1
            pc = _advance_single_char_op(code, pc)
            continue
        if (
            op == _REPEAT_ONE
            or op == _MIN_REPEAT_ONE
            or op == _POSSESSIVE_REPEAT_ONE
        ):
            lo, hi = code[pc + 2], code[pc + 3]
            if lo == hi and min_pre == max_pre:
                min_pre += lo
                max_pre += lo
                pc = pc + 1 + code[pc + 1]
                continue
            if min_pre == max_pre and hi == MAXREPEAT and best_fixed_lit is None:
                body_pc = pc + 4
                body_op = code[body_pc]
                fixed_before = min_pre
                min_pre = fixed_before + lo
                max_pre = MAXREPEAT
                if (
                    body_op == _ANY
                    and not has_anchor
                    and fixed_before == 0
                ):
                    pred_pc = -1
                else:
                    pred_pc = body_pc
                pc = pc + 1 + code[pc + 1]
                continue
            break
        break
    if best_fixed_lit is not None:
        return (best_fixed_lit[0], best_fixed_lit[0], best_fixed_lit[1], -1, 0)
    return None


def _extract_branch_literals(code, first_pc, is_bytes):
    n = len(code)
    pc = first_pc
    prefix_chars = []
    while pc < n:
        op = code[pc]
        if op == _MARK:
            pc += 2
            continue
        if op == _LITERAL:
            prefix_chars.append(code[pc + 1])
            pc += 2
            continue
        break
    if pc >= n or code[pc] != _BRANCH:
        return None
    arms = []
    at = pc + 1
    while at < n and code[at] != _FAILURE:
        arms.append(at + 1)
        at = at + code[at]
    if not arms:
        return None
    items = []
    for arm_pc in arms:
        cur = arm_pc
        offset = len(prefix_chars)
        best_offset = 0 if prefix_chars else -1
        best_chars = list(prefix_chars) if prefix_chars else None
        in_prefix_lit = True
        while cur < n:
            op = code[cur]
            if op == _MARK or op == _AT:
                cur += 2
                continue
            if op == _LITERAL:
                run_chars = []
                while cur < n:
                    rop = code[cur]
                    if rop == _MARK:
                        cur += 2
                        continue
                    if rop == _LITERAL:
                        run_chars.append(code[cur + 1])
                        cur += 2
                        continue
                    break
                if in_prefix_lit and prefix_chars:
                    combined = prefix_chars + run_chars
                    best_offset = 0
                    best_chars = combined
                elif best_chars is None or len(run_chars) > len(best_chars):
                    best_offset = offset
                    best_chars = run_chars
                offset += len(run_chars)
                in_prefix_lit = False
                continue
            if _is_single_char_op(op):
                offset += 1
                cur = _advance_single_char_op(code, cur)
                in_prefix_lit = False
                continue
            break
        if best_chars is None:
            return None
        lit = bytes(best_chars) if is_bytes else "".join(chr(c) for c in best_chars)
        items.append((best_offset, lit))
    if not items:
        return None
    return tuple(items)


def _charset_matches_ascii_code(code, pc, char, in_op):
    negate = False
    matched = False
    while True:
        op = code[pc]
        if op == _FAILURE:
            break
        if op == _NEGATE:
            negate = True
            pc += 1
        elif op == _LITERAL:
            wanted = code[pc + 1]
            if in_op == _IN_UNI_IGNORE:
                if unicode_tolower(char) == unicode_tolower(wanted) or char == wanted:
                    matched = True
            elif in_op == _IN_IGNORE or in_op == _IN_LOC_IGNORE:
                if ascii_tolower(char) == ascii_tolower(wanted):
                    matched = True
            elif char == wanted:
                matched = True
            pc += 2
        elif op == _RANGE or op == _RANGE_UNI_IGNORE:
            lo, hi = code[pc + 1], code[pc + 2]
            if in_op == _IN_UNI_IGNORE:
                lowered = unicode_tolower(char)
                if (lo <= lowered <= hi) or (lo <= char <= hi):
                    matched = True
            elif lo <= char <= hi:
                matched = True
            pc += 3
        elif op == _CATEGORY:
            if _category_matches_code(code[pc + 1], char):
                matched = True
            pc += 2
        elif op == _CHARSET:
            word = code[pc + 1 + char // 32]
            if (word >> (char % 32)) & 1:
                matched = True
            pc += 1 + 8
        elif op == _BIGCHARSET:
            count = code[pc + 1]
            base = pc + 2
            index = char // 256
            word = code[base + index // 4]
            block = (word >> ((index % 4) * 8)) & 255
            chunk = base + 64 + block * 8
            bit = char % 256
            if (code[chunk + bit // 32] >> (bit % 32)) & 1:
                matched = True
            pc += 2 + 64 + count * 8
        else:
            return True
    return matched != negate


def _charset_allows_non_ascii(code, pc, in_op):
    if in_op == _IN_UNI_IGNORE:
        return True
    while True:
        op = code[pc]
        if op == _FAILURE:
            return False
        if op == _NEGATE or op == _BIGCHARSET or op == _CATEGORY:
            return True
        if op == _LITERAL:
            if code[pc + 1] >= 128:
                return True
            pc += 2
        elif op == _RANGE or op == _RANGE_UNI_IGNORE:
            if code[pc + 2] >= 128:
                return True
            pc += 3
        elif op == _CHARSET:
            if (
                code[pc + 5] != 0
                or code[pc + 6] != 0
                or code[pc + 7] != 0
                or code[pc + 8] != 0
            ):
                return True
            pc += 1 + 8
        else:
            return True


def _fill_pc_start_chars(code, pc, allowed, non_ascii_box, depth):
    if depth > 8:
        return False
    n = len(code)
    while pc < n:
        op = code[pc]
        if op == _MARK:
            pc += 2
            continue
        if op == _AT:
            atcode = code[pc + 1]
            if atcode == 0 or atcode == 2 or atcode == 7:
                return True
            if atcode == 5 or atcode == 6:
                allowed[10] = 1
                return True
            pc += 2
            continue
        if op == _ASSERT or op == _ASSERT_NOT:
            pc = pc + 1 + code[pc + 1]
            continue
        if op == _LITERAL:
            c = code[pc + 1]
            if c < 128:
                allowed[c] = 1
            else:
                non_ascii_box[0] = True
            return True
        if (
            op == _LITERAL_IGNORE
            or op == _LITERAL_LOC_IGNORE
            or op == _LITERAL_UNI_IGNORE
        ):
            w = code[pc + 1]
            uni = op == _LITERAL_UNI_IGNORE
            c = 0
            while c < 128:
                folded = unicode_tolower(c) if uni else ascii_tolower(c)
                if folded == w:
                    allowed[c] = 1
                c += 1
            if uni:
                non_ascii_box[0] = True
            return True
        if op == _CATEGORY:
            cat = code[pc + 1]
            c = 0
            while c < 128:
                if _category_matches_code(cat, c):
                    allowed[c] = 1
                c += 1
            if (
                cat != _CATEGORY_DIGIT
                and cat != _CATEGORY_SPACE
                and cat != _CATEGORY_WORD
            ):
                non_ascii_box[0] = True
            return True
        if (
            op == _IN
            or op == _IN_IGNORE
            or op == _IN_UNI_IGNORE
            or op == _IN_LOC_IGNORE
        ):
            c = 0
            while c < 128:
                if _charset_matches_ascii_code(code, pc + 2, c, op):
                    allowed[c] = 1
                c += 1
            if _charset_allows_non_ascii(code, pc + 2, op):
                non_ascii_box[0] = True
            return True
        if (
            op == _REPEAT_ONE
            or op == _MIN_REPEAT_ONE
            or op == _POSSESSIVE_REPEAT_ONE
        ):
            if code[pc + 2] >= 1:
                pc += 4
                continue
            return _fill_pc_start_chars(
                code, pc + 4, allowed, non_ascii_box, depth + 1
            ) and _fill_pc_start_chars(
                code, pc + 1 + code[pc + 1], allowed, non_ascii_box, depth + 1
            )
        if op == _REPEAT or op == _POSSESSIVE_REPEAT:
            if code[pc + 2] >= 1:
                pc += 4
                continue
            return _fill_pc_start_chars(
                code, pc + 4, allowed, non_ascii_box, depth + 1
            ) and _fill_pc_start_chars(
                code, pc + 2 + code[pc + 1], allowed, non_ascii_box, depth + 1
            )
        if op == _BRANCH:
            at = pc + 1
            any_arm = False
            while at < n and code[at] != _FAILURE:
                any_arm = True
                if not _fill_pc_start_chars(
                    code, at + 1, allowed, non_ascii_box, depth + 1
                ):
                    return False
                at = at + code[at]
            return any_arm
        return False
    return False


def _build_ascii_start_table(code, first_pc, branch_pcs):
    if not code or code[0] != _INFO:
        return None
    allowed = bytearray(128)
    non_ascii_box = [False]
    if branch_pcs is not None:
        for arm_pc in branch_pcs:
            if not _fill_pc_start_chars(code, arm_pc, allowed, non_ascii_box, 0):
                return None
    else:
        if not _fill_pc_start_chars(code, first_pc, allowed, non_ascii_box, 0):
            return None
    count = 0
    i = 0
    while i < 128:
        count += allowed[i]
        i += 1
    if count >= 96:
        return None
    return bytes(allowed), non_ascii_box[0]


class Pattern:
    def __init__(self, pattern, flags, code, ngroups, groupindex, indexgroup):
        self.pattern = pattern
        self.flags = flags
        self._code = code
        self.groups = ngroups
        self._ngroups = ngroups + 1
        self.groupindex = groupindex
        self._indexgroup = indexgroup
        start_pc = 1 + code[1] if code and code[0] == _INFO else 0
        self._start_pc = start_pc
        pc = start_pc
        while pc < len(code) and code[pc] == _MARK:
            pc += 2
        self._first_pc = pc
        op = code[pc] if pc < len(code) else _FAILURE
        self._first_op = op
        self._single_char_only = False
        self._branch_pcs = None
        self._min_width = 0
        self._is_literal = False
        self._anchored_start = False
        self._req_lit = None
        self._branch_lits = None
        self._ascii_start_table = None
        self._non_ascii_can_start = True
        self._empty_marks = [-1] * (2 * self._ngroups)
        if _is_single_char_op(op):
            if op == _ANY or op == _ANY_ALL:
                next_pc = pc + 1
            elif (
                op == _IN
                or op == _IN_IGNORE
                or op == _IN_UNI_IGNORE
                or op == _IN_LOC_IGNORE
            ):
                next_pc = pc + 1 + code[pc + 1]
            else:
                next_pc = pc + 2
            if (
                self.groups == 0
                and next_pc < len(code)
                and code[next_pc] == _SUCCESS
            ):
                self._single_char_only = True
        elif op == _BRANCH:
            arms = []
            at = pc + 1
            while at < len(code) and code[at] != _FAILURE:
                arm = at + 1
                while arm < len(code) and code[arm] == _MARK:
                    arm += 2
                arms.append(arm)
                at = at + code[at]
            if arms:
                self._branch_pcs = tuple(arms)

        if code and code[0] == _INFO:
            min_w = code[3]
            if min_w > 0:
                self._min_width = min_w
            info_flags = code[2]
            if (info_flags & SRE_INFO_LITERAL) and self.groups == 0:
                self._is_literal = True
        if self._first_op == _AT and (
            code[self._first_pc + 1] == 0 or code[self._first_pc + 1] == 2
        ):
            self._anchored_start = True
        elif self._branch_pcs is not None:
            all_anchored = True
            for arm_pc in self._branch_pcs:
                if not (
                    arm_pc < len(code)
                    and code[arm_pc] == _AT
                    and (code[arm_pc + 1] == 0 or code[arm_pc + 1] == 2)
                ):
                    all_anchored = False
                    break
            if all_anchored:
                self._anchored_start = True
        if not self._anchored_start and not self._is_literal:
            is_bytes = isinstance(pattern, bytes)
            self._branch_lits = _extract_branch_literals(
                code, self._first_pc, is_bytes
            )
            if self._branch_lits is None:
                self._req_lit = _extract_required_literal(
                    code, self._first_pc, is_bytes
                )
            table_info = _build_ascii_start_table(
                code, self._first_pc, self._branch_pcs
            )
            if table_info is not None:
                self._ascii_start_table, self._non_ascii_can_start = table_info

    def _engine(self, string, endpos):
        length = len(string)
        end = length if (endpos is None or endpos > length) else (0 if endpos < 0 else endpos)
        engine = _Engine(self._code, string, end)
        engine.groups = self._ngroups
        return engine

    def _pc_can_start_at(self, engine, pc, pos):
        code = self._code
        while pc < len(code):
            op = code[pc]
            if op == _MARK:
                pc += 2
                continue
            if _is_single_char_op(op):
                return engine._match_one(pc, pos)
            if (
                op == _REPEAT_ONE
                or op == _MIN_REPEAT_ONE
                or op == _POSSESSIVE_REPEAT_ONE
            ):
                if engine._match_one(pc + 4, pos):
                    return True
                if code[pc + 2] >= 1:
                    return False
                pc = pc + 1 + code[pc + 1]
                continue
            if op == _REPEAT or op == _POSSESSIVE_REPEAT:
                if code[pc + 2] >= 1:
                    pc += 4
                    continue
                return self._pc_can_start_at(
                    engine, pc + 4, pos
                ) or self._pc_can_start_at(engine, pc + 2 + code[pc + 1], pos)
            if op == _AT:
                return engine._at_matches(code[pc + 1], pos)
            return True
        return True

    def _can_start_at(self, engine, pos):
        table = self._ascii_start_table
        if table is not None and 0 < pos < engine.end:
            c = engine.char(pos)
            if c < 128:
                if not table[c]:
                    return False
            elif not self._non_ascii_can_start:
                return False
        branch_pcs = self._branch_pcs
        if branch_pcs is not None:
            i = 0
            n = len(branch_pcs)
            while i < n:
                if self._pc_can_start_at(engine, branch_pcs[i], pos):
                    return True
                i += 1
            return False
        return self._pc_can_start_at(engine, self._first_pc, pos)

    def _match_at_engine(self, engine, pos, anchored_end=False):
        if pos + self._min_width > engine.end:
            return None
        if not self._can_start_at(engine, pos):
            return None
        if self._single_char_only:
            if anchored_end and pos + 1 != engine.end:
                return None
            return Match(self, engine.string, [pos, pos + 1], pos, engine.end)
        found = engine._search_state(self._start_pc, pos, self._empty_marks, anchored_end)
        if found is None:
            return None
        stop, found_marks = found
        return Match(self, engine.string, [pos, stop, *found_marks], pos, engine.end)

    def _match_at(self, string, pos, endpos, anchored_end=False):
        engine = self._engine(string, endpos)
        return self._match_at_engine(engine, pos, anchored_end)

    def match(self, string, pos=0, endpos=None):
        pos = 0 if pos < 0 else pos
        return self._match_at(string, pos, endpos)

    def fullmatch(self, string, pos=0, endpos=None):
        pos = 0 if pos < 0 else pos
        return self._match_at(string, pos, endpos, anchored_end=True)

    def _search_engine(self, engine, start):
        engine_end = engine.end
        min_w = self._min_width
        if start + min_w > engine_end:
            return None
        if self._anchored_start:
            if start > 0:
                return None
            return self._match_at_engine(engine, 0)
        string = engine.string
        if self._is_literal and engine.is_str_or_bytes:
            pat = self.pattern
            idx = string.find(pat, start, engine_end)
            if idx < 0:
                return None
            stop = idx + len(pat)
            return Match(self, string, [idx, stop], start, engine_end)
        req_lit = self._req_lit
        if req_lit is not None and engine.is_str_or_bytes:
            min_pre, max_pre, lit, pred_pc, fixed_before = req_lit
            if min_pre == max_pre:
                offset = min_pre
                while start + min_w <= engine_end:
                    idx = string.find(lit, start + offset, engine_end)
                    if idx < 0:
                        return None
                    cand = idx - offset
                    found = self._match_at_engine(engine, cand)
                    if found is not None:
                        return found
                    start = cand + 1
                return None
            elif pred_pc < 0:
                # Unbounded .* or .+ before literal: if found, match starts at `start` (for .*)
                # or after the last newline before `idx` (for _ANY).
                idx = string.find(lit, start + min_pre, engine_end)
                if idx < 0:
                    return None
                while start + min_w <= engine_end:
                    if idx < start + min_pre:
                        idx = string.find(lit, start + min_pre, engine_end)
                        if idx < 0:
                            return None
                    found = self._match_at_engine(engine, start)
                    if found is not None:
                        return found
                    # Advance start to after the next newline if _ANY failed to cross a newline
                    nl = string.find(b"\n" if engine.bytes else "\n", start, idx)
                    if nl >= 0:
                        start = nl + 1
                    else:
                        start += 1
                return None
            else:
                # Variable-length prefix with unbounded single-char repeat pred_pc
                min_start = start
                lit_search_from = start + min_pre
                while min_start + min_w <= engine_end:
                    if lit_search_from < min_start + min_pre:
                        lit_search_from = min_start + min_pre
                    idx = string.find(lit, lit_search_from, engine_end)
                    if idx < 0:
                        return None
                    var_start = idx
                    limit_var = min_start + fixed_before
                    while var_start > limit_var and engine._match_one(pred_pc, var_start - 1):
                        var_start -= 1
                    cand = var_start - fixed_before
                    if cand < min_start:
                        cand = min_start
                    max_cand = idx - min_pre
                    while cand <= max_cand:
                        found = self._match_at_engine(engine, cand)
                        if found is not None:
                            return found
                        cand += 1
                    next_min_start = var_start - fixed_before
                    if max_cand + 1 > next_min_start:
                        next_min_start = max_cand + 1
                    if next_min_start > min_start:
                        min_start = next_min_start
                    lit_search_from = idx + 1
                return None
        branch_lits = self._branch_lits
        if branch_lits is not None and engine.is_str_or_bytes:
            n_arms = len(branch_lits)
            # Cache next occurrence position for each arm across _search_engine calls
            next_cand = engine._branch_next_cand
            if next_cand is None:
                next_cand = [-2] * n_arms
                engine._branch_next_cand = next_cand
            while start + min_w <= engine_end:
                best_cand = engine_end + 1
                i = 0
                while i < n_arms:
                    c_pos = next_cand[i]
                    if c_pos != -1 and c_pos < start:
                        off, lit = branch_lits[i]
                        idx = string.find(lit, start + off, engine_end)
                        if idx < 0:
                            c_pos = -1
                        else:
                            c_pos = idx - off
                        next_cand[i] = c_pos
                    if c_pos >= 0 and c_pos < best_cand:
                        best_cand = c_pos
                    i += 1
                if best_cand > engine_end:
                    return None
                found = self._match_at_engine(engine, best_cand)
                if found is not None:
                    return found
                start = best_cand + 1
            return None
        limit = engine_end - min_w
        table = self._ascii_start_table
        if table is not None:
            non_ascii_ok = self._non_ascii_can_start
            while start <= limit:
                if 0 < start < engine_end:
                    c = engine.char(start)
                    if c < 128:
                        if not table[c]:
                            start += 1
                            continue
                    elif not non_ascii_ok:
                        start += 1
                        continue
                found = self._match_at_engine(engine, start)
                if found is not None:
                    return found
                start += 1
            return None
        while start <= limit:
            found = self._match_at_engine(engine, start)
            if found is not None:
                return found
            start += 1
        return None

    def search(self, string, pos=0, endpos=None):
        engine = self._engine(string, endpos)
        start = 0 if pos < 0 else pos
        return self._search_engine(engine, start)

    def finditer(self, string, pos=0, endpos=None):
        engine = self._engine(string, endpos)
        start = 0 if pos < 0 else pos
        limit = engine.end
        while start <= limit:
            found = self._search_engine(engine, start)
            if found is None:
                return
            yield found
            begin, stop = found.span()
            start = stop if stop > begin else stop + 1

    def findall(self, string, pos=0, endpos=None):
        result = []
        engine = self._engine(string, endpos)
        start = 0 if pos < 0 else pos
        limit = engine.end
        if self._is_literal and engine.is_str_or_bytes:
            pat = self.pattern
            pat_len = len(pat)
            if pat_len > 0:
                while start <= limit:
                    idx = string.find(pat, start, limit)
                    if idx < 0:
                        break
                    result.append(string[idx : idx + pat_len])
                    start = idx + pat_len
                return result
        groups = self._ngroups
        while start <= limit:
            found = self._search_engine(engine, start)
            if found is None:
                break
            if groups == 1:
                result.append(found.group())
            elif groups == 2:
                result.append(found.group(1))
            else:
                result.append(found.groups())
            begin, stop = found.span()
            start = stop if stop > begin else stop + 1
        return result

    def split(self, string, maxsplit=0):
        result = []
        last = 0
        count = 0
        engine = self._engine(string, None)
        start = 0
        limit = engine.end
        groups = self._ngroups
        while start <= limit:
            if maxsplit and count >= maxsplit:
                break
            found = self._search_engine(engine, start)
            if found is None:
                break
            begin, stop = found.span()
            result.append(string[last:begin])
            group = 1
            while group < groups:
                result.append(found.group(group))
                group += 1
            last = stop
            count += 1
            start = stop if stop > begin else stop + 1
        result.append(string[last:])
        return result

    def subn(self, repl, string, count=0):
        engine = self._engine(string, None)
        limit = engine.end
        found = self._search_engine(engine, 0)
        if found is None or (count and count < 0):
            return (string[:] if isinstance(string, (str, bytes)) else _join_like(string, [string])), 0
        is_callable = callable(repl)
        literal_repl = False
        if not is_callable:
            if isinstance(repl, str):
                literal_repl = "\\" not in repl
            elif isinstance(repl, bytes):
                literal_repl = b"\\" not in repl
        result = []
        last = 0
        made = 0
        while found is not None:
            if count and made >= count:
                break
            begin, stop = found.span()
            result.append(string[last:begin])
            if literal_repl:
                result.append(repl)
            else:
                result.append(self._replacement(repl, found))
            last = stop
            made += 1
            start = stop if stop > begin else stop + 1
            if start > limit or (count and made >= count):
                break
            found = self._search_engine(engine, start)
        result.append(string[last:])
        joined = _join_like(string, result)
        return joined, made

    def sub(self, repl, string, count=0):
        if (
            self._is_literal
            and self._min_width > 0
            and isinstance(string, (str, bytes))
            and isinstance(repl, (str, bytes))
        ):
            no_backslash = (
                ("\\" not in repl) if isinstance(repl, str) else (b"\\" not in repl)
            )
            if no_backslash:
                if count < 0:
                    return string[:]
                if count == 0:
                    return string.replace(self.pattern, repl)
                return string.replace(self.pattern, repl, count)
        return self.subn(repl, string, count)[0]

    def _replacement(self, repl, match):
        if callable(repl):
            return repl(match)
        result = []
        index = 0
        text = repl
        while index < len(text):
            char = text[index]
            index += 1
            if char != "\\" and char != b"\\":
                result.append(char if isinstance(text, str) else bytes([char]))
                continue
            if index >= len(text):
                raise ValueError("Trailing backslash in replacement")
            esc = text[index]
            index += 1
            esc_char = chr(esc) if isinstance(text, bytes) else esc
            if esc_char == "g":
                if index >= len(text) or text[index] != (60 if isinstance(text, bytes) else "<"):
                    raise ValueError("missing < in replacement")
                close = index + 1
                while text[close] != (62 if isinstance(text, bytes) else ">"):
                    close += 1
                name = text[index + 1:close]
                index = close + 1
                if isinstance(text, bytes):
                    name_text = name.decode("latin1")
                else:
                    name_text = name
                group = int(name_text) if name_text.isdecimal() else self.groupindex[name_text]
                value = match.group(group)
                result.append(_empty_like(text) if value is None else value)
            elif esc_char.isdigit() and esc_char != "0":
                number = esc_char
                if index < len(text):
                    nxt = text[index]
                    nxt_char = chr(nxt) if isinstance(text, bytes) else nxt
                    if nxt_char.isdigit():
                        number += nxt_char
                        index += 1
                value = match.group(int(number))
                result.append(_empty_like(text) if value is None else value)
            elif esc_char == "n":
                result.append("\n" if isinstance(text, str) else b"\n")
            elif esc_char == "t":
                result.append("\t" if isinstance(text, str) else b"\t")
            elif esc_char == "r":
                result.append("\r" if isinstance(text, str) else b"\r")
            elif esc_char == "\\":
                result.append("\\" if isinstance(text, str) else b"\\")
            else:
                result.append(_empty_like(text) + (esc_char if isinstance(text, str) else bytes([esc])))
                result[-1] = ("\\" + esc_char) if isinstance(text, str) else b"\\" + bytes([esc])
        return _join_like(text, result)

    def scanner(self, string, pos=0, endpos=None):
        return _Scanner(self, string, pos, endpos)

    def __repr__(self):
        return "re.compile(" + repr(self.pattern) + ")"


def _empty_like(text):
    return b"" if isinstance(text, bytes) else ""


def _join_like(sample, parts):
    if isinstance(sample, bytes):
        return b"".join(parts)
    return "".join(parts)


class _Scanner:
    def __init__(self, pattern, string, pos, endpos):
        self.pattern = pattern
        self._matches = pattern.finditer(string, pos, endpos)

    def match(self):
        try:
            return next(self._matches)
        except StopIteration:
            return None


class Match:
    def __init__(self, pattern, string, marks, pos, endpos):
        self.re = pattern
        self.string = string
        self._marks = marks
        self.pos = pos
        self.endpos = endpos

    @property
    def lastindex(self):
        marks = self._marks
        group = self.re._ngroups - 1
        while group > 0:
            if marks[2 * group] >= 0:
                return group
            group -= 1
        return None

    @property
    def lastgroup(self):
        lastindex = self.lastindex
        if lastindex is not None:
            pattern = self.re
            indexgroup = pattern._indexgroup
            if indexgroup and lastindex < len(indexgroup):
                return indexgroup[lastindex]
            if pattern.groupindex:
                for name, index in pattern.groupindex.items():
                    if index == lastindex:
                        return name
        return None

    def span(self, group=0):
        return self.start(group), self.end(group)

    def start(self, group=0):
        if isinstance(group, str):
            group = self.re.groupindex[group]
        return self._marks[2 * group]

    def end(self, group=0):
        if isinstance(group, str):
            group = self.re.groupindex[group]
        return self._marks[2 * group + 1]

    def _group_one(self, group):
        if isinstance(group, str):
            group = self.re.groupindex[group]
        start = self._marks[2 * group]
        if start < 0:
            return None
        return self.string[start : self._marks[2 * group + 1]]

    def group(self, *groups):
        n = len(groups)
        if n == 0:
            return self._group_one(0)
        if n == 1:
            return self._group_one(groups[0])
        values = []
        i = 0
        while i < n:
            values.append(self._group_one(groups[i]))
            i += 1
        return tuple(values)

    def groups(self, default=None):
        values = []
        marks = self._marks
        string = self.string
        group = 1
        limit = self.re._ngroups
        while group < limit:
            start = marks[2 * group]
            values.append(
                default if start < 0 else string[start : marks[2 * group + 1]]
            )
            group += 1
        return tuple(values)

    def groupdict(self, default=None):
        if not self.re.groupindex:
            return {}
        return {
            name: (self._group_one(index) if self._marks[2 * index] >= 0 else default)
            for name, index in self.re.groupindex.items()
        }

    def expand(self, template_text):
        return self.re._replacement(template_text, self)

    def __getitem__(self, group):
        return self._group_one(group)

    def __repr__(self):
        return "<re.Match object; span=" + repr(self.span()) + ", match=" + repr(self.group()) + ">"
