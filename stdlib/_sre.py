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


def ascii_iscased(code):
    return 65 <= code <= 90 or 97 <= code <= 122


def ascii_tolower(code):
    return code + 32 if 65 <= code <= 90 else code


def unicode_iscased(code):
    char = chr(code)
    return char.lower() != char or char.upper() != char


def unicode_tolower(code):
    lowered = chr(code).lower()
    return ord(lowered) if len(lowered) == 1 else code


def _is_word(char):
    return char == "_" or char.isalnum()


def _is_ascii_word(code):
    return (
        (48 <= code <= 57)
        or (65 <= code <= 90)
        or (97 <= code <= 122)
        or code == 95
    )


def _is_ascii_space(code):
    return 9 <= code <= 13 or code == 32


def _category_matches(category, char, code):
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
        return char.isdecimal()
    if category == 11:
        return not char.isdecimal()
    if category == 12:
        return char.isspace()
    if category == 13:
        return not char.isspace()
    if category == 14:
        return _is_word(char)
    if category == 15:
        return not _is_word(char)
    if category == 16:
        return char in "\n\r\v\f\x1c\x1d\x1e\x85\u2028\u2029"
    return char not in "\n\r\v\f\x1c\x1d\x1e\x85\u2028\u2029"


class _Engine:
    def __init__(self, code, string, end):
        self.code = code
        self.string = string
        self.end = end
        self.bytes = isinstance(string, bytes)

    def char(self, pos):
        value = self.string[pos]
        return value if self.bytes else ord(value)

    def text_char(self, pos):
        value = self.string[pos]
        return chr(value) if self.bytes else value

    def run(self, pc, pos, marks, halt=-1):
        code = self.code
        string = self.string
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
            if op == _INFO:
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
                marks = list(marks)
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
                if not _category_matches(code[pc + 1], self.text_char(pos), self.char(pos)):
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
            # re's compiler proves these bodies consume exactly one character
            # and contain no capture marks. Keep repetition in this activation.
            count = 0
            while count < minimum:
                if not self._match_one(pc + 4, pos + count):
                    return
                count += 1
            if greedy:
                while count < maximum and self._match_one(pc + 4, pos + count):
                    count += 1
                while count >= minimum:
                    yield from self.run(following, pos + count, marks, halt)
                    if possessive:
                        return
                    count -= 1
            else:
                while True:
                    yield from self.run(following, pos + count, marks, halt)
                    if count >= maximum or not self._match_one(pc + 4, pos + count):
                        return
                    count += 1
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
            return _category_matches(
                code[pc + 1], self.text_char(pos), self.char(pos)
            )
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
                stack.append((nxt, next_marks, count + 1, None, nxt == current))
        finally:
            for _, _, _, iterator, _ in stack:
                if iterator is not None:
                    iterator.close()

    def _search_state(self, pc, pos, marks, anchored_end=False):
        code = self.code
        string = self.string
        while True:
            op = code[pc]
            if op == _SUCCESS:
                if anchored_end and pos != self.end:
                    return None
                return pos, marks
            if op == _FAILURE:
                return None
            if op == _INFO:
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
                marks = list(marks)
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
                if not _category_matches(
                    code[pc + 1], self.text_char(pos), self.char(pos)
                ):
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
            count = 0
            while count < minimum:
                if not self._match_one(pc + 4, pos + count):
                    return None
                count += 1
            if greedy:
                while count < maximum and self._match_one(pc + 4, pos + count):
                    count += 1
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
                while True:
                    if not tail_single or self._match_one(tail_pc, pos + count):
                        found = self._search_state(
                            following, pos + count, marks, anchored_end
                        )
                        if found is not None:
                            return found
                    if count >= maximum or not self._match_one(pc + 4, pos + count):
                        return None
                    count += 1
            return None

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
        text = None
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
                if text is None:
                    text = self.text_char(pos)
                if _category_matches(code[pc + 1], text, char):
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
            before = pos > 0 and _is_word(self.text_char(pos - 1))
            after = pos < self.end and _is_word(self.text_char(pos))
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


class Pattern:
    def __init__(self, pattern, flags, code, ngroups, groupindex, indexgroup):
        self.pattern = pattern
        self.flags = flags
        self._code = code
        self.groups = ngroups
        self._ngroups = ngroups + 1
        self.groupindex = groupindex
        self._indexgroup = indexgroup
        pc = 1 + code[1] if code and code[0] == _INFO else 0
        while pc < len(code) and code[pc] == _MARK:
            pc += 2
        self._first_pc = pc
        op = code[pc] if pc < len(code) else _FAILURE
        self._first_op = op
        self._single_char_only = False
        self._branch_pcs = None
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

    def _engine(self, string, endpos):
        length = len(string)
        end = length if (endpos is None or endpos > length) else (0 if endpos < 0 else endpos)
        engine = _Engine(self._code, string, end)
        engine.groups = self._ngroups
        return engine

    def _pc_can_start_at(self, engine, pc, pos):
        code = self._code
        op = code[pc] if pc < len(code) else _FAILURE
        if _is_single_char_op(op):
            return engine._match_one(pc, pos)
        if (
            op == _REPEAT_ONE
            or op == _MIN_REPEAT_ONE
            or op == _POSSESSIVE_REPEAT_ONE
        ):
            if code[pc + 2] >= 1:
                return engine._match_one(pc + 4, pos)
            return True
        if op == _AT:
            return engine._at_matches(code[pc + 1], pos)
        return True

    def _can_start_at(self, engine, pos):
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
        if not self._can_start_at(engine, pos):
            return None
        if self._single_char_only:
            if anchored_end and pos + 1 != engine.end:
                return None
            return Match(self, engine.string, [pos, pos + 1], pos, engine.end)
        marks = [-1] * (2 * self._ngroups)
        found = engine._search_state(0, pos, marks, anchored_end)
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
        anchored_start = (
            self._first_op == _AT
            and (self._code[self._first_pc + 1] == 0 or self._code[self._first_pc + 1] == 2)
        )
        while start <= engine_end:
            found = self._match_at_engine(engine, start)
            if found is not None:
                return found
            if anchored_start:
                return None
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
        result = []
        last = 0
        made = 0
        while found is not None:
            if count and made >= count:
                break
            begin, stop = found.span()
            result.append(string[last:begin])
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
        self.lastindex = None
        group = pattern._ngroups - 1
        while group > 0:
            if marks[2 * group] >= 0:
                self.lastindex = group
                break
            group -= 1
        self.lastgroup = None
        if self.lastindex is not None:
            indexgroup = pattern._indexgroup
            if indexgroup and self.lastindex < len(indexgroup):
                self.lastgroup = indexgroup[self.lastindex]
            elif pattern.groupindex:
                for name, index in pattern.groupindex.items():
                    if index == self.lastindex:
                        self.lastgroup = name
                        break

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
