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


def _category_matches(category, char, code):
    if category in (0, 10):
        return char.isdecimal()
    if category in (1, 11):
        return not char.isdecimal()
    if category in (2, 12):
        return char.isspace()
    if category in (3, 13):
        return not char.isspace()
    if category in (4, 8, 14):
        return _is_word(char)
    if category in (5, 9, 15):
        return not _is_word(char)
    if category in (6, 16):
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
            elif op in (_LITERAL_IGNORE, _LITERAL_LOC_IGNORE, _LITERAL_UNI_IGNORE,
                        _NOT_LITERAL_IGNORE, _NOT_LITERAL_LOC_IGNORE,
                        _NOT_LITERAL_UNI_IGNORE):
                negate = op in (_NOT_LITERAL_IGNORE, _NOT_LITERAL_LOC_IGNORE,
                                _NOT_LITERAL_UNI_IGNORE)
                if pos >= self.end:
                    return
                char = self.char(pos)
                if op in (_LITERAL_UNI_IGNORE, _NOT_LITERAL_UNI_IGNORE):
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
                    yield from self.run(at + 1, pos, marks, halt)
                    at = at + code[at]
                    if code[at] == _FAILURE:
                        break
                return
            elif op in (_IN, _IN_IGNORE, _IN_UNI_IGNORE, _IN_LOC_IGNORE):
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
            elif op in (_GROUPREF, _GROUPREF_IGNORE, _GROUPREF_LOC_IGNORE,
                        _GROUPREF_UNI_IGNORE):
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
                    pc = pc + 2 + code[pc + 2]
            elif op in (_REPEAT, _REPEAT_ONE, _MIN_REPEAT_ONE,
                        _POSSESSIVE_REPEAT, _POSSESSIVE_REPEAT_ONE):
                yield from self._repeat(pc, pos, marks, halt)
                return
            elif op == _ATOMIC_GROUP:
                stop = pc + 1 + code[pc + 1]
                for state in self.run(pc + 2, pos, marks, stop):
                    yield from self.run(stop, state[0], state[1], halt)
                    break
                return
            elif op in (_ASSERT, _ASSERT_NOT):
                stop = pc + 1 + code[pc + 1]
                direction = code[pc + 2]
                states = self._assert_states(pc + 3, pos, direction, stop)
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

    def _assert_states(self, pc, pos, direction, stop):
        if direction == 0:
            return list(self.run(pc, pos, self._fresh_marks(), stop))
        start = pos - direction
        if start < 0:
            return []
        return [state for state in self.run(pc, start, self._fresh_marks(), stop)
                if state[0] == pos]

    def _fresh_marks(self):
        return [-1] * (2 * self.groups)

    groups = 1

    def _repeat(self, pc, pos, marks, halt):
        code = self.code
        op = code[pc]
        stop = pc + 1 + code[pc + 1]
        following = stop + 1 if op == _REPEAT else stop
        minimum, maximum = code[pc + 2], code[pc + 3]
        greedy = op not in (_MIN_REPEAT_ONE,)
        # For a general REPEAT the tail is MAX_UNTIL or MIN_UNTIL.
        if op == _REPEAT:
            greedy = code[stop] == _MAX_UNTIL
        possessive = op in (_POSSESSIVE_REPEAT, _POSSESSIVE_REPEAT_ONE)

        def repeated(at, seen_marks, count):
            exits = []

            def search(current, current_marks, n):
                if n >= minimum:
                    exits.append((current, current_marks))
                if n < maximum:
                    for nxt, nm in self.run(pc + 4, current, current_marks, stop):
                        if nxt == current and nm == current_marks:
                            continue
                        search(nxt, nm, n + 1)

            search(at, seen_marks, count)
            return exits

        exits = repeated(pos, marks, 0)
        if not greedy:
            exits.reverse()
        if possessive:
            exits = exits[-1:] if greedy and exits else exits[:1]
            for at, seen_marks in exits:
                yield from self.run(following, at, seen_marks, halt)
            return
        # Greedy enumeration collected fewest-first; present most-first.
        if greedy:
            exits.reverse()
        for at, seen_marks in exits:
            yield from self.run(following, at, seen_marks, halt)

    def _in_charset(self, pc, pos, in_op):
        code = self.code
        char = self.char(pos)
        text = self.text_char(pos)
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
            elif op in (_RANGE, _RANGE_UNI_IGNORE):
                lo, hi = code[pc + 1], code[pc + 2]
                if in_op == _IN_UNI_IGNORE:
                    if unicode_tolower(char) in range(lo, hi + 1) or char in range(lo, hi + 1):
                        matched = True
                elif lo <= char <= hi:
                    matched = True
                pc += 3
            elif op == _CATEGORY:
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
        if in_op in (_IN_IGNORE, _IN_LOC_IGNORE):
            return ascii_tolower(char) == ascii_tolower(wanted)
        return char == wanted

    def _at_matches(self, at, pos):
        string = self.string
        if at in (0, 2):
            return pos == 0
        if at == 1:
            return pos == 0 or self.char(pos - 1) == 10
        if at == 7:
            return pos == self.end
        if at == 6:
            return pos == self.end or (pos == self.end - 1 and self.char(pos) == 10)
        if at == 5:
            return pos == self.end or self.char(pos) == 10
        # word boundary codes 3, 4 and locale/unicode variants 8..11
        before = pos > 0 and _is_word(self.text_char(pos - 1))
        after = pos < self.end and _is_word(self.text_char(pos))
        boundary = before != after
        return not boundary if at in (4, 9, 11) else boundary


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


class Pattern:
    def __init__(self, pattern, flags, code, ngroups, groupindex, indexgroup):
        self.pattern = pattern
        self.flags = flags
        self._code = code
        self.groups = ngroups + 1
        self.groupindex = groupindex
        self._indexgroup = indexgroup

    def _engine(self, string, endpos):
        end = len(string) if endpos is None else min(endpos, len(string))
        engine = _Engine(self._code, string, end)
        engine.groups = self.groups
        engine._marks_template = [-1] * (2 * self.groups)
        return engine

    def _match_at(self, string, pos, endpos, anchored_end=False):
        engine = self._engine(string, endpos)
        marks = [-1] * (2 * self.groups)
        marks[0] = pos
        for stop, found in engine.run(0, pos, marks):
            if anchored_end and stop != engine.end:
                continue
            found = list(found)
            found[1] = stop
            return Match(self, string, found, pos, engine.end)
        return None

    def match(self, string, pos=0, endpos=None):
        pos = max(0, pos)
        return self._match_at(string, pos, endpos)

    def fullmatch(self, string, pos=0, endpos=None):
        return self._match_at(string, max(0, pos), endpos, anchored_end=True)

    def search(self, string, pos=0, endpos=None):
        engine_end = len(string) if endpos is None else min(endpos, len(string))
        start = max(0, pos)
        while start <= engine_end:
            found = self._match_at(string, start, endpos)
            if found is not None:
                return found
            start += 1
        return None

    def finditer(self, string, pos=0, endpos=None):
        start = max(0, pos)
        limit = len(string) if endpos is None else min(endpos, len(string))
        while start <= limit:
            found = self.search(string, start, endpos)
            if found is None:
                return
            yield found
            begin, stop = found.span()
            start = stop if stop > begin else stop + 1

    def findall(self, string, pos=0, endpos=None):
        result = []
        for found in self.finditer(string, pos, endpos):
            if self.groups == 1:
                result.append(found.group())
            elif self.groups == 2:
                result.append(found.group(1))
            else:
                result.append(found.groups())
        return result

    def split(self, string, maxsplit=0):
        result = []
        last = 0
        count = 0
        for found in self.finditer(string):
            if maxsplit and count >= maxsplit:
                break
            begin, stop = found.span()
            result.append(string[last:begin])
            for group in range(1, self.groups):
                result.append(found.group(group))
            last = stop
            count += 1
        result.append(string[last:])
        return result

    def subn(self, repl, string, count=0):
        result = []
        last = 0
        made = 0
        for found in self.finditer(string):
            if count and made >= count:
                break
            begin, stop = found.span()
            result.append(string[last:begin])
            result.append(self._replacement(repl, found))
            last = stop
            made += 1
        result.append(string[last:])
        joined = string[:0].join(result) if False else _join_like(string, result)
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
        for group in range(pattern.groups - 1, 0, -1):
            if marks[2 * group] >= 0:
                self.lastindex = group
                break
        self.lastgroup = None
        if self.lastindex is not None:
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

    def group(self, *groups):
        if not groups:
            groups = (0,)
        values = []
        for group in groups:
            if isinstance(group, str):
                group = self.re.groupindex[group]
            start, stop = self._marks[2 * group], self._marks[2 * group + 1]
            values.append(None if start < 0 else self.string[start:stop])
        return values[0] if len(values) == 1 else tuple(values)

    def groups(self, default=None):
        return tuple(
            self.group(group) if self._marks[2 * group] >= 0 else default
            for group in range(1, self.re.groups)
        )

    def groupdict(self, default=None):
        return {
            name: (self.group(index) if self._marks[2 * index] >= 0 else default)
            for name, index in self.re.groupindex.items()
        }

    def expand(self, template_text):
        return self.re._replacement(template_text, self)

    def __getitem__(self, group):
        return self.group(group)

    def __repr__(self):
        return "<re.Match object; span=" + repr(self.span()) + ", match=" + repr(self.group()) + ">"
