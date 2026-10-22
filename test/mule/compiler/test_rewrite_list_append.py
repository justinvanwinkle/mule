"""Exact-list append rewrites preserve storage, ownership, and method semantics."""

import re

import pytest

from test.mule.compiler.rewrite_support import build_case

pytestmark = pytest.mark.integration


def test_list_append_worklist_uses_direct_storage_and_records_runtime_values(tmp_path):
    case = build_case(
        tmp_path,
        """
def accumulate_records(text):
    pending = [0]
    records = []
    cursor = 0
    while cursor < len(pending):
        position = pending[cursor]
        cursor += 1
        if position < len(text):
            records.append((position, text[position]))
            pending.append(position + 1)
    return records

print(accumulate_records(input()))
""",
    )
    for text in ("", "abc", "record-雪-🙂" * 12):
        case.compare(stdin=text + "\n")

    kernel = case.function_ir(r"accumulate_records")
    assert re.search(r"call\s+ptr\s+@mule_storage_append(?:_move)?\(", kernel)
    assert "@mule_resolve_method" not in kernel
    assert "@mule_call_resolved" not in kernel

    # Follow the actual storage helpers, not just the caller's selected symbol.
    # Vec::push must write the element into indexed storage and advance length.
    # Capacity growth, GC tracking, and error cleanup remain legitimate calls.
    for symbol in sorted(
        set(re.findall(r"@(mule_storage_append(?:_move)?)\(", kernel))
    ):
        storage = case.function_assembly(r"^" + symbol + r"$")
        assert re.search(r"\bmov\w*\s+%\w+,\s*(?:0x[0-9a-f]+)?\(%\w+,%\w+,8\)", storage)
        assert re.search(r"\b(?:inc\w*|add\w*\s+\$0x1,)", storage)
        assert "mule_resolve_method" not in storage
        assert "mule_call_resolved" not in storage
        assert "mule_get_attr" not in storage


def test_list_append_preserves_aliases_receiver_order_and_failed_arguments(tmp_path):
    case = build_case(
        tmp_path,
        """
text = input()
values = []
alias = values
payload = [text]
print(values.append(payload) is None)
payload.append('changed')
print(values[0] is payload, alias is values, values)
del payload
print(values[0])

def receiver():
    print('receiver')
    return values

def argument():
    print('argument')
    values.append('inner')
    return text

receiver().append(argument())
print(values)

def fail():
    print('failed argument')
    raise ValueError('stop')

before = len(values)
try:
    values.append(fail())
except ValueError:
    print('unchanged', len(values) == before)

original = values
replacement = []
def replace_receiver():
    global values
    values = replacement
    return text

values.append(replace_receiver())
print(original[-1], values, original is alias)

cycle = []
cycle.append(cycle)
print(cycle[0] is cycle)
cycle.clear()
print(cycle)
""",
    )
    case.compare(stdin="owned runtime record\n")


def test_list_append_subclasses_super_and_arity_keep_fallbacks(tmp_path):
    case = build_case(
        tmp_path,
        """
text = input()
class Records(list):
    def append(self, value):
        print('override', value)
        super().append(('stored', value))
        return 'override result'

class Inherited(list):
    pass

def insert(target, value):
    return target.append(value)

plain = []
custom = Records()
inherited = Inherited()
print(insert(plain, text), plain)
print(insert(custom, text), custom)
print(insert(inherited, text), inherited)
print(custom.append(value=text), custom)

def effect(label):
    print('evaluate', label)
    return text

try:
    plain.append()
except TypeError:
    print('missing rejected', plain)
try:
    plain.append(effect('first'), effect('second'))
except TypeError:
    print('extra rejected', plain)

bound = plain.append
print(bound(text), plain)
""",
    )
    case.compare(stdin="subclass runtime record\n")


@pytest.mark.parametrize("keyword", ["value", "object"])
def test_list_append_rejects_keywords_after_evaluating_argument(tmp_path, keyword):
    case = build_case(
        tmp_path,
        f"""
text = input()
records = []
def effect():
    print('evaluate keyword')
    return text

try:
    records.append({keyword}=effect())
except TypeError:
    print('keyword rejected')
print(records)
""",
    )
    case.compare(stdin="keyword runtime record\n")
