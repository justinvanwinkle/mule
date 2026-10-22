"""Exact text affix rewrites preserve filtering, bounds, and fallback effects."""

import re

import pytest

from test.mule.compiler.rewrite_support import build_case

pytestmark = pytest.mark.integration


FILTER_SOURCE = """
def select_record(text: str, prefix: str, suffix: str):
    return (text.startswith(prefix), text.endswith(suffix),
            text.startswith((prefix, suffix)), text.endswith((prefix, suffix)))

prefix = input()
suffix = input()
for record in input().split('|'):
    print(select_record(str(record), prefix, suffix))
"""


BOUNDS_SOURCE = """
def select_window(text: str, prefix: str, suffix: str, offset: int, limit: int):
    start = (offset & 31) - 8
    stop = (limit & 31) - 8
    return (text.startswith(prefix, start), text.endswith(suffix, start),
            text.startswith((prefix, suffix), start, stop),
            text.endswith((prefix, suffix), start, stop),
            text.startswith(prefix, None, stop), text.endswith(suffix, None, None))

text = input()
prefix = input()
suffix = input()
print(select_window(text, prefix, suffix, int(input()), int(input())))
"""


TUPLE_SOURCE = """
def prefix_choices(text: str, first, second):
    return text.startswith((first, second))

def suffix_choices(text: str, first, second):
    return text.endswith((first, second))

def evaluated(value, label):
    print(label)
    return value

text = input()
marker = input()
for choices in ((marker, 7), (7, marker), ('not-present', 7), ('', 7)):
    try:
        print('prefix', prefix_choices(text, choices[0], choices[1]))
    except TypeError:
        print('prefix TypeError')
    try:
        print('suffix', suffix_choices(text, choices[0], choices[1]))
    except TypeError:
        print('suffix TypeError')
print('empty', text.startswith(()), text.endswith(()))
try:
    print(text.startswith((evaluated(marker, 'first'), evaluated(7, 'second'))))
except TypeError:
    print('evaluated TypeError')
"""


FALLBACK_SOURCE = """
class LoggedText(str):
    def startswith(self, prefix, start=0, end=None):
        print('override prefix', prefix, start, end)
        return 'prefix override'
    def endswith(self, suffix, start=0, end=None):
        print('override suffix', suffix, start, end)
        return 'suffix override'

class Index:
    def __index__(self):
        print('index conversion')
        return 1

class Affix(str):
    def __len__(self):
        raise AssertionError('subclass length override')
    def __getitem__(self, key):
        raise AssertionError('subclass indexing override')

def fallback_subclass(text, affix):
    return text.startswith(affix), text.endswith(affix)

def fallback_affix_subclass(text: str, affix):
    return text.startswith(affix), text.endswith(affix)

def fallback_keyword(text: str, affix: str):
    return text.startswith(prefix=affix)

def fallback_bound_keyword(text: str, affix: str):
    return text.endswith(affix, start=0)

def fallback_missing(text: str):
    return text.startswith()

def fallback_extra(text: str, affix: str):
    return text.endswith(affix, 0, 2, 3)

def fallback_bad_bound(text: str, affix: str):
    return text.startswith(affix, 0.5)

def fallback_index(text: str, affix: str, index):
    return text.startswith(affix, index), text.endswith(affix, index)

def fallback_large_bound(text: str, affix: str):
    return (text.startswith(affix, 2 ** 100),
            text.endswith(affix, -(2 ** 100)),
            text.startswith(affix, 0, 2 ** 100))

text = input()
affix = input()
print(fallback_subclass(LoggedText(text), affix))
print(fallback_affix_subclass(text, Affix(affix)))
for operation in (lambda: fallback_keyword(text, affix),
                  lambda: fallback_bound_keyword(text, affix),
                  lambda: fallback_missing(text),
                  lambda: fallback_extra(text, affix),
                  lambda: fallback_bad_bound(text, affix)):
    try:
        print(operation())
    except TypeError:
        print('TypeError')
print(fallback_index(text, affix, Index()))
print(fallback_large_bound(text, affix))
"""


def _assert_direct_affix_path(case, function):
    # Public entries and guarded call wrappers legitimately retain dynamic
    # dispatch. Inspect the actual text-specialized implementation instead.
    body = case.function_ir(rf"^mule_fn_{function}\.spec\.\d+$")
    assert "@mule_text_affix(" in body, body
    dispatch = [
        line.strip()
        for line in body.splitlines()
        if "@mule_resolve_method(" in line or "@mule_call_resolved(" in line
    ]
    assert not dispatch, (
        f"{function}'s text-specialized implementation still dispatches affix calls:\n"
        + "\n".join(dispatch)
    )


def _assert_in_place_runtime_comparison(case):
    # The runtime has a legitimate cold method-resolution fallback. Follow only
    # its text comparison helpers, not every function reachable from that arm.
    body = case.function_assembly(r"^mule_text_affix$")
    comparison = body
    for helper in ("slice_eq", "slice_range_eq"):
        if re.search(rf"<[^>]*{helper}[^>]*>", comparison):
            comparison += case.function_assembly(rf".*{helper}.*")
    assert re.search(r"<(?:bcmp|memcmp)(?:@[^>]*)?>", comparison), comparison
    for allocating_operation in (
        "mule_getslice",
        "mule_text_slice",
        "mule_text_concat",
        "from_codepoints",
        "__rust_alloc",
    ):
        assert allocating_operation not in comparison, comparison


def test_affix_filter_rewrites_runtime_string_and_tuple_affixes(tmp_path):
    case = build_case(tmp_path, FILTER_SOURCE, optimize=3, lto=False)
    for stdin in (
        "pre\n.txt\nprelude.txt|prefix|notes.txt|other|\n",
        "hé\n🐍\nhéllo🐍|hé|🐍|other|hé🐍hé\n",
        "\n\nnonempty||🐍\n",
        "longer-than-record\nabsent\na|bc|\n",
    ):
        case.compare(stdin=stdin)
    _assert_direct_affix_path(case, "select_record")
    _assert_in_place_runtime_comparison(case)


def test_affix_window_rewrite_preserves_unicode_and_clamped_bounds(tmp_path):
    case = build_case(tmp_path, BOUNDS_SOURCE, optimize=3, lto=False)
    for text, prefix, suffix, offset, limit in (
        ("abcdef", "b", "e", 9, 13),
        ("héllo🐍", "é", "🐍", 9, 14),
        ("héllo🐍", "", "", 15, 14),
        ("abcdef", "", "", 31, 31),
        ("abcdef", "a", "f", 0, 31),
        ("abcdef", "a", "f", 12, 9),
        ("", "", "", 8, 8),
    ):
        case.compare(stdin=f"{text}\n{prefix}\n{suffix}\n{offset}\n{limit}\n")
    _assert_direct_affix_path(case, "select_window")
    _assert_in_place_runtime_comparison(case)


def test_affix_tuple_rewrite_preserves_short_circuit_and_evaluation_order(tmp_path):
    case = build_case(tmp_path, TUPLE_SOURCE, optimize=3, lto=False)
    for stdin in ("tag-middle-tag\ntag\n", "other\ntag\n", "🐍x🐍\n🐍\n"):
        case.compare(stdin=stdin)
    _assert_direct_affix_path(case, "prefix_choices")
    _assert_direct_affix_path(case, "suffix_choices")
    _assert_in_place_runtime_comparison(case)


def test_affix_ineligible_calls_preserve_lookup_arguments_and_errors(tmp_path):
    case = build_case(tmp_path, FALLBACK_SOURCE, optimize=3, lto=False)
    for stdin in ("ababa\nba\n", "héllo🐍\n🐍\n", "\n\n"):
        case.compare(stdin=stdin)
    for function in (
        "fallback_subclass",
        "fallback_affix_subclass",
        "fallback_keyword",
        "fallback_bound_keyword",
        "fallback_missing",
        "fallback_extra",
        "fallback_bad_bound",
        "fallback_index",
        "fallback_large_bound",
    ):
        body = case.function_ir(rf"mule_fn_{function}(?:\.|$)")
        assert "@mule_text_affix(" not in body, body
