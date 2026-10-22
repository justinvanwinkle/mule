"""Exact text searches keep codepoint semantics without method dispatch."""

import re

import pytest

from test.mule.compiler.rewrite_support import build_case


@pytest.mark.integration
def test_text_find_scans_runtime_tokens_without_resolving_each_method(tmp_path):
    case = build_case(
        tmp_path,
        """
def scan_tokens(text: str, needle: str) -> int:
    total = 0
    pos = 0
    while pos < len(text):
        found = text.find(needle, pos)
        if found < 0:
            break
        total += found + 1
        pos = found + len(needle)
    return total

def last_token(text: str, needle: str) -> int:
    return text.rfind(needle, None, None)

text = input()
needle = input()
print(scan_tokens(text, needle), last_token(text, needle))
print(text, needle)
""",
    )
    for text, needle in (
        ("key::value::key::tail", "::"),
        ("aaaaabaaaaabaaa", "aab"),
        ("é🐍key🐍é🐍tail", "🐍"),
        ("αβαβαβ", "αβ"),
        ("no matches", "absent"),
    ):
        case.compare(stdin=f"{text}\n{needle}\n")

    # Inspect optimized specialized definitions, rather than matching the raw
    # emitter's parameter spelling or LLVM's post-optimization attributes.
    body = case.function_ir(r"^mule_fn_scan_tokens\.spec\.\d+$")
    assert "@mule_text_find(" in body
    assert "@mule_resolve_method(" not in body
    assert "@mule_call_resolved(" not in body

    # Follow the native implementation, not just its selected ABI. Its Latin-1
    # candidate scan compares the remaining bytes, and Unicode scans codepoints.
    # Cold dynamic dispatch in mule_text_find remains legitimate.
    native = case.function_assembly(r"^mule_text_find$")
    search_symbols = re.findall(r"<([^>]*4Text[^>]*(?:4find|5rfind)[^>]*)>", native)
    for symbol in set(search_symbols):
        native += case.function_assembly(f"^{re.escape(symbol)}$")
    assert re.search(r"\bcmp[bwlq]?\b", native)
    assert re.search(r"\bj(?:e|ne|a|ae|b|be|g|ge|l|le)\b", native)
    assert re.search(r"<(?:bcmp|memcmp)(?:@[^>]*)?>", native), (
        "The actual search path must compare candidate bytes, not delegate "
        "all search work to generic language dispatch"
    )
    assert "mule_text_slice" not in native

    # Search returns an immediate index and borrows its arguments. Inspect both
    # explicit retain calls and their optimized inline reference-count updates.
    last = case.function_ir(r"^mule_fn_last_token\.spec\.\d+$")
    assert "@mule_resolve_method(" not in last, last
    assert "@mule_call_resolved(" not in last
    assert "@mule_text_find(" in last
    assert "@mule_retain(" not in last
    assert not re.search(r"atomicrmw\s+add\b", last)


@pytest.mark.integration
def test_text_find_unicode_empty_and_bounds_match_python(tmp_path):
    case = build_case(
        tmp_path,
        """
def search(text: str, needle: str, start: int, end: int):
    print(text.find(needle), text.rfind(needle))
    print(text.find(needle, start), text.rfind(needle, start))
    print(text.find(needle, start, end), text.rfind(needle, start, end))
    print(text.find(needle, None, end), text.rfind(needle, start, None))

text = input()
needle = input()
start = int(input())
end = int(input())
search(text, needle, start, end)
print(text, needle)
""",
    )
    for text, needle in (
        ("", ""),
        ("é🐍é🐍z", "🐍"),
        ("é🐍é🐍z", ""),
        ("é🐍é🐍z", "🐍é"),
        ("aaaaa", "aaa"),
        ("abc", "abcdef"),
        ("abc", "🐍"),
    ):
        for start, end in ((0, 99), (-99, 99), (-4, -1), (2, 1), (99, 99)):
            case.compare(stdin=f"{text}\n{needle}\n{start}\n{end}\n")


@pytest.mark.integration
def test_text_find_fallback_preserves_argument_and_override_boundaries(tmp_path):
    case = build_case(
        tmp_path,
        """
class Bound:
    def __index__(self):
        print('index')
        return 1

class Needle(str):
    pass

class Text(str):
    def find(self, *args, **kwargs):
        print('override-find', args, kwargs)
        return 71
    def rfind(self, *args, **kwargs):
        print('override-rfind', args, kwargs)
        return 72

def report(text, needle, start, end):
    try:
        print(text.find(needle, start, end))
        print(text.rfind(needle, start, end))
    except (TypeError, OverflowError) as error:
        print(type(error).__name__)

text = input()
needle = input()
huge = int(input())
report(text, needle, huge, -huge)
report(text, needle, -huge, huge)
report(text, needle, Bound(), None)
report(text, Needle(needle), None, None)
report(text, needle, 1.5, None)
report(text, b'x', None, None)
report(Text(text), needle, 0, None)

def value(label, result):
    print(label)
    return result

try:
    print(value('receiver', text).find(value('needle', needle),
          start=value('keyword', 0)))
except TypeError:
    print('keyword TypeError')
try:
    print(text.rfind())
except TypeError:
    print('missing TypeError')
try:
    print(text.find(needle, 0, None, 0))
except TypeError:
    print('extra TypeError')
print(text.find(needle), text.rfind(needle), text, needle)
""",
    )
    case.compare(stdin="é🐍é🐍tail\n🐍\n1267650600228229401496703205376\n")
