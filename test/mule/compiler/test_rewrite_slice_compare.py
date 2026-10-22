"""One-sided substring comparisons in a runtime token scanner."""

import re

import pytest

from test.mule.compiler.rewrite_support import build_case

SCANNER = """
def scan_tokens(text):
    i = 0
    score = 0
    while i < len(text):
        if text[i:i + 4] == "true":
            score += 1
        if "false" != text[i:i + 5]:
            score += 2
        i += 1
    return score

def scan_local(text, i, end):
    word = text[i:end]
    i = end + 1
    if word == "tag":
        return i
    return 0

def scan_open(text, end):
    return text[:end] == "hé", "llo🚀" == text[end:], text[:-2] != "héll"

def scan_edges(text: str, begin: int, end: int, target: str):
    return text[begin:end] == target, target != text[begin:end]

text = input()
target = input()
print(scan_tokens(text))
print(scan_local(text, 0, 3))
print(scan_open(text, 2))
for begin, end in ((0, 3), (-4, -1), (-100, 100), (4, 2), (100, 200)):
    print(scan_edges(text, begin, end, target))
"""


def _assert_no_substring_after_content_comparison(case):
    # The test supplies exact builtin str inputs and fixnum/None bounds. These
    # satisfy mule_text_slice_eq's guards (sequences.rs), not its cold fallback.
    # Follow the linked comparison arm through its return, allowing the separate
    # fallback to retain its required allocating semantics.
    helper = case.function_assembly(r"^mule_text_slice_eq$")
    instructions = []
    for line in helper.splitlines():
        match = re.match(r"\s*([0-9a-f]+):\s+(\w+)\s*(.*)", line)
        if match:
            instructions.append((int(match[1], 16), match[2], match[3]))
    assert instructions
    positions = {address: index for index, (address, _, _) in enumerate(instructions)}
    comparisons = [
        index
        for index, (_, opcode, operands) in enumerate(instructions)
        if opcode.startswith("call")
        and re.search(r"<(?:[^>]*slice_range_eq[^>]*|memcmp[^>]*|bcmp[^>]*)>", operands)
    ]
    assert comparisons, helper
    # Substring allocation belongs only to the explicit getslice fallback.
    # Reject direct allocation even before the content comparison begins.
    for _, opcode, operands in instructions:
        if opcode.startswith("call"):
            assert not re.search(
                r"alloc|realloc|from_codepoints|from_latin1_slice", operands
            ), operands
    forbidden = r"mule_fast_getslice|mule_compare|alloc|realloc|from_codepoints|from_latin1_slice"
    for start in comparisons:
        pending = [start]
        seen = set()
        returned = False
        while pending:
            index = pending.pop()
            if index in seen:
                continue
            seen.add(index)
            _, opcode, operands = instructions[index]
            assert not re.search(forbidden, operands), instructions[index]
            if opcode.startswith("ret"):
                returned = True
                continue
            if opcode.startswith("j"):
                destination = re.match(r"([0-9a-f]+)\s", operands)
                assert destination, instructions[index]
                address = int(destination[1], 16)
                assert address in positions, instructions[index]
                pending.append(positions[address])
                if opcode == "jmp":
                    continue
            assert index + 1 < len(instructions), instructions[index]
            pending.append(index + 1)
        assert returned, helper

    # Rust may outline the physical comparison. Follow that actual call instead
    # of accepting a helper label as proof of byte/codepoint comparison work.
    if "slice_range_eq" in helper:
        comparison = case.function_assembly(r"slice_range_eq")
        assert re.search(r"memcmp|bcmp", comparison)
        assert not re.search(forbidden, comparison)


@pytest.mark.integration
def test_slice_comparison_scanner_uses_in_place_comparison(tmp_path):
    case = build_case(tmp_path, SCANNER)
    for text, target in (
        ("tag true false true", "tag"),
        ("héllo🚀", "éll"),
        ("", ""),
        ("false", "false"),
    ):
        case.compare(f"{text}\n{target}\n")

    # Inspect each selected entry independently: a different successful rewrite
    # elsewhere in the scanner must not conceal an allocating local/open slice.
    for function in ("scan_tokens", "scan_local", "scan_open", "scan_edges"):
        body = case.function_ir(rf"mule_fn_{function}\.spec\.")
        assert not re.search(r"@(?:mule_fast_getslice|mule\.fast\.getslice)\(", body), (
            f"{function} still materializes the compared substring:\n{body}"
        )
        assert not re.search(r"@mule_(?:text_slice|sequence_slice)\(", body)
        assert "@mule_text_slice_eq(" in body

    _assert_no_substring_after_content_comparison(case)


BOUNDARIES = """
events = []

def mark(value):
    events.append(value)
    return value

def fail():
    events.append("throw")
    raise ValueError("comparison operand")

def overwrite(text, end):
    word = text[:end]
    text = "replacement"
    end = 0
    return word == "tag", text, end

def calls_between(text, end):
    word = text[:end]
    target = mark("tag")
    return word == target

def throwing_between(text, end):
    word = text[:end]
    try:
        return word == fail()
    except ValueError:
        return word

def multiple_uses(text, end):
    word = text[:end]
    matched = word == "tag"
    return matched, word, len(word)

def stepped(text, step):
    try:
        return text[::step] == "tag"
    except ValueError:
        return "zero step"

class Bound:
    def __index__(self):
        events.append("index")
        return 3

class Target:
    def __eq__(self, other):
        events.append(other)
        return True
    def __ne__(self, other):
        events.append(other)
        return False

class Source:
    def __getitem__(self, key):
        events.append("getitem")
        return "tag"

def unknown_source(text):
    return text[:3] == "tag"

def unknown_target(text, target):
    return text[:3] == target, text[:3] != target

def custom_bound(text):
    return text[:Bound()] == "tag"

text = input()
print(overwrite(text, 3))
print(calls_between(text, 3))
print(throwing_between(text, 3))
print(multiple_uses(text, 3))
for step in (1, 2, -1, 0):
    print(stepped(text, step))
print(custom_bound(text))
print(unknown_source(Source()))
print(unknown_source(text))
print(unknown_target(text, Target()))
print(unknown_target(text, "tag"))
print(events)
"""


@pytest.mark.integration
def test_slice_comparison_preserves_values_effects_errors_and_fallbacks(tmp_path):
    case = build_case(tmp_path, BOUNDARIES)
    for text in ("tag suffix", "hé🚀", "", "gat"):
        case.compare(text + "\n")

    # Repeated use must preserve the substring as an observable value. Unknown
    # steps cannot become unit-step comparisons, including when zero must throw.
    for function in ("multiple_uses", "stepped", "calls_between", "throwing_between"):
        body = case.function_ir(rf"mule_fn_{function}\.spec\.")
        assert "@mule_text_slice_eq(" not in body
