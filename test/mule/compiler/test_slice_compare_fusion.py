"""Tests for IR2 substring-slice equality comparison fusion (``text-slice-compare``)."""

from mule.compiler import emit_llvm
from test.mule.compiler.test_native import run_native


def _function_llvm_body(llvm: str, header: str) -> str:
    start = llvm.find(header)
    assert start != -1, f"missing {header!r} in LLVM IR"
    end = llvm.find("\n}\n", start)
    assert end != -1
    return llvm[start : end + 3]


def test_direct_slice_equality_and_inequality_fuse_to_mule_text_slice_eq():
    llvm = emit_llvm(
        """
def check(text: str, i: int) -> int:
    acc = 0
    if text[i : i + 4] == "true":
        acc += 1
    if text[i : i + 5] != "false":
        acc += 2
    if "null" == text[i : i + 4]:
        acc += 4
    if text[:4] == "head":
        acc += 8
    if text[i:] != "tail":
        acc += 16
    return acc

check("true", 0)
"""
    )
    body = _function_llvm_body(llvm, 'define internal ptr @"mule_fn_check.spec.0"')
    assert body.count("@mule_text_slice_eq(") == 5
    assert "@mule.fast.getslice(" not in body


def test_single_use_local_slice_equality_fuses_across_index_update():
    llvm = emit_llvm(
        """
def scan_word(text: str, i: int, end: int) -> int:
    word = text[i + 1 : end]
    i = end + 1
    if word == "tag":
        return i
    return 0

scan_word('"tag"', 0, 4)
scan_word('"other"', 1, 5)
"""
    )
    body = _function_llvm_body(llvm, 'define internal ptr @"mule_fn_scan_word.spec.0"')
    assert body.count("@mule_text_slice_eq(") == 1
    assert "@mule.fast.getslice(" not in body
    assert "@mule.heap.retain(" not in body


def test_multi_use_local_slice_is_not_fused():
    llvm = emit_llvm(
        """
def keep_word(text: str, i: int, end: int) -> str:
    word = text[i + 1 : end]
    i = end + 1
    if word == "tag":
        return word
    return ""

keep_word('"tag"', 0, 4)
keep_word('"other"', 1, 5)
"""
    )
    body = _function_llvm_body(llvm, 'define internal ptr @"mule_fn_keep_word.spec.0"')
    assert "@mule_text_slice_eq(" not in body
    assert "@mule.fast.getslice(" in body


def test_slice_compare_fusion_native_semantics(tmp_path):
    source = """
def scan_payload(text: str) -> tuple[int, int, int]:
    n = len(text)
    i = 0
    acc = 0
    tokens = 0
    tags = 0
    while i < n:
        ch = text[i]
        if ch == '"' :
            end = i + 1
            while end < n and text[end] != '"':
                end += 1
            word = text[i + 1 : end]
            i = end + 1
            if word == "tag":
                tags += 1
                acc += 10
            elif word != "skip":
                acc += len(word)
            tokens += 1
        elif ch == "t":
            if text[i : i + 4] == "true":
                acc += 1
                tokens += 1
            i += 4
        elif ch == "f":
            if text[i : i + 5] != "false":
                acc += 100
            else:
                acc += 2
                tokens += 1
            i += 5
        else:
            i += 1
    return acc, tokens, tags


def check_edges(s: str, i: int, j: int, target: str) -> tuple[bool, bool]:
    eq = s[i:j] == target
    ne = s[i:j] != target
    return eq, ne


def check_open_bounds(s: str, k: int) -> tuple[bool, bool, bool, bool]:
    a = s[:k] == "hé"
    b = s[k:] == "llo🚀"
    c = s[-2:] == "o🚀"
    d = s[:-2] != "héll"
    return a, b, c, d


print(scan_payload('"tag" true false "skip" "abc"'))
print(check_edges("abcdef", 1, 4, "bcd"))
print(check_edges("abcdef", 1, 4, "bce"))
print(check_edges("abcdef", -4, -1, "cde"))
print(check_edges("abcdef", -100, 100, "abcdef"))
print(check_edges("abcdef", 4, 2, ""))
print(check_edges("abcdef", 10, 20, ""))
print(check_edges("héllo🚀world", 1, 6, "éllo🚀"))
print(check_edges("héllo🚀world", 1, 6, "ello🚀"))
print(check_edges("ascii", 0, 5, "héllo"))
print(check_open_bounds("héllo🚀", 2))
"""
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "(16, 5, 1)\n"
        "(True, False)\n"
        "(False, True)\n"
        "(True, False)\n"
        "(True, False)\n"
        "(True, False)\n"
        "(True, False)\n"
        "(True, False)\n"
        "(False, True)\n"
        "(False, True)\n"
        "(True, True, True, False)\n"
    )
