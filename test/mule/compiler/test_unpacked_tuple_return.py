"""Unpacked tuple return convention eliminates intermediate heap tuple allocation."""

from mule.compiler import emit_llvm
from test.mule.compiler.test_native import run_native


def _function_block(llvm: str, symbol: str) -> str:
    marker = f"define internal ptr {symbol}("
    start = llvm.index(marker)
    end = llvm.index("\n}\n", start) + 3
    return llvm[start:end]


def test_unpacked_tuple_return_eliminates_heap_tuple_on_fast_path():
    llvm = emit_llvm(
        """
def scan(text: str, pos: int):
    if pos < len(text):
        return text[pos:], pos + 1, 1
    return "", pos, 0

def caller(s: str):
    chunk, next_pos, count = scan(s, 0)
    return len(chunk) + next_pos + count
"""
    )
    unpacked_body = _function_block(llvm, '@"mule_fn_scan.spec.0.unpacked"')
    assert "ptr %tuple.out" in unpacked_body
    assert "@mule_sequence_tuple_owned" not in unpacked_body
    assert "getelementptr ptr, ptr %tuple.out, i64 0" in unpacked_body
    assert "getelementptr ptr, ptr %tuple.out, i64 1" in unpacked_body
    assert "getelementptr ptr, ptr %tuple.out, i64 2" in unpacked_body

    caller_body = _function_block(llvm, '@"mule_fn_caller.body"')
    assert "%unpack.out = alloca [3 x ptr]" in caller_body
    assert '@"mule.direct.unpack.mule_fn_scan.2.3.' in caller_body
    assert "@mule.fast.unpack.3" not in caller_body
    assert "@mule_tuple_slot" not in caller_body


def test_mismatched_return_arity_keeps_boxed_tuple_return():
    llvm = emit_llvm(
        """
def maybe_pair(flag, x):
    if flag:
        return x, x + 1
    return x, x + 1, x + 2

def caller(x):
    a, b = maybe_pair(True, x)
    return a + b
"""
    )
    assert ".unpacked(" not in llvm
    caller_body = _function_block(llvm, '@"mule_fn_caller.body"')
    assert "@mule.fast.unpack.2" in caller_body


def test_unpacked_tuple_return_execution_and_rebinding_fallback(tmp_path):
    completed = run_native(
        tmp_path,
        """
from mule import optimize
def step(label, index):
    if index == 0:
        return "zero", 1, index + 10
    return label + "!", index + 1, index * 2

@optimize(debug=3)
def run_once(s, i):
    tag, nxt, score = step(s, i)
    return f"{tag}:{nxt}:{score}"

print(run_once("hi", 0))
print(run_once("hi", 3))
boxed = step("box", 4)
print(boxed)

def replacement_list(label, index):
    return [label + "?", index + 100, index + 200]

step = replacement_list
print(run_once("dyn", 5))

def replacement_bad(label, index):
    return (label, index)

step = replacement_bad
try:
    run_once("bad", 1)
except ValueError:
    print("caught wrong length")
""",
    )
    assert completed.returncode == 0
    assert completed.stdout == (
        "zero:1:10\nhi!:4:6\n('box!', 5, 8)\ndyn?:105:205\ncaught wrong length\n"
    )
