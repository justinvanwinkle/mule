"""Tuple conventions use value facts and independent projection consumers."""

import re
import subprocess

import pytest

from mule.compiler import emit_llvm
from test.mule.compiler.rewrite_support import _llvm_functions, build_case
from test.mule.compiler.test_native import run_native
from test.python_host import python_host


def test_tuple_alias_returns_select_unpacked_entry():
    llvm = emit_llvm("""
def pair(x):
    values = (x, x + 1)
    if x:
        result = values
    else:
        result = (x + 2, x + 3)
    return result

def caller(x):
    a, b = pair(x)
    return a + b
""")
    assert '@"mule_fn_pair.body.unpacked"(' in llvm
    assert "mule.direct.unpack.mule_fn_pair" in llvm


def test_tuple_delivery_nonlocal_targets_preserves_effects(tmp_path):
    result = run_native(
        tmp_path,
        """
class Target:
    pass
target = Target()
items = [0, 0]
def pair(x):
    return x, x + 1
def receiver():
    print("receiver", items[0])
    return target
def run(x):
    items[0], receiver().value = pair(x)
    print(items[0], target.value)
run(10)
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "receiver 10\n10 11\n"


def test_tuple_nonlocal_projection_uses_slots():
    llvm = emit_llvm("""
def pair(x):
    return x, x + 1
def assign(items, target, x):
    items[0], target.value = pair(x)
""")
    assert "mule.direct.unpack.mule_fn_pair" in llvm


def test_tuple_target_failure_cleans_unconsumed_children(tmp_path):
    source = """
from _weakref import ref
refs = []
class Child:
    def __init__(self, label):
        self.label = label
        def destroyed(unused):
            print("del", label)
        refs.append(ref(self, destroyed))
class Target:
    def __setattr__(self, name, value):
        global first, setter_value
        setter_value = value
        print("setter-before")
        first = None
        print("setter-after")
        raise ValueError("target")
def pack():
    return Child("first"), Child("second"), Child("third")
try:
    first, Target().value, last = pack()
except ValueError:
    print("caught")
setter_value = None
print("end")
"""
    oracle = subprocess.run(
        [python_host(), "-c", source], capture_output=True, text=True, check=True
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == oracle.stdout


def test_tuple_alias_return_preserves_child_identity(tmp_path):
    result = run_native(
        tmp_path,
        """
def choose(flag, child):
    pair = (child, child)
    if flag:
        result = pair
    else:
        result = (child, child)
    return result
child = []
a, b = choose(True, child)
print(a is child, b is child)
saved = choose(False, child)
print(saved[0] is child, saved[1] is child)
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "True True\nTrue True\n"


def test_target_lookup_failure_releases_remaining_tuple_slots(tmp_path):
    source = """
from _weakref import ref
refs = []
class Child:
    def __init__(self, label):
        def destroyed(unused):
            print('destroy', label)
        refs.append(ref(self, destroyed))
class Target:
    pass
def pack():
    return Child('first'), Child('second'), Child('third')
def assign(targets: list[Target]):
    try:
        first, targets[0].value, last = pack()
    except (TypeError, AttributeError):
        print('caught')
    del first
assign([None])
print('done')
"""
    oracle = subprocess.run(
        [python_host(), "-c", source], capture_output=True, text=True, check=True
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == oracle.stdout


@pytest.mark.integration
def test_tuple_delivery_optimized_nonlocal_path_avoids_materialization(tmp_path):
    case = build_case(
        tmp_path,
        """
class Target:
    pass
def pair(value):
    return value, value + 1
def assign(items, target, value):
    items[0], target.value = pair(value)
    return items[0] + target.value
value = int(input())
print(assign([0], Target(), value))
""",
    )
    assert case.compare("17\n").stdout == "35\n"
    definitions = _llvm_functions(case.optimized_llvm)
    deliveries = [
        (name, body)
        for name, body in definitions
        if name.startswith("mule.direct.unpack.mule_fn_pair.")
    ]
    assert deliveries, "Inspect the real guarded delivery helper"
    caller = case.function_ir(r"^mule_fn_assign\.body$")
    for name, body in deliveries:
        assert name in caller
        # The generic callable fallback must still validate arbitrary results.
        # Follow its CFG successors rather than banning valid cold operations.
        blocks = dict(
            re.findall(
                r"^([\w.$-]+):[^\n]*\n(.*?)(?=^[\w.$-]+:|^})",
                body,
                re.MULTILINE | re.DOTALL,
            )
        )
        cold = {label for label, text in blocks.items() if "@mule_call_owned(" in text}
        assert cold
        pending = list(cold)
        while pending:
            for successor in re.findall(r"label %([\w.$-]+)", blocks[pending.pop()]):
                if successor not in cold:
                    cold.add(successor)
                    pending.append(successor)
        direct = "\n".join(text for label, text in blocks.items() if label not in cold)
        assert "%tuple.out" in direct and "store ptr" in direct
        assert "@mule_sequence_tuple_owned(" not in direct
        assert "@mule_fast_unpack(" not in direct
        assert "@mule_storage_tuple(" not in direct
        assert "@mule_tuple_slot(" not in direct
        assert "@mule_storage_read(" not in direct
