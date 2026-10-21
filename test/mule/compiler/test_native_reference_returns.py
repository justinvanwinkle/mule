"""Native reference returns preserve identity and transfer their owning roots."""

import re
import subprocess

import pytest

from mule.compiler import build, emit_llvm
from mule.compiler.modules import STDLIB
from test.mule.compiler.test_native import run_native
from test.python_host import python_host

NODE = """
from mule import int64, optimize
@optimize(speed=3, safety=3)
class Node:
    value: int64
    def __init__(self, value: int64):
        self.value = value
    def identity(self):
        return self
    def choose(self, create: int64):
        if create:
            return Node(6)
        return self
    def fresh(self, value: int64):
        return Node(value)
    def chain(self, value: int64):
        return self.fresh(value)
    def add(self, amount: int64) -> int64:
        return self.value + amount
    def get_bound(self):
        return self.add
    def fail_after_fresh(self, value: int64):
        item = self.fresh(9)
        over: int64 = value + 1
        return item
    def loop(self, limit: int64):
        item = self
        index: int64 = 0
        while index < limit:
            item = self.fresh(index)
            index += 1
        return item

@optimize(speed=3, safety=3)
def get_class():
    return Node
"""


def compare_reference_program(tmp_path, tail):
    source = NODE + tail
    oracle_directory = tmp_path / "oracle"
    oracle_directory.mkdir()
    # Use the real host-compatible annotation library, not replacement markers.
    (oracle_directory / "mule.py").write_bytes((STDLIB / "mule.py").read_bytes())
    oracle_source = oracle_directory / "program.py"
    oracle_source.write_text(source)
    oracle = subprocess.run(
        [python_host(), oracle_source],
        check=False,
        capture_output=True,
        text=True,
        timeout=10,
    )
    assert oracle.returncode == 0, oracle.stderr
    native = run_native(tmp_path, source)
    assert native.returncode == 0, native.stderr
    assert native.stdout == oracle.stdout
    return source


def object_entry_body(llvm, function):
    match = re.search(
        rf'^define[^\n]*@"[^"\n]*{re.escape(function)}\.body"'
        r"\([^\n]*\)[^\n]*\{\n(.*?)^\}",
        llvm,
        re.MULTILINE | re.DOTALL,
    )
    assert match is not None, f"missing object entry for {function}"
    return match.group(1)


def native_reference_body(llvm, function):
    adapter = object_entry_body(llvm, function)
    call = re.search(r"\bcall ptr (@mule\.native\.\d+)\(", adapter)
    assert call is not None, (
        f"{function} must select a reference-returning native entry"
    )
    match = re.search(
        rf"^define internal ptr {re.escape(call.group(1))}\([^\n]*\) \{{\n(.*?)^\}}",
        llvm,
        re.MULTILINE | re.DOTALL,
    )
    assert match is not None
    return match.group(1)


def test_native_argument_and_class_returns_preserve_original_identity(tmp_path):
    source = compare_reference_program(
        tmp_path,
        """
original = Node(12)
alias = original.identity()
print(alias is original, alias.value)
alias.value = 19
print(original.value, get_class() is Node)
""",
    )
    llvm = emit_llvm(source)
    native_reference_body(llvm, "Node.identity")
    native_reference_body(llvm, "get_class")


def test_fresh_instance_returns_survive_two_native_calls_and_temporary_receivers(
    tmp_path,
):
    source = compare_reference_program(
        tmp_path,
        """
original = Node(3)
result = original.chain(42)
print(result.value, result is original, original.value)
print(Node(7).identity().value, Node(1).chain(-17).value)
""",
    )
    llvm = emit_llvm(source)
    native_reference_body(llvm, "Node.fresh")
    body = native_reference_body(llvm, "Node.chain")
    assert re.search(r"store ptr [^\n]+, ptr %owner\.\d+", body)


def test_returned_bound_method_keeps_temporary_receiver_and_binding(tmp_path):
    source = compare_reference_program(
        tmp_path,
        """
bound = Node(5).get_bound()
print(bound(7), bound.__self__.value, bound.__func__ is Node.add)
receiver = Node(11)
saved = receiver.get_bound()
receiver.value = 20
del receiver
print(saved(2))
""",
    )
    native_reference_body(emit_llvm(source), "Node.get_bound")


def test_native_returned_instances_release_their_last_owner(tmp_path):
    result = run_native(
        tmp_path,
        NODE
        + """
from _mule_native import weak_handle, weak_target
original = Node(10)
alias = original.identity()
handle = weak_handle(original, None, None)
del original
print(weak_target(handle) is alias, alias.value)
del alias
print(weak_target(handle) is None)
fresh = Node(1).chain(42)
handle = weak_handle(fresh, None, None)
print(fresh.value)
del fresh
print(weak_target(handle) is None)
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "True 10\nTrue\n42\nTrue\n"


def test_returned_method_owns_receiver_until_bound_method_is_released(tmp_path):
    result = run_native(
        tmp_path,
        NODE
        + """
from _mule_native import weak_handle, weak_target
receiver = Node(5)
handle = weak_handle(receiver, None, None)
bound = receiver.get_bound()
del receiver
print(weak_target(handle) is None)
print(bound(7))
del bound
print(weak_target(handle) is None)
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "False\n12\nTrue\n"


def test_error_after_owned_native_call_result_preserves_caller_state(tmp_path):
    source = (
        NODE
        + """
from _mule_native import weak_handle, weak_target
owner = Node(1)
good = owner.fail_after_fresh(0)
print(good.value)
handle = weak_handle(good, None, None)
del good
print(weak_target(handle) is None)
try:
    owner.fail_after_fresh(int('9223372036854775807'))
except OverflowError:
    print('overflow')
print(owner.identity() is owner)
"""
    )
    body = native_reference_body(emit_llvm(source), "Node.fail_after_fresh")
    assert "@llvm.sadd.with.overflow.i64" in body
    assert re.search(r"store ptr [^\n]+, ptr %owner\.\d+", body)
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "9\nTrue\noverflow\nTrue\n"


def test_owned_results_inside_loop_keep_ordinary_lifetime_handling(tmp_path):
    source = compare_reference_program(
        tmp_path,
        """
owner = Node(50)
print(owner.loop(0) is owner)
result = owner.loop(3)
print(result.value, result is owner)
""",
    )
    body = object_entry_body(emit_llvm(source), "Node.loop")
    assert not re.search(r"\bcall ptr @mule\.native\.\d+\(", body)


def test_boxed_native_method_still_rejects_a_wrong_receiver(tmp_path):
    result = run_native(
        tmp_path,
        NODE
        + """
class Other:
    pass
try:
    Node.loop(Other(), 0)
except TypeError:
    print('wrong receiver')
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "wrong receiver\n"


def test_unused_native_class_dependency_does_not_raise_early(tmp_path):
    source = compare_reference_program(
        tmp_path,
        """
original = Node(12)
del Node
print(original.choose(0) is original)
try:
    original.choose(1)
except NameError:
    print('missing constructor')
""",
    )
    native_reference_body(emit_llvm(source), "Node.choose")


@pytest.mark.integration
def test_optimized_native_reference_returns_preserve_lifetimes_and_fallbacks(tmp_path):
    source = tmp_path / "program.py"
    source.write_text(
        NODE
        + """
from _mule_native import weak_handle, weak_target
original = Node(3)
handle = weak_handle(original, None, None)
returned = original.identity()
del original
print(returned.value)
fresh = returned.chain(9)
fresh_handle = weak_handle(fresh, None, None)
bound = returned.get_bound()
del returned
print(bound(4), weak_target(handle) is None)
del bound
print(weak_target(handle) is None, fresh.value)
del fresh
print(weak_target(fresh_handle) is None)
value = Node(11)
del Node
print(value.choose(0) is value)
try:
    value.choose(1)
except NameError:
    print('missing constructor')
"""
    )
    executable = build(source, tmp_path / "program", optimize=3, lto=False, workers=1)
    source.unlink()
    result = subprocess.run(
        [executable], check=False, capture_output=True, text=True, timeout=10
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "3\n7 False\nTrue 9\nTrue\nTrue\nmissing constructor\n"
