"""Imported procedures retain guarded call targets when their result is unused."""

import re

import pytest

from mule.compiler import emit_llvm
from mule.compiler.ir1tran_lambda import lambda_blocks
from mule.compiler.modules import compile_modules
from mule.compiler.node import CallKind, Combination, GlobalVar, Ref
from test.mule.compiler.test_modules import execute


@pytest.mark.parametrize("unpacked", [False, True], ids=["discarded", "unpacked"])
def test_guarded_call_uses_the_callable_defining_namespace(tmp_path, unpacked):
    definition = (
        "offset = 7\ndef update(value):\n    return value, value + offset\n"
        if unpacked
        else "offset = 7\ndef update(records, key, value):\n    records[key] = value + offset\n"
    )
    source = definition + (
        "def apply(value):\n    first, second = update(value)\n    return first + second\n"
        if unpacked
        else "def apply(records, key, value):\n    update(records, key, value)\n"
    )
    llvm = emit_llvm(source, str(tmp_path / "main.py"), roots=(tmp_path,))
    helpers = re.findall(
        r'^define internal ptr @"(mule\.direct\.[^"]*mule_fn_update[^"]*)"'
        r"\([^\n]*\)[^\n]*\{\n(.*?)^\}",
        llvm,
        re.MULTILINE | re.DOTALL,
    )
    assert helpers, "the procedure needs a guarded internal-call path"
    for name, helper in helpers:
        metadata = re.search(
            r"(%[-\w.$]+) = call ptr @mule_method_entry\(ptr %[-\w.$]+, ptr (%[-\w.$]+)\)",
            helper,
        )
        assert metadata is not None, (
            f"{name} must read the callable's entry and actual namespace"
        )
        entry, namespace_slot = metadata.groups()
        assert re.search(rf"icmp eq ptr {re.escape(entry)}, @", helper)
        namespace = re.search(
            rf"(%[-\w.$]+) = load ptr, ptr {re.escape(namespace_slot)}\b", helper
        )
        assert namespace is not None
        invocation = re.search(
            r'call ptr @"[^"]*mule_fn_update[^"]*"\(ptr (%[-\w.$]+)', helper
        )
        assert invocation is not None
        assert invocation.group(1) == namespace.group(1), (
            f"{name} passes the caller's namespace instead of the callable's namespace"
        )


@pytest.mark.parametrize("return_statement", ["", "return "])
def test_imported_procedure_retains_a_predicted_guarded_target(
    tmp_path, return_statement
):
    (tmp_path / "_worker.py").write_text(
        "def update(records, key, value):\n    records[key] = value\n"
    )
    source = (
        "from _worker import update\n"
        "def apply(records, key, value):\n"
        f"    {return_statement}update(records, key, value)\n"
    )
    compilation, imports = compile_modules(
        source, str(tmp_path / "main.py"), roots=(tmp_path,)
    )
    caller = next(lam for lam in compilation.lambdas if lam.name == "mule_fn_apply")
    target = imports.exported_targets["_worker", "update"]
    nodes = [node for block in lambda_blocks(caller) for node in block.nodes()]
    reference = next(
        node
        for node in nodes
        if isinstance(node, Ref)
        and isinstance(node.leaf, GlobalVar)
        and node.leaf.name == "update"
    )
    call = next(
        node
        for node in nodes
        if isinstance(node, Combination) and node.fun.use() is reference
    )
    assert compilation.facts is not None
    assert compilation.facts[caller].targets.get(reference) is target
    assert call.kind is CallKind.GUARDED
    assert call.predicted is target
    assert not call.proven


def test_rebound_imported_procedure_uses_the_actual_callable(tmp_path):
    result = execute(
        tmp_path,
        {"_worker.py": "def update(records, key, value):\n    records[key] = value\n"},
        """
from _worker import update
def apply(records, key, value):
    update(records, key, value)
def replace(records, key, value):
    records[key] = value + 100
    return 'replacement'
records = {}
apply(records, 'before', 1)
globals()['update'] = replace
apply(records, 'after', 2)
print(records['before'], records['after'])
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "1 102\n"


def test_rebound_imported_procedure_releases_its_discarded_result(tmp_path):
    result = execute(
        tmp_path,
        {"_worker.py": "def update(records, key, value):\n    records[key] = value\n"},
        """
from _worker import update
from _weakref import ref
references = []
class Result:
    pass
def discarded(reference):
    print('discarded', reference() is None)
def apply(records, key, value):
    update(records, key, value)
def replace(records, key, value):
    records[key] = value
    result = Result()
    references.append(ref(result, discarded))
    return result
globals()['update'] = replace
records = {}
apply(records, 'value', 42)
print(records['value'])
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "discarded True\n42\n"
