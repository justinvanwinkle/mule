"""Imported procedures retain guarded call targets when their result is unused."""

import pytest

from mule.compiler.ir1tran_lambda import lambda_blocks
from mule.compiler.modules import compile_modules
from mule.compiler.node import CallKind, Combination, GlobalVar, Ref
from test.mule.compiler.test_modules import execute


@pytest.mark.parametrize("return_statement", ["", "return "])
def test_imported_procedure_retains_a_predicted_guarded_target(tmp_path, return_statement):
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
class Result:
    def __del__(self):
        print('discarded')
def apply(records, key, value):
    update(records, key, value)
def replace(records, key, value):
    records[key] = value
    return Result()
globals()['update'] = replace
records = {}
apply(records, 'value', 42)
print(records['value'])
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "discarded\n42\n"
