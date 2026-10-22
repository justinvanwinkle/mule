"""Mutable calls enforce their checked definition-derived result contract."""

import pytest

from test.mule.compiler.test_modules import execute


def run_source(tmp_path, source):
    result = execute(tmp_path, {}, source)
    assert result.returncode == 0, result.stderr
    return result.stdout


@pytest.mark.parametrize("safety", [0, 2, 3])
@pytest.mark.parametrize("bad_result", ["1.5", "11", "True"])
@pytest.mark.parametrize(
    "replacement", ["produce = incompatible", "globals()['produce'] = incompatible"]
)
def test_incompatible_rebinding_errors_without_freezing_compatible_replacement(
    tmp_path, safety, replacement, bad_result
):
    source = f"""__safety__ = {safety}
def produce(flag):
    return 3 if flag else 10
def consume(flag):
    return produce(flag) + 2
for flag in (True, False):
    print(consume(flag))
def compatible(flag):
    return 7
produce = compatible
print(consume(True))
def incompatible(flag):
    return {bad_result}
{replacement}
try:
    consume(False)
except TypeError:
    print('incompatible')
"""
    assert run_source(tmp_path, source) == "5\n12\n9\nincompatible\n"


@pytest.mark.parametrize(
    "value", ["[7, 'changed']", "(7, 42)", "(7, 'changed', 1)", "(11, 'changed')"]
)
def test_original_tuple_shape_and_fields_are_checked_before_unpacking(tmp_path, value):
    source = f"""def produce(flag):
    return (3, 'a') if flag else (10, 'b')
def consume(flag):
    number, text = produce(flag)
    return number + len(text)
print(consume(True))
def replacement(flag):
    return {value}
globals()['produce'] = replacement
try:
    consume(False)
except TypeError:
    print('incompatible tuple')
"""
    assert run_source(tmp_path, source) == "4\nincompatible tuple\n"


def test_result_assertion_precedes_later_operand_effects(tmp_path):
    source = """def produce():
    return 3
def later():
    print('later operand')
    return 2
def consume():
    return produce() + later()
def replacement():
    return 1.5
globals()['produce'] = replacement
try:
    consume()
except TypeError:
    print('checked first')
"""
    assert run_source(tmp_path, source) == "checked first\n"


def test_recursive_tuple_contract_accepts_branches_and_checks_rebinding(tmp_path):
    source = """def make_tree(item, depth):
    if depth <= 0:
        return (item, None, None)
    return (item, make_tree(item, depth - 1), make_tree(item, depth - 1))
def root_shape(depth):
    tree = make_tree(1, depth)
    return (tree[0], tree[1] is None)
for depth in (0, 1, 3):
    print(root_shape(depth))
def incompatible(item, depth):
    return ('wrong', None, None)
globals()['make_tree'] = incompatible
try:
    root_shape(1)
except TypeError:
    print('incompatible tree')
"""
    assert run_source(tmp_path, source) == (
        "(1, True)\n(1, False)\n(1, False)\nincompatible tree\n"
    )


@pytest.mark.parametrize("directive", ["__debug_level__ = 3", "__speed__ = 0"])
def test_existing_policy_opt_out_keeps_result_type_dynamic(tmp_path, directive):
    source = f"""{directive}
def produce():
    return 3
def consume():
    return produce() + 2
def replacement():
    return 1.5
produce = replacement
print(consume())
"""
    assert run_source(tmp_path, source) == "3.5\n"
