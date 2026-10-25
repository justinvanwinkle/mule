"""Borrowed descriptor lifetimes remain protected across control flow."""

import pytest

from mule.compiler import emit_llvm
from mule.compiler.codegen import compile_component
from mule.compiler.ctype import UNKNOWN
from mule.compiler.ltn import EntryContext
from mule.compiler.main import compile_source
from test.mule.compiler.test_exception_groups import compare
from test.mule.compiler.test_llvm_boxing import function_body


def component_for(source, *, resume=False, specialized=False):
    compilation = compile_source(source)
    function = compilation.lambdas[-1] if resume else compilation.lambdas[1]
    assert compilation.facts is not None
    facts = compilation.facts[function]
    if specialized and facts.variants:
        variant = facts.variants[0]
        context = EntryContext(
            variant.id,
            variant.types,
            variant.targets,
            variant.parameters,
            variant.return_type,
            element_contracts=variant.element_contracts,
        )
    else:
        context = EntryContext(
            None,
            facts.types,
            facts.targets,
            facts.parameters or (UNKNOWN,) * len(function.vars),
            facts.return_type,
            element_contracts=facts.element_contracts,
        )
    return compile_component(function, context)


@pytest.mark.parametrize("loop", [False, True], ids=["branch", "loop"])
def test_stable_local_borrows_across_branch_and_loop(loop):
    source = (
        "def check(value, flag):\n"
        "    result = False\n"
        + (
            "    for index in range(flag):\n"
            "        result = value is (None if index else False)\n"
            if loop
            else "    result = value is (None if flag else False)\n"
        )
        + "    return result, value is None\n"
    )
    component = component_for(source)
    loads = [
        vop
        for block in component.blocks
        for vop in block.vops
        if vop.info.name == "load-local"
        and vop.codegen_info.name == "value"
        and not vop.codegen_info.move
    ]
    assert loads
    assert any(
        load.codegen_info.borrow and load.results[0].tn.borrowed_from is load.args[0].tn
        for load in loads
    )


def test_owner_reassignment_does_not_borrow_previous_value():
    component = component_for(
        "def check(value, flag):\n"
        "    result = value is (None if flag else (value := False))\n"
        "    return result, value\n"
    )
    loads = [
        vop
        for block in component.blocks
        for vop in block.vops
        if vop.info.name == "load-local" and vop.codegen_info.name == "value"
    ]
    assert loads
    assert not loads[0].codegen_info.borrow


@pytest.mark.parametrize(
    "replace_owner", [False, True], ids=["arithmetic", "new-generation"]
)
def test_loop_borrow_ends_before_later_operations_and_next_acquisition(replace_owner):
    component = component_for(
        "def check(value, count):\n"
        "    hits = 0\n"
        "    saved = value\n"
        "    for index in range(count):\n"
        "        hits += value is None\n"
        + ("        saved = value\n        value = index\n" if replace_owner else "")
        + "    return hits, saved, value\n"
    )
    comparisons = [
        vop
        for block in component.blocks
        for vop in block.vops
        if vop.info.name == "compare-is"
    ]
    assert comparisons
    compared = comparisons[0].args[0].tn
    writer, _ = component.writes[compared][0]
    assert writer.info.name == "load-local"
    assert not writer.codegen_info.move
    assert writer.codegen_info.borrow
    assert compared.borrowed_from is writer.args[0].tn


@pytest.mark.parametrize(
    "expression",
    ["(alias := alias + 1.0)", "(yield flag)"],
    ids=["alias-sensitive-mutation", "coroutine-suspension"],
)
def test_alias_mutation_and_suspension_keep_owned_temporaries(expression):
    component = component_for(
        "def check(value, flag):\n"
        "    alias = value\n"
        f"    result = value is (None if flag else {expression})\n"
        "    return result, value, alias\n",
        resume=expression == "(yield flag)",
    )
    loads = [
        vop
        for block in component.blocks
        for vop in block.vops
        if vop.info.name == "load-local" and vop.codegen_info.name == "value"
    ]
    # The first load creates the independently owned local alias.
    assert not loads[0].codegen_info.borrow
    assert not loads[1].codegen_info.borrow


def test_cfg_borrow_omits_retain_for_protected_parameter():
    llvm = emit_llvm(
        "def check(value, flag):\n"
        "    result = value is (None if flag else False)\n"
        "    return result, value is None\n"
    )
    body = function_body(llvm, '"mule_fn_check.body"')
    assert "call ptr @mule.fast.retain(" not in body
    assert "icmp eq ptr" in body


def test_exception_handler_can_still_read_protecting_owner(tmp_path):
    compare(
        tmp_path,
        """
def check(value, flag, divisor):
    try:
        result = value is (10 // divisor if flag else None)
    except ZeroDivisionError:
        return value is None, value
    return result, value
for value in (None, False, []):
    print(check(value, True, 0))
    print(check(value, True, 2))
    print(check(value, False, 0))
""",
    )


def test_borrowed_branches_loops_and_reassignment_preserve_results(tmp_path):
    compare(
        tmp_path,
        """
def check(value, count):
    total = 0
    for index in range(count):
        total += value is (None if index else False)
    return total, value is None
def replace(value, flag):
    result = value is (None if flag else (value := False))
    return result, value
for value in (None, False, []):
    print(check(value, 4))
    print(replace(value, True))
    print(replace(value, False))
""",
    )


def test_nested_list_element_local_borrows_across_inner_loop():
    component = component_for(
        "def mat_sum(grid: list[list[int]], n: int) -> int:\n"
        "    total = 0\n"
        "    for i in range(n):\n"
        "        row = grid[i]\n"
        "        for j in range(n):\n"
        "            total += row[j]\n"
        "    return total\n"
        "mat_sum([[1, 2], [3, 4]], 2)\n",
        specialized=True,
    )
    getitems = [
        vop
        for block in component.blocks
        for vop in block.vops
        if vop.info.name == "protocol-getitem"
    ]
    assert getitems
    assert all(vop.codegen_info == "list_borrow" for vop in getitems)
    row_tns = [
        tn for tn in component.tns if tn.leaf is not None and tn.leaf.name == "row"
    ]
    assert row_tns
    assert all(tn.borrowed_from is not None for tn in row_tns)


def test_structural_unpack_from_exact_list_borrows_tuple_and_elements():
    component = component_for(
        "def search_pairs(pairs: list[tuple[int, int]], target: int) -> int:\n"
        "    lo = 0\n"
        "    hi = len(pairs) - 1\n"
        "    while lo <= hi:\n"
        "        mid = (lo + hi) >> 1\n"
        "        start, end = pairs[mid]\n"
        "        if end < target:\n"
        "            lo = mid + 1\n"
        "        else:\n"
        "            hi = mid - 1\n"
        "    return lo\n"
        "search_pairs([(1, 2), (3, 4)], 3)\n",
        specialized=True,
    )
    getitems = [
        vop
        for block in component.blocks
        for vop in block.vops
        if vop.info.name == "protocol-getitem"
    ]
    unpacks = [
        vop
        for block in component.blocks
        for vop in block.vops
        if vop.info.name == "structural-unpack"
    ]
    refs = [
        vop
        for block in component.blocks
        for vop in block.vops
        if vop.info.name == "unpack-ref"
    ]
    assert getitems and all(vop.codegen_info == "list_borrow" for vop in getitems)
    assert unpacks and all(vop.codegen_info.proved for vop in unpacks)
    assert refs and all(vop.codegen_info.borrow_element for vop in refs)


def test_exact_tuple_getitem_borrows_and_inlines_bounds_check():
    llvm = emit_llvm(
        "def tuple_widths(pair: tuple[str, str], idx: int) -> int:\n"
        "    return len(pair[idx]) + len(pair[0])\n"
        "tuple_widths(('ab', 'cde'), 1)\n"
    )
    body = function_body(llvm, '"mule_fn_tuple_widths.spec.0"')
    assert "call ptr @mule.fast.retain(" not in body
    assert "icmp ult i64" in body


def test_container_local_element_retains_when_container_mutated_before_read():
    component = component_for(
        "def mutated_before_read(rows: list[str], i: int) -> int:\n"
        "    row = rows[i]\n"
        "    rows.clear()\n"
        "    return len(row)\n"
        "mutated_before_read(['a', 'b'], 0)\n",
        specialized=True,
    )
    getitems = [
        vop
        for block in component.blocks
        for vop in block.vops
        if vop.info.name == "protocol-getitem"
    ]
    assert getitems
    assert all(vop.codegen_info == "list_owned" for vop in getitems)
