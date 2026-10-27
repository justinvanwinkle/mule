"""Dead local stores are determined at each assignment, not by write count."""

import re

import pytest

from mule.compiler import emit_llvm, ir2tran
from mule.compiler.ctype import (
    FLOAT,
    FUNCTION_TYPE,
    INTEGER,
    NONE_TYPE,
    TEXT_TYPE,
    ClassType,
    IteratorType,
    ListType,
    TupleType,
)
from test.mule.compiler.test_borrowed_lifetimes import component_for
from test.mule.compiler.test_exception_groups import compare
from test.mule.compiler.test_llvm_boxing import function_body


def local_stores(component, name):
    return [
        vop
        for block in component.blocks
        for vop in block.vops
        if vop.info.name == "move"
        and vop.results[0].tn.leaf is not None
        and vop.results[0].tn.leaf.name == name
    ]


@pytest.mark.parametrize(
    "fact, inert",
    [
        (INTEGER, True),
        (FLOAT, True),
        (NONE_TYPE, True),
        (TEXT_TYPE, True),
        (TupleType(2, (INTEGER, TEXT_TYPE)), True),
        (ClassType(), False),
        (FUNCTION_TYPE, False),
        (IteratorType(INTEGER), False),
        (ListType(INTEGER), False),
        (TupleType(1, (ClassType(),)), False),
    ],
)
def test_dead_store_requires_destruction_safety(fact, inert):
    # A class can be pure for operations yet expose destruction through weakrefs.
    # A mutable list's current element type cannot prove its lifetime-wide type.
    assert fact.accept(ir2tran._INERT_LOCAL_VALUE) is inert


def test_repeated_writes_only_keep_the_observed_assignment():
    component = component_for(
        "def check(value, flag):\n"
        "    target = 1\n"
        "    target = value\n"
        "    print(target)\n"
        "    target = 2\n"
        "    return flag\n"
    )
    stores = local_stores(component, "target")
    assert len(stores) == 3
    assert [store.codegen_info is ir2tran.DEAD_LOCAL_STORE for store in stores] == [
        True,
        False,
        True,
    ]


def test_dead_raw_stores_do_not_emit_assignments():
    native = function_body(
        emit_llvm(
            "def check(flag):\n    target = 123\n    target = 456\n    return flag\n"
        ),
        '"mule_fn_check.body"',
    )
    slots = re.findall(r"(%[\w.]+) = alloca i64", native)
    assert slots
    for slot in slots:
        assigned = re.findall(rf"store i64 ([^,]+), ptr {re.escape(slot)}\b", native)
        assert assigned
        # Frame initialization is required; neither dead assignment reaches it.
        assert all(value == "0" for value in assigned)


def test_dead_store_borrows_its_live_source_instead_of_retaining():
    component = component_for(
        "def check(value):\n"
        "    source = str(value)\n"
        "    target = source\n"
        "    print(source)\n"
    )
    store = local_stores(component, "target")[0]
    assert store.codegen_info is ir2tran.DEAD_LOCAL_STORE
    [(load, _)] = component.writes[store.args[0].tn]
    assert load.info.name == "load-local"
    assert load.codegen_info.borrow


@pytest.mark.parametrize(
    "source",
    [
        "def check(value, flag):\n    target = value\n    return flag\n",
        "def check(value, flag):\n    target = 1\n    try:\n        target = 2\n        print(value)\n    except Exception:\n        return target\n    return flag\n",
    ],
    ids=["destructor", "handler"],
)
def test_observable_stores_remain_live(source):
    component = component_for(source)
    stores = local_stores(component, "target")
    assert stores
    assert stores[-1].codegen_info is not ir2tran.DEAD_LOCAL_STORE


def test_suspended_frame_keeps_visible_binding():
    component = component_for(
        "def check(value):\n    target = 1\n    yield value\n", resume=True
    )
    stores = local_stores(component, "target")
    assert stores
    assert stores[0].codegen_info is not ir2tran.DEAD_LOCAL_STORE


def test_captured_binding_keeps_cell_stores():
    component = component_for(
        "def check(value, flag):\n"
        "    target = 1\n"
        "    def captured():\n"
        "        return target\n"
        "    target = 2\n"
        "    return captured\n"
    )
    assert any(var.name == "target" for var in component.environment.cells)
    stores = [
        vop
        for block in component.blocks
        for vop in block.vops
        if vop.info.name in ("value-cell-set", "local-cell-set")
    ]
    assert stores
    assert all(vop.codegen_info is not ir2tran.DEAD_LOCAL_STORE for vop in stores)


@pytest.mark.parametrize(
    "body, expected",
    [
        (
            "    target = 1\n    if flag:\n        target = 2\n    return target\n",
            [False, False],
        ),
        (
            "    target = 1\n    if flag:\n        target = 2\n    else:\n        target = 3\n    return target\n",
            [True, False, False],
        ),
        (
            "    target = 1\n    while flag:\n        target = 2\n        flag = False\n    return flag\n",
            [True, True],
        ),
    ],
    ids=["one-branch-overwrite", "both-branches-overwrite", "loop"],
)
def test_dead_stores_follow_control_flow(body, expected):
    component = component_for("def check(flag):\n" + body)
    assert [
        vop.codegen_info is ir2tran.DEAD_LOCAL_STORE
        for vop in local_stores(component, "target")
    ] == expected


def test_dead_stores_preserve_values_handlers_and_captures(tmp_path):
    compare(
        tmp_path,
        """
def check(value, divisor):
    target = value
    old = target
    target = 1
    target = 2
    try:
        target = 3
        print(12 // divisor)
    except ZeroDivisionError:
        print(target)
    target = "discarded"
    return old is value
def captured():
    target = 1
    def read():
        return target
    target = 2
    return read()
for divisor in (0, 3):
    print(check([7], divisor))
print(captured())
""",
    )
