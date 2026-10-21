"""Local copy propagation preserves ownership and Python alias semantics."""

import pytest

from mule.compiler import emit_llvm
from mule.compiler.ir2tran import _BorrowedLifetime
from test.mule.compiler.test_borrowed_lifetimes import component_for
from test.mule.compiler.test_exception_groups import compare
from test.mule.compiler.test_llvm_boxing import function_body


@pytest.mark.parametrize(
    "body",
    [
        "    alias = value\n    return alias is value\n",
        "    alias = value\n    other = alias\n    return other is value\n",
        (
            "    alias = value\n    if flag is None:\n        return alias is value\n"
            "    return alias is value\n"
        ),
        (
            "    alias = value\n    result = alias is value\n"
            "    print(result)\n    return result\n"
        ),
    ],
    ids=["local", "chain", "branch", "effect-after-consumption"],
)
def test_stable_local_alias_needs_no_additional_reference(body):
    llvm = emit_llvm("def check(value, flag):\n" + body)
    native = function_body(llvm, '"mule_fn_check.body"')
    assert "call ptr @mule.fast.retain(" not in native


@pytest.mark.parametrize(
    "body",
    [
        "    alias = value\n    value = []\n    return alias is value\n",
        "    alias = value\n    alias = []\n    return alias is value\n",
        "    alias = value\n    value += 1.0\n    return alias is value\n",
        (
            "    alias = value\n    if flag:\n        result = value is None\n"
            "        print(result)\n        return result\n    return alias is value\n"
        ),
        (
            "    alias = value\n    def capture():\n        return alias\n"
            "    return alias is capture()\n"
        ),
        (
            "    while flag:\n        alias = value\n        print(alias is value)\n"
            "        flag = False\n    return value\n"
        ),
        (
            "    alias = value\n    while flag:\n        flag = False\n"
            "        print(alias is value)\n    return True\n"
        ),
        ("    alias = value\n    result = 1 // flag\n    return alias is value\n"),
        (
            "    alias = value\n    if flag:\n        return alias is value\n"
            "    return alias is value\n"
        ),
        (
            "    alias = value\n    while flag is None:\n        pass\n"
            "    return alias is value\n"
        ),
    ],
    ids=[
        "owner-overwrite",
        "alias-overwrite",
        "uniqueness",
        "unread-path",
        "capture",
        "loop-definition",
        "exit-live",
        "implicit-error",
        "truth-callback",
        "loop-safepoint",
    ],
)
def test_alias_with_unproved_lifetime_keeps_its_own_reference(body):
    component = component_for("def check(value, flag):\n" + body)
    aliases = [
        vop.args[0].tn
        for block in component.blocks
        for vop in block.vops
        if vop.info.name == "load-local"
        and vop.codegen_info.name == "alias"
        and (vop.args[0].tn.leaf is None or vop.args[0].tn.leaf.name == "alias")
    ]
    assert aliases
    assert all(alias.borrowed_from is None for alias in aliases)


def test_moved_owner_protects_another_operand_of_its_terminal_operation():
    llvm = emit_llvm("def check(value):\n    return value is value\n")
    native = function_body(llvm, '"mule_fn_check.body"')
    assert "call ptr @mule.fast.retain(" not in native
    assert "call void @mule.fast.release(" in native


def test_transferred_owner_cannot_protect_a_later_borrower_consumer():
    component = component_for(
        "def check(value):\n    alias = value\n    return alias is value\n"
    )
    operations = [vop for block in component.blocks for vop in block.vops]
    transfer = next(
        vop
        for vop in operations
        if vop.info.name == "load-local" and vop.codegen_info.move
    )
    terminal = next(vop for vop in operations if vop.info.name == "compare-is")
    later = next(vop for vop in operations if vop.info.name == "return")
    proof = _BorrowedLifetime(component)
    source = transfer.args[0].tn
    assert proof.transfers_to_consumer(transfer, source, {terminal})
    # Model a second borrower use at the following return program point.
    # Membership of the owner's terminal alone must not validate that region.
    assert not proof.transfers_to_consumer(transfer, source, {terminal, later})


def test_aliases_preserve_values_across_owner_changes_and_callbacks(tmp_path):
    compare(
        tmp_path,
        """
def identity(value):
    alias = value
    other = alias
    return other is value
def replace(value):
    alias = value
    value = [2]
    return alias, value
def append(value):
    value.append(9)
def callback(value):
    alias = value
    append(value)
    return alias, value
def arithmetic(value):
    alias = value
    value += 1.0
    return alias, value
def fail(value):
    alias = value
    try:
        value = 1 // 0
    except ZeroDivisionError:
        return alias, value
def conditional(value, divisor):
    try:
        result = 1 // divisor
        alias = value
    except ZeroDivisionError:
        try:
            return alias
        except UnboundLocalError:
            return "unbound alias"
    return alias is value
def captured(value):
    alias = value
    def read():
        return alias
    value = [2]
    return read(), value
for value in (2.5, [1], "text", 1, None):
    print(identity(value))
print(replace([1]))
print(callback([1]))
print(arithmetic(2.5))
print(fail([1]))
print(conditional([1], 0))
print(conditional([1], 1))
print(captured([1]))
""",
    )
