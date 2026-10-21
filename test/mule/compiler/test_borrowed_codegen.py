"""Borrowed acquisitions remain distinct from owners of the same object."""

import pytest

from mule.compiler.codegen import (
    FunctionGenerator,
    ModuleGenerator,
    Program,
    _needs_cleanup_release,
    compile_component,
)
from mule.compiler.ctype import UNKNOWN
from mule.compiler.ltn import EntryContext
from mule.compiler.main import compile_toplevel, ir1_phases
from mule.compiler.vop import DESCRIPTOR, FLOAT_BOX_LAYOUT, T, TNRef


def generator():
    compilation = compile_toplevel("def keep(value):\n return value\n")
    ir1_phases(compilation)
    function = compilation.lambdas[1]
    context = EntryContext(None, parameters=(UNKNOWN,))
    component = compile_component(function, context)
    assert compilation.facts is not None
    program = Program(compilation.lambdas, compilation.entry, compilation.facts)
    return FunctionGenerator(ModuleGenerator(program), component)


@pytest.mark.parametrize("private", [False, True])
def test_retaining_borrow_acquires_independently_releasable_owner(private):
    gen = generator()
    root = gen.component.tn(T, name="$owner")
    root.sc = DESCRIPTOR
    borrowed = gen.component.tn(T)
    borrowed.sc = DESCRIPTOR
    borrowed.borrowed_from = root
    if private:
        borrowed.private_box = FLOAT_BOX_LAYOUT
    gen.record_facts(borrowed, "%borrow")
    assert not gen.owns_reference("%borrow")

    owned = gen.retain("%borrow")
    assert owned != "%borrow"
    assert owned not in gen.borrowed_values
    assert gen.owns_reference(owned)
    gen.release(owned)
    emitted = len(gen.lines)
    gen.release("%borrow")
    assert len(gen.lines) == emitted
    assert any("release" in line and f"ptr {owned}" in line for line in gen.lines)


def test_spilled_borrow_reloads_remain_nonowning_and_do_not_consume_slot():
    gen = generator()
    root = gen.component.tn(T, name="$owner")
    root.sc = DESCRIPTOR
    borrowed = gen.component.tn(T)
    borrowed.sc = DESCRIPTOR
    borrowed.borrowed_from = root
    borrowed.slot = "%borrowed.slot"
    ref = TNRef(borrowed, False)

    first = gen.take(ref)
    second = gen.take(ref)
    assert first != second
    assert {first, second} <= gen.borrowed_values
    assert not _needs_cleanup_release(borrowed)
    assert _needs_cleanup_release(root)
    assert not any("store ptr null" in line for line in gen.lines)
    gen.release(first)
    gen.release(second)
    assert not any("release" in line for line in gen.lines)
