"""Compiled protocol entries preserve caller-proven argument domains."""

import pytest

from mule.compiler import fndb
from mule.compiler.codegen import Program
from mule.compiler.ctype import SET_TYPE, UNKNOWN, InstanceType
from mule.compiler.node import ClassConstructorTarget
from mule.compiler.sequence_protocol import prepare, protocol_argument_types


@pytest.mark.parametrize(
    ("name", "arity"),
    [("set_add", 2), ("set_discard", 2), ("set_remove", 2)],
)
def test_exact_set_protocol_declares_receiver_domain(name, arity):
    assert fndb.PROTOCOLS[name].argument_types == (
        SET_TYPE,
        *((UNKNOWN,) * (arity - 1)),
    )


def test_protocol_domain_uses_owner_layout_and_leaves_other_arguments_unknown():
    layout = InstanceType("implementation", fixed_slots=("entries",))
    target = ClassConstructorTarget(base_type=SET_TYPE, layout_type=layout)
    assert protocol_argument_types("set_add", {"set": target}) == (layout, UNKNOWN)
    assert protocol_argument_types("getitem", {"set": target}) is None


@pytest.mark.parametrize(
    "targets",
    [
        {},
        {"set": ClassConstructorTarget(base_type=SET_TYPE)},
        {
            "first": ClassConstructorTarget(
                base_type=SET_TYPE,
                layout_type=InstanceType("first", fixed_slots=("entries",)),
            ),
            "second": ClassConstructorTarget(
                base_type=SET_TYPE,
                layout_type=InstanceType("second", fixed_slots=("entries",)),
            ),
        },
    ],
)
def test_protocol_domain_without_unique_owner_layout_keeps_general_entry(targets):
    assert protocol_argument_types("set_add", targets) is None


def test_protocol_preparation_keeps_general_body_and_adds_declared_entry():
    program = Program([], None, {}, unit="builtins", exports=True, bootstrap=True)
    _, functions, _ = prepare(program)
    for name in ("set_add",):
        function = functions[name]
        declared = function.entry_parameter_types
        assert declared is not None
        assert declared[0].fixed_slot_index("_used") == 4
        facts = program.facts[function]
        assert facts.parameters[0] is UNKNOWN
        context = next(
            context for context in facts.variants if context.parameters == declared
        )
        assert context is not None
