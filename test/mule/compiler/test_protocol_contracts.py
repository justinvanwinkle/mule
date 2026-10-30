"""Compiled protocol entries preserve caller-proven argument domains."""

import pytest

from mule.compiler.codegen import Program
from mule.compiler.ctype import SET_TYPE, UNKNOWN, InstanceType
from mule.compiler.node import ClassConstructorTarget
from mule.compiler.sequence_protocol import prepare, protocol_argument_types


def test_protocol_domain_for_unconstrained_protocol_returns_none():
    layout = InstanceType("implementation", fixed_slots=("entries",))
    target = ClassConstructorTarget(base_type=SET_TYPE, layout_type=layout)
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
    assert protocol_argument_types("getitem", targets) is None


def test_protocol_preparation_keeps_general_body():
    program = Program([], None, {}, unit="builtins", exports=True, bootstrap=True)
    _, functions, _ = prepare(program)
    for name in ("contains", "getitem", "setitem"):
        function = functions[name]
        assert function.entry_parameter_types is None
        facts = program.facts[function]
        assert facts.parameters[0] is UNKNOWN
