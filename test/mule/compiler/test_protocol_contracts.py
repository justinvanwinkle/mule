"""Compiled protocol entries preserve caller-proven argument domains."""

import pytest

from mule.compiler import fndb
from mule.compiler.codegen import Program
from mule.compiler.ctype import DICT_TYPE, UNKNOWN, InstanceType, IntegerType
from mule.compiler.ir1util import known_calls
from mule.compiler.node import ClassConstructorTarget
from mule.compiler.sequence_protocol import prepare, protocol_argument_types


@pytest.mark.parametrize(
    ("name", "arity"),
    [("dict_get", 3), ("dict_getitem", 2), ("dict_setitem", 3)],
)
def test_exact_dict_protocol_declares_receiver_domain(name, arity):
    assert fndb.PROTOCOLS[name].argument_types == (
        DICT_TYPE,
        *((UNKNOWN,) * (arity - 1)),
    )


def test_protocol_domain_uses_owner_layout_and_leaves_other_arguments_unknown():
    layout = InstanceType("implementation", fixed_slots=("entries",))
    target = ClassConstructorTarget(base_type=DICT_TYPE, layout_type=layout)
    assert protocol_argument_types("dict_get", {"dict": target}) == (
        layout,
        UNKNOWN,
        UNKNOWN,
    )
    assert protocol_argument_types("getitem", {"dict": target}) is None


@pytest.mark.parametrize(
    "targets",
    [
        {},
        {"dict": ClassConstructorTarget(base_type=DICT_TYPE)},
        {
            "first": ClassConstructorTarget(
                base_type=DICT_TYPE,
                layout_type=InstanceType("first", fixed_slots=("entries",)),
            ),
            "second": ClassConstructorTarget(
                base_type=DICT_TYPE,
                layout_type=InstanceType("second", fixed_slots=("entries",)),
            ),
        },
    ],
)
def test_protocol_domain_without_unique_owner_layout_keeps_general_entry(targets):
    assert protocol_argument_types("dict_get", targets) is None


def test_protocol_preparation_keeps_general_body_and_adds_declared_entry():
    program = Program([], None, {}, unit="builtins", exports=True, bootstrap=True)
    _, functions, _ = prepare(program)
    for name in ("dict_get", "dict_getitem", "dict_setitem"):
        function = functions[name]
        declared = function.entry_parameter_types
        assert declared is not None
        assert declared[0].fixed_slot_index("_indices") == 0
        facts = program.facts[function]
        assert facts.parameters[0] is UNKNOWN
        context = next(
            context for context in facts.variants if context.parameters == declared
        )
        indices = [
            node
            for node in known_calls(function, "%physical_field_ref")
            if node.info.name == "_indices"
        ]
        assert indices
        assert all(
            context.types[node].getitem_type(IntegerType(0, 0)).is_fixnum
            for node in indices
        )
