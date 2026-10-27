"""A predicted field receiver does not establish its callee's return type."""

import pytest

from mule.compiler.ctype import PYTHON_INTEGER, UNKNOWN
from mule.compiler.protocols import compile_protocol


@pytest.mark.parametrize("checked_return", [False, True])
def test_predicted_physical_receiver_preserves_general_return(checked_return):
    annotation = " -> int" if checked_return else ""
    compilation = compile_protocol(
        """
class A:
    __slots__ = ('field',)
    def __init__(self):
        self.field = 7
class Envelope:
    __slots__ = ('child',)
    def __init__(self):
        self.child = A()
    def read(self):
        child = self.child
        if child is None:
            return None
        return read_field(child)
"""
        + f"def read_field(value){annotation}:\n"
        + "    return _instance_field(value, 'field')\n",
        "<physical-return>",
        namespace="$mule.physical_return",
    )
    reader = next(
        function
        for function in compilation.lambdas
        if function.code is not None and function.code.qualname == "Envelope.read"
    )
    variants = compilation.facts[reader].variants
    assert variants
    for context in variants:
        if checked_return:
            assert context.return_type.narrow_none(False).fits(PYTHON_INTEGER)
        else:
            assert context.return_type is UNKNOWN


def test_unknown_return_forwarder_keeps_narrow_argument_context():
    compilation = compile_protocol(
        """
class Box:
    __slots__ = ('field',)
    def __init__(self, value):
        self.field = value
def helper(box):
    return _instance_field(box, 'field')
def forward(box):
    return helper(box)
forward(Box(None))
""",
        "<physical-forward>",
        namespace="$mule.physical_forward",
    )
    forward = next(
        function
        for function in compilation.lambdas
        if function.code is not None and function.code.qualname == "forward"
    )
    facts = compilation.facts[forward]
    assert facts.return_type is UNKNOWN
    assert facts.variants
    assert any(
        context.parameters[0].fixed_slot_index("field") == 0
        and context.return_type is UNKNOWN
        for context in facts.variants
    )
