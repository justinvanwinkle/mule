"""Same-unit inferred results become checked IR1 facts, not producer promises."""

import pytest

from mule.compiler.ctype import NONE_TYPE, UNKNOWN, IntegerType, TupleType
from mule.compiler.main import compile_source
from mule.compiler.node import CallKind, Cast, Combination


@pytest.mark.parametrize(
    "left,right", [("3", "10"), ('"a"', '"b"'), ("(3, 'a')", "(10, 'b')")]
)
def test_predicted_same_unit_result_is_refined_only_after_cast(left, right):
    compilation = compile_source(
        f"def produce(flag):\n return {left} if flag else {right}\n"
        "def consume(flag):\n value = produce(flag)\n return value\n"
    )
    caller = compilation.lambdas[2]
    nodes = [node for block in caller.component.blocks() for node in block.nodes()]
    call = next(
        node
        for node in nodes
        if isinstance(node, Combination) and node.kind is CallKind.GUARDED
    )
    cast = next(node for node in nodes if isinstance(node, Cast))
    assert cast.value.use() is call
    assert call.derived_type is UNKNOWN
    assert cast.derived_type == cast.contract.fact
    assert cast.type_check
    assert call.next.next is cast


def test_tail_call_does_not_introduce_inferred_result_assertion():
    compilation = compile_source(
        "def produce():\n return 3\ndef consume():\n return produce()\n"
    )
    caller = compilation.lambdas[2]
    assert not any(
        isinstance(node, Cast)
        for block in caller.component.blocks()
        for node in block.nodes()
    )


def test_recursive_tuple_result_contract_includes_non_leaf_nodes():
    compilation = compile_source(
        "def make_tree(item, depth):\n"
        " if depth <= 0:\n  return (item, None, None)\n"
        " return (item, make_tree(item, depth - 1), make_tree(item, depth - 1))\n"
        "def consume(depth):\n tree = make_tree(1, depth)\n return tree[0]\n"
    )
    caller = compilation.lambdas[2]
    cast = next(
        node
        for block in caller.component.blocks()
        for node in block.nodes()
        if isinstance(node, Cast)
    )
    leaf = TupleType(3, (IntegerType(1, 1), NONE_TYPE, NONE_TYPE))
    nested = TupleType(3, (IntegerType(1, 1), leaf, leaf))
    assert leaf.fits(cast.contract.fact)
    assert nested.fits(cast.contract.fact)
    assert cast.type_check
