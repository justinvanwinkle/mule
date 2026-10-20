"""Tests for flow-sensitive container element/entry tracking, iteration protocol
element propagation, fixnum range preparation, 64-element tuple literals, and
pre-populated global constant Ref propagation."""

from mule import ast
from mule.compiler.annotation_types import AnnotationLedger
from mule.compiler.constraint import constant_type, propagate
from mule.compiler.ctype import (
    BOOLEAN_TRUE,
    EMPTY_LIST_TYPE,
    TEXT_TYPE,
    UNKNOWN,
    DictType,
    IntegerType,
    TupleType,
)
from mule.compiler.main import compile_source, compile_toplevel
from mule.compiler.node import Combination, GlobalVar, Ref, Set
from mule.parser import parse


def lower(source: str):
    return compile_source(source)


def function(compilation, index: int):
    return compilation.lambdas[index]


def nodes(lambda_):
    return [node for block in lambda_.component.blocks() for node in block.nodes()]


def stored_value(lambda_, name: str):
    return next(
        node.value
        for node in nodes(lambda_)
        if isinstance(node, Set) and node.var.name == name
    )


def fact(compilation, lambda_, node):
    return compilation.facts[lambda_].types[node]


def test_empty_list_append_and_for_iteration_infers_element_type():
    compilation = lower(
        "def f(n: int):\n"
        " positive_weights = []\n"
        " positive_weights.append(1)\n"
        " total = 0\n"
        " for w in positive_weights:\n"
        "  cur = w\n"
        "  total = total + cur\n"
        " return total\n"
    )
    fn = function(compilation, 1)
    assert (
        fact(compilation, fn, stored_value(fn, "positive_weights").use())
        == EMPTY_LIST_TYPE
    )
    w_type = fact(compilation, fn, stored_value(fn, "cur").use())
    assert w_type == IntegerType(1, 1)
    assert w_type.is_fixnum is True


def test_list_constructor_from_argument_has_unknown_length():
    compilation = lower(
        "def f(source):\n values = list(source)\n n = len(values)\n return n\n"
    )
    fn = function(compilation, 1)
    assert fact(compilation, fn, stored_value(fn, "n").use()) != IntegerType(0, 0)


def test_dict_get_setitem_and_items_iteration_infers_entry_types():
    compilation = lower(
        "def f(words):\n"
        " counts = {}\n"
        " for raw in words:\n"
        "  k = str(raw)\n"
        "  counts[k] = counts.get(k, 0) + 1\n"
        " total = 0\n"
        " for token, count in counts.items():\n"
        "  seen_token = token\n"
        "  seen_count = count\n"
        "  total = total + seen_count\n"
        " return total\n"
    )
    fn = function(compilation, 1)
    token_type = fact(compilation, fn, stored_value(fn, "seen_token").use())
    count_type = fact(compilation, fn, stored_value(fn, "seen_count").use())
    assert token_type == TEXT_TYPE
    assert count_type == IntegerType(1, None)


def test_dict_keys_and_values_iteration_infers_types():
    compilation = lower(
        "def f(k_in):\n"
        " d = {str(k_in): 42}\n"
        " for k in d.keys():\n"
        "  out_k = k\n"
        " for v in d.values():\n"
        "  out_v = v\n"
        " return d\n"
    )
    fn = function(compilation, 1)
    assert fact(compilation, fn, stored_value(fn, "d").use()) == DictType(
        TEXT_TYPE, IntegerType(42, 42)
    )
    assert fact(compilation, fn, stored_value(fn, "out_k").use()) == TEXT_TYPE
    assert fact(compilation, fn, stored_value(fn, "out_v").use()) == IntegerType(42, 42)


def test_range_prepare_and_range_advance_with_fixnum_bound():
    compilation = compile_toplevel(
        "def f(x: int):\n"
        " n = x & 1023\n"
        " total = 0\n"
        " for i in range(n):\n"
        "  idx = i\n"
        "  total = total + idx\n"
        " return total\n",
        "test_range.py",
    )
    fn = compilation.lambdas[1]
    body = propagate(
        fn,
        {},
        [IntegerType()],
        lambda *_args: UNKNOWN,
        AnnotationLedger(),
    )
    types = body.types()
    range_prepares = [
        node
        for node in nodes(fn)
        if isinstance(node, Combination)
        and node.fun_info is not None
        and node.fun_info.name == "%range_prepare"
    ]
    assert len(range_prepares) == 1
    assert types[range_prepares[0]] is BOOLEAN_TRUE
    idx_types = [
        types[node.value.use()]
        for node in nodes(fn)
        if isinstance(node, Set)
        and node.var.name == "idx"
        and node.value.use() in types
    ]
    assert idx_types == [IntegerType(0, 1022)]
    assert idx_types[0].is_fixnum is True
    assert body.return_type == IntegerType(0, None)


def test_tuple_literal_up_to_64_elements_retains_element_types():
    words = ", ".join(f"'w{i}'" for i in range(32))
    expr = parse(f"({words})", mode="eval")
    assert isinstance(expr, ast.ExpressionInput)
    tup_type = constant_type(expr.expression)
    assert isinstance(tup_type, TupleType)
    assert tup_type.arity == 32
    assert tup_type.elements is not None
    assert len(tup_type.elements) == 32
    assert tup_type.getitem_type(IntegerType(0, 31)) == TEXT_TYPE


def test_prepopulated_global_ref_derived_type_propagates_across_invalidation():
    compilation = compile_toplevel(
        "_EMPTY = -1\ndef f(g):\n g()\n val = _EMPTY\n return val\n",
        "test_global_const.py",
    )
    fn = compilation.lambdas[1]
    for node in nodes(fn):
        if (
            isinstance(node, Ref)
            and isinstance(node.leaf, GlobalVar)
            and node.leaf.name == "_EMPTY"
        ):
            node.derived_type = IntegerType(-1, -1)
    body = propagate(
        fn,
        {},
        [UNKNOWN],
        lambda *_args: UNKNOWN,
        AnnotationLedger(),
    )
    types = body.types()
    assert types[stored_value(fn, "val").use()] == IntegerType(-1, -1)
