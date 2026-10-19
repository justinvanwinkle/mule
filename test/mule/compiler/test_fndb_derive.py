"""Tests for ``fndb.py`` ``derive_type`` hooks and ``locall.py`` feedback into IR1."""

from contextlib import contextmanager

import pytest

from mule import ast
from mule.compiler import fndb
from mule.compiler.ctype import (
    BOOLEAN,
    BOTTOM,
    BYTES_TYPE,
    FLOAT,
    INTEGER,
    LIST_TYPE,
    TEXT_TYPE,
    UNKNOWN,
    IntegerType,
    ListType,
    TupleType,
)
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1opt import (
    constant_lvar_value,
    ir1_optimize,
    lvar_type,
    replace_combination_with_constant,
    replace_combination_with_lvar,
)
from mule.compiler.ir1util import known_calls
from mule.compiler.knownfun import defoptimizer, deftransform, fun_info
from mule.compiler.main import compile_source, compile_toplevel
from mule.compiler.node import (
    Combination,
    Lvar,
    ValuedNode,
)


@contextmanager
def saved_fun_info(*names: str):
    infos = [fun_info(name) for name in names]
    snapshots = [
        (info, info.derive_type, info.optimizer, info.transforms) for info in infos
    ]
    try:
        yield
    finally:
        for info, derive_type, optimizer, transforms in snapshots:
            object.__setattr__(info, "derive_type", derive_type)
            object.__setattr__(info, "optimizer", optimizer)
            object.__setattr__(info, "transforms", transforms)


def test_fndb_arg_type_handles_empty_cached_and_multi_use_lvars():
    empty = Lvar()
    assert fndb._arg_type(empty) is BOTTOM

    compilation = compile_toplevel("def f():\n return 10, 20\n")
    fn = compilation.lambdas[1]
    tuple_call = next(known_calls(fn, "%build_tuple"))
    first_use = tuple_call.args[0].use()
    second_use = tuple_call.args[1].use()

    joined_lvar = Lvar()
    joined_lvar.uses = [first_use, second_use]
    assert fndb._arg_type(joined_lvar) == IntegerType(10, 20)

    cached_lvar = Lvar()
    cached_lvar.derived_type = TEXT_TYPE
    assert fndb._arg_type(cached_lvar) is TEXT_TYPE


@pytest.mark.parametrize(
    ("source", "known_name", "expected"),
    [
        ("def f():\n return 15 // 3\n", "%floor_divide", IntegerType(5, 5)),
        ("def f():\n return 1.5 + 2\n", "%add", FLOAT),
        ("def f(x):\n return not x\n", "%unary_not", BOOLEAN),
        ("def f():\n return ~(10 // 3)\n", "%unary_invert", IntegerType(-4, -4)),
        ("def f(x, y):\n return x is y\n", "%compare_is", BOOLEAN),
        ("def f():\n return (10 // 2) < 9\n", "%compare_less", BOOLEAN),
        ("def f(x, y):\n return [x, y]\n", "%build_list", LIST_TYPE),
        ("def f(x, y, z):\n return (x, y, z)\n", "%build_tuple", TupleType(3)),
        ("def f(x):\n return f'{x}'\n", "%format", TEXT_TYPE),
        ("def f(i):\n return 'hello'[i]\n", "%getitem", TEXT_TYPE),
    ],
)
def test_fndb_derive_type_during_ir1_optimize(source, known_name, expected):
    compilation = compile_toplevel(source)
    fn = compilation.lambdas[1]
    ir1_optimize(fn.component)
    call = next(known_calls(fn, known_name))
    assert call.derived_type == expected
    assert lvar_type(call.lvar) == expected


def test_fndb_derive_type_for_inplace_getslice_unpack_and_boolean_knowns():
    compilation = compile_toplevel(
        "def f(seq):\n"
        " a, b = seq\n"
        " for i in range(3):\n"
        "  pass\n"
        " return 'hello'[1:3], a, b, i\n"
    )
    fn = compilation.lambdas[1]
    ir1_optimize(fn.component)

    slice_call = next(known_calls(fn, "%getslice"))
    assert slice_call.derived_type == TEXT_TYPE

    unpack_call = next(known_calls(fn, "%unpack"))
    assert unpack_call.derived_type == TupleType(2)

    for name in ("%range_prepare", "%range_available"):
        call = next(known_calls(fn, name))
        assert call.derived_type == BOOLEAN

    assert fndb.HAS_BINDING.derive_type is not None
    assert fndb.SEQUENCE_AVAILABLE.derive_type is not None

    compilation_iadd = compile_toplevel("def f(x):\n x += 1\n return x\n")
    fn_iadd = compilation_iadd.lambdas[1]
    fn_iadd.vars[0].type = IntegerType(5, 5)
    ir1_optimize(fn_iadd.component)
    assert next(known_calls(fn_iadd, "%iadd")).derived_type == IntegerType(6, 6)

    compilation_bytes = compile_toplevel("def f():\n return b'abc'[1:2]\n")
    fn_bytes = compilation_bytes.lambdas[1]
    ir1_optimize(fn_bytes.component)
    assert next(known_calls(fn_bytes, "%getslice")).derived_type == BYTES_TYPE

    compilation_list = compile_toplevel("def f(x):\n return [x, 1][0:1]\n")
    fn_list = compilation_list.lambdas[1]
    ir1_optimize(fn_list.component)
    assert next(known_calls(fn_list, "%getslice")).derived_type == LIST_TYPE


def test_locall_publishes_derived_types_to_all_valued_nodes_and_lvars():
    compilation = compile_source(
        "def f(flag):\n"
        " x = 10\n"
        " y = 20\n"
        " z = x + y\n"
        " items = [x, y]\n"
        " pair = (x, items)\n"
        " if flag:\n"
        "  res = z\n"
        " else:\n"
        "  res = 40\n"
        " return res, pair\n"
    )
    fn = compilation.lambdas[1]
    general_types = compilation.facts[fn].types

    valued_nodes = [
        node
        for block in fn.component.blocks()
        for node in block.nodes()
        if isinstance(node, ValuedNode)
    ]
    assert valued_nodes
    for node in valued_nodes:
        assert node.derived_type is not None
        assert node.derived_type == general_types.get(node, UNKNOWN)
        if node.lvar is not None:
            assert node.lvar.derived_type is not None
            expected_join = BOTTOM
            for use in node.lvar.uses:
                expected_join = expected_join.join(use.derived_type)
            assert node.lvar.derived_type == expected_join

    ret = fn.return_
    assert ret is not None
    returned_tuple = ret.result.use()
    assert isinstance(returned_tuple, Combination)
    assert returned_tuple.fun_info is fndb.BUILD_TUPLE
    assert returned_tuple.args[0].derived_type == IntegerType(30, 40)
    assert returned_tuple.args[1].derived_type == TupleType(
        2, (IntegerType(10, 10), ListType(IntegerType(10, 20)))
    )


def test_locall_feedback_triggers_ir1opt_transform_on_flow_narrowed_local():
    with saved_fun_info("%floor_divide"):

        @deftransform("%floor_divide", (IntegerType(0, 100), IntegerType(1, 1)))
        def _eliminate_div_by_one(node: Combination) -> bool:
            replace_combination_with_lvar(node, node.args[0])
            return True

        # Before locall runs, `x` is an unannotated local variable so `Ref(x)` has
        # type UNKNOWN in the first ir1opt pass. `locall.analyze` proves `x` is
        # `IntegerType(42, 42)`, publishes it to `node.args[0].derived_type`, and
        # triggers a second `ir1opt` round that fires `_eliminate_div_by_one`.
        compilation = compile_source("def f():\n x = 42\n return x // 1\n")
        fn = compilation.lambdas[1]
        assert list(known_calls(fn, "%floor_divide")) == []
        assert compilation.facts[fn].return_type == IntegerType(42, 42)
        check_ir1_consistency(compilation.components)


def test_locall_feedback_triggers_transform_from_branch_and_call_narrowing():
    with saved_fun_info("%modulo", "%right_shift"):

        @deftransform("%modulo", (IntegerType(2, 9), IntegerType(10, 10)))
        def _small_positive_mod(node: Combination) -> bool:
            replace_combination_with_lvar(node, node.args[0])
            return True

        @defoptimizer("%right_shift")
        def _shift_by_proven_zero(node: Combination) -> bool:
            shift = constant_lvar_value(node.args[1])
            if isinstance(shift, ast.IntegerLiteral) and shift.value == 0:
                replace_combination_with_constant(node, ast.IntegerLiteral(77))
                return True
            return False

        compilation = compile_source(
            "def f(x: int):\n"
            " s = 0\n"
            " a = x >> s\n"
            " if x > 1:\n"
            "  if x < 10:\n"
            "   return (x % 10) + a\n"
            " return a\n"
        )
        fn = compilation.lambdas[1]
        assert list(known_calls(fn, "%modulo")) == []
        assert list(known_calls(fn, "%right_shift")) == []
        check_ir1_consistency(compilation.components)


def test_locall_feedback_terminates_cleanly_when_transform_declines():
    calls = 0
    with saved_fun_info("%floor_divide"):

        @deftransform("%floor_divide", (INTEGER, INTEGER))
        def _declining_transform(node: Combination) -> bool:
            nonlocal calls
            calls += 1
            return False

        compilation = compile_source("def f():\n x = 20\n return x // 3\n")
        fn = compilation.lambdas[1]
        div_call = next(known_calls(fn, "%floor_divide"))
        assert div_call.derived_type == IntegerType(6, 6)
        assert div_call.lvar.derived_type == IntegerType(6, 6)
        assert calls == 1
        check_ir1_consistency(compilation.components)
