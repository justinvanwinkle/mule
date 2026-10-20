"""Tests for IR1 transforms in ``mule.compiler.typetran``."""

import pytest

from mule import ast
from mule.compiler import typetran  # noqa: F401
from mule.compiler.ctype import (
    BOOLEAN,
    BOOLEAN_FALSE,
    BOOLEAN_TRUE,
    BYTES_TYPE,
    FLOAT,
    FUNCTION_TYPE,
    IMMORTAL,
    INTEGER,
    LIST_TYPE,
    NONE_TYPE,
    OPTIONAL_FIXNUM,
    PYTHON_INTEGER,
    REAL,
    TEXT_TYPE,
    UNKNOWN,
    IntegerType,
    OptionalIntegerType,
    TextType,
    TupleType,
)
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1opt import constant_literal, ir1_optimize_until_done
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source, compile_toplevel
from mule.compiler.node import If, Ref, Return, Set


def test_double_negation_eliminated_for_boolean_operand():
    compilation = compile_toplevel("def f(b):\n return not (not b)\n")
    fn = compilation.lambdas[1]
    fn.vars[0].type = BOOLEAN
    assert len(list(known_calls(fn, "%unary_not"))) == 2
    assert fn.component is not None
    assert ir1_optimize_until_done(fn.component)
    assert list(known_calls(fn, "%unary_not")) == []
    ret = fn.return_
    assert isinstance(ret, Return)
    ret_use = ret.result.use()
    assert isinstance(ret_use, Ref) and ret_use.leaf is fn.vars[0]
    check_ir1_consistency(compilation.components)


def test_double_negation_eliminated_after_flow_analysis():
    compilation = compile_source("def f(x):\n b = bool(x)\n return not (not b)\n")
    fn = compilation.lambdas[1]
    assert list(known_calls(fn, "%unary_not")) == []
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize("operand_type", [UNKNOWN, INTEGER, PYTHON_INTEGER, TEXT_TYPE])
def test_double_negation_preserved_for_non_boolean_operand(operand_type):
    compilation = compile_toplevel("def f(x):\n return not (not x)\n")
    fn = compilation.lambdas[1]
    fn.vars[0].type = operand_type
    assert fn.component is not None
    ir1_optimize_until_done(fn.component)
    assert len(list(known_calls(fn, "%unary_not"))) == 2
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize(
    ("expr", "original_known", "inverted_known"),
    [
        ("not (a is b)", "%compare_is", "%compare_is_not"),
        ("not (a is not b)", "%compare_is_not", "%compare_is"),
        ("not (a in b)", "%compare_in", "%compare_not_in"),
        ("not (a not in b)", "%compare_not_in", "%compare_in"),
    ],
)
def test_negated_identity_and_containment_comparisons_invert(
    expr, original_known, inverted_known
):
    compilation = compile_source(f"def f(a, b):\n return {expr}\n")
    fn = compilation.lambdas[1]
    assert list(known_calls(fn, "%unary_not")) == []
    assert list(known_calls(fn, original_known)) == []
    inverted = list(known_calls(fn, inverted_known))
    assert len(inverted) == 1
    assert [
        u.leaf for arg in inverted[0].args if isinstance(u := arg.use(), Ref)
    ] == list(fn.vars)
    check_ir1_consistency(compilation.components)


def test_double_negated_comparison_simplifies_to_original_comparison():
    compilation = compile_source("def f(a, b):\n return not (not (a is b))\n")
    fn = compilation.lambdas[1]
    assert list(known_calls(fn, "%unary_not")) == []
    assert list(known_calls(fn, "%compare_is_not")) == []
    assert len(list(known_calls(fn, "%compare_is"))) == 1
    check_ir1_consistency(compilation.components)


def test_identity_comparison_folds_none_and_boolean_singletons():
    compilation = compile_source(
        "def f():\n"
        " a = None\n"
        " b = None\n"
        " t = True\n"
        " fa = False\n"
        " return a is b, a is not b, t is True, t is fa, t is not fa\n"
    )
    fn = compilation.lambdas[1]
    assert list(known_calls(fn, "%compare_is")) == []
    assert list(known_calls(fn, "%compare_is_not")) == []
    ret = fn.return_
    assert isinstance(ret, Return)
    folded = constant_literal(ret.result)
    assert isinstance(folded, ast.Tuple)
    assert [elem.value for elem in folded.elements] == [True, False, True, False, True]
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize(
    "disjoint_type",
    [
        PYTHON_INTEGER,
        INTEGER,
        IntegerType(0, 10),
        BOOLEAN,
        BOOLEAN_TRUE,
        BOOLEAN_FALSE,
        FLOAT,
        REAL,
        TEXT_TYPE,
        BYTES_TYPE,
        LIST_TYPE,
        TupleType(2),
        FUNCTION_TYPE,
    ],
)
def test_identity_comparison_with_none_folds_for_disjoint_types(disjoint_type):
    compilation = compile_toplevel(
        "def f(x):\n return x is None, x is not None, None is x, None is not x\n"
    )
    fn = compilation.lambdas[1]
    fn.vars[0].type = disjoint_type
    assert fn.component is not None
    assert ir1_optimize_until_done(fn.component)
    assert list(known_calls(fn, "%compare_is")) == []
    assert list(known_calls(fn, "%compare_is_not")) == []
    ret = fn.return_
    assert isinstance(ret, Return)
    folded = constant_literal(ret.result)
    assert isinstance(folded, ast.Tuple)
    assert [elem.value for elem in folded.elements] == [False, True, False, True]
    check_ir1_consistency(compilation.components)


def test_annotated_int_is_none_folds_and_eliminates_dead_branch():
    compilation = compile_source(
        "def f(x: int) -> int:\n"
        " if x is None:\n"
        "  return 1\n"
        " if x is not None:\n"
        "  return x + 2\n"
        " return 3\n"
    )
    fn = compilation.lambdas[1]
    assert list(known_calls(fn, "%compare_is")) == []
    assert list(known_calls(fn, "%compare_is_not")) == []
    assert fn.component is not None
    branches = [
        node
        for block in fn.component.blocks()
        for node in block.nodes()
        if isinstance(node, If)
    ]
    assert branches == []
    assert len(list(known_calls(fn, "%add"))) == 1
    check_ir1_consistency(compilation.components)


def test_negated_is_none_on_annotated_int_inverts_and_folds():
    compilation = compile_source(
        "def f(x: int) -> int:\n if not (x is not None):\n  return 1\n return x\n"
    )
    fn = compilation.lambdas[1]
    assert list(known_calls(fn, "%unary_not")) == []
    assert list(known_calls(fn, "%compare_is")) == []
    assert list(known_calls(fn, "%compare_is_not")) == []
    assert fn.component is not None
    branches = [
        node
        for block in fn.component.blocks()
        for node in block.nodes()
        if isinstance(node, If)
    ]
    assert branches == []
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize(
    "non_disjoint_type",
    [
        OPTIONAL_FIXNUM,
        OptionalIntegerType(),
        OptionalIntegerType(IntegerType(0, 5)),
        IMMORTAL,
        UNKNOWN,
        INTEGER.with_unbound(),
        NONE_TYPE.with_unbound(),
    ],
)
def test_optional_integer_and_nullable_types_do_not_fold_is_none(non_disjoint_type):
    compilation = compile_toplevel("def f(x):\n return x is None, x is not None\n")
    fn = compilation.lambdas[1]
    fn.vars[0].type = non_disjoint_type
    assert fn.component is not None
    ir1_optimize_until_done(fn.component)
    assert len(list(known_calls(fn, "%compare_is"))) == 1
    assert len(list(known_calls(fn, "%compare_is_not"))) == 1
    check_ir1_consistency(compilation.components)


def test_identity_comparison_preserves_side_effects_and_possibly_unbound_reads():
    compilation = compile_source(
        "def make_int() -> int:\n"
        " return 7\n"
        "def f(c):\n"
        " if c:\n"
        "  x = 1\n"
        " return make_int() is None, x is None\n"
    )
    fn = next(lam for lam in compilation.lambdas if lam.source_name == "f")
    assert len(list(known_calls(fn, "%compare_is"))) == 2
    check_ir1_consistency(compilation.components)


def test_negative_index_check_eliminated_by_interval_comparison_folding():
    compilation = compile_source(
        "def f(x: int):\n i = x & 255\n if i < 0:\n  return -1\n return i\n"
    )
    fn = compilation.lambdas[1]
    assert list(known_calls(fn, "%compare_less")) == []
    assert fn.component is not None
    branches = [
        node
        for block in fn.component.blocks()
        for node in block.nodes()
        if isinstance(node, If)
    ]
    assert branches == []
    i_var = next(
        node.var
        for block in fn.component.blocks()
        for node in block.nodes()
        if isinstance(node, Set) and node.var.name == "i"
    )
    assert len(i_var.refs) == 1
    assert fn.return_ is not None
    assert i_var.refs[0].lvar is fn.return_.result
    check_ir1_consistency(compilation.components)


def test_integer_interval_comparisons_fold_across_all_six_operators():
    compilation = compile_source(
        "def f(x: int):\n"
        " i = x & 15\n"
        " return (\n"
        "  i < 16,\n"
        "  i < 0,\n"
        "  i <= 15,\n"
        "  i <= -1,\n"
        "  i > -1,\n"
        "  i > 15,\n"
        "  i >= 0,\n"
        "  i >= 16,\n"
        "  i == 16,\n"
        "  i == -1,\n"
        "  i != -1,\n"
        "  i != 16,\n"
        " )\n"
    )
    fn = compilation.lambdas[1]
    for name in (
        "%compare_less",
        "%compare_less_equal",
        "%compare_greater",
        "%compare_greater_equal",
        "%compare_equal",
        "%compare_not_equal",
    ):
        assert list(known_calls(fn, name)) == []
    assert fn.component is not None
    i_var = next(
        node.var
        for block in fn.component.blocks()
        for node in block.nodes()
        if isinstance(node, Set) and node.var.name == "i"
    )
    assert i_var.refs == []
    ret = fn.return_
    assert isinstance(ret, Return)
    folded = constant_literal(ret.result)
    assert isinstance(folded, ast.Tuple)
    assert [elem.value for elem in folded.elements] == [
        True,
        False,
        True,
        False,
        True,
        False,
        True,
        False,
        False,
        False,
        True,
        True,
    ]
    check_ir1_consistency(compilation.components)


def test_disjoint_two_variable_intervals_fold():
    compilation = compile_toplevel(
        "def f(a, b):\n return a < b, a <= b, a > b, a >= b, a == b, a != b\n"
    )
    fn = compilation.lambdas[1]
    fn.vars[0].type = IntegerType(0, 15)
    fn.vars[1].type = IntegerType(16, 31)
    assert fn.component is not None
    assert ir1_optimize_until_done(fn.component)
    for name in (
        "%compare_less",
        "%compare_less_equal",
        "%compare_greater",
        "%compare_greater_equal",
        "%compare_equal",
        "%compare_not_equal",
    ):
        assert list(known_calls(fn, name)) == []
    assert fn.vars[0].refs == []
    assert fn.vars[1].refs == []
    ret = fn.return_
    assert isinstance(ret, Return)
    folded = constant_literal(ret.result)
    assert isinstance(folded, ast.Tuple)
    assert [elem.value for elem in folded.elements] == [
        True,
        True,
        False,
        False,
        False,
        True,
    ]
    check_ir1_consistency(compilation.components)


def test_overlapping_integer_intervals_do_not_fold():
    compilation = compile_source(
        "def f(x: int):\n"
        " i = x & 15\n"
        " return i < 10, i <= 10, i > 10, i >= 10, i == 10, i != 10\n"
    )
    fn = compilation.lambdas[1]
    for name in (
        "%compare_less",
        "%compare_less_equal",
        "%compare_greater",
        "%compare_greater_equal",
        "%compare_equal",
        "%compare_not_equal",
    ):
        assert len(list(known_calls(fn, name))) == 1
    check_ir1_consistency(compilation.components)


def test_singleton_variable_comparisons_and_side_effects_are_preserved():
    compilation = compile_toplevel("def f(a, b):\n return a < b, a == b\n")
    fn = compilation.lambdas[1]
    fn.vars[0].type = IntegerType(1, 1)
    fn.vars[1].type = IntegerType(2, 2)
    assert fn.component is not None
    ir1_optimize_until_done(fn.component)
    assert len(list(known_calls(fn, "%compare_less"))) == 1
    assert len(list(known_calls(fn, "%compare_equal"))) == 1
    check_ir1_consistency(compilation.components)


def test_text_literal_comparisons_fold_across_all_six_operators():
    compilation = compile_toplevel(
        "def f(s):\n"
        " return (\n"
        '  s == "true",\n'
        '  s == "false",\n'
        '  s != "false",\n'
        '  s != "true",\n'
        '  s < "z",\n'
        '  s <= "true",\n'
        '  s > "false",\n'
        '  s >= "z",\n'
        " )\n"
    )
    fn = compilation.lambdas[1]
    fn.vars[0].type = TextType("true")
    assert fn.component is not None
    assert ir1_optimize_until_done(fn.component)
    for name in (
        "%compare_less",
        "%compare_less_equal",
        "%compare_greater",
        "%compare_greater_equal",
        "%compare_equal",
        "%compare_not_equal",
    ):
        assert list(known_calls(fn, name)) == []
    assert fn.vars[0].refs == []
    ret = fn.return_
    assert isinstance(ret, Return)
    folded = constant_literal(ret.result)
    assert isinstance(folded, ast.Tuple)
    assert [elem.value for elem in folded.elements] == [
        True,
        False,
        True,
        False,
        True,
        True,
        True,
        False,
    ]
    check_ir1_consistency(compilation.components)


def test_non_literal_text_comparisons_do_not_fold():
    compilation = compile_source(
        'def f(raw):\n s = str(raw)\n return s == "true", s != "false", s < "z"\n'
    )
    fn = compilation.lambdas[1]
    assert len(list(known_calls(fn, "%compare_equal"))) == 1
    assert len(list(known_calls(fn, "%compare_not_equal"))) == 1
    assert len(list(known_calls(fn, "%compare_less"))) == 1
    check_ir1_consistency(compilation.components)


def test_disjoint_type_equality_and_inequality_fold():
    compilation = compile_source(
        "def f(x: int, raw, a: int, c: int):\n"
        " s = str(raw)\n"
        " b = s.encode()\n"
        " t = (a, c)\n"
        " return (\n"
        '  x == "abc",\n'
        '  x != "abc",\n'
        "  s == None,\n"
        "  s != None,\n"
        '  b == "abc",\n'
        '  b != "abc",\n'
        "  t == [1, 2],\n"
        "  t != [1, 2],\n"
        "  (x, a) == (x, a, c),\n"
        "  (x, a) != (x, a, c),\n"
        " )\n"
    )
    fn = compilation.lambdas[1]
    assert list(known_calls(fn, "%compare_equal")) == []
    assert list(known_calls(fn, "%compare_not_equal")) == []
    assert list(known_calls(fn, "%build_list")) == []
    ret = fn.return_
    assert isinstance(ret, Return)
    folded = constant_literal(ret.result)
    assert isinstance(folded, ast.Tuple)
    assert [elem.value for elem in folded.elements] == [
        False,
        True,
        False,
        True,
        False,
        True,
        False,
        True,
        False,
        True,
    ]
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize(
    ("left_type", "right_type"),
    [
        (INTEGER, BOOLEAN),
        (PYTHON_INTEGER, FLOAT),
        (INTEGER, REAL),
        (TupleType(2), TupleType(2)),
        (TupleType(2), TupleType(None)),
        (OPTIONAL_FIXNUM, NONE_TYPE),
        (IMMORTAL, NONE_TYPE),
        (UNKNOWN, TEXT_TYPE),
    ],
)
def test_non_disjoint_equality_families_do_not_fold(left_type, right_type):
    compilation = compile_toplevel("def f(a, b):\n return a == b, a != b\n")
    fn = compilation.lambdas[1]
    fn.vars[0].type = left_type
    fn.vars[1].type = right_type
    assert fn.component is not None
    ir1_optimize_until_done(fn.component)
    assert len(list(known_calls(fn, "%compare_equal"))) == 1
    assert len(list(known_calls(fn, "%compare_not_equal"))) == 1
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize(
    ("expr", "original_known", "inverted_known"),
    [
        ("not (a == b)", "%compare_equal", "%compare_not_equal"),
        ("not (a != b)", "%compare_not_equal", "%compare_equal"),
        ("not (a < b)", "%compare_less", "%compare_greater_equal"),
        ("not (a <= b)", "%compare_less_equal", "%compare_greater"),
        ("not (a > b)", "%compare_greater", "%compare_less_equal"),
        ("not (a >= b)", "%compare_greater_equal", "%compare_less"),
    ],
)
@pytest.mark.parametrize(
    "domain_type",
    [PYTHON_INTEGER, INTEGER, BOOLEAN, TEXT_TYPE, TextType("tag"), BYTES_TYPE],
)
def test_negated_order_comparisons_invert_for_exact_total_order_domains(
    expr, original_known, inverted_known, domain_type
):
    compilation = compile_toplevel(f"def f(a, b):\n return {expr}\n")
    fn = compilation.lambdas[1]
    fn.vars[0].type = domain_type
    fn.vars[1].type = domain_type if domain_type != TextType("tag") else TEXT_TYPE
    assert fn.component is not None
    assert ir1_optimize_until_done(fn.component)
    assert list(known_calls(fn, "%unary_not")) == []
    assert list(known_calls(fn, original_known)) == []
    inverted = list(known_calls(fn, inverted_known))
    assert len(inverted) == 1
    assert [
        u.leaf for arg in inverted[0].args if isinstance(u := arg.use(), Ref)
    ] == list(fn.vars)
    check_ir1_consistency(compilation.components)


def test_negated_annotated_int_and_str_comparisons_invert():
    compilation = compile_source(
        "def f(x: int, n: int, raw):\n"
        " s = str(raw)\n"
        ' return not (x < n), not (x == 0), not (s == "tag")\n'
    )
    fn = compilation.lambdas[1]
    assert list(known_calls(fn, "%unary_not")) == []
    assert list(known_calls(fn, "%compare_less")) == []
    assert list(known_calls(fn, "%compare_equal")) == []
    assert len(list(known_calls(fn, "%compare_greater_equal"))) == 1
    assert len(list(known_calls(fn, "%compare_not_equal"))) == 2
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize(
    ("left_type", "right_type"),
    [
        (FLOAT, FLOAT),
        (REAL, REAL),
        (INTEGER, FLOAT),
        (UNKNOWN, UNKNOWN),
        (TupleType(2), TupleType(2)),
        (LIST_TYPE, LIST_TYPE),
    ],
)
def test_negated_order_comparisons_do_not_invert_for_float_or_dynamic_types(
    left_type, right_type
):
    compilation = compile_toplevel("def f(a, b):\n return not (a < b), not (a == b)\n")
    fn = compilation.lambdas[1]
    fn.vars[0].type = left_type
    fn.vars[1].type = right_type
    assert fn.component is not None
    ir1_optimize_until_done(fn.component)
    assert len(list(known_calls(fn, "%unary_not"))) == 2
    assert len(list(known_calls(fn, "%compare_less"))) == 1
    assert len(list(known_calls(fn, "%compare_equal"))) == 1
    check_ir1_consistency(compilation.components)
