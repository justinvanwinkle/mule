"""Tests for small sequence containment IR1 transforms in ``mule.compiler.containtran``."""

from mule import ast
from mule.compiler import containtran
from mule.compiler.ctype import INTEGER, TEXT_TYPE, UNKNOWN_UNBOUND, TextType
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1opt import constant_literal, ir1_optimize_until_done
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source, compile_toplevel
from mule.compiler.node import Constant, Ref, Return


def test_empty_tuple_and_list_containment_fold_to_boolean():
    cases = [
        ("x in ()", False),
        ("x in []", False),
        ("x not in ()", True),
        ("x not in []", True),
    ]
    for expr, expected in cases:
        compilation = compile_source(f"def f(x: int):\n return {expr}\n")
        fn = compilation.lambdas[1]
        assert list(known_calls(fn, "%build_tuple")) == []
        assert list(known_calls(fn, "%build_list")) == []
        assert list(known_calls(fn, "%compare_in")) == []
        assert list(known_calls(fn, "%compare_not_in")) == []
        # Only the parameter annotation check reference in the prologue remains.
        assert len(fn.vars[0].refs) == 1
        ret = fn.return_
        assert isinstance(ret, Return)
        lit = constant_literal(ret.result)
        assert isinstance(lit, ast.BooleanLiteral) and lit.value is expected
        check_ir1_consistency(compilation.components)

        comp_top = compile_toplevel(f"def f(x):\n return {expr}\n")
        fn_top = comp_top.lambdas[1]
        fn_top.vars[0].type = INTEGER
        assert ir1_optimize_until_done(fn_top.component)
        assert fn_top.vars[0].refs == []
        check_ir1_consistency(comp_top.components)


def test_unfolded_empty_tuple_containment_direct_transform():
    compilation = compile_toplevel("def f(x):\n return x in ()\n")
    fn = compilation.lambdas[1]
    fn.vars[0].type = INTEGER
    in_call = next(known_calls(fn, "%compare_in"))
    assert containtran._transform_compare_in(in_call)
    assert list(known_calls(fn, "%build_tuple")) == []
    assert list(known_calls(fn, "%compare_in")) == []
    assert fn.vars[0].refs == []
    ret = fn.return_
    assert isinstance(ret, Return)
    lit = constant_literal(ret.result)
    assert isinstance(lit, ast.BooleanLiteral) and lit.value is False
    check_ir1_consistency(compilation.components)


def test_empty_containment_preserves_unbound_or_side_effecting_left_operand():
    # 1. Function call with potential side effects
    comp_call = compile_source("def f(g):\n return g() in ()\n")
    fn_call = comp_call.lambdas[1]
    assert len(list(known_calls(fn_call, "%compare_in"))) == 1
    check_ir1_consistency(comp_call.components)

    # 2. Conditionally bound local variable
    comp_cond = compile_source("def f(c):\n if c:\n  x = 1\n return x in []\n")
    fn_cond = comp_cond.lambdas[1]
    assert len(list(known_calls(fn_cond, "%compare_in"))) == 1
    check_ir1_consistency(comp_cond.components)

    # 3. Deleted local variable
    comp_del = compile_source("def f(x):\n del x\n return x not in ()\n")
    fn_del = comp_del.lambdas[1]
    assert len(list(known_calls(fn_del, "%compare_not_in"))) == 1
    check_ir1_consistency(comp_del.components)

    # 4. Parameter with explicit may_be_unbound type
    comp_unbound = compile_toplevel("def f(x):\n return x in []\n")
    fn_unbound = comp_unbound.lambdas[1]
    fn_unbound.vars[0].type = UNKNOWN_UNBOUND
    ir1_optimize_until_done(fn_unbound.component)
    assert len(list(known_calls(fn_unbound, "%compare_in"))) == 1
    check_ir1_consistency(comp_unbound.components)

    # 5. Global variable (reading may raise NameError)
    comp_global = compile_source("def f():\n return unknown_global in ()\n")
    fn_global = comp_global.lambdas[1]
    assert len(list(known_calls(fn_global, "%compare_in"))) == 1
    check_ir1_consistency(comp_global.components)


def test_single_element_tuple_and_list_containment_rewrites_to_equality():
    # 1. x in (42,) -> x == 42
    comp_tuple = compile_source("def f(x):\n return x in (42,)\n")
    fn_tuple = comp_tuple.lambdas[1]
    assert list(known_calls(fn_tuple, "%build_tuple")) == []
    assert list(known_calls(fn_tuple, "%compare_in")) == []
    eq_calls = list(known_calls(fn_tuple, "%compare_equal"))
    assert len(eq_calls) == 1
    lit = constant_literal(eq_calls[0].args[1])
    assert isinstance(lit, ast.IntegerLiteral) and lit.value == 42
    check_ir1_consistency(comp_tuple.components)

    # 2. x not in ['abc'] -> x != 'abc'
    comp_list = compile_source("def f(x):\n return x not in ['abc']\n")
    fn_list = comp_list.lambdas[1]
    assert list(known_calls(fn_list, "%build_list")) == []
    assert list(known_calls(fn_list, "%compare_not_in")) == []
    neq_calls = list(known_calls(fn_list, "%compare_not_equal"))
    assert len(neq_calls) == 1
    lit_str = constant_literal(neq_calls[0].args[1])
    assert isinstance(lit_str, ast.TextLiteral) and lit_str.value == "abc"
    check_ir1_consistency(comp_list.components)

    # 3. x in [y] -> x == y when y: int
    comp_var = compile_source("def f(x, y: int):\n return x in [y]\n")
    fn_var = comp_var.lambdas[1]
    assert list(known_calls(fn_var, "%build_list")) == []
    assert list(known_calls(fn_var, "%compare_in")) == []
    eq_var_calls = list(known_calls(fn_var, "%compare_equal"))
    assert len(eq_var_calls) == 1
    check_ir1_consistency(comp_var.components)

    # 4. x in [y] -> x == y when x: int (left operand has reflexive equality)
    comp_left = compile_source("def f(x: int, y):\n return x not in (y,)\n")
    fn_left = comp_left.lambdas[1]
    assert list(known_calls(fn_left, "%build_tuple")) == []
    assert list(known_calls(fn_left, "%compare_not_in")) == []
    assert len(list(known_calls(fn_left, "%compare_not_equal"))) == 1
    check_ir1_consistency(comp_left.components)

    # 5. Constant folding after rewriting: 42 in (42,) -> True
    comp_fold = compile_source("def f():\n return 42 in (42,)\n")
    fn_fold = comp_fold.lambdas[1]
    assert list(known_calls(fn_fold, "%compare_in")) == []
    assert list(known_calls(fn_fold, "%compare_equal")) == []
    ret = fn_fold.return_
    assert isinstance(ret, Return)
    use = ret.result.use()
    assert isinstance(use, Ref) and isinstance(use.leaf, Constant)
    assert (
        isinstance(use.leaf.value, ast.BooleanLiteral) and use.leaf.value.value is True
    )
    check_ir1_consistency(comp_fold.components)


def test_single_element_float_or_unknown_containment_not_rewritten():
    # Float could be NaN, where nan in (nan,) is True by identity if same object,
    # whereas nan == nan is False.
    for expr in ("x in (1.5,)", "x in [1.5]", "x not in (1.5,)", "x in [y]"):
        compilation = compile_source(f"def f(x, y):\n return {expr}\n")
        fn = compilation.lambdas[1]
        assert list(known_calls(fn, "%compare_equal")) == []
        assert list(known_calls(fn, "%compare_not_equal")) == []
        in_calls = list(known_calls(fn, "%compare_in")) + list(
            known_calls(fn, "%compare_not_in")
        )
        assert len(in_calls) == 1
        check_ir1_consistency(compilation.components)


def test_multi_element_constant_and_text_literal_containment_folding():
    cases = [
        ('"mem" in ("cpu", "mem", "disk")', True),
        ('"foo" in ("cpu", "mem", "disk")', False),
        ('"foo" not in ["cpu", "mem"]', True),
        ('"mem" not in ["cpu", "mem"]', False),
        ("1 in (True, 2)", True),
        ("0 in (False, 2)", True),
        ("True in (0, 1, 2)", True),
        ("False in [1, 2, 3]", False),
        ('1 in ("1", "2")', False),
        ("None in (1, None, 'a')", True),
        ("None not in [1, 2, 'a']", True),
        ('b"a" in (b"x", b"a")', True),
    ]
    for expr, expected in cases:
        compilation = compile_source(f"def f():\n return {expr}\n")
        fn = compilation.lambdas[1]
        assert list(known_calls(fn, "%build_tuple")) == []
        assert list(known_calls(fn, "%build_list")) == []
        assert list(known_calls(fn, "%compare_in")) == []
        assert list(known_calls(fn, "%compare_not_in")) == []
        ret = fn.return_
        assert isinstance(ret, Return)
        lit = constant_literal(ret.result)
        assert isinstance(lit, ast.BooleanLiteral) and lit.value is expected
        check_ir1_consistency(compilation.components)

    # TextType(literal) on left operand folds against constant tuple and list displays.
    for container, op, expected in (
        ('("cpu", "mem", "disk")', "in", True),
        ('("cpu", "disk")', "in", False),
        ('["cpu", "disk"]', "not in", True),
        ('["cpu", "mem"]', "not in", False),
    ):
        comp_text = compile_toplevel(f"def f(s):\n return s {op} {container}\n")
        fn_text = comp_text.lambdas[1]
        fn_text.vars[0].type = TextType("mem")
        assert ir1_optimize_until_done(fn_text.component)
        assert list(known_calls(fn_text, "%build_tuple")) == []
        assert list(known_calls(fn_text, "%build_list")) == []
        assert list(known_calls(fn_text, "%compare_in")) == []
        assert list(known_calls(fn_text, "%compare_not_in")) == []
        assert fn_text.vars[0].refs == []
        ret_text = fn_text.return_
        assert isinstance(ret_text, Return)
        lit_text = constant_literal(ret_text.result)
        assert isinstance(lit_text, ast.BooleanLiteral) and lit_text.value is expected
        check_ir1_consistency(comp_text.components)

    # Float elements are never folded because of NaN semantics.
    comp_float = compile_source("def f():\n return 1.5 in (1.5, 2.5)\n")
    fn_float = comp_float.lambdas[1]
    assert len(list(known_calls(fn_float, "%compare_in"))) == 1
    check_ir1_consistency(comp_float.components)


def test_interval_disjoint_integer_containment_folding():
    disjoint_cases = [
        ("i in (10, 20, 30)", False),
        ("i in [10, 20, 30]", False),
        ("i not in (-5, -1, 8, 100)", True),
        ("i not in [-5, -1, 8, 100]", True),
    ]
    for expr, expected in disjoint_cases:
        compilation = compile_source(f"def f(x: int):\n i = x & 7\n return {expr}\n")
        fn = compilation.lambdas[1]
        assert list(known_calls(fn, "%build_tuple")) == []
        assert list(known_calls(fn, "%build_list")) == []
        assert list(known_calls(fn, "%compare_in")) == []
        assert list(known_calls(fn, "%compare_not_in")) == []
        ret = fn.return_
        assert isinstance(ret, Return)
        lit = constant_literal(ret.result)
        assert isinstance(lit, ast.BooleanLiteral) and lit.value is expected
        check_ir1_consistency(compilation.components)

    # Overlapping interval is NOT folded.
    comp_overlap = compile_source(
        "def f(x: int):\n i = x & 7\n return i in (3, 10, 20)\n"
    )
    fn_overlap = comp_overlap.lambdas[1]
    assert len(list(known_calls(fn_overlap, "%compare_in"))) == 1
    check_ir1_consistency(comp_overlap.components)


def test_type_disjoint_primitive_containment_folding():
    # 1. TEXT_TYPE variable against integer/boolean/bytes/None container elements
    for expr, expected in (
        ("s in (1, 2, None)", False),
        ("s not in (1, 2, None)", True),
        ("s in [1, True, b'x', None]", False),
        ("s not in [1, True, b'x', None]", True),
    ):
        comp_top = compile_toplevel(f"def f(s):\n return {expr}\n")
        fn_top = comp_top.lambdas[1]
        fn_top.vars[0].type = TEXT_TYPE
        assert ir1_optimize_until_done(fn_top.component)
        assert list(known_calls(fn_top, "%build_tuple")) == []
        assert list(known_calls(fn_top, "%build_list")) == []
        assert list(known_calls(fn_top, "%compare_in")) == []
        assert list(known_calls(fn_top, "%compare_not_in")) == []
        assert fn_top.vars[0].refs == []
        ret_top = fn_top.return_
        assert isinstance(ret_top, Return)
        lit_top = constant_literal(ret_top.result)
        assert isinstance(lit_top, ast.BooleanLiteral) and lit_top.value is expected
        check_ir1_consistency(comp_top.components)

        comp_fstr = compile_source(f"def f(x: int):\n s = f'{{x}}'\n return {expr}\n")
        fn_fstr = comp_fstr.lambdas[1]
        assert list(known_calls(fn_fstr, "%build_tuple")) == []
        assert list(known_calls(fn_fstr, "%build_list")) == []
        assert list(known_calls(fn_fstr, "%compare_in")) == []
        assert list(known_calls(fn_fstr, "%compare_not_in")) == []
        ret_fstr = fn_fstr.return_
        assert isinstance(ret_fstr, Return)
        lit_fstr = constant_literal(ret_fstr.result)
        assert isinstance(lit_fstr, ast.BooleanLiteral) and lit_fstr.value is expected
        check_ir1_consistency(comp_fstr.components)

    # 2. PYTHON_INTEGER / int parameter against str/bytes/None container elements
    for expr, expected in (
        ('n in ("a", "b", None)', False),
        ('n not in ["a", b"b", None]', True),
    ):
        compilation = compile_source(f"def f(n: int):\n return {expr}\n")
        fn = compilation.lambdas[1]
        assert list(known_calls(fn, "%build_tuple")) == []
        assert list(known_calls(fn, "%build_list")) == []
        assert list(known_calls(fn, "%compare_in")) == []
        assert list(known_calls(fn, "%compare_not_in")) == []
        assert len(fn.vars[0].refs) == 1
        ret = fn.return_
        assert isinstance(ret, Return)
        lit = constant_literal(ret.result)
        assert isinstance(lit, ast.BooleanLiteral) and lit.value is expected
        check_ir1_consistency(compilation.components)
