"""Tests for sequence IR1 transforms in ``mule.compiler.seqtran``."""

from mule import ast
from mule.compiler import seqtran
from mule.compiler.ctype import (
    INTEGER,
    TEXT_TYPE,
    UNKNOWN_UNBOUND,
    IntegerType,
    TextType,
)
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1opt import constant_literal, ir1_optimize_until_done
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source, compile_toplevel
from mule.compiler.node import Constant, Ref, Return


def test_constant_tuple_subscript_folds_to_element():
    compilation = compile_toplevel("def f():\n return (10, 20, 30)[1]\n")
    fn = compilation.lambdas[1]
    assert list(known_calls(fn, "%build_tuple"))
    assert list(known_calls(fn, "%getitem"))
    assert fn.component is not None
    assert ir1_optimize_until_done(fn.component)
    assert list(known_calls(fn, "%build_tuple")) == []
    assert list(known_calls(fn, "%getitem")) == []
    ret = fn.return_
    assert isinstance(ret, Return)
    lit = constant_literal(ret.result)
    assert isinstance(lit, ast.IntegerLiteral) and lit.value == 20
    check_ir1_consistency(compilation.components)


def test_unfolded_build_tuple_constant_subscript_direct_transform():
    compilation = compile_toplevel("def f():\n return (10, 20, 30)[1]\n")
    fn = compilation.lambdas[1]
    getitem = next(known_calls(fn, "%getitem"))
    assert seqtran._transform_tuple_getitem(getitem)
    assert list(known_calls(fn, "%build_tuple")) == []
    assert list(known_calls(fn, "%getitem")) == []
    ret = fn.return_
    assert isinstance(ret, Return)
    lit = constant_literal(ret.result)
    assert isinstance(lit, ast.IntegerLiteral) and lit.value == 20
    check_ir1_consistency(compilation.components)


def test_tuple_subscript_boolean_and_negative_indices():
    compilation = compile_source(
        "def f(a: int, b: int):\n"
        " x = (a, b)[-1]\n"
        " y = (a, b)[False]\n"
        " z = (a, b)[True]\n"
        " return x, y, z\n"
    )
    fn = compilation.lambdas[1]
    assert list(known_calls(fn, "%getitem")) == []
    # Only the final return tuple remains.
    build_tuples = list(known_calls(fn, "%build_tuple"))
    assert len(build_tuples) == 1
    check_ir1_consistency(compilation.components)


def test_negative_index_on_lambda_vars_removes_unused_ref():
    compilation = compile_toplevel("def f(a, b):\n return (a, b)[-1]\n")
    fn = compilation.lambdas[1]
    var_a, var_b = fn.vars
    var_a.type = INTEGER
    var_b.type = INTEGER
    assert len(var_a.refs) == 1
    assert len(var_b.refs) == 1
    assert fn.component is not None
    assert ir1_optimize_until_done(fn.component)
    assert list(known_calls(fn, "%build_tuple")) == []
    assert list(known_calls(fn, "%getitem")) == []
    assert var_a.refs == []
    assert len(var_b.refs) == 1
    ret = fn.return_
    assert isinstance(ret, Return)
    use = ret.result.use()
    assert isinstance(use, Ref) and use.leaf is var_b
    check_ir1_consistency(compilation.components)


def test_tuple_subscript_out_of_bounds_kept_for_runtime_index_error():
    for expr in ("(10, 20)[5]", "(10, 20)[-3]", "(a, b)[2]", "(a, b)[-3]"):
        compilation = compile_toplevel(f"def f(a, b):\n return {expr}\n")
        fn = compilation.lambdas[1]
        for var in fn.vars:
            var.type = INTEGER
        assert fn.component is not None
        ir1_optimize_until_done(fn.component)
        assert len(list(known_calls(fn, "%getitem"))) == 1
        check_ir1_consistency(compilation.components)


def test_tuple_subscript_non_integer_index_kept():
    compilation = compile_toplevel("def f(a, b):\n return (a, b)['bad']\n")
    fn = compilation.lambdas[1]
    for var in fn.vars:
        var.type = INTEGER
    assert fn.component is not None
    ir1_optimize_until_done(fn.component)
    assert len(list(known_calls(fn, "%getitem"))) == 1
    assert len(list(known_calls(fn, "%build_tuple"))) == 1
    check_ir1_consistency(compilation.components)


def test_tuple_subscript_preserves_side_effecting_dropped_element():
    compilation = compile_toplevel("def f(g):\n return (g(), 20)[1]\n")
    fn = compilation.lambdas[1]
    assert fn.component is not None
    ir1_optimize_until_done(fn.component)
    assert len(list(known_calls(fn, "%build_tuple"))) == 1
    assert len(list(known_calls(fn, "%getitem"))) == 1
    check_ir1_consistency(compilation.components)


def test_tuple_subscript_preserves_potentially_unbound_dropped_elements():
    # 1. Conditionally bound local variable
    comp_cond = compile_source("def f(c):\n if c:\n  a = 1\n return (a, 20)[1]\n")
    fn_cond = comp_cond.lambdas[1]
    assert len(list(known_calls(fn_cond, "%build_tuple"))) == 1
    assert len(list(known_calls(fn_cond, "%getitem"))) == 1
    check_ir1_consistency(comp_cond.components)

    # 2. Local variable assigned only after the tuple subscript
    comp_late = compile_source("def f():\n r = (a, 20)[1]\n a = 1\n return r\n")
    fn_late = comp_late.lambdas[1]
    assert len(list(known_calls(fn_late, "%build_tuple"))) == 1
    assert len(list(known_calls(fn_late, "%getitem"))) == 1
    check_ir1_consistency(comp_late.components)

    # 3. Deleted parameter
    comp_del = compile_source("def f(a):\n del a\n return (a, 20)[1]\n")
    fn_del = comp_del.lambdas[1]
    assert len(list(known_calls(fn_del, "%build_tuple"))) == 1
    assert len(list(known_calls(fn_del, "%getitem"))) == 1
    check_ir1_consistency(comp_del.components)

    # 4. Parameter with explicit may_be_unbound type
    comp_unbound = compile_toplevel("def f(a):\n return (a, 20)[1]\n")
    fn_unbound = comp_unbound.lambdas[1]
    fn_unbound.vars[0].type = UNKNOWN_UNBOUND
    assert fn_unbound.component is not None
    ir1_optimize_until_done(fn_unbound.component)
    assert len(list(known_calls(fn_unbound, "%build_tuple"))) == 1
    assert len(list(known_calls(fn_unbound, "%getitem"))) == 1
    check_ir1_consistency(comp_unbound.components)

    # 5. Global variable (reading may raise NameError)
    comp_global = compile_source("def f():\n return (unknown_global, 20)[1]\n")
    fn_global = comp_global.lambdas[1]
    assert len(list(known_calls(fn_global, "%build_tuple"))) == 1
    assert len(list(known_calls(fn_global, "%getitem"))) == 1
    check_ir1_consistency(comp_global.components)


def test_tuple_subscript_allows_definitely_bound_local_in_same_block():
    compilation = compile_source("def f():\n a = 10\n return (a, 20)[1]\n")
    fn = compilation.lambdas[1]
    assert list(known_calls(fn, "%build_tuple")) == []
    assert list(known_calls(fn, "%getitem")) == []
    ret = fn.return_
    assert isinstance(ret, Return)
    lit = constant_literal(ret.result)
    assert isinstance(lit, ast.IntegerLiteral) and lit.value == 20
    check_ir1_consistency(compilation.components)


def test_empty_list_and_tuple_unary_not_fold_to_true():
    for expr in ("not []", "not ()"):
        compilation = compile_toplevel(f"def f():\n return {expr}\n")
        fn = compilation.lambdas[1]
        assert fn.component is not None
        assert ir1_optimize_until_done(fn.component)
        assert list(known_calls(fn, "%build_list")) == []
        assert list(known_calls(fn, "%build_tuple")) == []
        assert list(known_calls(fn, "%unary_not")) == []
        ret = fn.return_
        assert isinstance(ret, Return)
        use = ret.result.use()
        assert isinstance(use, Ref) and isinstance(use.leaf, Constant)
        assert isinstance(use.leaf.value, ast.BooleanLiteral)
        assert use.leaf.value.value is True
        check_ir1_consistency(compilation.components)


def test_unfolded_empty_tuple_unary_not_direct_transform():
    compilation = compile_toplevel("def f():\n return not ()\n")
    fn = compilation.lambdas[1]
    not_call = next(known_calls(fn, "%unary_not"))
    assert seqtran._transform_empty_container_not(not_call)
    assert list(known_calls(fn, "%build_tuple")) == []
    assert list(known_calls(fn, "%unary_not")) == []
    ret = fn.return_
    assert isinstance(ret, Return)
    lit = constant_literal(ret.result)
    assert isinstance(lit, ast.BooleanLiteral) and lit.value is True
    check_ir1_consistency(compilation.components)


def test_non_empty_list_unary_not_preserved():
    compilation = compile_toplevel("def f(x):\n return not [x]\n")
    fn = compilation.lambdas[1]
    assert fn.component is not None
    ir1_optimize_until_done(fn.component)
    assert len(list(known_calls(fn, "%build_list"))) == 1
    assert len(list(known_calls(fn, "%unary_not"))) == 1
    check_ir1_consistency(compilation.components)


def test_tuple_and_list_unpacking_elision():
    for rhs in ("x, y", "[x, y]"):
        compilation = compile_source(f"def f(x, y):\n a, b = {rhs}\n return a, b\n")
        fn = compilation.lambdas[1]
        assert list(known_calls(fn, "%unpack")) == []
        assert list(known_calls(fn, "%unpack_ref")) == []
        assert list(known_calls(fn, "%build_list")) == []
        # Only the final return tuple remains.
        assert len(list(known_calls(fn, "%build_tuple"))) == 1
        assert [v for v in fn.locals if v.expression_temporary] == []
        check_ir1_consistency(compilation.components)


def test_unpacking_swap_elision():
    compilation = compile_source("def f(a, b):\n a, b = b, a\n return a, b\n")
    fn = compilation.lambdas[1]
    assert list(known_calls(fn, "%unpack")) == []
    assert list(known_calls(fn, "%unpack_ref")) == []
    assert len(list(known_calls(fn, "%build_tuple"))) == 1
    check_ir1_consistency(compilation.components)


def test_unpacking_list_and_constant_tuple_and_empty_elision():
    comp_list = compile_source("def f():\n a, b, c = [1, 2, 3]\n return a, b, c\n")
    fn_list = comp_list.lambdas[1]
    assert list(known_calls(fn_list, "%unpack")) == []
    assert list(known_calls(fn_list, "%unpack_ref")) == []
    assert list(known_calls(fn_list, "%build_list")) == []
    check_ir1_consistency(comp_list.components)

    comp_empty = compile_source("def f():\n () = ()\n [] = []\n return 42\n")
    fn_empty = comp_empty.lambdas[1]
    assert list(known_calls(fn_empty, "%unpack")) == []
    assert list(known_calls(fn_empty, "%unpack_ref")) == []
    assert list(known_calls(fn_empty, "%build_tuple")) == []
    assert list(known_calls(fn_empty, "%build_list")) == []
    check_ir1_consistency(comp_empty.components)


def test_unpacking_constant_tuple_flow_analysis_types():
    compilation = compile_source(
        "def f():\n a, b, c = 1, 'hello', 'x'\n return a, b, c\n"
    )
    fn = compilation.lambdas[1]
    assert list(known_calls(fn, "%unpack")) == []
    assert list(known_calls(fn, "%unpack_ref")) == []
    assert compilation.facts is not None
    types = compilation.facts[fn].types
    locals_by_name = {var.name: var for var in fn.locals}
    ref_a = locals_by_name["a"].refs[0]
    ref_b = locals_by_name["b"].refs[0]
    ref_c = locals_by_name["c"].refs[0]
    assert types[ref_a] == IntegerType(1, 1)
    assert types[ref_b] == TEXT_TYPE
    assert types[ref_c] == TextType("x")
    check_ir1_consistency(compilation.components)


def test_unpacking_length_mismatch_preserved():
    for src in (
        "def f():\n a, b = (1, 2, 3)\n return a, b\n",
        "def f(x, y, z):\n a, b = (x, y, z)\n return a, b\n",
        "def f(x):\n a, b = [x]\n return a, b\n",
    ):
        compilation = compile_source(src)
        fn = compilation.lambdas[1]
        assert len(list(known_calls(fn, "%unpack"))) == 1
        assert len(list(known_calls(fn, "%unpack_ref"))) == 2
        check_ir1_consistency(compilation.components)


def test_unpacking_non_lambda_var_or_nested_targets_preserved():
    # Subscript target may raise and requires temporary slot cleanup on unwind.
    comp_sub = compile_source("def f(target):\n target[0], b = (1, 2)\n return b\n")
    fn_sub = comp_sub.lambdas[1]
    assert len(list(known_calls(fn_sub, "%unpack"))) == 1
    assert len(list(known_calls(fn_sub, "%unpack_ref"))) == 2
    check_ir1_consistency(comp_sub.components)

    # Outer unpack has nested target (b, c), so outer unpack is preserved,
    # while inner unpack (b, c) is not fed directly by %build_tuple.
    comp_nested = compile_source(
        "def f():\n a, (b, c) = (1, (2, 3))\n return a, b, c\n"
    )
    fn_nested = comp_nested.lambdas[1]
    assert len(list(known_calls(fn_nested, "%unpack"))) == 2
    check_ir1_consistency(comp_nested.components)


def test_len_on_known_text_rewrites_to_text_length():
    compilation = compile_source("def f():\n s = 'hello'\n return len(s)\n")
    fn = compilation.lambdas[1]
    assert len(list(known_calls(fn, "_text_length"))) == 1
    check_ir1_consistency(compilation.components)


def test_len_on_known_list_tuple_bytes_rewrites_to_storage_length():
    for src in (
        "def f():\n xs = [1, 2, 3]\n return len(xs)\n",
        "def f():\n t = (1, 2)\n return len(t)\n",
        "def f():\n b = b'abc'\n return len(b)\n",
    ):
        compilation = compile_source(src)
        fn = compilation.lambdas[1]
        assert len(list(known_calls(fn, "_storage_length"))) == 1
        check_ir1_consistency(compilation.components)


def test_len_negative_cases_preserve_full_call():
    for src in (
        "__safety__ = 3\ndef f():\n s = 'abc'\n return len(s)\n",
        "len = lambda x: 0\ndef f():\n s = 'abc'\n return len(s)\n",
        "def f():\n return len()\n",
        "def f():\n return len('a', 'b')\n",
        "def f():\n return len(obj='abc')\n",
        "def f(x):\n return len(x)\n",
    ):
        compilation = compile_source(src)
        fn = compilation.lambdas[-1]
        assert list(known_calls(fn, "_text_length")) == []
        assert list(known_calls(fn, "_storage_length")) == []
        check_ir1_consistency(compilation.components)


def test_len_ir2_specialization_and_fast_len_emission():
    from mule.compiler.driver import emit_llvm

    ir = emit_llvm(
        "def scan(text: str):\n"
        " if text.startswith('a'):\n"
        "  return len(text)\n"
        " return 0\n"
        "print(scan('abc'))\n"
    )
    spec_start = ir.index('define internal ptr @"mule_fn_scan.spec.0"')
    spec_end = ir.index("\n}", spec_start)
    spec_body = ir[spec_start:spec_end]
    assert "@mule_text_length(" in spec_body
    assert "@mule_fast_len(" not in spec_body

    body_start = ir.index('define internal ptr @"mule_fn_scan.body"')
    body_end = ir.index("\n}", body_start)
    general_body = ir[body_start:body_end]
    assert "@mule_fast_len(" in general_body


def test_len_native_execution(tmp_path):
    from test.mule.compiler.test_native import run_native

    source = (
        "class Sized:\n"
        " def __len__(self):\n"
        "  return 7\n"
        "def measure_unknown(x):\n"
        " return len(x)\n"
        "def scan(text: str):\n"
        " if text.startswith('h'):\n"
        "  return len(text)\n"
        " return 0\n"
        "s = 'hello'\n"
        "xs = [10, 20, 30, 40]\n"
        "t = (1, 2)\n"
        "b = b'xyz'\n"
        "print(len(s), len(xs), len(t), len(b))\n"
        "print(scan('hello'), measure_unknown({'a': 1, 'b': 2}), measure_unknown(Sized()))\n"
        "try:\n"
        " measure_unknown(42)\n"
        "except TypeError:\n"
        " print('type-error')\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "5 4 2 3\n5 2 7\ntype-error\n"


def test_range_prepare_fixnum_literal_and_bounded_eliminate_generic_loop():
    from mule.compiler.driver import emit_llvm

    for src in (
        "def f():\n total = 0\n for i in range(10):\n  total += i\n return total\n",
        "def f():\n total = 0\n for i in range(2, 10, 3):\n  total += i\n return total\n",
        "def f(x: int):\n n = x & 1023\n total = 0\n for i in range(n):\n  total += i\n return total\n",
        "def f(x: int, y: int):\n a = x & 255\n b = y & 255\n total = 0\n for i in range(a, b, -1):\n  total += i\n return total\n",
    ):
        compilation = compile_source(src)
        fn = compilation.lambdas[1]
        prepares = list(known_calls(fn, "%range_prepare"))
        assert len(prepares) == 1
        assert prepares[0].lvar is None
        assert len(list(known_calls(fn, "%range_available"))) == 1
        assert len(list(known_calls(fn, "%range_advance"))) == 1
        assert list(known_calls(fn, "%sequence_available")) == []
        assert list(known_calls(fn, "%next_item")) == []
        assert list(known_calls(fn, "%catch_exception")) == []
        check_ir1_consistency(compilation.components)

        ir = emit_llvm(src + "print(0)\n")
        body_start = ir.index('define internal ptr @"mule_fn_f.')
        body_end = ir.index("\n}", body_start)
        body = ir[body_start:body_end]
        assert "@mule_range_storage(" in body
        assert "@mule_catch_exception(" not in body


def test_range_prepare_unbounded_or_shadowed_keeps_generic_fallback():
    for src in (
        "def f(n: int):\n total = 0\n for i in range(n):\n  total += i\n return total\n",
        "def f(n):\n total = 0\n for i in range(n):\n  total += i\n return total\n",
        "__safety__ = 3\ndef f():\n total = 0\n for i in range(10):\n  total += i\n return total\n",
        "range = None\ndef f():\n total = 0\n for i in range(10):\n  total += i\n return total\n",
    ):
        compilation = compile_source(src)
        fn = compilation.lambdas[1]
        prepares = list(known_calls(fn, "%range_prepare"))
        assert len(prepares) == 1
        assert prepares[0].lvar is not None
        assert len(list(known_calls(fn, "%sequence_available"))) == 1
        assert len(list(known_calls(fn, "%next_item"))) == 1
        check_ir1_consistency(compilation.components)
