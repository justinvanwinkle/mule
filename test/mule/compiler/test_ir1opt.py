"""Tests for IR1 type derivation, constant lvar queries, optimizers, and transforms."""

from contextlib import contextmanager

from mule import ast
from mule.compiler.ctype import (
    BOOLEAN,
    BOOLEAN_FALSE,
    BOOLEAN_TRUE,
    BOTTOM,
    FLOAT,
    FUNCTION_TYPE,
    INTEGER,
    NONE_TYPE,
    IntegerType,
    TextType,
)
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1opt import (
    constant_literal,
    constant_lvar_value,
    ir1_optimize,
    ir1_optimize_until_done,
    lvar_type,
    node_derived_type,
    replace_combination_with_constant,
    replace_combination_with_lvar,
    retarget_known_combination,
)
from mule.compiler.ir1util import (
    delete_ref,
    insert_node_before,
    known_calls,
    unlink_node,
    use_lvar,
)
from mule.compiler.knownfun import (
    Transform,
    defderive,
    defoptimizer,
    deftransform,
    fun_info,
)
from mule.compiler.main import compile_source, compile_toplevel
from mule.compiler.node import (
    Cast,
    Combination,
    Constant,
    Lambda,
    LambdaVar,
    Lvar,
    Ref,
    Return,
    Set,
)


@contextmanager
def saved_fun_info(*names: str):
    """Restore ``FunInfo`` hooks after a test registers temporary rules."""
    infos = [fun_info(name) for name in names]
    snapshots = [
        (info, info.derive_type, info.optimizer, info.transforms) for info in infos
    ]
    for info in infos:
        object.__setattr__(info, "transforms", ())
    try:
        yield
    finally:
        for info, derive_type, optimizer, transforms in snapshots:
            object.__setattr__(info, "derive_type", derive_type)
            object.__setattr__(info, "optimizer", optimizer)
            object.__setattr__(info, "transforms", transforms)


def test_node_derived_type_and_lvar_type_for_constants_and_lambdas():
    compilation = compile_toplevel(
        "def f():\n return 42, 'a', True, False, None, 1.5\n"
    )
    fn = compilation.lambdas[1]
    tuple_call = next(known_calls(fn, "%build_tuple"))
    types = [lvar_type(arg) for arg in tuple_call.args]
    assert types == [
        IntegerType(42, 42),
        TextType("a"),
        BOOLEAN_TRUE,
        BOOLEAN_FALSE,
        NONE_TYPE,
        FLOAT,
    ]
    for arg, expected in zip(tuple_call.args, types, strict=True):
        assert arg.derived_type == expected
        assert node_derived_type(arg.use()) == expected

    make_fn = next(known_calls(compilation.entry, "%make_function"))
    lambda_lvar = make_fn.args[0]
    lambda_use = lambda_lvar.use()
    assert isinstance(lambda_use, Ref) and isinstance(lambda_use.leaf, Lambda)
    assert node_derived_type(lambda_lvar.use()) == FUNCTION_TYPE
    assert lvar_type(lambda_lvar) == FUNCTION_TYPE

    empty_lvar = Lvar()
    assert lvar_type(empty_lvar) is BOTTOM


def test_node_derived_type_for_cast_and_lambda_var():
    compilation = compile_toplevel(
        "import mule_types\ndef f(x: mule_types.int64) -> mule_types.int64:\n return x\n"
    )
    fn = compilation.lambdas[1]
    assert fn.component is not None
    casts = [
        node
        for block in fn.component.blocks()
        for node in block.nodes()
        if isinstance(node, Cast)
    ]
    assert casts
    for cast in casts:
        derived = node_derived_type(cast)
        assert derived.fits(INTEGER)
        assert lvar_type(cast.lvar) == derived

    var = fn.vars[0]
    assert isinstance(var, LambdaVar)
    ref = var.refs[0]
    ref.derived_type = None
    var.type = IntegerType(10, 20)
    assert node_derived_type(ref) == IntegerType(10, 20)


def test_lvar_type_joins_if_branch_uses():
    compilation = compile_toplevel("def f(c):\n return 10 if c else 20\n")
    fn = compilation.lambdas[1]
    assert fn.component is not None
    branch_sets = [
        node
        for block in fn.component.blocks()
        for node in block.nodes()
        if isinstance(node, Set)
        and isinstance(node.var, LambdaVar)
        and len(node.var.sets) == 2
    ]
    if branch_sets:
        joined = lvar_type(branch_sets[0].value).join(lvar_type(branch_sets[1].value))
        assert joined == IntegerType(10, 20)
    refs = [
        node
        for block in fn.component.blocks()
        for node in block.nodes()
        if isinstance(node, Ref) and isinstance(node.leaf, Constant)
    ]
    int_refs = [r for r in refs if isinstance(r.leaf.value, ast.IntegerLiteral)]
    assert len(int_refs) == 2
    multi = Lvar()
    multi.uses = list(int_refs)
    assert lvar_type(multi) == IntegerType(10, 20)


def test_defderive_computes_combination_derived_type_and_reoptimizes_lvar():
    with saved_fun_info("%floor_divide"):

        @defderive("%floor_divide")
        def _derive_floor_div(node: Combination):
            left = lvar_type(node.args[0])
            right = lvar_type(node.args[1])
            if left.fits(INTEGER) and right.fits(INTEGER):
                return IntegerType(7, 7)
            return None

        compilation = compile_toplevel("def f():\n return 15 // 2\n")
        fn = compilation.lambdas[1]
        call = next(known_calls(fn, "%floor_divide"))
        assert fn.component is not None
        ir1_optimize(fn.component)
        assert call.derived_type == IntegerType(7, 7)
        assert call.lvar is not None
        assert lvar_type(call.lvar) == IntegerType(7, 7)
        check_ir1_consistency(compilation.components)


def test_constant_lvar_value_for_literals_and_singleton_types():
    with saved_fun_info("%floor_divide", "%modulo", "%power", "%left_shift"):
        defderive("%floor_divide", lambda node: IntegerType(99, 99))
        defderive("%modulo", lambda node: BOOLEAN_TRUE)
        defderive("%power", lambda node: BOOLEAN_FALSE)
        defderive("%left_shift", lambda node: NONE_TYPE)

        compilation = compile_toplevel(
            "def f(x):\n"
            " a = 42\n"
            " b = x // 2\n"
            " c = x % 2\n"
            " d = x ** 2\n"
            " e = x << 2\n"
            " return a, b, c, d, e\n"
        )
        fn = compilation.lambdas[1]
        div_call = next(known_calls(fn, "%floor_divide"))
        mod_call = next(known_calls(fn, "%modulo"))
        pow_call = next(known_calls(fn, "%power"))
        lshift_call = next(known_calls(fn, "%left_shift"))

        lit = constant_literal(div_call.args[1])
        assert isinstance(lit, ast.IntegerLiteral) and lit.value == 2
        val = constant_lvar_value(div_call.args[1])
        assert isinstance(val, ast.IntegerLiteral) and val.value == 2

        assert div_call.lvar is not None
        assert mod_call.lvar is not None
        assert pow_call.lvar is not None
        assert lshift_call.lvar is not None
        assert constant_literal(div_call.lvar) is None
        assert constant_lvar_value(div_call.lvar) == ast.IntegerLiteral(99)
        assert constant_lvar_value(mod_call.lvar) == ast.BooleanLiteral(True)
        assert constant_lvar_value(pow_call.lvar) == ast.BooleanLiteral(False)
        assert isinstance(constant_lvar_value(lshift_call.lvar), ast.NoneLiteral)

        assert constant_lvar_value(div_call.args[0]) is None
        div_call.derived_type = IntegerType(1, 2)
        div_call.lvar.derived_type = None
        assert constant_lvar_value(div_call.lvar) is None

        mod_call.derived_type = BOOLEAN
        mod_call.lvar.derived_type = None
        assert constant_lvar_value(mod_call.lvar) is None

        div_call.derived_type = TextType("x")
        div_call.lvar.derived_type = None
        assert constant_lvar_value(div_call.lvar) == ast.TextLiteral("x")


def test_deftransform_matches_arg_types_and_policy_and_replaces_with_lvar():
    with saved_fun_info("%add"):

        @deftransform(
            "%add",
            (INTEGER, INTEGER),
            policy=lambda node: node.lexenv.policy.speed >= 1,
        )
        def _add_zero(node: Combination) -> bool:
            right = constant_lvar_value(node.args[1])
            if isinstance(right, ast.IntegerLiteral) and right.value == 0:
                replace_combination_with_lvar(node, node.args[0])
                return True
            return False

        assert isinstance(fun_info("%add").transforms[-1], Transform)

        compilation = compile_toplevel("def f(x):\n return x + 0\n")
        fn = compilation.lambdas[1]
        fn.vars[0].type = INTEGER
        assert list(known_calls(fn, "%add"))
        assert fn.component is not None
        changed = ir1_optimize_until_done(fn.component)
        assert changed
        assert list(known_calls(fn, "%add")) == []
        check_ir1_consistency(compilation.components)

        # Unannotated unknown operand does not match (INTEGER, INTEGER)
        compilation_unknown = compile_toplevel("def f(x):\n return x + 0\n")
        fn_unknown = compilation_unknown.lambdas[1]
        assert fn_unknown.component is not None
        ir1_optimize_until_done(fn_unknown.component)
        assert len(list(known_calls(fn_unknown, "%add"))) == 1
        check_ir1_consistency(compilation_unknown.components)

        # Policy gate blocks transform when speed == 0
        compilation_slow = compile_toplevel("def f(x):\n return x + 0\n")
        fn_slow = compilation_slow.lambdas[1]
        fn_slow.vars[0].type = INTEGER
        add_call = next(known_calls(fn_slow, "%add"))
        add_call.lexenv = add_call.lexenv.derive(
            policy=add_call.lexenv.policy.__class__(speed=0)
        )
        assert fn_slow.component is not None
        ir1_optimize_until_done(fn_slow.component)
        assert len(list(known_calls(fn_slow, "%add"))) == 1
        check_ir1_consistency(compilation_slow.components)


def test_replace_combination_with_lvar_positions_and_multi_use():
    with saved_fun_info("%floor_divide"):

        @deftransform("%floor_divide")
        def _div_one(node: Combination) -> bool:
            right = constant_lvar_value(node.args[1])
            if isinstance(right, ast.IntegerLiteral) and right.value == 1:
                replace_combination_with_lvar(node, node.args[0])
                return True
            return False

        compilation = compile_toplevel(
            "def f(x, y):\n a = x // 1\n return (a // 1) + (y // 1)\n"
        )
        fn = compilation.lambdas[1]
        assert len(list(known_calls(fn, "%floor_divide"))) == 3
        assert fn.component is not None
        assert ir1_optimize_until_done(fn.component)
        assert list(known_calls(fn, "%floor_divide")) == []
        check_ir1_consistency(compilation.components)

        compilation2 = compile_toplevel(
            "def f(c, x, y):\n return (x if c else y) // 1\n"
        )
        fn2 = compilation2.lambdas[1]
        assert list(known_calls(fn2, "%floor_divide"))
        assert fn2.component is not None
        assert ir1_optimize_until_done(fn2.component)
        assert list(known_calls(fn2, "%floor_divide")) == []
        check_ir1_consistency(compilation2.components)


def test_replace_combination_with_lvar_with_multiple_producers_on_value():
    """Verify replace_combination_with_lvar when value.uses has >1 producer."""
    compilation = compile_toplevel(
        "def f(c, x, y):\n if c:\n  z = x\n else:\n  z = y\n return z // 1\n"
    )
    fn = compilation.lambdas[1]
    call = next(known_calls(fn, "%floor_divide"))
    assert fn.component is not None
    sets = [
        node
        for block in fn.component.blocks()
        for node in block.nodes()
        if isinstance(node, Set) and node.var.name == "z"
    ]
    assert len(sets) == 2
    value_lvar = call.args[0]
    old_ref = value_lvar.use()
    assert isinstance(old_ref, Ref)
    old_ref.lvar = None
    value_lvar.uses.clear()
    for s in sets:
        producer = s.value.use()
        s.value.uses.remove(producer)
        producer.lvar = value_lvar
        value_lvar.uses.append(producer)
        const = Constant(ast.Tuple([]))
        dummy_ref = Ref(s.lexenv, s.source, const)
        const.refs.append(dummy_ref)
        insert_node_before(s, dummy_ref)
        use_lvar(dummy_ref, s.value)
    delete_ref(old_ref)
    unlink_node(old_ref)
    check_ir1_consistency(compilation.components)

    assert len(value_lvar.uses) == 2
    replace_combination_with_lvar(call, value_lvar)
    check_ir1_consistency(compilation.components)
    ret = fn.return_
    assert isinstance(ret, Return)
    assert len(ret.result.uses) == 2


def test_deftransform_retarget_known_combination_and_constant_replacement():
    with saved_fun_info("%modulo", "%left_shift", "%right_shift", "%bit_or"):

        @deftransform("%modulo", (INTEGER, INTEGER))
        def _mod_one(node: Combination) -> bool:
            right = constant_lvar_value(node.args[1])
            if isinstance(right, ast.IntegerLiteral) and right.value == 1:
                replace_combination_with_constant(node, ast.IntegerLiteral(0))
                return True
            return False

        @deftransform("%left_shift", (INTEGER, INTEGER))
        def _lshift_zero_to_bit_or(node: Combination) -> bool:
            right = constant_lvar_value(node.args[1])
            if isinstance(right, ast.IntegerLiteral) and right.value == 0:
                retarget_known_combination(
                    node, fun_info("%bit_or"), [node.args[0], node.args[1]]
                )
                return True
            return False

        defoptimizer(
            "%right_shift",
            lambda node: (
                replace_combination_with_lvar(node, node.args[0]) or True
                if isinstance(
                    (right := constant_lvar_value(node.args[1])), ast.IntegerLiteral
                )
                and right.value == 0
                else False
            ),
        )

        compilation = compile_toplevel(
            "def f(x):\n a = x % 1\n b = x << 0\n c = x >> 0\n return a + b + c\n"
        )
        fn = compilation.lambdas[1]
        fn.vars[0].type = INTEGER
        assert fn.component is not None
        assert ir1_optimize_until_done(fn.component)
        assert list(known_calls(fn, "%modulo")) == []
        assert list(known_calls(fn, "%left_shift")) == []
        assert list(known_calls(fn, "%right_shift")) == []
        assert len(list(known_calls(fn, "%bit_or"))) == 1
        check_ir1_consistency(compilation.components)


def test_getitem_slice_optimizer_registered_via_defoptimizer():
    assert fun_info("%getitem").optimizer is not None
    compilation = compile_source("def f(s):\n return s[1:3]\n")
    fn = compilation.lambdas[1]
    assert list(known_calls(fn, "%getitem")) == []
    assert list(known_calls(fn, "%make_slice")) == []
    assert len(list(known_calls(fn, "%getslice"))) == 1
    check_ir1_consistency(compilation.components)


def test_constant_lvar_value_unifies_multi_set_and_flushes_bound_refs():
    compilation = compile_source(
        "def f(cond, x):\n"
        " y = x\n"
        " if cond:\n"
        "  state = 'ready'\n"
        " else:\n"
        "  state = 'ready'\n"
        " return (y, state)[1] == 'ready'\n"
    )
    fn = compilation.lambdas[1]
    assert list(known_calls(fn, "%build_tuple")) == []
    assert list(known_calls(fn, "%getitem")) == []
    assert list(known_calls(fn, "%compare_equal")) == []
    assert fn.component is not None
    sets = [
        node
        for block in fn.component.blocks()
        for node in block.nodes()
        if isinstance(node, Set)
    ]
    assert {s.var.name for s in sets} == {"y", "state"}
    for s in sets:
        assert s.var.refs == []
    ret = fn.return_
    assert isinstance(ret, Return)
    use = ret.result.use()
    assert isinstance(use, Ref)
    assert isinstance(use.leaf, Constant)
    assert isinstance(use.leaf.value, ast.BooleanLiteral)
    assert use.leaf.value.value is True
    check_ir1_consistency(compilation.components)


def test_narrowed_if_branch_folds_in_second_ir1_pass_and_preserves_derived_types():
    from mule.compiler.ir1opt import derive_node_type
    from mule.compiler.node import If

    compilation = compile_source(
        "def f(x) -> int:\n"
        " flag = bool(x)\n"
        " if flag:\n"
        "  if flag:\n"
        "   return 42\n"
        "  return 0\n"
        " return -1\n"
    )
    fn = compilation.lambdas[1]
    assert fn.component is not None
    ifs = [
        node
        for block in fn.component.blocks()
        for node in block.nodes()
        if isinstance(node, If)
    ]
    assert len(ifs) == 1
    ret = fn.return_
    assert isinstance(ret, Return)
    derive_node_type(ret.result.uses[0], IntegerType(42, 42))
    assert ret.result.uses[0].derived_type == IntegerType(42, 42)
    derive_node_type(ret.result.uses[0], INTEGER)
    assert ret.result.uses[0].derived_type == IntegerType(42, 42)
    check_ir1_consistency(compilation.components)
