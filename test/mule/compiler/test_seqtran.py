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


def test_unpacking_non_lambda_var_or_nested_targets_elided():
    # The element Lvars retain cleanup ownership if a target raises.
    comp_sub = compile_source("def f(target):\n target[0], b = (1, 2)\n return b\n")
    fn_sub = comp_sub.lambdas[1]
    assert not list(known_calls(fn_sub, "%unpack"))
    assert not list(known_calls(fn_sub, "%unpack_ref"))
    check_ir1_consistency(comp_sub.components)

    # Forwarding the inner tuple value exposes its own display destructuring.
    comp_nested = compile_source(
        "def f():\n a, (b, c) = (1, (2, 3))\n return a, b, c\n"
    )
    fn_nested = comp_nested.lambdas[1]
    assert not list(known_calls(fn_nested, "%unpack"))
    assert not list(known_calls(fn_nested, "%unpack_ref"))
    check_ir1_consistency(comp_nested.components)


def test_len_on_known_text_rewrites_to_text_length():
    compilation = compile_source(
        "def f(c):\n s = 'hello' if c else 'world!'\n return len(s)\n"
    )
    fn = compilation.lambdas[1]
    assert len(list(known_calls(fn, "_text_length"))) == 1
    check_ir1_consistency(compilation.components)


def test_len_on_known_list_tuple_bytes_rewrites_to_storage_length():
    for src in (
        "def f():\n xs = [1, 2, 3]\n return len(xs)\n",
        "def f(c):\n t = (1, 2) if c else (1, 2, 3)\n return len(t)\n",
        "def f(c):\n b = b'abc' if c else b'abcd'\n return len(b)\n",
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
        body_start = ir.index('define internal ptr @"mule_fn_f.body"(')
        body_end = ir.index("\n}", body_start)
        body = ir[body_start:body_end]
        assert "@mule_range_storage(" not in body
        assert "@mule.fast.iterate" not in body
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


def test_seqtran_folds_bound_local_constants_in_getitem_getslice_not_and_getattr():
    compilation = compile_source(
        "def f(obj):\n"
        " t = (10, 20, 30)\n"
        " i = 1\n"
        " s = 'abcdef'\n"
        " lo = 1\n"
        " hi = 4\n"
        " empty = ()\n"
        " attr = 'value'\n"
        " return (t[i], s[lo:hi], not empty, getattr(obj, attr))\n"
    )
    fn = compilation.lambdas[1]
    assert list(known_calls(fn, "%getitem")) == []
    assert list(known_calls(fn, "%getslice")) == []
    assert list(known_calls(fn, "%unary_not")) == []
    assert len(list(known_calls(fn, "%getattr"))) == 1
    check_ir1_consistency(compilation.components)


def test_range_prepare_dynamic_int_fuses_fast_fixnum_branch():
    from mule.compiler.driver import emit_llvm

    src = "def f(n: int):\n total = 0\n for i in range(n):\n  total += i\n return total\nprint(f(5))\n"
    ir = emit_llvm(src)
    body_start = ir.index('define internal ptr @"mule_fn_f.body"(')
    body_end = ir.index("\n}", body_start)
    body = ir[body_start:body_end]
    assert "\nrange.fast." in body
    assert "\nrange.slow." in body
    fast_start = body.index("\nrange.fast.")
    slow_start = body.index("\nrange.slow.", fast_start)
    fast_block = body[fast_start:slow_start]
    assert "call " not in fast_block
    assert "store i64 " in fast_block


def test_bytearray_varint_chain_specializes_getitem_setitem_append_and_modulo():
    from mule.compiler.driver import emit_llvm

    src = """def decode_varint(buf, idx):
    shift = 0
    result = 0
    while True:
        b = buf[idx]
        idx += 1
        result |= (b & 0x7F) << shift
        if not (b & 0x80):
            return result, idx

def decode_packets(packets):
    checksum = 0
    for packet in packets:
        n, idx = decode_varint(packet, 0)
        decoded = bytearray()
        for j in range(n):
            decoded.append(packet[idx + j] ^ (j & 0xFF))
        if n > 1:
            decoded[0], decoded[n - 1] = decoded[n - 1], decoded[0]
        a = 1
        for b in decoded:
            a = (a + b) % 65521
        checksum ^= a
    return checksum

print(decode_packets([b"\\x03abc"]))
"""
    ir = emit_llvm(src)
    spec_start = ir.index('define internal ptr @"mule_fn_decode_packets.spec.0"(')
    spec_end = ir.index("\n}", spec_start)
    spec_body = ir[spec_start:spec_end]
    assert "\nrange.fast." in spec_body
    assert "bytearray_append" in spec_body
    assert "bytearray_getitem" in spec_body
    assert "bytearray_setitem" in spec_body
    assert "urem i64 " in spec_body
    assert ", 65521" in spec_body


def test_bytearray_argument_guard_and_annotation_specialization():
    from mule.compiler.driver import emit_llvm

    src = """def mutate(buf: bytearray) -> bytearray:
    buf.append(buf[0] ^ 1)
    return buf

def call_known():
    return mutate(bytearray(b"a"))

def call_unknown(x):
    return mutate(x)

alias = mutate
print(call_known())
"""
    ir = emit_llvm(src)
    assert '@"mule.class.layout.mule_sequences_bytearray" = weak_odr global i64 0' in ir
    assert 'load i64, ptr @"mule.class.layout.mule_sequences_bytearray"' in ir
    spec_start = ir.index('define internal ptr @"mule_fn_mutate.spec.0"(')
    spec_end = ir.index("\n}", spec_start)
    spec_body = ir[spec_start:spec_end]
    assert "bytearray_getitem" in spec_body
    assert "bytearray_append" in spec_body


def test_bytearray_protocol_fast_paths_execution(tmp_path):
    import subprocess

    from mule.compiler.driver import build_executable

    src = """def push_first(buf):
    buf.append(buf[0] + 1)
    return len(buf)

def call_unknown(fn, obj):
    return fn(obj)

def annotated_step(buf: bytearray) -> bytearray:
    buf[0] = (buf[0] + 2) & 0xFF
    return buf

ba = bytearray(b"abc")
ba.extend(b"de")
ba.extend(bytearray(b"fg"))
ba += b"hi"
assert ba == b"abcdefghi"
assert ba == bytearray(b"abcdefghi")
assert ba != b"abcdefgh"
assert list(ba) == [97, 98, 99, 100, 101, 102, 103, 104, 105]
assert tuple(ba[:3]) == (97, 98, 99)
x, y, z = ba[:3]
assert (x, y, z) == (97, 98, 99)
assert 100 in ba
assert 200 not in ba
assert min(ba) == 97
assert max(ba) == 105
assert any(ba) is True
assert all(ba) is True
assert any(bytearray(3)) is False
assert all(bytearray(b"\\x01\\x00")) is False
assert bytes(ba) == b"abcdefghi"
assert push_first(ba) == 10
assert call_unknown(push_first, [10]) == 2
assert bytes(annotated_step(bytearray(b"\\x05"))) == b"\\x07"
print("ok")
"""
    exe = build_executable(
        src,
        str(tmp_path / "bytearray_fast_paths.py"),
        roots=None,
        workers=1,
    )
    completed = subprocess.run([str(exe)], capture_output=True, text=True, check=False)
    assert completed.returncode == 0, completed.stderr
    assert completed.stdout == "ok\n"


def test_list_and_tuple_in_bounds_unboxed_indexing_and_setitem(tmp_path):
    import subprocess

    from mule.compiler.driver import build_executable, emit_llvm

    src = """def accumulate_and_search(items: list[int], targets: tuple[int, ...]) -> int:
    n = len(items)
    for i in range(n):
        items[i] = items[i] + (i >> 1)
    total = 0
    for q in range(len(targets)):
        target = targets[q]
        lo = 0
        hi = n - 1
        while lo <= hi:
            mid = (lo + hi) >> 1
            val = items[mid]
            if target < val:
                hi = mid - 1
            elif target > val:
                lo = mid + 1
            else:
                total += val
                break
    return total

print(accumulate_and_search([10, 20, 30, 40], (10, 20, 31, 99)))
"""
    ir = emit_llvm(src)
    spec_start = ir.index(
        'define internal ptr @"mule_fn_accumulate_and_search.spec.0"('
    )
    spec_end = ir.index("\n}", spec_start)
    spec_body = ir[spec_start:spec_end]
    assert "@mule_list_getitem_borrow" not in spec_body
    assert "@mule_tuple_getitem_borrow" not in spec_body
    assert "@mule.fast.setitem.move" not in spec_body
    assert "icmp ult i64" not in spec_body

    exe_src = (
        src
        + """
def check_bounds(items: list[int], idx: int) -> int:
    return items[idx]

assert check_bounds([5, 6, 7], -1) == 7
try:
    check_bounds([5, 6, 7], 3)
    raise AssertionError("expected IndexError")
except IndexError:
    print("bounds_ok")
"""
    )
    exe = build_executable(
        exe_src,
        str(tmp_path / "list_tuple_in_bounds.py"),
        roots=None,
        workers=1,
    )
    completed = subprocess.run([str(exe)], capture_output=True, text=True, check=False)
    assert completed.returncode == 0, completed.stderr
    assert completed.stdout == "61\nbounds_ok\n"


def test_enumerate_for_loop_open_codes_index_and_sequence_iteration():
    compilation = compile_source(
        "def weighted_sum(items: list[float]) -> float:\n"
        " total = 0.0\n"
        " for idx, val in enumerate(items, 2):\n"
        "  total += idx * val\n"
        " return total\n"
    )
    fn = compilation.lambdas[1]
    assert list(known_calls(fn, "%unpack")) == []
    assert list(known_calls(fn, "%unpack_ref")) == []
    check_ir1_consistency(compilation.components)
