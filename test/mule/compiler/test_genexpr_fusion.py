"""Tests for IR1 open-coding of generator expression consumers."""

import pytest

from mule.compiler import emit_llvm
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_toplevel, ir1_phases
from test.mule.compiler.test_native import run_native


def _has_genexpr_lambda(compilation) -> bool:
    return any(fn.name.endswith(".<genexpr>") for fn in compilation.lambdas)


def test_builtin_consumers_eliminate_genexpr_lambda_and_use_direct_range():
    source = (
        "def list_comp():\n"
        "    return [x * 2 for x in range(10)]\n"
        "def list_gen():\n"
        "    return list(x * 2 for x in range(10))\n"
        "def set_gen():\n"
        "    return set(x * 2 for x in range(10))\n"
        "def tuple_gen():\n"
        "    return tuple(x * 2 for x in range(10))\n"
        "def dict_pair_gen():\n"
        "    return dict((x, x * 2) for x in range(10))\n"
        "def any_gen(k: int) -> bool:\n"
        "    return any(x == k for x in range(10))\n"
        "def all_gen() -> bool:\n"
        "    return all(x >= 0 for x in range(10))\n"
        "def sum_gen() -> int:\n"
        "    return sum(x * x for x in range(10))\n"
        "def max_gen() -> int:\n"
        "    return max(x * 2 for x in range(10) if x >= 0)\n"
        "def min_gen() -> int:\n"
        "    return min(x * 2 for x in range(10) if x >= 0)\n"
        "def for_gen() -> int:\n"
        "    acc = 0\n"
        "    for v in (x + 1 for x in range(10)):\n"
        "        acc += v\n"
        "    return acc\n"
        "def unpack_gen():\n"
        "    return [0, *(x + 1 for x in range(10))]\n"
    )
    compilation = compile_toplevel(source)
    ir1_phases(compilation)
    check_ir1_consistency(compilation.components)
    assert not _has_genexpr_lambda(compilation)
    user_fns = [
        fn for fn in compilation.lambdas[1:] if not fn.name.endswith(".__annotate__")
    ]
    assert len(user_fns) == 12
    for fn in user_fns:
        assert list(known_calls(fn, "make_generator")) == []
        assert list(known_calls(fn, "iterate")) == []
        prepares = list(known_calls(fn, "%range_prepare"))
        assert len(prepares) == 1
        assert prepares[0].info.direct is True

    llvm_ir = emit_llvm(source)
    assert "@coroutine_new" not in llvm_ir
    assert "make_generator" not in llvm_ir


def test_frozenset_sorted_and_star_call_eliminate_genexpr_lambda():
    source = (
        "def fs(xs):\n"
        "    return frozenset(x + 1 for x in xs)\n"
        "def srt(xs):\n"
        "    return sorted((x * 2 for x in xs), reverse=True)\n"
        "def star_one(f, xs):\n"
        "    return f(*(x + 1 for x in xs))\n"
        "def star_two(f, xs):\n"
        "    return f(0, *(x + 1 for x in xs))\n"
    )
    compilation = compile_toplevel(source)
    ir1_phases(compilation)
    check_ir1_consistency(compilation.components)
    assert not _has_genexpr_lambda(compilation)
    for fn in compilation.lambdas[1:]:
        assert list(known_calls(fn, "make_generator")) == []


def test_shadowed_or_non_fusible_genexpr_keeps_generator():
    source = (
        "def local_shadow(xs, list):\n"
        "    return list(x for x in xs)\n"
        "def with_locals(xs):\n"
        "    return list(locals() for x in xs)\n"
        "def yielding_gen(xs):\n"
        "    return list((yield x) for x in xs)\n"
        "def rejected_sum_start(xs):\n"
        "    return sum((x for x in xs), '')\n"
    )
    compilation = compile_toplevel(source)
    ir1_phases(compilation)
    check_ir1_consistency(compilation.components)
    genexprs = [fn for fn in compilation.lambdas if fn.name.endswith(".<genexpr>")]
    assert len(genexprs) == 4

    with pytest.raises(SyntaxError, match="class body"):
        compile_toplevel(
            "class Box:\n    items = [1, 2]\n    bad = list((w := x) for x in items)\n"
        )


def test_genexpr_fusion_runtime_semantics(tmp_path):
    source = """
log = []
def step(tag, val):
    log.append((tag, val))
    return val

# 1. Multi-clause any/all short-circuiting across nested loops
log.clear()
r_any = any(
    step("elem", x + y) > 12
    for x in [1, 2, 3]
    if step("outer", x) > 0
    for y in [10, 20, 30]
)
print("any_multi", r_any, log)

log.clear()
r_all = all(
    step("elem", x + y) < 22
    for x in [1, 2, 3]
    for y in [10, 20, 30]
)
print("all_multi", r_all, log)

# 2. list, set, tuple, frozenset, sorted, dict (pairs and dynamic items)
print("list", list(x * x for x in range(5) if x % 2 == 1))
print("set", sorted(set(x % 3 for x in range(7))))
print("tuple", tuple(x + 10 for x in range(4)))
print("frozenset", sorted(frozenset(x * 2 for x in range(4))))
print("sorted", sorted((x for x in [3, 1, 4, 1, 5]), key=lambda v: -v))
print("dict_pair", dict((x, x * 10) for x in range(3)))
pairs = [(1, "a"), (2, "b")]
print("dict_dyn", dict(p for p in pairs))

# 3. sum with default start, positional start, keyword start, and TypeError on str start
print("sum0", sum(x for x in range(6)))
print("sum_pos", sum((x for x in range(4)), 100))
print("sum_kw", sum((x for x in range(4)), start=200))
try:
    sum((str(x) for x in range(2)), "")
except TypeError:
    print("sum_str_err", "TypeError")

# 4. min and max with key, default, key=None, and empty ValueError
print("min_basic", min(x * x - 4 * x for x in range(6)))
print("max_basic", max(x * x - 4 * x for x in range(6)))
print("min_key", min(("bb", "a", "ccc")[i] for i in range(3) if i != 1))
print("max_key_fn", max((x for x in [-5, 2, -8, 4]), key=abs))
print("min_key_none", min((x for x in [5, 2, 8]), key=None))
print("min_default", min((x for x in range(5) if x > 10), default=99))
print("max_key_default", max((x for x in ()), key=abs, default=-1))
try:
    min(x for x in range(3) if x > 10)
except ValueError:
    print("min_empty_err", "ValueError")

# 5. for loop over genexpr with target name collision and inside generator function
x = 999
seen = []
for x in (x * 10 for x in range(4)):
    seen.append(x)
print("for_collision", seen, x)

def gen_from_genexpr(n):
    for item in (k + 100 for k in range(n) if k % 2 == 0):
        yield item
print("gen_yield", list(gen_from_genexpr(5)))

# 6. Starred unpacking in displays and calls
print("unpack_list", [1, *(x * 2 for x in range(3)), 9])
print("unpack_tuple", (1, *(x + 5 for x in range(2))))
print("unpack_set", sorted({*(x for x in [3, 1, 2])}))
def collect(*args):
    return args
print("star_call_1", collect(*(x * 3 for x in range(3))))
print("star_call_2", collect(0, *(x * 3 for x in range(3)), 99))

# 7. Assignment expression in fused genexpr and class body lookup
last = 0
vals = list((last := v + 1) for v in range(4))
print("walrus", vals, last)

offset = 100
class Box:
    items = [10, 20]
    offset = 999
    captured = list(item + offset for item in items)
print("class_scope", Box.captured)
"""
    result = run_native(tmp_path, source)
    expected = (
        "any_multi True [('outer', 1), ('elem', 11), ('elem', 21)]\n"
        "all_multi False [('elem', 11), ('elem', 21), ('elem', 31)]\n"
        "list [1, 9]\n"
        "set [0, 1, 2]\n"
        "tuple (10, 11, 12, 13)\n"
        "frozenset [0, 2, 4, 6]\n"
        "sorted [5, 4, 3, 1, 1]\n"
        "dict_pair {0: 0, 1: 10, 2: 20}\n"
        "dict_dyn {1: 'a', 2: 'b'}\n"
        "sum0 15\n"
        "sum_pos 106\n"
        "sum_kw 206\n"
        "sum_str_err TypeError\n"
        "min_basic -4\n"
        "max_basic 5\n"
        "min_key bb\n"
        "max_key_fn -8\n"
        "min_key_none 2\n"
        "min_default 99\n"
        "max_key_default -1\n"
        "min_empty_err ValueError\n"
        "for_collision [0, 10, 20, 30] 30\n"
        "gen_yield [100, 102, 104]\n"
        "unpack_list [1, 0, 2, 4, 9]\n"
        "unpack_tuple (1, 5, 6)\n"
        "unpack_set [1, 2, 3]\n"
        "star_call_1 (0, 3, 6)\n"
        "star_call_2 (0, 0, 3, 6, 99)\n"
        "walrus [1, 2, 3, 4] 4\n"
        "class_scope [110, 120]\n"
    )
    assert result.stdout == expected
