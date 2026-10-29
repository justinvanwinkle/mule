"""Tests for IR1 open-coding of generator expression consumers."""

import pytest

from mule.compiler import emit_llvm
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_toplevel, ir1_phases
from test.mule.compiler.test_exception_groups import compare
from test.mule.compiler.test_native import run_native

NESTED_FUSION_SOURCE = """
def ordered(n):
    return sorted((value for value in (item for item in range(n))), reverse=True)
"""

NESTED_SOURCE_SCOPE_SOURCE = """
def scoped():
    value = 10
    return sorted((value for value in (value for _ in [1])), reverse=True)
print(scoped())
"""

BUILTIN_CONSUMERS_SOURCE = """def list_comp():
    return [x * 2 for x in range(10)]
def list_gen():
    return list(x * 2 for x in range(10))
def set_gen():
    return set(x * 2 for x in range(10))
def tuple_gen():
    return tuple(x * 2 for x in range(10))
def dict_pair_gen():
    return dict((x, x * 2) for x in range(10))
def any_gen(k: int) -> bool:
    return any(x == k for x in range(10))
def all_gen() -> bool:
    return all(x >= 0 for x in range(10))
def sum_gen() -> int:
    return sum(x * x for x in range(10))
def max_gen() -> int:
    return max(x * 2 for x in range(10) if x >= 0)
def min_gen() -> int:
    return min(x * 2 for x in range(10) if x >= 0)
def for_gen() -> int:
    acc = 0
    for v in (x + 1 for x in range(10)):
        acc += v
    return acc
def unpack_gen():
    return [0, *(x + 1 for x in range(10))]
"""

FROZEN_SORTED_STAR_SOURCE = """def fs(xs):
    return frozenset(x + 1 for x in xs)
def srt(xs):
    return sorted((x * 2 for x in xs), reverse=True)
def star_one(f, xs):
    return f(*(x + 1 for x in xs))
def star_two(f, xs):
    return f(0, *(x + 1 for x in xs))
"""

NON_FUSIBLE_SOURCE = """def local_shadow(xs, list):
    return list(x for x in xs)
def with_locals(xs):
    return list(locals() for x in xs)
def yielding_gen(xs):
    return list((yield x) for x in xs)
def rejected_sum_start(xs):
    return sum((x for x in xs), '')
"""

CLASS_BODY_WALRUS_SOURCE = """class Box:
    items = [1, 2]
    bad = list((w := x) for x in items)
"""

GENEXPR_RUNTIME_SOURCE = """
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

GENEXPR_RUNTIME_EXPECTED = """any_multi True [('outer', 1), ('elem', 11), ('elem', 21)]
all_multi False [('elem', 11), ('elem', 21), ('elem', 31)]
list [1, 9]
set [0, 1, 2]
tuple (10, 11, 12, 13)
frozenset [0, 2, 4, 6]
sorted [5, 4, 3, 1, 1]
dict_pair {0: 0, 1: 10, 2: 20}
dict_dyn {1: 'a', 2: 'b'}
sum0 15
sum_pos 106
sum_kw 206
sum_str_err TypeError
min_basic -4
max_basic 5
min_key bb
max_key_fn -8
min_key_none 2
min_default 99
max_key_default -1
min_empty_err ValueError
for_collision [0, 10, 20, 30] 30
gen_yield [100, 102, 104]
unpack_list [1, 0, 2, 4, 9]
unpack_tuple (1, 5, 6)
unpack_set [1, 2, 3]
star_call_1 (0, 3, 6)
star_call_2 (0, 0, 3, 6, 99)
walrus [1, 2, 3, 4] 4
class_scope [110, 120]
"""

BODY_STOPITERATION_TEMPLATE = """
def stop():
    raise StopIteration("body")
def collect(*args):
    return args
try:
    __EXPRESSION__
except Exception as error:
    print(type(error).__name__, type(error.__cause__).__name__)
"""

EXCEPTION_BOUNDARY_TEMPLATE = """
def stop(*args):
    raise StopIteration("boundary")
def fail():
    raise ValueError("ordinary")
class BadIter:
    def __iter__(self):
        return stop()
class Exhausted:
    def __iter__(self):
        return self
    def __next__(self):
        raise StopIteration
class BadBool:
    def __bool__(self):
        return stop()
class BadSum:
    def __add__(self, value):
        return stop()
try:
    __STATEMENT__
except Exception as error:
    print(type(error).__name__, type(error.__cause__).__name__)
else:
    print("normal exhaustion")
"""

SHADOWED_STOPITERATION_SOURCE = """
class MyStop(StopIteration):
    pass
original = MyStop("original")
runtime_error = RuntimeError
def stop():
    raise original
StopIteration = ValueError
RuntimeError = ValueError
try:
    list(stop() for _ in range(1))
except runtime_error as error:
    print(type(error).__name__, error.__cause__ is original)
"""

ITERATION_ORDER_TEMPLATE = """
events = []
class Items:
    def __init__(self):
        self.index = 0
    def __iter__(self):
        events.append("iter")
        return self
    def __next__(self):
        events.append("next")
        if self.index == 2:
            raise StopIteration
        self.index += 1
        return self.index
def source():
    events.append("source")
    return Items()
def element(value):
    events.append("element")
    return value
def key(value):
    events.append("key call")
    return value
def later(label, value):
    events.append(label)
    return value
print(__EXPRESSION__)
print(events)
"""

CREATION_ARGUMENT_ORDER_TEMPLATE = """
events = []
def stop(label):
    events.append(label)
    raise StopIteration(label)
class BadIter:
    def __iter__(self):
        return stop("iter")
def later():
    events.append("later")
    return None
try:
    __EXPRESSION__
except Exception as error:
    print(type(error).__name__, str(error), type(error.__cause__).__name__)
print(events)
"""


def _has_genexpr_lambda(compilation) -> bool:
    return any(fn.name.endswith(".<genexpr>") for fn in compilation.lambdas)


def test_builtin_consumers_eliminate_genexpr_lambda_and_use_direct_range():
    source = BUILTIN_CONSUMERS_SOURCE
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
    source = FROZEN_SORTED_STAR_SOURCE
    compilation = compile_toplevel(source)
    ir1_phases(compilation)
    check_ir1_consistency(compilation.components)
    assert not _has_genexpr_lambda(compilation)
    for fn in compilation.lambdas[1:]:
        assert list(known_calls(fn, "make_generator")) == []


def test_nested_sorted_genexpr_keeps_direct_range_without_generator_allocation():
    compilation = compile_toplevel(NESTED_FUSION_SOURCE)
    ir1_phases(compilation)
    check_ir1_consistency(compilation.components)
    assert not _has_genexpr_lambda(compilation)
    assert not any(
        list(known_calls(function, "make_generator"))
        for function in compilation.lambdas
    )
    assert any(
        list(known_calls(function, "%range_prepare"))
        for function in compilation.lambdas
    )


def test_nested_genexpr_retains_its_defining_source_scope(tmp_path):
    compare(tmp_path, NESTED_SOURCE_SCOPE_SOURCE)


def test_shadowed_or_non_fusible_genexpr_keeps_generator():
    source = NON_FUSIBLE_SOURCE
    compilation = compile_toplevel(source)
    ir1_phases(compilation)
    check_ir1_consistency(compilation.components)
    genexprs = [fn for fn in compilation.lambdas if fn.name.endswith(".<genexpr>")]
    assert len(genexprs) == 4

    with pytest.raises(SyntaxError, match="class body"):
        compile_toplevel(CLASS_BODY_WALRUS_SOURCE)


def test_genexpr_fusion_runtime_semantics(tmp_path):
    source = GENEXPR_RUNTIME_SOURCE
    result = run_native(tmp_path, source)
    assert result.stdout == GENEXPR_RUNTIME_EXPECTED


@pytest.mark.parametrize(
    "expression",
    [
        f"{consumer}(stop() for _ in range(1))"
        for consumer in (
            "list",
            "set",
            "tuple",
            "frozenset",
            "sorted",
            "dict",
            "any",
            "all",
            "sum",
            "min",
            "max",
        )
    ]
    + ["[*(stop() for _ in range(1))]", "collect(*(stop() for _ in range(1)))"],
)
def test_fused_genexpr_body_stopiteration_converts_without_generator_allocation(
    tmp_path, expression
):
    source = BODY_STOPITERATION_TEMPLATE.replace("__EXPRESSION__", expression)
    compilation = compile_toplevel(source)
    ir1_phases(compilation)
    check_ir1_consistency(compilation.components)
    assert not _has_genexpr_lambda(compilation)
    assert not any(
        list(known_calls(function, "make_generator"))
        for function in compilation.lambdas
    )
    compare(tmp_path, source)


@pytest.mark.parametrize(
    "statement",
    [
        "list(value for value in [1] if stop())",
        "list(value for value, ignored in [BadIter()])",
        "list(fail() for value in [1])",
        "list(inner for outer in [1] for inner in stop())",
        "list(inner for outer in [1] for inner in BadIter())",
        "list(value for value in stop())",
        "list(value for value in BadIter())",
        "list(value for value in Exhausted())",
        "any(value for value in [BadBool()])",
        "dict(value for value in [BadIter()])",
        "sum((value for value in [1]), BadSum())",
        "min((value for value in [1]), key=stop)",
        "for value in (item for item in [1]):\n        stop()",
    ],
)
def test_fused_genexpr_distinguishes_body_iterator_and_consumer_exceptions(
    tmp_path, statement
):
    compare(
        tmp_path,
        EXCEPTION_BOUNDARY_TEMPLATE.replace("__STATEMENT__", statement),
    )


def test_fused_genexpr_preserves_stopiteration_subclass_cause_when_globals_shadowed(
    tmp_path,
):
    compare(
        tmp_path,
        SHADOWED_STOPITERATION_SOURCE,
    )


@pytest.mark.parametrize(
    "expression",
    [
        'sorted((element(value) for value in source()), key=later("key", key), reverse=later("reverse", False))',
        'sum((element(value) for value in source()), later("start", 0))',
        'min((element(value) for value in source()), key=later("key", key), default=later("default", 0))',
        'max((element(value) for value in source()), default=later("default", 0), key=later("key", key))',
    ],
)
def test_fused_genexpr_creates_iterator_before_arguments_and_drains_afterward(
    tmp_path, expression
):
    source = ITERATION_ORDER_TEMPLATE.replace("__EXPRESSION__", expression)
    compare(tmp_path, source)


@pytest.mark.parametrize(
    "expression",
    [
        'sorted((stop("body") for value in [1]), key=stop("argument"))',
        "sum((value for value in BadIter()), later())",
        "min((value for value in BadIter()), default=later())",
        "max((value for value in BadIter()), key=later())",
    ],
)
def test_fused_genexpr_creation_and_argument_errors_precede_body_evaluation(
    tmp_path, expression
):
    source = CREATION_ARGUMENT_ORDER_TEMPLATE.replace("__EXPRESSION__", expression)
    compare(tmp_path, source)
