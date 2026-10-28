"""SetType lattice, set[T]/frozenset[T] annotations, and table/set protocol specialization."""

from mule import ast
from mule.compiler import emit_llvm
from mule.compiler.ctype import (
    BOOLEAN,
    BOTTOM,
    EMPTY_FROZENSET_TYPE,
    EMPTY_SET_TYPE,
    FROZENSET_TYPE,
    INTEGER,
    NONE_TYPE,
    PYTHON_INTEGER,
    SET_TYPE,
    TEXT_TYPE,
    IteratorType,
    SetType,
    value_type_from_dict,
)
from mule.compiler.main import compile_source
from test.mule.compiler.test_exception_groups import compare
from test.mule.compiler.test_llvm_boxing import function_body


def test_set_type_lattice_and_serialization():
    int_set = SetType("mule_dict_set", element=INTEGER)
    str_set = SetType("mule_dict_set", element=TEXT_TYPE)
    int_frozenset = SetType("mule_dict_frozenset", element=INTEGER)

    assert EMPTY_SET_TYPE.fits(SET_TYPE)
    assert int_set.fits(SET_TYPE)
    assert not int_set.fits(FROZENSET_TYPE)
    assert EMPTY_FROZENSET_TYPE.fits(FROZENSET_TYPE)
    assert int_frozenset.fits(FROZENSET_TYPE)
    assert not int_frozenset.fits(SET_TYPE)

    assert EMPTY_SET_TYPE.with_element(INTEGER) == int_set
    assert int_set.join(EMPTY_SET_TYPE) == int_set
    assert int_set.join(str_set) == SetType(
        "mule_dict_set", element=INTEGER.join(TEXT_TYPE)
    )

    assert int_set.iterate_type() == IteratorType(INTEGER)
    assert EMPTY_SET_TYPE.flow_iterate_type() == IteratorType(BOTTOM)
    assert EMPTY_SET_TYPE.exact_length() == 0
    assert int_set.exact_length() is None

    assert INTEGER.comparison(ast.ComparisonOperator.IN, int_set) is BOOLEAN
    assert INTEGER.comparison(ast.ComparisonOperator.NOT_IN, int_frozenset) is BOOLEAN

    for op in (
        ast.BinaryOperator.BIT_AND,
        ast.BinaryOperator.BIT_OR,
        ast.BinaryOperator.SUBTRACT,
        ast.BinaryOperator.BIT_XOR,
    ):
        assert int_set.binary(op, int_set) == int_set
        assert int_frozenset.binary(op, int_set) == int_frozenset

    assert int_set.binary(ast.BinaryOperator.BIT_OR, str_set) == SetType(
        "mule_dict_set", element=INTEGER.join(TEXT_TYPE)
    )
    assert int_set.binary(ast.BinaryOperator.SUBTRACT, str_set) == int_set

    res, updated, child_idx, returns_child, _ = EMPTY_SET_TYPE.method_call(
        "add", (INTEGER,), False
    )
    assert res is NONE_TYPE
    assert updated == int_set
    assert child_idx == 0
    assert not returns_child

    res, updated, _, returns_child, _ = int_set.method_call("pop", (), False)
    assert res == INTEGER
    assert updated is None
    assert returns_child

    for sample in (EMPTY_SET_TYPE, int_set, EMPTY_FROZENSET_TYPE, int_frozenset):
        assert value_type_from_dict(sample.to_dict()) == sample


def test_set_type_inference_and_element_contracts():
    compilation = compile_source("""
def build_and_consume(items: list[int]) -> tuple[set[int], int]:
    s: set[int] = set()
    for x in items:
        s.add(x)
    other = {1, 2, 3}
    combined = (s | other) - {2}
    total = 0
    for val in combined:
        total += val
    return combined, total
""")
    fn = compilation.lambdas[1]
    variant = compilation.facts[fn].variants[0]
    var_types = {var.name: variant.types[var.refs[-1]] for var in fn.locals if var.refs}
    assert var_types["s"] == SetType("mule_dict_set", element=PYTHON_INTEGER)
    assert var_types["combined"] == SetType("mule_dict_set", element=PYTHON_INTEGER)
    assert var_types["val"] == PYTHON_INTEGER
    assert var_types["total"] == INTEGER


def test_set_and_dict_protocol_specialization_in_llvm():
    llvm = emit_llvm("""
def exercise_set(n: int) -> int:
    s: set[int] = set()
    t: set[int] = {1, 2}
    d: dict[int, int] = {1: 10}
    s.add(n)
    s.discard(1)
    s.remove(n)
    a = s & t
    b = s | t
    c = s - t
    x = s ^ t
    if n in s or n not in t or n in d or n not in d:
        return len(a) + len(b) + len(c) + len(x) + len(d)
    return 0
""")
    body = function_body(llvm, '"mule_fn_exercise_set.spec.0"')
    for protocol in (
        "empty_set",
        "set_add",
        "set_discard",
        "set_remove",
        "set_and",
        "set_or",
        "set_sub",
        "set_xor",
        "set_contains",
        "set_not_contains",
        "dict_contains",
        "dict_not_contains",
        "table_len",
        "dict_len",
    ):
        assert f'@"mule.protocol.{protocol}"' in body
    assert '@"mule.protocol.call_resolved"' not in body
    assert '@"mule.protocol.contains"' not in body
    assert '@"mule.protocol.not_contains"' not in body


def test_set_annotations_and_operations_runtime_semantics(tmp_path):
    compare(
        tmp_path,
        """
def operate(left: set[int], right: frozenset[int], extra: int) -> set[int]:
    out: set[int] = set()
    for item in left:
        out.add(item + 1)
    out.discard(2)
    out.add(extra)
    out.remove(extra)
    out.add(extra * 2)
    merged = out | set(right)
    common = merged & left
    diff = merged - left
    sym = merged ^ left
    total = len(merged) + len(common) + len(diff) + len(sym)
    for v in sym:
        if v in merged and v not in common:
            total += v
    out.add(total)
    return out

print(sorted(operate({1, 2, 3}, frozenset([3, 4, 5]), 10)))

try:
    operate([1, 2], frozenset([1]), 3)
except TypeError as exc:
    print("bad_left:", type(exc).__name__)

def bad_elem(s: set[int]) -> int:
    total = 0
    for x in s:
        total += x
    return total

raw = set()
raw.add("not-an-int")
try:
    print(bad_elem(raw))
except TypeError as exc:
    print("bad_elem:", type(exc).__name__)
""",
    )
