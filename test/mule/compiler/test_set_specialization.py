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
    SET_TYPE,
    TEXT_TYPE,
    IteratorType,
    SetType,
    value_type_from_dict,
)
from mule.compiler.main import compile_source
from test.mule.compiler.test_exception_groups import compare
from test.mule.compiler.test_llvm_boxing import function_body
from test.mule.compiler.test_native import run_native


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
    from mule.compiler.ctype import PYTHON_INTEGER

    assert var_types["s"] == SET_TYPE
    assert var_types["combined"] == SET_TYPE
    assert var_types["val"] == PYTHON_INTEGER


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
    # The set and dict kinds alone select the protocols, so the general body
    # uses them. The multiply referenced sets carry no element types, and no
    # separate guarded entry is emitted for ``n: int``.
    body = function_body(llvm, '"mule_fn_exercise_set.body"')
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


def test_conservative_container_mutations_and_declared_instance_fields(tmp_path):
    source = """
from mule_types import optimize

def mutate_via_alias():
    xs = [1, 2]
    alias = xs
    alias[0] = "hello"
    return xs[0]

def mutate_nested_list():
    outer = [[1, 2]]
    inner = outer[0]
    inner[0] = "world"
    return outer[0][0]

def mutate_list_length():
    xs = [10, 20, 30]
    ys = xs
    ys.pop()
    ys.pop()
    return len(xs), xs[0]

def augmented_assign_list():
    xs = [1, 2]
    ys = xs
    ys += ["three"]
    return xs[2]

class Holder:
    def __init__(self, v):
        self.v = v

def mutate_undeclared_field():
    h = Holder(10)
    h.v = "changed"
    return h.v

class DeclaredBox:
    count: int
    items: list[int]
    __secret: int

    def __init__(self, count: int, items: list[int], secret: int):
        self.count = count
        self.items = items
        self.__secret = secret

    def total(self) -> int:
        s = self.count + self.__secret
        for x in self.items:
            s += x
        return s

class BaseField:
    value: int | str

    def __init__(self, v: int | str):
        self.value = v

class NarrowedSub(BaseField):
    value: int

    def read_narrowed(self) -> int:
        return self.value + 1

@optimize(safety=0)
def store_unchecked(box: DeclaredBox, bad_val):
    box.count = bad_val

print("alias:", mutate_via_alias())
print("nested:", mutate_nested_list())
print("length:", mutate_list_length())
print("aug:", augmented_assign_list())
print("undeclared:", mutate_undeclared_field())

def exercise_declared_fields():
    box = DeclaredBox(5, [10, 20], 7)
    print("box_total:", box.total())

    try:
        box.count = "not-an-int"
    except TypeError as exc:
        print("checked_field_store:", type(exc).__name__)

    try:
        box.items.append("not-an-int")
    except TypeError as exc:
        print("checked_field_elem_append:", type(exc).__name__)

    try:
        box.items[0] = "not-an-int"
    except TypeError as exc:
        print("checked_field_elem_setitem:", type(exc).__name__)

    # Unresolved dynamic setattr and @optimize(safety=0) bypass compile-time store
    # checks; reading the declared field trusts the declared type without a runtime
    # check on read.
    store_unchecked(box, 12)
    print("after_unchecked:", box.total())
    setattr(box, "count", 15)
    print("after_setattr:", box.total())

    sub = NarrowedSub(41)
    print("narrowed_ok:", sub.read_narrowed())
    setattr(sub, "value", "oops")
    try:
        print(sub.read_narrowed())
    except TypeError as exc:
        print("narrowed_read_checked:", type(exc).__name__)

exercise_declared_fields()
"""
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "alias: hello\n"
        "nested: world\n"
        "length: (1, 10)\n"
        "aug: three\n"
        "undeclared: changed\n"
        "box_total: 42\n"
        "checked_field_store: TypeError\n"
        "checked_field_elem_append: TypeError\n"
        "checked_field_elem_setitem: TypeError\n"
        "after_unchecked: 49\n"
        "after_setattr: 52\n"
        "narrowed_ok: 42\n"
        "narrowed_read_checked: TypeError\n"
    )
    ir = emit_llvm(source)
    unchecked_body = function_body(ir, '"mule_fn_store_unchecked.body"')
    assert "@mule_native_type_error" not in unchecked_body
