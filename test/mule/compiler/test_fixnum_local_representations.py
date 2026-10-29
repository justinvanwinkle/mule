"""Flow-proven integers select machine storage without a whole-function plan."""

import re

import pytest

from mule.compiler import emit_llvm
from mule.compiler.codegen import compile_component
from mule.compiler.ctype import UNKNOWN
from mule.compiler.ltn import EntryContext
from mule.compiler.main import compile_source
from mule.compiler.vop import FIXNUM, SIGNED
from test.mule.compiler.rewrite_support import build_case
from test.mule.compiler.test_exception_groups import compare
from test.mule.compiler.test_llvm_boxing import function_body

RECURRENCE = """
def calculate(n, label):
    value = 0
    for index in range(n):
        value = (value + 1) & 65535
    print(label)
    return value
"""


def ordinary_component(source):
    compilation = compile_source(source)
    function = compilation.lambdas[1]
    facts = compilation.facts[function]
    context = EntryContext(
        None,
        facts.types,
        facts.targets,
        facts.parameters or (UNKNOWN,) * len(function.vars),
        facts.return_type,
    )
    return compile_component(function, context)


def test_fixnum_recurrence_uses_signed_local_in_ordinary_entry():
    component = ordinary_component(RECURRENCE)
    local = next(
        tn
        for variable, tn in component.environment.vars.items()
        if variable.name == "value"
    )
    assert component.context.plan is None
    assert local.primitive_type is FIXNUM
    assert local.sc is SIGNED
    uses = {
        block.number
        for block in component.blocks
        for vop in block.vops
        for ref in (*vop.args, *vop.results)
        if ref.tn is local
    }
    assert len(uses) > 1
    for block in component.blocks:
        for vop in block.vops:
            if vop.info.name in ("move", "load-local"):
                assert vop.args[0].tn.sc.llvm_type == vop.results[0].tn.sc.llvm_type


def test_fixnum_operations_preserve_borrowed_container_local(tmp_path):
    source = """
def measure(grid: list[list[int]], n: int):
    row = grid[n & 3]
    count = (n & 65535) + 1
    positive = count > 5
    return row[count & 1], positive, count, len(grid)
print(measure([[1, 2], [3, 4], [5, 6], [7, 8]], 7))
print(measure([[1, 2], [3, 4], [5, 6], [7, 8]], 3))
"""
    body = function_body(emit_llvm(source), '"mule_fn_measure.numeric.observed"')
    getters = re.findall(r"call ptr @(mule_list_getitem(?:_borrow)?)\(", body)
    assert getters and getters[0] == "mule_list_getitem_borrow"
    assert "icmp sgt i64" in body
    compare(tmp_path, source)


@pytest.mark.integration
def test_fixnum_recurrence_executes_native_arithmetic_across_blocks(tmp_path):
    case = build_case(tmp_path, RECURRENCE + "print(calculate(int(input()), 'done'))\n")
    for value in (0, 3, 65537):
        case.compare(stdin=f"{value}\n")
    body = case.function_ir(r"^mule_fn_calculate\.body$")
    # Follow the accumulator, not range dispatch or the unused loop index.
    # Each loop clone must feed raw arithmetic directly back to an i64 phi.
    recurrences = list(
        re.finditer(r"(%[\w.$-]+) = and i64 (%[\w.$-]+), (65535|131070)\b", body)
    )
    assert recurrences, body
    for recurrence in recurrences:
        result, operand, mask = recurrence.groups()
        assert mask == "65535"
        addition = re.search(
            rf"{re.escape(operand)} = add (?:\w+ )*i64 (%[\w.$-]+), 1\b", body
        )
        assert addition, recurrence.group()
        accumulator = addition.group(1)
        assert re.search(rf"{re.escape(accumulator)} = phi i64 ", body)
        assert re.search(rf"phi i64 [^\n]*\[ {re.escape(result)},", body)


@pytest.mark.parametrize(
    "source",
    [
        """
def choose(flag):
    if flag:
        value = -4611686018427387904
    else:
        value = 4611686018427387903
    saved = [value]
    print(value, value + 1, value - 1, saved[0] == value)
choose(False)
choose(True)
""",
        """
def wide(flag):
    if flag:
        value = 7
    else:
        value = 4611686018427387904
    saved = [value]
    if value < 128:
        print(value * 2)
    print(value, value + 1, saved[0] is value, type(value).__name__)
wide(False)
wide(True)
""",
        """
def mixed(flag):
    value = 3
    if flag:
        value = True
    print(value, type(value).__name__)
mixed(False)
mixed(True)
""",
        """
def unbound(flag):
    if flag:
        value = 7
    try:
        print(value)
    except UnboundLocalError:
        print('unbound')
    value = 9
    del value
    try:
        print(value)
    except UnboundLocalError:
        print('deleted')
unbound(False)
unbound(True)
""",
        """
def failing(n):
    value = 0
    for index in range(n):
        value = (value + 1) & 255
    try:
        value = value // 0
    except ZeroDivisionError:
        print(value)
failing(0)
failing(3)
""",
        """
def captured(n):
    value = 0
    def read():
        return value
    for index in range(n):
        value = (value + 1) & 255
        print(read())
    return read
read = captured(3)
print(read())
""",
        """
def local_capture(n):
    value = 0
    def read():
        return value
    for index in range(n):
        value = (value + 1) & 255
        print(read())
local_capture(0)
local_capture(3)
""",
        """
def operations(n, flag):
    if flag:
        value = -7
    else:
        value = 7
    for index in range(n):
        value = ((value * 3 - 2) ^ 31) & 255
    print(value, -value, +value, ~value, value < 128, value >= 128)
operations(0, False)
operations(0, True)
operations(4, False)
operations(4, True)
""",
        """
def generate():
    value = 0
    for index in range(3):
        value = (value + 1) & 255
        yield value
iterator = generate()
print(next(iterator))
print(iterator.gi_frame.f_locals['value'])
print(next(iterator))
print(iterator.gi_frame.f_locals['value'])
""",
        """
def generate(flag):
    if flag:
        value = 7
    yield 0
iterator = generate(False)
print(next(iterator))
print('value' in iterator.gi_frame.f_locals)
iterator = generate(True)
print(next(iterator))
print(iterator.gi_frame.f_locals['value'])
""",
    ],
    ids=[
        "fixnum-endpoints",
        "wide-integer-join",
        "boolean-join",
        "binding-state",
        "failed-assignment",
        "closure-slot",
        "stack-closure-slot",
        "branch-arithmetic",
        "suspended-locals",
        "suspended-unbound-local",
    ],
)
def test_fixnum_representations_preserve_python_boundaries(tmp_path, source):
    compare(tmp_path, source)
