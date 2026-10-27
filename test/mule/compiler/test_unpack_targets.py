"""Display destructuring uses value edges independently of assignment targets."""

import pytest

from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case


@pytest.mark.parametrize(
    "assignment",
    [
        "target.value, other = (left, right)",
        "target[0], other = [left, right]",
        "global_value, other = (left, right)",
        "target[0], (other, target[1]) = (left, (right, left))",
        "target[index()], other = (left, right)",
    ],
)
def test_display_unpack_arbitrary_targets_remove_allocation_and_extraction(assignment):
    compilation = compile_source(
        "def assign(target, left, right, index):\n"
        " global global_value\n"
        f" {assignment}\n"
        " return other\n"
    )
    function = compilation.lambdas[1]
    for operation in ("%build_tuple", "%build_list", "%unpack", "%unpack_ref"):
        assert not list(known_calls(function, operation)), operation
    check_ir1_consistency(compilation.components)


@pytest.mark.integration
def test_display_unpack_target_order_exceptions_and_owners_match_python(tmp_path):
    source = """
import weakref
references = []

class Token:
    def __init__(self, label):
        self.label = label

class Target:
    def __setattr__(self, name, value):
        print('attribute', name, value.label)

def produce(label):
    print('rhs', label)
    value = Token(label)
    references.append(weakref.ref(value, lambda ref: print('destroy', label)))
    return value

def target_index():
    print('index')
    return 0

def assign(target, sink, label):
    other = 'old'
    try:
        target.bad, other = (produce(label + '-one'), produce(label + '-two'))
    except AttributeError:
        print('caught attribute', other)
    try:
        sink[target_index()], other = [produce(label + '-three'), produce(label + '-four')]
    except IndexError:
        print('caught subscript', other.label if target is not None else other)
    print('after assignments')
    del other

def clear_head():
    global head
    print('before clear')
    head = None
    print('after clear')
    return 0

def overwrite(label):
    global head
    sink = [None]
    head, sink[clear_head()] = (produce(label + '-head'), produce(label + '-tail'))
    print('after overwrite', head is None, sink[0].label)
    del sink

def nested(sink, value):
    other = 'old'
    try:
        sink[0], (other, missing) = (value, [value])
    except ValueError:
        print('partial nested', sink, other)

label = input()
assign(Target(), [], label)
assign(None, [], label)
overwrite(label)
nested([0], label)
values = [label, label + '!']
values[0], values[1] = values[1], values[0]
print('swap', values)
"""
    case = build_case(tmp_path, source)
    body = case.function_ir(r"^mule_fn_assign\.")
    for symbol in (
        "mule_sequence_tuple_owned",
        "mule_sequence_list",
        "mule_tuple_slot",
        "mule_fast_unpack",
    ):
        assert symbol not in body
    case.compare(stdin="owned\n")
