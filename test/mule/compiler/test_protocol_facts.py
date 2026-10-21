"""Physical storage result facts and expression-temporary ownership."""

import pytest

from mule.compiler import fndb
from mule.compiler.codegen import FunctionGenerator, ModuleGenerator, compile_component
from mule.compiler.ctype import (
    BOTTOM,
    UNBOUND,
    UNKNOWN,
    BooleanType,
    IntegerType,
    TupleType,
    UnionType,
)
from mule.compiler.driver import compile_program
from mule.compiler.main import compile_source, compile_toplevel
from mule.compiler.modules import ModuleCompiler
from mule.compiler.node import Combination, GlobalVar, Ref, Set
from mule.compiler.protocols import PRIMITIVES, compile_protocol


@pytest.mark.parametrize(
    "source,arity",
    [
        ("def value():\n return (None, None)\n", 2),
        ("def value(item):\n return (item, None)\n", 2),
        ("def value(flag):\n if flag:\n  return (1, 2)\n return (3, 4)\n", 2),
        ("def value(flag):\n if flag:\n  return (1,)\n return (2, 3)\n", None),
    ],
)
def test_tuple_results_retain_exact_builtin_arity(source, arity):
    compilation = compile_source(source)
    assert compilation.facts is not None
    result = compilation.facts[compilation.lambdas[1]].return_type
    assert isinstance(result, TupleType)
    assert result.arity == arity


def test_successful_unknown_iterable_unpack_proves_tuple_capture_arity():
    compilation = compile_source("def capture(value):\n a, b = value\n return a\n")
    assert compilation.facts is not None
    function = compilation.lambdas[1]
    assert function.component is not None
    nodes = [node for block in function.component.blocks() for node in block.nodes()]
    unpack = next(
        node
        for node in nodes
        if isinstance(node, Combination)
        and node.fun_info is not None
        and node.fun_info.name == "%unpack"
    )
    capture = compilation.facts[function].types[unpack]
    assert isinstance(capture, TupleType)
    assert capture.arity == 2
    for node in nodes:
        if (
            isinstance(node, Combination)
            and node.fun_info is not None
            and node.fun_info.name == "%unpack_ref"
        ):
            assert compilation.facts[function].types[node] == UNKNOWN


@pytest.mark.parametrize(
    "left,right,expected",
    [
        (TupleType(2), TupleType(2), TupleType(2)),
        (TupleType(2), TupleType(3), TupleType()),
        (TupleType(2), TupleType(), TupleType()),
        (TupleType(2), UNKNOWN, UNKNOWN),
        (
            TupleType(2),
            IntegerType(),
            UnionType(frozenset((TupleType(2), IntegerType()))),
        ),
        (TupleType(2), BOTTOM, TupleType(2)),
        (TupleType(2), UNBOUND, TupleType(2, may_be_unbound=True)),
        (TupleType(2), UNKNOWN.with_unbound(), UNKNOWN.with_unbound()),
    ],
)
def test_tuple_arity_joins_preserve_only_common_facts(left, right, expected):
    assert left.join(right) == expected
    assert right.join(left) == expected


@pytest.mark.parametrize(
    "actual,expected,fits",
    [
        (TupleType(2), TupleType(2), True),
        (TupleType(2), TupleType(), True),
        (TupleType(), TupleType(2), False),
        (TupleType(2), TupleType(3), False),
        (TupleType(2, may_be_unbound=True), TupleType(), False),
        (TupleType(2), UNKNOWN, True),
        (UNKNOWN, TupleType(), False),
    ],
)
def test_tuple_arity_fits_respect_unknown_and_unbound_values(actual, expected, fits):
    assert actual.fits(expected) is fits
    assert actual.dispatch_fits(expected) is fits


def test_tuple_comparison_still_invalidates_globals_for_element_callbacks():
    compilation = compile_source(
        "def compare(item):\n global result\n result = (1, 2)\n"
        " same = (item,) == (item,)\n return result\n"
    )
    assert compilation.facts is not None
    assert compilation.facts[compilation.lambdas[1]].return_type == UNKNOWN
    assert not TupleType(2).is_immortal


def test_tuple_control_flow_join_with_non_tuple_discards_tuple_proof():
    compilation = compile_source(
        "def choose(flag):\n if flag:\n  return (1, 2)\n return [1, 2]\n"
    )
    assert compilation.facts is not None
    result = compilation.facts[compilation.lambdas[1]].return_type
    assert isinstance(result, UnionType)
    assert not result.fits_tuple(TupleType())
    assert any(member.fits_tuple(TupleType()) for member in result.members)
    assert any(member.is_exact_list for member in result.members)


@pytest.mark.parametrize(
    "primitive,expected",
    [
        ("_tuple_storage", BooleanType()),
        ("_storage_length", IntegerType(0, (1 << 60) - 1)),
        ("_integer_storage", IntegerType()),
    ],
)
def test_physical_primitive_results_have_proven_types(primitive, expected):
    compilation = compile_protocol(
        f"def probe(value):\n return {primitive}(value)\n",
        "<primitive-proof>",
        namespace="$primitive-proof",
    )
    probe = compilation.lambdas[1]
    assert compilation.facts[probe].return_type == expected


@pytest.mark.parametrize(
    "binding",
    [
        "_active_exception = None\n",
        "from _mule_native import active_exception as _active_exception\n",
    ],
)
def test_module_binding_shadowing_primitive_uses_mutable_global(binding):
    compilation = compile_toplevel(
        binding + "def probe():\n return _active_exception\n",
        imports=ModuleCompiler("<primitive-shadow>"),
        primitives=PRIMITIVES,
    )
    stores = [
        node
        for component in compilation.components
        for block in component.blocks()
        for node in block.nodes()
        if isinstance(node, Set) and node.var.name == "_active_exception"
    ]
    assert len(stores) == 1
    variable = stores[0].var
    assert isinstance(variable, GlobalVar)
    probe = compilation.lambdas[1]
    assert probe.component is not None
    assert any(
        isinstance(node, Ref) and node.leaf is variable
        for block in probe.component.blocks()
        for node in block.nodes()
    )


def test_unpack_capture_belongs_to_exception_transfer_cleanup():
    program = compile_program(
        "def capture(value):\n"
        " try:\n  a, b = value\n"
        " except ValueError:\n  return None\n"
        " return a\n"
    )
    function = program.lambdas[1]
    assert function.component is not None
    unpack = next(
        node
        for block in function.component.blocks()
        for node in block.nodes()
        if isinstance(node, Combination)
        and node.fun_info is not None
        and node.fun_info.name == "%unpack"
    )
    assert unpack.lvar is not None
    store = unpack.lvar.dest
    assert isinstance(store, Set)
    capture = store.var
    module = ModuleGenerator(program)
    general, _ = module.contexts(function)
    generator = FunctionGenerator(module, compile_component(function, general))
    capture_tn = generator.environment.vars[capture]
    assert capture_tn in generator.value_tns
    accesses = [
        vop
        for block in generator.component.blocks
        for vop in block.vops
        if vop.info.translate is fndb.UNPACK_REF
    ]
    assert accesses
    for access in accesses:
        assert access.args[0].tn is capture_tn
