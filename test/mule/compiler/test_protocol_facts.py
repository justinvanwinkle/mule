"""Physical storage result facts and expression-temporary ownership."""

import pytest

from mule.compiler.codegen import FunctionGenerator, ModuleGenerator, compile_component
from mule.compiler.ctype import BooleanType, IntegerType
from mule.compiler.driver import compile_program
from mule.compiler.node import Combination, Set
from mule.compiler.protocols import compile_protocol


@pytest.mark.parametrize(
    "primitive,expected",
    [
        ("_tuple_storage", BooleanType()),
        ("_storage_length", IntegerType(0, None)),
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
