"""IR1 conversion rejects invalid programs and orders effects as Python does."""

import pytest

from mule.compiler import UnsupportedSyntax
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import class_method_definitions
from mule.compiler.main import compile_source
from mule.compiler.node import (
    CallKind,
    Combination,
    GlobalVar,
    LambdaVar,
    NodeVisitor,
    Ref,
)


def lower(source, filename="<unknown>"):
    return compile_source(source, filename)


def nodes(lambda_):
    return [node for block in lambda_.component.blocks() for node in block.nodes()]


def local_names(lambda_):
    return tuple(var.name for var in lambda_.locals if not var.hidden)


@pytest.mark.parametrize(
    "source",
    [
        "print((1 if True else 2) if False else 3)\n",
        "print(1 and (2 or 3), 0 or (4 and 5))\n",
        "if True:\n if False:\n  print(1)\n else:\n  print(2)\n",
        "while False:\n while True:\n  break\n",
        "def f(x):\n if x:\n  return x\n return None\n",
    ],
)
def test_control_flow_graph_verifies_independent_of_block_creation_order(source):
    check_ir1_consistency(lower(source).components)


@pytest.mark.parametrize(
    "source",
    [
        "return 1\n",
        "break\n",
        "continue\n",
        "while False:\n pass\nelse:\n break\n",
        "def f(x):\n global x\n",
        "def f():\n x = 1\n global x\n",
        "def f():\n print(x)\n global x\n",
        "def f(x, x):\n pass\n",
    ],
)
def test_invalid_context_is_rejected_before_emission(source):
    with pytest.raises(SyntaxError):
        lower(source)


@pytest.mark.parametrize(
    "source",
    [
        "import os\n",
        "try:\n pass\nexcept* ValueError:\n pass\n",
        "async def f(y):\n return [x async for x in y]\n",
    ],
)
def test_unsupported_syntax_fails_explicitly_even_when_unreachable(source):
    with pytest.raises(UnsupportedSyntax):
        lower(source, "limited.py")


def test_error_preserves_original_filename_and_unicode_columns():
    with pytest.raises(UnsupportedSyntax) as caught:
        lower("名 = 1\n名 = [x async for x in y]\n", "unicode.py")
    error = caught.value
    assert (error.filename, error.lineno, error.offset) == ("unicode.py", 2, 5)
    assert error.text == "名 = [x async for x in y]\n"


def test_late_assignment_classifies_all_function_reads_as_local():
    compilation = lower("x = 1\ndef f():\n print(x)\n x = 2\n")
    function = compilation.lambdas[1]
    assert local_names(function) == ("x",)
    reads = [node for node in nodes(function) if isinstance(node, Ref)]
    assert isinstance(
        next(read for read in reads if read.leaf.source_name == "x").leaf, LambdaVar
    )
    assert isinstance(
        next(read for read in reads if read.leaf.source_name == "print").leaf,
        GlobalVar,
    )


def class_definitions(lambda_):
    return [
        node.info
        for node in nodes(lambda_)
        if isinstance(node, Combination)
        and node.kind is CallKind.KNOWN
        and node.fun_info.name == "%make_class"
    ]


def test_class_layout_uses_receiver_writes_across_all_methods():
    compilation = lower(
        "class Point:\n"
        " def __init__(point, x):\n"
        "  point.x = x\n"
        " def move(point, amount):\n"
        "  point.x += amount\n"
        "  if amount:\n"
        "   point.y = 2\n"
        "  return point.x\n"
    )
    definitions = class_definitions(compilation.lambdas[0])
    assert len(definitions) == 1
    definition = definitions[0]
    assert definition.public_name == "Point"
    assert definition.fields == ("x", "y")
    methods = class_method_definitions(compilation.lambdas[0])[definition.identity]
    assert tuple(methods) == ("__init__", "move")
    assert local_names(compilation.lambdas[1]) == ("point", "x")
    assert "y" not in local_names(compilation.lambdas[2])


def test_class_method_scope_does_not_change_module_global_declarations():
    compilation = lower(
        "class C:\n"
        " def method(self, value):\n"
        "  global answer\n"
        "  answer = value\n"
        "  return answer\n"
        "global answer\n"
        "answer = 42\n"
    )
    assert local_names(compilation.lambdas[0]) == ()
    assert local_names(compilation.lambdas[1]) == ("self", "value")


def test_attribute_assignment_evaluates_value_before_receiver():
    trace = instruction_trace("receiver().field = value()\n")
    assert trace == [
        ("load", "value"),
        ("call", 0),
        ("load", "receiver"),
        ("call", 0),
        ("set", "field"),
    ]


def test_augmented_attribute_evaluates_receiver_once_before_value():
    trace = instruction_trace("receiver().field += value()\n")
    assert trace == [
        ("load", "receiver"),
        ("call", 0),
        ("get", "field"),
        ("load", "value"),
        ("call", 0),
        ("set", "field"),
    ]


def test_method_call_resolves_attribute_before_argument_side_effects():
    trace = instruction_trace("receiver().method(value())\n")
    assert trace == [
        ("load", "receiver"),
        ("call", 0),
        ("resolve", "method"),
        ("load", "value"),
        ("call", 0),
        ("method-call", 1),
    ]


def test_classes_reject_unimplemented_features_explicitly():
    with pytest.raises(UnsupportedSyntax):
        lower("class C[T]:\n pass\n")


class InstructionTrace(NodeVisitor):
    """The order of variable reads, calls, and attribute operations."""

    def __init__(self):
        self.events = []

    def visit_Ref(self, node):
        leaf = node.leaf
        if isinstance(leaf, (LambdaVar, GlobalVar)) and not leaf.source_name.startswith(
            "$"
        ):
            self.events.append(("load", leaf.source_name))

    def visit_Combination(self, node):
        if node.kind is not CallKind.KNOWN:
            self.events.append(("call", len(node.args)))
            return
        name = node.fun_info.name
        if name == "%getattr":
            self.events.append(("get", node.info.name))
        elif name == "%setattr":
            self.events.append(("set", node.info.name))
        elif name == "%resolve_method":
            self.events.append(("resolve", node.info.name))
        elif name == "%call_resolved":
            self.events.append(("method-call", len(node.args) - 1))


def instruction_trace(source):
    compilation = lower(source)
    trace = InstructionTrace()
    for node in nodes(compilation.lambdas[0]):
        node.accept(trace)
    return trace.events
