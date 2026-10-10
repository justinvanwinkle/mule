import pytest

from mule import ast
from mule.compiler import UnsupportedSyntax, ir, lower


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
    ir.verify(lower(source))


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
        "class C(A, B):\n pass\n",
        "x = [1, 2]\n",
        "for i in range(3):\n print(i)\n",
        "print(value=1)\n",
        "def f(x=1):\n pass\n",
        "def f():\n def g():\n  pass\n",
        "def f():\n return\n yield 1\n",
        "x: int = 1\n",
    ],
)
def test_unsupported_syntax_fails_explicitly_even_when_unreachable(source):
    with pytest.raises(UnsupportedSyntax):
        lower(source, "limited.py")


def test_error_preserves_original_filename_and_unicode_columns():
    with pytest.raises(UnsupportedSyntax) as caught:
        lower("名 = 1\n名 = [2]\n", "unicode.py")
    error = caught.value
    assert (error.filename, error.lineno, error.offset) == ("unicode.py", 2, 5)
    assert error.text == "名 = [2]\n"


def test_late_assignment_classifies_all_function_reads_as_local():
    module = lower("x = 1\ndef f():\n print(x)\n x = 2\n")
    function = module.functions[1]
    assert function.locals == ("x",)
    collector = LoadCollector()
    for block in function.blocks:
        for instruction in block.instructions:
            instruction.accept(collector)
    reads = collector.reads
    assert next(read for read in reads if read.name == "x").local
    assert not next(read for read in reads if read.name == "print").local


def test_ir_rejects_multiply_consumed_expression_result():
    module = ir.Module(
        [
            ir.Function(
                "module",
                (),
                (),
                [
                    ir.Block(
                        "entry",
                        [
                            ir.Constant("a", ast.IntegerLiteral(1)),
                            ir.Binary("b", ast.BinaryOperator.ADD, "a", "a"),
                        ],
                        ir.Return("b"),
                    )
                ],
            )
        ],
        "module",
    )
    with pytest.raises(ValueError, match="multiply consumed"):
        ir.verify(module)


def test_ir_rejects_value_not_dominating_its_use():
    module = ir.Module(
        [
            ir.Function(
                "module",
                (),
                (),
                [
                    ir.Block(
                        "entry",
                        [ir.Constant("condition", ast.BooleanLiteral(True))],
                        ir.Branch("condition", "left", "right"),
                    ),
                    ir.Block(
                        "left",
                        [ir.Constant("a", ast.IntegerLiteral(1))],
                        ir.Jump("right"),
                    ),
                    ir.Block("right", [], ir.Return("a")),
                ],
            )
        ],
        "module",
    )
    with pytest.raises(ValueError, match="dominate"):
        ir.verify(module)


class LoadCollector(ir.Visitor):
    def __init__(self):
        self.reads = []

    def visit_Load(self, instruction):
        self.reads.append(instruction)


def test_class_layout_uses_receiver_writes_across_all_methods():
    module = lower(
        "class Point:\n"
        " def __init__(point, x):\n"
        "  point.x = x\n"
        " def move(point, amount):\n"
        "  point.x += amount\n"
        "  if amount:\n"
        "   point.y = 2\n"
        "  return point.x\n"
    )
    collector = ClassCollector()
    for instruction in module.functions[0].blocks[0].instructions:
        instruction.accept(collector)
    assert len(collector.classes) == 1
    definition = collector.classes[0]
    assert definition.public_name == "Point"
    assert definition.fields == ("x", "y")
    assert definition.method_names == ("__init__", "move")
    assert module.functions[1].locals == ("point", "x")
    assert "y" not in module.functions[2].locals


def test_class_method_scope_does_not_change_module_global_declarations():
    module = lower(
        "class C:\n"
        " def method(self, value):\n"
        "  global answer\n"
        "  answer = value\n"
        "  return answer\n"
        "global answer\n"
        "answer = 42\n"
    )
    assert module.functions[0].locals == ()
    assert module.functions[1].locals == ("self", "value")


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


@pytest.mark.parametrize(
    "source",
    [
        "@decorator\nclass C:\n pass\n",
        "class C[T]:\n pass\n",
        "class C(A, B):\n pass\n",
        "class C(metaclass=Meta):\n pass\n",
        "class C:\n x = 1\n",
        "class C:\n if True:\n  pass\n",
        "def f():\n class C:\n  pass\n",
        "class C:\n @decorator\n def f(self):\n  pass\n",
        "class C:\n def f(self):\n  return\n  def nested():\n   pass\n",
    ],
)
def test_classes_reject_unimplemented_features_explicitly(source):
    with pytest.raises(UnsupportedSyntax):
        lower(source)


class ClassCollector(ir.Visitor):
    def __init__(self):
        self.classes = []

    def visit_DefineClass(self, instruction):
        self.classes.append(instruction)


class InstructionTrace(ir.Visitor):
    def __init__(self):
        self.events = []

    def visit_Load(self, instruction):
        if not instruction.name.startswith("$"):
            self.events.append(("load", instruction.name))

    def visit_Call(self, instruction):
        self.events.append(("call", len(instruction.args)))

    def visit_LoadAttribute(self, instruction):
        self.events.append(("get", instruction.name))

    def visit_StoreAttribute(self, instruction):
        self.events.append(("set", instruction.name))

    def visit_ResolveMethod(self, instruction):
        self.events.append(("resolve", instruction.name))

    def visit_CallMethod(self, instruction):
        self.events.append(("method-call", len(instruction.args)))


def instruction_trace(source):
    module = lower(source)
    trace = InstructionTrace()
    for block in module.functions[0].blocks:
        for instruction in block.instructions:
            instruction.accept(trace)
    return trace.events
