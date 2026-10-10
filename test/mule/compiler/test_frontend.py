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
        "class C:\n pass\n",
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
    reads = [
        instruction
        for block in function.blocks
        for instruction in block.instructions
        if isinstance(instruction, ir.Load)
    ]
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
