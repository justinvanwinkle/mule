"""Execute hand-built IR graphs through the real LLVM/runtime boundary."""

import shutil
import subprocess

import pytest

from mule import ast
from mule.compiler import ir
from mule.compiler.driver import link_llvm
from mule.compiler.llvm import emit


@pytest.fixture
def execute(tmp_path):
    clang = shutil.which("clang")
    if clang is None:
        pytest.skip("LLVM emitter execution tests require clang")

    def run(module):
        executable = tmp_path / "program"
        link_llvm(emit(module), executable, clang=clang)
        return subprocess.run(
            [str(executable)], capture_output=True, text=True, check=False
        )

    return run


def printing(instructions, value="value", *, locals_=()):
    return ir.Module(
        [
            ir.Function(
                "module",
                (),
                locals_,
                [
                    ir.Block(
                        "entry",
                        [
                            *instructions,
                            ir.Load("print", "print", False),
                            ir.Call("returned", "print", (value,)),
                        ],
                        ir.Return("returned"),
                    )
                ],
            )
        ],
        "module",
    )


@pytest.mark.parametrize(
    ("value", "output"),
    [
        (None, "None\n"),
        (True, "True\n"),
        (-(10**100), "-" + "1" + "0" * 100 + "\n"),
        (-0.0, "-0.0\n"),
        ("π\x00雪", "π\x00雪\n"),
        (b"\x00\xff", "b'\\x00\\xff'\n"),
    ],
    ids=["none", "bool", "huge-integer", "negative-zero", "unicode-nul", "bytes"],
)
def test_native_constants_preserve_values(execute, value, output):
    result = execute(printing([ir.Constant("value", value)]))
    assert result.returncode == 0, result.stderr
    assert result.stdout == output


def test_native_operators_and_clear_load_transfer_references(execute):
    module = printing(
        [
            ir.Constant("left", 7),
            ir.Constant("right", 3),
            ir.Binary("product", ast.BinaryOperator.MULTIPLY, "left", "right"),
            ir.Store("merge", "product", True),
            ir.Load("moved", "merge", True, clear=True),
            ir.Unary("negative", ast.UnaryOperator.NEGATIVE, "moved"),
            ir.Constant("expected", -21),
            ir.Compare("value", ast.ComparisonOperator.EQUAL, "negative", "expected"),
        ],
        locals_=("merge",),
    )
    result = execute(module)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "True\n"


def test_native_integer_constant_exceeds_host_decimal_conversion_limit(execute):
    module = printing(
        [
            ir.Constant("huge", 1 << 20000),
            ir.Constant("zero", 0),
            ir.Compare("value", ast.ComparisonOperator.GREATER, "huge", "zero"),
        ]
    )
    result = execute(module)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "True\n"


def test_native_text_constants_preserve_lone_surrogates(execute):
    module = printing(
        [
            ir.Constant("surrogate", "\ud800"),
            ir.Load("ascii", "ascii", False),
            ir.Call("value", "ascii", ("surrogate",)),
        ]
    )
    result = execute(module)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "'\\ud800'\n"


def test_native_functions_bind_arguments_and_see_current_globals(execute):
    function = ir.Function(
        "function",
        ("x",),
        ("x",),
        [
            ir.Block(
                "entry",
                [
                    ir.Load("x", "x", True),
                    ir.Load("global", "offset", False),
                    ir.Binary("sum", ast.BinaryOperator.ADD, "x", "global"),
                ],
                ir.Return("sum"),
            )
        ],
    )
    module = printing(
        [
            ir.Constant("first", 1),
            ir.Store("offset", "first", False),
            ir.DefineFunction("function", "function", "add_offset"),
            ir.Constant("second", 40),
            ir.Store("offset", "second", False),
            ir.Constant("argument", 2),
            ir.Call("value", "function", ("argument",)),
        ]
    )
    module.functions.append(function)
    result = execute(module)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "42\n"


def test_native_error_cleanup_preserves_unbound_local_exception(execute):
    module = printing([ir.Load("value", "missing", True)], locals_=("missing",))
    result = execute(module)
    assert result.returncode == 1
    assert result.stdout == ""
    assert "UnboundLocalError" in result.stderr
    assert "missing" in result.stderr


@pytest.mark.parametrize("truth", [False, True], ids=["false-edge", "true-edge"])
def test_native_branches_transfer_selected_value_through_local(execute, truth):
    module = ir.Module(
        [
            ir.Function(
                "module",
                (),
                ("merge",),
                [
                    ir.Block(
                        "entry",
                        [ir.Constant("condition", truth)],
                        ir.Branch("condition", "yes", "no"),
                    ),
                    ir.Block(
                        "yes",
                        [ir.Constant("yes", 11), ir.Store("merge", "yes", True)],
                        ir.Jump("join"),
                    ),
                    ir.Block(
                        "no",
                        [ir.Constant("no", 22), ir.Store("merge", "no", True)],
                        ir.Jump("join"),
                    ),
                    ir.Block(
                        "join",
                        [
                            ir.Load("value", "merge", True, clear=True),
                            ir.Load("print", "print", False),
                            ir.Call("printed", "print", ("value",)),
                            ir.Discard("printed"),
                            ir.Constant("return", None),
                        ],
                        ir.Return("return"),
                    ),
                ],
            )
        ],
        "module",
    )
    result = execute(module)
    assert result.returncode == 0, result.stderr
    assert result.stdout == ("11\n" if truth else "22\n")
