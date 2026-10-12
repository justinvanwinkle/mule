"""Accept Python annotations; recognized int contracts remain unbounded."""

import pytest

from mule import ast
from mule.compiler import emit_llvm
from mule.compiler.main import compile_source
from mule.compiler.node import CallKind, Combination
from test.mule.compiler.test_annotations import contradiction_warnings
from test.mule.compiler.test_native import run_native
from test.mule.parser.ast_query import expect


@pytest.mark.parametrize("value", (0, -(2**63) - 1, 2**128, -(2**256)))
def test_builtin_int_argument_and_return_preserve_arbitrary_precision(tmp_path, value):
    result = run_native(
        tmp_path,
        "from mule import optimize\n"
        "@optimize(safety=3)\n"
        "def identity(value: int) -> int:\n return value\n"
        f"print(identity(int('{value}')))\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == f"{value}\n"


def test_builtin_int_module_and_local_declarations_track_reassignments(tmp_path):
    value = 2**200
    result = run_native(
        tmp_path,
        f"value: int = {value}\n"
        "value = -value\n"
        "def calculate(argument: int) -> int:\n"
        " result: int\n result = argument\n result += argument\n return result\n"
        "print(value, calculate(value))\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == f"{-value} {-2 * value}\n"


def test_builtin_int_arithmetic_grows_beyond_machine_integer(tmp_path):
    value = 2**63 - 1
    result = run_native(
        tmp_path,
        "from mule import optimize\n"
        "@optimize(speed=3, safety=3)\n"
        "def square(value: int) -> int:\n result: int = value * value\n return result + 1\n"
        f"print(square(int('{value}')))\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == f"{value * value + 1}\n"


@pytest.mark.parametrize(
    ("value", "expected"),
    (
        ("True", "True True 3 3\n"),
        ("False", "False True 2 0\n"),
        ("bool(42)", "True True 3 3\n"),
    ),
)
def test_builtin_int_accepts_bool_without_replacing_the_object(
    tmp_path, value, expected
):
    result = run_native(
        tmp_path,
        "from mule import optimize\n"
        "@optimize(safety=3)\n"
        "def identity(value: int) -> int:\n return value\n"
        f"original = {value}\n"
        "result: int = identity(original)\n"
        "print(result, result is original, result + 2, result * 3)\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == expected


@pytest.mark.parametrize(
    "body",
    (
        "@optimize(safety=3)\ndef consume(value: int):\n print(value)\nconsume(str(42))\n",
        "@optimize(safety=3)\ndef consume() -> int:\n return str(42)\nprint(consume())\n",
        "@optimize(safety=3)\ndef consume():\n value: int = 1\n value = str(42)\nconsume()\n",
    ),
)
def test_checked_builtin_int_rejects_unknown_non_integer(tmp_path, body):
    result = run_native(tmp_path, "from mule import optimize\n" + body)
    assert result.returncode != 0
    assert "TypeError" in result.stderr


@pytest.mark.parametrize(
    ("source", "line"),
    (
        ("value: int = 'wrong'\n", 1),
        ("value: int = 1\nvalue = 1.5\n", 2),
        ("def bad() -> int:\n return 'wrong'\n", 2),
        ("def bad():\n value: int\n value = None\n", 3),
    ),
)
def test_known_builtin_int_contradictions_warn_with_source_location(
    caplog, source, line
):
    warnings = contradiction_warnings(caplog, source, "builtin_annotation.py")
    assert any(
        warning.startswith(f"builtin_annotation.py:{line}:") for warning in warnings
    )


def test_trusted_builtin_int_does_not_emit_runtime_category_checks():
    llvm = emit_llvm(
        "from mule import optimize\n"
        "@optimize(safety=0)\ndef identity(value: int) -> int:\n return value\n"
    )
    assert "call i32 @mule_assert_" not in llvm


@pytest.mark.parametrize(
    "annotation",
    (
        "MissingType",
        "'ForwardReference'",
        "bool",
        "str",
        "bytes",
        "object",
        "None",
        "list[int]",
        "dict[str, int]",
        "int | str",
        "int | None",
        "typing.Annotated[int, 'metadata']",
        "UserClass",
        "missing_factory()",
    ),
)
def test_arbitrary_annotations_are_accepted_without_evaluation(tmp_path, annotation):
    # Definitions are lazy in Python 3.14, and local annotations never evaluate.
    # Neither missing names nor annotation-only generic/union operations execute.
    source = (
        "class UserClass:\n pass\n"
        f"module_value: {annotation} = 20\n"
        f"def add(value: {annotation}) -> {annotation}:\n"
        f" local_value: {annotation}\n"
        " local_value = value + 1\n return local_value\n"
        "print(add(module_value))\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "21\n"


@pytest.mark.parametrize(
    "source",
    (
        "int = 42\nvalue: int = 'opaque'\nprint(value)\n",
        "int = 42\ndef identity(value: int) -> int:\n return value\nprint(identity('opaque'))\n",
        "def consume():\n int = 42\n value: int = 'opaque'\n return value\nprint(consume())\n",
    ),
)
def test_shadowed_builtin_int_is_an_opaque_annotation(tmp_path, source):
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "opaque\n"


@pytest.mark.parametrize("annotation", ("MissingType", "int", "missing_factory()"))
def test_ordinary_class_annotations_are_lazy_and_do_not_select_raw_fields(
    tmp_path, annotation
):
    result = run_native(
        tmp_path,
        "class Counter:\n"
        f" value: {annotation}\n"
        " def __init__(self):\n  self.value = 40\n"
        " def add(self):\n  self.value += 2\n"
        "counter = Counter()\ncounter.add()\nprint(counter.value)\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "42\n"


@pytest.mark.parametrize("annotation", ("MissingType", "int", "missing_factory()"))
def test_annotated_attribute_assignment_executes_value_but_not_annotation(
    tmp_path, annotation
):
    result = run_native(
        tmp_path,
        "class Counter:\n"
        f" def __init__(self):\n  self.value: {annotation} = 42\n"
        "print(Counter().value)\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "42\n"


@pytest.mark.parametrize(
    "imports, annotation",
    (
        pytest.param(
            "import typing as t\n",
            "t.Optional[int]",
            marks=[
                pytest.mark.integration,
                pytest.mark.xfail(
                    strict=True,
                    raises=SyntaxError,
                    reason="Compiling typing requires general class bodies (abc)",
                ),
            ],
        ),
        pytest.param(
            "from typing import Any as Value\n",
            "Value",
            marks=[
                pytest.mark.integration,
                pytest.mark.xfail(
                    strict=True,
                    raises=SyntaxError,
                    reason="Compiling typing requires general class bodies (abc)",
                ),
            ],
        ),
        pytest.param(
            "from typing_extensions import Annotated\n",
            "Annotated[int, 'metadata']",
            marks=pytest.mark.xfail(
                strict=True,
                raises=AssertionError,
                reason="typing_extensions is not a bundled library; importing "
                "it raises ModuleNotFoundError at run time",
            ),
        ),
        ("from __future__ import annotations\n", "UnknownClass | None"),
        ("import mule\n", "mule.intp"),
    ),
)
def test_annotation_imports_accept_lazy_expressions(tmp_path, imports, annotation):
    result = run_native(
        tmp_path,
        imports + f"def identity(value: {annotation}) -> {annotation}:\n"
        " return value\nprint(identity(42))\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "42\n"


def test_builtin_float_contract_uses_checked_float_category(tmp_path):
    result = run_native(
        tmp_path,
        "from mule import optimize\n@optimize(safety=3)\n"
        "def twice(value: float) -> float:\n return value + value\n"
        "print(twice(float('1.25')))\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "2.5\n"


@pytest.mark.parametrize(
    "policy, marker", (("", "int64"), ("speed=2", "int64"), ("speed=3", "int32"))
)
def test_class_annotations_select_boxed_storage_when_not_native(
    tmp_path, policy, marker
):
    result = run_native(
        tmp_path,
        "import mule\n"
        + (f"@mule.optimize({policy})\n" if policy else "")
        + f"class Counter:\n value: mule.{marker}\n"
        " def __init__(self):\n  self.value = 42\nprint(Counter().value)\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "42\n"


def test_bare_attribute_and_subscript_annotations_evaluate_target_prefixes(tmp_path):
    result = run_native(
        tmp_path,
        "def prefix():\n print(1)\n return 0\n"
        "def index():\n print(2)\n return 0\n"
        "prefix().missing: nonexistent()\n"
        "prefix()[index()]: nonexistent()\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "1\n1\n2\n"


def test_local_annotation_does_not_count_as_runtime_global_use(tmp_path):
    result = run_native(
        tmp_path,
        "def calculate():\n local: T\n global T\n T = 42\n return T\n"
        "print(calculate())\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "42\n"


def test_ir_retains_annotation_expressions_separately_from_contracts():
    compilation = compile_source(
        "module_value: Unknown[int] = 0\n"
        "def identity(value: 'Later') -> list[int]:\n local: bool\n return value\n"
        "class C:\n field: Custom | None\n"
    )
    function = compilation.lambdas[1]
    signature = function.signature
    assert signature.annotation_expressions[0].value == "Later"
    return_annotation = expect(signature.return_annotation_expression, ast.Subscript)
    assert expect(return_annotation.value, ast.Name).identifier == "list"
    assert (
        expect(function.annotation_expressions["local"], ast.Name).identifier == "bool"
    )
    assert signature.annotations == (None,)
    assert signature.return_annotation is None
    entry = compilation.lambdas[0]
    module_annotation = expect(
        entry.annotation_expressions["module_value"], ast.Subscript
    )
    assert expect(module_annotation.value, ast.Name).identifier == "Unknown"
    assert entry.component is not None
    definitions = [
        node.info
        for block in entry.component.blocks()
        for node in block.nodes()
        if isinstance(node, Combination)
        and node.kind is CallKind.KNOWN
        and node.fun_info.name == "%make_class"
    ]
    assert definitions[0].annotation_expressions[0][0] == "field"
    assert definitions[0].annotation_expressions[0][1].left.identifier == "Custom"
