"""Accept Python annotations; recognized int contracts remain unbounded."""

import pytest

from mule import ast
from mule.compiler import emit_llvm
from mule.compiler.main import compile_source
from mule.compiler.node import CallKind, Combination
from test.mule.compiler.rewrite_support import build_case
from test.mule.compiler.test_annotations import contradiction_warnings
from test.mule.compiler.test_native import run_native
from test.mule.parser.ast_query import expect


@pytest.mark.parametrize("value", (0, -(2**63) - 1, 2**128, -(2**256)))
def test_builtin_int_argument_and_return_preserve_arbitrary_precision(tmp_path, value):
    result = run_native(
        tmp_path,
        "from mule_types import optimize\n"
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
        "from mule_types import optimize\n"
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
        "from mule_types import optimize\n"
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
    result = run_native(tmp_path, "from mule_types import optimize\n" + body)
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
        "from mule_types import optimize\n"
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
            marks=pytest.mark.integration,
        ),
        pytest.param(
            "from typing import Any as Value\n",
            "Value",
            marks=pytest.mark.integration,
        ),
        pytest.param(
            "from typing import Annotated\n",
            "Annotated[int, 'metadata']",
            marks=pytest.mark.integration,
        ),
        ("from __future__ import annotations\n", "UnknownClass | None"),
        ("import mule_types\n", "mule_types.intp"),
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
        "from mule_types import optimize\n@optimize(safety=3)\n"
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
        "import mule_types\n"
        + (f"@mule_types.optimize({policy})\n" if policy else "")
        + f"class Counter:\n value: mule_types.{marker}\n"
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


def test_builtin_int_annotation_specializes_arithmetic_and_elides_return_check():
    llvm = emit_llvm(
        "from mule_types import optimize\n"
        "@optimize(safety=3)\n"
        "def compute(x: int, y: int) -> int:\n"
        " masked = (x & 255) + (y & 255)\n"
        " return -(masked % y)\n"
    )
    body = llvm.split('define internal ptr @"mule_fn_compute.body"(')[1].split("\n}")[0]
    assert body.count("call i32 @mule_assert_integer(") == 2
    assert "mule_format_modulo" not in body
    assert "unary.protocol" not in body
    assert " = add i64 " in body


def test_declaration_recognition_covers_containers_unions_classes_and_varargs():
    compilation = compile_source(
        "class Point:\n"
        " x: int\n"
        " y: int\n"
        " def __init__(self, x: int, y: int):\n"
        "  self.x = x\n"
        "  self.y = y\n"
        "def f(\n"
        " b: bool,\n"
        " n: None,\n"
        " pair: tuple[int, str],\n"
        " nums: tuple[int, ...],\n"
        " mapping: dict[str, list[int]],\n"
        " nested: list[tuple[int, int]],\n"
        " maybe: int | None,\n"
        " pt: Point,\n"
        " *args: int,\n"
        " **kwargs: int,\n"
        ") -> tuple[int, int]:\n"
        " return (1, 2)\n"
        "print(f)\n"
    )
    fn = next(lam for lam in compilation.lambdas if lam.name == "mule_fn_f")
    assert {k: v.display_name for k, v in fn.local_annotations.items()} == {
        "b": "bool",
        "n": "None",
        "pair": "tuple[int, str]",
        "nums": "tuple[int, ...]",
        "mapping": "dict[str, list[int]]",
        "nested": "list[tuple[int, int]]",
        "maybe": "int | None",
        "pt": "Point",
        "args": "tuple[int, ...]",
        "kwargs": "dict[str, int]",
    }


def test_parameter_facts_specialize_callbacks_across_tuple_dict_nested_and_class_contracts(
    tmp_path,
):
    source = (
        "class Point:\n"
        " x: int\n"
        " y: int\n"
        " def __init__(self, x: int, y: int):\n"
        "  self.x = x\n"
        "  self.y = y\n"
        " def sum_coords(self) -> int:\n"
        "  return self.x + self.y\n"
        "\n"
        "def sum_pair(pair: tuple[int, int]) -> int:\n"
        " a, b = pair\n"
        " return a + b + pair[0]\n"
        "\n"
        "def sum_nested_pairs(pairs: list[tuple[int, int]]) -> int:\n"
        " total = 0\n"
        " for a, b in pairs:\n"
        "  total += a * b\n"
        " return total\n"
        "\n"
        "def sum_matrix(grid: list[list[int]]) -> int:\n"
        " total = 0\n"
        " for i in range(len(grid)):\n"
        "  row = grid[i]\n"
        "  for x in row:\n"
        "   total += x\n"
        " return total\n"
        "\n"
        "def lookup_add(d: dict[str, int], k: str) -> int:\n"
        " return d[k] + d.get(k, 0)\n"
        "\n"
        "def point_metric(p: Point, scale: int | None) -> int:\n"
        " base = p.x * p.y + p.sum_coords()\n"
        " if scale is not None:\n"
        "  return base * scale\n"
        " return base\n"
        "\n"
        "callbacks = [sum_pair, sum_nested_pairs, sum_matrix, lookup_add, point_metric]\n"
        "print(callbacks[0]((10, 20)))\n"
        "print(callbacks[1]([(2, 3), (4, 5)]))\n"
        "print(callbacks[2]([[1, 2], [3, 4]]))\n"
        "print(callbacks[3]({'a': 21}, 'a'))\n"
        "print(callbacks[4](Point(3, 4), 2))\n"
        "print(callbacks[4](Point(3, 4), None))\n"
    )
    llvm = emit_llvm(source)
    for fn_name in (
        "sum_pair",
        "sum_nested_pairs",
        "sum_matrix",
        "lookup_add",
        "point_metric",
    ):
        assert f'define internal ptr @"mule_fn_{fn_name}.spec.0"(' in llvm

    pair_spec = llvm.split('define internal ptr @"mule_fn_sum_pair.spec.0"(')[1].split(
        "\n}"
    )[0]
    assert "@mule.fast.tuple.slot(" in pair_spec
    assert "@mule_fast_unpack(" not in pair_spec
    assert "@llvm.sadd.with.overflow.i64(" in pair_spec

    nested_spec = llvm.split('define internal ptr @"mule_fn_sum_nested_pairs.spec.0"(')[
        1
    ].split("\n}")[0]
    assert "@mule.fast.next_unpack.2(" in nested_spec
    assert "@llvm.sadd.with.overflow.i64(" in nested_spec

    matrix_spec = llvm.split('define internal ptr @"mule_fn_sum_matrix.spec.0"(')[
        1
    ].split("\n}")[0]
    assert "getelementptr ptr, ptr " in matrix_spec
    assert "@mule_list_getitem(" not in matrix_spec
    assert "@llvm.sadd.with.overflow.i64(" in matrix_spec

    lookup_spec = llvm.split('define internal ptr @"mule_fn_lookup_add.spec.0"(')[
        1
    ].split("\n}")[0]
    assert '@"mule.protocol.dict_get"(' in lookup_spec
    assert "@llvm.sadd.with.overflow.i64(" in lookup_spec

    point_spec = llvm.split('define internal ptr @"mule_fn_point_metric.spec.0"(')[
        1
    ].split("\n}")[0]
    assert "@mule.attr.field(" in point_spec
    assert "mule.direct.mule_fn_Point.sum_coords" in point_spec
    assert "@llvm.sadd.with.overflow.i64(" in point_spec

    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "40\n26\n10\n42\n38\n19\n"


@pytest.mark.parametrize(
    "source",
    (
        (
            "def f(xs: list[int]) -> int:\n"
            " return xs[0] + 1\n"
            "bad = ['not-an-int']\n"
            "f(bad)\n"
        ),
        (
            "def mutate(alias):\n"
            " alias[0] = 'corrupted'\n"
            "def f(xs: list[int], cb) -> int:\n"
            " first = xs[0]\n"
            " cb(xs)\n"
            " return first + xs[0]\n"
            "data = [10]\n"
            "f(data, mutate)\n"
        ),
        (
            "def f(pairs: list[tuple[int, int]]) -> int:\n"
            " total = 0\n"
            " for a, b in pairs:\n"
            "  total += a + b\n"
            " return total\n"
            "bad = [(1, 'not-an-int')]\n"
            "f(bad)\n"
        ),
        (
            "def f(d: dict[str, int], k: str) -> int:\n"
            " return d[k] + 1\n"
            "bad = {'k': 'not-an-int'}\n"
            "f(bad, 'k')\n"
        ),
    ),
)
def test_container_element_contracts_guard_elements_against_bad_inputs_and_alias_mutation(
    tmp_path, source
):
    result = run_native(tmp_path, source)
    assert result.returncode != 0
    assert "TypeError" in result.stderr


def test_safety_zero_retains_nested_element_types_without_contract_checks(tmp_path):
    source = """\
from mule_types import int64, optimize

@optimize(speed=3, safety=0, debug=0)
def dot(data: tuple[list[list[int]], list[list[int]]]) -> int:
    left, right = data
    total: int64 = 0
    for i in range(len(left)):
        row_a = left[i]
        row_b = right[i]
        for k in range(len(row_a)):
            total += row_a[k] * row_b[k]
    return total

value = int(input())
callbacks = [dot]
print(callbacks[0](([[value, 2], [3, 4]], [[5, 6], [7, value]])))
"""
    llvm = emit_llvm(source)
    specialized = llvm.split('define internal ptr @"mule_fn_dot.spec.0"(')[1].split(
        "\n}"
    )[0]
    assert "@mule_list_getitem_borrow(" in specialized
    assert "list.read" in specialized
    assert "@mule_fast_getitem(" not in specialized
    assert "@mule_assert_" not in specialized
    assert "@mule_integer_in_bounds(" not in specialized
    assert "getelementptr i8, ptr" not in specialized

    case = build_case(tmp_path, source, optimize=2)
    optimized = case.function_ir(r"^mule_fn_dot\.spec\.0$")
    assert "@mule_list_getitem_borrow(" in optimized
    assert "@mule_fast_getitem(" not in optimized
    assert "@mule_assert_" not in optimized
    assert "@mule_integer_in_bounds(" not in optimized
    for value in ("3\n", "-7\n"):
        case.compare(value)
