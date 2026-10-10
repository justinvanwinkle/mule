"""Accept Python annotations; recognized int contracts remain unbounded."""

import pytest

from mule.compiler import emit_llvm
from test.mule.compiler.test_native import run_native


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
    ("source", "line", "text"),
    (
        ("value: int = 'wrong'\n", 1, "value: int = 'wrong'\n"),
        ("value: int = 1\nvalue = 1.5\n", 2, "value = 1.5\n"),
        (
            "def bad() -> int:\n return 'wrong'\n",
            2,
            " return 'wrong'\n",
        ),
        (
            "def bad():\n value: int\n value = None\n",
            3,
            " value = None\n",
        ),
    ),
)
def test_known_builtin_int_contradictions_are_source_located(source, line, text):
    with pytest.raises(SyntaxError, match="contradicts annotation") as caught:
        emit_llvm(source, "builtin_annotation.py")
    assert caught.value.filename == "builtin_annotation.py"
    assert caught.value.lineno == line
    assert caught.value.text == text


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
