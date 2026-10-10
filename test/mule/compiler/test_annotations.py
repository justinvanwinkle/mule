"""Exercise imported compiler annotation contracts through native execution."""

import runpy
from pathlib import Path

import pytest

from mule.compiler import emit_llvm, lower
from test.mule.compiler.test_native import run_native

SIGNED = tuple((f"int{bits}", bits) for bits in (8, 16, 32, 64, 128))
UNSIGNED = tuple((f"uint{bits}", bits) for bits in (8, 16, 32, 64, 128))
FLOATS = ("float16", "float32", "float64", "float128")


@pytest.mark.parametrize(("marker", "bits"), SIGNED + UNSIGNED)
def test_integer_annotation_boundaries_preserve_values(tmp_path, marker, bits):
    unsigned = marker.startswith("uint")
    minimum = 0 if unsigned else -(2 ** (bits - 1))
    maximum = 2 ** (bits if unsigned else bits - 1) - 1
    result = run_native(
        tmp_path,
        f"from mule import {marker}\n"
        f"def identity(value: {marker}) -> {marker}:\n return value\n"
        f"first: {marker} = {minimum}\n"
        f"last: {marker} = {maximum}\n"
        "print(identity(first), identity(last))\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == f"{minimum} {maximum}\n"


@pytest.mark.parametrize("marker", FLOATS)
def test_float_annotation_accepts_float_values(tmp_path, marker):
    result = run_native(
        tmp_path,
        f"import mule\ndef twice(value: mule.{marker}) -> mule.{marker}:\n"
        " return value + value\nprint(twice(1.25))\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "2.5\n"


@pytest.mark.parametrize(
    "source",
    [
        "import mule as m\ndef add(x: m.int32) -> m.int32:\n return x + 1\nprint(add(41))\n",
        "from mule import int32 as Number\ndef add(x: Number) -> Number:\n return x + 1\nprint(add(41))\n",
        "from mule import int32\ndef add(x: int32) -> int32:\n y: int32\n y = x + 1\n return y\nprint(add(41))\n",
        "from mule import int32\nx: int32\nx = 42\nprint(x)\n",
        "from mule import int32, optimize as policy\n@policy(speed=3, safety=3, debug=0)\ndef add(x: int32) -> int32:\n return x + 1\nprint(add(41))\n",
        "import mule as m\n@m.optimize(safety=0)\ndef add(x: m.int32) -> m.int32:\n return x + 1\nprint(add(41))\n",
    ],
)
def test_imported_aliases_and_declarations_compile(tmp_path, source):
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "42\n"


@pytest.mark.parametrize(("marker", "bits"), SIGNED + UNSIGNED)
@pytest.mark.parametrize("boundary", ("below", "above"))
def test_integer_annotation_rejects_statically_out_of_range_values(
    marker, bits, boundary
):
    unsigned = marker.startswith("uint")
    minimum = 0 if unsigned else -(2 ** (bits - 1))
    maximum = 2 ** (bits if unsigned else bits - 1) - 1
    wrong = minimum - 1 if boundary == "below" else maximum + 1
    source = f"from mule import {marker}\nvalue: {marker} = {wrong}\n"
    with pytest.raises(SyntaxError) as caught:
        emit_llvm(source, "range_error.py")
    assert caught.value.filename == "range_error.py"
    assert caught.value.lineno == 2


@pytest.mark.parametrize(
    "body",
    [
        "value: int32 = 'wrong'\n",
        "value: int32 = 1.25\n",
        "value: int32 = 1\nvalue = 'wrong'\n",
        "def bad() -> int32:\n return 'wrong'\nbad()\n",
        "def bad() -> int32:\n pass\nbad()\n",
        "def identity(value: int32):\n return value\nidentity('wrong')\n",
        "def bad():\n value: int32\n value = 'wrong'\nbad()\n",
    ],
)
def test_statically_wrong_annotations_are_compilation_errors(body):
    with pytest.raises(SyntaxError):
        emit_llvm("from mule import int32\n" + body, "wrong_annotation.py")


@pytest.mark.parametrize(
    ("declaration_before_writer", "write_line"), [(True, 5), (False, 4)]
)
def test_module_annotation_rejects_wrong_global_write_in_either_definition_order(
    declaration_before_writer, write_line
):
    declaration = "value: int32 = 1\n"
    writer = "def bad():\n global value\n value = 'wrong'\n"
    source = "from mule import int32\n"
    source += (
        declaration + writer if declaration_before_writer else writer + declaration
    )
    source += "bad()\n"
    with pytest.raises(SyntaxError) as caught:
        emit_llvm(source, "global_write.py")
    assert caught.value.filename == "global_write.py"
    assert caught.value.lineno == write_line
    assert caught.value.offset == 2
    assert caught.value.text == " value = 'wrong'\n"


@pytest.mark.parametrize("declaration_before_writer", (True, False))
def test_module_annotation_checks_unknown_global_write_in_either_definition_order(
    tmp_path, declaration_before_writer
):
    declaration = "value: int32 = 1\n"
    writer = "@optimize(safety=3)\ndef bad():\n global value\n value = str(42)\n"
    source = "from mule import int32, optimize\n"
    source += (
        declaration + writer if declaration_before_writer else writer + declaration
    )
    source += "bad()\n"
    result = run_native(tmp_path, source)
    assert result.returncode != 0
    assert "TypeError" in result.stderr


@pytest.mark.parametrize("marker", FLOATS)
def test_float_annotation_rejects_statically_wrong_object(marker):
    with pytest.raises(SyntaxError):
        emit_llvm(f"from mule import {marker}\nvalue: {marker} = 'wrong'\n")


@pytest.mark.parametrize("safety", (0, 1, 2, 3))
def test_known_wrong_annotation_is_rejected_at_every_safety(safety):
    source = (
        "from mule import int32, optimize\n"
        f"@optimize(safety={safety})\ndef bad() -> int32:\n return 'wrong'\n"
    )
    with pytest.raises(SyntaxError):
        emit_llvm(source)


@pytest.mark.parametrize("marker", ("int32", "uint128"))
@pytest.mark.parametrize("value", ("True", "False"))
def test_fixed_integer_annotation_rejects_known_boolean(marker, value):
    with pytest.raises(SyntaxError):
        emit_llvm(f"from mule import {marker}\nvalue: {marker} = {value}\n")


def test_uninitialized_annotated_local_still_raises_unbound_local_error(tmp_path):
    result = run_native(
        tmp_path,
        "from mule import int32\ndef read():\n value: int32\n return value\nread()\n",
    )
    assert result.returncode != 0
    assert "UnboundLocalError" in result.stderr


@pytest.mark.parametrize(
    ("policy", "attributes"),
    [("speed=0", "noinline optnone"), ("debug=3", "noinline")],
)
def test_policy_is_applied_to_native_function_definition(policy, attributes):
    llvm = emit_llvm(
        "from mule import optimize\n"
        f"@optimize({policy})\ndef identity(value):\n return value\n"
    )
    definitions = [line for line in llvm.splitlines() if line.startswith("define ")]
    assert any(line.endswith(f") {attributes} {{") for line in definitions)


@pytest.mark.parametrize("policy", ("", "@optimize(safety=0)\n"))
def test_trusted_annotations_do_not_emit_runtime_annotation_checks(policy):
    llvm = emit_llvm(
        "from mule import int32, optimize\n"
        f"{policy}def identity(value: int32) -> int32:\n return value\n"
    )
    assert "call i32 @mule_assert_integer(" not in llvm


def test_checked_annotation_emits_check_for_unknown_parameter():
    llvm = emit_llvm(
        "from mule import int32, optimize\n"
        "@optimize(safety=3)\ndef identity(value: int32) -> int32:\n return value\n"
    )
    assert "call i32 @mule_assert_integer(" in llvm


@pytest.mark.parametrize("safety", (1, 2))
def test_intermediate_safety_levels_check_unknown_annotation_values(tmp_path, safety):
    result = run_native(
        tmp_path,
        "from mule import int32, optimize\n"
        f"@optimize(safety={safety})\ndef consume(value: int32):\n print(value)\n"
        "consume(str(42))\n",
    )
    assert result.returncode != 0
    assert "TypeError" in result.stderr


@pytest.mark.parametrize(
    "source",
    [
        "from mule import int32, optimize\n@optimize(safety=3)\ndef consume(value: int32):\n print(value)\nconsume(str(42))\n",
        "from mule import int32, optimize\n@optimize(safety=3)\ndef consume() -> int32:\n return str(42)\nprint(consume())\n",
        "from mule import int32, optimize\n@optimize(safety=3)\ndef consume():\n value: int32 = str(42)\n print(value)\nconsume()\n",
    ],
)
def test_safety_three_checks_unknown_annotation_values(tmp_path, source):
    # Generic builtin calls currently have unknown return facts.
    result = run_native(tmp_path, source)
    assert result.returncode != 0
    assert "TypeError" in result.stderr


@pytest.mark.parametrize(
    ("class_policy", "method_policy"),
    [
        ("@optimize(safety=3)", ""),
        ("@optimize(safety=0)", " @optimize(safety=3)\n"),
    ],
)
def test_class_policy_and_method_override_check_annotations(
    tmp_path, class_policy, method_policy
):
    result = run_native(
        tmp_path,
        "from mule import int32, optimize\n"
        f"{class_policy}\nclass Consumer:\n"
        f"{method_policy}"
        " def consume(self, value: int32):\n  print(value)\n"
        "Consumer().consume(str(42))\n",
    )
    assert result.returncode != 0
    assert "TypeError" in result.stderr


@pytest.mark.parametrize(
    ("marker", "value"),
    [
        ("int8", -128),
        ("uint8", 255),
        ("int128", -(2**127)),
        ("int128", 2**127 - 1),
        ("uint128", 2**128 - 1),
    ],
)
def test_safety_three_accepts_unknown_integer_within_range(tmp_path, marker, value):
    result = run_native(
        tmp_path,
        f"from mule import {marker}, optimize\n"
        f"@optimize(safety=3)\ndef consume(value: {marker}) -> {marker}:\n"
        " return value\n"
        f"print(consume(int('{value}')))\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == f"{value}\n"


@pytest.mark.parametrize(
    ("marker", "value"),
    [
        ("int8", -129),
        ("int8", 128),
        ("uint8", -1),
        ("uint8", 256),
        ("int128", -(2**127) - 1),
        ("int128", 2**127),
        ("uint128", 2**128),
    ],
)
def test_safety_three_rejects_unknown_integer_outside_range(tmp_path, marker, value):
    result = run_native(
        tmp_path,
        f"from mule import {marker}, optimize\n"
        f"@optimize(safety=3)\ndef consume(value: {marker}):\n print(value)\n"
        f"consume(int('{value}'))\n",
    )
    assert result.returncode != 0
    assert "TypeError" in result.stderr


@pytest.mark.parametrize(
    "source",
    [
        "from mule import intp\n",
        "from mule import uintp\n",
        "from mule import isize\n",
        "from mule import usize\n",
        "from mule import int256\n",
        "from mule import *\n",
        "from mule import int32\nint32 = 42\n",
        "from mule import int32\ndef f(int32):\n pass\n",
        "import mule\nmule = 42\n",
        "import mule\nprint(mule.int32)\n",
        "from mule import int32\nprint(int32)\n",
        "from mule import int32\nprint(int32(42))\n",
        "if True:\n from mule import int32\n",
        "from mule import optimize\n@optimize(3)\ndef f():\n pass\n",
        "from mule import optimize\n@optimize(safety=4)\ndef f():\n pass\n",
        "from mule import optimize\n@optimize(safety=-1)\ndef f():\n pass\n",
        "from mule import optimize\n@optimize(safety=1.0)\ndef f():\n pass\n",
        "from mule import optimize\n@optimize(safety=3, safety=0)\ndef f():\n pass\n",
        "from mule import optimize\n@optimize(unknown=3)\ndef f():\n pass\n",
        "from mule import optimize\nlevel = 3\n@optimize(safety=level)\ndef f():\n pass\n",
    ],
)
def test_invalid_compiler_imports_and_directives_are_rejected(source):
    with pytest.raises(SyntaxError):
        lower(source)


def library_namespace():
    return runpy.run_path(str(Path(__file__).parents[3] / "stdlib/mule.py"))


def test_host_library_exports_annotation_markers_and_identity_policy():
    library = library_namespace()
    expected = {marker for marker, _ in SIGNED + UNSIGNED} | set(FLOATS) | {"optimize"}
    assert set(library["__all__"]) == expected
    for marker in expected - {"optimize"}:
        assert library[marker].__name__ == marker

    def identity(value):
        return value

    assert library["optimize"]()(identity) is identity
    assert library["optimize"](speed=0, safety=3, debug=3)(identity) is identity
    assert library["optimize"](speed=3, safety=0, debug=0)(identity)(42) == 42


@pytest.mark.parametrize("option", ("speed", "safety", "debug"))
@pytest.mark.parametrize("level", (-1, 4))
def test_host_policy_rejects_levels_outside_zero_to_three(option, level):
    with pytest.raises(ValueError):
        library_namespace()["optimize"](**{option: level})


def test_host_policy_rejects_noninteger_level():
    with pytest.raises(TypeError):
        library_namespace()["optimize"](safety=1.0)
