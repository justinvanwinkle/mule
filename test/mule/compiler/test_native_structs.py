"""Exercise opt-in classes with native int64 storage and method arguments."""

import re

import pytest

from mule.compiler import emit_llvm
from mule.compiler.main import compile_source as lower
from test.mule.compiler.test_native import run_native

COUNTER = (
    "from mule import int64, optimize\n"
    "@optimize(speed=3, safety=0)\n"
    "class Counter:\n"
    " value: int64\n"
    " def __init__(self, start: int64):\n  self.value = start\n"
    " def add(self, amount: int64):\n  self.value += amount\n"
    " def read(self) -> int64:\n  return self.value\n"
)


def test_native_counter_uses_int64_fields_and_direct_method_entry():
    llvm = emit_llvm(COUNTER)
    native_functions = re.findall(
        r"^define internal ([^\n]+@mule\.native\.\d+\([^\n]+) \{\n(.*?)^\}",
        llvm,
        re.MULTILINE | re.DOTALL,
    )
    updates = [
        (signature, body)
        for signature, body in native_functions
        if re.search(r"\badd(?: nsw)? i64\b", body)
    ]
    assert updates, "Counter.add must have a native integer entry"
    signature, body = updates[0]
    assert "i64 " in signature, "The method amount must use a machine integer argument"
    assert "load i64, ptr " in body
    assert "store i64 " in body
    for helper in (
        "mule_binary",
        "mule_get_attr",
        "mule_set_attr",
        "mule_resolve_method",
        "mule_call_resolved",
        "mule_literal_int",
        "mule_assert_integer",
    ):
        assert not re.search(rf"\bcall\b[^\n]*@{helper}\w*\(", body)


def test_observed_scalar_call_context_keeps_native_class_entry():
    source = (
        COUNTER + "def count(limit: int64) -> int64:\n"
        " counter = Counter(0)\n index: int64 = 0\n"
        " while index < limit:\n  counter.add(3)\n  index += 1\n"
        " return counter.value\nprint(count(100))\n"
    )
    llvm = emit_llvm(source)
    variants = re.findall(
        r"^define internal ptr @mule\.body\.\d+\.spec\.\d+\([^\n]+\) \{\n(.*?)^\}",
        llvm,
        re.MULTILINE | re.DOTALL,
    )
    guarded = [body for body in variants if "@mule_native_class_matches(" in body]
    assert guarded, "The observed count argument must retain a guarded native entry"
    assert all(re.search(r"call i64 @mule\.native\.\d+\(", body) for body in guarded)


def test_debug_three_prevents_native_method_body_inlining():
    llvm = emit_llvm(COUNTER.replace("safety=0", "safety=0, debug=3"))
    definitions = [
        line
        for line in llvm.splitlines()
        if line.startswith("define internal ") and "@mule.native." in line
    ]
    assert definitions
    assert all(line.endswith(" noinline {") for line in definitions)


@pytest.mark.parametrize("value", (-(2**63), -(2**62) - 1, 0, 2**62, 2**63 - 1))
def test_native_int64_field_preserves_full_width_values(tmp_path, value):
    result = run_native(
        tmp_path,
        COUNTER + f"counter = Counter({value})\nprint(counter.value, counter.read())\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == f"{value} {value}\n"


def test_native_fields_preserve_instance_aliasing_and_independent_storage(tmp_path):
    result = run_native(
        tmp_path,
        COUNTER + "first = Counter(10)\nsecond = Counter(20)\nalias = first\n"
        "alias.add(3)\nprint(first.read(), second.read(), alias is first)\n"
        "second.value = -7\nprint(first.value, second.value)\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "13 20 True\n13 -7\n"


def test_native_class_multiple_fields_keep_distinct_offsets(tmp_path):
    source = (
        "import mule as m\n@m.optimize(speed=3, safety=0)\nclass Pair:\n"
        " left: m.int64\n right: m.int64\n"
        " def __init__(self, left: m.int64, right: m.int64):\n"
        "  self.left = left\n  self.right = right\n"
        " def move(self, amount: m.int64):\n"
        "  self.left += amount\n  self.right -= amount\n"
        " def difference(self) -> m.int64:\n  return self.left - self.right\n"
        "pair = Pair(11, 4)\npair.move(3)\n"
        "print(pair.left, pair.right, pair.difference())\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "14 1 13\n"


def test_native_methods_return_none_and_integer_results(tmp_path):
    result = run_native(
        tmp_path,
        COUNTER + "counter = Counter(5)\nprint(counter.add(-2), counter.read())\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "None 3\n"


def test_unannotated_native_method_return_propagates_through_forward_call(tmp_path):
    source = (
        "from mule import int64, optimize\n"
        "@optimize(speed=3, safety=0)\nclass C:\n value: int64\n"
        " def __init__(self):\n  self.value = 7\n"
        " def first(self):\n  return self.second()\n"
        " def second(self):\n  return self.value\n"
        "@optimize(speed=3, safety=0)\ndef run():\n return C().first()\n"
        "print(run())\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "7\n"


def test_native_int64_method_arithmetic_and_comparison_loop(tmp_path):
    source = (
        "from mule import int64, optimize\n"
        "@optimize(speed=3, safety=0)\nclass Calculator:\n value: int64\n"
        " def __init__(self):\n  self.value = -3\n"
        " def transform(self, amount: int64) -> int64:\n"
        "  self.value = (self.value * amount) ^ 5\n"
        "  return self.value\n"
        "@optimize(speed=3, safety=0)\ndef calculate(limit: int64) -> int64:\n"
        " calculator = Calculator()\n index: int64 = 0\n"
        " while index < limit:\n  calculator.transform(-2)\n  index += 1\n"
        " return calculator.value\nprint(calculate(int('4')))\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "-9\n"


@pytest.mark.parametrize("value", ("'wrong'", "1.25", str(-(2**63) - 1), str(2**63)))
def test_native_field_rejects_known_wrong_constructor_assignment(value):
    source = (
        "from mule import int64, optimize\n@optimize(speed=3, safety=0)\n"
        f"class Bad:\n value: int64\n def __init__(self):\n  self.value = {value}\n"
        "Bad()\n"
    )
    with pytest.raises(SyntaxError) as caught:
        emit_llvm(source, "native_field.py")
    assert caught.value.filename == "native_field.py"
    assert caught.value.lineno == 6
    assert caught.value.text == f"  self.value = {value}\n"


@pytest.mark.parametrize(
    "value", ("str(42)", "int('-9223372036854775809')", "int('9223372036854775808')")
)
@pytest.mark.parametrize("safety", (0, 3))
def test_native_field_boundary_rejects_unknown_external_assignment(
    tmp_path, value, safety
):
    source = (
        COUNTER.replace("safety=0", f"safety={safety}")
        + f"counter = Counter(0)\ncounter.value = {value}\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode != 0
    assert "TypeError" in result.stderr


@pytest.mark.parametrize(
    "source",
    [
        "from mule import int64, optimize\n@optimize(speed=3)\nclass C:\n value: int64 = 0\n",
        "from mule import int64, optimize\n@optimize(speed=3)\nclass C:\n value: int64\n def __init__(self):\n  self.other = 1\n",
        "from mule import int64, optimize\nclass Base:\n pass\n@optimize(speed=3)\nclass C(Base):\n value: int64\n",
        "from mule import int64, optimize\n@optimize(speed=3)\nclass C:\n value: int64\n def __getattr__(self, name):\n  return 1\n",
    ],
)
def test_unsupported_native_class_forms_are_rejected(source):
    with pytest.raises(SyntaxError):
        lower(source)


@pytest.mark.parametrize(
    ("tail", "error"),
    [
        ("Counter(0).other = 1\n", "AttributeError"),
        ("Counter.other = 1\n", "TypeError"),
        ("print(Counter(0).missing)\n", "AttributeError"),
    ],
)
def test_native_class_rejects_undeclared_attributes_and_class_mutation(
    tmp_path, tail, error
):
    result = run_native(tmp_path, COUNTER + tail)
    assert result.returncode != 0
    assert error in result.stderr


def test_native_constructor_rejects_return_without_initializing_field():
    source = (
        "from mule import int64, optimize\n@optimize(speed=3, safety=3)\n"
        "class Uninitialized:\n value: int64\n def __init__(self):\n  pass\n"
        "print(Uninitialized().value)\n"
    )
    with pytest.raises(SyntaxError, match="initializ"):
        lower(source)


def test_native_class_requires_constructor():
    source = (
        "from mule import int64, optimize\n@optimize(speed=3, safety=3)\n"
        "class Uninitialized:\n value: int64\n"
        "print(Uninitialized().value)\n"
    )
    with pytest.raises(SyntaxError, match="__init__"):
        lower(source)


@pytest.mark.parametrize(
    "body",
    [
        "  print(self.value)\n  self.value = 0\n",
        "  if flag:\n   self.value = 0\n",
        "  if flag:\n   return\n  self.value = 0\n",
        "  expose(self)\n  self.value = 0\n",
    ],
)
def test_native_constructor_rejects_uninitialized_read_return_or_escape(body):
    source = (
        "from mule import int64, optimize\ndef expose(value):\n pass\n"
        "@optimize(speed=3, safety=3)\nclass C:\n value: int64\n"
        " def __init__(self, flag: int64):\n" + body
    )
    with pytest.raises(SyntaxError, match="initializ"):
        lower(source)


@pytest.mark.parametrize(
    ("operation", "start", "amount"),
    [("+", 2**63 - 1, 1), ("-", -(2**63), 1), ("*", 2**62, 2)],
)
def test_checked_native_int64_arithmetic_reports_overflow(
    tmp_path, operation, start, amount
):
    source = (
        "from mule import int64, optimize\n"
        "@optimize(speed=3, safety=3)\nclass C:\n value: int64\n"
        " def __init__(self, value: int64):\n  self.value = value\n"
        f" def apply(self, amount: int64):\n  self.value {operation}= amount\n"
        f"counter = C(int('{start}'))\ncounter.apply(int('{amount}'))\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode != 0
    assert "OverflowError" in result.stderr


def test_native_unbound_method_rejects_wrong_receiver(tmp_path):
    source = (
        COUNTER + "class Other:\n def __init__(self):\n  self.value = 73\n"
        "print(Counter.read(Other()))\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode != 0
    assert "TypeError" in result.stderr


def test_native_class_cannot_be_subclassed(tmp_path):
    source = COUNTER + "class Child(Counter):\n pass\nChild(0)\n"
    try:
        result = run_native(tmp_path, source)
    except SyntaxError as error:
        assert "inherit" in str(error) or "subclass" in str(error)
        return
    assert result.returncode != 0
    assert "TypeError" in result.stderr


def test_rebinding_native_class_name_preserves_actual_constructor_and_method(tmp_path):
    source = (
        COUNTER + "saved = Counter\nclass Other:\n"
        " def __init__(self, start):\n  self.value = start + 100\n"
        " def read(self):\n  return self.value\n"
        "Counter = Other\nprint(Counter(5).read(), saved(5).read())\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "105 5\n"


def test_typed_caller_falls_back_after_native_class_global_rebinding(tmp_path):
    source = (
        COUNTER + "@optimize(speed=3, safety=0)\ndef count(limit: int64) -> int64:\n"
        " counter = Counter(0)\n index: int64 = 0\n"
        " while index < limit:\n  counter.add(3)\n  index += 1\n"
        " return counter.read()\n"
        "class Other:\n"
        " def __init__(self, start):\n  self.value = start\n"
        " def add(self, amount):\n  self.value += amount * 2\n"
        " def read(self):\n  return self.value\n"
        "Counter = Other\nprint(count(2))\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "12\n"


def test_native_method_keeps_dynamic_module_function_lookup(tmp_path):
    source = (
        "from mule import int64, optimize\n"
        "def amount() -> int64:\n return 1\n"
        "@optimize(speed=3, safety=0)\nclass C:\n value: int64\n"
        " def __init__(self):\n  self.value = 0\n"
        " def add(self):\n  self.value += amount()\n"
        "def replacement() -> int64:\n return 3\n"
        "counter = C()\namount = replacement\ncounter.add()\nprint(counter.value)\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "3\n"


@pytest.mark.parametrize(
    ("expression", "value", "expected"),
    [
        ("value + value", 2**62, 2**63),
        ("value * value", 2**62, 2**124),
        ("-value", -(2**63), 2**63),
    ],
)
def test_int64_parameters_do_not_narrow_unannotated_integer_results(
    tmp_path, expression, value, expected
):
    source = (
        "from mule import int64, optimize\n"
        f"@optimize(speed=3, safety=0)\ndef calculate(value: int64):\n return {expression}\n"
        f"print(calculate(int('{value}')))\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == f"{expected}\n"


def test_unannotated_integer_intermediates_preserve_arbitrary_precision(tmp_path):
    source = (
        "from mule import int64, optimize\n"
        "@optimize(speed=3, safety=3)\ndef calculate(value: int64) -> int64:\n"
        " doubled = value + value\n answer = doubled - value\n return answer\n"
        f"print(calculate(int('{2**62}')))\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == f"{2**62}\n"


def test_method_receiver_reassignment_preserves_local_binding_semantics(tmp_path):
    source = (
        "from mule import int64, optimize\n"
        "@optimize(speed=3, safety=0)\nclass C:\n value: int64\n"
        " def __init__(self):\n  self.value = 7\n"
        " def read(self) -> int64:\n  self = 42\n  return self\n"
        "instance = C()\nprint(instance.read(), instance.value)\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "42 7\n"


def test_native_constructor_effects_and_non_none_return_are_preserved(tmp_path):
    source = (
        "from mule import int64, optimize\n"
        "def argument():\n print('argument')\n return 7\n"
        "@optimize(speed=3, safety=0)\nclass C:\n value: int64\n"
        " def __init__(self, value: int64):\n"
        "  print('constructor')\n  self.value = value\n  return 1\n"
        "C(argument())\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode != 0
    assert "TypeError" in result.stderr
    assert result.stdout == "argument\nconstructor\n"


def test_native_optional_local_preserves_unbound_error(tmp_path):
    source = (
        "from mule import int64, optimize\n"
        "@optimize(speed=3, safety=0)\ndef read(flag: int64) -> int64:\n"
        " if flag:\n  value: int64 = 42\n return value\nread(0)\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode != 0
    assert "UnboundLocalError" in result.stderr


def test_native_branch_join_preserves_initialized_local_on_both_paths(tmp_path):
    source = (
        "from mule import int64, optimize\n"
        "@optimize(speed=3, safety=3)\ndef choose(flag: int64) -> int64:\n"
        " if flag:\n  value: int64 = 7\n else:\n  value = 11\n return value\n"
        "print(choose(1), choose(0))\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "7 11\n"


def test_constructor_site_inside_loop_preserves_instances_and_effects(tmp_path):
    source = (
        "from mule import int64, optimize\n"
        "@optimize(speed=3, safety=0)\nclass C:\n value: int64\n"
        " def __init__(self, value: int64):\n"
        "  self.value = value\n  print(value)\n"
        "@optimize(speed=3, safety=0)\ndef count(limit: int64) -> int64:\n"
        " index: int64 = 0\n total: int64 = 0\n"
        " while index < limit:\n"
        "  instance = C(index)\n  total += instance.value\n  index += 1\n"
        " return total\nprint(count(3))\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "0\n1\n2\n3\n"


@pytest.mark.parametrize("recursive_constructor", (False, True))
def test_recursive_native_class_calls_preserve_recursion_error(
    tmp_path, recursive_constructor
):
    if recursive_constructor:
        body = " def __init__(self):\n  self.value = 0\n  C()\n"
        call = "C()\n"
    else:
        body = (
            " def __init__(self):\n  self.value = 0\n"
            " def recurse(self, depth: int64) -> int64:\n"
            "  if depth:\n   return self.recurse(depth - 1)\n"
            "  return self.recurse(0)\n"
        )
        call = "C().recurse(0)\n"
    source = (
        "from mule import int64, optimize\n"
        "@optimize(speed=3, safety=0)\nclass C:\n value: int64\n" + body + call
    )
    result = run_native(tmp_path, source)
    assert result.returncode != 0
    assert "RecursionError" in result.stderr
