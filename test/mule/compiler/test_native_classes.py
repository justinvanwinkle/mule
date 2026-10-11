"""Exercise compiled classes through independently executed native binaries."""

import pytest

from mule.compiler import UnsupportedSyntax
from mule.compiler.main import compile_source as lower
from test.mule.compiler.test_native import run_native


@pytest.mark.parametrize(
    ("source", "expected"),
    [
        (
            (
                "class Empty:\n pass\nfirst = Empty()\nsecond = Empty()\n"
                "print(first is first, first is second, bool(first))\n"
            ),
            "True False True\n",
        ),
        (
            (
                "class Point:\n"
                " def __init__(self, x, y):\n  self.x = x\n  self.y = y\n"
                " def sum(self):\n  return self.x + self.y\n"
                "point = Point(20, 22)\nprint(point.x, point.y, point.sum())\n"
            ),
            "20 22 42\n",
        ),
        (
            (
                "class Box:\n"
                " def __init__(receiver, value):\n  receiver.value = value\n"
                " def read(receiver):\n  return receiver.value\n"
                "print(Box(42).read())\n"
            ),
            "42\n",
        ),
        (
            (
                "class Box:\n"
                " def __init__(self, value):\n  self.value = value\n"
                "box = Box(1)\nalias = box\nalias.value = 'changed'\n"
                "print(box.value, alias is box)\nbox.value = 2 ** 100\n"
                "print(alias.value)\nbox.value = None\nprint(alias.value)\n"
            ),
            "changed True\n1267650600228229401496703205376\nNone\n",
        ),
        (
            (
                "class Box:\n"
                " def __init__(self, value):\n  self.value = value\n"
                "a = Box(7)\nb = Box('text')\nprint(a.value, b.value)\n"
                "a.value = b\nprint(a.value.value)\n"
            ),
            "7 text\ntext\n",
        ),
        (
            (
                "class Box:\n"
                " def __init__(self, value):\n  self.value = value\n"
                " def read(self):\n  return self.value\n"
                "box = Box(1)\nread = box.read\nbox.value = 42\nbox = None\n"
                "print(read())\n"
            ),
            "42\n",
        ),
        (
            (
                "class Counter:\n"
                " def __init__(self):\n  self.count = 0\n"
                " def increment(self, amount):\n  self.count += amount\n"
                "counter = Counter()\ni = 0\nwhile i < 100:\n"
                " counter.increment(2)\n i += 1\nprint(counter.count)\n"
            ),
            "200\n",
        ),
        (
            (
                "class Late:\n"
                " def initialize(self, value):\n  self.value = value\n"
                "late = Late()\nlate.value = 42\nprint(late.value)\n"
            ),
            "42\n",
        ),
        (
            (
                "class Box:\n"
                " def __init__(self):\n  self.value = None\n"
                "box = Box()\nprint(box.value)\nbox.value = box\n"
                "print(box.value is box)\nbox.value = None\nprint(box.value)\n"
            ),
            "None\nTrue\nNone\n",
        ),
        (
            (
                "class Scale:\n"
                " def __init__(self, factor):\n  self.factor = factor\n"
                " def apply(self, value):\n  return self.factor * value\n"
                "scale = Scale(6)\nprint(Scale.apply(scale, 7))\n"
            ),
            "42\n",
        ),
        (
            (
                "class Base:\n"
                " def __init__(self, value):\n  self.value = value\n"
                " def read(self):\n  return self.value\n"
                "class Child(Base):\n pass\nchild = Child(42)\n"
                "print(child.value, child.read())\n"
            ),
            "42 42\n",
        ),
        (
            (
                "class Base:\n"
                " def __init__(self):\n  self.base = 20\n"
                " def result(self):\n  return self.base\n"
                "class Child(Base):\n"
                " def __init__(self):\n  Base.__init__(self)\n  self.extra = 22\n"
                " def result(self):\n  return self.base + self.extra\n"
                "child = Child()\nprint(child.base, child.extra, child.result())\n"
            ),
            "20 22 42\n",
        ),
        (
            (
                "class Base:\n"
                " def result(self):\n  return self.compute()\n"
                " def compute(self):\n  return 1\n"
                "class Child(Base):\n"
                " def compute(self):\n  return 42\n"
                "print(Base().result(), Child().result())\n"
            ),
            "1 42\n",
        ),
        (
            (
                "class First:\n"
                " def __init__(self):\n  self.x = 11\n  self.y = 12\n"
                " def result(self):\n  return 21\n"
                "class Second:\n"
                " def __init__(self):\n  self.y = 22\n  self.x = 23\n"
                " def result(self):\n  return 24\n"
                "def read(value):\n print(value.x, value.result())\n"
                "first = First()\nsecond = Second()\n"
                "read(first)\nread(second)\nread(first)\n"
            ),
            "11 21\n23 24\n11 21\n",
        ),
        (
            (
                "class Original:\n"
                " def result(self):\n  return 42\n"
                "saved = Original\nOriginal = 17\nprint(saved().result(), Original)\n"
            ),
            "42 17\n",
        ),
        (
            (
                "value = 1\nclass Read:\n"
                " def result(self):\n  return value\n"
                "reader = Read()\nprint(reader.result())\nvalue = 42\n"
                "print(reader.result())\n"
            ),
            "1\n42\n",
        ),
        (
            (
                "def operation(value):\n return value + 1\nclass Apply:\n"
                " def result(self, value):\n  return operation(value)\n"
                "obj = Apply()\nprint(obj.result(3))\n"
                "def operation(value):\n return value * 2\nprint(obj.result(3))\n"
            ),
            "4\n6\n",
        ),
        (
            (
                "class Box:\n"
                " def __init__(self, value):\n  print('init', value)\n  self.value = value\n"
                "def argument():\n print('argument')\n return 42\n"
                "print(Box(argument()).value)\n"
            ),
            "argument\ninit 42\n42\n",
        ),
        (
            (
                "class Callable:\n"
                " def __call__(self, value):\n  return value + 1\n"
                "class Box:\n"
                " def __init__(self, action):\n  self.action = action\n"
                "def argument():\n box.action = None\n return 41\n"
                "box = Box(Callable())\nprint(box.action(argument()))\n"
                "print(box.action)\n"
            ),
            "42\nNone\n",
        ),
    ],
    ids=[
        "empty-instance-identity",
        "constructor-and-method",
        "receiver-parameter-spelling",
        "aliases-and-changing-field-values",
        "independent-instances-and-object-fields",
        "bound-method-retains-receiver",
        "augmented-field-assignment",
        "fields-discovered-outside-constructor",
        "none-and-temporary-self-reference",
        "unbound-method-call",
        "inherited-initializer-and-field",
        "inherited-and-new-fields",
        "virtual-self-dispatch",
        "attribute-and-method-cache-class-change",
        "class-global-rebinding",
        "method-dynamic-global-read",
        "method-dynamic-function-rebinding",
        "constructor-evaluation-order",
        "callable-field-resolved-before-arguments",
    ],
)
def test_native_classes(tmp_path, source, expected):
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == expected
    assert result.stderr == ""


@pytest.mark.parametrize(
    ("source", "exception", "expected_stdout"),
    [
        ("class Empty:\n pass\nprint(Empty().missing)\n", "AttributeError", ""),
        ("class Empty:\n pass\nobj = Empty()\nobj.new = 1\n", "AttributeError", ""),
        (
            (
                "class Late:\n def initialize(self):\n  self.value = 42\n"
                "print(Late().value)\n"
            ),
            "AttributeError",
            "",
        ),
        ("class Bad:\n def __init__(self):\n  return 1\nBad()\n", "TypeError", ""),
        ("class Empty:\n pass\nEmpty(1)\n", "TypeError", ""),
        (
            (
                "class NeedsArgument:\n def __init__(self, value):\n  self.value = value\n"
                "NeedsArgument()\n"
            ),
            "TypeError",
            "",
        ),
        (
            "class Method:\n def run(self, value):\n  return value\nMethod().run()\n",
            "TypeError",
            "",
        ),
        ("class Empty:\n pass\nEmpty.new = 1\n", "TypeError", ""),
        (
            (
                "class Method:\n def run(self):\n  return 1\n"
                "setattr(Method, 'run', None)\n"
            ),
            "TypeError",
            "",
        ),
        (
            (
                "class Empty:\n pass\ndef argument():\n print('argument')\n return 1\n"
                "Empty().missing(argument())\n"
            ),
            "AttributeError",
            "",
        ),
        ("print((42).missing)\n", "AttributeError", ""),
        ("class Bad(42):\n pass\n", "TypeError", ""),
    ],
    ids=[
        "missing-instance-attribute",
        "undeclared-instance-field",
        "uninitialized-declared-field",
        "non-none-constructor-result",
        "arguments-without-constructor",
        "missing-constructor-argument",
        "missing-method-argument",
        "immutable-class-attributes",
        "immutable-class-setattr",
        "missing-method-prevents-argument-evaluation",
        "scalar-attribute-error",
        "invalid-base",
    ],
)
def test_native_class_errors(tmp_path, source, exception, expected_stdout):
    result = run_native(tmp_path, source)
    assert result.returncode != 0
    assert exception in result.stderr
    assert result.stdout == expected_stdout


@pytest.mark.parametrize(
    ("source", "expected"),
    [
        (
            (
                "class Sized:\n def __init__(self, size):\n  self.size = size\n"
                " def __len__(self):\n  return self.size\n"
                "empty = Sized(0)\nfull = Sized(3)\n"
                "print(len(empty), bool(empty), len(full), bool(full))\n"
            ),
            "0 False 3 True\n",
        ),
        (
            (
                "class Truth:\n def __bool__(self):\n  print('bool')\n  return False\n"
                " def __len__(self):\n  print('len')\n  return 3\n"
                "obj = Truth()\nprint(bool(obj))\nprint(len(obj))\n"
            ),
            "bool\nFalse\nlen\n3\n",
        ),
        (
            (
                "class Base:\n def __len__(self):\n  return 2\n"
                "class Child(Base):\n def __len__(self):\n  return 0\n"
                "print(len(Base()), bool(Base()), len(Child()), bool(Child()))\n"
            ),
            "2 True 0 False\n",
        ),
    ],
)
def test_native_class_length_and_truth_protocols(tmp_path, source, expected):
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == expected


@pytest.mark.parametrize(
    ("method", "value", "expression", "exception"),
    [
        ("__len__", "-1", "len(obj)", "ValueError"),
        ("__len__", "-1", "bool(obj)", "ValueError"),
        ("__len__", "'invalid'", "len(obj)", "TypeError"),
        ("__len__", "2 ** 100", "len(obj)", "OverflowError"),
        ("__bool__", "1", "bool(obj)", "TypeError"),
        ("__bool__", "None", "bool(obj)", "TypeError"),
    ],
)
def test_native_class_invalid_protocol_results(
    tmp_path, method, value, expression, exception
):
    source = f"class Bad:\n def {method}(self):\n  return {value}\nobj = Bad()\n{expression}\n"
    result = run_native(tmp_path, source)
    assert result.returncode != 0
    assert exception in result.stderr


@pytest.mark.parametrize(
    "source",
    [
        "class A:\n pass\nclass B:\n pass\nclass C(A, B):\n pass\n",
        "class C(metaclass=type):\n pass\n",
        "class C[T]:\n pass\n",
    ],
)
def test_unimplemented_class_features_are_explicitly_rejected(source):
    with pytest.raises(UnsupportedSyntax):
        lower(source)


@pytest.mark.parametrize(
    "docstring", ["'Class documentation.'", "''", "('Adjacent ' 'strings.')"]
)
def test_class_docstring_compiles(docstring):
    lower(f"class Documented:\n {docstring}\n")


def test_class_docstring_preserves_instance_behavior(tmp_path):
    result = run_native(
        tmp_path,
        'class Documented:\n """Class documentation.\n More documentation.\n """\n'
        " def value(self):\n  return 42\nprint(Documented().value())\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "42\n"
