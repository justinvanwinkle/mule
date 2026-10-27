"""Exercise compiled classes through independently executed native binaries."""

import pytest

from mule.compiler import UnsupportedSyntax, emit_llvm
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
        "uninitialized-declared-field",
        "non-none-constructor-result",
        "arguments-without-constructor",
        "missing-constructor-argument",
        "missing-method-argument",
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


def test_unimplemented_class_features_are_explicitly_rejected():
    with pytest.raises(UnsupportedSyntax):
        lower("class C[T]:\n pass\n")


def test_instances_and_classes_accept_new_attributes(tmp_path):
    result = run_native(
        tmp_path,
        "class Empty:\n pass\n"
        "obj = Empty()\nobj.new = 1\nEmpty.shared = 2\n"
        "class Method:\n def run(self):\n  return 1\n"
        "setattr(Method, 'run', None)\n"
        "print(obj.new, obj.shared, Method.run)\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "1 2 None\n"


def test_multiple_inheritance_combines_unrelated_bases(tmp_path):
    result = run_native(
        tmp_path,
        "class A:\n def first(self):\n  return 'A'\n"
        "class B:\n def second(self):\n  return 'B'\n"
        "class C(A, B):\n pass\n"
        "print(C().first(), C().second())\n"
        "print([base.__name__ for base in C.__bases__])\n"
        "print([cls.__name__ for cls in C.mro()])\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "A B\n['A', 'B']\n['C', 'A', 'B', 'object']\n"


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


def test_method_self_types_receiver_and_preserves_override_semantics(tmp_path):
    source = (
        "class Counter:\n"
        " def step(self, x):\n"
        "  return x + 1\n"
        " def bump(self, x):\n"
        "  if self is None:\n"
        "   return -1\n"
        "  return self.step(x) + 10\n"
        " def same_kind(self, other):\n"
        "  if type(other) is type(self):\n"
        "   return other.step(5)\n"
        "  return 0\n"
        "class SubCounter(Counter):\n"
        " def step(self, x):\n"
        "  return x + 100\n"
        "c = Counter()\n"
        "s = SubCounter()\n"
        "print(c.bump(1), s.bump(1), Counter.bump(None, 1))\n"
        "print(c.same_kind(Counter()), c.same_kind(s), s.same_kind(SubCounter()))\n"
    )
    ir = emit_llvm(source)
    assert '@"mule.method.mule_fn_Counter.bump.' in ir
    assert '@"mule.method.mule_fn_Counter.step.' in ir
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "12 111 -1\n6 0 105\n"


@pytest.mark.parametrize("replacement", ["'ready'", "42", "{'ready': 42}"])
def test_optional_field_constructor_with_kwargs_preserves_initial_value(
    tmp_path, replacement
):
    source = (
        "class Summary:\n"
        " def __init__(self, value=None, **kwargs):\n"
        "  self.value = value\n"
        " def fill(self):\n"
        "  if self.value is None:\n"
        f"   self.value = {replacement}\n"
        "  return self.value\n"
        "empty = Summary()\n"
        "print(empty.value is None)\n"
        f"print(empty.fill() == {replacement})\n"
        "provided = Summary(value=False, extra=True)\n"
        "print(provided.fill() is False)\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "True\nTrue\nTrue\n"


def test_instance_field_types_specialize_method_calls_and_preserve_dynamic_override(
    tmp_path,
):
    source = (
        "class Leaf:\n"
        " def __init__(self, v):\n"
        "  self.v = v\n"
        "  self.next = None\n"
        " def compute(self, x):\n"
        "  return self.v + x\n"
        "class AltLeaf:\n"
        " def __init__(self):\n"
        "  self.next = None\n"
        " def compute(self, x):\n"
        "  return x * 10\n"
        "class CustomMap:\n"
        " def __setitem__(self, k, v):\n"
        "  self.last = (k, v)\n"
        " def get(self, k):\n"
        "  return AltLeaf()\n"
        "class Holder:\n"
        " def __init__(self, leaf):\n"
        "  self.leaf = leaf\n"
        "  self.table = {}\n"
        " def put(self, k, leaf):\n"
        "  self.table[k] = leaf\n"
        "  leaf.next = self.leaf\n"
        " def eval_leaf(self, x):\n"
        "  if self.leaf is None:\n"
        "   return -1\n"
        "  return self.leaf.compute(x)\n"
        " def eval_key(self, k, x):\n"
        "  node = self.table.get(k)\n"
        "  if node is None:\n"
        "   return -2\n"
        "  nxt = node.next\n"
        "  base = node.compute(x)\n"
        "  if nxt is not None:\n"
        "   return base + nxt.compute(1)\n"
        "  return base\n"
        "h = Holder(Leaf(5))\n"
        "h.put('a', Leaf(20))\n"
        "print(h.eval_leaf(3), h.eval_key('a', 4), h.eval_key('missing', 4))\n"
        "h.leaf = AltLeaf()\n"
        "print(h.eval_leaf(3))\n"
        "h.leaf = None\n"
        "print(h.eval_leaf(3))\n"
        "h.table = CustomMap()\n"
        "h.put('b', Leaf(7))\n"
        "print(h.eval_key('b', 4))\n"
    )
    ir = emit_llvm(source)
    assert '@"mule.method.mule_fn_Leaf.compute.' in ir
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "8 30 -2\n30\n-1\n40\n"


def test_scalar_and_dict_instance_field_specialization_and_method_return_contracts(
    tmp_path,
):
    source = (
        "class Entry:\n"
        " def __init__(self, key: int, val: int):\n"
        "  self.key = key\n"
        "  self.val = val\n"
        "  self.prev = None\n"
        "class Box:\n"
        " def __init__(self, limit: int):\n"
        "  self.limit = limit\n"
        "  self.table = {}\n"
        " def put(self, k: int, v: int) -> int:\n"
        "  cur = self.table.get(k)\n"
        "  if cur is not None:\n"
        "   old = cur.val\n"
        "   cur.val = v\n"
        "   return old\n"
        "  if len(self.table) >= self.limit:\n"
        "   return -1\n"
        "  self.table[k] = Entry(k, v)\n"
        "  return 0\n"
        " def get(self, k: int) -> int:\n"
        "  cur = self.table.get(k)\n"
        "  if cur is None:\n"
        "   return -1\n"
        "  return cur.val\n"
        "def run_box(n: int) -> int:\n"
        " b = Box(8)\n"
        " acc = 0\n"
        " for i in range(n):\n"
        "  acc = (acc + b.put(i & 3, i + 1)) & 0xFFFF\n"
        "  acc = (acc + b.get(i & 3)) & 0xFFFF\n"
        " return acc\n"
        "print(run_box(12))\n"
        "e = Entry(1, 10)\n"
        "setattr(e, 'val', 'bad')\n"
        "b2 = Box(4)\n"
        "b2.table[1] = e\n"
        "try:\n"
        " print(b2.get(1))\n"
        "except TypeError:\n"
        " print('caught-field-type-error')\n"
    )
    ir = emit_llvm(source)
    assert '@"mule.protocol.dict_get"' in ir
    assert "@mule_fast_len(" in ir
    assert "@llvm.sadd.with.overflow.i64(" in ir
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "114\ncaught-field-type-error\n"
