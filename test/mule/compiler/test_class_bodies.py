"""General class bodies: namespace execution, decorators, descriptors, super."""

import pytest

from mule.compiler import UnsupportedSyntax
from mule.compiler.main import compile_source as lower
from test.mule.compiler.test_native import run_native


def assert_output(tmp_path, source, expected):
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == expected


def test_class_body_runs_statements_against_its_namespace(tmp_path):
    source = (
        "x = 'global'\n"
        "class C:\n"
        " '''Documented.'''\n"
        " before = x\n"
        " x = 'class'\n"
        " after = x\n"
        " total = 0\n"
        " for i in range(4):\n"
        "  total = total + i\n"
        " del i\n"
        " if total > 5:\n"
        "  big = True\n"
        " else:\n"
        "  big = False\n"
        "print(C.before, C.after, C.total, C.big, x)\n"
        "print(C.__doc__, C.__qualname__, C.__module__)\n"
    )
    assert_output(
        tmp_path, source, "global class 6 True global\nDocumented. C __main__\n"
    )


def test_methods_skip_the_class_scope(tmp_path):
    source = (
        "value = 'module'\n"
        "class C:\n"
        " value = 'class'\n"
        " def read(self):\n"
        "  return value\n"
        "print(C().read(), C.value)\n"
    )
    assert_output(tmp_path, source, "module class\n")


def test_unset_field_falls_back_to_class_attribute(tmp_path):
    source = (
        "class C:\n"
        " label = 'default'\n"
        " def rename(self, label):\n"
        "  self.label = label\n"
        "first, second = C(), C()\n"
        "second.rename('custom')\n"
        "print(first.label, second.label, C.label)\n"
    )
    assert_output(tmp_path, source, "default custom default\n")


def test_decorators_apply_innermost_first_after_evaluating_in_order(tmp_path):
    source = (
        "def tag(name):\n"
        " print('evaluate', name)\n"
        " def apply(function):\n"
        "  print('apply', name)\n"
        "  return function\n"
        " return apply\n"
        "@tag('outer')\n"
        "@tag('inner')\n"
        "def f():\n"
        " return 1\n"
        "@tag('class')\n"
        "class C:\n"
        " @tag('method')\n"
        " def m(self):\n"
        "  return 2\n"
        "def replace(function):\n"
        " return 42\n"
        "@replace\n"
        "def g():\n"
        " pass\n"
        "print(f(), C().m(), g)\n"
    )
    assert_output(
        tmp_path,
        source,
        "evaluate outer\nevaluate inner\napply inner\napply outer\n"
        "evaluate class\nevaluate method\napply method\napply class\n1 2 42\n",
    )


def test_builtin_descriptors(tmp_path):
    source = (
        "class C:\n"
        " def __init__(self, value):\n"
        "  self.value = value\n"
        " @staticmethod\n"
        " def double(x):\n"
        "  return x * 2\n"
        " @classmethod\n"
        " def make(cls, value):\n"
        "  return cls(value)\n"
        " @property\n"
        " def half(self):\n"
        "  return self.value // 2\n"
        " @half.setter\n"
        " def half(self, amount):\n"
        "  self.value = amount * 2\n"
        "c = C.make(10)\n"
        "print(C.double(3), c.double(4), c.half)\n"
        "c.half = 7\n"
        "print(c.value, c.half)\n"
        "extra = C.make(1)\n"
        "extra.missing = 1\n"
        "print(extra.missing)\n"
    )
    assert_output(tmp_path, source, "6 8 5\n14 7\n1\n")


def test_property_takes_precedence_over_a_field_of_the_same_name(tmp_path):
    source = (
        "class C:\n"
        " def __init__(self):\n"
        "  self.stored = 0\n"
        "  self.value = 5\n"
        " @property\n"
        " def value(self):\n"
        "  return self.stored + 100\n"
        " @value.setter\n"
        " def value(self, value):\n"
        "  self.stored = value\n"
        "c = C()\n"
        "print(c.value, c.stored)\n"
    )
    assert_output(tmp_path, source, "105 5\n")


def test_super_and_class_cell(tmp_path):
    source = (
        "class Base:\n"
        " def __init__(self, value):\n"
        "  self.value = value\n"
        " def describe(self):\n"
        "  return 'base ' + str(self.value)\n"
        " @classmethod\n"
        " def kind(cls):\n"
        "  return 'kind of ' + cls.__qualname__\n"
        "class Child(Base):\n"
        " def __init__(self, value):\n"
        "  super().__init__(value * 10)\n"
        " def describe(self):\n"
        "  return 'child ' + super().describe()\n"
        " @classmethod\n"
        " def kind(cls):\n"
        "  return super().kind() + '!'\n"
        " def owner(self):\n"
        "  return __class__\n"
        "c = Child(4)\n"
        "print(c.describe(), Child.kind(), c.owner() is Child)\n"
    )
    assert_output(tmp_path, source, "child base 40 kind of Child! True\n")


def test_nested_and_function_local_classes(tmp_path):
    source = (
        "def make(n):\n"
        " class Local:\n"
        "  size = n\n"
        "  def total(self):\n"
        "   return self.size + n\n"
        " return Local\n"
        "class Outer:\n"
        " class Inner:\n"
        "  value = 3\n"
        " doubled = Inner.value * 2\n"
        "print(make(4)().total(), Outer.Inner.value, Outer.doubled)\n"
        "print(Outer.Inner.__qualname__, make(1).__qualname__)\n"
    )
    assert_output(tmp_path, source, "8 3 6\nOuter.Inner make.<locals>.Local\n")


def test_getattr_hook_handles_missing_attributes(tmp_path):
    source = (
        "class C:\n"
        " present = 1\n"
        " def __getattr__(self, name):\n"
        "  return 'computed ' + name\n"
        "c = C()\n"
        "print(c.present, c.absent)\n"
    )
    assert_output(tmp_path, source, "1 computed absent\n")


def test_unimplemented_class_features_are_explicitly_rejected():
    with pytest.raises(UnsupportedSyntax):
        lower("class C(*bases):\n pass\n")


def test_metaclass_receives_class_keywords(tmp_path):
    # The metaclass methods use super(), so they are closures over __class__,
    # and keyword calls must still reach them.
    source = (
        "class Meta(type):\n"
        " def __new__(mcs, name, bases, namespace, **options):\n"
        "  cls = super().__new__(mcs, name, bases, namespace)\n"
        "  cls.options = options\n"
        "  return cls\n"
        " def __init__(cls, name, bases, namespace, **options):\n"
        "  super().__init__(name, bases, namespace)\n"
        "class C(metaclass=Meta, flag=True):\n"
        " pass\n"
        "options = {'flag': False}\n"
        "class D(metaclass=Meta, **options):\n"
        " pass\n"
        "print(C.options, D.options, type(C) is Meta)\n"
        "print(Meta('E', (), {}, level=1).options)\n"
    )
    assert_output(
        tmp_path,
        source,
        "{'flag': True} {'flag': False} True\n{'level': 1}\n",
    )
