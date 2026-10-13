"""Native annotation callbacks, deferred evaluation, and function metadata."""

import pytest

from test.mule.compiler.test_modules import execute


def test_annotations_evaluate_lazily_in_order_and_cache_only_value_access(tmp_path):
    result = execute(
        tmp_path,
        {},
        "events = []\n"
        "Current = int\n"
        "def annotation():\n events.append('annotation')\n return Current\n"
        "def decorate(function):\n"
        " print(function.__annotate__ is not None, events)\n return function\n"
        "@decorate\n"
        "def f(x: annotation()) -> annotation():\n return x\n"
        "print(f(3), events, f.__dict__)\n"
        "print(f.__annotate__(4))\n"
        "Current = str\n"
        "cached = f.__annotations__\n"
        "print(cached['x'] is str, cached['return'] is str, events)\n"
        "Current = int\n"
        "print(f.__annotations__ is cached, f.__annotations__['x'] is str)\n"
        "print(f.__annotate__(1)['x'] is int, len(events))\n"
        "print(f.__annotations__ is cached)\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "True []\n3 [] {}\n{'x': 'annotation()', 'return': 'annotation()'}\n"
        "True True ['annotation', 'annotation']\nTrue True\nTrue 4\nTrue\n"
    )


def test_annotations_retain_enclosing_cells_and_current_class_namespace(tmp_path):
    result = execute(
        tmp_path,
        {},
        "def outer():\n"
        " T = int\n"
        " def f(x: T):\n  return x\n"
        " def change():\n  nonlocal T\n  T = str\n"
        " return f, change\n"
        "f, change = outer()\nchange()\n"
        "print(f.__annotations__['x'] is str)\n"
        "class C:\n"
        " T = int\n"
        " def method(self, x: T) -> T:\n  pass\n"
        "view = C.__dict__\nC.T = str\n"
        "print(view['T'] is str, C.method.__annotations__['x'] is str)\n"
        "method = C().method\n"
        "print(method.__func__ is C.method)\n"
        "print(method.__annotations__ is C.method.__annotations__)\n"
        "print(method.__annotate__(1)['return'] is str)\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "True\nTrue True\nTrue\nTrue\nTrue\n"


def test_annotation_failure_does_not_cache_and_string_format_does_not_evaluate(
    tmp_path,
):
    result = execute(
        tmp_path,
        {},
        "def f(x: Missing):\n pass\n"
        "print(f.__annotate__(4))\n"
        "try:\n f.__annotations__\n"
        "except NameError:\n print('missing')\n"
        "Missing = str\n"
        "print(f.__annotations__['x'] is str)\n"
        "try:\n f.__annotate__(3)\n"
        "except NotImplementedError:\n print('unsupported format')\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "{'x': 'Missing'}\nmissing\nTrue\nunsupported format\n"


def test_future_annotations_remain_strings(tmp_path):
    result = execute(
        tmp_path,
        {},
        "from __future__ import annotations\n"
        "def f(x: Missing | None) -> 'Later':\n return x\n"
        "print(f.__annotations__)\n"
        "print(f.__annotate__(2) == f.__annotations__)\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "{'x': 'Missing | None', 'return': \"'Later'\"}\nTrue\n"


def test_annotation_assignment_invalidation_and_deletion(tmp_path):
    result = execute(
        tmp_path,
        {},
        "Current = int\n"
        "def f(x: Current):\n pass\n"
        "print(f.__annotations__['x'] is int)\n"
        "Current = str\n"
        "del f.__annotations__\n"
        "print(f.__annotations__['x'] is str)\n"
        "replacement = {'x': bytes}\n"
        "f.__annotations__ = replacement\n"
        "print(f.__annotate__ is None, f.__annotations__ is replacement)\n"
        "events = []\n"
        "def callback(format):\n events.append(format)\n return {'x': float}\n"
        "f.__annotate__ = callback\n"
        "print(f.__annotations__['x'] is float, events)\n"
        "print(f.__annotations__['x'] is float, events)\n"
        "f.__annotate__ = None\n"
        "print(f.__annotations__['x'] is float, events)\n"
        "for invalid in (3, []):\n"
        " try:\n  f.__annotations__ = invalid\n"
        " except TypeError:\n  print('invalid annotations')\n"
        "try:\n f.__annotate__ = 3\n"
        "except TypeError:\n print('invalid callback')\n"
        "f.__annotations__ = None\n"
        "print(f.__annotations__, f.__annotate__)\n"
        "def plain():\n pass\n"
        "print(plain.__annotate__, plain.__annotations__ is plain.__annotations__)\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "True\nTrue\nTrue True\nTrue [1]\nTrue [1]\nTrue [1]\n"
        "invalid annotations\ninvalid annotations\ninvalid callback\n{} None\nNone True\n"
    )


def test_class_relationship_snapshots_do_not_keep_children_alive(tmp_path):
    result = execute(
        tmp_path,
        {},
        "from collections.abc import Sequence\n"
        "print(isinstance([], Sequence), None.__class__ is type(None))\n"
        "class Parent:\n pass\n"
        "class First(Parent):\n pass\n"
        "class Second(Parent):\n pass\n"
        "snapshot = Parent.__subclasses__()\n"
        "print(snapshot == [First, Second])\n"
        "del First\n"
        "print(len(Parent.__subclasses__()))\n"
        "del snapshot\n"
        "print(Parent.__subclasses__() == [Second])\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "True True\nTrue\n2\nTrue\n"


@pytest.mark.integration
def test_annotationlib_and_singledispatch_use_compiled_callbacks(tmp_path):
    result = execute(
        tmp_path,
        {},
        "import annotationlib\n"
        "import functools\n"
        "@functools.singledispatch\n"
        "def show(value):\n return 'default'\n"
        "@show.register\n"
        "def text(value: str):\n return 'text'\n"
        "@show.register\n"
        "def absent(value: None):\n return 'none'\n"
        "print(show(1), show('x'), show(None))\n"
        "print(annotationlib.get_annotations(text)['value'] is str)\n"
        "print(annotationlib.get_annotations(text, format=annotationlib.Format.STRING))\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "default text none\nTrue\n{'value': 'str'}\n"
