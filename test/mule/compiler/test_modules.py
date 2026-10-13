"""Native module initialization, isolated globals, and compiled assertions."""

import subprocess

import pytest

from mule.compiler import build


def execute(tmp_path, sources, main):
    for name, source in {**sources, "main.py": main}.items():
        path = tmp_path / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(source)
    executable = build(
        tmp_path / "main.py", tmp_path / "program", optimize=False, workers=1
    )
    for source in tmp_path.rglob("*.py"):
        source.unlink()
    return subprocess.run(
        [executable], check=False, capture_output=True, text=True, timeout=10
    )


def test_imported_globals_are_isolated_mutable_and_initialized_once(tmp_path):
    result = execute(
        tmp_path,
        {
            "first.py": "print('first init')\nvalue = 3\ndef get():\n return value\ndef bump():\n global value\n value += 1\n",
            "second.py": "value = 100\ndef get():\n return value\n",
        },
        "import first as a\nimport first as b\nimport second\nprint(a is b, a.get(), second.get())\na.bump()\nprint(b.value, second.value)\nsecond.value = 200\nprint(a.get(), second.get())\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "first init\nTrue 3 100\n4 100\n4 200\n"


def test_circular_imports_share_the_initializing_module_namespace(tmp_path):
    result = execute(
        tmp_path,
        {
            "first.py": "value = 7\nimport second\ndef get():\n return second.get()\n",
            "second.py": "import first\ndef get():\n return first.value\n",
        },
        "import first\nprint(first.get())\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "7\n"


def test_main_module_registry_shares_live_globals_before_and_after_sys(tmp_path):
    result = execute(
        tmp_path,
        {"__main__.py": "raise RuntimeError('second main namespace')\n"},
        "value = 7\n"
        "import __main__ as early\n"
        "import sys\n"
        "import __main__ as late\n"
        "print(early is late, early is sys.modules['__main__'])\n"
        "print(early.__name__, early.value)\n"
        "early.value = 42\n"
        "print(value, globals()['value'])\n"
        "globals()['value'] = 100\n"
        "print(late.value)\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "True True\n__main__ 7\n42 42\n100\n"


@pytest.mark.integration
def test_dataclass_string_pseudofields_resolve_main_module_imports(tmp_path):
    result = execute(
        tmp_path,
        {},
        "import dataclasses as dc\n"
        "import typing as t\n"
        "from typing import ClassVar as CV\n"
        "from dataclasses import InitVar as IV, KW_ONLY as KO\n"
        "class Probe:\n"
        " default = 1\n"
        "for annotation in ['CV[int]', 't.ClassVar[int]']:\n"
        " field = dc._get_field(Probe, 'default', annotation, False)\n"
        " print(field._field_type is dc._FIELD_CLASSVAR)\n"
        "for annotation in ['IV[int]', 'dc.InitVar[int]']:\n"
        " field = dc._get_field(Probe, 'default', annotation, False)\n"
        " print(field._field_type is dc._FIELD_INITVAR)\n"
        "for annotation in ['KO', 'dc.KW_ONLY']:\n"
        " print(dc._is_type(annotation, Probe, dc, dc.KW_ONLY, dc._is_kw_only))\n"
        "for annotation in ['ClassVar[int]', 'wrong.ClassVar[int]', 'CVExtra']:\n"
        " field = dc._get_field(Probe, 'default', annotation, False)\n"
        " print(field._field_type is dc._FIELD)\n"
        "field = dc._get_field(Probe, 'required', 'int', False)\n"
        "print(field.default is dc.MISSING)\n"
        "@dc.dataclass\n"
        "class Example:\n"
        " shared: CV[int] = 7\n"
        " required: int\n"
        " init_only: IV[int] = 9\n"
        " def __post_init__(self, init_only):\n"
        "  self.required += init_only\n"
        "value = Example(33)\n"
        "print(value.required, value.shared)\n"
        "print([field.name for field in dc.fields(Example)])\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "True\n" * 10 + "42 7\n['required']\n"


@pytest.mark.parametrize(
    "lazy_read",
    [
        "print(metadata.lazy)\n",
        "print(getattr(metadata, 'lazy'))\n",
        "from metadata import lazy\nprint(lazy)\n",
    ],
)
def test_module_getattr_handles_lazy_attributes_without_affecting_globals(
    tmp_path, lazy_read
):
    result = execute(
        tmp_path,
        {
            "metadata.py": (
                "stored = None\n"
                "def __getattr__(name):\n"
                " print('hook', name)\n"
                " if name == 'lazy':\n"
                "  globals()[name] = 42\n"
                "  return 42\n"
                " if name == 'broken':\n"
                "  raise ValueError('hook failed')\n"
                " raise AttributeError(name)\n"
                "def read_global():\n"
                " return absent\n"
            ),
            "plain.py": "stored = None\n",
        },
        "import metadata\n"
        "print(metadata.stored)\n"
        "try:\n"
        " object.__getattribute__(metadata, 'lazy')\n"
        "except AttributeError:\n"
        " print('bypass')\n" + lazy_read + "from metadata import lazy\n"
        "print(getattr(metadata, 'lazy'), lazy)\n"
        "try:\n"
        " metadata.absent\n"
        "except AttributeError:\n"
        " print('missing')\n"
        "try:\n"
        " hasattr(metadata, 'broken')\n"
        "except ValueError:\n"
        " print('hook error')\n"
        "try:\n"
        " metadata.read_global()\n"
        "except NameError:\n"
        " print('global missing')\n"
        "import plain\n"
        "print(hasattr(plain, 'absent'))\n"
        "try:\n"
        " plain.absent\n"
        "except AttributeError as error:\n"
        " print('absent' in str(error), '__getattr__' in str(error))\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "None\nbypass\nhook lazy\n42\n42 42\nhook absent\nmissing\n"
        "hook broken\nhook error\nglobal missing\nFalse\nTrue False\n"
    )


def test_module_getattr_bound_to_none_raises_type_error(tmp_path):
    result = execute(
        tmp_path,
        {"metadata.py": "__getattr__ = None\n"},
        "import metadata\n"
        "try:\n"
        " metadata.absent\n"
        "except TypeError:\n"
        " print('hook is not callable')\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "hook is not callable\n"


def test_module_subclass_dictionary_updates_its_existing_attribute_storage(tmp_path):
    result = execute(
        tmp_path,
        {"original.py": "stored = None\n"},
        "import original\n"
        "class Replacement(type(original)):\n"
        " def __getattr__(self, name):\n"
        "  if name == 'lazy':\n"
        "   return 42\n"
        "  raise AttributeError(name)\n"
        "replacement = Replacement('replacement')\n"
        "replacement.__dict__.update(original.__dict__)\n"
        "print(type(replacement) is Replacement)\n"
        "print(replacement.stored is None, replacement.__dict__['stored'] is None, replacement.lazy)\n"
        "replacement.__dict__['stored'] = 42\n"
        "print(replacement.stored, original.stored)\n"
        "replacement.stored = 43\n"
        "print(replacement.__dict__['stored'])\n"
        "del replacement.__dict__['stored']\n"
        "print('stored' in replacement.__dict__, hasattr(replacement, 'stored'))\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "True\nTrue True 42\n42 None\n43\nFalse False\n"


@pytest.mark.parametrize("sys_first", [False, True])
def test_failed_import_is_removed_and_retry_preserves_the_exception(
    tmp_path, sys_first
):
    result = execute(
        tmp_path,
        {"broken.py": "print('initializing')\nraise ValueError('broken import')\n"},
        ("import sys\n" if sys_first else "") + "for attempt in range(2):\n"
        " try:\n"
        "  import broken\n"
        " except ValueError as error:\n"
        "  print(error)\n"
        "import sys\n"
        "print('broken' in sys.modules)\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "initializing\nbroken import\ninitializing\nbroken import\nFalse\n"
    )


def test_packages_and_from_imports_use_the_same_module_objects(tmp_path):
    result = execute(
        tmp_path,
        {
            "package/__init__.py": "print('package init')\nvalue = 42\n",
            "package/child.py": "from package import value\ndef get():\n return value\n",
        },
        "import package.child\nimport package.child as other\nfrom package import child\nprint(other is child, package.child is child, child.get(), child.__name__)\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "package init\nTrue True 42 package.child\n"


@pytest.mark.parametrize("failure", [False, True])
def test_assertions_evaluate_the_message_only_on_failure(tmp_path, failure):
    result = execute(
        tmp_path,
        {},
        "def message():\n print('message evaluated')\n return 'broken'\n"
        + f"assert {not failure}, message()\nprint('continued')\n",
    )
    if failure:
        assert result.returncode != 0
        assert result.stdout == "message evaluated\n"
        assert result.stderr.startswith("Traceback (most recent call last):\n")
        assert "line 4, in <module>\n" in result.stderr
        assert result.stderr.endswith("\nAssertionError: broken\n")
    else:
        assert result.returncode == 0, result.stderr
        assert result.stdout == "continued\n"


@pytest.mark.parametrize(
    ("imports", "annotation"),
    [
        ("import metadata as m", "m.Marker[int]"),
        ("from metadata import Marker as Contract", "Contract | None"),
    ],
)
def test_ordinary_annotation_imports_execute_without_evaluating_metadata(
    tmp_path, imports, annotation
):
    result = execute(
        tmp_path,
        {"metadata.py": "print('metadata imported')\nclass Marker:\n pass\n"},
        f"{imports}\ndef identity(value: {annotation}) -> {annotation}:\n return value\nprint(identity(42))\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "metadata imported\n42\n"


def test_imports_inside_functions_are_local_and_lazy(tmp_path):
    result = execute(
        tmp_path,
        {"dependency.py": "print('dependency init')\nvalue = 42\n"},
        "dependency = 7\ndef read():\n import dependency\n return dependency.value\nprint('before import')\nprint(read(), dependency)\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "before import\ndependency init\n42 7\n"
