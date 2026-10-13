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
