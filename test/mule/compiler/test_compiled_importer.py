"""Importlib discovers linked artifacts without opening or evaluating source."""

import subprocess

import pytest

from test.mule.compiler.test_modules import execute
from test.python_host import python_host


@pytest.mark.integration
def test_importlib_loads_linked_lazy_modules_and_cleans_failed_imports(tmp_path):
    sources = {
        "package/__init__.py": "print('package init')\n",
        "package/child.py": "print('child init')\nvalue = 42\n",
        "broken.py": "print('broken init')\nraise ValueError('broken')\n",
    }
    source = """
import importlib
import sys
if False:
    import package.child
    import broken
child = importlib.import_module('package.child')
print(child.value, child.__name__, child.__package__)
print(child is importlib.import_module('.child', 'package'))
print(child is sys.modules['package'].child)
print(child.__spec__.name, child.__loader__ is child.__spec__.loader)
for attempt in range(2):
    try:
        importlib.import_module('broken')
    except ValueError as error:
        print(str(error), 'broken' in sys.modules)
try:
    importlib.import_module('unlinked_unknown_module')
except ModuleNotFoundError:
    print('missing module')
"""
    for name, contents in {**sources, "main.py": source}.items():
        path = tmp_path / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(contents)
    oracle = subprocess.run(
        [python_host(), str(tmp_path / "main.py")],
        capture_output=True,
        text=True,
        check=False,
        timeout=10,
    )
    assert oracle.returncode == 0, oracle.stderr
    native = execute(tmp_path, sources, source)
    assert native.returncode == 0, native.stderr
    assert native.stdout == oracle.stdout


@pytest.mark.integration
def test_dynamic_names_select_linked_children_without_initializing_other_modules(
    tmp_path,
):
    sources = {
        "package/__init__.py": "print('package init')\n",
        "package/loader.py": "import importlib\ndef load(name):\n return importlib.import_module('package.' + name)\n",
        "package/plugins/__init__.py": "print('plugins init')\n",
        "package/plugins/first.py": "print('first init')\nvalue = 42\n",
        "package/unused.py": "raise AssertionError('linked module was initialized eagerly')\n",
    }
    source = """
if False:
    import package.plugins.first
    import package.unused
from package.loader import load
first = load('plugins.first')
print(first.value, first is load('plugins.first'))
"""
    for name, contents in {**sources, "main.py": source}.items():
        path = tmp_path / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(contents)
    oracle = subprocess.run(
        [python_host(), str(tmp_path / "main.py")],
        capture_output=True,
        text=True,
        check=False,
        timeout=10,
    )
    assert oracle.returncode == 0, oracle.stderr
    native = execute(tmp_path, sources, source)
    assert native.returncode == 0, native.stderr
    assert native.stdout == oracle.stdout
