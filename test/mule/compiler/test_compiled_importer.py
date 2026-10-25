"""Importlib discovers linked artifacts without opening or evaluating source."""

import subprocess

import pytest

from mule.compiler import build
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


@pytest.mark.integration
def test_declared_namespace_module_supports_separate_creation_execution_and_reload(
    tmp_path,
):
    sources = {
        "pyproject.toml": '[tool.mule]\ninclude_modules = ["plugins.chosen"]\n',
        "plugins/chosen.py": "print('body')\ncount = globals().get('count', 0) + 1\ndef current():\n return count\n",
        "plugins/unselected.py": "raise AssertionError('unselected body ran')\n",
    }
    source = """
import importlib
import importlib.util
import sys
name = '.'.join(['plugins', 'chosen'])
spec = importlib.util.find_spec(name)
module = importlib.util.module_from_spec(spec)
print(module.__name__, 'count' in module.__dict__, name in sys.modules)
print(module.__spec__ is spec, module.__file__ == spec.origin)
sys.modules[name] = module
spec.loader.exec_module(module)
print(module.current(), importlib.import_module(name) is module)
print(importlib.reload(module) is module, module.current())
parent = sys.modules['plugins']
print(parent.__spec__.origin, parent.__file__, len(parent.__path__))
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
def test_unlinked_module_compiles_and_loads_dynamically_at_runtime(tmp_path):
    (tmp_path / "dynamic_plugin.py").write_text(
        "def compute(x: int) -> int:\n    return x * 6 + 6\n"
    )
    main_source = tmp_path / "main.py"
    main_source.write_text(
        "import importlib\n"
        "name = '_'.join(['dynamic', 'plugin'])\n"
        "mod = importlib.import_module(name)\n"
        "print(mod.compute(6))\n"
    )
    executable = build(main_source, tmp_path / "program", optimize=False, workers=1)
    native = subprocess.run(
        [executable], capture_output=True, text=True, check=False, timeout=60
    )
    assert native.returncode == 0, native.stderr
    assert native.stdout == "42\n"
