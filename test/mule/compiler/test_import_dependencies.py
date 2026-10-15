"""Computed imports reserve artifacts without running module bodies."""

import subprocess

import pytest

from mule.compiler import build
from mule.compiler.inputs import directory_entries, recording, unchanged
from mule.compiler.main import compile_toplevel
from mule.compiler.modules import ModuleCompiler
from test.python_host import python_host


def test_prefix_dependencies_include_only_immediate_modules(tmp_path):
    package = tmp_path / "plugins"
    package.mkdir()
    for name in ("__init__", "alpha", "beta"):
        (package / (name + ".py")).write_text("value = 7\n")
    nested = package / "nested"
    nested.mkdir()
    (nested / "__init__.py").write_text("")
    (nested / "unused.py").write_text('raise ValueError("unused")\n')
    compiler = ModuleCompiler(tmp_path / "main.py", roots=(tmp_path,))
    compile_toplevel(
        """
import importlib
unrelated = 'missing_module'
def load(name):
    target = 'plugins.' + name if name else name
    alias = target
    return importlib.import_module(alias)
""",
        imports=compiler,
    )
    assert {
        "plugins",
        "plugins.alpha",
        "plugins.beta",
        "plugins.nested",
    } <= compiler.modules.keys()
    assert "plugins.nested.unused" not in compiler.modules
    assert "missing_module" not in compiler.modules


def test_package_inventory_invalidates_cache_when_module_is_added(tmp_path):
    (tmp_path / "first.py").write_text("")
    with recording() as inputs:
        assert directory_entries(tmp_path) == ["first.py"]
    assert unchanged(inputs)
    (tmp_path / "second.py").write_text("")
    assert not unchanged(inputs)
    with recording() as inputs:
        directory_entries(tmp_path)
    (tmp_path / "second.py").unlink()
    (tmp_path / "second.py").mkdir()
    assert not unchanged(inputs)


def test_namespace_resolution_merges_roots_and_prefers_regular_packages(tmp_path):
    roots = (tmp_path / "first", tmp_path / "second")
    for root in roots:
        (root / "plugins").mkdir(parents=True)
    (roots[0] / "plugins" / "alpha.py").write_text("")
    (roots[1] / "plugins" / "beta.py").write_text("")
    compiler = ModuleCompiler(tmp_path / "main.py", roots=roots)
    compiler.include_patterns(["plugins."])
    assert compiler.modules["plugins"].namespace_paths == tuple(
        str(root / "plugins") for root in roots
    )
    assert {"plugins.alpha", "plugins.beta"} <= compiler.modules.keys()
    (roots[1] / "plugins" / "__init__.py").write_text("")
    compiler = ModuleCompiler(tmp_path / "main.py", roots=roots)
    compiler.include_patterns(["plugins."])
    assert not compiler.modules["plugins"].namespace_paths
    assert "plugins.alpha" not in compiler.modules
    assert "plugins.beta" in compiler.modules


def test_project_dependencies_and_namespace_changes_are_cache_inputs(tmp_path):
    project = tmp_path / "pyproject.toml"
    project.write_text('[tool.mule]\ninclude_modules = ["plugins.chosen"]\n')
    (tmp_path / "plugins").mkdir()
    (tmp_path / "plugins" / "chosen.py").write_text("")
    with recording() as inputs:
        compiler = ModuleCompiler(tmp_path / "main.py", roots=(tmp_path,))
        compiler.configure_project()
        compiler.include_patterns(compiler.project_modules)
    assert compiler.project_modules == ("plugins.chosen",)
    assert "plugins.chosen" in compiler.modules
    assert unchanged(inputs)
    (tmp_path / "plugins" / "__init__.py").write_text("")
    assert not unchanged(inputs)
    with recording() as inputs:
        compiler.configure_project()
    project.write_text("[tool.mule]\ninclude_modules = []\n")
    assert not unchanged(inputs)


def test_project_package_pattern_links_only_immediate_artifacts(tmp_path):
    (tmp_path / "pyproject.toml").write_text(
        '[tool.mule]\ninclude_modules = ["plugins.*"]\n'
    )
    package = tmp_path / "plugins"
    package.mkdir()
    (package / "alpha.py").write_text('raise AssertionError("not initialized")\n')
    nested = package / "nested"
    nested.mkdir()
    (nested / "unused.py").write_text("")
    with recording() as inputs:
        compiler = ModuleCompiler(tmp_path / "main.py", roots=(tmp_path,))
        compiler.configure_project()
        compiler.include_patterns(compiler.project_modules)
    assert compiler.project_modules == ("plugins.",)
    assert {"plugins", "plugins.alpha", "plugins.nested"} == compiler.modules.keys()
    assert unchanged(inputs)
    (package / "beta.py").write_text("")
    assert not unchanged(inputs)


@pytest.mark.parametrize("name", ["*", "plugins.*.*", "plugins.", ".plugins.*"])
def test_project_rejects_invalid_module_patterns(tmp_path, name):
    (tmp_path / "pyproject.toml").write_text(
        '[tool.mule]\ninclude_modules = ["' + name + '"]\n'
    )
    compiler = ModuleCompiler(tmp_path / "main.py", roots=(tmp_path,))
    with pytest.raises(ValueError, match="invalid module dependency"):
        compiler.configure_project()


@pytest.mark.integration
def test_literal_and_prefixed_imports_load_after_sources_are_deleted(tmp_path):
    package = tmp_path / "plugins"
    package.mkdir()
    (package / "__init__.py").write_text("print('package')\n")
    (package / "alpha.py").write_text("print('alpha')\nvalue = 42\n")
    (package / "unused.py").write_text(
        "raise ValueError('unused module initialized')\n"
    )
    (tmp_path / "literal.py").write_text("print('literal')\nvalue = 7\n")
    source = tmp_path / "main.py"
    source.write_text("""
import importlib
def choose(name):
    target = 'plugins.' + name if name else name
    return importlib.import_module(target)
print(choose('alpha').value)
print(importlib.import_module('literal').value)
print(choose('alpha') is choose('alpha'))
""")
    oracle = subprocess.run(
        [python_host(), source],
        capture_output=True,
        text=True,
        check=False,
        timeout=10,
    )
    assert oracle.returncode == 0, oracle.stderr
    executable = build(source, tmp_path / "program", optimize=False, workers=1)
    for path in (*package.glob("*.py"), source, tmp_path / "literal.py"):
        path.unlink()
    native = subprocess.run(
        [executable], capture_output=True, text=True, check=False, timeout=10
    )
    assert native.returncode == 0, native.stderr
    assert native.stdout == oracle.stdout


def _unit_imports(tmp_path, name, source):
    """The modules the unit of ``source`` links, compiled as module ``name``,
    or as the main module when ``name`` is None."""
    from mule.compiler.driver import cached_builtins_interface, compile_unit

    interface = cached_builtins_interface()
    filename = tmp_path / ((name or "main") + ".py")
    filename.write_text(source)
    module = None
    if name is not None:
        module = ModuleCompiler(str(filename), roots=(tmp_path,)).resolve(
            name, None, None
        )
    _, imported = compile_unit(
        source, str(filename), interface, module=module, roots=(tmp_path,)
    )
    return {descriptor.name for descriptor in imported}


def test_main_guard_imports_link_only_in_the_main_module(tmp_path):
    for name in ("selftest", "support"):
        (tmp_path / (name + ".py")).write_text("")
    source = (
        "import support\n"
        "def run():\n"
        " return 1\n"
        'if __name__ == "__main__":\n'
        " import selftest\n"
        " run()\n"
    )
    assert _unit_imports(tmp_path, "tool", source) == {"support"}
    assert _unit_imports(tmp_path, None, source) == {"support", "selftest"}


def test_rebound_module_name_keeps_guarded_imports(tmp_path):
    (tmp_path / "selftest.py").write_text("")
    for source in (
        '__name__ = "__main__"\nif __name__ == "__main__":\n import selftest\n',
        (
            "def rename():\n global __name__\n __name__ = '__main__'\n"
            'rename()\nif __name__ == "__main__":\n import selftest\n'
        ),
    ):
        assert _unit_imports(tmp_path, "tool", source) == {"selftest"}


def test_module_name_folds_to_the_unit_name(tmp_path):
    from mule.compiler import emit_llvm

    llvm = emit_llvm(
        'if __name__ == "__main__":\n print("main")\nelse:\n print("imported")\n',
        str(tmp_path / "main.py"),
        roots=(tmp_path,),
    )
    assert "imported" not in llvm
    assert "mule_compare" not in llvm.split('define ptr @"mule_module"')[-1]
