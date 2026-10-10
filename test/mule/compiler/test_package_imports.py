"""Package-relative imports and runtime from-import binding precedence."""

import pytest

from mule.compiler import UnsupportedSyntax, emit_llvm
from test.mule.compiler.test_modules import execute


def test_relative_siblings_parent_packages_and_from_submodule_imports(tmp_path):
    result = execute(
        tmp_path,
        {
            "package/__init__.py": "value = 7\n",
            "package/child.py": "from . import value\nprint('child init')\ndef read():\n return value\n",
            "package/nested/__init__.py": "from ..child import read\n",
            "package/nested/reader.py": "from .. import child\nfrom . import read\nprint(child.read(), read())\n",
        },
        "from package import child\nimport package.nested.reader\nprint(child.__name__)\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "child init\n7 7\npackage.child\n"


def test_existing_package_binding_including_none_precedes_submodule(tmp_path):
    result = execute(
        tmp_path,
        {
            "package/__init__.py": "child = None\n",
            "package/child.py": "print('child initialized')\nvalue = 42\n",
        },
        "from package import child\nprint(child)\n"
        "import package.child as module\nfrom package import child as imported\n"
        "print(imported is module, imported.value)\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "None\nchild initialized\nTrue 42\n"


def test_relative_import_without_package_is_rejected():
    with pytest.raises(UnsupportedSyntax, match="no known parent package"):
        emit_llvm("from . import sibling\n")


def test_relative_import_cannot_escape_package(tmp_path):
    (tmp_path / "package").mkdir()
    (tmp_path / "package/__init__.py").write_text("from .. import outside\n")
    with pytest.raises(UnsupportedSyntax, match="beyond top-level package"):
        emit_llvm("import package\n", str(tmp_path / "main.py"))
