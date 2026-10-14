"""Compilation uses the active interpreter's module source search paths."""

import sys

from mule.compiler.modules import STDLIB, find_module, search_path


def test_source_roots_preserve_precedence_and_runtime_paths(tmp_path, monkeypatch):
    script = tmp_path / "script"
    extra = tmp_path / "extra"
    installed = tmp_path / "installed"
    monkeypatch.setenv("PYTHONPATH", str(extra))
    monkeypatch.setattr(sys, "path", [str(installed), str(extra), str(STDLIB)])
    assert search_path(script) == (
        script.resolve(),
        extra.resolve(),
        (STDLIB / "bootstrap").resolve(),
        STDLIB.resolve(),
        installed.resolve(),
    )


def test_runtime_source_path_can_resolve_a_new_module(tmp_path, monkeypatch):
    installed = tmp_path / "installed"
    installed.mkdir()
    source = installed / "runtime_module.py"
    source.write_text("answer = 42\n")
    monkeypatch.delenv("PYTHONPATH", raising=False)
    monkeypatch.setattr(sys, "path", [str(installed)])
    path, package = find_module("runtime_module", search_path(tmp_path / "script"))
    assert path == source
    assert package == ""


def test_empty_runtime_path_entry_uses_current_directory(tmp_path, monkeypatch):
    monkeypatch.chdir(tmp_path)
    monkeypatch.delenv("PYTHONPATH", raising=False)
    monkeypatch.setattr(sys, "path", ["", str(tmp_path)])
    roots = search_path(tmp_path / "script")
    assert roots[-1] == tmp_path
    assert roots.count(tmp_path) == 1
