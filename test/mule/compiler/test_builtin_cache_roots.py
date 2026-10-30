"""Cached bootstrap outputs retain the protocol sources of their checkout."""

from mule.compiler import driver
from mule.compiler.artifacts import Graph
from mule.compiler.inputs import read_source


def test_builtin_cache_uses_each_checkout_protocols_and_invalidates_edits(
    tmp_path, monkeypatch
):
    monkeypatch.setenv("XDG_CACHE_HOME", str(tmp_path / "cache"))
    roots = [tmp_path / "first", tmp_path / "second"]
    for root, content in zip(roots, ("first protocols", "second protocols")):
        root.mkdir()
        (root / "_str.py").write_text(content)
    runs = []

    class Interface:
        def to_json(self):
            return "{}"

    class DummyUnit:
        def __init__(self, text):
            self.text = text
            self.interface_bytes = None

    def compile_protocols():
        runs.append(driver.STDLIB)
        return DummyUnit(read_source(driver.STDLIB / "_str.py")), {}

    monkeypatch.setattr(driver, "compile_builtins", compile_protocols)
    monkeypatch.setattr(driver, "builtins_interface", lambda unit, modules: Interface())
    monkeypatch.setattr(driver, "generate", lambda unit: unit.text)

    def output(root):
        monkeypatch.setattr(driver, "STDLIB", root)
        with Graph(1) as graph:
            step = driver.Build(graph, driver.Toolchain(optimize=False)).builtins()
            graph.run(step)
            return step.path("unit.ll").read_text()

    assert output(roots[0]) == "first protocols"
    assert output(roots[1]) == "second protocols"
    assert output(roots[0]) == "first protocols"
    assert runs == roots
    (roots[0] / "_str.py").write_text("edited protocols")
    assert output(roots[0]) == "edited protocols"
    assert runs == [*roots, roots[0]]
