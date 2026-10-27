"""Physical reference slots keep descriptor-independent storage semantics."""

import os
import subprocess

import pytest

from mule.compiler import build
from mule.compiler.codegen import ModuleGenerator, Program, compile_component
from mule.compiler.protocols import compile_protocol
from test.python_host import python_host

_PROBE = """
events = []
class Box:
    __slots__ = ("field",)
    def read(self):
        return _instance_field(self, "field")
    def write(self, value):
        return _set_instance_field(self, "field", value)
    def read_effect(self):
        return _instance_field(self, name())
def name():
    events.append("name")
    return "field"
def read_any(value, attribute):
    return _instance_field(value, attribute)
"""


@pytest.mark.parametrize(
    ("method", "operation"),
    [("read", "physical-field-read"), ("write", "physical-field-write")],
)
def test_literal_field_selects_general_fixed_slot_vop(method, operation):
    compilation = compile_protocol(
        _PROBE, "<physical-probe>", namespace="$mule.physical_probe"
    )
    module = ModuleGenerator(Program(compilation.lambdas, None, compilation.facts))
    function = next(
        function
        for function in compilation.lambdas
        if function.code is not None and function.code.qualname == f"Box.{method}"
    )
    _, variants = module.contexts(function)
    assert variants
    component = compile_component(function, variants[0])
    accesses = [
        step
        for block in component.blocks
        for step in block.vops
        if step.info.name == operation
    ]
    assert accesses
    assert all(step.codegen_info.name == "field" for step in accesses)
    assert all(step.codegen_info.index == 0 for step in accesses)
    assert all(step.args[0].tn.type.fixed_slot_index("field") == 0 for step in accesses)


def test_inferred_member_receiver_keeps_physical_runtime_guard():
    compilation = compile_protocol(
        _PROBE
        + """
class Envelope:
    def __init__(self):
        self.child = Box()
    def read(self):
        child = self.child
        if child is None:
            return None
        return _instance_field(child, "field")
""",
        "<physical-probe>",
        namespace="$mule.physical_probe",
    )
    module = ModuleGenerator(Program(compilation.lambdas, None, compilation.facts))
    function = next(
        function
        for function in compilation.lambdas
        if function.code is not None and function.code.qualname == "Envelope.read"
    )
    _, variants = module.contexts(function)
    assert variants
    component = compile_component(function, variants[0])
    accesses = [
        step
        for block in component.blocks
        for step in block.vops
        if step.info.name == "physical-field-read-guarded"
    ]
    assert accesses
    assert any(step.args[0].tn.type.fixed_slot_index("field") == 0 for step in accesses)


def _compare_physical_program(tmp_path, source):
    probe = tmp_path / "_physical_probe.py"
    probe.write_text(
        "from _mule_native import instance_field as _instance_field\n"
        "from _mule_native import set_instance_field as _set_instance_field\n" + _PROBE
    )
    main = tmp_path / "main.py"
    main.write_text(source)
    executable = tmp_path / "program"
    build(main, executable, optimize=1, workers=1)
    environment = os.environ.copy()
    environment["LC_ALL"] = "C.UTF-8"
    native = subprocess.run(
        [executable],
        capture_output=True,
        text=True,
        check=False,
        timeout=30,
        cwd=tmp_path,
        env=environment,
    )
    # CPython's saved member descriptor is the authoritative physical-slot
    # adapter. It remains usable after Box.field becomes a different descriptor.
    reference = _PROBE.replace("_set_instance_field(", "_reference_write(").replace(
        "_instance_field(", "_reference_read("
    )
    reference += """
_saved = Box.field
def _reference_read(value, attribute):
    if attribute != "field":
        raise AttributeError(attribute)
    return _saved.__get__(value, type(value))
def _reference_write(value, attribute, replacement):
    if attribute != "field":
        raise AttributeError(attribute)
    _saved.__set__(value, replacement)
"""
    probe.write_text(reference)
    oracle = subprocess.run(
        [python_host(), main],
        capture_output=True,
        text=True,
        check=False,
        timeout=30,
        cwd=tmp_path,
        env=environment,
    )
    assert oracle.returncode == 0, oracle.stderr
    assert native.returncode == 0, native.stderr
    assert native.stdout == oracle.stdout


def test_physical_slots_bypass_descriptors_and_preserve_unset_and_none(tmp_path):
    _compare_physical_program(
        tmp_path,
        """
from _physical_probe import Box, events, read_any
box = Box()
try:
    box.read()
except AttributeError:
    print("unset")
print(box.write(None), box.read() is None)
value = [1, 2]
box.write(value)
Box.field = property(lambda self: 99)
print(box.field, box.read() is value)
print(box.read_effect() is value, events)
box.write([3])
print(box.field, box.read())
for receiver, attribute in ((box, "missing"), (7, "field")):
    try:
        read_any(receiver, attribute)
    except AttributeError:
        print("missing")
    except TypeError:
        print("wrong receiver")
""",
    )


@pytest.mark.integration
def test_physical_read_owns_result_until_caller_releases_it(tmp_path):
    _compare_physical_program(
        tmp_path,
        """
from weakref import ref
from _physical_probe import Box
class Payload:
    pass
box = Box()
box.write(Payload())
events = []
watch = ref(box.read(), lambda ignored: events.append("released"))
held = box.read()
box.write(None)
print(watch() is held, events)
del held
print(watch() is None, events)
""",
    )
