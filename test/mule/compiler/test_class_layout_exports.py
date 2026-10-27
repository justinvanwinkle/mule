"""Compiled class owners publish layouts separately from constructor results."""

from mule import ast
from mule.compiler.codegen import ForeignFunction
from mule.compiler.constraint import _InstanceFieldType
from mule.compiler.ctype import UNKNOWN, InstanceType
from mule.compiler.driver import BuiltinsInterface, ModuleInterface
from mule.compiler.locall import ClassFacts
from mule.compiler.main import compile_source
from mule.compiler.modules import ModuleCompiler
from mule.compiler.node import ClassConstructorTarget, Signature


def _record_class(tmp_path):
    compilation = compile_source(
        "class Record:\n"
        ' __slots__ = ("field",)\n'
        " def __init__(self): self.field = 7\n"
        " def read(self): return self.field\n"
    )
    imports = ModuleCompiler(str(tmp_path / "example.py"))
    namespace = compilation.entry.namespace
    imports.record_types(namespace, compilation)
    return imports, namespace


def test_class_layout_survives_module_interface_roundtrip(tmp_path):
    imports, namespace = _record_class(tmp_path)
    target = imports.exported_targets[namespace, "Record"]
    assert target.layout_type.fixed_slot_index("field") == 0
    interface = ModuleInterface({}, {}, {}, {(namespace, "Record"): target}, {}, [])
    restored = ModuleInterface.from_json(interface.to_json())
    decoded = restored.exported_targets[namespace, "Record"]
    assert decoded.layout_type == target.layout_type
    assert decoded.base_type == target.base_type
    assert decoded.implementation_type(decoded.base_type) == decoded.layout_type
    assert decoded.implementation_type(UNKNOWN) is UNKNOWN


def test_imported_field_facts_require_actual_owner_identity(tmp_path):
    imports, namespace = _record_class(tmp_path)
    layout = imports.exported_targets[namespace, "Record"].layout_type
    expected = imports.storage_methods[layout.identity, "field"]
    query = _InstanceFieldType({}, imports.storage_methods, "field")
    assert layout.accept(query) == expected
    assert InstanceType("other", fixed_slots=("field",)).accept(query) is UNKNOWN
    assert layout.with_unbound().accept(query) is UNKNOWN
    local_owner = ClassFacts(layout.identity, {}, {})
    assert (
        layout.accept(
            _InstanceFieldType(
                {layout.identity: local_owner}, imports.storage_methods, "field"
            )
        )
        is UNKNOWN
    )


def test_missing_implementation_layout_stays_unknown():
    target = ClassConstructorTarget(base_type=InstanceType("Record"))
    assert target.implementation_type(target.base_type) is UNKNOWN


def test_resolved_entry_domains_survive_both_foreign_interfaces(tmp_path):
    imports, namespace = _record_class(tmp_path)
    layout = imports.exported_targets[namespace, "Record"].layout_type
    signature = Signature(kinds=(ast.ParameterKind.POSITIONAL_ONLY,) * 2)
    fast = ForeignFunction(
        "fast",
        ("receiver", "key"),
        signature,
        False,
        entry_parameter_types=(layout, UNKNOWN),
    )
    generic = ForeignFunction("generic", ("receiver", "key"), signature, False)
    functions = {function.name: function for function in (fast, generic)}
    module = ModuleInterface(functions, {}, {}, {}, {}, [])
    builtins = BuiltinsInterface(functions, {}, {}, {}, {}, [], {}, {}, {})
    for interface in (
        ModuleInterface.from_json(module.to_json()),
        BuiltinsInterface.from_json(builtins.to_json()),
    ):
        assert interface.functions["fast"].entry_parameter_types == (layout, UNKNOWN)
        assert interface.functions["generic"].entry_parameter_types is None
