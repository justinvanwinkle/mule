"""Compile Python formatting rules and bind their physical primitives."""

from pathlib import Path

from . import ir
from .frontend import lower
from .protocols import Bindings, CompiledCall, Primitive


class LookupMethod(Primitive):
    def __init__(self, name):
        self.name = name

    def emit(self, emitter, arguments):
        return emitter.operation(
            f"call ptr @mule_lookup_special(ptr {arguments[0]}, "
            f"ptr {emitter.module.text(self.name)})"
        )


class RenderText(Primitive):
    def __init__(self, representation):
        self.representation = representation

    def emit(self, emitter, arguments):
        return emitter.operation(
            f"call ptr @mule_render_text(ptr {arguments[0]}, "
            f"i32 {int(self.representation)})"
        )


class TextResult(Primitive):
    def emit(self, emitter, arguments):
        return emitter.operation(f"call ptr @mule_text_result(ptr {arguments[0]})")


class TextAccess(Primitive):
    def __init__(self, primitive):
        self.primitive = primitive

    def emit(self, emitter, arguments):
        operands = ", ".join(f"ptr {value}" for value in arguments)
        return emitter.operation(f"call ptr @{self.primitive}({operands})")


class NumericModulo(Primitive):
    def emit(self, emitter, arguments):
        return emitter.operation(
            f"call ptr @mule.fast.binary.5(ptr {arguments[0]}, ptr {arguments[1]}, "
            "i32 0, ptr null)"
        )


class Formats(ir.Visitor):
    def __init__(self):
        self.needed = False

    def visit_FormatValue(self, operation):
        self.needed = True


def prepare(module, *, string_conversion=False):
    visitor = Formats()
    for function in module.functions:
        for block in function.blocks:
            for operation in block.instructions:
                operation.accept(visitor)
    if not visitor.needed and not string_conversion:
        return {}, set()
    path = Path(__file__).resolve().parents[2] / "stdlib/_format.py"
    lowered = lower(
        path.read_text(), str(path), prefix="mule_format_", namespace="$mule.format"
    )
    functions = lowered.functions[1:]
    names = iter(
        (
            "string",
            "representation",
            "align_string",
            "format_value",
            "decimal_integer",
            "percent_decimal",
            "modulo",
        )
    )
    for function in functions:
        function.name = "mule_format_" + next(names)
    primitives = {
        "_str_method": LookupMethod("__str__"),
        "_repr_method": LookupMethod("__repr__"),
        "_format_method": LookupMethod("__format__"),
        "_render_string": RenderText(False),
        "_render_representation": RenderText(True),
        "_text_result": TextResult(),
        "_string_protocol": CompiledCall("mule_format_string"),
        "_alignment_protocol": CompiledCall("mule_format_align_string"),
        "_decimal_protocol": CompiledCall("mule_format_decimal_integer"),
        "_percent_decimal_protocol": CompiledCall("mule_format_percent_decimal"),
        "_numeric_modulo": NumericModulo(),
        "_text_storage": TextAccess("mule_text_storage"),
        "_integer_storage": TextAccess("mule_integer_storage"),
        "_storage_compatible": TextAccess("mule_storage_compatible"),
        "_storage_length": TextAccess("mule_storage_length"),
        "_storage_read": TextAccess("mule_storage_read"),
        "_text_length": TextAccess("mule_text_length"),
        "_text_character": TextAccess("mule_text_character"),
        "_text_codepoint": TextAccess("mule_text_codepoint"),
        "_invalid_specification": TextAccess("mule_invalid_string_format"),
    }
    calls, references = {}, set()
    for function in functions:
        collector = Bindings(primitives)
        for block in function.blocks:
            for operation in block.instructions:
                operation.accept(collector)
        calls.update(
            ((function.name, result), primitive)
            for result, primitive in collector.calls.items()
        )
        references.update((function.name, result) for result in collector.references)
        module.functions.append(function)
    return calls, references
