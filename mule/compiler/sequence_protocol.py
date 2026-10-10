"""Compile sequence/exception language rules and bind physical storage operations."""

from pathlib import Path

from . import ir
from .format_protocol import LookupMethod, TextAccess
from .frontend import lower
from .protocols import Bindings


class NeedsProtocols(ir.Visitor):
    def __init__(self):
        self.needed = False

    def visit_ProtocolCall(self, operation):
        self.needed = True

    def visit_Raise(self, operation):
        self.needed = True

    def visit_CatchException(self, operation):
        self.needed = True

    def visit_Sequence(self, operation):
        self.needed = True

    def visit_Load(self, operation):
        if not operation.local and operation.name in (
            "range",
            "enumerate",
            "iter",
            "next",
            "list",
            "tuple",
            "chr",
            "BaseException",
            "Exception",
            "StopIteration",
            "IndexError",
            "KeyError",
            "ValueError",
            "TypeError",
            "RuntimeError",
            "ArithmeticError",
            "LookupError",
            "NameError",
            "UnboundLocalError",
            "ZeroDivisionError",
            "OverflowError",
            "AssertionError",
            "MemoryError",
            "SystemError",
            "AttributeError",
            "OSError",
            "EOFError",
            "RecursionError",
            "UnicodeError",
            "UnicodeEncodeError",
            "UnicodeDecodeError",
        ):
            self.needed = True


class Exports(ir.Visitor):
    def __init__(self):
        self.functions = {}

    def visit_DefineFunction(self, operation):
        self.functions[operation.public_name] = operation.function


def prepare(module):
    visitor = NeedsProtocols()
    for function in module.functions:
        for block in function.blocks:
            for operation in (*block.instructions, block.terminator):
                operation.accept(visitor)
    if not visitor.needed:
        return None, {}, {}, set()
    path = Path(__file__).resolve().parents[2] / "stdlib/_sequences.py"
    lowered = lower(
        path.read_text(),
        str(path),
        prefix="mule_sequences_",
        namespace="$mule.sequences",
    )
    exports = Exports()
    for block in lowered.functions[0].blocks:
        for operation in block.instructions:
            operation.accept(exports)
    primitives = {
        name: LookupMethod(method)
        for name, method in (
            ("_getitem_method", "__getitem__"),
            ("_setitem_method", "__setitem__"),
            ("_iter_method", "__iter__"),
            ("_next_method", "__next__"),
            ("_index_method", "__index__"),
        )
    }
    primitives.update(
        {
            name: TextAccess(primitive)
            for name, primitive in (
                ("_storage_length", "mule_storage_length"),
                ("_storage_read", "mule_storage_read"),
                ("_storage_write", "mule_storage_write"),
                ("_storage_append", "mule_storage_append"),
                ("_storage_tuple", "mule_storage_tuple"),
                ("_storage_compatible", "mule_storage_compatible"),
                ("_integer_storage", "mule_integer_storage"),
                ("_text_from_codepoint", "mule_text_from_codepoint"),
                ("_class_identity", "mule_class_identity"),
                ("_class_parent", "mule_class_parent"),
                ("_raised_class", "mule_raised_class"),
                ("_exception_targets", "mule_exception_targets"),
                ("_raise_pending", "mule_raise_pending"),
                ("_invalid_raise", "mule_invalid_raise"),
                ("_install_builtin", "mule_install_builtin"),
                ("_install_storage", "mule_install_storage"),
            )
        }
    )
    calls, references = {}, set()
    for function in lowered.functions:
        collector = Bindings(primitives)
        for block in function.blocks:
            for operation in block.instructions:
                operation.accept(collector)
        calls.update(
            ((function.name, result), primitive)
            for result, primitive in collector.calls.items()
        )
        references.update((function.name, result) for result in collector.references)
    module.functions.extend(lowered.functions)
    return lowered.entry, exports.functions, calls, references
