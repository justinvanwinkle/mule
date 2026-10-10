"""Bind physical OS primitives only in the reserved compiled OS module."""

from .protocols import Bindings, Primitive


class OSPrimitive(Primitive):
    def __init__(self, symbol):
        self.symbol = symbol

    def emit(self, emitter, arguments):
        operands = ", ".join(f"ptr {value}" for value in arguments)
        return emitter.operation(f"call ptr @{self.symbol}({operands})")


def prepare(module):
    primitives = {
        "_process_argument_count": OSPrimitive("mule_process_argument_count"),
        "_process_argument": OSPrimitive("mule_process_argument"),
    }
    calls, references = {}, set()
    for function in module.functions:
        if function.namespace != "_mule_os":
            continue
        collector = Bindings(primitives)
        for block in function.blocks:
            for operation in block.instructions:
                operation.accept(collector)
        calls.update(
            ((function.name, result), primitive)
            for result, primitive in collector.calls.items()
        )
        references.update((function.name, result) for result in collector.references)
    return calls, references
