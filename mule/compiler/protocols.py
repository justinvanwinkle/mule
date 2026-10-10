"""Shared bindings for compiler-private, compiled Mule language protocols."""

from . import ir


class Primitive:
    def emit(self, emitter, arguments):
        raise NotImplementedError


class Bindings(ir.Visitor):
    def __init__(self, primitives):
        self.primitives = primitives
        self.references = set()
        self.calls = {}
        self.values = {}

    def visit_Load(self, op):
        if not op.local and op.name in self.primitives:
            self.values[op.result] = self.primitives[op.name]
            self.references.add(op.result)

    def visit_Call(self, op):
        if op.callee in self.values:
            self.calls[op.result] = self.values[op.callee]


class CompiledCall(Primitive):
    def __init__(self, function, globals_value="%globals"):
        self.function = function
        self.globals_value = globals_value

    def emit(self, emitter, arguments):
        status = emitter.operation("call i32 @mule_enter_frame()")
        failed, entered, join = (
            emitter.fresh(label)[1:]
            for label in ("protocol.failed", "protocol.entered", "protocol.join")
        )
        bad = emitter.operation(f"icmp slt i32 {status}, 0")
        emitter.line(f"br i1 {bad}, label %{failed}, label %{entered}")
        emitter.lines.append(failed + ":")
        emitter.line(f"br label %{join}")
        emitter.lines.append(entered + ":")
        owned = [
            emitter.operation(f"call ptr @mule.fast.retain(ptr {value})")
            for value in arguments
        ]
        operands = ", ".join(
            [f"ptr {self.globals_value}", *(f"ptr {value}" for value in owned)]
        )
        result = emitter.operation(
            f"call ptr {emitter.module.bodies[self.function]}({operands})"
        )
        emitter.line("call void @mule_leave_frame()")
        emitter.line(f"br label %{join}")
        emitter.lines.append(join + ":")
        return emitter.operation(f"phi ptr [null, %{failed}], [{result}, %{entered}]")
