"""Lower Mule's control-flow IR to native LLVM functions.

Each temporary and local owns a runtime reference in an entry-block slot.
Taking a temporary clears its slot before borrowing it to a runtime operation;
the operation's result is published before its inputs are released. This also
leaves every still-live operand available to the shared error cleanup path.
"""

from __future__ import annotations

import struct

from mule import ast
from mule.compiler import ir as ir1

_BINARY = {
    ast.BinaryOperator.ADD: 0,
    ast.BinaryOperator.SUBTRACT: 1,
    ast.BinaryOperator.MULTIPLY: 2,
    ast.BinaryOperator.DIVIDE: 3,
    ast.BinaryOperator.FLOOR_DIVIDE: 4,
    ast.BinaryOperator.MODULO: 5,
    ast.BinaryOperator.MATRIX_MULTIPLY: 6,
    ast.BinaryOperator.POWER: 7,
    ast.BinaryOperator.LEFT_SHIFT: 8,
    ast.BinaryOperator.RIGHT_SHIFT: 9,
    ast.BinaryOperator.BIT_AND: 10,
    ast.BinaryOperator.BIT_XOR: 11,
    ast.BinaryOperator.BIT_OR: 12,
}
_UNARY = {
    ast.UnaryOperator.POSITIVE: 0,
    ast.UnaryOperator.NEGATIVE: 1,
    ast.UnaryOperator.INVERT: 2,
    ast.UnaryOperator.NOT: 3,
}
_COMPARE = {
    ast.ComparisonOperator.EQUAL: 0,
    ast.ComparisonOperator.NOT_EQUAL: 1,
    ast.ComparisonOperator.LESS: 2,
    ast.ComparisonOperator.LESS_EQUAL: 3,
    ast.ComparisonOperator.GREATER: 4,
    ast.ComparisonOperator.GREATER_EQUAL: 5,
    ast.ComparisonOperator.IS: 6,
    ast.ComparisonOperator.IS_NOT: 7,
    ast.ComparisonOperator.IN: 8,
    ast.ComparisonOperator.NOT_IN: 9,
}


def _decimal(value: int) -> str:
    """Render arbitrary-size integers without the host's digit conversion cap."""
    negative = value < 0
    value = abs(value)
    chunks = []
    while value:
        value, chunk = divmod(value, 1_000_000_000)
        chunks.append(chunk)
    if not chunks:
        return "0"
    return (
        ("-" if negative else "")
        + str(chunks[-1])
        + "".join(f"{chunk:09d}" for chunk in reversed(chunks[:-1]))
    )


_DECLARATIONS = """declare ptr @mule_literal_int(ptr)
declare ptr @mule_literal_float(double)
declare ptr @mule_literal_text(ptr, i64)
declare ptr @mule_literal_bytes(ptr, i64)
declare ptr @mule_none()
declare ptr @mule_bool(i32)
declare ptr @mule_load_global(ptr, ptr)
declare i32 @mule_store_global(ptr, ptr, ptr)
declare ptr @mule_load_local(ptr, ptr)
declare ptr @mule_binary(i32, ptr, ptr, i32)
declare ptr @mule_unary(i32, ptr)
declare ptr @mule_compare(i32, ptr, ptr)
declare i32 @mule_truth(ptr)
declare ptr @mule_call(ptr, i64, ptr)
declare ptr @mule_function(ptr, ptr, ptr)
declare i32 @mule_check_args(ptr, i32)
declare ptr @mule_arg(ptr, i32)
declare void @mule_release(ptr)
declare i32 @mule_run(ptr)
"""


def emit(module: ir1.Module) -> str:
    """Return a complete LLVM module with a native executable entry point."""
    ir1.verify(module)
    return _ModuleEmitter(module).emit()


class _ModuleEmitter:
    def __init__(self, module: ir1.Module):
        self.module = module
        self.functions = {
            function.name: f"@mule.fn.{index}"
            for index, function in enumerate(module.functions)
        }
        self.data: dict[bytes, str] = {}

    def literal_data(self, value: bytes) -> str:
        """Produce a NUL-terminated global, retaining embedded NUL bytes."""
        if value not in self.data:
            self.data[value] = f"@mule.data.{len(self.data)}"
        return (
            f"getelementptr inbounds ([{len(value) + 1} x i8], "
            f"ptr {self.data[value]}, i64 0, i64 0)"
        )

    def text(self, value: str) -> str:
        return self.literal_data(value.encode("utf-8"))

    def emit(self) -> str:
        functions = [
            _FunctionEmitter(self, function).emit()
            for function in self.module.functions
        ]
        globals_ = []
        for value, name in self.data.items():
            escaped = "".join(f"\\{byte:02X}" for byte in value + b"\0")
            globals_.append(
                f"{name} = private unnamed_addr constant "
                f'[{len(value) + 1} x i8] c"{escaped}"'
            )
        entry = self.functions[self.module.entry]
        main = (
            "define i32 @main() {\n"
            "entry:\n"
            f"  %status = call i32 @mule_run(ptr {entry})\n"
            "  ret i32 %status\n"
            "}"
        )
        return "\n\n".join([_DECLARATIONS, *globals_, *functions, main]) + "\n"


class _FunctionEmitter:
    def __init__(self, module: _ModuleEmitter, function: ir1.Function):
        self.module = module
        self.function = function
        self.lines: list[str] = []
        self.serial = 0
        self.local_slots = {
            name: f"%local.{index}" for index, name in enumerate(function.locals)
        }
        results = dict.fromkeys(
            instruction.result
            for block in function.blocks
            for instruction in block.instructions
            if hasattr(instruction, "result")
        )
        self.value_slots = {
            name: f"%value.{index}" for index, name in enumerate(results)
        }
        self.labels = {
            block.label: f"block.{index}" for index, block in enumerate(function.blocks)
        }

    def line(self, text: str) -> None:
        self.lines.append("  " + text)

    def fresh(self, prefix: str = "t") -> str:
        self.serial += 1
        return f"%{prefix}.{self.serial}"

    def operation(self, text: str) -> str:
        value = self.fresh()
        self.line(f"{value} = {text}")
        return value

    def guard(self, failed: str) -> None:
        continuation = self.fresh("ok")[1:]
        self.line(f"br i1 {failed}, label %error, label %{continuation}")
        self.lines.append(continuation + ":")

    def check_pointer(self, value: str) -> None:
        self.guard(self.operation(f"icmp eq ptr {value}, null"))

    def check_status(self, value: str) -> None:
        self.guard(self.operation(f"icmp slt i32 {value}, 0"))

    def take(self, value: str) -> str:
        slot = self.value_slots[value]
        result = self.operation(f"load ptr, ptr {slot}")
        self.line(f"store ptr null, ptr {slot}")
        return result

    def release(self, value: str) -> None:
        self.line(f"call void @mule_release(ptr {value})")

    def publish(self, result: str, value: str) -> None:
        slot = self.value_slots[result]
        old = self.operation(f"load ptr, ptr {slot}")
        self.line(f"store ptr {value}, ptr {slot}")
        self.release(old)

    def pointer_result(self, result: str, operation: str, consumed=()) -> None:
        value = self.operation(operation)
        self.publish(result, value)
        for operand in consumed:
            self.release(operand)
        self.check_pointer(value)

    def emit(self) -> str:
        self.lines.append(
            f"define ptr {self.module.functions[self.function.name]}"
            "(ptr %globals, ptr %args) {"
        )
        self.lines.append("entry:")
        self.line("%return = alloca ptr")
        self.line("store ptr null, ptr %return")
        for slot in [*self.local_slots.values(), *self.value_slots.values()]:
            self.line(f"{slot} = alloca ptr")
            self.line(f"store ptr null, ptr {slot}")
        max_args = max(
            (
                len(instruction.args)
                for block in self.function.blocks
                for instruction in block.instructions
                if isinstance(instruction, ir1.Call)
            ),
            default=0,
        )
        self.line(f"%argv = alloca [{max(1, max_args)} x ptr]")
        status = self.operation(
            f"call i32 @mule_check_args(ptr %args, i32 {len(self.function.parameters)})"
        )
        self.check_status(status)
        for index, parameter in enumerate(self.function.parameters):
            value = self.operation(f"call ptr @mule_arg(ptr %args, i32 {index})")
            self.line(f"store ptr {value}, ptr {self.local_slots[parameter]}")
            self.check_pointer(value)
        self.line(f"br label %{self.labels[self.function.blocks[0].label]}")
        for block in self.function.blocks:
            self.lines.append(self.labels[block.label] + ":")
            for instruction in block.instructions:
                self.instruction(instruction)
            self.terminator(block.terminator)
        self.lines.append("error:")
        self.line("br label %cleanup")
        self.lines.append("cleanup:")
        for slot in [*self.local_slots.values(), *self.value_slots.values()]:
            self.release(self.operation(f"load ptr, ptr {slot}"))
        value = self.operation("load ptr, ptr %return")
        self.line(f"ret ptr {value}")
        self.lines.append("}")
        return "\n".join(self.lines)

    def instruction(self, instruction: ir1.Instruction) -> None:
        match instruction:
            case ir1.Constant(result=result, value=value):
                self.pointer_result(result, self.constant(value))
            case ir1.Load(result=result, name=name, local=local, clear=clear):
                name_pointer = self.module.text(name)
                if local:
                    slot = self.local_slots[name]
                    value = self.operation(f"load ptr, ptr {slot}")
                    if clear:
                        self.line(f"store ptr null, ptr {slot}")
                        self.publish(result, value)
                        self.check_pointer(value)
                        return
                    operation = (
                        f"call ptr @mule_load_local(ptr {value}, ptr {name_pointer})"
                    )
                else:
                    operation = (
                        f"call ptr @mule_load_global(ptr %globals, ptr {name_pointer})"
                    )
                self.pointer_result(result, operation)
            case ir1.Store(name=name, value=operand, local=local):
                value = self.take(operand)
                if local:
                    slot = self.local_slots[name]
                    old = self.operation(f"load ptr, ptr {slot}")
                    self.line(f"store ptr {value}, ptr {slot}")
                    self.release(old)
                else:
                    status = self.operation(
                        "call i32 @mule_store_global(ptr %globals, "
                        f"ptr {self.module.text(name)}, ptr {value})"
                    )
                    self.release(value)
                    self.check_status(status)
            case ir1.Discard(value=operand):
                self.release(self.take(operand))
            case ir1.Binary(
                result=result,
                operator=operator,
                left=left,
                right=right,
                inplace=inplace,
            ):
                lhs, rhs = self.take(left), self.take(right)
                self.pointer_result(
                    result,
                    f"call ptr @mule_binary(i32 {_BINARY[operator]}, ptr {lhs}, "
                    f"ptr {rhs}, i32 {int(inplace)})",
                    (lhs, rhs),
                )
            case ir1.Unary(result=result, operator=operator, operand=operand):
                value = self.take(operand)
                self.pointer_result(
                    result,
                    f"call ptr @mule_unary(i32 {_UNARY[operator]}, ptr {value})",
                    (value,),
                )
            case ir1.Compare(result=result, operator=operator, left=left, right=right):
                lhs, rhs = self.take(left), self.take(right)
                self.pointer_result(
                    result,
                    f"call ptr @mule_compare(i32 {_COMPARE[operator]}, ptr {lhs}, ptr {rhs})",
                    (lhs, rhs),
                )
            case ir1.Call(result=result, callee=callee, args=args):
                function = self.take(callee)
                values = [self.take(argument) for argument in args]
                for index, value in enumerate(values):
                    slot = self.operation(f"getelementptr ptr, ptr %argv, i64 {index}")
                    self.line(f"store ptr {value}, ptr {slot}")
                self.pointer_result(
                    result,
                    f"call ptr @mule_call(ptr {function}, i64 {len(args)}, ptr %argv)",
                    (function, *values),
                )
            case ir1.DefineFunction(result=result, function=function, public_name=name):
                self.pointer_result(
                    result,
                    "call ptr @mule_function(ptr %globals, "
                    f"ptr {self.module.functions[function]}, ptr {self.module.text(name)})",
                )
            case _:
                raise TypeError(f"Unsupported LLVM instruction: {instruction!r}")

    def constant(self, value) -> str:
        if value is None:
            return "call ptr @mule_none()"
        if isinstance(value, bool):
            return f"call ptr @mule_bool(i32 {int(value)})"
        if isinstance(value, int):
            if -(1 << 62) <= value < (1 << 62):
                tagged = ((value << 1) | 1) & ((1 << 64) - 1)
                return f"inttoptr i64 {tagged} to ptr"
            return (
                f"call ptr @mule_literal_int(ptr {self.module.text(_decimal(value))})"
            )
        if isinstance(value, float):
            bits = struct.unpack(">Q", struct.pack(">d", value))[0]
            return f"call ptr @mule_literal_float(double 0x{bits:016X})"
        if isinstance(value, str):
            encoded = value.encode("utf-8", "surrogatepass")
            return (
                f"call ptr @mule_literal_text(ptr {self.module.literal_data(encoded)}, "
                f"i64 {len(encoded)})"
            )
        if isinstance(value, bytes):
            return (
                f"call ptr @mule_literal_bytes(ptr {self.module.literal_data(value)}, "
                f"i64 {len(value)})"
            )
        raise TypeError(f"Unsupported LLVM constant: {value!r}")

    def terminator(self, terminator) -> None:
        match terminator:
            case ir1.Jump(target=target):
                self.line(f"br label %{self.labels[target]}")
            case ir1.Branch(condition=condition, true=true, false=false):
                value = self.take(condition)
                truth = self.operation(f"call i32 @mule_truth(ptr {value})")
                self.release(value)
                self.check_status(truth)
                condition = self.operation(f"icmp ne i32 {truth}, 0")
                self.line(
                    f"br i1 {condition}, label %{self.labels[true]}, "
                    f"label %{self.labels[false]}"
                )
            case ir1.Return(value=value):
                result = self.take(value)
                self.line(f"store ptr {result}, ptr %return")
                self.line("br label %cleanup")
            case _:
                raise TypeError(f"Unsupported LLVM terminator: {terminator!r}")
