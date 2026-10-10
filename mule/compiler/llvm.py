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
from mule.compiler import llvm_ops
from mule.compiler.analysis import UNKNOWN, Context, ValueType, analyze

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
declare ptr @mule_binary_reuse(i32, ptr, ptr, i32, ptr)
declare ptr @mule_unary(i32, ptr)
declare ptr @mule_compare(i32, ptr, ptr)
declare i32 @mule_truth(ptr)
declare ptr @mule_call(ptr, i64, ptr)
declare ptr @mule_function(ptr, ptr, ptr)
declare i32 @mule_check_args(ptr, i32)
declare ptr @mule_arg(ptr, i32)
declare ptr @mule_retain(ptr)
declare void @mule_release(ptr)
declare i32 @mule_enter_frame()
declare void @mule_leave_frame()
declare i32 @mule_function_matches(ptr, ptr, ptr)
declare i32 @mule_run(ptr)
declare void @llvm.assume(i1)
"""


def emit(module: ir1.Module) -> str:
    """Return a complete LLVM module with a native executable entry point."""
    ir1.verify(module)
    return _ModuleEmitter(module).emit()


class _ModuleEmitter:
    def __init__(self, module: ir1.Module):
        self.module = module
        self.analysis = analyze(module)
        self.functions = {
            function.name: f"@mule.fn.{index}"
            for index, function in enumerate(module.functions)
        }
        self.bodies = {
            function.name: f"@mule.body.{index}"
            for index, function in enumerate(module.functions)
        }
        self.specialized = {
            (function.name, context.id): f"@mule.body.{index}.spec.{variant}"
            for index, function in enumerate(module.functions)
            for variant, context in enumerate(self.analysis.variants[function.name])
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
        functions.extend(
            _FunctionEmitter(self, function, context).emit()
            for function in self.module.functions
            for context in self.analysis.variants[function.name]
        )
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
        return (
            "\n\n".join(
                [_DECLARATIONS, llvm_ops.helpers(), *globals_, *functions, main]
            )
            + "\n"
        )


class _FunctionEmitter:
    def __init__(
        self,
        module: _ModuleEmitter,
        function: ir1.Function,
        context: Context | None = None,
    ):
        self.module = module
        self.function = function
        self.context = context
        self.facts = context.types if context else module.analysis.types[function.name]
        self.targets = (
            context.targets if context else module.analysis.targets[function.name]
        )
        self.body = (
            module.specialized[function.name, context.id]
            if context
            else module.bodies[function.name]
        )
        self.immortal_values: set[str] = set()
        self.lines: list[str] = []
        self.serial = 0
        self.local_slots = {
            name: f"%local.{index}" for index, name in enumerate(function.locals)
        }
        parameter_facts = dict(
            zip(
                function.parameters,
                context.parameters if context else [UNKNOWN] * len(function.parameters),
                strict=True,
            )
        )
        self.immortal_locals = {
            name
            for name in function.locals
            if name not in parameter_facts
            or parameter_facts[name].is_fixnum
            or parameter_facts[name].is_immortal
        }
        for block in function.blocks:
            for instruction in block.instructions:
                if isinstance(instruction, ir1.Store) and instruction.local:
                    fact = self.facts[instruction.value]
                    if not (fact.is_fixnum or fact.is_immortal):
                        self.immortal_locals.discard(instruction.name)
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
        self.mark_immortal(value, result)
        return result

    def mark_immortal(self, result: str, value: str) -> None:
        fact = self.facts[result]
        if fact.is_fixnum or fact.is_immortal:
            self.immortal_values.add(value)

    def refine(self, result: str, value: str) -> None:
        """Expose proven representation/range facts after the error check."""
        self.mark_immortal(result, value)
        self.refine_type(self.facts[result], value)

    def refine_type(self, fact: ValueType, value: str, *, nullable=False) -> None:
        if fact.is_fixnum:
            bits = self.operation(f"ptrtoint ptr {value} to i64")
            tagged = None
            if self.context is None:
                tag = self.operation(f"and i64 {bits}, 1")
                tagged = self.operation(f"icmp ne i64 {tag}, 0")
            decoded = self.operation(f"ashr i64 {bits}, 1")
            lower = self.operation(f"icmp sge i64 {decoded}, {fact.minimum}")
            upper = self.operation(f"icmp sle i64 {decoded}, {fact.maximum}")
            bounded = self.operation(f"and i1 {lower}, {upper}")
            valid = (
                self.operation(f"and i1 {tagged}, {bounded}")
                if tagged is not None
                else bounded
            )
            if nullable:
                missing = self.operation(f"icmp eq ptr {value}, null")
                valid = self.operation(f"or i1 {missing}, {valid}")
            self.line(f"call void @llvm.assume(i1 {valid})")

    def release(self, value: str) -> None:
        if value in self.immortal_values:
            return
        self.line(f"call void @mule.fast.release(ptr {value})")

    def publish(self, result: str, value: str) -> None:
        slot = self.value_slots[result]
        old = self.operation(f"load ptr, ptr {slot}")
        self.line(f"store ptr {value}, ptr {slot}")
        self.mark_immortal(result, old)
        self.mark_immortal(result, value)
        self.release(old)

    def pointer_result(self, result: str, operation: str, consumed=()) -> None:
        value = self.operation(operation)
        self.publish(result, value)
        for operand in consumed:
            self.release(operand)
        self.check_pointer(value)
        self.refine(result, value)

    def emit(self) -> str:
        wrapper = self.wrapper() if self.context is None else ""
        parameters = ", ".join(
            ["ptr %globals"]
            + [
                f"ptr %parameter.{index}"
                for index in range(len(self.function.parameters))
            ]
        )
        self.lines.append(f"define internal ptr {self.body}({parameters}) {{")
        self.lines.append("entry:")
        if self.context:
            for index, fact in enumerate(self.context.parameters):
                value = f"%parameter.{index}"
                if fact.is_fixnum or fact.is_immortal:
                    self.immortal_values.add(value)
                self.refine_type(fact, value)
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
        for index, parameter in enumerate(self.function.parameters):
            self.line(
                f"store ptr %parameter.{index}, ptr {self.local_slots[parameter]}"
            )
        self.line(f"br label %{self.labels[self.function.blocks[0].label]}")
        for block in self.function.blocks:
            self.lines.append(self.labels[block.label] + ":")
            for instruction in block.instructions:
                self.instruction(instruction)
            self.terminator(block.terminator)
        self.lines.append("error:")
        self.line("br label %cleanup")
        self.lines.append("cleanup:")
        for name, slot in self.local_slots.items():
            if name not in self.immortal_locals:
                self.release(self.operation(f"load ptr, ptr {slot}"))
        for result, slot in self.value_slots.items():
            value = self.operation(f"load ptr, ptr {slot}")
            self.mark_immortal(result, value)
            self.release(value)
        value = self.operation("load ptr, ptr %return")
        self.line(f"ret ptr {value}")
        self.lines.append("}")
        return wrapper + "\n\n" + "\n".join(self.lines)

    def wrapper(self) -> str:
        """General entry validates an argument view and transfers owned arguments."""
        count = len(self.function.parameters)
        lines = [
            (
                f"define ptr {self.module.functions[self.function.name]}"
                "(ptr %globals, ptr %args) {"
            ),
            "entry:",
        ]
        for index in range(count):
            lines.extend(
                [
                    f"  %slot.{index} = alloca ptr",
                    f"  store ptr null, ptr %slot.{index}",
                ]
            )
        lines.extend(
            [
                f"  %status = call i32 @mule_check_args(ptr %args, i32 {count})",
                "  %wrong = icmp slt i32 %status, 0",
                "  br i1 %wrong, label %failed, label %checked",
                "checked:",
            ]
        )
        for index in range(count):
            lines.extend(
                [
                    f"  %arg.{index} = call ptr @mule_arg(ptr %args, i32 {index})",
                    f"  store ptr %arg.{index}, ptr %slot.{index}",
                    f"  %missing.{index} = icmp eq ptr %arg.{index}, null",
                    f"  br i1 %missing.{index}, label %failed, label %arg.ok.{index}",
                    f"arg.ok.{index}:",
                ]
            )
        arguments = ", ".join(
            ["ptr %globals", *(f"ptr %arg.{i}" for i in range(count))]
        )
        lines.extend(
            [
                f"  %result = call ptr {self.module.bodies[self.function.name]}({arguments})",
                "  ret ptr %result",
                "failed:",
            ]
        )
        for index in range(count):
            lines.extend(
                [
                    f"  %old.{index} = load ptr, ptr %slot.{index}",
                    f"  call void @mule.fast.release(ptr %old.{index})",
                ]
            )
        lines.extend(["  ret ptr null", "}"])
        return "\n".join(lines)

    def decode(self, value: str) -> str:
        bits = self.operation(f"ptrtoint ptr {value} to i64")
        return self.operation(f"ashr i64 {bits}, 1")

    def encode(self, value: str) -> str:
        shifted = self.operation(f"shl i64 {value}, 1")
        tagged = self.operation(f"or i64 {shifted}, 1")
        return self.operation(f"inttoptr i64 {tagged} to ptr")

    def native_result(self, result: str, value: str, consumed=()) -> None:
        self.publish(result, value)
        for operand in consumed:
            self.release(operand)
        self.refine(result, value)

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
                    fact = self.facts[result]
                    helper = (
                        "local.move"
                        if clear or fact.is_fixnum or fact.is_immortal
                        else "local"
                    )
                    operation = (
                        f"call ptr @mule.fast.{helper}(ptr {value}, ptr {name_pointer})"
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
                    if name not in self.immortal_locals:
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
                reuse_local=reuse_local,
            ):
                lhs, rhs = self.take(left), self.take(right)
                opcode = _BINARY[operator]
                fixnums = self.facts[left].is_fixnum and self.facts[right].is_fixnum
                primitive = {
                    0: "add",
                    1: "sub",
                    2: "mul",
                    10: "and",
                    11: "xor",
                    12: "or",
                }
                if fixnums and self.facts[result].is_fixnum and opcode in primitive:
                    a, b = self.decode(lhs), self.decode(rhs)
                    integer = self.operation(f"{primitive[opcode]} i64 {a}, {b}")
                    self.native_result(result, self.encode(integer), (lhs, rhs))
                    return
                prefix = "fixnum" if fixnums else "fast"
                reuse = (
                    self.local_slots[reuse_local] if reuse_local is not None else "null"
                )
                operation = (
                    f"@mule.{prefix}.binary.{opcode}"
                    if opcode in (0, 1, 2, 4, 5, 8, 9, 10, 11, 12)
                    else "@mule_binary_reuse"
                )
                arguments = (
                    f"ptr {lhs}, ptr {rhs}, i32 {int(inplace)}, ptr {reuse}"
                    if operation != "@mule_binary_reuse"
                    else f"i32 {opcode}, ptr {lhs}, ptr {rhs}, i32 {int(inplace)}, ptr {reuse}"
                )
                self.pointer_result(
                    result,
                    f"call ptr {operation}({arguments})",
                    (lhs, rhs),
                )
            case ir1.Unary(result=result, operator=operator, operand=operand):
                value = self.take(operand)
                opcode = _UNARY[operator]
                fixnum = self.facts[operand].is_fixnum
                if fixnum and self.facts[result].is_fixnum and opcode < 3:
                    decoded = self.decode(value)
                    integer = (
                        decoded
                        if opcode == 0
                        else self.operation(
                            f"sub i64 0, {decoded}"
                            if opcode == 1
                            else f"xor i64 {decoded}, -1"
                        )
                    )
                    self.native_result(result, self.encode(integer), (value,))
                    return
                if fixnum and opcode == 3:
                    bits = self.operation(f"ptrtoint ptr {value} to i64")
                    zero = self.operation(f"icmp eq i64 {bits}, 1")
                    boolean = self.operation(
                        f"select i1 {zero}, ptr @mule.true, ptr @mule.false"
                    )
                    self.native_result(result, boolean, (value,))
                    return
                prefix = "fixnum" if fixnum and opcode < 3 else "fast"
                self.pointer_result(
                    result,
                    f"call ptr @mule.{prefix}.unary.{opcode}(ptr {value})",
                    (value,),
                )
            case ir1.Compare(result=result, operator=operator, left=left, right=right):
                lhs, rhs = self.take(left), self.take(right)
                opcode = _COMPARE[operator]
                if (
                    opcode < 6
                    and self.facts[left].is_fixnum
                    and self.facts[right].is_fixnum
                ):
                    a, b = self.decode(lhs), self.decode(rhs)
                    predicate = ("eq", "ne", "slt", "sle", "sgt", "sge")[opcode]
                    condition = self.operation(f"icmp {predicate} i64 {a}, {b}")
                    boolean = self.operation(
                        f"select i1 {condition}, ptr @mule.true, ptr @mule.false"
                    )
                    self.native_result(result, boolean, (lhs, rhs))
                    return
                operation = (
                    f"call ptr @mule.fast.compare.{opcode}(ptr {lhs}, ptr {rhs})"
                    if opcode < 8
                    else f"call ptr @mule_compare(i32 {opcode}, ptr {lhs}, ptr {rhs})"
                )
                self.pointer_result(
                    result,
                    operation,
                    (lhs, rhs),
                )
            case ir1.Call(result=result, callee=callee, args=args):
                function = self.take(callee)
                values = [self.take(argument) for argument in args]
                for index, value in enumerate(values):
                    slot = self.operation(f"getelementptr ptr, ptr %argv, i64 {index}")
                    self.line(f"store ptr {value}, ptr {slot}")
                value = self.call(callee, function, args, values)
                self.publish(result, value)
                for operand in (function, *values):
                    self.release(operand)
                self.check_pointer(value)
                self.refine(result, value)
            case ir1.DefineFunction(result=result, function=function, public_name=name):
                self.pointer_result(
                    result,
                    "call ptr @mule_function(ptr %globals, "
                    f"ptr {self.module.functions[function]}, ptr {self.module.text(name)})",
                )
            case _:
                raise TypeError(f"Unsupported LLVM instruction: {instruction!r}")

    @staticmethod
    def fits(actual: ValueType, expected: ValueType) -> bool:
        """A caller proof can discharge the specialization's dispatch checks."""
        if expected.kind == "unknown":
            return True
        if expected.is_fixnum:
            return (
                actual.is_fixnum
                and expected.minimum <= actual.minimum
                and actual.maximum <= expected.maximum
            )
        if expected.kind == "bool":
            return (
                actual.kind == "bool"
                and actual.is_immortal
                and (
                    expected.minimum is None
                    or (
                        actual.minimum is not None
                        and expected.minimum <= actual.minimum
                    )
                )
                and (
                    expected.maximum is None
                    or (
                        actual.maximum is not None
                        and actual.maximum <= expected.maximum
                    )
                )
            )
        if expected.is_immortal:
            return actual.is_immortal and (
                expected.kind == "immortal" or actual.kind == expected.kind
            )
        return False

    def argument_guard(self, value: str, expected: ValueType) -> str:
        """Check representation/range without touching or invoking an object."""
        if expected.is_fixnum:
            bits = self.operation(f"ptrtoint ptr {value} to i64")
            tag = self.operation(f"and i64 {bits}, 1")
            tagged = self.operation(f"icmp ne i64 {tag}, 0")
            decoded = self.operation(f"ashr i64 {bits}, 1")
            lower = self.operation(f"icmp sge i64 {decoded}, {expected.minimum}")
            upper = self.operation(f"icmp sle i64 {decoded}, {expected.maximum}")
            bounds = self.operation(f"and i1 {lower}, {upper}")
            return self.operation(f"and i1 {tagged}, {bounds}")
        if expected.kind == "none":
            return self.operation(f"icmp eq ptr {value}, @mule.none")
        true = self.operation(f"icmp eq ptr {value}, @mule.true")
        false = self.operation(f"icmp eq ptr {value}, @mule.false")
        boolean = self.operation(f"or i1 {true}, {false}")
        if expected.kind == "bool":
            allows_false = (expected.minimum is None or expected.minimum <= 0) and (
                expected.maximum is None or expected.maximum >= 0
            )
            allows_true = (expected.minimum is None or expected.minimum <= 1) and (
                expected.maximum is None or expected.maximum >= 1
            )
            if allows_false and allows_true:
                return boolean
            if allows_true:
                return true
            if allows_false:
                return false
            return self.operation("icmp eq i1 true, false")
        if expected.kind != "immortal":
            raise ValueError(f"Unsupported specialization parameter: {expected!r}")
        none = self.operation(f"icmp eq ptr {value}, @mule.none")
        return self.operation(f"or i1 {boolean}, {none}")

    def call(
        self,
        callee_result: str,
        callee: str,
        argument_results: tuple[str, ...],
        arguments: list[str],
    ) -> str:
        """Dispatch once to a guarded typed entry or the general native entry."""
        target = self.targets.get(callee_result)
        function = next(
            (
                function
                for function in self.module.module.functions
                if function.name == target
                and len(function.parameters) == len(arguments)
            ),
            None,
        )
        if function is None:
            return self.operation(
                f"call ptr @mule_call(ptr {callee}, i64 {len(arguments)}, ptr %argv)"
            )
        join = self.fresh("call.join")[1:]
        direct = self.fresh("call.direct")[1:]
        fallback_label = self.fresh("call.fallback")[1:]
        matched = self.operation(
            f"call i32 @mule_function_matches(ptr {callee}, "
            f"ptr {self.module.functions[function.name]}, ptr %globals)"
        )
        matching = self.operation(f"icmp ne i32 {matched}, 0")
        self.line(f"br i1 {matching}, label %{direct}, label %{fallback_label}")
        self.lines.append(direct + ":")
        incoming = []

        def invoke(context: Context | None) -> None:
            entered = self.fresh("call.entered")[1:]
            failed = self.fresh("call.failed")[1:]
            frame = self.operation("call i32 @mule_enter_frame()")
            rejected = self.operation(f"icmp slt i32 {frame}, 0")
            self.line(f"br i1 {rejected}, label %{failed}, label %{entered}")
            self.lines.append(failed + ":")
            self.line(f"br label %{join}")
            incoming.append(("null", failed))
            self.lines.append(entered + ":")
            parameter_facts = (
                context.parameters if context else [UNKNOWN] * len(arguments)
            )
            owned = [
                argument
                if argument in self.immortal_values
                or fact.is_fixnum
                or fact.is_immortal
                else self.operation(f"call ptr @mule.fast.retain(ptr {argument})")
                for argument, fact in zip(arguments, parameter_facts, strict=True)
            ]
            operands = ", ".join(["ptr %globals", *(f"ptr {value}" for value in owned)])
            body = (
                self.module.specialized[function.name, context.id]
                if context
                else self.module.bodies[function.name]
            )
            result = self.operation(f"call ptr {body}({operands})")
            self.line("call void @mule_leave_frame()")
            if context:
                # The callable identity and arguments were checked on this arm.
                # NULL remains an error result; the generic fallback has no fact.
                self.refine_type(context.return_type, result, nullable=True)
            self.line(f"br label %{join}")
            incoming.append((result, entered))

        contexts = sorted(
            self.module.analysis.variants[function.name],
            key=lambda context: sum(
                fact.maximum - fact.minimum if fact.is_fixnum else 2**64
                for fact in context.parameters
            ),
        )
        for context in contexts:
            guards = [
                self.argument_guard(value, expected)
                for source, value, expected in zip(
                    argument_results, arguments, context.parameters, strict=True
                )
                if not self.fits(self.facts[source], expected)
            ]
            if not guards:
                invoke(context)
                break
            valid = guards[0]
            for guard in guards[1:]:
                valid = self.operation(f"and i1 {valid}, {guard}")
            typed = self.fresh("call.typed")[1:]
            next_ = self.fresh("call.general")[1:]
            self.line(f"br i1 {valid}, label %{typed}, label %{next_}")
            self.lines.append(typed + ":")
            invoke(context)
            self.lines.append(next_ + ":")
        else:
            invoke(None)
        self.lines.append(fallback_label + ":")
        fallback = self.operation(
            f"call ptr @mule_call(ptr {callee}, i64 {len(arguments)}, ptr %argv)"
        )
        self.line(f"br label %{join}")
        incoming.append((fallback, fallback_label))
        self.lines.append(join + ":")
        alternatives = ", ".join(f"[ {value}, %{label} ]" for value, label in incoming)
        return self.operation(f"phi ptr {alternatives}")

    def constant(self, value) -> str:
        if value is None:
            return "getelementptr i8, ptr @mule.none, i64 0"
        if isinstance(value, bool):
            name = "true" if value else "false"
            return f"getelementptr i8, ptr @mule.{name}, i64 0"
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
                if self.facts[condition].is_fixnum:
                    bits = self.operation(f"ptrtoint ptr {value} to i64")
                    nonzero = self.operation(f"icmp ne i64 {bits}, 1")
                    truth = self.operation(f"zext i1 {nonzero} to i32")
                else:
                    truth = self.operation(f"call i32 @mule.fast.truth(ptr {value})")
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
