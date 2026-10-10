"""Lower Mule's control-flow IR to native LLVM functions.

Each temporary and local owns a runtime reference in an entry-block slot.
Taking a temporary clears its slot before borrowing it to a runtime operation;
the operation's result is published before its inputs are released. This also
leaves every still-live operand available to the shared error cleanup path.
"""

from __future__ import annotations

import struct

from mule import ast
from mule.compiler import format_protocol, llvm_ops, native, native_protocol
from mule.compiler import ir as ir1
from mule.compiler.analysis import UNKNOWN, Context, ValueType, analyze
from mule.compiler.protocols import CompiledCall

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
declare ptr @mule_namespace()
declare i32 @mule_namespace_bound(ptr, ptr)
declare i32 @mule_assertion_failure(ptr)
declare ptr @mule_sequence_list(i64, ptr)
declare ptr @mule_sequence_tuple(i64, ptr)
declare ptr @mule_lookup_special(ptr, ptr)
declare ptr @mule_render_text(ptr, i32)
declare ptr @mule_text_result(ptr)
declare ptr @mule_text_length(ptr)
declare ptr @mule_text_character(ptr, ptr)
declare ptr @mule_text_codepoint(ptr, ptr)
declare ptr @mule_invalid_string_format()
declare ptr @mule_load_local(ptr, ptr)
declare ptr @mule_binary_reuse(i32, ptr, ptr, i32, ptr)
declare ptr @mule_unary(i32, ptr)
declare ptr @mule_compare(i32, ptr, ptr)
declare i32 @mule_truth(ptr)
declare ptr @mule_call(ptr, i64, ptr)
declare ptr @mule_function(ptr, ptr, ptr)
declare ptr @mule_class(ptr, ptr, i64, ptr, i64, ptr, ptr)
declare ptr @mule_native_class(ptr, ptr, ptr, i64, ptr, i64, ptr, ptr, ptr, ptr, ptr)
declare i32 @mule_native_class_matches(ptr, ptr, ptr)
declare ptr @mule_native_instance_payload(ptr, ptr, ptr)
declare ptr @mule_native_new(ptr)
declare ptr @mule_native_payload(ptr)
declare i32 @mule_unbox_i64(ptr, ptr)
declare ptr @mule_box_i64(i64)
declare i32 @mule_native_type_error()
declare i32 @mule_native_initializer_error()
declare i32 @mule_native_overflow()
declare ptr @mule_get_attr_cached(ptr, ptr, ptr)
declare i32 @mule_set_attr_cached(ptr, ptr, ptr, ptr)
declare ptr @mule_resolve_method(ptr, ptr, ptr, ptr)
declare ptr @mule_call_resolved(ptr, ptr, i64, ptr)
declare i32 @mule_check_args(ptr, i32)
declare i32 @mule_assert_integer(ptr, ptr, ptr, ptr)
declare i32 @mule_assert_float(ptr, ptr)
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


class _MethodTargets(ir1.Visitor):
    """Collect native method predictions; calls still check callable identity."""

    def __init__(self):
        self.definitions = {}
        self.methods = {}
        self.resolutions = {}

    def visit_DefineFunction(self, instruction):
        self.definitions[instruction.result] = instruction.function

    def visit_DefineClass(self, instruction):
        for name, value in zip(
            instruction.method_names, instruction.methods, strict=True
        ):
            target = self.definitions.get(value)
            if target is not None:
                self.methods.setdefault(name, target)

    def visit_ResolveMethod(self, instruction):
        self.resolutions[instruction.result] = instruction.name


class _Imports(ir1.Visitor):
    def __init__(self, imports):
        self.imports = imports

    def visit_ImportModule(self, operation):
        self.imports.setdefault(operation.module_name, operation)


class _ModuleEmitter:
    def __init__(self, module: ir1.Module):
        module = ir1.Module(list(module.functions), module.entry)
        self.module = module
        self.layouts, self.protocol_calls, self.protocol_references = (
            native_protocol.prepare(module)
        )
        calls, references = format_protocol.prepare(module)
        self.protocol_calls.update(calls)
        self.protocol_references.update(references)
        ir1.verify(module)
        self.analysis = analyze(module)
        self.native_plans = native.select(module, self.layouts, self.analysis)
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
        self.auxiliary: list[str] = []
        self.method_targets = {}
        self.imports = {}
        for function in module.functions:
            collector = _MethodTargets()
            for block in function.blocks:
                for instruction in block.instructions:
                    instruction.accept(collector)
                    instruction.accept(_Imports(self.imports))
            for name, target in collector.methods.items():
                self.method_targets.setdefault(name, target)

    def attribute_cache(self) -> str:
        name = f"@mule.cache.{len(self.auxiliary)}"
        self.auxiliary.append(f"{name} = internal global [2 x i64] zeroinitializer")
        return name

    def names(self, names: tuple[str, ...]) -> str:
        if not names:
            return "null"
        values = ", ".join(f"ptr {self.text(name)}" for name in names)
        name = f"@mule.names.{len(self.auxiliary)}"
        self.auxiliary.append(
            f"{name} = private constant [{len(names)} x ptr] [{values}]"
        )
        return name

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
        functions.extend(
            native.Emitter(self, plan).emit() for plan in self.native_plans.values()
        )
        functions.extend(
            native_protocol.callbacks(self, layout) for layout in self.layouts.values()
        )
        functions.extend(
            self.import_initializer(op, index)
            for index, op in enumerate(self.imports.values())
        )
        self.auxiliary.extend(
            f"{layout.schema} = internal global i8 0"
            for layout in self.layouts.values()
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
            + "".join(
                f"  %import.{index} = load ptr, ptr @mule.import.cache.{index}\n"
                f"  call void @mule_release(ptr %import.{index})\n"
                f"  store ptr null, ptr @mule.import.cache.{index}\n"
                for index in range(len(self.imports))
            )
            + "  ret i32 %status\n"
            "}"
        )
        return (
            "\n\n".join(
                [
                    _DECLARATIONS,
                    llvm_ops.helpers(),
                    *globals_,
                    *self.auxiliary,
                    *functions,
                    main,
                ]
            )
            + "\n"
        )

    def import_initializer(self, op, index):
        cache = f"@mule.import.cache.{index}"
        self.auxiliary.append(f"{cache} = internal global ptr null")
        lines = [
            f"define internal ptr @mule.import.{index}() {{",
            "entry:",
            f"  %cached = load ptr, ptr {cache}",
            "  %ready = icmp ne ptr %cached, null",
            "  br i1 %ready, label %existing, label %initialize",
            "existing:",
            "  %owned = call ptr @mule_retain(ptr %cached)",
            "  ret ptr %owned",
            "initialize:",
            "  %globals = call ptr @mule_namespace()",
            f"  store ptr %globals, ptr {cache}",
        ]
        for field, value in (("__name__", op.module_name), ("__file__", op.filename)):
            literal = self.literal_data(value.encode())
            key = self.text(field)
            label = field.strip("_")
            lines.extend(
                [
                    f"  %{label} = call ptr @mule_literal_text(ptr {literal}, i64 {len(value.encode())})",
                    f"  %status.{label} = call i32 @mule_store_global(ptr %globals, ptr {key}, ptr %{label})",
                    f"  call void @mule_release(ptr %{label})",
                    f"  %bad.{label} = icmp slt i32 %status.{label}, 0",
                    f"  br i1 %bad.{label}, label %failed, label %after.{label}",
                    f"after.{label}:",
                ]
            )
        lines.extend(
            [
                "  %frame = call i32 @mule_enter_frame()",
                "  %bad.frame = icmp slt i32 %frame, 0",
                "  br i1 %bad.frame, label %failed, label %execute",
                "execute:",
                f"  %result = call ptr {self.bodies[op.initializer]}(ptr %globals)",
                "  call void @mule_leave_frame()",
                "  %bad.result = icmp eq ptr %result, null",
                "  call void @mule_release(ptr %result)",
                "  br i1 %bad.result, label %failed, label %success",
                "success:",
                "  %module = call ptr @mule_retain(ptr %globals)",
                "  ret ptr %module",
                "failed:",
                f"  store ptr null, ptr {cache}",
                "  call void @mule_release(ptr %globals)",
                "  ret ptr null",
                "}",
            ]
        )
        return "\n".join(lines)


class _LiteralEmitter(ast.Visitor):
    """Each literal chooses its native representation without host type checks."""

    def __init__(self, module: _ModuleEmitter):
        self.module = module

    def visit_Node(self, node: ast.Node) -> str:
        raise TypeError("Unsupported LLVM constant")

    def visit_Literal(self, literal: ast.Literal) -> str:
        raise TypeError("Unsupported LLVM constant")

    def visit_NoneLiteral(self, literal: ast.NoneLiteral) -> str:
        return "getelementptr i8, ptr @mule.none, i64 0"

    def visit_BooleanLiteral(self, literal: ast.BooleanLiteral) -> str:
        name = "true" if literal.value else "false"
        return f"getelementptr i8, ptr @mule.{name}, i64 0"

    def visit_IntegerLiteral(self, literal: ast.IntegerLiteral) -> str:
        value = literal.value
        if -(1 << 62) <= value < (1 << 62):
            tagged = ((value << 1) | 1) & ((1 << 64) - 1)
            return f"inttoptr i64 {tagged} to ptr"
        return f"call ptr @mule_literal_int(ptr {self.module.text(_decimal(value))})"

    def visit_FloatLiteral(self, literal: ast.FloatLiteral) -> str:
        bits = struct.unpack(">Q", struct.pack(">d", literal.value))[0]
        return f"call ptr @mule_literal_float(double 0x{bits:016X})"

    def visit_TextLiteral(self, literal: ast.TextLiteral) -> str:
        encoded = literal.value.encode("utf-8", "surrogatepass")
        return (
            f"call ptr @mule_literal_text(ptr {self.module.literal_data(encoded)}, "
            f"i64 {len(encoded)})"
        )

    def visit_BytesLiteral(self, literal: ast.BytesLiteral) -> str:
        value = literal.value
        return (
            f"call ptr @mule_literal_bytes(ptr {self.module.literal_data(value)}, "
            f"i64 {len(value)})"
        )

    def visit_ComplexLiteral(self, literal: ast.ComplexLiteral) -> str:
        return self.visit_Literal(literal)

    def visit_EllipsisLiteral(self, literal: ast.EllipsisLiteral) -> str:
        return self.visit_Literal(literal)


class _ContractAssertion:
    def __init__(self, emitter, value):
        self.emitter = emitter
        self.value = value

    def visit_Contract(self, contract):
        raise TypeError(f"unsupported annotation contract {contract}")

    def visit_IntegerContract(self, contract):
        fact = contract.fact
        module = self.emitter.module
        minimum = module.text(_decimal(fact.minimum))
        maximum = module.text(_decimal(fact.maximum))
        name = module.text(contract.display_name)
        status = self.emitter.operation(
            f"call i32 @mule_assert_integer(ptr {self.value}, ptr {minimum}, "
            f"ptr {maximum}, ptr {name})"
        )
        self.emitter.check_status(status)

    def visit_FloatContract(self, contract):
        name = self.emitter.module.text(contract.display_name)
        status = self.emitter.operation(
            f"call i32 @mule_assert_float(ptr {self.value}, ptr {name})"
        )
        self.emitter.check_status(status)

    def visit_PythonIntegerContract(self, contract):
        emitter = self.emitter
        true = emitter.operation(f"icmp eq ptr {self.value}, @mule.true")
        false = emitter.operation(f"icmp eq ptr {self.value}, @mule.false")
        boolean = emitter.operation(f"or i1 {true}, {false}")
        integer = emitter.fresh("integer.check")[1:]
        done = emitter.fresh("integer.checked")[1:]
        emitter.line(f"br i1 {boolean}, label %{done}, label %{integer}")
        emitter.lines.append(integer + ":")
        status = emitter.operation(
            f"call i32 @mule_assert_integer(ptr {self.value}, ptr null, "
            f"ptr null, ptr {emitter.module.text(contract.display_name)})"
        )
        emitter.check_status(status)
        emitter.line(f"br label %{done}")
        emitter.lines.append(done + ":")


class _ArgumentGuard:
    """Emit the entry check selected by the parameter fact's own behavior."""

    def __init__(self, emitter, value: str):
        self.emitter = emitter
        self.value = value

    def visit_ValueType(self, fact: ValueType) -> str:
        raise ValueError(f"Unsupported specialization parameter: {fact!r}")

    def visit_UnknownType(self, fact: ValueType) -> str:
        return "true"

    def visit_IntegerType(self, fact: ValueType) -> str:
        if not fact.is_fixnum:
            return self.visit_ValueType(fact)
        operation = self.emitter.operation
        bits = operation(f"ptrtoint ptr {self.value} to i64")
        tag = operation(f"and i64 {bits}, 1")
        tagged = operation(f"icmp ne i64 {tag}, 0")
        decoded = operation(f"ashr i64 {bits}, 1")
        lower = operation(f"icmp sge i64 {decoded}, {fact.minimum}")
        upper = operation(f"icmp sle i64 {decoded}, {fact.maximum}")
        bounds = operation(f"and i1 {lower}, {upper}")
        return operation(f"and i1 {tagged}, {bounds}")

    def visit_NoneType(self, fact: ValueType) -> str:
        return self.emitter.operation(f"icmp eq ptr {self.value}, @mule.none")

    def boolean(self) -> tuple[str, str, str]:
        operation = self.emitter.operation
        true = operation(f"icmp eq ptr {self.value}, @mule.true")
        false = operation(f"icmp eq ptr {self.value}, @mule.false")
        return true, false, operation(f"or i1 {true}, {false}")

    def visit_BooleanType(self, fact: ValueType) -> str:
        true, false, boolean = self.boolean()
        allows_false = (fact.minimum is None or fact.minimum <= 0) and (
            fact.maximum is None or fact.maximum >= 0
        )
        allows_true = (fact.minimum is None or fact.minimum <= 1) and (
            fact.maximum is None or fact.maximum >= 1
        )
        if allows_false and allows_true:
            return boolean
        if allows_true:
            return true
        if allows_false:
            return false
        return self.emitter.operation("icmp eq i1 true, false")

    def visit_ImmortalType(self, fact: ValueType) -> str:
        _, _, boolean = self.boolean()
        none = self.emitter.operation(f"icmp eq ptr {self.value}, @mule.none")
        return self.emitter.operation(f"or i1 {boolean}, {none}")


class _FunctionEmitter(ir1.Visitor):
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
                stored = instruction.stored_local()
                if stored is not None:
                    name, value = stored
                    fact = self.facts[value]
                    if not (fact.is_fixnum or fact.is_immortal):
                        self.immortal_locals.discard(name)
        results = dict.fromkeys(
            instruction.defined_result()
            for block in function.blocks
            for instruction in block.instructions
            if instruction.defined_result() is not None
        )
        self.value_slots = {
            name: f"%value.{index}" for index, name in enumerate(results)
        }
        self.receiver_slots = {
            name: f"%receiver.{index}"
            for index, name in enumerate(
                instruction.resolved_receiver()
                for block in function.blocks
                for instruction in block.instructions
                if instruction.resolved_receiver() is not None
            )
        }
        self.labels = {
            block.label: f"block.{index}" for index, block in enumerate(function.blocks)
        }
        collector = _MethodTargets()
        for block in function.blocks:
            for instruction in block.instructions:
                instruction.accept(collector)
        self.method_names = collector.resolutions

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
        adapter = ""
        plan = self.module.native_plans.get(self.function.name)
        if plan is not None:
            adapter = native.adapter(self, plan)
            self.body += ".boxed"
            self.lines = []
        parameters = ", ".join(
            ["ptr %globals"]
            + [
                f"ptr %parameter.{index}"
                for index in range(len(self.function.parameters))
            ]
        )
        attributes = ""
        if self.function.policy.speed == 0:
            attributes = " noinline optnone"
        elif self.function.policy.debug == 3:
            attributes = " noinline"
        self.lines.append(
            f"define internal ptr {self.body}({parameters}){attributes} {{"
        )
        self.lines.append("entry:")
        if self.context:
            for index, fact in enumerate(self.context.parameters):
                value = f"%parameter.{index}"
                if fact.is_fixnum or fact.is_immortal:
                    self.immortal_values.add(value)
                self.refine_type(fact, value)
        self.line("%return = alloca ptr")
        self.line("store ptr null, ptr %return")
        for slot in [
            *self.local_slots.values(),
            *self.value_slots.values(),
            *self.receiver_slots.values(),
        ]:
            self.line(f"{slot} = alloca ptr")
            self.line(f"store ptr null, ptr {slot}")
        max_args = max(
            (
                instruction.argument_count()
                for block in self.function.blocks
                for instruction in block.instructions
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
        for slot in self.receiver_slots.values():
            self.release(self.operation(f"load ptr, ptr {slot}"))
        value = self.operation("load ptr, ptr %return")
        self.line(f"ret ptr {value}")
        self.lines.append("}")
        return wrapper + "\n\n" + adapter + "\n\n" + "\n".join(self.lines)

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
        instruction.accept(self)

    def visit_Operation(self, operation: ir1.Operation) -> None:
        raise TypeError(f"Unsupported LLVM operation: {operation!r}")

    def visit_Constant(self, instruction: ir1.Constant) -> None:
        result = instruction.result
        value = instruction.value
        self.pointer_result(result, self.constant(value))

    def visit_AssertType(self, instruction: ir1.AssertType) -> None:
        actual = self.facts[instruction.value]
        if instruction.safety and not actual.fits(instruction.contract.fact):
            # Keep the unrefined operand owned by its original slot until the
            # assertion succeeds, so error cleanup releases its actual value.
            value = self.operation(
                f"load ptr, ptr {self.value_slots[instruction.value]}"
            )
            instruction.contract.accept(_ContractAssertion(self, value))
        value = self.take(instruction.value)
        self.publish(instruction.result, value)
        self.refine(instruction.result, value)

    def visit_ImportModule(self, instruction):
        index = list(self.module.imports).index(instruction.module_name)
        self.pointer_result(instruction.result, f"call ptr @mule.import.{index}()")

    def visit_HasModuleBinding(self, instruction):
        module = self.take(instruction.module)
        status = self.operation(
            f"call i32 @mule_namespace_bound(ptr {module}, "
            f"ptr {self.module.text(instruction.name)})"
        )
        self.release(module)
        self.check_status(status)
        self.pointer_result(instruction.result, f"call ptr @mule_bool(i32 {status})")

    def visit_AssertionFailure(self, instruction):
        message = self.take(instruction.message)
        status = self.operation(f"call i32 @mule_assertion_failure(ptr {message})")
        self.release(message)
        self.check_status(status)

    def sequence(self, instruction, primitive):
        values = [self.take(element) for element in instruction.elements]
        for index, value in enumerate(values):
            slot = self.operation(f"getelementptr ptr, ptr %argv, i64 {index}")
            self.line(f"store ptr {value}, ptr {slot}")
        self.pointer_result(
            instruction.result,
            f"call ptr @{primitive}(i64 {len(values)}, ptr %argv)",
            values,
        )

    def visit_BuildList(self, instruction):
        self.sequence(instruction, "mule_sequence_list")

    def visit_BuildTuple(self, instruction):
        self.sequence(instruction, "mule_sequence_tuple")

    def visit_FormatValue(self, instruction):
        value = self.take(instruction.value)
        if instruction.conversion is not None:
            protocol = {"s": "string", "r": "representation"}[instruction.conversion]
            converted = CompiledCall("mule_format_" + protocol).emit(self, [value])
            self.publish(instruction.result, converted)
            self.release(value)
            self.check_pointer(converted)
            value = self.take(instruction.result)
        specification = self.take(instruction.specification)
        result = CompiledCall("mule_format_format_value").emit(
            self, [value, specification]
        )
        self.publish(instruction.result, result)
        self.release(value)
        self.release(specification)
        self.check_pointer(result)

    def visit_Load(self, instruction: ir1.Load) -> None:
        result = instruction.result
        if (self.function.name, result) in self.module.protocol_references:
            self.publish(result, "@mule.none")
            return
        name = instruction.name
        local = instruction.local
        clear = instruction.clear
        name_pointer = self.module.text(name)
        if local:
            slot = self.local_slots[name]
            value = self.operation(f"load ptr, ptr {slot}")
            if clear:
                self.line(f"store ptr null, ptr {slot}")
            fact = self.facts[result]
            helper = (
                "local.move" if clear or fact.is_fixnum or fact.is_immortal else "local"
            )
            operation = f"call ptr @mule.fast.{helper}(ptr {value}, ptr {name_pointer})"
        else:
            operation = f"call ptr @mule_load_global(ptr %globals, ptr {name_pointer})"
        self.pointer_result(result, operation)

    def visit_Store(self, instruction: ir1.Store) -> None:
        name = instruction.name
        operand = instruction.value
        local = instruction.local
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

    def visit_Discard(self, instruction: ir1.Discard) -> None:
        operand = instruction.value
        self.release(self.take(operand))

    def visit_Binary(self, instruction: ir1.Binary) -> None:
        result = instruction.result
        operator = instruction.operator
        left = instruction.left
        right = instruction.right
        inplace = instruction.inplace
        reuse_local = instruction.reuse_local
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
        reuse = self.local_slots[reuse_local] if reuse_local is not None else "null"
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

    def visit_Unary(self, instruction: ir1.Unary) -> None:
        result = instruction.result
        operator = instruction.operator
        operand = instruction.operand
        value = self.take(operand)
        opcode = _UNARY[operator]
        fixnum = self.facts[operand].is_fixnum
        if fixnum and self.facts[result].is_fixnum and opcode < 3:
            decoded = self.decode(value)
            integer = (
                decoded
                if opcode == 0
                else self.operation(
                    f"sub i64 0, {decoded}" if opcode == 1 else f"xor i64 {decoded}, -1"
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

    def visit_Compare(self, instruction: ir1.Compare) -> None:
        result = instruction.result
        operator = instruction.operator
        left = instruction.left
        right = instruction.right
        lhs, rhs = self.take(left), self.take(right)
        opcode = _COMPARE[operator]
        if opcode < 6 and self.facts[left].is_fixnum and self.facts[right].is_fixnum:
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

    def visit_Call(self, instruction: ir1.Call) -> None:
        result = instruction.result
        callee = instruction.callee
        args = instruction.args
        function = self.take(callee)
        values = [self.take(argument) for argument in args]
        primitive = self.module.protocol_calls.get((self.function.name, result))
        if primitive is not None:
            value = primitive.emit(self, values)
            self.publish(result, value)
            for operand in (function, *values):
                self.release(operand)
            self.check_pointer(value)
            return
        for index, value in enumerate(values):
            slot = self.operation(f"getelementptr ptr, ptr %argv, i64 {index}")
            self.line(f"store ptr {value}, ptr {slot}")
        value = self.call(callee, function, args, values)
        self.publish(result, value)
        for operand in (function, *values):
            self.release(operand)
        self.check_pointer(value)
        self.refine(result, value)

    def visit_DefineFunction(self, instruction: ir1.DefineFunction) -> None:
        result = instruction.result
        function = instruction.function
        name = instruction.public_name
        self.pointer_result(
            result,
            "call ptr @mule_function(ptr %globals, "
            f"ptr {self.module.functions[function]}, ptr {self.module.text(name)})",
        )

    def visit_DefineClass(self, instruction: ir1.DefineClass) -> None:
        base = self.take(instruction.base) if instruction.base is not None else "null"
        methods = [self.take(method) for method in instruction.methods]
        for index, value in enumerate(methods):
            slot = self.operation(f"getelementptr ptr, ptr %argv, i64 {index}")
            self.line(f"store ptr {value}, ptr {slot}")
        layout = self.module.layouts.get(instruction.class_id)
        if layout is not None:
            self.pointer_result(
                instruction.result,
                f"call ptr @mule_native_class(ptr {self.module.text(instruction.public_name)}, "
                f"ptr {layout.schema}, ptr %globals, i64 {len(instruction.fields)}, ptr {self.module.names(instruction.fields)}, "
                f"i64 {len(methods)}, ptr {self.module.names(instruction.method_names)}, ptr %argv, "
                f"ptr {layout.constructor}, ptr {layout.getter}, ptr {layout.setter})",
                methods,
            )
            return
        self.pointer_result(
            instruction.result,
            f"call ptr @mule_class(ptr {self.module.text(instruction.public_name)}, "
            f"ptr {base}, i64 {len(instruction.fields)}, ptr {self.module.names(instruction.fields)}, "
            f"i64 {len(instruction.methods)}, ptr {self.module.names(instruction.method_names)}, ptr %argv)",
            (base, *methods),
        )

    def visit_LoadAttribute(self, instruction: ir1.LoadAttribute) -> None:
        receiver = self.take(instruction.receiver)
        self.pointer_result(
            instruction.result,
            f"call ptr @mule_get_attr_cached(ptr {receiver}, ptr {self.module.text(instruction.name)}, "
            f"ptr {self.module.attribute_cache()})",
            (receiver,),
        )

    def visit_StoreAttribute(self, instruction: ir1.StoreAttribute) -> None:
        receiver, value = self.take(instruction.receiver), self.take(instruction.value)
        status = self.operation(
            f"call i32 @mule_set_attr_cached(ptr {receiver}, ptr {self.module.text(instruction.name)}, "
            f"ptr {value}, ptr {self.module.attribute_cache()})"
        )
        self.release(receiver)
        self.release(value)
        self.check_status(status)

    def visit_ResolveMethod(self, instruction: ir1.ResolveMethod) -> None:
        receiver = self.take(instruction.receiver)
        slot = self.receiver_slots[instruction.result]
        # Loop iterations reuse this slot; the previous call normally cleared it.
        old = self.operation(f"load ptr, ptr {slot}")
        self.line(f"store ptr null, ptr {slot}")
        self.release(old)
        self.pointer_result(
            instruction.result,
            f"call ptr @mule_resolve_method(ptr {receiver}, ptr {self.module.text(instruction.name)}, "
            f"ptr {self.module.attribute_cache()}, ptr {slot})",
            (receiver,),
        )

    def visit_CallMethod(self, instruction: ir1.CallMethod) -> None:
        function = self.take(instruction.callee)
        slot = self.receiver_slots[instruction.callee]
        receiver = self.operation(f"load ptr, ptr {slot}")
        self.line(f"store ptr null, ptr {slot}")
        values = [self.take(argument) for argument in instruction.args]
        for index, value in enumerate(values):
            argv = self.operation(f"getelementptr ptr, ptr %argv, i64 {index}")
            self.line(f"store ptr {value}, ptr {argv}")
        value = self.call_method(instruction, function, receiver, values)
        self.publish(instruction.result, value)
        for operand in (function, receiver, *values):
            self.release(operand)
        self.check_pointer(value)

    def call_method(self, instruction, callable, receiver, arguments):
        """Guard a predicted method and pass owned arguments to its native body."""
        target = self.module.method_targets.get(self.method_names[instruction.callee])
        function = next(
            (
                function
                for function in self.module.module.functions
                if function.name == target
                and len(function.parameters) == len(arguments) + 1
            ),
            None,
        )
        fallback_call = (
            f"call ptr @mule_call_resolved(ptr {callable}, ptr {receiver}, "
            f"i64 {len(arguments)}, ptr %argv)"
        )
        if function is None:
            return self.operation(fallback_call)
        matched = self.operation(
            f"call i32 @mule_function_matches(ptr {callable}, "
            f"ptr {self.module.functions[function.name]}, ptr %globals)"
        )
        identity = self.operation(f"icmp ne i32 {matched}, 0")
        bound = self.operation(f"icmp ne ptr {receiver}, null")
        valid = self.operation(f"and i1 {identity}, {bound}")
        direct, fallback, entered, failed, join = (
            self.fresh(label)[1:]
            for label in (
                "method.direct",
                "method.fallback",
                "method.entered",
                "method.failed",
                "method.join",
            )
        )
        self.line(f"br i1 {valid}, label %{direct}, label %{fallback}")
        self.lines.append(direct + ":")
        frame = self.operation("call i32 @mule_enter_frame()")
        rejected = self.operation(f"icmp slt i32 {frame}, 0")
        self.line(f"br i1 {rejected}, label %{failed}, label %{entered}")
        self.lines.append(failed + ":")
        self.line(f"br label %{join}")
        self.lines.append(entered + ":")
        owned = [
            value
            if value in self.immortal_values
            else self.operation(f"call ptr @mule.fast.retain(ptr {value})")
            for value in (receiver, *arguments)
        ]
        operands = ", ".join(["ptr %globals", *(f"ptr {value}" for value in owned)])
        result = self.operation(
            f"call ptr {self.module.bodies[function.name]}({operands})"
        )
        self.line("call void @mule_leave_frame()")
        self.line(f"br label %{join}")
        self.lines.append(fallback + ":")
        general = self.operation(fallback_call)
        self.line(f"br label %{join}")
        self.lines.append(join + ":")
        return self.operation(
            f"phi ptr [null, %{failed}], [{result}, %{entered}], [{general}, %{fallback}]"
        )

    def argument_guard(self, value: str, expected: ValueType) -> str:
        """Check representation/range without touching or invoking an object."""
        return expected.accept(_ArgumentGuard(self, value))

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
                if not self.facts[source].dispatch_fits(expected)
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

    def constant(self, literal: ast.Literal) -> str:
        return literal.accept(_LiteralEmitter(self.module))

    def terminator(self, terminator: ir1.Terminator) -> None:
        terminator.accept(self)

    def visit_Jump(self, terminator: ir1.Jump) -> None:
        target = terminator.target
        self.line(f"br label %{self.labels[target]}")

    def visit_Branch(self, terminator: ir1.Branch) -> None:
        condition = terminator.condition
        true = terminator.true
        false = terminator.false
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

    def visit_Return(self, terminator: ir1.Return) -> None:
        value = terminator.value
        result = self.take(value)
        self.line(f"store ptr {result}, ptr %return")
        self.line("br label %cleanup")
