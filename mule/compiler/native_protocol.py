"""Compile Mule class protocols and bind their physical storage primitives.

SBCL defstruct generates Lisp constructors and accessor forms. Here the
constructor and dynamic field adapters come from stdlib/_native_class.py and
pass through the same parser, frontend, ownership lowering, and LLVM backend
as user functions. Only primitive operations have backend templates.
"""

from dataclasses import dataclass, field
from pathlib import Path

from mule.compiler import ir
from mule.compiler.frontend import lower
from mule.compiler.protocols import Bindings, Primitive


@dataclass(slots=True)
class Layout:
    definition: ir.DefineClass
    methods: dict[str, ir.Function]
    index: int
    protocols: dict[str, str] = field(default_factory=dict)

    @property
    def schema(self):
        return f"@mule.layout.{self.index}"

    @property
    def constructor(self):
        return f"@mule.construct.{self.index}"

    @property
    def getter(self):
        return f"@mule.getfield.{self.index}"

    @property
    def setter(self):
        return f"@mule.setfield.{self.index}"


class _Definitions(ir.Visitor):
    def __init__(self, functions):
        self.functions = functions
        self.values = {}
        self.layouts = {}
        self.bindings = {}

    def visit_DefineFunction(self, op):
        self.values[op.result] = self.functions[op.function]

    def visit_DefineClass(self, op):
        if op.class_id is not None:
            layout = Layout(
                op,
                {
                    name: self.values[value]
                    for name, value in zip(op.method_names, op.methods, strict=True)
                },
                len(self.layouts),
            )
            self.layouts[op.class_id] = layout
            self.values[op.result] = layout

    def visit_Store(self, op):
        if not op.local and op.value in self.values:
            self.bindings[op.name] = self.values[op.value]


class Allocate(Primitive):
    def emit(self, emitter, arguments):
        return emitter.operation(f"call ptr @mule_native_new(ptr {arguments[0]})")


class Initialize(Primitive):
    def __init__(self, layout):
        self.layout = layout

    def emit(self, emitter, arguments):
        target = emitter.module.bodies[self.layout.methods["__init__"].name]
        status = emitter.operation("call i32 @mule_enter_frame()")
        emitter.check_status(status)
        owned = [
            emitter.operation(f"call ptr @mule.fast.retain(ptr {value})")
            for value in arguments
        ]
        params = ", ".join(["ptr %globals", *(f"ptr {value}" for value in owned)])
        result = emitter.operation(f"call ptr {target}({params})")
        emitter.line("call void @mule_leave_frame()")
        return result


class InvalidInitializer(Primitive):
    def emit(self, emitter, arguments):
        emitter.line("call i32 @mule_native_initializer_error()")
        return "null"


class ReadInt64(Primitive):
    def emit(self, emitter, arguments):
        payload = emitter.operation(
            f"call ptr @mule_native_payload(ptr {arguments[0]})"
        )
        emitter.check_pointer(payload)
        index = emitter.decode(arguments[1])
        address = emitter.operation(f"getelementptr i64, ptr {payload}, i64 {index}")
        value = emitter.operation(f"load i64, ptr {address}")
        return emitter.operation(f"call ptr @mule_box_i64(i64 {value})")


class WriteInt64(Primitive):
    def emit(self, emitter, arguments):
        payload = emitter.operation(
            f"call ptr @mule_native_payload(ptr {arguments[0]})"
        )
        emitter.check_pointer(payload)
        slot = emitter.operation("alloca i64")
        status = emitter.operation(
            f"call i32 @mule_unbox_i64(ptr {arguments[2]}, ptr {slot})"
        )
        emitter.check_status(status)
        value = emitter.operation(f"load i64, ptr {slot}")
        index = emitter.decode(arguments[1])
        address = emitter.operation(f"getelementptr i64, ptr {payload}, i64 {index}")
        emitter.line(f"store i64 {value}, ptr {address}")
        return "@mule.none"


def prepare(module):
    """Append compiled stdlib protocols; return layouts and intrinsic bindings."""
    definitions = _Definitions({fn.name: fn for fn in module.functions})
    for fn in module.functions:
        definitions.values = {}
        for block in fn.blocks:
            for op in block.instructions:
                op.accept(definitions)
    calls, references = {}, set()
    if not definitions.layouts:
        return definitions.layouts, calls, references
    template = (Path(__file__).parents[2] / "stdlib/_native_class.py").read_text()
    for layout in definitions.layouts.values():
        parameters = ", ".join(
            f"p{i}" for i in range(len(layout.methods["__init__"].parameters) - 1)
        )
        source = template.replace(
            ", PARAMETERS", ", " + parameters if parameters else ""
        )
        lowered = lower(
            source,
            str(Path(__file__).parents[2] / "stdlib/_native_class.py"),
            namespace=f"$mule.native.{layout.index}",
        )
        names = iter(("construct", "get_field", "set_field"))
        primitives = {
            "_allocate": Allocate(),
            "_initialize": Initialize(layout),
            "_invalid_initializer": InvalidInitializer(),
            "_read_int64": ReadInt64(),
            "_write_int64": WriteInt64(),
        }
        for function in lowered.functions:
            if function.name == lowered.entry:
                continue
            role = next(names)
            function.name = f"mule_protocol_{layout.index}_{role}"
            layout.protocols[role] = function.name
            collector = Bindings(primitives)
            for block in function.blocks:
                for op in block.instructions:
                    op.accept(collector)
            calls.update(
                {
                    (function.name, value): primitive
                    for value, primitive in collector.calls.items()
                }
            )
            references.update((function.name, value) for value in collector.references)
            module.functions.append(function)
    return definitions.layouts, calls, references


def callbacks(module, layout):
    """Adapt borrowed primitive ABI arguments to owned compiled Mule entries."""
    count = len(layout.methods["__init__"].parameters) - 1
    constructor = [
        f"define internal ptr {layout.constructor}(ptr %globals, ptr %class, i64 %count, ptr %args) {{",
        "entry:",
        f"  %wrong = icmp ne i64 %count, {count}",
        "  br i1 %wrong, label %error, label %checked",
        "checked:",
        "  %owned.class = call ptr @mule.fast.retain(ptr %class)",
    ]
    parameters = ["ptr %globals", "ptr %owned.class"]
    for i in range(count):
        constructor.extend(
            [
                f"  %slot.{i} = getelementptr ptr, ptr %args, i64 {i}",
                f"  %arg.{i} = load ptr, ptr %slot.{i}",
                f"  %owned.{i} = call ptr @mule.fast.retain(ptr %arg.{i})",
            ]
        )
        parameters.append(f"ptr %owned.{i}")
    constructor.extend(
        [
            f"  %result = call ptr {module.bodies[layout.protocols['construct']]}({', '.join(parameters)})",
            "  ret ptr %result",
            "error:",
            "  call i32 @mule_native_type_error()",
            "  ret ptr null",
            "}",
        ]
    )
    initializer = layout.methods["__init__"]
    if initializer.default_parameters or not initializer.positional_body():
        constructor = [
            f"define internal ptr {layout.constructor}(ptr %globals, ptr %class, i64 %count, ptr %args) {{",
            "entry:",
            "  %result = call ptr @mule_construct_arguments(ptr %class, i64 %count, ptr %args)",
            "  ret ptr %result",
            "}",
        ]
    index = [
        "  %shifted = shl i64 %index, 1",
        "  %tagged = or i64 %shifted, 1",
        "  %index.value = inttoptr i64 %tagged to ptr",
        "  %owned.instance = call ptr @mule.fast.retain(ptr %instance)",
    ]
    getter = [
        f"define internal ptr {layout.getter}(ptr %globals, ptr %instance, i64 %index) {{",
        "entry:",
        *index,
        f"  %result = call ptr {module.bodies[layout.protocols['get_field']]}(ptr %globals, ptr %owned.instance, ptr %index.value)",
        "  ret ptr %result",
        "}",
    ]
    setter = [
        f"define internal i32 {layout.setter}(ptr %globals, ptr %instance, i64 %index, ptr %value) {{",
        "entry:",
        *index,
        "  %owned.value = call ptr @mule.fast.retain(ptr %value)",
        f"  %result = call ptr {module.bodies[layout.protocols['set_field']]}(ptr %globals, ptr %owned.instance, ptr %index.value, ptr %owned.value)",
        "  %failed = icmp eq ptr %result, null",
        "  call void @mule.fast.release(ptr %result)",
        "  %status = select i1 %failed, i32 -1, i32 0",
        "  ret i32 %status",
        "}",
    ]
    return "\n\n".join("\n".join(lines) for lines in (constructor, getter, setter))
