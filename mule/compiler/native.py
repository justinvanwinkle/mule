"""Select raw representations and local entries from proven IR contracts.

Corresponds to SBCL's represent.lisp and locall.lisp: boxed public entries stay
available while a closed set of primitive operations gets an unboxed entry.
Class bindings are predictions guarded once at entry, never assumed immutable.
"""

from dataclasses import dataclass, field

from mule import ast
from mule.compiler import ir
from mule.compiler.annotation_types import EXPORTS


class NotNative(Exception):
    pass


class Deferred(Exception):
    pass


class Representation:
    llvm_type = "i64"
    zero = "0"

    def numeric(self):
        raise NotNative

    def python_integer(self):
        raise NotNative

    def field(self, name):
        raise NotNative

    def method(self, name):
        raise NotNative

    def constructor(self):
        raise NotNative

    def truth(self, emitter, value):
        raise NotNative

    def box(self, emitter, value):
        raise NotNative

    def parameter(self, emitter, value):
        raise NotNative


class Int64(Representation):
    def numeric(self):
        return self

    def python_integer(self):
        return self

    def truth(self, emitter, value):
        return emitter.operation(f"icmp ne i64 {value}, 0")

    def box(self, emitter, value):
        return emitter.operation(f"call ptr @mule_box_i64(i64 {value})")

    def parameter(self, emitter, value):
        slot = emitter.operation("alloca i64")
        status = emitter.operation(f"call i32 @mule_unbox_i64(ptr {value}, ptr {slot})")
        emitter.check_status(status)
        return emitter.operation(f"load i64, ptr {slot}")


class Boolean(Representation):
    llvm_type = "i1"
    zero = "false"

    def python_integer(self):
        return self

    def truth(self, emitter, value):
        return value

    def box(self, emitter, value):
        wide = emitter.operation(f"zext i1 {value} to i32")
        return emitter.operation(f"call ptr @mule_bool(i32 {wide})")


class Nothing(Representation):
    def box(self, emitter, value):
        return "@mule.none"


@dataclass(frozen=True)
class Instance(Representation):
    layout_id: str
    layout: object = field(compare=False)
    llvm_type = "ptr"
    zero = "null"

    def field(self, name):
        if name not in self.layout.definition.fields:
            raise NotNative
        return INT64

    def method(self, name):
        target = self.layout.methods.get(name)
        if target is None:
            raise NotNative
        return Method(self, target)

    def parameter(self, emitter, value):
        payload = emitter.operation(
            f"call ptr @mule_native_instance_payload(ptr {value}, "
            f"ptr {self.layout.schema}, ptr %globals)"
        )
        emitter.check_pointer(payload)
        return payload


@dataclass(frozen=True)
class Class(Representation):
    binding: str
    layout_id: str
    layout: object = field(compare=False)
    llvm_type = "ptr"
    zero = "null"

    def constructor(self):
        return Instance(self.layout_id, self.layout)


@dataclass(frozen=True)
class Method(Representation):
    receiver: Instance
    target: object
    llvm_type = "ptr"
    zero = "null"


INT64, BOOL, NONE = Int64(), Boolean(), Nothing()


class _Contract:
    def visit_Contract(self, contract):
        raise NotNative

    def visit_IntegerContract(self, contract):
        if contract.bits != 64 or not contract.signed:
            raise NotNative
        return INT64

    visit_FloatContract = visit_Contract
    visit_PythonIntegerContract = visit_Contract


class _LocalContract(_Contract):
    def visit_PythonIntegerContract(self, contract):
        # Unbounded Python int is a semantic constraint, not a machine width.
        # Select storage from the local's definitions and arithmetic proofs.
        return None


class _AssertionContract(_Contract):
    def __init__(self, representation):
        self.representation = representation

    def visit_PythonIntegerContract(self, contract):
        return self.representation.python_integer()


class _Literal(ast.Visitor):
    def visit_Node(self, node):
        raise NotNative

    def visit_IntegerLiteral(self, node):
        if not -(1 << 63) <= node.value < (1 << 63):
            raise NotNative
        return INT64

    def visit_BooleanLiteral(self, node):
        return BOOL

    def visit_NoneLiteral(self, node):
        return NONE


class _ClassBindings(ir.Visitor):
    def __init__(self, layouts):
        self.layouts = layouts
        self.values, self.bindings = {}, {}

    def visit_DefineClass(self, op):
        if op.class_id in self.layouts:
            self.values[op.result] = op.class_id

    def visit_Store(self, op):
        if not op.local and op.value in self.values:
            self.bindings.setdefault(op.name, self.values[op.value])


@dataclass(slots=True)
class Plan:
    function: ir.Function
    index: int
    parameters: tuple[Representation, ...]
    locals: dict[str, Representation]
    values: dict[str, Representation] = field(default_factory=dict)
    returned: Representation | None = None
    dependencies: dict[str, Class] = field(default_factory=dict)
    calls: set[str] = field(default_factory=set)
    allocations: dict[str, Class] = field(default_factory=dict)

    @property
    def entry(self):
        return f"@mule.native.{self.index}"


_BINARY = {
    ast.BinaryOperator.ADD: "add",
    ast.BinaryOperator.SUBTRACT: "sub",
    ast.BinaryOperator.MULTIPLY: "mul",
    ast.BinaryOperator.BIT_AND: "and",
    ast.BinaryOperator.BIT_OR: "or",
    ast.BinaryOperator.BIT_XOR: "xor",
}
_COMPARE = {
    ast.ComparisonOperator.EQUAL: "eq",
    ast.ComparisonOperator.NOT_EQUAL: "ne",
    ast.ComparisonOperator.LESS: "slt",
    ast.ComparisonOperator.LESS_EQUAL: "sle",
    ast.ComparisonOperator.GREATER: "sgt",
    ast.ComparisonOperator.GREATER_EQUAL: "sge",
}


class _Select(ir.Visitor):
    def __init__(self, plan, layouts, bindings, known_plans):
        self.plan, self.layouts, self.bindings = plan, layouts, bindings
        self.known_plans = known_plans
        self.block = None

    def value(self, name):
        value = self.plan.values.get(name)
        if value is None:
            raise Deferred
        return value

    def visit_Operation(self, op):
        raise NotNative

    def visit_Constant(self, op):
        return op.value.accept(_Literal())

    def visit_AssertType(self, op):
        value = self.value(op.value)
        expected = op.contract.accept(_AssertionContract(value))
        if value != expected:
            raise NotNative
        return expected

    def visit_Load(self, op):
        if op.local:
            result = self.plan.locals.get(op.name)
            if result is None:
                raise Deferred
            return result
        identity = self.bindings.get(op.name)
        if identity is None:
            raise NotNative
        result = Class(op.name, identity, self.layouts[identity])
        self.plan.dependencies[op.name] = result
        return result

    def visit_Store(self, op):
        if not op.local:
            raise NotNative
        value = self.value(op.value)
        old = self.plan.locals.get(op.name)
        if old is not None and old != value:
            raise NotNative
        self.plan.locals[op.name] = value

    def visit_Binary(self, op):
        if op.operator not in _BINARY:
            raise NotNative
        self.value(op.left).numeric()
        self.value(op.right).numeric()
        return INT64

    def visit_Unary(self, op):
        value = self.value(op.operand)
        if op.operator == ast.UnaryOperator.NOT:
            value.truth(_TruthProbe(), "0")
            return BOOL
        value.numeric()
        if op.operator not in {
            ast.UnaryOperator.POSITIVE,
            ast.UnaryOperator.NEGATIVE,
            ast.UnaryOperator.INVERT,
        }:
            raise NotNative
        return INT64

    def visit_Compare(self, op):
        if op.operator not in _COMPARE:
            raise NotNative
        self.value(op.left).numeric()
        self.value(op.right).numeric()
        return BOOL

    def visit_LoadAttribute(self, op):
        return self.value(op.receiver).field(op.name)

    def visit_StoreAttribute(self, op):
        if self.value(op.receiver).field(op.name) != self.value(op.value):
            raise NotNative

    def visit_ResolveMethod(self, op):
        return self.value(op.receiver).method(op.name)

    def visit_CallMethod(self, op):
        method = self.value(op.callee)
        target = method.target
        if len(target.parameters) != len(op.args) + 1:
            raise NotNative
        for argument, contract in zip(
            op.args, target.parameter_annotations[1:], strict=True
        ):
            if contract is None or self.value(argument) != contract.accept(_Contract()):
                raise NotNative
        self.plan.calls.add(target.name)
        if target.return_annotation is not None:
            return target.return_annotation.accept(_Contract())
        known = self.known_plans.get(target.name)
        if known is None:
            raise Deferred
        return known.returned

    def visit_Call(self, op):
        callee = self.value(op.callee)
        instance = callee.constructor()
        initializer = instance.layout.methods["__init__"]
        if len(op.args) != len(initializer.parameters) - 1:
            raise NotNative
        for argument, contract in zip(
            op.args, initializer.parameter_annotations[1:], strict=True
        ):
            if contract is None or self.value(argument) != contract.accept(_Contract()):
                raise NotNative
        # A reused allocation site in a loop needs an escape/lifetime analysis
        # before borrowed instance references can safely replace owned values.
        blocks = {block.label: block for block in self.plan.function.blocks}
        pending, visited = list(self.block.terminator.successors()), set()
        while pending:
            label = pending.pop()
            if label == self.block.label:
                raise NotNative
            if label not in visited:
                visited.add(label)
                pending.extend(blocks[label].terminator.successors())
        self.plan.calls.add(initializer.name)
        self.plan.allocations[op.result] = callee
        return instance

    def visit_Discard(self, op):
        self.value(op.value)

    def visit_Jump(self, op):
        pass

    def visit_Branch(self, op):
        self.value(op.condition).truth(_TruthProbe(), "0")

    def visit_Return(self, op):
        value = self.value(op.value)
        # Instance escape requires owned-return representation conversion.
        value.box(_TruthProbe(), "0")
        if self.plan.returned is not None and self.plan.returned != value:
            raise NotNative
        self.plan.returned = value


class _TruthProbe:
    def operation(self, text):
        return "0"


def select(module, layouts, analysis):
    """Choose native entries; remove any caller whose local edges lack a body."""
    collector = _ClassBindings(layouts)
    for fn in module.functions:
        collector.values = {}
        for block in fn.blocks:
            for op in block.instructions:
                op.accept(collector)
    plans = {}
    previous_count = -1
    while len(plans) != previous_count:
        previous_count = len(plans)
        for index, function in enumerate(module.functions):
            if (
                function.name == module.entry
                or function.policy.speed < 3
                or function.name in plans
            ):
                continue
            try:
                plans[function.name] = _select_function(
                    function, index, layouts, collector.bindings, plans, analysis
                )
            except NotNative:
                continue
    # Native recursion requires SBCL's stack/entry checks before local edges can
    # bypass the existing boxed frame guard. Keep recursive components boxed.
    cyclic = set()
    for name, plan in plans.items():
        pending, seen = list(plan.calls), set()
        while pending:
            target = pending.pop()
            if target == name:
                cyclic.add(name)
                break
            if target not in seen and target in plans:
                seen.add(target)
                pending.extend(plans[target].calls)
    for name in cyclic:
        del plans[name]
    while True:
        invalid = {
            name
            for name, plan in plans.items()
            if any(
                target not in plans or plans[target].dependencies
                for target in plan.calls
            )
        }
        if not invalid:
            return plans
        for name in invalid:
            del plans[name]


def _select_function(function, index, layouts, bindings, plans, analysis):
    parameters = []
    for ordinal, contract in enumerate(function.parameter_annotations):
        if ordinal == 0 and function.owner_class is not None:
            parameters.append(
                Instance(function.owner_class, layouts[function.owner_class])
            )
        elif contract is not None:
            parameters.append(contract.accept(_Contract()))
        else:
            raise NotNative
    if len(parameters) != len(function.parameters):
        raise NotNative
    plan = Plan(
        function,
        index,
        tuple(parameters),
        dict(zip(function.parameters, parameters, strict=True)),
    )
    for name, contract in function.local_annotations.items():
        representation = contract.accept(_LocalContract())
        if representation is not None:
            plan.locals[name] = representation
    selector = _Select(plan, layouts, bindings, plans)
    previous = None
    while True:
        deferred = False
        for block in function.blocks:
            if block.exception_target is not None or function.variadic:
                raise NotNative
            selector.block = block
            for operation in (*block.instructions, block.terminator):
                try:
                    result = operation.accept(selector)
                    name = operation.defined_result()
                    if name is not None:
                        plan.values[name] = result
                except Deferred:
                    deferred = True
        if not deferred:
            break
        state = (dict(plan.locals), dict(plan.values))
        if state == previous:
            break
        previous = state
    if deferred or plan.returned is None:
        raise NotNative
    for block in function.blocks:
        for op in block.instructions:
            op.accept(_BoundLocals(plan, analysis.types[function.name]))
    constraints = _ResultContracts(function)
    for block in function.blocks:
        for operation in (*block.instructions, block.terminator):
            operation.accept(constraints)
    arithmetic = _ArithmeticProof(analysis.types[function.name], constraints.values)
    for block in function.blocks:
        for op in block.instructions:
            op.accept(arithmetic)
    return plan


class _BoundLocals(ir.Visitor):
    def __init__(self, plan, facts):
        self.plan, self.facts = plan, facts

    def visit_Load(self, op):
        if op.local and self.facts[op.result].may_be_unbound:
            raise NotNative


class _ResultContracts(ir.Visitor):
    def __init__(self, function):
        self.function, self.values = function, set()

    def visit_AssertType(self, op):
        if op.contract is EXPORTS["int64"]:
            self.values.add(op.value)

    def visit_Return(self, op):
        if self.function.return_annotation is EXPORTS["int64"]:
            self.values.add(op.value)


class _ArithmeticProof(ir.Visitor):
    def __init__(self, facts, constrained):
        self.facts, self.constrained = facts, constrained

    def require(self, op):
        if op.result not in self.constrained and not self.facts[op.result].fits(
            EXPORTS["int64"].fact
        ):
            raise NotNative

    def visit_Binary(self, op):
        if op.operator not in {
            ast.BinaryOperator.BIT_AND,
            ast.BinaryOperator.BIT_OR,
            ast.BinaryOperator.BIT_XOR,
        }:
            self.require(op)

    def visit_Unary(self, op):
        if op.operator != ast.UnaryOperator.NOT:
            self.require(op)


class _NativeLiteral(ast.Visitor):
    def visit_IntegerLiteral(self, literal):
        return str(literal.value)

    def visit_BooleanLiteral(self, literal):
        return "true" if literal.value else "false"

    def visit_NoneLiteral(self, literal):
        return "0"


def adapter(emitter, plan):
    """Boxed entry checks physical boundaries once, then calls a raw entry."""
    parameters = ", ".join(
        ["ptr %globals", *(f"ptr %parameter.{i}" for i in range(len(plan.parameters)))]
    )
    attributes = " noinline" if plan.function.policy.debug == 3 else ""
    emitter.lines = [
        f"define internal ptr {emitter.body}({parameters}){attributes} {{",
        "entry:",
    ]
    emitter.line("%result = alloca ptr")
    emitter.line("store ptr null, ptr %result")
    emitter.line("%native.error = alloca i1")
    emitter.line("store i1 false, ptr %native.error")
    dependencies = []
    slots = []
    for index, (name, prediction) in enumerate(plan.dependencies.items()):
        slot = f"%dependency.{index}"
        slots.append(slot)
        emitter.line(f"{slot} = alloca ptr")
        emitter.line(f"store ptr null, ptr {slot}")
    for index, (name, prediction) in enumerate(plan.dependencies.items()):
        value = emitter.operation(
            f"call ptr @mule_load_global(ptr %globals, ptr {emitter.module.text(name)})"
        )
        emitter.line(f"store ptr {value}, ptr {slots[index]}")
        emitter.check_pointer(value)
        matched = emitter.operation(
            f"call i32 @mule_native_class_matches(ptr {value}, ptr {prediction.layout.schema}, ptr %globals)"
        )
        wrong = emitter.operation(f"icmp eq i32 {matched}, 0")
        continuation = emitter.fresh("layout.ok")[1:]
        emitter.line(f"br i1 {wrong}, label %fallback, label %{continuation}")
        emitter.lines.append(continuation + ":")
        dependencies.append(value)
    arguments = [
        rep.parameter(emitter, f"%parameter.{i}")
        for i, rep in enumerate(plan.parameters)
    ]
    call = ["ptr %globals", "ptr %native.error"]
    call.extend(
        f"{rep.llvm_type} {value}"
        for rep, value in zip(plan.parameters, arguments, strict=True)
    )
    call.extend(f"ptr {value}" for value in dependencies)
    result = emitter.operation(
        f"call {plan.returned.llvm_type} {plan.entry}({', '.join(call)})"
    )
    failed = emitter.operation("load i1, ptr %native.error")
    emitter.guard(failed)
    boxed = plan.returned.box(emitter, result)
    emitter.line(f"store ptr {boxed}, ptr %result")
    emitter.line("br label %cleanup")
    if plan.dependencies:
        emitter.lines.append("fallback:")
        for slot in slots:
            emitter.release(emitter.operation(f"load ptr, ptr {slot}"))
        result = emitter.operation(f"call ptr {emitter.body}.boxed({parameters})")
        emitter.line(f"ret ptr {result}")
    emitter.lines.append("error:")
    emitter.line("br label %cleanup")
    emitter.lines.append("cleanup:")
    for slot in slots:
        emitter.release(emitter.operation(f"load ptr, ptr {slot}"))
    for i in range(len(plan.parameters)):
        emitter.release(f"%parameter.{i}")
    result = emitter.operation("load ptr, ptr %result")
    emitter.line(f"ret ptr {result}")
    emitter.lines.append("}")
    return "\n".join(emitter.lines)


class Emitter(ir.Visitor):
    """Native entry: borrowed raw slots and scalars, ownership only at edges."""

    def __init__(self, module, plan):
        self.module, self.plan = module, plan
        self.lines, self.serial = [], 0
        self.values = {}
        self.labels = {
            block.label: f"block.{i}" for i, block in enumerate(plan.function.blocks)
        }
        self.locals = {name: f"%local.{i}" for i, name in enumerate(plan.locals)}
        self.owners = {name: f"%owner.{i}" for i, name in enumerate(plan.allocations)}
        self.dependencies = {
            name: f"%class.{i}" for i, name in enumerate(plan.dependencies)
        }

    def line(self, text):
        self.lines.append("  " + text)

    def fresh(self, prefix="n"):
        self.serial += 1
        return f"%{prefix}.{self.serial}"

    def operation(self, text):
        result = self.fresh()
        self.line(f"{result} = {text}")
        return result

    def guard(self, failed):
        continuation = self.fresh("ok")[1:]
        self.line(f"br i1 {failed}, label %error, label %{continuation}")
        self.lines.append(continuation + ":")

    def check_pointer(self, value):
        self.guard(self.operation(f"icmp eq ptr {value}, null"))

    def check_status(self, value):
        self.guard(self.operation(f"icmp slt i32 {value}, 0"))

    def field_address(self, receiver, name):
        instance = self.plan.values[receiver]
        index = instance.layout.definition.fields.index(name)
        return self.operation(
            f"getelementptr i64, ptr {self.values[receiver]}, i64 {index}"
        )

    def arithmetic(self, opcode, left, right):
        if opcode in ("add", "sub", "mul"):
            if self.plan.function.policy.safety:
                checked = self.operation(
                    f"call {{i64, i1}} @llvm.s{opcode}.with.overflow.i64(i64 {left}, i64 {right})"
                )
                value = self.operation(f"extractvalue {{i64, i1}} {checked}, 0")
                overflow = self.operation(f"extractvalue {{i64, i1}} {checked}, 1")
                failed, continuation = (
                    self.fresh("overflow")[1:],
                    self.fresh("bounded")[1:],
                )
                self.line(f"br i1 {overflow}, label %{failed}, label %{continuation}")
                self.lines.append(failed + ":")
                self.line("call i32 @mule_native_overflow()")
                self.line("br label %error")
                self.lines.append(continuation + ":")
                return value
            return self.operation(f"{opcode} nsw i64 {left}, {right}")
        return self.operation(f"{opcode} i64 {left}, {right}")

    def emit(self):
        plan = self.plan
        parameters = ["ptr %globals", "ptr %error.flag"]
        parameters.extend(
            f"{rep.llvm_type} %parameter.{i}" for i, rep in enumerate(plan.parameters)
        )
        parameters.extend(f"ptr {value}" for value in self.dependencies.values())
        attributes = " noinline" if plan.function.policy.debug == 3 else ""
        self.lines = [
            f"define internal {plan.returned.llvm_type} {plan.entry}({', '.join(parameters)}){attributes} {{",
            "entry:",
        ]
        self.line(f"%return = alloca {plan.returned.llvm_type}")
        self.line(f"store {plan.returned.llvm_type} {plan.returned.zero}, ptr %return")
        self.line("%call.error = alloca i1")
        for name, rep in plan.locals.items():
            self.line(f"{self.locals[name]} = alloca {rep.llvm_type}")
        for slot in self.owners.values():
            self.line(f"{slot} = alloca ptr")
            self.line(f"store ptr null, ptr {slot}")
        for i, name in enumerate(plan.function.parameters):
            self.line(
                f"store {plan.parameters[i].llvm_type} %parameter.{i}, ptr {self.locals[name]}"
            )
        self.line(f"br label %{self.labels[plan.function.blocks[0].label]}")
        for block in plan.function.blocks:
            self.lines.append(self.labels[block.label] + ":")
            for op in block.instructions:
                op.accept(self)
            block.terminator.accept(self)
        self.lines.append("error:")
        self.line("store i1 true, ptr %error.flag")
        self.line("br label %cleanup")
        self.lines.append("cleanup:")
        for slot in self.owners.values():
            owner = self.operation(f"load ptr, ptr {slot}")
            self.line(f"call void @mule.fast.release(ptr {owner})")
        result = self.operation(f"load {plan.returned.llvm_type}, ptr %return")
        self.line(f"ret {plan.returned.llvm_type} {result}")
        self.lines.append("}")
        return "\n".join(self.lines)

    def visit_Constant(self, op):
        self.values[op.result] = op.value.accept(_NativeLiteral())

    def visit_AssertType(self, op):
        self.values[op.result] = self.values[op.value]

    def visit_Load(self, op):
        if op.local:
            rep = self.plan.locals[op.name]
            self.values[op.result] = self.operation(
                f"load {rep.llvm_type}, ptr {self.locals[op.name]}"
            )
        else:
            self.values[op.result] = self.dependencies[op.name]

    def visit_Store(self, op):
        rep = self.plan.locals[op.name]
        self.line(
            f"store {rep.llvm_type} {self.values[op.value]}, ptr {self.locals[op.name]}"
        )

    def visit_Binary(self, op):
        self.values[op.result] = self.arithmetic(
            _BINARY[op.operator], self.values[op.left], self.values[op.right]
        )

    def visit_Unary(self, op):
        value = self.values[op.operand]
        if op.operator == ast.UnaryOperator.NOT:
            truth = self.plan.values[op.operand].truth(self, value)
            value = self.operation(f"xor i1 {truth}, true")
        elif op.operator == ast.UnaryOperator.NEGATIVE:
            value = self.arithmetic("sub", "0", value)
        elif op.operator == ast.UnaryOperator.INVERT:
            value = self.operation(f"xor i64 {value}, -1")
        self.values[op.result] = value

    def visit_Compare(self, op):
        self.values[op.result] = self.operation(
            f"icmp {_COMPARE[op.operator]} i64 {self.values[op.left]}, {self.values[op.right]}"
        )

    def visit_LoadAttribute(self, op):
        self.values[op.result] = self.operation(
            f"load i64, ptr {self.field_address(op.receiver, op.name)}"
        )

    def visit_StoreAttribute(self, op):
        self.line(
            f"store i64 {self.values[op.value]}, ptr {self.field_address(op.receiver, op.name)}"
        )

    def visit_ResolveMethod(self, op):
        self.values[op.result] = self.values[op.receiver]

    def visit_CallMethod(self, op):
        method = self.plan.values[op.callee]
        target = self.module.native_plans[method.target.name]
        self.line("store i1 false, ptr %call.error")
        parameters = [
            "ptr %globals",
            "ptr %call.error",
            f"ptr {self.values[op.callee]}",
        ]
        parameters.extend(
            f"{self.plan.values[name].llvm_type} {self.values[name]}"
            for name in op.args
        )
        result = self.operation(
            f"call {target.returned.llvm_type} {target.entry}({', '.join(parameters)})"
        )
        failed = self.operation("load i1, ptr %call.error")
        self.guard(failed)
        self.values[op.result] = result

    def visit_Call(self, op):
        callee = self.plan.allocations[op.result]
        boxed = [
            self.plan.values[value].box(self, self.values[value]) for value in op.args
        ]
        owned_class = self.operation(
            f"call ptr @mule.fast.retain(ptr {self.values[op.callee]})"
        )
        parameters = [
            "ptr %globals",
            f"ptr {owned_class}",
            *(f"ptr {value}" for value in boxed),
        ]
        body = self.module.bodies[callee.layout.protocols["construct"]]
        instance = self.operation(f"call ptr {body}({', '.join(parameters)})")
        self.line(f"store ptr {instance}, ptr {self.owners[op.result]}")
        self.check_pointer(instance)
        payload = self.operation(f"call ptr @mule_native_payload(ptr {instance})")
        self.check_pointer(payload)
        self.values[op.result] = payload

    def visit_Discard(self, op):
        pass

    def visit_Jump(self, op):
        self.line(f"br label %{self.labels[op.target]}")

    def visit_Branch(self, op):
        truth = self.plan.values[op.condition].truth(self, self.values[op.condition])
        self.line(
            f"br i1 {truth}, label %{self.labels[op.true]}, label %{self.labels[op.false]}"
        )

    def visit_Return(self, op):
        self.line(
            f"store {self.plan.returned.llvm_type} {self.values[op.value]}, ptr %return"
        )
        self.line("br label %cleanup")
