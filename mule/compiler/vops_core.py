"""Core VOP definitions, layout constants, literal emitters, and control/call/check templates."""

from __future__ import annotations

import struct
from enum import IntEnum

from mule import ast, procedures

from . import fndb
from .annotation_types import ContractVisitor
from .ctype import (
    INTEGER,
    UNKNOWN,
    ValueTypeVisitor,
)
from .vmdef import define_vop
from .vop import (
    BOOLEAN,
    CLASS,
    DESCRIPTOR,
    DOUBLE,
    FLOAT_BOX_LAYOUT,
    IMMEDIATE,
    NULL,
    REFERENCE,
    SIGNED,
)

HOT_BRANCH = '!prof !{!"branch_weights", i32 2000, i32 1}'
COLD_BRANCH = '!prof !{!"branch_weights", i32 1, i32 2000}'

OBJECT = (DESCRIPTOR, IMMEDIATE)
RAW = "raw"

BINARY_OPCODES = {
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
UNARY_OPCODES = {
    ast.UnaryOperator.POSITIVE: 0,
    ast.UnaryOperator.NEGATIVE: 1,
    ast.UnaryOperator.INVERT: 2,
    ast.UnaryOperator.NOT: 3,
}
COMPARE_OPCODES = {
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
_PRIMITIVE_BINARY = {0: "add", 1: "sub", 2: "mul", 10: "and", 11: "xor", 12: "or"}
_RAW_BINARY = {
    ast.BinaryOperator.ADD: "add",
    ast.BinaryOperator.SUBTRACT: "sub",
    ast.BinaryOperator.MULTIPLY: "mul",
    ast.BinaryOperator.BIT_AND: "and",
    ast.BinaryOperator.BIT_OR: "or",
    ast.BinaryOperator.BIT_XOR: "xor",
}
_RAW_COMPARE = {
    ast.ComparisonOperator.EQUAL: "eq",
    ast.ComparisonOperator.NOT_EQUAL: "ne",
    ast.ComparisonOperator.LESS: "slt",
    ast.ComparisonOperator.LESS_EQUAL: "sle",
    ast.ComparisonOperator.GREATER: "sgt",
    ast.ComparisonOperator.GREATER_EQUAL: "sge",
}
_RAW_FLOAT_BINARY = {
    ast.BinaryOperator.ADD: "fadd",
    ast.BinaryOperator.SUBTRACT: "fsub",
    ast.BinaryOperator.MULTIPLY: "fmul",
    ast.BinaryOperator.DIVIDE: "fdiv",
}
# Ordered predicates: a NaN operand compares unequal, as in Python.
_RAW_FLOAT_COMPARE = {
    ast.ComparisonOperator.EQUAL: "oeq",
    ast.ComparisonOperator.NOT_EQUAL: "une",
    ast.ComparisonOperator.LESS: "olt",
    ast.ComparisonOperator.LESS_EQUAL: "ole",
    ast.ComparisonOperator.GREATER: "ogt",
    ast.ComparisonOperator.GREATER_EQUAL: "oge",
}


# Fixed templates: emitted by the IR2 converter for IR1 nodes other than
# known calls.


@define_vop("insert-safepoint")
def insert_safepoint(gen, vop):
    requested = gen.operation(
        "load atomic i8, ptr @mule_safepoint_requested acquire, align 1"
    )
    pending = gen.operation(f"icmp ne i8 {requested}, 0")
    park, ready = (
        gen.fresh(name)[1:] for name in ("safepoint.park", "safepoint.ready")
    )
    gen.line(f"br i1 {pending}, label %{park}, label %{ready}, {COLD_BRANCH}")
    gen.lines.append(park + ":")
    prev_cold = gen.in_cold_block
    gen.in_cold_block = True
    status = gen.operation("call i32 @mule_safepoint()")
    gen.check_status(status)
    gen.in_cold_block = prev_cold
    gen.line(f"br label %{ready}")
    gen.lines.append(ready + ":")


@define_vop(
    "constant",
    args=(),
    results=((DESCRIPTOR, IMMEDIATE, SIGNED, BOOLEAN, NULL, DOUBLE),),
)
def constant(gen, vop):
    result = vop.results[0]
    literal = vop.codegen_info
    if result.tn.sc in (SIGNED, BOOLEAN, NULL, DOUBLE):
        gen.store(result, literal.accept(_RawLiteral()))
        return
    literal.accept(_BoxedConstant(gen, result, vop))


class _BoxedConstant(ast.Visitor):
    """Materialize constants using their compiled representation."""

    def __init__(self, gen, result, vop=None):
        self.gen = gen
        self.result = result
        self.vop = vop

    def visit_Node(self, node):
        if self.result.tn.borrowed_from is not None or not self.result.tn.sc.owned:
            self.gen.pointer_result(self.result, self.gen.borrowed_constant(node))
            return
        self.gen.pointer_result(self.result, self.gen.constant(node))

    def _infallible(self, node):
        if self.result.tn.borrowed_from is not None or not self.result.tn.sc.owned:
            self.gen.native_result(
                self.result, self.gen.operation(self.gen.borrowed_constant(node))
            )
            return
        self.gen.native_result(self.result, self.gen.operation(self.gen.constant(node)))

    def visit_TextLiteral(self, node):
        if _is_inline_text_compare_only(self.gen, self.result.tn, node.value):
            self.gen.native_result(self.result, "null")
            return
        value = self.gen.operation(self.gen.borrowed_constant(node))
        self.gen.immortal_values.add(value)
        self.gen.native_result(self.result, value)

    def visit_BytesLiteral(self, node):
        self._infallible(node)

    def visit_FloatLiteral(self, node):
        self._infallible(node)

    def visit_Tuple(self, node):
        if self.vop is not None and self.gen.is_unpacked_return_vop(self.vop):
            values = [
                self.gen.operation(self.gen.constant(element))
                for element in node.elements
            ]
            self.gen.unpacked_return_result(self.result, values)
            return
        self.visit_Node(node)

    def visit_NoneLiteral(self, node):
        self.gen.native_result(self.result, "@mule.none")

    def visit_BooleanLiteral(self, node):
        self.gen.native_result(
            self.result, "@mule.true" if node.value else "@mule.false"
        )

    def visit_IntegerLiteral(self, node):
        value = node.value
        if -(1 << 62) <= value < (1 << 62):
            self.gen.native_result(
                self.result, self.gen.operation(self.gen.constant(node))
            )
            return
        self.visit_Node(node)

    def visit_ComplexLiteral(self, node):
        # Float boxing is infallible. The compiled call consumes both owned
        # components on every path, including failed frame entry.
        components = [
            self.gen.operation(
                f"call ptr @mule_literal_float(double {double_constant(value)})"
            )
            for value in (node.value.real, node.value.imag)
        ]
        value = self.gen.sequence_call("complex_literal", components)
        self.gen.publish(self.result, value)
        self.gen.check_pointer(value)


class _RawLiteral(ast.Visitor):
    def visit_Node(self, node):
        raise TypeError(f"no raw representation for {node!r}")

    def visit_IntegerLiteral(self, node):
        return str(node.value)

    def visit_FloatLiteral(self, node):
        return double_constant(node.value)

    def visit_BooleanLiteral(self, node):
        return "true" if node.value else "false"

    def visit_NoneLiteral(self, node):
        return "0"


def double_constant(value: float) -> str:
    """LLVM's exact hexadecimal spelling of a binary64 constant."""
    bits = struct.unpack(">Q", struct.pack(">d", value))[0]
    return f"0x{bits:016X}"


@define_vop("load-local", args=(None,), results=(None,))
def load_local(gen, vop):
    if gen.is_unpacked_discard_load(vop):
        gen.immortal_values.add("@mule.none")
        gen.publish(vop.results[0], "@mule.none")
        return
    source, result = vop.args[0], vop.results[0]
    access = vop.codegen_info
    if source.tn.sc not in OBJECT:
        value = gen.load(source)
        if source.tn.sc is REFERENCE and not access.bound:
            missing = gen.operation(f"icmp eq ptr {value}, null")
            absent, present = (
                gen.fresh(name)[1:] for name in ("local.absent", "local.present")
            )
            gen.line(
                f"br i1 {missing}, label %{absent}, label %{present}, {COLD_BRANCH}"
            )
            gen.lines.append(absent + ":")
            with gen.cold_block():
                name = gen.module.text(access.name)
                gen.pointer_result(
                    result, f"call ptr @mule.fast.local.move(ptr null, ptr {name})"
                )
                gen.line(f"br label %{present}")
            gen.lines.append(present + ":")
        gen.store(result, value)
        return
    value = gen.load(source)
    if access.move:
        gen.line(f"store ptr null, ptr {source.tn.slot}")
    if access.bound:
        # Slot loads already record their facts; register loads do not.
        if source.tn in gen.registers:
            gen.record_facts(source.tn, value)
        gen.record_facts(result.tn, value)
        if (
            not access.move
            and not access.borrow
            and result.tn.sc.owned
            and source.tn.sc.owned
        ):
            value = gen.retain(value)
        gen.publish(result, value)
        return
    if access.allow_unbound:
        gen.pointer_result(result, f"call ptr @mule.fast.local.optional(ptr {value})")
        return
    helper = (
        "local.move"
        if access.move or not result.tn.sc.owned or not source.tn.sc.owned
        else "local"
    )
    name = gen.module.text(access.name)
    gen.pointer_result(result, f"call ptr @mule.fast.{helper}(ptr {value}, ptr {name})")


@define_vop("load-global", results=(None,))
def load_global(gen, vop):
    result = vop.results[0]
    if vop.codegen_info in gen.context.final_bindings:
        cell = gen.module.cell(vop.codegen_info.namespace, vop.codegen_info.name)
        value = gen.operation(f"load ptr, ptr {cell}, !nonnull !{{}}")
        if result.tn.sc.owned and result.tn.borrowed_from is None:
            value = gen.retain(value)
        gen.publish(result, value)
        return
    if result.tn.sc is CLASS:
        # A native body's class dependency was loaded and guarded at entry.
        gen.store(result, gen.dependency(vop.codegen_info))
        return
    if result.tn.borrowed_from is not None:
        cell = gen.module.cell(vop.codegen_info.namespace, vop.codegen_info.name)
        value = gen.operation(f"load ptr, ptr {cell}, !nonnull !{{}}")
        gen.publish(result, value)
        return
    operation = gen.module.load_global(vop.codegen_info, gen.lambda_.policy.safety)
    if operation.startswith("call ptr @mule.global.unchecked("):
        gen.native_result(result, gen.operation(operation))
        return
    gen.pointer_result(result, operation)


@define_vop("move", args=(None,), results=(None,))
def move(gen, vop):
    # ir2tran imports this template table during module initialization.
    from .ir2tran import DEAD_LOCAL_STORE

    if vop.codegen_info is DEAD_LOCAL_STORE:
        source, target = vop.args[0], vop.results[0]
        value = gen.take(source)
        if target.tn.sc in OBJECT:
            # The previous binding still loses its reference at this assignment.
            gen.store(target, "null")
        if source.tn.sc in OBJECT:
            gen.release(value)
        return
    if gen.is_unpacked_move(vop):
        gen.take(vop.args[0])
        return
    value = gen.take(vop.args[0])
    gen.store(vop.results[0], value, release=vop.codegen_info is not False)


@define_vop("coerce", args=(None,), results=(None,), arg_escapes=(False,))
def coerce(gen, vop):
    """Convert scalar storage while consuming only owned temporaries."""
    source, target = vop.args[0], vop.results[0]
    value = gen.load(source) if source.tn.leaf is not None else gen.take(source)
    if source.tn.sc in OBJECT and target.tn.sc is SIGNED:
        gen.store(target, gen.decode(value))
        return
    if source.tn.sc is SIGNED and target.tn.sc in OBJECT:
        gen.native_result(target, gen.encode(value))
        return
    if source.tn.sc is DESCRIPTOR and target.tn.sc is DOUBLE:
        raw = gen.operation(
            f"call double @{FLOAT_BOX_LAYOUT.symbol('load')}(ptr {value})"
        )
        gen.store(target, raw)
        if source.tn.leaf is None:
            gen.release(value)
        return
    if source.tn.sc is DOUBLE and target.tn.sc is DESCRIPTOR:
        gen.pointer_result(
            target, f"call ptr @{FLOAT_BOX_LAYOUT.symbol()}(double {value})"
        )
        return
    raise ValueError(f"unsupported scalar coercion: {source.tn} -> {target.tn}")


@define_vop("move-argument", results=(None,))
def move_argument(gen, vop):
    result = vop.results[0]
    gen.store(result, gen.parameter_value(vop.codegen_info, result), release=False)
    if vop.codegen_info == len(gen.lambda_.vars) - 1:
        gen.check_native_receiver()


@define_vop("load-closure", results=(None,))
def load_closure(gen, vop):
    result = vop.results[0]
    # Bind enumerates the compiler-created immutable environment once. Its
    # physical buffer stays live through the callable's owned closure tuple.
    if vop.codegen_info == 0:
        gen.line("%closure.slots = call ptr @mule_closure_slots(ptr %closure)")
    slot = gen.operation(
        f"getelementptr ptr, ptr %closure.slots, i64 {vop.codegen_info}"
    )
    value = gen.operation(f"load ptr, ptr {slot}")
    value = gen.retain(value)
    gen.publish(result, value, release=False)
    gen.refine(result.tn, value)


@define_vop("local-address", args=(None,), results=(None,))
def local_address(gen, vop):
    gen.store(vop.results[0], vop.args[0].tn.slot)


@define_vop("local-load-closure", results=(None,))
def local_load_closure(gen, vop):
    index, indirect = vop.codegen_info
    slot = gen.operation(f"getelementptr ptr, ptr %closure, i64 {index}")
    address = gen.operation(f"load ptr, ptr {slot}")
    if indirect:
        gen.store(vop.results[0], address)
    else:
        value = gen.operation(f"load ptr, ptr {address}")
        if vop.results[0].tn.sc.owned:
            value = gen.retain(value)
        gen.publish(vop.results[0], value, release=False)


@define_vop("local-cell-ref", args=(None,), results=(None,))
def local_cell_ref(gen, vop):
    address = gen.load(vop.args[0])
    value = gen.operation(f"load ptr, ptr {address}")
    missing = gen.operation(f"icmp eq ptr {value}, null")
    absent, present, join = (
        gen.fresh(name)[1:]
        for name in ("capture.absent", "capture.present", "capture.join")
    )
    gen.line(f"br i1 {missing}, label %{absent}, label %{present}, {COLD_BRANCH}")
    gen.lines.append(absent + ":")
    with gen.cold_block():
        optional = "@mule.true" if vop.codegen_info else "@mule.false"
        fallback = gen.sequence_call("cell_unbound", [optional])
        absent_block = gen.current_block()
        gen.line(f"br label %{join}")
    gen.lines.append(present + ":")
    if vop.results[0].tn.sc.owned:
        value = gen.retain(value)
    present_block = gen.current_block()
    gen.line(f"br label %{join}")
    gen.lines.append(join + ":")
    gen.pointer_result(
        vop.results[0],
        f"phi ptr [{fallback}, %{absent_block}], [{value}, %{present_block}]",
    )


@define_vop("local-cell-set", args=(None, None))
def local_cell_set(gen, vop):
    address = gen.load(vop.args[0])
    value = gen.take(vop.args[1])
    old = gen.operation(f"load ptr, ptr {address}")
    gen.line(f"store ptr {value}, ptr {address}")
    if vop.codegen_info:
        gen.release(old)


@define_vop("local-cell-clear", args=(None,))
def local_cell_clear(gen, vop):
    address = gen.load(vop.args[0])
    old = gen.operation(f"load ptr, ptr {address}")
    gen.line(f"store ptr null, ptr {address}")
    if vop.codegen_info:
        gen.release(old)


@define_vop("local-function", results=(None,))
def local_function(gen, vop):
    environment = gen.fresh("local.environment")
    gen.lines.insert(
        gen.entry_end, f"  {environment} = alloca [{max(1, len(vop.args))} x ptr]"
    )
    for index, ref in enumerate(vop.args):
        slot = gen.operation(f"getelementptr ptr, ptr {environment}, i64 {index}")
        gen.line(f"store ptr {gen.take(ref)}, ptr {slot}")
    gen.store(vop.results[0], environment)


@define_vop("local-extent-call", results=(None,))
def local_extent_call(gen, vop):
    environment = gen.take(vop.args[0])
    arguments = [gen.take(ref) for ref in vop.args[1:]]
    value = gen.call(
        vop.codegen_info,
        environment,
        vop.args[1:],
        arguments,
        unpack_arity=gen.unpacked_call_arity(vop),
        result_contract=gen.call_result_contract(vop),
    )
    gen.publish(vop.results[0], value)
    gen.check_pointer(value)


@define_vop("unbind-local", results=(None,))
def unbind_local(gen, vop):
    target = vop.results[0]
    if target.tn.sc.owned and vop.codegen_info is not False:
        value = gen.load(target)
        gen.line(f"store ptr null, ptr {target.tn.slot}")
        gen.release(value)
    else:
        gen.line(
            f"store {target.tn.sc.llvm_type} {target.tn.sc.zero}, ptr {target.tn.slot}"
        )


@define_vop("store-global", args=(None,))
def store_global(gen, vop):
    value = gen.take(vop.args[0])
    var, final_binding = vop.codegen_info
    status = gen.operation(
        gen.module.store_global(
            var.namespace, var.name, value, final_binding=final_binding
        )
    )
    gen.release(value)
    gen.check_status(status)


@define_vop("unbind-global")
def unbind_global(gen, vop):
    var, missing_ok = vop.codegen_info
    status = gen.operation(gen.module.delete_global(var, missing_ok=missing_ok))
    gen.check_status(status)


@define_vop("discard", args=(None,), arg_escapes=(False,))
def discard(gen, vop):
    operand = vop.args[0]
    if operand.tn.sc in OBJECT:
        gen.release(gen.take(operand))


@define_vop("type-check", args=(None,), results=(None,))
def type_check(gen, vop):
    operand, result = vop.args[0], vop.results[0]
    if gen.is_unpacked_result_check(vop):
        gen.publish(result, gen.take(operand))
        return
    check = vop.codegen_info
    if operand.tn.sc not in OBJECT:
        gen.store(result, gen.take(operand))
        return
    actual = operand.tn.type or UNKNOWN
    checked = (
        check.safety
        and not actual.fits(check.contract.checked_type)
        and not gen.is_direct_result_check(vop)
    )
    if checked:
        # Keep the unrefined operand owned by its original slot until the
        # assertion succeeds, so error cleanup releases its actual value.
        value = gen.load(operand)
        check.contract.accept(_ContractAssertion(gen, value))
    value = gen.take(operand)
    if checked:
        # Failure landings still own the unchecked input. Keep its SSA identity
        # separate so successful refinement cannot change their release facts.
        value = gen.operation(f"getelementptr i8, ptr {value}, i64 0")
    gen.publish(result, value)
    if checked or not actual.fits(result.tn.type or UNKNOWN):
        gen.refine(result.tn, value)
    else:
        gen.record_facts(result.tn, value)


class _ContractAssertion(ContractVisitor):
    def __init__(self, gen, value):
        self.gen = gen
        self.value = value

    def visit_Contract(self, contract):
        raise TypeError(f"unsupported annotation contract {contract}")

    def visit_InferredReturnContract(self, contract):
        valid = contract.fact.accept(_InferredResultPredicate(self, self.value))
        self.finish_inferred_check(contract, valid)

    def finish_inferred_check(self, contract, valid):
        accepted = self.gen.fresh("return.checked")[1:]
        rejected = self.gen.fresh("return.incompatible")[1:]
        self.gen.line(
            f"br i1 {valid}, label %{accepted}, label %{rejected}, {HOT_BRANCH}"
        )
        self.gen.lines.append(rejected + ":")
        with self.gen.cold_block():
            expected = self.gen.module.text(f"{contract.display_name}: {contract.fact}")
            self.gen.line(f"call i32 @mule_inferred_return_error(ptr {expected})")
            self.gen.propagate()
        self.gen.lines.append(accepted + ":")

    def visit_IntegerContract(self, contract):
        fact = contract.fact
        module = self.gen.module
        minimum = self.integer_bound(fact.minimum)
        maximum = self.integer_bound(fact.maximum)
        name = module.text(contract.display_name)
        status = self.gen.operation(
            f"call i32 @mule_assert_integer(ptr {self.value}, ptr {minimum}, "
            f"ptr {maximum}, ptr {name})"
        )
        self.gen.check_status(status)

    def integer_bound(self, value: int | None) -> str:
        if value is None:
            return "null"
        bound = self.gen.operation(
            ast.IntegerLiteral(value).accept(BorrowedLiteralEmitter(self.gen.module))
        )
        if not -(1 << 62) <= value < (1 << 62):
            self.gen.check_pointer(bound)
        return bound

    def visit_FloatContract(self, contract):
        name = self.gen.module.text(contract.display_name)
        status = self.gen.operation(
            f"call i32 @mule_assert_float(ptr {self.value}, ptr {name})"
        )
        self.gen.check_status(status)

    def visit_PythonIntegerContract(self, contract):
        gen = self.gen
        bits = gen.operation(f"ptrtoint ptr {self.value} to i64")
        tag = gen.operation(f"and i64 {bits}, 1")
        fixnum = gen.operation(f"icmp ne i64 {tag}, 0")
        non_fixnum = gen.fresh("integer.non_fixnum")[1:]
        integer = gen.fresh("integer.check")[1:]
        done = gen.fresh("integer.checked")[1:]
        gen.line(f"br i1 {fixnum}, label %{done}, label %{non_fixnum}, {HOT_BRANCH}")
        gen.lines.append(non_fixnum + ":")
        true = gen.operation(f"icmp eq ptr {self.value}, @mule.true")
        false = gen.operation(f"icmp eq ptr {self.value}, @mule.false")
        boolean = gen.operation(f"or i1 {true}, {false}")
        gen.line(f"br i1 {boolean}, label %{done}, label %{integer}")
        gen.lines.append(integer + ":")
        status = gen.operation(
            f"call i32 @mule_assert_integer(ptr {self.value}, ptr null, "
            f"ptr null, ptr {gen.module.text(contract.display_name)})"
        )
        gen.check_status(status)
        gen.line(f"br label %{done}")
        gen.lines.append(done + ":")

    def check_contract_type(self, contract):
        valid = contract.checked_type.accept(_InferredResultPredicate(self, self.value))
        self.finish_inferred_check(contract, valid)

    def visit_BooleanContract(self, contract):
        self.check_contract_type(contract)

    def visit_NoneContract(self, contract):
        self.check_contract_type(contract)

    def visit_TextContract(self, contract):
        self.check_contract_type(contract)

    def visit_BytesContract(self, contract):
        self.check_contract_type(contract)

    def visit_ByteArrayContract(self, contract):
        self.check_contract_type(contract)

    def visit_ListContract(self, contract):
        self.check_contract_type(contract)

    def visit_TupleContract(self, contract):
        self.check_contract_type(contract)

    def visit_DictContract(self, contract):
        self.check_contract_type(contract)

    def visit_SetContract(self, contract):
        self.check_contract_type(contract)

    def visit_ClassContract(self, contract):
        self.check_contract_type(contract)

    def visit_UnionContract(self, contract):
        self.check_contract_type(contract)


class _UnpackedReturnShape(ValueTypeVisitor):
    def __init__(self, arity):
        self.arity = arity

    def visit_ValueType(self, fact):
        return False

    def visit_TupleType(self, fact):
        return (fact.arity is None or fact.arity == self.arity) and (
            fact.elements is None or len(fact.elements) == self.arity
        )


def checks_unpacked_return(contract, arity):
    return contract.fact.accept(_UnpackedReturnShape(arity))


def assert_unpacked_return(gen, contract, slots, arity):
    assertion = _ContractAssertion(gen, slots)
    predicate = contract.fact.accept(_UnpackedResultPredicate(assertion, arity))
    assertion.finish_inferred_check(contract, predicate)


class _UnpackedResultPredicate(ValueTypeVisitor):
    def __init__(self, assertion, arity):
        self.assertion = assertion
        self.arity = arity

    def visit_ValueType(self, fact):
        raise TypeError(f"unsupported unpacked return contract {fact!r}")

    def visit_TupleType(self, fact):
        if not fact.elements or all(element is UNKNOWN for element in fact.elements):
            return "true"
        gen = self.assertion.gen
        predicates = []
        for index, element in enumerate(fact.elements):
            if element is UNKNOWN:
                continue
            slot = gen.operation(
                f"getelementptr ptr, ptr {self.assertion.value}, i64 {index}"
            )
            value = gen.operation(f"load ptr, ptr {slot}")
            predicates.append(
                element.accept(_InferredResultPredicate(self.assertion, value))
            )
        return _InferredResultPredicate(self.assertion, "null").combine(
            "and", predicates, "true"
        )


class _ReturnValueKind(IntEnum):
    """Physical borrowed-value categories in runtime/annotations.rs."""

    INTEGER = 1
    BOOLEAN = 2
    FLOAT = 3
    NONE = 4
    TEXT = 5
    BYTES = 6
    TUPLE = 7
    LIST = 8
    DICT = 9
    CLASS = 10
    FUNCTION = 11


class _InferredResultPredicate(ValueTypeVisitor):
    """Check normalized return facts without acquiring the value or its fields."""

    def __init__(self, assertion, value):
        self.assertion = assertion
        self.gen = assertion.gen
        self.value = value

    def visit_ValueType(self, fact):
        raise TypeError(f"unsupported inferred return contract {fact!r}")

    def visit_UnknownType(self, fact):
        return "true"

    def kind(self, expected):
        actual = self.gen.operation(f"call i32 @mule_value_kind(ptr {self.value})")
        return self.gen.operation(f"icmp eq i32 {actual}, {int(expected)}")

    def combine(self, operator, values, identity):
        result = identity
        for value in values:
            result = self.gen.operation(f"{operator} i1 {result}, {value}")
        return result

    def visit_IntegerType(self, fact):
        if fact.is_fixnum:
            bits = self.gen.operation(f"ptrtoint ptr {self.value} to i64")
            tag = self.gen.operation(f"and i64 {bits}, 1")
            valid = self.gen.operation(f"icmp ne i64 {tag}, 0")
            decoded = self.gen.operation(f"ashr i64 {bits}, 1")
            lower = self.gen.operation(f"icmp sge i64 {decoded}, {fact.minimum}")
            upper = self.gen.operation(f"icmp sle i64 {decoded}, {fact.maximum}")
            return self.combine("and", (valid, lower, upper), "true")
        if (fact.minimum is None or fact.minimum <= (1 << 62) - 1) and (
            fact.maximum is None or fact.maximum >= -(1 << 62)
        ):
            bits = self.gen.operation(f"ptrtoint ptr {self.value} to i64")
            tag = self.gen.operation(f"and i64 {bits}, 1")
            fixnum = self.gen.operation(f"icmp ne i64 {tag}, 0")
            checks = []
            need_min = fact.minimum is not None and fact.minimum > -(1 << 62)
            need_max = fact.maximum is not None and fact.maximum < (1 << 62) - 1
            if need_min or need_max:
                decoded = self.gen.operation(f"ashr i64 {bits}, 1")
                if need_min:
                    checks.append(
                        self.gen.operation(f"icmp sge i64 {decoded}, {fact.minimum}")
                    )
                if need_max:
                    checks.append(
                        self.gen.operation(f"icmp sle i64 {decoded}, {fact.maximum}")
                    )
            fast_ok = self.combine("and", checks, "true") if checks else "true"
            source = self.gen.current_block()
            slow_label = self.gen.fresh("return.int.slow")[1:]
            done_label = self.gen.fresh("return.int.done")[1:]
            self.gen.line(
                f"br i1 {fixnum}, label %{done_label}, label %{slow_label}, {HOT_BRANCH}"
            )
            self.gen.lines.append(slow_label + ":")
            with self.gen.cold_block():
                minimum = self.assertion.integer_bound(fact.minimum)
                maximum = self.assertion.integer_bound(fact.maximum)
                valid = self.gen.operation(
                    f"call i32 @mule_integer_in_bounds(ptr {self.value}, ptr {minimum}, ptr {maximum})"
                )
                slow_ok = self.gen.operation(f"icmp ne i32 {valid}, 0")
                last = self.gen.current_block()
                self.gen.line(f"br label %{done_label}")
            self.gen.lines.append(done_label + ":")
            return self.gen.operation(
                f"phi i1 [{fast_ok}, %{source}], [{slow_ok}, %{last}]"
            )
        minimum = self.assertion.integer_bound(fact.minimum)
        maximum = self.assertion.integer_bound(fact.maximum)
        valid = self.gen.operation(
            f"call i32 @mule_integer_in_bounds(ptr {self.value}, ptr {minimum}, ptr {maximum})"
        )
        return self.gen.operation(f"icmp ne i32 {valid}, 0")

    def visit_PythonIntegerType(self, fact):
        integer = self.visit_IntegerType(fact)
        false = self.gen.operation(f"icmp eq ptr {self.value}, @mule.false")
        true = self.gen.operation(f"icmp eq ptr {self.value}, @mule.true")
        choices = [integer]
        if (fact.minimum is None or fact.minimum <= 0) and (
            fact.maximum is None or fact.maximum >= 0
        ):
            choices.append(false)
        if (fact.minimum is None or fact.minimum <= 1) and (
            fact.maximum is None or fact.maximum >= 1
        ):
            choices.append(true)
        return self.combine("or", choices, "false")

    def visit_BooleanType(self, fact):
        choices = []
        if (fact.minimum is None or fact.minimum <= 0) and (
            fact.maximum is None or fact.maximum >= 0
        ):
            choices.append(self.gen.operation(f"icmp eq ptr {self.value}, @mule.false"))
        if (fact.minimum is None or fact.minimum <= 1) and (
            fact.maximum is None or fact.maximum >= 1
        ):
            choices.append(self.gen.operation(f"icmp eq ptr {self.value}, @mule.true"))
        return self.combine("or", choices, "false")

    def visit_NoneType(self, fact):
        return self.gen.operation(f"icmp eq ptr {self.value}, @mule.none")

    def visit_FloatType(self, fact):
        return self.kind(_ReturnValueKind.FLOAT)

    def visit_RealType(self, fact):
        return self.combine(
            "or",
            (self.visit_PythonIntegerType(fact), self.visit_FloatType(fact)),
            "false",
        )

    def visit_InstanceType(self, fact):
        layout = self.gen.operation(f"call i64 @mule.attr.layout(ptr {self.value})")
        expected = self.gen.operation(
            f"load i64, ptr {self.gen.module.class_layout_slot(fact.identity)}"
        )
        known = self.gen.operation(f"icmp ne i64 {expected}, 0")
        same = self.gen.operation(f"icmp eq i64 {layout}, {expected}")
        return self.gen.operation(f"and i1 {known}, {same}")

    def visit_TextType(self, fact):
        return self.kind(_ReturnValueKind.TEXT)

    def visit_BytesType(self, fact):
        return self.kind(_ReturnValueKind.BYTES)

    def visit_ByteArrayType(self, fact):
        layout = self.gen.operation(f"call i64 @mule.attr.layout(ptr {self.value})")
        expected = self.gen.operation(
            f"load i64, ptr {self.gen.module.class_layout_slot('mule_sequences_bytearray')}"
        )
        known = self.gen.operation(f"icmp ne i64 {expected}, 0")
        same = self.gen.operation(f"icmp eq i64 {layout}, {expected}")
        return self.gen.operation(f"and i1 {known}, {same}")

    def visit_ListType(self, fact):
        valid = self.kind(_ReturnValueKind.LIST)
        if fact.element is UNKNOWN:
            return valid
        start = self.gen.fresh("return.list.start")[1:]
        header = self.gen.fresh("return.list.header")[1:]
        loop = self.gen.fresh("return.list.loop")[1:]
        body = self.gen.fresh("return.list.body")[1:]
        step = self.gen.fresh("return.list.step")[1:]
        done = self.gen.fresh("return.list.done")[1:]
        self.gen.line(f"br label %{start}")
        self.gen.lines.append(start + ":")
        self.gen.line(f"br i1 {valid}, label %{header}, label %{done}")
        self.gen.lines.append(header + ":")
        len_addr = self.gen.operation(
            f"getelementptr i64, ptr {self.value}, i64 {VALUE_BUFFER_LENGTH_OFFSET // 8}"
        )
        length = self.gen.operation(f"load i64, ptr {len_addr}")
        data_addr = self.gen.operation(
            f"getelementptr ptr, ptr {self.value}, i64 {VALUE_BUFFER_DATA_OFFSET // 8}"
        )
        data = self.gen.operation(f"load ptr, ptr {data_addr}")
        self.gen.line(f"br label %{loop}")
        self.gen.lines.append(loop + ":")
        idx = self.gen.operation(f"phi i64 [0, %{header}], [%next.{step}, %{step}]")
        at_end = self.gen.operation(f"icmp eq i64 {idx}, {length}")
        self.gen.line(f"br i1 {at_end}, label %{done}, label %{body}")
        self.gen.lines.append(body + ":")
        slot = self.gen.operation(f"getelementptr ptr, ptr {data}, i64 {idx}")
        item = self.gen.operation(f"load ptr, ptr {slot}")
        elem_valid = fact.element.accept(_InferredResultPredicate(self.assertion, item))
        elem_end = self.gen.current_block()
        self.gen.line(f"br i1 {elem_valid}, label %{step}, label %{done}")
        self.gen.lines.append(step + ":")
        self.gen.lines.append(f"  %next.{step} = add i64 {idx}, 1")
        self.gen.line(f"br label %{loop}")
        self.gen.lines.append(done + ":")
        return self.gen.operation(
            f"phi i1 [false, %{start}], [true, %{loop}], [false, %{elem_end}]"
        )

    def visit_DictType(self, fact):
        return self.kind(_ReturnValueKind.DICT)

    def visit_ClassType(self, fact):
        return self.kind(_ReturnValueKind.CLASS)

    def visit_FunctionType(self, fact):
        return self.kind(_ReturnValueKind.FUNCTION)

    def visit_UnionType(self, fact):
        return self.combine(
            "or", (member.accept(self) for member in fact.members), "false"
        )

    def visit_TupleType(self, fact):
        arity = fact.arity
        if arity is None and fact.elements is not None:
            arity = len(fact.elements)
        if arity is None:
            valid = self.kind(_ReturnValueKind.TUPLE)
        else:
            valid = self.gen.operation(
                f"call i1 @mule.fast.tuple.shape(ptr {self.value}, i64 {arity})"
            )
        if not fact.elements or all(element is UNKNOWN for element in fact.elements):
            return valid
        source = self.gen.current_block()
        fields = self.gen.fresh("return.tuple.fields")[1:]
        done = self.gen.fresh("return.tuple.checked")[1:]
        self.gen.line(f"br i1 {valid}, label %{fields}, label %{done}")
        self.gen.lines.append(fields + ":")
        predicates = []
        for index, element in enumerate(fact.elements):
            if element is UNKNOWN:
                continue
            item = self.gen.operation(
                f"call ptr @mule.fast.tuple.borrow(ptr {self.value}, i64 {index})"
            )
            predicates.append(
                element.accept(_InferredResultPredicate(self.assertion, item))
            )
        checked = self.combine("and", predicates, "true")
        last = self.gen.current_block()
        self.gen.line(f"br label %{done}")
        self.gen.lines.append(done + ":")
        return self.gen.operation(f"phi i1 [false, %{source}], [{checked}, %{last}]")


@define_vop("branch", args=(None,))
def branch(gen, vop):
    operand = vop.args[0]
    targets = vop.codegen_info
    value = gen.take(operand)
    truth = gen.truth(operand.tn, value)
    cons_cold = targets.consequent in gen.cold_blocks
    alt_cold = targets.alternative in gen.cold_blocks
    if cons_cold and not alt_cold:
        gen.line(
            f"br i1 {truth}, label %{targets.consequent}, label %{targets.alternative}, {COLD_BRANCH}"
        )
    elif alt_cold and not cons_cold:
        gen.line(
            f"br i1 {truth}, label %{targets.consequent}, label %{targets.alternative}, {HOT_BRANCH}"
        )
    else:
        gen.line(
            f"br i1 {truth}, label %{targets.consequent}, label %{targets.alternative}"
        )


@define_vop("jump")
def jump(gen, vop):
    gen.line(f"br label %{vop.codegen_info}")


@define_vop("return", args=(None,))
def return_(gen, vop):
    gen.return_value(vop.args[0])


@define_vop("local-call")
def local_call(gen, vop):
    gen.numeric_call(vop)


@define_vop("full-call", results=(None,))
def full_call(gen, vop):
    result = vop.results[0]
    site = vop.codegen_info
    function = gen.take(vop.args[0]) if site.callee_required else "null"
    arguments = vop.args[1:] if site.callee_required else vop.args
    values = [gen.take(argument) for argument in arguments]
    value = gen.call(
        site,
        function,
        arguments,
        values,
        unpack_arity=gen.unpacked_call_arity(vop),
        result_contract=gen.call_result_contract(vop),
    )
    gen.publish(result, value)
    if site.callee_required:
        gen.release(function)
    feeds_raise = (
        gen.block is not None
        and len(gen.block.vops) >= 2
        and gen.block.vops[-2] is vop
        and gen.block.vops[-1].info.name == "raise"
        and gen.block.vops[-1].args[0].tn is result.tn
        and not gen.expiring.get(vop)
        and set(gen.live) <= {result.tn}
    )
    if not (
        feeds_raise
        and (gen.module.program.bootstrap or not gen.lambda_.policy.records_traceback)
    ):
        gen.check_pointer(value)
    gen.refine(result.tn, value)


@define_vop("fast-len", args=(OBJECT,), results=(OBJECT,))
def fast_len(gen, vop):
    value = gen.take(vop.args[0])
    result = gen.operation(f"call ptr @mule_fast_len(ptr {value})")
    gen.publish(vop.results[0], result)
    gen.release(value)
    gen.check_pointer(result)
    gen.refine(vop.results[0].tn, result)


@define_vop(
    "parse-int-text",
    translate=fndb.PARSE_INT_TEXT,
    args=(OBJECT,),
    results=(OBJECT,),
    arg_escapes=(False,),
)
def parse_int_text(gen, vop):
    value = gen.take(vop.args[0])
    gen.pointer_result(
        vop.results[0], f"call ptr @mule_parse_int_text(ptr {value})", (value,)
    )


@define_vop(
    "parse-int-slice",
    translate=fndb.PARSE_INT_SLICE,
    args=(OBJECT, OBJECT, OBJECT),
    results=(OBJECT,),
    arg_escapes=(False, False, False),
)
def parse_int_slice(gen, vop):
    sequence, start, stop = (gen.take(operand) for operand in vop.args)
    gen.pointer_result(
        vop.results[0],
        f"call ptr @mule_parse_int_slice(ptr {sequence}, ptr {start}, ptr {stop})",
        (sequence, start, stop),
    )


TERMINATORS = frozenset(
    {"branch", "jump", "return", "raise", "reraise", "resume-exception", "unwind"}
)


def _pack_inline_latin1_words(
    literal: str | None,
) -> tuple[int, tuple[int, ...]] | None:
    if literal is None or len(literal) > 24:
        return None
    try:
        raw = literal.encode("latin-1")
    except UnicodeEncodeError:
        return None
    n = len(raw)
    if n == 0:
        return 0, ()
    padded = raw.ljust(24, b"\x00")
    w0 = int.from_bytes(padded[0:8], "little", signed=True)
    if n <= 8:
        return n, (w0,)
    w1 = int.from_bytes(padded[8:16], "little", signed=True)
    if n <= 16:
        return n, (w0, w1)
    w2 = int.from_bytes(padded[16:24], "little", signed=True)
    return n, (w0, w1, w2)


class _ConstantTextLiteral(ast.Visitor):
    def visit_Node(self, node) -> str | None:
        return None

    def visit_TextLiteral(self, node: ast.TextLiteral) -> str | None:
        return node.value


_CONSTANT_TEXT_LITERAL = _ConstantTextLiteral()


def _constant_text_value(gen, tn) -> str | None:
    writes = gen.component.writes.get(tn, ())
    if len(writes) != 1:
        return None
    writer, _ = writes[0]
    if writer.info is not CONSTANT or writer.codegen_info is None:
        return None
    return writer.codegen_info.accept(_CONSTANT_TEXT_LITERAL)


def _tn_exact_text(gen, tn, value_type) -> str | None:
    exact = value_type.exact_text()
    if exact is not None:
        return exact
    return _constant_text_value(gen, tn)


def _uses_pointer_text_compare(gen, left_tn, left_type, right_tn, right_type) -> bool:
    if (
        _pack_inline_latin1_words(_constant_text_value(gen, left_tn)) is not None
        or _pack_inline_latin1_words(_constant_text_value(gen, right_tn)) is not None
    ):
        return False
    return (
        left_type.singleton_text() is not None
        or right_type.singleton_text() is not None
        or (left_type.is_interned_text and right_type.is_interned_text)
    )


def _uses_inline_latin1_compare(gen, left_tn, left_type, right_tn, right_type) -> bool:
    if not (left_type.fits_text(left_type) and right_type.fits_text(right_type)):
        return False
    if _uses_pointer_text_compare(gen, left_tn, left_type, right_tn, right_type):
        return False
    return (
        _pack_inline_latin1_words(_tn_exact_text(gen, right_tn, right_type)) is not None
        or _pack_inline_latin1_words(_tn_exact_text(gen, left_tn, left_type))
        is not None
    )


def _is_inline_text_compare_only(gen, tn, value: str) -> bool:
    if (
        gen.raw
        or (
            tn.borrowed_from is None
            and tn.sc.owned
            and not (tn.type or UNKNOWN).is_immortal
        )
        or tn not in gen.registers
        or _pack_inline_latin1_words(value) is None
    ):
        return False
    reads = gen.component.reads.get(tn, ())
    if not reads:
        return False
    for reader, _ref in reads:
        if (
            reader.info.name not in ("compare-equal", "compare-not_equal")
            or len(reader.args) != 2
        ):
            return False
        left_tn, right_tn = reader.args[0].tn, reader.args[1].tn
        if left_tn is right_tn:
            return False
        left_type = left_tn.type or UNKNOWN
        right_type = right_tn.type or UNKNOWN
        if not _uses_inline_latin1_compare(
            gen, left_tn, left_type, right_tn, right_type
        ):
            return False
    return True


def _decode_fixnum_operand(gen, operand) -> str:
    if operand.tn.sc is SIGNED:
        return gen.take(operand)
    value = gen.take(operand)
    bits = gen.operation(f"ptrtoint ptr {value} to i64")
    decoded = gen.operation(f"ashr i64 {bits}, 1")
    if operand.tn.type is not None and operand.tn.type.fits(INTEGER):
        return decoded
    is_fixnum = gen.operation(f"and i64 {bits}, 1")
    fixnum_ok = gen.operation(f"icmp ne i64 {is_fixnum}, 0")
    is_true = gen.operation(f"icmp eq ptr {value}, @mule.true")
    bool_val = gen.operation(f"zext i1 {is_true} to i64")
    return gen.operation(f"select i1 {fixnum_ok}, i64 {decoded}, i64 {bool_val}")


# Runtime helper templates shared by the object templates live in
# `mule/procedures/*.ll` (one `.ll` file per procedure).

INTRINSICS = procedures.load_procedure("core/intrinsics.ll")


# Byte offsets of runtime object storage that compiled attribute sites read;
# `mule/runtime/object.rs` defines them and its tests check them.
DATA_OFFSET = 16
TEXT_TAG = 4
TEXT_KIND_TAG_OFFSET = 24
TEXT_INLINE_LEN_OFFSET = 25
TEXT_INLINE_WORDS_OFFSET = 32
TEXT_HEAP_DATA_OFFSET = 40
TEXT_HEAP_LEN_OFFSET = 48
LIST_TAG = 13
ARGS_TAG = 9
# The authoritative repr(C) ValueBuffer header follows the Data discriminator.
# Runtime layout tests check these offsets against both List and Args objects.
VALUE_BUFFER_DATA_OFFSET = 24
VALUE_BUFFER_LENGTH_OFFSET = 32
SEQUENCE_CLASS_OFFSET = 48
INSTANCE_TAG = 16
CLASS_TAG = 15
INSTANCE_CLASS_OFFSET = 24
INSTANCE_FIELDS_OFFSET = 32
INSTANCE_ATTRIBUTES_OFFSET = 48
CLASS_LAYOUT_OFFSET = 24
# `AttrCache` words (`mule/runtime/classes.rs`): layout address, encoding,
# epoch, and the borrowed method a METHOD encoding resolved to.
ATTR_CACHE_WORDS = 29
ATTR_FIELD = 1
ATTR_METHOD = 2
ATTR_KIND_MASK = 7
ATTR_SHIFT = 3


def _ownership() -> str:
    return procedures.load_category("ownership")


def helpers(*, bootstrap: bool = False) -> str:
    """Native guarded templates; LLVM removes unused templates after inlining."""
    return "\n\n".join(text for _, text in procedures.all_procedures())


class LiteralEmitter(ast.Visitor):
    """Each literal chooses its native representation without host type checks."""

    def __init__(self, module):
        self.module = module

    def visit_Node(self, node: ast.Node) -> str:
        raise TypeError("Unsupported LLVM constant")

    def visit_Literal(self, node: ast.Literal) -> str:
        raise TypeError("Unsupported LLVM constant")

    def visit_NoneLiteral(self, node: ast.NoneLiteral) -> str:
        return "getelementptr i8, ptr @mule.none, i64 0"

    def visit_BooleanLiteral(self, node: ast.BooleanLiteral) -> str:
        name = "true" if node.value else "false"
        return f"getelementptr i8, ptr @mule.{name}, i64 0"

    def visit_IntegerLiteral(self, node: ast.IntegerLiteral) -> str:
        value = node.value
        if -(1 << 62) <= value < (1 << 62):
            tagged = ((value << 1) | 1) & ((1 << 64) - 1)
            return f"inttoptr i64 {tagged} to ptr"
        return self.module.constant_bigint(value)

    def visit_FloatLiteral(self, node: ast.FloatLiteral) -> str:
        return self.module.constant_float(node)

    def visit_TextLiteral(self, node: ast.TextLiteral) -> str:
        return self.module.constant_text(node)

    def visit_BytesLiteral(self, node: ast.BytesLiteral) -> str:
        return self.module.constant_bytes(node)

    def visit_ComplexLiteral(self, node: ast.ComplexLiteral) -> str:
        return self.visit_Literal(node)

    def visit_EllipsisLiteral(self, node: ast.EllipsisLiteral) -> str:
        return self.visit_Literal(node)

    def visit_Tuple(self, node: ast.Tuple) -> str:
        return self.module.constant_tuple(node)


class BorrowedLiteralEmitter(LiteralEmitter):
    """Emit borrowed references to unit-owned cached constants without retaining."""

    def visit_IntegerLiteral(self, node: ast.IntegerLiteral) -> str:
        value = node.value
        if -(1 << 62) <= value < (1 << 62):
            tagged = ((value << 1) | 1) & ((1 << 64) - 1)
            return f"inttoptr i64 {tagged} to ptr"
        return self.module.constant_bigint(value, borrowed=True)

    def visit_FloatLiteral(self, node: ast.FloatLiteral) -> str:
        return self.module.constant_float(node, borrowed=True)

    def visit_TextLiteral(self, node: ast.TextLiteral) -> str:
        return self.module.constant_text(node, borrowed=True)

    def visit_BytesLiteral(self, node: ast.BytesLiteral) -> str:
        return self.module.constant_bytes(node, borrowed=True)

    def visit_Tuple(self, node: ast.Tuple) -> str:
        return self.module.constant_tuple(node, borrowed=True)


CONSTANT = constant
