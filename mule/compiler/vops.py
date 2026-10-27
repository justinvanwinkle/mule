"""The LLVM backend's templates: one ``define_vop`` per operation.

This is the counterpart of SBCL's ``src/compiler/<backend>/*.lisp`` files.
Each template names the known function it translates (or is a fixed template
the IR2 converter emits for references, assignments, branches, calls, casts,
and returns), restricts its operands' storage classes, and carries the
generator that writes the LLVM text for one VOP.

Object bodies use the templates over tagged object pointers; the raw
templates (``ltn_policy="raw"``) implement the same operations over machine
integers, booleans, and native instance payloads for bodies whose
representation plan proved every value's machine type. The runtime helper
functions shared by the object templates are emitted once per module from
``helpers()``.
"""

from __future__ import annotations

import struct
from enum import IntEnum

from mule import ast

from . import fndb
from .annotation_types import ContractVisitor
from .ctype import FLOAT, INTEGER, PYTHON_INTEGER, REAL, UNKNOWN, ValueTypeVisitor
from .vmdef import define_vop
from .vop import (
    BOOLEAN,
    BOOLEAN_T,
    BOX_LAYOUTS,
    BOXED_FLOAT,
    BOXED_I64,
    CLASS,
    DESCRIPTOR,
    DOUBLE,
    FIXNUM,
    FLOAT_BOX_LAYOUT,
    IMMEDIATE,
    INSTANCE,
    INT64_BOX_LAYOUT,
    NULL,
    REFERENCE,
    SIGNED,
)

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
    gen.line(f"br i1 {pending}, label %{park}, label %{ready}")
    gen.lines.append(park + ":")
    status = gen.operation("call i32 @mule_safepoint()")
    gen.check_status(status)
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

    def visit_TextLiteral(self, node):
        if _is_inline_text_compare_only(self.gen, self.result.tn, node.value):
            self.gen.native_result(self.result, self.gen.fresh("inline.text"))
            return
        if not self.result.tn.sc.owned or (self.result.tn.type or UNKNOWN).is_immortal:
            self.gen.native_result(
                self.result, self.gen.operation(self.gen.borrowed_constant(node))
            )
            return
        self.visit_Node(node)

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
            gen.line(f"br i1 {missing}, label %{absent}, label %{present}")
            gen.lines.append(absent + ":")
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
    if result.tn.sc is CLASS:
        # A native body's class dependency was loaded and guarded at entry.
        gen.store(result, gen.dependency(vop.codegen_info))
        return
    if result.tn.borrowed_from is not None:
        cell = gen.module.cell(vop.codegen_info.namespace, vop.codegen_info.name)
        value = gen.operation(f"load ptr, ptr {cell}, !nonnull !{{}}")
        gen.publish(result, value)
        return
    gen.pointer_result(
        result, gen.module.load_global(vop.codegen_info, gen.lambda_.policy.safety)
    )


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
    gen.line(f"br i1 {missing}, label %{absent}, label %{present}")
    gen.lines.append(absent + ":")
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
    var = vop.codegen_info
    status = gen.operation(gen.module.store_global(var.namespace, var.name, value))
    gen.release(value)
    gen.check_status(status)


@define_vop("unbind-global")
def unbind_global(gen, vop):
    var, missing_ok = vop.codegen_info
    if missing_ok:
        gen.line(gen.module.delete_global(var, missing_ok=True))
    else:
        status = gen.operation(gen.module.delete_global(var))
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
        self.gen.line(f"br i1 {valid}, label %{accepted}, label %{rejected}")
        self.gen.lines.append(rejected + ":")
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

    def visit_PythonFloatContract(self, contract):
        name = self.gen.module.text(contract.display_name)
        status = self.gen.operation(
            f"call i32 @mule_assert_real(ptr {self.value}, ptr {name})"
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
        gen.line(f"br i1 {fixnum}, label %{done}, label %{non_fixnum}")
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
            self.gen.line(f"br i1 {fixnum}, label %{done_label}, label %{slow_label}")
            self.gen.lines.append(slow_label + ":")
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
        return self.kind(_ReturnValueKind.LIST)

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
    function = gen.take(vop.args[0])
    arguments = vop.args[1:]
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
    gen.release(function)
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


@define_vop("parse-int-text", args=(OBJECT,), results=(OBJECT,))
def parse_int_text(gen, vop):
    value = gen.take(vop.args[0])
    gen.pointer_result(
        vop.results[0], f"call ptr @mule_parse_int_text(ptr {value})", (value,)
    )


@define_vop("parse-int-slice", args=(OBJECT, OBJECT, OBJECT), results=(OBJECT,))
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

# Known functions over objects.


def _binary_generic(gen, vop, opcode, *, inplace):
    result = vop.results[0]
    left, right = vop.args
    lhs, rhs = gen.take(left), gen.take(right)
    left_type = left.tn.type or UNKNOWN
    if opcode == 5 and not inplace and not left_type.fits(REAL):
        value = gen.compiled_call_named("mule_format_modulo", (lhs, rhs))
        gen.publish(result, value)
        gen.check_pointer(value)
        gen.refine(result.tn, value)
        return
    right_type = right.tn.type or UNKNOWN
    fixnums = left_type.is_fixnum and right_type.is_fixnum
    result_type = result.tn.type or UNKNOWN
    if fixnums and result_type.is_fixnum:
        prim_op = _PRIMITIVE_BINARY.get(opcode)
        if (
            prim_op is None
            and opcode in (8, 9)
            and right_type.minimum is not None
            and right_type.maximum is not None
            and 0 <= right_type.minimum
            and right_type.maximum <= 62
        ):
            prim_op = "shl" if opcode == 8 else "ashr"
        if (
            prim_op is None
            and opcode in (4, 5)
            and left_type.minimum is not None
            and left_type.minimum >= 0
            and right_type.minimum is not None
            and right_type.minimum > 0
        ):
            prim_op = "udiv" if opcode == 4 else "urem"
        if prim_op is not None:
            a, b = gen.decode(lhs), gen.decode(rhs)
            integer = gen.operation(f"{prim_op} i64 {a}, {b}")
            gen.native_result(result, gen.encode(integer), (lhs, rhs))
            return
        if opcode in (4, 5) and (
            (right_type.minimum is not None and right_type.minimum > 0)
            or (right_type.maximum is not None and right_type.maximum < 0)
        ):
            a, b = gen.decode(lhs), gen.decode(rhs)
            rem = gen.operation(f"srem i64 {a}, {b}")
            pred = (
                "slt"
                if right_type.minimum is not None and right_type.minimum > 0
                else "sgt"
            )
            adjust = gen.operation(f"icmp {pred} i64 {rem}, 0")
            if opcode == 5:
                adjusted = gen.operation(f"add nsw i64 {rem}, {b}")
                integer = gen.operation(
                    f"select i1 {adjust}, i64 {adjusted}, i64 {rem}"
                )
            else:
                quot = gen.operation(f"sdiv i64 {a}, {b}")
                corr = gen.operation(f"zext i1 {adjust} to i64")
                integer = gen.operation(f"sub nsw i64 {quot}, {corr}")
            gen.native_result(result, gen.encode(integer), (lhs, rhs))
            return
    prefix = "fixnum" if fixnums else "fast"
    reuse = "null"
    reuse_slot = "null"
    info = vop.codegen_info
    if (
        inplace
        and not fixnums
        and opcode in (0, 1, 2)
        and info is not None
        and info.reuse_local is not None
        and gen.local_owned(info.reuse_local)
    ):
        reuse = gen.operation(f"load ptr, ptr {gen.local_slot(info.reuse_local)}")
        reuse_slot = gen.reuse_slot()
    rejected = gen.context.rejected_fixnum_guard
    if rejected is not None and rejected.rejects_operands(gen.component, (left, right)):
        value = _binary_fallback(gen, lhs, rhs, opcode, inplace, reuse, reuse_slot)
        gen.native_result(result, value)
        gen.refine(result.tn, value)
        return
    if opcode in (0, 1):
        _tagged_add_sub(
            gen, result, lhs, rhs, opcode, fixnums, inplace, reuse, reuse_slot
        )
        return
    if opcode in (2, 4, 5, 8, 9, 10, 11, 12):
        operation = (
            f"call ptr @mule.{prefix}.binary.{opcode}(ptr {lhs}, ptr {rhs}, "
            f"i32 {int(inplace)}, ptr {reuse}, ptr {reuse_slot})"
        )
    else:
        operation = (
            f"call ptr @mule_binary_reuse(i32 {opcode}, ptr {lhs}, ptr {rhs}, "
            f"i32 {int(inplace)}, ptr null)"
        )
    gen.pointer_result(result, operation, (lhs, rhs))


def _tagged_add_sub(gen, result, lhs, rhs, opcode, fixnums, inplace, reuse, reuse_slot):
    """Keep infallible tagged arithmetic separate from fallible object dispatch."""
    slow = gen.fresh("arithmetic.slow")[1:]
    fast = gen.fresh("arithmetic.fast")[1:]
    done = gen.fresh("arithmetic.done")[1:]
    left_bits = gen.operation(f"ptrtoint ptr {lhs} to i64")
    right_bits = gen.operation(f"ptrtoint ptr {rhs} to i64")
    if not fixnums:
        tags = gen.operation(f"and i64 {left_bits}, {right_bits}")
        tag = gen.operation(f"and i64 {tags}, 1")
        integers = gen.operation(f"icmp ne i64 {tag}, 0")
        arithmetic = gen.fresh("arithmetic.fixnums")[1:]
        gen.line(f"br i1 {integers}, label %{arithmetic}, label %{slow}")
        gen.lines.append(arithmetic + ":")
    intrinsic = "sadd" if opcode == 0 else "ssub"
    cleared = gen.operation(f"and i64 {left_bits if opcode == 0 else right_bits}, -2")
    left = cleared if opcode == 0 else left_bits
    right = right_bits if opcode == 0 else cleared
    checked = gen.operation(
        f"call {{i64, i1}} @llvm.{intrinsic}.with.overflow.i64(i64 {left}, i64 {right})"
    )
    overflow = gen.operation(f"extractvalue {{i64, i1}} {checked}, 1")
    gen.line(f"br i1 {overflow}, label %{slow}, label %{fast}")
    gen.lines.append(fast + ":")
    bits = gen.operation(f"extractvalue {{i64, i1}} {checked}, 0")
    value = gen.operation(f"inttoptr i64 {bits} to ptr")
    # This edge proves both operands are immediate fixnums, including when
    # their incoming TNs were unknown. Neither operand owns heap storage here.
    gen.line(f"br label %{done}")

    gen.lines.append(slow + ":")
    fallback = _binary_fallback(gen, lhs, rhs, opcode, inplace, reuse, reuse_slot)
    slow_success = gen.current_block()
    gen.line(f"br label %{done}")
    gen.lines.append(done + ":")
    merged = gen.operation(f"phi ptr [{value}, %{fast}], [{fallback}, %{slow_success}]")
    gen.native_result(result, merged)
    gen.refine(result.tn, merged)


def _binary_fallback(gen, lhs, rhs, opcode, inplace, reuse, reuse_slot):
    """Dispatch owned operands and check only this fallible operation's result."""
    if reuse_slot != "null":
        gen.line(f"store ptr {reuse}, ptr {reuse_slot}")
    fallback = gen.operation(
        f"call ptr @mule_binary_reuse(i32 {opcode}, ptr {lhs}, ptr {rhs}, "
        f"i32 {int(inplace)}, ptr {reuse_slot})"
    )
    # The operands were already consumed. Release them on failure too,
    # while leaving unrelated live temporaries to the ordinary failure landing.
    for operand in (lhs, rhs):
        gen.release(operand)
    gen.check_pointer(fallback)
    return fallback


def _define_binary(operator, opcode):
    @define_vop(
        f"binary-{operator.name.lower()}",
        translate=fndb.BINARY[operator],
        args=(OBJECT, OBJECT),
        results=(OBJECT,),
    )
    def generic(gen, vop):
        _binary_generic(gen, vop, opcode, inplace=False)

    @define_vop(
        f"inplace-{operator.name.lower()}",
        translate=fndb.INPLACE_BINARY[operator],
        args=(OBJECT, OBJECT),
        results=(OBJECT,),
    )
    def inplace(gen, vop):
        _binary_generic(gen, vop, opcode, inplace=True)


for _operator, _opcode in BINARY_OPCODES.items():
    _define_binary(_operator, _opcode)


def _fixnum_binary(operator, opcode):
    @define_vop(
        f"fixnum-{operator.name.lower()}",
        translate=fndb.BINARY[operator],
        also_translate=(fndb.INPLACE_BINARY[operator],),
        args=((SIGNED,), (SIGNED,)),
        results=((SIGNED,),),
        arg_types=((FIXNUM,), (FIXNUM,)),
        result_types=((FIXNUM,),),
        cost=-1,
        arg_escapes=(False, False),
    )
    def generator(gen, vop):
        # The result range proof excludes Python integer overflow. No
        # int64 contract or whole-function native representation is needed.
        left, right = (gen.take(operand) for operand in vop.args)
        gen.store(vop.results[0], gen.operation(f"{opcode} i64 {left}, {right}"))


for _operator, _opcode in _RAW_BINARY.items():
    _fixnum_binary(_operator, _opcode)


def _fixnum_compare(operator, predicate):
    @define_vop(
        f"fixnum-compare-{operator.name.lower()}",
        translate=fndb.COMPARE[operator],
        args=((SIGNED,), (SIGNED,)),
        results=(OBJECT,),
        arg_types=((FIXNUM,), (FIXNUM,)),
        cost=-1,
        arg_escapes=(False, False),
    )
    def generator(gen, vop):
        left, right = (gen.take(operand) for operand in vop.args)
        condition = gen.operation(f"icmp {predicate} i64 {left}, {right}")
        gen.native_result(
            vop.results[0],
            gen.operation(f"select i1 {condition}, ptr @mule.true, ptr @mule.false"),
        )


for _operator, _predicate in _RAW_COMPARE.items():
    _fixnum_compare(_operator, _predicate)


def _fixnum_unary(operator, opcode):
    @define_vop(
        f"fixnum-unary-{operator.name.lower()}",
        translate=fndb.UNARY[operator],
        args=((SIGNED,),),
        results=((SIGNED,),),
        arg_types=((FIXNUM,),),
        result_types=((FIXNUM,),),
        cost=-1,
        arg_escapes=(False,),
    )
    def generator(gen, vop):
        value = gen.take(vop.args[0])
        if opcode == 1:
            value = gen.operation(f"sub i64 0, {value}")
        elif opcode == 2:
            value = gen.operation(f"xor i64 {value}, -1")
        gen.store(vop.results[0], value)


for _operator, _opcode in UNARY_OPCODES.items():
    if _opcode < 3:
        _fixnum_unary(_operator, _opcode)


def unbox_float_operand(gen, operand):
    if operand.tn.sc is DOUBLE:
        return gen.load(operand)
    return gen.operation(
        f"call double @{FLOAT_BOX_LAYOUT.symbol('load')}(ptr {gen.load(operand)})"
    )


def _boxed_float_binary(operator, opcode):
    @define_vop(
        f"boxed-float-{operator.name.lower()}",
        translate=fndb.BINARY[operator],
        also_translate=(fndb.INPLACE_BINARY[operator],),
        args=((DESCRIPTOR, DOUBLE), (DESCRIPTOR, DOUBLE)),
        results=((DESCRIPTOR, DOUBLE),),
        arg_types=((BOXED_FLOAT,), (BOXED_FLOAT,)),
        result_types=((BOXED_FLOAT,),),
        cost=-1,
        fresh_result=True,
        reuses_local=True,
        arg_escapes=(False, False),
    )
    def generator(gen, vop):
        # Keep both operand slots owned through a possible division error.
        operands = [unbox_float_operand(gen, operand) for operand in vop.args]
        value = gen.float_arithmetic(opcode, *operands, check_zero=True)
        left, right = (gen.take(operand) for operand in vop.args)
        consumed = [
            value
            for operand, value in zip(vop.args, (left, right))
            if operand.tn.sc is DESCRIPTOR
        ]
        if vop.results[0].tn.sc is DOUBLE:
            gen.store(vop.results[0], value)
            for operand in consumed:
                gen.release(operand)
            return
        info = vop.codegen_info
        if (
            info is not None
            and info.reuse_local is not None
            and gen.local_owned(info.reuse_local)
            and vop.args[0].tn.sc is DESCRIPTOR
        ):
            local = gen.operation(f"load ptr, ptr {gen.local_slot(info.reuse_local)}")
            operation = (
                f"call ptr @mule_box_f64_move(double {value}, ptr {left}, ptr {local})"
            )
            consumed = [right] if vop.args[1].tn.sc is DESCRIPTOR else []
        else:
            operation = f"call ptr @{FLOAT_BOX_LAYOUT.symbol()}(double {value})"
        gen.pointer_result(vop.results[0], operation, consumed)


for _operator, _opcode in _RAW_FLOAT_BINARY.items():
    _boxed_float_binary(_operator, _opcode)


def _mixed_float_binary(operator, opcode, left_float: bool):
    arg_scs = (
        ((DESCRIPTOR, DOUBLE), (IMMEDIATE,))
        if left_float
        else ((IMMEDIATE,), (DESCRIPTOR, DOUBLE))
    )
    arg_ptypes = (
        ((BOXED_FLOAT,), (FIXNUM, BOOLEAN_T))
        if left_float
        else ((FIXNUM, BOOLEAN_T), (BOXED_FLOAT,))
    )
    side = "left" if left_float else "right"

    @define_vop(
        f"mixed-float-{operator.name.lower()}-{side}",
        translate=fndb.BINARY[operator],
        also_translate=(fndb.INPLACE_BINARY[operator],),
        args=arg_scs,
        results=((DESCRIPTOR, DOUBLE),),
        arg_types=arg_ptypes,
        result_types=((BOXED_FLOAT,),),
        cost=-1,
        fresh_result=True,
        arg_escapes=(False, False),
    )
    def generator(gen, vop):
        if left_float:
            left_f64 = unbox_float_operand(gen, vop.args[0])
            right_i64 = _decode_fixnum_operand(gen, vop.args[1])
            right_f64 = gen.operation(f"sitofp i64 {right_i64} to double")
        else:
            left_i64 = _decode_fixnum_operand(gen, vop.args[0])
            left_f64 = gen.operation(f"sitofp i64 {left_i64} to double")
            right_f64 = unbox_float_operand(gen, vop.args[1])
        value = gen.float_arithmetic(opcode, left_f64, right_f64, check_zero=True)
        float_op = vop.args[0] if left_float else vop.args[1]
        taken = gen.take(float_op)
        consumed = [taken] if float_op.tn.sc is DESCRIPTOR else []
        if vop.results[0].tn.sc is DOUBLE:
            gen.store(vop.results[0], value)
            for operand in consumed:
                gen.release(operand)
            return
        operation = f"call ptr @{FLOAT_BOX_LAYOUT.symbol()}(double {value})"
        gen.pointer_result(vop.results[0], operation, consumed)


for _operator, _opcode in _RAW_FLOAT_BINARY.items():
    _mixed_float_binary(_operator, _opcode, True)
    _mixed_float_binary(_operator, _opcode, False)


@define_vop(
    "boxed-float-math-sqrt",
    translate=fndb.MATH_SQRT,
    args=((DESCRIPTOR, DOUBLE),),
    results=((DESCRIPTOR, DOUBLE),),
    arg_types=((BOXED_FLOAT,),),
    result_types=((BOXED_FLOAT,),),
    cost=-1,
    fresh_result=True,
    arg_escapes=(False,),
)
def boxed_float_math_sqrt(gen, vop):
    operand = vop.args[0]
    raw = unbox_float_operand(gen, operand)
    value = gen.float_sqrt(raw)
    taken = gen.take(operand)
    consumed = [taken] if operand.tn.sc is DESCRIPTOR else []
    if vop.results[0].tn.sc is DOUBLE:
        gen.store(vop.results[0], value)
        for item in consumed:
            gen.release(item)
        return
    gen.pointer_result(
        vop.results[0],
        f"call ptr @{FLOAT_BOX_LAYOUT.symbol()}(double {value})",
        consumed,
    )


@define_vop(
    "math-sqrt",
    translate=fndb.MATH_SQRT,
    args=(OBJECT,),
    results=(OBJECT,),
)
def math_sqrt(gen, vop):
    operand = vop.args[0]
    op_type = operand.tn.type or UNKNOWN
    if op_type.is_fixnum:
        raw = gen.decode(gen.take(operand))
        fval = gen.operation(f"sitofp i64 {raw} to double")
        check_domain = op_type.minimum is None or op_type.minimum < 0
        res = gen.float_sqrt(fval, check_domain=check_domain)
        gen.pointer_result(
            vop.results[0],
            f"call ptr @{FLOAT_BOX_LAYOUT.symbol()}(double {res})",
        )
        return
    val = gen.take(operand)
    slot = gen.operation("alloca double")
    status = gen.operation(f"call i32 @mule_unbox_real(ptr {val}, ptr {slot})")
    gen.release(val)
    gen.check_status(status)
    fval = gen.operation(f"load double, ptr {slot}")
    res = gen.float_sqrt(fval)
    gen.pointer_result(
        vop.results[0],
        f"call ptr @{FLOAT_BOX_LAYOUT.symbol()}(double {res})",
    )


def _define_minmax2(name, info, is_max: bool):
    int_pred = "slt" if is_max else "sgt"
    cmp_opcode = (
        COMPARE_OPCODES[ast.ComparisonOperator.GREATER]
        if is_max
        else COMPARE_OPCODES[ast.ComparisonOperator.LESS]
    )

    @define_vop(
        name,
        translate=info,
        args=(OBJECT, OBJECT),
        results=(OBJECT,),
    )
    def generator(gen, vop):
        result = vop.results[0]
        left, right = vop.args
        lhs, rhs = gen.take(left), gen.take(right)
        left_type = left.tn.type or UNKNOWN
        right_type = right.tn.type or UNKNOWN
        if left_type.is_fixnum and right_type.is_fixnum:
            a, b = gen.decode(lhs), gen.decode(rhs)
            pick_right = gen.operation(f"icmp {int_pred} i64 {a}, {b}")
            chosen = gen.operation(f"select i1 {pick_right}, ptr {rhs}, ptr {lhs}")
            gen.native_result(result, chosen, (lhs, rhs))
            return
        left_bits = gen.operation(f"ptrtoint ptr {lhs} to i64")
        right_bits = gen.operation(f"ptrtoint ptr {rhs} to i64")
        tags = gen.operation(f"and i64 {left_bits}, {right_bits}")
        tag = gen.operation(f"and i64 {tags}, 1")
        both_fix = gen.operation(f"icmp ne i64 {tag}, 0")
        fast_lbl, slow_lbl, done_lbl = (
            gen.fresh("minmax.fast")[1:],
            gen.fresh("minmax.slow")[1:],
            gen.fresh("minmax.done")[1:],
        )
        gen.line(f"br i1 {both_fix}, label %{fast_lbl}, label %{slow_lbl}")
        gen.lines.append(fast_lbl + ":")
        fast_pick_right = gen.operation(
            f"icmp {int_pred} i64 {left_bits}, {right_bits}"
        )
        fast_chosen = gen.operation(
            f"select i1 {fast_pick_right}, ptr {rhs}, ptr {lhs}"
        )
        gen.line(f"br label %{done_lbl}")
        gen.lines.append(slow_lbl + ":")
        cmp_obj = gen.operation(
            f"call ptr @mule.fast.compare.{cmp_opcode}(ptr {rhs}, ptr {lhs})"
        )
        if not cmp_obj.startswith("@"):
            bad = gen.operation(f"icmp eq ptr {cmp_obj}, null")
            fail_lbl, ok_lbl = (
                gen.fresh("minmax.fail")[1:],
                gen.fresh("minmax.ok")[1:],
            )
            gen.line(f"br i1 {bad}, label %{fail_lbl}, label %{ok_lbl}")
            gen.lines.append(fail_lbl + ":")
            gen.release(lhs)
            gen.release(rhs)
            gen.line(f"br label %{gen.failure_target('error')}")
            gen.lines.append(ok_lbl + ":")
        truth = gen.operation(f"call i32 @mule.fast.truth(ptr {cmp_obj})")
        gen.release(cmp_obj)
        bad_truth = gen.operation(f"icmp slt i32 {truth}, 0")
        tfail_lbl, tok_lbl = (
            gen.fresh("minmax.tfail")[1:],
            gen.fresh("minmax.tok")[1:],
        )
        gen.line(f"br i1 {bad_truth}, label %{tfail_lbl}, label %{tok_lbl}")
        gen.lines.append(tfail_lbl + ":")
        gen.release(lhs)
        gen.release(rhs)
        gen.line(f"br label %{gen.failure_target('error')}")
        gen.lines.append(tok_lbl + ":")
        pick_right = gen.operation(f"icmp ne i32 {truth}, 0")
        slow_chosen = gen.operation(f"select i1 {pick_right}, ptr {rhs}, ptr {lhs}")
        discarded = gen.operation(f"select i1 {pick_right}, ptr {lhs}, ptr {rhs}")
        gen.release(discarded)
        slow_end = gen.current_block()
        gen.line(f"br label %{done_lbl}")
        gen.lines.append(done_lbl + ":")
        merged = gen.operation(
            f"phi ptr [{fast_chosen}, %{fast_lbl}], [{slow_chosen}, %{slow_end}]"
        )
        gen.publish(result, merged)
        gen.refine(result.tn, merged)


_define_minmax2("min2", fndb.MIN2, False)
_define_minmax2("max2", fndb.MAX2, True)


def _boxed_float_unary(operator, negate: bool):
    @define_vop(
        f"boxed-float-{operator.name.lower()}",
        translate=fndb.UNARY[operator],
        args=((DESCRIPTOR, DOUBLE),) if negate else (None,),
        results=((DESCRIPTOR, DOUBLE),) if negate else (None,),
        arg_types=((BOXED_FLOAT,),),
        result_types=((BOXED_FLOAT,),),
        cost=-1,
        fresh_result=negate,
        arg_escapes=(not negate,),
    )
    def generator(gen, vop):
        operand = vop.args[0]
        if not negate:
            result = vop.results[0]
            if operand.tn.sc is not result.tn.sc:
                raise ValueError("float identity coercion requires matching storage")
            taken = gen.take(operand)
            if operand.tn.sc is DOUBLE:
                gen.store(result, taken)
                return
            class_slot = gen.operation(
                f"getelementptr i8, ptr {taken}, i64 {FLOAT_BOX_LAYOUT.null_pointer_offsets[0]}"
            )
            class_value = gen.operation(f"load ptr, ptr {class_slot}")
            exact = gen.operation(f"icmp eq ptr {class_value}, null")
            forward, convert, joined = (
                gen.fresh("positive.identity")[1:],
                gen.fresh("positive.convert")[1:],
                gen.fresh("positive.done")[1:],
            )
            gen.line(f"br i1 {exact}, label %{forward}, label %{convert}")
            gen.lines.append(forward + ":")
            gen.line(f"br label %{joined}")
            gen.lines.append(convert + ":")
            raw = gen.operation(
                f"call double @{FLOAT_BOX_LAYOUT.symbol('load')}(ptr {taken})"
            )
            boxed = gen.operation(
                f"call ptr @{FLOAT_BOX_LAYOUT.symbol()}(double {raw})"
            )
            gen.line(f"br label %{joined}")
            gen.lines.append(joined + ":")
            value = gen.operation(
                f"phi ptr [{taken}, %{forward}], [{boxed}, %{convert}]"
            )
            consumed = gen.operation(f"select i1 {exact}, ptr null, ptr {taken}")
            gen.store(result, value)
            gen.release(consumed)
            return
        raw = unbox_float_operand(gen, operand)
        value = gen.operation(f"fneg double {raw}")
        taken = gen.take(operand)
        consumed = [taken] if operand.tn.sc is DESCRIPTOR else []
        if vop.results[0].tn.sc is DOUBLE:
            gen.store(vop.results[0], value)
            for item in consumed:
                gen.release(item)
            return
        gen.pointer_result(
            vop.results[0],
            f"call ptr @{FLOAT_BOX_LAYOUT.symbol()}(double {value})",
            consumed,
        )


_boxed_float_unary(ast.UnaryOperator.POSITIVE, False)
_boxed_float_unary(ast.UnaryOperator.NEGATIVE, True)


def _boxed_float_compare(operator, predicate):
    @define_vop(
        f"boxed-float-compare-{operator.name.lower()}",
        translate=fndb.COMPARE[operator],
        args=((DESCRIPTOR, DOUBLE), (DESCRIPTOR, DOUBLE)),
        results=(OBJECT,),
        arg_types=((BOXED_FLOAT,), (BOXED_FLOAT,)),
        cost=-1,
        arg_escapes=(False, False),
    )
    def generator(gen, vop):
        operands = [unbox_float_operand(gen, operand) for operand in vop.args]
        condition = gen.operation(
            f"fcmp {predicate} double {operands[0]}, {operands[1]}"
        )
        boolean = gen.operation(
            f"select i1 {condition}, ptr @mule.true, ptr @mule.false"
        )
        left, right = (gen.take(operand) for operand in vop.args)
        consumed = [
            val
            for operand, val in zip(vop.args, (left, right))
            if operand.tn.sc is DESCRIPTOR
        ]
        gen.native_result(vop.results[0], boolean, consumed)


for _operator, _predicate in _RAW_FLOAT_COMPARE.items():
    _boxed_float_compare(_operator, _predicate)


def _define_unary(operator, opcode):
    @define_vop(
        f"unary-{operator.name.lower()}",
        translate=fndb.UNARY[operator],
        args=(OBJECT,),
        results=(OBJECT,),
    )
    def generic(gen, vop):
        result = vop.results[0]
        operand = vop.args[0]
        value = gen.take(operand)
        operand_type = operand.tn.type or UNKNOWN
        fixnum = operand_type.is_fixnum
        if fixnum and (result.tn.type or UNKNOWN).is_fixnum and opcode < 3:
            decoded = gen.decode(value)
            integer = (
                decoded
                if opcode == 0
                else gen.operation(
                    f"sub i64 0, {decoded}" if opcode == 1 else f"xor i64 {decoded}, -1"
                )
            )
            gen.native_result(result, gen.encode(integer), (value,))
            return
        if fixnum and opcode == 3:
            bits = gen.operation(f"ptrtoint ptr {value} to i64")
            zero = gen.operation(f"icmp eq i64 {bits}, 1")
            boolean = gen.operation(
                f"select i1 {zero}, ptr @mule.true, ptr @mule.false"
            )
            gen.native_result(result, boolean, (value,))
            return
        if operand.tn.primitive_type is BOOLEAN_T and opcode == 3:
            is_false = gen.operation(f"icmp eq ptr {value}, @mule.false")
            boolean = gen.operation(
                f"select i1 {is_false}, ptr @mule.true, ptr @mule.false"
            )
            gen.native_result(result, boolean, (value,))
            return
        if opcode < 3 and not operand_type.fits(REAL):
            tagged = gen.operation(f"ptrtoint ptr {value} to i64")
            tag = gen.operation(f"and i64 {tagged}, 1")
            integer = gen.operation(f"icmp ne i64 {tag}, 0")
            fast, protocol, joined = (
                gen.fresh(name)[1:]
                for name in ("unary.integer", "unary.protocol", "unary.join")
            )
            gen.line(f"br i1 {integer}, label %{fast}, label %{protocol}")
            gen.lines.append(fast + ":")
            scalar = gen.operation(f"call ptr @mule.fixnum.unary.{opcode}(ptr {value})")
            gen.line(f"br label %{joined}")
            gen.lines.append(protocol + ":")
            name = ("positive", "negative", "invert")[opcode]
            dynamic = gen.sequence_call(name, [value])
            protocol_end = gen.current_block()
            gen.line(f"br label %{joined}")
            gen.lines.append(joined + ":")
            gen.pointer_result(
                result, f"phi ptr [{scalar}, %{fast}], [{dynamic}, %{protocol_end}]"
            )
            return
        prefix = "fixnum" if fixnum and opcode < 3 else "fast"
        gen.pointer_result(
            result, f"call ptr @mule.{prefix}.unary.{opcode}(ptr {value})", (value,)
        )


for _operator, _opcode in UNARY_OPCODES.items():
    _define_unary(_operator, _opcode)


def _pack_inline_latin1_words(
    literal: str | None,
) -> tuple[int, tuple[int, ...]] | None:
    if literal is None or not (2 <= len(literal) <= 24):
        return None
    try:
        raw = literal.encode("latin-1")
    except UnicodeEncodeError:
        return None
    n = len(raw)
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


def _uses_inline_latin1_compare(gen, left_tn, left_type, right_tn, right_type) -> bool:
    if not (left_type.fits_text(left_type) and right_type.fits_text(right_type)):
        return False
    if (
        left_type.singleton_text() is not None
        or right_type.singleton_text() is not None
        or (left_type.is_interned_text and right_type.is_interned_text)
    ):
        return False
    return (
        _pack_inline_latin1_words(_tn_exact_text(gen, right_tn, right_type)) is not None
        or _pack_inline_latin1_words(_tn_exact_text(gen, left_tn, left_type))
        is not None
    )


def _is_inline_text_compare_only(gen, tn, value: str) -> bool:
    if (
        gen.raw
        or tn.borrowed_from is None
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


def _emit_inline_latin1_compare(
    gen, value: str, length: int, words: tuple[int, ...], opcode: int
) -> str:
    header_ptr = gen.operation(
        f"getelementptr inbounds i8, ptr {value}, i64 {TEXT_KIND_TAG_OFFSET}"
    )
    header = gen.operation(f"load i16, ptr {header_ptr}, align 8")
    expected_header = length << 8
    matched = gen.operation(f"icmp eq i16 {header}, {expected_header}")
    for index, word in enumerate(words):
        word_offset = TEXT_INLINE_WORDS_OFFSET + index * 8
        word_ptr = gen.operation(
            f"getelementptr inbounds i8, ptr {value}, i64 {word_offset}"
        )
        loaded_word = gen.operation(f"load i64, ptr {word_ptr}, align 8")
        word_match = gen.operation(f"icmp eq i64 {loaded_word}, {word}")
        matched = gen.operation(f"and i1 {matched}, {word_match}")
    if opcode == 1:
        matched = gen.operation(f"xor i1 {matched}, true")
    return gen.operation(f"select i1 {matched}, ptr @mule.true, ptr @mule.false")


def _define_compare(operator, opcode):
    @define_vop(
        f"compare-{operator.name.lower()}",
        translate=fndb.COMPARE[operator],
        args=(OBJECT, OBJECT),
        results=(OBJECT,),
        arg_escapes=(False, False) if opcode in (6, 7) else None,
    )
    def generic(gen, vop):
        result = vop.results[0]
        left, right = vop.args
        lhs, rhs = gen.take(left), gen.take(right)
        if opcode in (8, 9):
            is_dict = (right.tn.type or UNKNOWN).fits_dict()
            if is_dict:
                name = "dict_contains" if opcode == 8 else "dict_not_contains"
            else:
                name = "contains" if opcode == 8 else "not_contains"
            value = gen.sequence_call(name, [rhs, lhs])
            gen.publish(result, value)
            if is_dict:
                gen.release(lhs)
                gen.release(rhs)
            gen.check_pointer(value)
            gen.refine(result.tn, value)
            return
        if opcode in (6, 7):
            predicate = "eq" if opcode == 6 else "ne"
            condition = gen.operation(f"icmp {predicate} ptr {lhs}, {rhs}")
            boolean = gen.operation(
                f"select i1 {condition}, ptr @mule.true, ptr @mule.false"
            )
            gen.native_result(result, boolean, (lhs, rhs))
            return
        left_type = left.tn.type or UNKNOWN
        right_type = right.tn.type or UNKNOWN
        if opcode < 6 and left_type.is_fixnum and right_type.is_fixnum:
            a, b = gen.decode(lhs), gen.decode(rhs)
            predicate = ("eq", "ne", "slt", "sle", "sgt", "sge")[opcode]
            condition = gen.operation(f"icmp {predicate} i64 {a}, {b}")
            boolean = gen.operation(
                f"select i1 {condition}, ptr @mule.true, ptr @mule.false"
            )
            gen.native_result(result, boolean, (lhs, rhs))
            return
        if (
            opcode < 6
            and left_type.fits_text(left_type)
            and right_type.fits_text(right_type)
        ):
            if opcode in (0, 1) and (
                left_type.singleton_text() is not None
                or right_type.singleton_text() is not None
                or (left_type.is_interned_text and right_type.is_interned_text)
            ):
                predicate = "eq" if opcode == 0 else "ne"
                condition = gen.operation(f"icmp {predicate} ptr {lhs}, {rhs}")
                boolean = gen.operation(
                    f"select i1 {condition}, ptr @mule.true, ptr @mule.false"
                )
            elif opcode in (0, 1):
                right_inline = _pack_inline_latin1_words(
                    _tn_exact_text(gen, right.tn, right_type)
                )
                left_inline = (
                    None
                    if right_inline is not None
                    else _pack_inline_latin1_words(
                        _tn_exact_text(gen, left.tn, left_type)
                    )
                )
                if right_inline is not None:
                    boolean = _emit_inline_latin1_compare(
                        gen, lhs, right_inline[0], right_inline[1], opcode
                    )
                elif left_inline is not None:
                    boolean = _emit_inline_latin1_compare(
                        gen, rhs, left_inline[0], left_inline[1], opcode
                    )
                else:
                    matched = gen.operation(
                        f"call i32 @mule_text_eq(ptr {lhs}, ptr {rhs})"
                    )
                    predicate = "ne" if opcode == 0 else "eq"
                    condition = gen.operation(f"icmp {predicate} i32 {matched}, 0")
                    boolean = gen.operation(
                        f"select i1 {condition}, ptr @mule.true, ptr @mule.false"
                    )
            else:
                boolean = gen.operation(
                    f"call ptr @mule_compare(i32 {opcode}, ptr {lhs}, ptr {rhs})"
                )
            gen.native_result(result, boolean, (lhs, rhs))
            return
        if vop.codegen_info is True and opcode < 6:
            operation = (
                f"call ptr @mule.fast.compare.bool.{opcode}(ptr {lhs}, ptr {rhs})"
            )
            gen.pointer_result(result, operation, (lhs, rhs))
            return
        operation = (
            f"call ptr @mule.fast.compare.{opcode}(ptr {lhs}, ptr {rhs})"
            if opcode < 8
            else f"call ptr @mule_compare(i32 {opcode}, ptr {lhs}, ptr {rhs})"
        )
        gen.pointer_result(result, operation, (lhs, rhs))


for _operator, _opcode in COMPARE_OPCODES.items():
    _define_compare(_operator, _opcode)


_TABLE_SLOTS = {
    "_indices": 0,
    "_hashes": 1,
    "_keys": 2,
    "_values": 3,
    "_used": 4,
}


@define_vop("getattr", translate=fndb.GETATTR, args=(OBJECT,), results=(OBJECT,))
def getattr_(gen, vop):
    receiver = gen.take(vop.args[0])
    result = vop.results[0]
    name = vop.codegen_info.name
    value = _read_attribute(gen, receiver, name, result.tn)
    gen.publish(result, value)
    gen.release(receiver)
    if not (
        gen.lambda_.is_protocol
        and name in _TABLE_SLOTS
        and result.tn.borrowed_from is not None
    ):
        gen.check_pointer(value)
    gen.refine(result.tn, value)


def _read_attribute(gen, receiver, name, result_tn=None):
    """An owned field value, with guarded physical access and dynamic lookup."""
    slot_index = _TABLE_SLOTS.get(name) if gen.lambda_.is_protocol else None
    if slot_index is not None:
        fields_addr = gen.operation(
            f"getelementptr i8, ptr {receiver}, i64 {INSTANCE_FIELDS_OFFSET}"
        )
        fields = gen.operation(f"load ptr, ptr {fields_addr}")
        slot = (
            fields
            if slot_index == 0
            else gen.operation(f"getelementptr ptr, ptr {fields}, i64 {slot_index}")
        )
        field = gen.operation(f"load ptr, ptr {slot}")
        if result_tn is not None and result_tn.borrowed_from is not None:
            return field
        unset = gen.operation(f"icmp eq ptr {field}, null")
        loaded = gen.fresh("attr.loaded")[1:]
        slow_label = gen.fresh("attr.slow")[1:]
        join = gen.fresh("attr.join")[1:]
        gen.line(f"br i1 {unset}, label %{slow_label}, label %{loaded}")
        gen.lines.append(f"{loaded}:")
        result_type = (result_tn.type if result_tn is not None else None) or UNKNOWN
        if not result_type.is_fixnum and not result_type.is_immortal:
            retain = "heap.retain" if result_type.is_heap_object else "fast.retain"
            gen.line(f"call ptr @mule.{retain}(ptr {field})")
        gen.line(f"br label %{join}")
        gen.lines.append(f"{slow_label}:")
        slow = gen.operation(
            f"call ptr @mule_get_attr_cached(ptr {receiver}, ptr {gen.module.text(name)}, ptr null)"
        )
        gen.line(f"br label %{join}")
        gen.lines.append(f"{join}:")
        return gen.operation(f"phi ptr [{field}, %{loaded}], [{slow}, %{slow_label}]")
    cache = gen.module.attribute_cache()
    labels = _AttributeSite(gen, receiver, cache, ATTR_FIELD)
    slot = gen.operation(
        f"call ptr @mule.attr.field(ptr {receiver}, i64 {labels.encoded})"
    )
    field = gen.operation(f"load ptr, ptr {slot}")
    unset = gen.operation(f"icmp eq ptr {field}, null")
    loaded = gen.fresh("attr.loaded")[1:]
    gen.line(f"br i1 {unset}, label %{labels.slow}, label %{loaded}")
    gen.lines.append(f"{loaded}:")
    gen.line(f"call ptr @mule.fast.retain(ptr {field})")
    gen.line(f"br label %{labels.join}")
    gen.lines.append(f"{labels.slow}:")
    slow = gen.operation(
        f"call ptr @mule_get_attr_cached(ptr {receiver}, ptr {gen.module.text(name)}, "
        f"ptr {cache})"
    )
    gen.line(f"br label %{labels.join}")
    gen.lines.append(f"{labels.join}:")
    return gen.operation(f"phi ptr [{field}, %{loaded}], [{slow}, %{labels.slow}]")


class _AttributeSite:
    """The inline cache probe opening one attribute site: falls through to
    the hit block when the site's cache holds an encoding of ``kind`` for
    the receiver's class, else branches to ``slow``. ``join`` is the label
    the caller merges both paths at."""

    def __init__(self, gen, receiver: str, cache: str, kind: int):
        self.encoded = gen.operation(
            f"call i64 @mule.attr.hit(ptr {receiver}, ptr {cache})"
        )
        found = gen.operation(f"and i64 {self.encoded}, {ATTR_KIND_MASK}")
        matched = gen.operation(f"icmp eq i64 {found}, {kind}")
        hit = gen.fresh("attr.hit")[1:]
        self.slow = gen.fresh("attr.slow")[1:]
        self.join = gen.fresh("attr.join")[1:]
        gen.line(f"br i1 {matched}, label %{hit}, label %{self.slow}")
        gen.lines.append(f"{hit}:")


@define_vop(
    "getattr-default",
    translate=fndb.GETATTR_DEFAULT,
    args=(OBJECT, OBJECT),
    results=(OBJECT,),
)
def getattr_default(gen, vop):
    """``getattr(x, "name", default)``: a per-site cache that also records
    misses, so a repeated miss returns the default after one class check."""
    receiver, default = gen.take(vop.args[0]), gen.take(vop.args[1])
    info = vop.codegen_info
    gen.pointer_result(
        vop.results[0],
        f"call ptr @mule_getattr_default(ptr {receiver}, ptr {gen.module.text(info.name)}, "
        f"ptr {default}, ptr {gen.module.attribute_cache()})",
        (receiver, default),
    )


@define_vop("hasattr", translate=fndb.HASATTR, args=(OBJECT,), results=(OBJECT,))
def hasattr_(gen, vop):
    receiver = gen.take(vop.args[0])
    info = vop.codegen_info
    gen.pointer_result(
        vop.results[0],
        f"call ptr @mule_hasattr(ptr {receiver}, ptr {gen.module.text(info.name)}, "
        f"ptr {gen.module.attribute_cache()})",
        (receiver,),
    )


@define_vop("setattr", translate=fndb.SETATTR, args=(OBJECT, OBJECT))
def setattr_(gen, vop):
    receiver, value = gen.take(vop.args[0]), gen.take(vop.args[1])
    move_value = gen.owns_reference(value) and value != receiver
    status = _write_attribute(
        gen,
        receiver,
        vop.codegen_info.name,
        value,
        move_value=move_value,
        value_tn=vop.args[1].tn,
    )
    gen.release(receiver)
    if not move_value:
        gen.release(value)
    gen.check_status(status)


def _write_attribute(
    gen, receiver, name, value, *, move_value: bool = False, value_tn=None
):
    """Assign a borrowed or moved value, completing old-value destruction before return."""
    slot_index = _TABLE_SLOTS.get(name) if gen.lambda_.is_protocol else None
    if slot_index is not None:
        fields_addr = gen.operation(
            f"getelementptr i8, ptr {receiver}, i64 {INSTANCE_FIELDS_OFFSET}"
        )
        fields = gen.operation(f"load ptr, ptr {fields_addr}")
        slot = (
            fields
            if slot_index == 0
            else gen.operation(f"getelementptr ptr, ptr {fields}, i64 {slot_index}")
        )
        val_type = (value_tn.type if value_tn is not None else None) or UNKNOWN
        if name == "_used" and val_type.is_fixnum:
            gen.line(f"store ptr {value}, ptr {slot}")
            return "0"
        old = gen.operation(f"load ptr, ptr {slot}")
        if not move_value and not val_type.is_fixnum and not val_type.is_immortal:
            retain = "heap.retain" if val_type.is_heap_object else "fast.retain"
            gen.line(f"call ptr @mule.{retain}(ptr {value})")
        gen.line(f"store ptr {value}, ptr {slot}")
        gen.line(f"call void @mule.fast.release(ptr {old})")
        return "0"
    cache = gen.module.attribute_cache()
    site = _AttributeSite(gen, receiver, cache, ATTR_FIELD)
    slot = gen.operation(
        f"call ptr @mule.attr.field(ptr {receiver}, i64 {site.encoded})"
    )
    old = gen.operation(f"load ptr, ptr {slot}")
    if not move_value:
        gen.line(f"call ptr @mule.fast.retain(ptr {value})")
    gen.line(f"store ptr {value}, ptr {slot}")
    gen.line(f"call void @mule.fast.release(ptr {old})")
    stored = gen.current_block()
    gen.line(f"br label %{site.join}")
    gen.lines.append(f"{site.slow}:")
    slow = gen.operation(
        f"call i32 @mule_set_attr_cached(ptr {receiver}, ptr {gen.module.text(name)}, "
        f"ptr {value}, ptr {cache})"
    )
    if move_value:
        gen.release(value)
    gen.line(f"br label %{site.join}")
    gen.lines.append(f"{site.join}:")
    return gen.operation(f"phi i32 [0, %{stored}], [{slow}, %{site.slow}]")


@define_vop("value-cell-ref", args=(OBJECT,), results=(OBJECT,), arg_escapes=(False,))
def value_cell_ref(gen, vop):
    """SBCL's value-cell-ref, guarded for Python's mutable class attributes."""
    cell = gen.load(vop.args[0])
    bound = _read_attribute(gen, cell, "bound")
    gen.check_pointer(bound)
    truth = gen.operation(f"call i32 @mule.fast.truth(ptr {bound})")
    gen.release(bound)
    gen.check_status(truth)
    present = gen.operation(f"icmp ne i32 {truth}, 0")
    loaded = gen.fresh("cell.bound")[1:]
    missing = gen.fresh("cell.unbound")[1:]
    join = gen.fresh("cell.join")[1:]
    gen.line(f"br i1 {present}, label %{loaded}, label %{missing}")
    gen.lines.append(loaded + ":")
    value = _read_attribute(gen, cell, "value")
    value_block = gen.current_block()
    gen.line(f"br label %{join}")
    gen.lines.append(missing + ":")
    optional = "@mule.true" if vop.codegen_info else "@mule.false"
    absent = gen.sequence_call("cell_unbound", [optional])
    absent_block = gen.current_block()
    gen.line(f"br label %{join}")
    gen.lines.append(join + ":")
    gen.pointer_result(
        vop.results[0],
        f"phi ptr [{value}, %{value_block}], [{absent}, %{absent_block}]",
    )


@define_vop("value-cell-set", args=(OBJECT, OBJECT), arg_escapes=(False, True))
def value_cell_set(gen, vop):
    cell = gen.load(vop.args[0])
    value = gen.take(vop.args[1])
    status = _write_attribute(gen, cell, "value", value)
    gen.release(value)
    gen.check_status(status)
    # Recheck the attribute epoch after old-value destruction: it can run Python.
    status = _write_attribute(gen, cell, "bound", "@mule.true")
    gen.check_status(status)


@define_vop("value-cell-clear", args=(OBJECT,), arg_escapes=(False,))
def value_cell_clear(gen, vop):
    cell = gen.load(vop.args[0])
    status = _write_attribute(gen, cell, "value", "@mule.none")
    gen.check_status(status)
    status = _write_attribute(gen, cell, "bound", "@mule.false")
    gen.check_status(status)


@define_vop("isinstance", translate=fndb.ISINSTANCE, results=(OBJECT,))
def isinstance_(gen, vop):
    """Single-class or multi-class ``isinstance`` without a tuple. A single
    class checks exact instance identity inline before calling the runtime;
    multiple classes are passed in a stack buffer."""
    value, *classes = (gen.take(arg) for arg in vop.args)
    if len(classes) == 1:
        gen.pointer_result(
            vop.results[0],
            f"call ptr @mule.fast.isinstance.1(ptr {value}, ptr {classes[0]})",
            (value, classes[0]),
        )
        return
    gen.argv(classes)
    gen.pointer_result(
        vop.results[0],
        f"call ptr @mule_isinstance_any(ptr {value}, ptr %argv, i64 {len(classes)})",
        (value, *classes),
    )


@define_vop(
    "resolve-method",
    translate=fndb.RESOLVE_METHOD,
    args=(OBJECT,),
    results=(OBJECT, OBJECT),
)
def resolve_method(gen, vop):
    receiver = gen.take(vop.args[0])
    method, bound = vop.results
    info = vop.codegen_info
    if info.is_exact:
        gen.immortal_values.add("@mule.none")
        gen.publish(method, "@mule.none")
        gen.publish(bound, receiver)
        return
    cache = gen.module.attribute_cache()
    site = _AttributeSite(gen, receiver, cache, ATTR_METHOD)
    # A hit binds the receiver without allocating: the cached function is
    # borrowed from the class and ``bound`` takes its own receiver reference,
    # as the runtime's resolution does; the operand itself may be borrowed.
    cached_address = gen.operation(f"getelementptr i64, ptr {cache}, i64 3")
    cached = gen.operation(f"load ptr, ptr {cached_address}")
    gen.line(f"call ptr @mule.heap.retain(ptr {cached})")
    gen.line(f"call ptr @mule.heap.retain(ptr {receiver})")
    gen.line(f"br label %{site.join}")
    hit = gen.current_block()
    gen.lines.append(f"{site.slow}:")
    out = gen.out_pointer()
    resolved = gen.operation(
        f"call ptr @mule_resolve_method(ptr {receiver}, ptr {gen.module.text(info.name)}, "
        f"ptr {cache}, ptr {out})"
    )
    resolved_bound = gen.operation(f"load ptr, ptr {out}")
    gen.line(f"br label %{site.join}")
    gen.lines.append(f"{site.join}:")
    value = gen.operation(f"phi ptr [{cached}, %{hit}], [{resolved}, %{site.slow}]")
    receiver_bound = gen.operation(
        f"phi ptr [{receiver}, %{hit}], [{resolved_bound}, %{site.slow}]"
    )
    gen.publish(method, value)
    gen.publish(bound, receiver_bound)
    gen.release(receiver)
    gen.check_pointer(value)
    gen.refine(method.tn, value)


@define_vop(
    "resolve-super-method",
    translate=fndb.RESOLVE_SUPER_METHOD,
    args=(OBJECT, OBJECT, OBJECT),
    results=(OBJECT, OBJECT),
)
def resolve_super_method(gen, vop):
    super_callee = gen.take(vop.args[0])
    thisclass = gen.take(vop.args[1])
    receiver = gen.take(vop.args[2])
    method, bound = vop.results
    info = vop.codegen_info
    out = gen.out_pointer()
    value = gen.operation(
        f"call ptr @mule_resolve_super_method(ptr {super_callee}, ptr {thisclass}, "
        f"ptr {receiver}, ptr {gen.module.text(info.name)}, "
        f"ptr {gen.module.attribute_cache()}, ptr {out})"
    )
    gen.publish(method, value)
    gen.publish(bound, gen.operation(f"load ptr, ptr {out}"))
    gen.release(super_callee)
    gen.release(thisclass)
    gen.release(receiver)
    gen.check_pointer(value)
    gen.refine(method.tn, value)


@define_vop("call-resolved", translate=fndb.CALL_RESOLVED, results=(OBJECT,))
def call_resolved(gen, vop):
    method, bound, *arguments = vop.args
    function = gen.take(method)
    receiver = gen.take(bound)
    values = [gen.take(argument) for argument in arguments]
    value = gen.call_method(
        vop.codegen_info,
        function,
        receiver,
        values,
        unpack_arity=gen.unpacked_call_arity(vop),
        argument_refs=arguments,
        result_contract=gen.call_result_contract(vop),
    )
    gen.publish(vop.results[0], value)
    gen.release(function)
    gen.check_pointer(value)
    gen.refine(vop.results[0].tn, value)


@define_vop(
    "make-function", translate=fndb.MAKE_FUNCTION, results=(OBJECT, OBJECT, OBJECT)
)
def make_function(gen, vop):
    result = vop.results[0]
    definition = vop.codegen_info
    target = definition.lambda_
    info = definition.info
    module = gen.module
    closure_count = len(target.environment.closure) if target.environment else 0
    defaults_refs = vop.args[: len(vop.args) - closure_count]
    closure_refs = vop.args[len(vop.args) - closure_count :]
    native = module.functions[target]
    name = module.text(info.public_name)
    defaults_slot, closure_slot = vop.results[1:]
    defaults = gen.tuple_of(defaults_slot, defaults_refs)
    closure = gen.tuple_of(closure_slot, closure_refs)
    if defaults is not None:
        defaults = gen.take(defaults_slot)
    if closure is not None:
        closure = gen.take(closure_slot)
    if defaults is None and target.binding is not None:
        defaults = "null"
    consumed = [value for value in (defaults, closure) if value not in (None, "null")]
    # A function taking keywords gets its keyword entry from
    # ``mule_function_defaults``, even without defaults ("null").
    if defaults is not None:
        call = (
            f"call ptr @mule_function_defaults(ptr %globals, ptr {native}, ptr {name}, "
            f"ptr {defaults})"
        )
    else:
        call = f"call ptr @mule_function(ptr %globals, ptr {native}, ptr {name})"
    if closure is not None:
        # Assigned captured variables live in cells; the others are captured
        # by value. ``__closure__`` presents every entry as a cell.
        cells = f"@mule.closure.cells.{len(module.auxiliary)}"
        flags = ", ".join(
            f"i8 {int(var.indirect)}" for var in target.environment.closure
        )
        module.auxiliary.append(
            f"{cells} = private constant [{closure_count} x i8] [{flags}], align 1"
        )
        function = gen.operation(call)
        call = f"call ptr @mule_function_closure(ptr {function}, ptr {closure}, ptr {cells})"
    gen.pointer_result(result, call, consumed)

    metadata = gen.operation(gen.constant(module.code_metadata(target)))
    gen.check_pointer(metadata)
    function = gen.load(result)
    gen.line(
        f"call void @mule_set_function_metadata(ptr {function}, ptr {module.code_descriptor(target)}, ptr {metadata})"
    )
    gen.release(metadata)
    if info.docstring is not None:
        docstring = gen.operation(gen.constant(info.docstring))
        gen.check_pointer(docstring)
        status = gen.operation(
            f"call i32 @mule_set_attr_cached(ptr {function}, "
            f"ptr {module.text('__doc__')}, ptr {docstring}, ptr null)"
        )
        gen.release(docstring)
        gen.check_status(status)


@define_vop("make-class", translate=fndb.MAKE_CLASS, results=(OBJECT,))
def make_class(gen, vop):
    info = vop.codegen_info
    module = gen.module
    operands = [gen.take(operand) for operand in vop.args]
    bases, namespace, keywords = operands
    layout = module.layouts.get(info.class_id)
    name = module.text(info.public_name)
    if layout is not None:
        gen.pointer_result(
            vop.results[0],
            f"call ptr @mule_native_class(ptr {name}, "
            f"ptr {layout.schema}, ptr %globals, i64 {len(info.fields)}, ptr {module.names(info.fields)}, "
            f"ptr {namespace}, ptr {layout.constructor}, ptr {layout.getter}, ptr {layout.setter})",
            operands,
        )
        return
    gen.pointer_result(
        vop.results[0],
        f"call ptr @mule_class(ptr {name}, ptr {bases}, "
        f"i64 {len(info.fields)}, ptr {module.names(info.fields)}, ptr {namespace}, "
        f"ptr {keywords})",
        operands,
    )
    # Record the class object's layout for receiver guards on its instances.
    # A metaclass may return something other than a class; the slot then
    # holds zero and every guard fails.
    created = gen.load(vop.results[0])
    layout = gen.operation(f"call i64 @mule.class.layout(ptr {created})")
    gen.line(f"store i64 {layout}, ptr {module.class_layout_slot(info.identity)}")


@define_vop(
    "class-namespace",
    translate=fndb.CLASS_NAMESPACE,
    args=(OBJECT, OBJECT, OBJECT),
    results=(OBJECT,),
)
def class_namespace(gen, vop):
    bases, keywords, name = (gen.take(operand) for operand in vop.args)
    gen.pointer_result(
        vop.results[0],
        f"call ptr @mule_class_prepare(ptr {name}, ptr {bases}, ptr {keywords})",
        (bases, keywords, name),
    )


@define_vop("class-load", translate=fndb.CLASS_LOAD, args=(OBJECT,), results=(OBJECT,))
def class_load(gen, vop):
    namespace = gen.take(vop.args[0])
    gen.pointer_result(
        vop.results[0],
        f"call ptr @mule_class_load(ptr {namespace}, ptr %globals, "
        f"ptr {gen.module.text(vop.codegen_info.name)})",
        (namespace,),
    )


@define_vop(
    "class-load-or",
    translate=fndb.CLASS_LOAD_OR,
    args=(OBJECT, OBJECT),
    results=(OBJECT,),
)
def class_load_or(gen, vop):
    namespace, fallback = gen.take(vop.args[0]), gen.take(vop.args[1])
    gen.pointer_result(
        vop.results[0],
        f"call ptr @mule_class_load_or(ptr {namespace}, ptr {fallback}, "
        f"ptr {gen.module.text(vop.codegen_info.name)})",
        (namespace, fallback),
    )


@define_vop("class-store", translate=fndb.CLASS_STORE, args=(OBJECT, OBJECT))
def class_store(gen, vop):
    namespace, value = gen.take(vop.args[0]), gen.take(vop.args[1])
    status = gen.operation(
        f"call i32 @mule_class_store(ptr {namespace}, "
        f"ptr {gen.module.text(vop.codegen_info.name)}, ptr {value})"
    )
    gen.release(namespace)
    gen.release(value)
    gen.check_status(status)


@define_vop("class-delete", translate=fndb.CLASS_DELETE, args=(OBJECT,))
def class_delete(gen, vop):
    namespace = gen.take(vop.args[0])
    info = vop.codegen_info
    status = gen.operation(
        f"call i32 @mule_class_delete(ptr {namespace}, "
        f"ptr {gen.module.text(info.name)}, i32 {int(info.missing_ok)})"
    )
    gen.release(namespace)
    gen.check_status(status)


@define_vop("import", translate=fndb.IMPORT, results=(OBJECT,))
def import_(gen, vop):
    symbol = gen.module.import_symbol(vop.codegen_info)
    gen.pointer_result(vop.results[0], f"call ptr {symbol}()")


@define_vop("yield", translate=fndb.YIELD, args=(OBJECT,), results=(OBJECT,))
def yield_(gen, vop):
    value = gen.take(vop.args[0])
    gen.sync_coroutine_locals()
    gen.pointer_result(
        vop.results[0], f"call ptr @mule_coroutine_yield(ptr {value})", (value,)
    )


@define_vop("import-star", translate=fndb.IMPORT_STAR, args=(OBJECT,))
def import_star(gen, vop):
    module = gen.take(vop.args[0])
    status = gen.operation(f"call i32 @mule_import_star(ptr {module}, ptr %globals)")
    gen.release(module)
    gen.check_status(status)


@define_vop(
    "has-binding", translate=fndb.HAS_BINDING, args=(OBJECT,), results=(OBJECT,)
)
def has_binding(gen, vop):
    module = gen.take(vop.args[0])
    status = gen.operation(
        f"call i32 @mule_namespace_bound(ptr {module}, "
        f"ptr {gen.module.text(vop.codegen_info)})"
    )
    gen.release(module)
    gen.check_status(status)
    bound = gen.operation(f"icmp ne i32 {status}, 0")
    result = gen.operation(f"select i1 {bound}, ptr @mule.true, ptr @mule.false")
    gen.native_result(vop.results[0], result)


@define_vop("current-globals", translate=fndb.CURRENT_GLOBALS, results=(OBJECT,))
def current_globals(gen, vop):
    # The entry's globals argument, retained for the result's owner.
    gen.pointer_result(vop.results[0], "call ptr @mule.fast.retain(ptr %globals)")


def _sequence(gen, vop, primitive):
    values = [gen.take(element) for element in vop.args]
    gen.argv(values)
    buf = "%argv" if values else "null"
    gen.pointer_result(
        vop.results[0], f"call ptr @{primitive}(i64 {len(values)}, ptr {buf})", values
    )


@define_vop("build-list", translate=fndb.BUILD_LIST, results=(OBJECT,))
def build_list(gen, vop):
    _sequence(gen, vop, "mule_sequence_list")


@define_vop("build-tuple", translate=fndb.BUILD_TUPLE, results=(OBJECT,))
def build_tuple(gen, vop):
    values = [gen.take(element) for element in vop.args]
    if gen.is_unpacked_return_vop(vop):
        gen.unpacked_return_result(vop.results[0], values)
        return
    gen.argv(values)
    buf = "%argv" if values else "null"
    gen.pointer_result(
        vop.results[0],
        f"call ptr @mule_sequence_tuple_owned(i64 {len(values)}, ptr {buf})",
    )


@define_vop("format", translate=fndb.FORMAT, args=(OBJECT, OBJECT), results=(OBJECT,))
def format_(gen, vop):
    result = vop.results[0]
    value = gen.take(vop.args[0])
    conversion = vop.codegen_info.conversion
    if conversion is not None:
        protocol = {"s": "string", "r": "representation", "a": "ascii_representation"}[
            conversion
        ]
        converted = gen.compiled_call_named("mule_format_" + protocol, [value])
        gen.publish(result, converted)
        gen.check_pointer(converted)
        value = gen.take(result)
    specification = gen.take(vop.args[1])
    formatted = gen.compiled_call_named(
        "mule_format_format_value", [value, specification]
    )
    gen.publish(result, formatted)
    gen.check_pointer(formatted)
    gen.refine(result.tn, formatted)


def _decode_fixnum_operand(gen, operand) -> str:
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


@define_vop(
    "range-prepare",
    translate=fndb.RANGE_PREPARE,
    results=(OBJECT, OBJECT),
    arg_escapes=(False, False, False, False),
)
def range_prepare(gen, vop):
    info = vop.codegen_info
    if info is not None and info.direct:
        state = vop.args[-1].tn.slot
        arity = len(vop.args) - 1
        if arity == 1:
            start = "0"
            stop = _decode_fixnum_operand(gen, vop.args[0])
            step = "1"
        elif arity == 2:
            start = _decode_fixnum_operand(gen, vop.args[0])
            stop = _decode_fixnum_operand(gen, vop.args[1])
            step = "1"
        else:
            start = _decode_fixnum_operand(gen, vop.args[0])
            stop = _decode_fixnum_operand(gen, vop.args[1])
            step = _decode_fixnum_operand(gen, vop.args[2])
        for index, word in enumerate((start, stop, step)):
            address = gen.operation(f"getelementptr i64, ptr {state}, i64 {index}")
            gen.line(f"store i64 {word}, ptr {address}")
        if vop.results:
            gen.publish(vop.results[0], "@mule.true")
            gen.refine(vop.results[0].tn, "@mule.true")
        return
    if info is not None and info.fused:
        state = vop.args[-1].tn.slot
        arg_operands = vop.args[:-1]
        arity = len(arg_operands)
        result, iterator_out = vop.results
        if info.step_sign != 0 and all(
            op.tn.type is not None
            and op.tn.type.fits(PYTHON_INTEGER)
            and op.tn.type.is_fixnum
            and not op.tn.type.may_be_unbound
            for op in arg_operands
        ):
            if arity == 1:
                start = "0"
                stop = _decode_fixnum_operand(gen, arg_operands[0])
                step = "1"
            elif arity == 2:
                start = _decode_fixnum_operand(gen, arg_operands[0])
                stop = _decode_fixnum_operand(gen, arg_operands[1])
                step = "1"
            else:
                start = _decode_fixnum_operand(gen, arg_operands[0])
                stop = _decode_fixnum_operand(gen, arg_operands[1])
                step = _decode_fixnum_operand(gen, arg_operands[2])
            for index, word in enumerate((start, stop, step)):
                address = gen.operation(f"getelementptr i64, ptr {state}, i64 {index}")
                gen.line(f"store i64 {word}, ptr {address}")
            gen.publish(result, "@mule.true")
            gen.refine(result.tn, "@mule.true")
            return
        arg_values = [gen.take(op) for op in arg_operands]
        bits_list = []
        fast_ok = None
        for val in arg_values:
            bits = gen.operation(f"ptrtoint ptr {val} to i64")
            bits_list.append(bits)
            tag = gen.operation(f"and i64 {bits}, 1")
            is_fix = gen.operation(f"icmp ne i64 {tag}, 0")
            fast_ok = (
                is_fix
                if fast_ok is None
                else gen.operation(f"and i1 {fast_ok}, {is_fix}")
            )
        step_decoded = None
        if arity == 3 and info.step_sign == 0:
            step_decoded = gen.operation(f"ashr i64 {bits_list[2]}, 1")
            step_nz = gen.operation(f"icmp ne i64 {step_decoded}, 0")
            fast_ok = gen.operation(f"and i1 {fast_ok}, {step_nz}")
        fast_label, slow_label, done_label = (
            gen.fresh(name)[1:] for name in ("range.fast", "range.slow", "range.done")
        )
        gen.line(f"br i1 {fast_ok}, label %{fast_label}, label %{slow_label}")
        gen.lines.append(f"{fast_label}:")
        if arity == 1:
            start = "0"
            stop = gen.operation(f"ashr i64 {bits_list[0]}, 1")
            step = "1"
        elif arity == 2:
            start = gen.operation(f"ashr i64 {bits_list[0]}, 1")
            stop = gen.operation(f"ashr i64 {bits_list[1]}, 1")
            step = "1"
        else:
            start = gen.operation(f"ashr i64 {bits_list[0]}, 1")
            stop = gen.operation(f"ashr i64 {bits_list[1]}, 1")
            step = (
                step_decoded
                if step_decoded is not None
                else gen.operation(f"ashr i64 {bits_list[2]}, 1")
            )
        for index, word in enumerate((start, stop, step)):
            address = gen.operation(f"getelementptr i64, ptr {state}, i64 {index}")
            gen.line(f"store i64 {word}, ptr {address}")
        fast_end = gen.current_block()
        gen.line(f"br label %{done_label}")
        gen.lines.append(f"{slow_label}:")
        callee = gen.operation(
            gen.module.load_global(info.callee_leaf, gen.lambda_.policy.safety)
        )
        call_helper = gen.module.positional_call_helper(arity, borrowed=True)
        operands = ", ".join([f"ptr {callee}", *(f"ptr {val}" for val in arg_values)])
        range_obj = gen.operation(f"call ptr {call_helper}({operands})")
        gen.release(callee)
        for val in arg_values:
            gen.release(val)
        gen.check_pointer(range_obj)
        iter_helper = gen.module.iterate_helper(move=True)
        iter_obj = gen.operation(f"call ptr {iter_helper}(ptr {range_obj})")
        gen.check_pointer(iter_obj)
        gen.store(iterator_out, iter_obj)
        stored = gen.operation(
            f"call i32 @mule_range_storage(ptr {iter_obj}, ptr %range.buffer)"
        )
        words = gen.operation("load [3 x i64], ptr %range.buffer")
        gen.line(f"store [3 x i64] {words}, ptr {state}")
        accepted = gen.operation(f"icmp ne i32 {stored}, 0")
        slow_res = gen.operation(
            f"select i1 {accepted}, ptr @mule.true, ptr @mule.false"
        )
        slow_end = gen.current_block()
        gen.line(f"br label %{done_label}")
        gen.lines.append(f"{done_label}:")
        final_res = gen.operation(
            f"phi ptr [@mule.true, %{fast_end}], [{slow_res}, %{slow_end}]"
        )
        gen.publish(result, final_res)
        gen.refine(result.tn, final_res)
        return
    callee, iterator = gen.take(vop.args[0]), gen.take(vop.args[1])
    state = vop.args[2].tn.slot
    # No callable-identity guard: `range` is a class, and the storage check
    # only accepts iterators of the protocol's own RangeIterator class.
    stored = gen.operation(
        f"call i32 @mule_range_storage(ptr {iterator}, ptr %range.buffer)"
    )
    words = gen.operation("load [3 x i64], ptr %range.buffer")
    gen.line(f"store [3 x i64] {words}, ptr {state}")
    accepted = gen.operation(f"icmp ne i32 {stored}, 0")
    result = gen.operation(f"select i1 {accepted}, ptr @mule.true, ptr @mule.false")
    gen.publish(vop.results[0], result)
    gen.refine(vop.results[0].tn, result)
    gen.release(callee)
    gen.release(iterator)


def _range_words(gen, state):
    words = []
    for index in range(3):
        address = gen.operation(f"getelementptr i64, ptr {state}, i64 {index}")
        words.append(gen.operation(f"load i64, ptr {address}"))
    return words


@define_vop("range-available", translate=fndb.RANGE_AVAILABLE, results=(OBJECT,))
def range_available(gen, vop):
    info = vop.codegen_info
    state = vop.args[0].tn.slot
    if info is not None and info.step_sign > 0:
        current = gen.operation(f"load i64, ptr {state}")
        stop_addr = gen.operation(f"getelementptr i64, ptr {state}, i64 1")
        stop = gen.operation(f"load i64, ptr {stop_addr}")
        available = gen.operation(f"icmp slt i64 {current}, {stop}")
    elif info is not None and info.step_sign < 0:
        current = gen.operation(f"load i64, ptr {state}")
        stop_addr = gen.operation(f"getelementptr i64, ptr {state}, i64 1")
        stop = gen.operation(f"load i64, ptr {stop_addr}")
        available = gen.operation(f"icmp sgt i64 {current}, {stop}")
    else:
        current, stop, step = _range_words(gen, state)
        forward = gen.operation(f"icmp sgt i64 {step}, 0")
        below = gen.operation(f"icmp slt i64 {current}, {stop}")
        above = gen.operation(f"icmp sgt i64 {current}, {stop}")
        available = gen.operation(f"select i1 {forward}, i1 {below}, i1 {above}")
    result = gen.operation(f"select i1 {available}, ptr @mule.true, ptr @mule.false")
    gen.publish(vop.results[0], result)
    gen.refine(vop.results[0].tn, result)


@define_vop("range-advance", translate=fndb.RANGE_ADVANCE, results=(OBJECT,))
def range_advance(gen, vop):
    info = vop.codegen_info
    state = vop.args[0].tn.slot
    if info is not None and len(info.arguments) <= 2:
        current = gen.operation(f"load i64, ptr {state}")
        following = gen.operation(f"add nsw i64 {current}, 1")
    else:
        current, _, step = _range_words(gen, state)
        following = gen.operation(f"add nsw i64 {current}, {step}")
    gen.line(f"store i64 {following}, ptr {state}")
    encoded = gen.operation(f"shl nsw i64 {current}, 1")
    tagged = gen.operation(f"or disjoint i64 {encoded}, 1")
    result = gen.operation(f"inttoptr i64 {tagged} to ptr")
    gen.publish(vop.results[0], result)
    gen.refine(vop.results[0].tn, result)


@define_vop("sequence-available", translate=fndb.SEQUENCE_AVAILABLE, results=(OBJECT,))
def sequence_available(gen, vop):
    iterator = gen.load(vop.args[0])
    out = gen.out_pointer()
    unpack_arity = gen.unpacked_sequence_available_arity(vop)
    if unpack_arity is not None:
        helper = gen.module.next_unpack_helper(unpack_arity)
        status = gen.operation(
            f"call i32 {helper}(ptr {iterator}, ptr %unpack.out, ptr {out})"
        )
    else:
        helper = gen.module.next_item_helper()
        status = gen.operation(f"call i32 {helper}(ptr {iterator}, ptr {out})")
    gen.check_status(status)
    available = gen.operation(f"icmp ne i32 {status}, 0")
    result = gen.operation(f"select i1 {available}, ptr @mule.true, ptr @mule.false")
    gen.publish(vop.results[0], result)
    gen.refine(vop.results[0].tn, result)


@define_vop(
    "protocol-next_item", translate=fndb.PROTOCOLS["next_item"], results=(OBJECT,)
)
def protocol_next_item(gen, vop):
    iterator = gen.take(vop.args[0])
    if iterator in ("null", "@mule.none"):
        item = gen.operation(f"load ptr, ptr {gen.out_pointer()}")
        gen.publish(vop.results[0], item)
        gen.refine(vop.results[0].tn, item)
        return
    result = gen.sequence_call("next_item", [iterator])
    gen.publish(vop.results[0], result)
    gen.check_pointer(result)
    gen.refine(vop.results[0].tn, result)


@define_vop("protocol-iterate", translate=fndb.PROTOCOLS["iterate"], results=(OBJECT,))
def protocol_iterate(gen, vop):
    sequence = gen.take(vop.args[0])
    move = gen.owns_reference(sequence)
    helper = gen.module.iterate_helper(move=move)
    result = gen.operation(f"call ptr {helper}(ptr {sequence})")
    if not move:
        gen.release(sequence)
    gen.publish(vop.results[0], result)
    gen.check_pointer(result)
    gen.refine(vop.results[0].tn, result)


_SUSPENSION_PROTOCOLS = frozenset(
    {"yield_from", "await_value", "async_generator_yield"}
)


def _define_protocol(name, info):
    suspends = name in _SUSPENSION_PROTOCOLS

    @define_vop(f"protocol-{name}", translate=info, results=(OBJECT,))
    def protocol(gen, vop):
        arguments = [gen.take(value) for value in vop.args]
        if suspends:
            gen.sync_coroutine_locals()
        result = gen.sequence_call(name, arguments)
        gen.publish(vop.results[0], result)
        gen.check_pointer(result)
        gen.refine(vop.results[0].tn, result)


@define_vop("structural-unpack", translate=fndb.PROTOCOLS["unpack"], results=(OBJECT,))
def structural_unpack(gen, vop):
    value, count = [gen.take(argument) for argument in vop.args]
    if gen.is_unpacked_unpack(vop):
        gen.immortal_values.add("@mule.none")
        gen.publish(vop.results[0], "@mule.none")
        gen.release(count)
        return
    slot_arity = gen.unpacked_slot_arity(vop)
    if slot_arity is not None:
        if vop.codegen_info.proved:
            # The exact tuple and its arity are already proven. Acquire every
            # child before a target can mutate another binding to the source.
            # Iterator delivery uses null to signal that these slots are filled.
            already = gen.operation(f"icmp eq ptr {value}, null")
            copy_label = gen.fresh("unpack.copy")[1:]
            done_label = gen.fresh("unpack.done")[1:]
            gen.line(f"br i1 {already}, label %{done_label}, label %{copy_label}")
            gen.lines.append(copy_label + ":")
            for index in range(slot_arity):
                child = gen.operation(
                    f"call ptr @mule.fast.tuple.slot(ptr {value}, i64 {index})"
                )
                slot = gen.operation(f"getelementptr ptr, ptr %unpack.out, i64 {index}")
                gen.line(f"store ptr {child}, ptr {slot}")
            gen.release(value)
            gen.line(f"br label %{done_label}")
            gen.lines.append(done_label + ":")
            gen.release(count)
            gen.immortal_values.add("@mule.none")
            gen.publish(vop.results[0], "@mule.none")
            return
        helper = gen.module.unpack_slots_helper(slot_arity)
        gen.pointer_result(
            vop.results[0],
            f"call ptr {helper}(ptr {value}, ptr {count}, ptr %unpack.out)",
        )
        return
    if vop.codegen_info.proved:
        gen.publish(vop.results[0], value)
        gen.refine(vop.results[0].tn, value)
        gen.release(count)
        return
    helper = gen.module.unpack_helper(vop.codegen_info.arity)
    gen.pointer_result(vop.results[0], f"call ptr {helper}(ptr {value}, ptr {count})")


def _exact_dict_call(gen, vop, name):
    """Call the ``_sequences`` entry for an operand proven to be an exact
    ``dict``. Both the VOPs and the compiled ``dict_getitem`` /
    ``dict_setitem`` entries borrow their operands."""
    operands = [gen.take(ref) for ref in vop.args]
    result = gen.sequence_call(name, operands)
    for operand in operands:
        gen.release(operand)
    gen.publish(vop.results[0], result)
    gen.check_pointer(result)
    gen.refine(vop.results[0].tn, result)


@define_vop("protocol-getitem", translate=fndb.PROTOCOLS["getitem"], results=(OBJECT,))
def protocol_getitem(gen, vop):
    seq_type = vop.args[0].tn.type or UNKNOWN
    idx_type = vop.args[1].tn.type or UNKNOWN
    if seq_type.fits_dict():
        _exact_dict_call(gen, vop, "dict_getitem")
        return
    if seq_type.fits_bytearray() and idx_type.fits(PYTHON_INTEGER):
        if idx_type.is_fixnum:
            sequence = gen.take(vop.args[0])
            index = gen.take(vop.args[1])
            result = gen.operation(
                f"call ptr @mule_bytearray_getitem_fast(ptr {sequence}, ptr {index})"
            )
            gen.immortal_values.add(result)
            gen.release(sequence)
            gen.release(index)
            gen.publish(vop.results[0], result)
            gen.check_pointer(result)
            gen.refine(vop.results[0].tn, result)
            return
        _exact_dict_call(gen, vop, "bytearray_getitem")
        return
    if seq_type.fits_bytes(seq_type) and idx_type.is_fixnum:
        sequence = gen.take(vop.args[0])
        index = gen.take(vop.args[1])
        result = gen.operation(
            f"call ptr @mule_bytes_getitem(ptr {sequence}, ptr {index})"
        )
        gen.immortal_values.add(result)
        gen.release(sequence)
        gen.release(index)
        gen.publish(vop.results[0], result)
        gen.check_pointer(result)
        gen.refine(vop.results[0].tn, result)
        return
    sequence = gen.take(vop.args[0])
    index = gen.take(vop.args[1])
    info = vop.codegen_info
    if info in ("list_borrow", "list_owned", "tuple_borrow", "tuple_owned"):
        if info == "list_borrow":
            helper = "@mule_list_getitem_borrow"
        elif info == "list_owned":
            helper = "@mule_list_getitem"
        else:
            helper = gen.module.getitem_helper()
        seq_type = vop.args[0].tn.type or UNKNOWN
        idx_type = vop.args[1].tn.type or UNKNOWN
        non_negative = idx_type.minimum is not None and idx_type.minimum >= 0
        bits = gen.operation(f"ptrtoint ptr {index} to i64")
        raw = gen.operation(f"ashr i64 {bits}, 1")
        exact_len = seq_type.exact_length()
        if exact_len is not None:
            length = str(exact_len)
        else:
            len_addr = gen.operation(
                f"getelementptr i64, ptr {sequence}, i64 {VALUE_BUFFER_LENGTH_OFFSET // 8}"
            )
            length = gen.operation(f"load i64, ptr {len_addr}")
        if non_negative:
            pos = raw
        else:
            neg = gen.operation(f"icmp slt i64 {raw}, 0")
            wrapped = gen.operation(f"add i64 {raw}, {length}")
            pos = gen.operation(f"select i1 {neg}, i64 {wrapped}, i64 {raw}")
        inside = gen.operation(f"icmp ult i64 {pos}, {length}")
        read_lbl = gen.fresh("list.read")[1:]
        slow_lbl = gen.fresh("list.slow")[1:]
        done_lbl = gen.fresh("list.done")[1:]
        gen.line(
            f"br i1 {inside}, label %{read_lbl}, label %{slow_lbl}, "
            f'!prof !{{!"branch_weights", i32 2000, i32 1}}'
        )
        gen.lines.append(read_lbl + ":")
        data_addr = gen.operation(
            f"getelementptr ptr, ptr {sequence}, i64 {VALUE_BUFFER_DATA_OFFSET // 8}"
        )
        data = gen.operation(f"load ptr, ptr {data_addr}")
        slot = gen.operation(f"getelementptr ptr, ptr {data}, i64 {pos}")
        fast_val = gen.operation(f"load ptr, ptr {slot}, !nonnull !{{}}")
        res_tn = vop.results[0].tn
        res_type = res_tn.type or UNKNOWN
        unchecked_contract = gen._has_unchecked_element_contract(res_tn)
        immortal_elem = not unchecked_contract and (
            not res_tn.sc.owned or res_type.is_fixnum or res_type.is_immortal
        )
        if info in ("list_owned", "tuple_owned") and not immortal_elem:
            if not unchecked_contract and res_type.is_heap_object:
                gen.heap_values.add(fast_val)
            fast_val = gen.retain(fast_val)
        gen.line(f"br label %{done_lbl}")
        gen.lines.append(slow_lbl + ":")
        slow_val = gen.operation(f"call ptr {helper}(ptr {sequence}, ptr {index})")
        gen.line(f"br label %{done_lbl}")
        gen.lines.append(done_lbl + ":")
        result = gen.operation(
            f"phi ptr [{fast_val}, %{read_lbl}], [{slow_val}, %{slow_lbl}]"
        )
    else:
        if info is True:
            helper = "@mule_text_codepoint_getitem"
        else:
            helper = gen.module.getitem_helper()
        result = gen.operation(f"call ptr {helper}(ptr {sequence}, ptr {index})")
    gen.release(sequence)
    gen.release(index)
    gen.publish(vop.results[0], result)
    gen.check_pointer(result)
    gen.refine(vop.results[0].tn, result)


@define_vop("protocol-getslice", translate=fndb.GETSLICE, results=(OBJECT,))
def protocol_getslice(gen, vop):
    sequence = gen.take(vop.args[0])
    start = gen.take(vop.args[1])
    stop = gen.take(vop.args[2])
    step = gen.take(vop.args[3])
    helper = gen.module.getslice_helper()
    result = gen.operation(
        f"call ptr {helper}(ptr {sequence}, ptr {start}, ptr {stop}, ptr {step})"
    )
    gen.release(sequence)
    gen.release(start)
    gen.release(stop)
    gen.release(step)
    gen.publish(vop.results[0], result)
    gen.check_pointer(result)
    gen.refine(vop.results[0].tn, result)


@define_vop(
    "text-slice-compare",
    args=(OBJECT, OBJECT, OBJECT, OBJECT),
    results=(OBJECT,),
)
def text_slice_compare(gen, vop):
    sequence = gen.take(vop.args[0])
    start = gen.take(vop.args[1])
    stop = gen.take(vop.args[2])
    target = gen.take(vop.args[3])
    invert = int(bool(vop.codegen_info))
    boolean = gen.operation(
        f"call ptr @mule_text_slice_eq(ptr {sequence}, ptr {start}, ptr {stop}, "
        f"ptr {target}, i32 {invert})"
    )
    gen.native_result(vop.results[0], boolean, (sequence, start, stop, target))


@define_vop(
    "text-slice2-compare",
    args=(OBJECT, OBJECT, OBJECT, OBJECT, OBJECT, OBJECT),
    results=(OBJECT,),
)
def text_slice2_compare(gen, vop):
    left = gen.take(vop.args[0])
    start1 = gen.take(vop.args[1])
    stop1 = gen.take(vop.args[2])
    right = gen.take(vop.args[3])
    start2 = gen.take(vop.args[4])
    stop2 = gen.take(vop.args[5])
    invert = int(bool(vop.codegen_info))
    boolean = gen.operation(
        f"call ptr @mule_text_slice2_eq(ptr {left}, ptr {start1}, ptr {stop1}, "
        f"ptr {right}, ptr {start2}, ptr {stop2}, i32 {invert})"
    )
    gen.native_result(
        vop.results[0], boolean, (left, start1, stop1, right, start2, stop2)
    )


@define_vop(
    "protocol-setitem",
    translate=fndb.PROTOCOLS["setitem"],
    args=(OBJECT, OBJECT, (*OBJECT, DOUBLE)),
    results=(OBJECT,),
)
def protocol_setitem(gen, vop):
    seq_type = vop.args[0].tn.type or UNKNOWN
    idx_type = vop.args[1].tn.type or UNKNOWN
    if vop.args[2].tn.sc is DOUBLE:
        sequence = gen.take(vop.args[0])
        index = gen.take(vop.args[1])
        raw_item = gen.take(vop.args[2])
        if (
            seq_type.is_exact_list
            and idx_type.is_fixnum
            and seq_type.iterate_type().next_item_type() == FLOAT
        ):
            non_negative = idx_type.minimum is not None and idx_type.minimum >= 0
            bits = gen.operation(f"ptrtoint ptr {index} to i64")
            raw_idx = gen.operation(f"ashr i64 {bits}, 1")
            len_addr = gen.operation(
                f"getelementptr i64, ptr {sequence}, i64 {VALUE_BUFFER_LENGTH_OFFSET // 8}"
            )
            length = gen.operation(f"load i64, ptr {len_addr}")
            if non_negative:
                pos = raw_idx
            else:
                neg = gen.operation(f"icmp slt i64 {raw_idx}, 0")
                wrapped = gen.operation(f"add i64 {raw_idx}, {length}")
                pos = gen.operation(f"select i1 {neg}, i64 {wrapped}, i64 {raw_idx}")
            inside = gen.operation(f"icmp ult i64 {pos}, {length}")
            set_lbl = gen.fresh("list.fset")[1:]
            reuse_lbl = gen.fresh("list.freuse")[1:]
            alloc_lbl = gen.fresh("list.falloc")[1:]
            slow_lbl = gen.fresh("list.fslow")[1:]
            done_lbl = gen.fresh("list.fdone")[1:]
            gen.line(
                f"br i1 {inside}, label %{set_lbl}, label %{slow_lbl}, "
                f'!prof !{{!"branch_weights", i32 2000, i32 1}}'
            )
            gen.lines.append(set_lbl + ":")
            data_addr = gen.operation(
                f"getelementptr ptr, ptr {sequence}, i64 {VALUE_BUFFER_DATA_OFFSET // 8}"
            )
            data = gen.operation(f"load ptr, ptr {data_addr}")
            slot = gen.operation(f"getelementptr ptr, ptr {data}, i64 {pos}")
            old_box = gen.operation(f"load ptr, ptr {slot}, !nonnull !{{}}")
            refs = gen.operation(f"load i64, ptr {old_box}")
            exclusive = gen.operation(f"icmp eq i64 {refs}, 1")
            gen.line(
                f"br i1 {exclusive}, label %{reuse_lbl}, label %{alloc_lbl}, "
                f'!prof !{{!"branch_weights", i32 2000, i32 1}}'
            )
            gen.lines.append(reuse_lbl + ":")
            payload = gen.operation(
                f"getelementptr i8, ptr {old_box}, i64 {FLOAT_BOX_LAYOUT.payload_offset}"
            )
            gen.line(f"store double {raw_item}, ptr {payload}")
            gen.line(f"br label %{done_lbl}")
            gen.lines.append(alloc_lbl + ":")
            new_box = gen.operation(
                f"call ptr @{FLOAT_BOX_LAYOUT.symbol()}(double {raw_item})"
            )
            gen.line(f"store ptr {new_box}, ptr {slot}")
            gen.line(f"call void @mule.fast.release(ptr {old_box})")
            gen.line(f"br label %{done_lbl}")
            gen.lines.append(slow_lbl + ":")
            slow_box = gen.operation(
                f"call ptr @{FLOAT_BOX_LAYOUT.symbol()}(double {raw_item})"
            )
            helper = gen.module.setitem_helper(move=True)
            slow_res = gen.operation(
                f"call ptr {helper}(ptr {sequence}, ptr {index}, ptr {slow_box})"
            )
            slow_end = gen.current_block()
            gen.line(f"br label %{done_lbl}")
            gen.lines.append(done_lbl + ":")
            result = gen.operation(
                f"phi ptr [@mule.none, %{reuse_lbl}], [@mule.none, %{alloc_lbl}], [{slow_res}, %{slow_end}]"
            )
            gen.immortal_values.add(result)
            gen.release(sequence)
            gen.release(index)
            gen.publish(vop.results[0], result)
            gen.check_pointer(result)
            gen.refine(vop.results[0].tn, result)
            return
        boxed_item = gen.operation(
            f"call ptr @{FLOAT_BOX_LAYOUT.symbol()}(double {raw_item})"
        )
        helper = gen.module.setitem_helper(move=True)
        result = gen.operation(
            f"call ptr {helper}(ptr {sequence}, ptr {index}, ptr {boxed_item})"
        )
        gen.immortal_values.add(result)
        gen.release(sequence)
        gen.release(index)
        gen.publish(vop.results[0], result)
        gen.check_pointer(result)
        gen.refine(vop.results[0].tn, result)
        return
    if seq_type.fits_dict():
        _exact_dict_call(gen, vop, "dict_setitem")
        return
    if seq_type.fits_bytearray() and idx_type.fits(PYTHON_INTEGER):
        if idx_type.is_fixnum:
            sequence = gen.take(vop.args[0])
            index = gen.take(vop.args[1])
            item = gen.take(vop.args[2])
            status = gen.operation(
                f"call i32 @mule_bytearray_setitem_fast(ptr {sequence}, ptr {index}, ptr {item})"
            )
            hit = gen.operation(f"icmp ne i32 {status}, 0")
            done_lbl, slow_lbl = (
                gen.fresh("bytearray.set.done")[1:],
                gen.fresh("bytearray.set.slow")[1:],
            )
            fast_end = gen.current_block()
            gen.line(f"br i1 {hit}, label %{done_lbl}, label %{slow_lbl}")
            gen.lines.append(slow_lbl + ":")
            slow = gen.sequence_call("bytearray_setitem", [sequence, index, item])
            slow_end = gen.current_block()
            gen.line(f"br label %{done_lbl}")
            gen.lines.append(done_lbl + ":")
            result = gen.operation(
                f"phi ptr [@mule.none, %{fast_end}], [{slow}, %{slow_end}]"
            )
            gen.immortal_values.add(result)
            gen.release(sequence)
            gen.release(index)
            gen.release(item)
            gen.publish(vop.results[0], result)
            gen.check_pointer(result)
            gen.refine(vop.results[0].tn, result)
            return
        _exact_dict_call(gen, vop, "bytearray_setitem")
        return
    sequence = gen.take(vop.args[0])
    index = gen.take(vop.args[1])
    item = gen.take(vop.args[2])
    move_item = gen.owns_reference(item) and item != sequence and item != index
    helper = gen.module.setitem_helper(move=move_item)
    result = gen.operation(
        f"call ptr {helper}(ptr {sequence}, ptr {index}, ptr {item})"
    )
    gen.immortal_values.add(result)
    gen.release(sequence)
    gen.release(index)
    if not move_item:
        gen.release(item)
    gen.publish(vop.results[0], result)
    gen.check_pointer(result)
    gen.refine(vop.results[0].tn, result)


for _name, _info in fndb.PROTOCOLS.items():
    if _name not in ("unpack", "getitem", "setitem", "next_item", "iterate"):
        _define_protocol(_name, _info)


@define_vop("unpack-ref", translate=fndb.UNPACK_REF, results=(OBJECT,))
def unpack_ref(gen, vop):
    slot_idx = gen.unpacked_ref_slot(vop)
    if slot_idx is not None:
        position = gen.take(vop.args[1])
        slot = gen.operation(f"getelementptr ptr, ptr %unpack.out, i64 {slot_idx}")
        result = gen.operation(f"load ptr, ptr {slot}")
        gen.unpacked_remaining_slots = gen.unpacked_ref_remaining(vop)
        if gen.is_unpacked_immortal_ref(vop):
            gen.immortal_values.add(result)
        gen.publish(vop.results[0], result)
        gen.release(position)
        gen.refine(vop.results[0].tn, result)
        return
    access = vop.codegen_info
    parent = gen.load(vop.args[0]) if access.borrow_parent else gen.take(vop.args[0])
    position = gen.take(vop.args[1])
    res_tn = vop.results[0].tn
    res_type = res_tn.type or UNKNOWN
    immortal_elem = not gen._has_unchecked_element_contract(res_tn) and (
        not res_tn.sc.owned or res_type.is_fixnum or res_type.is_immortal
    )
    if access.proved:
        slot_helper = (
            "@mule.fast.tuple.borrow"
            if access.borrow_element or immortal_elem
            else "@mule.fast.tuple.slot"
        )
        result = gen.operation(
            f"call ptr {slot_helper}(ptr {parent}, i64 {access.index})"
        )
    else:
        result = gen.operation(
            f"call ptr @mule_storage_read(ptr {parent}, ptr {position})"
        )
    gen.publish(vop.results[0], result)
    if not access.borrow_parent:
        gen.release(parent)
    gen.release(position)
    if not access.proved:
        gen.check_pointer(result)
    gen.refine(vop.results[0].tn, result)


@define_vop("catch-exception", translate=fndb.CATCH_EXCEPTION, results=(OBJECT,))
def catch_exception(gen, vop):
    gen.pointer_result(vop.results[0], "call ptr @mule_catch_exception()")


@define_vop("enter-handler", translate=fndb.ENTER_HANDLER, args=(OBJECT,))
def enter_handler(gen, vop):
    value = gen.take(vop.args[0])
    status = gen.operation(f"call i32 @mule_enter_handler(ptr {value})")
    gen.release(value)
    gen.check_status(status)


@define_vop("leave-handler", translate=fndb.LEAVE_HANDLER)
def leave_handler(gen, vop):
    gen.line("call void @mule_leave_handler()")


@define_vop("assertion-failure", translate=fndb.ASSERTION_FAILURE, args=(OBJECT,))
def assertion_failure(gen, vop):
    if vop.codegen_info is False:
        unused = gen.take(vop.args[0])
        gen.release(unused)
        status = gen.operation("call i32 @mule_assertion_failure(ptr null)")
        gen.check_status(status)
        return
    message = gen.take(vop.args[0])
    status = gen.operation(f"call i32 @mule_assertion_failure(ptr {message})")
    gen.release(message)
    gen.check_status(status)


@define_vop("raise", translate=fndb.RAISE, args=(OBJECT,))
def raise_(gen, vop):
    value = gen.take(vop.args[0])
    result = gen.sequence_call("raise_value", [value])
    gen.check_pointer(result)
    gen.line(f"call ptr @mule_raise_pending(ptr {result})")
    gen.release(result)
    gen.propagate(explicit_raise=True)


@define_vop("resume-exception", translate=fndb.RESUME_EXCEPTION, args=(OBJECT,))
def resume_exception(gen, vop):
    value = gen.take(vop.args[0])
    gen.line(f"call ptr @mule_raise_pending(ptr {value})")
    gen.release(value)
    gen.propagate(record_traceback=False)


@define_vop("reraise", translate=fndb.RERAISE)
def reraise(gen, vop):
    gen.line("call i32 @mule_reraise()")
    gen.propagate()


@define_vop("unwind", translate=fndb.UNWIND)
def unwind(gen, vop):
    gen.propagate()


# Compiled-protocol primitives: the physical operations the compiled stdlib
# modules call by underscored name.


def primitive(name, info, emit, *, consumes_arguments=False):
    """Register ``emit(gen, vop, values)`` as the template of ``info``: the
    operands leave their TNs and the result is published before error checking.
    Borrowed physical primitives need caller release; compiled calls consume
    their arguments on every arm."""

    @define_vop(f"primitive-{name}", translate=info, results=(OBJECT,))
    def generator(gen, vop):
        values = [gen.take(value) for value in vop.args]
        result = emit(gen, vop, values)
        gen.publish(vop.results[0], result)
        if not consumes_arguments:
            for value in values:
                gen.release(value)
        if not result.startswith("@mule."):
            gen.check_pointer(result)
        gen.refine(vop.results[0].tn, result)

    return generator


def text_access(function):
    storage_helper = {
        "mule_storage_read": "@mule.fast.storage.read",
        "mule_storage_write": "@mule.fast.storage.write",
        "mule_storage_length": "@mule.fast.storage.length",
    }.get(function)

    def emit(gen, vop, values):
        arg_type = (vop.args[0].tn.type or UNKNOWN) if vop.args else UNKNOWN
        if storage_helper is not None:
            if function == "mule_storage_length" and (
                arg_type.is_exact_list or arg_type.is_exact_tuple
            ):
                exact_len = arg_type.exact_length()
                if exact_len is not None:
                    count = str(exact_len)
                else:
                    len_addr = gen.operation(
                        f"getelementptr i64, ptr {values[0]}, i64 {VALUE_BUFFER_LENGTH_OFFSET // 8}"
                    )
                    count = gen.operation(f"load i64, ptr {len_addr}")
                return gen.encode(count)
            if (
                function == "mule_storage_read"
                and (arg_type.is_exact_list or arg_type.is_exact_tuple)
                and len(vop.args) == 2
                and (vop.args[1].tn.type or UNKNOWN).is_fixnum
                and vop.results
            ):
                sequence, index = values[0], values[1]
                idx_type = vop.args[1].tn.type or UNKNOWN
                non_negative = idx_type.minimum is not None and idx_type.minimum >= 0
                raw = gen.decode(index)
                exact_len = arg_type.exact_length()
                if exact_len is not None:
                    length = str(exact_len)
                else:
                    len_addr = gen.operation(
                        f"getelementptr i64, ptr {sequence}, i64 {VALUE_BUFFER_LENGTH_OFFSET // 8}"
                    )
                    length = gen.operation(f"load i64, ptr {len_addr}")
                if non_negative:
                    pos = raw
                else:
                    neg = gen.operation(f"icmp slt i64 {raw}, 0")
                    wrapped = gen.operation(f"add i64 {raw}, {length}")
                    pos = gen.operation(f"select i1 {neg}, i64 {wrapped}, i64 {raw}")
                inside = gen.operation(f"icmp ult i64 {pos}, {length}")
                read_lbl = gen.fresh("storage.read")[1:]
                slow_lbl = gen.fresh("storage.slow")[1:]
                done_lbl = gen.fresh("storage.done")[1:]
                gen.line(
                    f"br i1 {inside}, label %{read_lbl}, label %{slow_lbl}, "
                    f'!prof !{{!"branch_weights", i32 2000, i32 1}}'
                )
                gen.lines.append(read_lbl + ":")
                data_addr = gen.operation(
                    f"getelementptr ptr, ptr {sequence}, i64 {VALUE_BUFFER_DATA_OFFSET // 8}"
                )
                data = gen.operation(f"load ptr, ptr {data_addr}")
                slot = gen.operation(f"getelementptr ptr, ptr {data}, i64 {pos}")
                fast_val = gen.operation(f"load ptr, ptr {slot}, !nonnull !{{}}")
                res_tn = vop.results[0].tn
                res_type = res_tn.type or UNKNOWN
                unchecked_contract = gen._has_unchecked_element_contract(res_tn)
                immortal_elem = not unchecked_contract and (
                    not res_tn.sc.owned or res_type.is_fixnum or res_type.is_immortal
                )
                if vop.codegen_info != "storage_borrow" and not immortal_elem:
                    if not unchecked_contract and res_type.is_heap_object:
                        gen.heap_values.add(fast_val)
                    fast_val = gen.retain(fast_val)
                read_end = gen.current_block()
                gen.line(f"br label %{done_lbl}")
                gen.lines.append(slow_lbl + ":")
                slow_val = gen.operation(
                    f"call ptr @mule_storage_read(ptr {sequence}, ptr {index})"
                )
                slow_end = gen.current_block()
                gen.line(f"br label %{done_lbl}")
                gen.lines.append(done_lbl + ":")
                return gen.operation(
                    f"phi ptr [{fast_val}, %{read_end}], [{slow_val}, %{slow_end}]"
                )
            if (
                function == "mule_storage_write"
                and arg_type.is_exact_list
                and len(vop.args) == 3
                and (vop.args[1].tn.type or UNKNOWN).is_fixnum
            ):
                sequence, index, replacement = values[0], values[1], values[2]
                idx_type = vop.args[1].tn.type or UNKNOWN
                val_type = vop.args[2].tn.type or UNKNOWN
                elem_type = arg_type.iterate_type().next_item_type()
                non_negative = idx_type.minimum is not None and idx_type.minimum >= 0
                raw = gen.decode(index)
                len_addr = gen.operation(
                    f"getelementptr i64, ptr {sequence}, i64 {VALUE_BUFFER_LENGTH_OFFSET // 8}"
                )
                length = gen.operation(f"load i64, ptr {len_addr}")
                if non_negative:
                    pos = raw
                else:
                    neg = gen.operation(f"icmp slt i64 {raw}, 0")
                    wrapped = gen.operation(f"add i64 {raw}, {length}")
                    pos = gen.operation(f"select i1 {neg}, i64 {wrapped}, i64 {raw}")
                inside = gen.operation(f"icmp ult i64 {pos}, {length}")
                write_lbl = gen.fresh("storage.write")[1:]
                slow_lbl = gen.fresh("storage.slow")[1:]
                done_lbl = gen.fresh("storage.done")[1:]
                gen.line(
                    f"br i1 {inside}, label %{write_lbl}, label %{slow_lbl}, "
                    f'!prof !{{!"branch_weights", i32 2000, i32 1}}'
                )
                gen.lines.append(write_lbl + ":")
                data_addr = gen.operation(
                    f"getelementptr ptr, ptr {sequence}, i64 {VALUE_BUFFER_DATA_OFFSET // 8}"
                )
                data = gen.operation(f"load ptr, ptr {data_addr}")
                slot = gen.operation(f"getelementptr ptr, ptr {data}, i64 {pos}")
                old_val = gen.operation(f"load ptr, ptr {slot}, !nonnull !{{}}")
                if val_type.is_fixnum or val_type.is_immortal:
                    owned_val = replacement
                else:
                    if val_type.is_heap_object:
                        gen.heap_values.add(replacement)
                    owned_val = gen.retain(replacement)
                gen.line(f"store ptr {owned_val}, ptr {slot}")
                if not (
                    val_type.is_fixnum
                    or val_type.is_immortal
                    or val_type.fits_text(val_type)
                ):
                    gen.line(
                        f"call void @mule_storage_track(ptr {sequence}, ptr {replacement})"
                    )
                if not (elem_type.is_fixnum or elem_type.is_immortal):
                    gen.release(old_val)
                write_end = gen.current_block()
                gen.line(f"br label %{done_lbl}")
                gen.lines.append(slow_lbl + ":")
                slow_val = gen.operation(
                    f"call ptr @mule_storage_write(ptr {sequence}, ptr {index}, ptr {replacement})"
                )
                slow_end = gen.current_block()
                gen.line(f"br label %{done_lbl}")
                gen.lines.append(done_lbl + ":")
                return gen.operation(
                    f"phi ptr [@mule.none, %{write_end}], [{slow_val}, %{slow_end}]"
                )
            helper = storage_helper
            if function == "mule_storage_read" and vop.results:
                res_tn = vop.results[0].tn
                res_type = res_tn.type or UNKNOWN
                immortal_elem = not gen._has_unchecked_element_contract(res_tn) and (
                    not res_tn.sc.owned or res_type.is_fixnum or res_type.is_immortal
                )
                if vop.codegen_info == "storage_borrow" or immortal_elem:
                    helper = "@mule.fast.storage.read.borrow"
            operands = ", ".join(f"ptr {value}" for value in values)
            return gen.operation(f"call ptr {helper}({operands})")
        if function == "mule_integer_storage":
            if arg_type.is_fixnum:
                return values[0]
            return gen.operation(
                f"call ptr @mule.fast.integer_storage(ptr {values[0]})"
            )
        if function == "mule_text_storage" and arg_type.fits_text(arg_type):
            return "@mule.true"
        if function == "mule_text_codepoint" and len(vop.args) == 2:
            index_type = vop.args[1].tn.type or UNKNOWN
            if index_type.is_fixnum and (index_type.minimum or 0) >= 0:
                index = gen.decode(values[1])
                return gen.operation(
                    f"call ptr @mule_text_codepoint_at(ptr {values[0]}, i64 {index})"
                )
        operands = ", ".join(f"ptr {value}" for value in values)
        return gen.operation(f"call ptr @{function}({operands})")

    return emit


def lookup_method(name, *, include_object_defaults=False):
    entry = (
        "mule_lookup_special_including_defaults"
        if include_object_defaults
        else "mule_lookup_special"
    )

    def emit(gen, vop, values):
        if name == "__index__":
            if (vop.args[0].tn.type or UNKNOWN).is_fixnum:
                return "@mule.none"
            return gen.operation(
                f"call ptr @mule.fast.index_method(ptr {values[0]}, ptr {gen.module.text(name)})"
            )
        return gen.operation(
            f"call ptr @{entry}(ptr {values[0]}, ptr {gen.module.text(name)})"
        )

    return emit


def render_text(representation):
    def emit(gen, vop, values):
        return gen.operation(
            f"call ptr @mule_render_text(ptr {values[0]}, i32 {int(representation)})"
        )

    return emit


def text_result(gen, vop, values):
    return gen.operation(f"call ptr @mule_text_result(ptr {values[0]})")


def compiled_call(name):
    def emit(gen, vop, values):
        return gen.compiled_call_named(name, values)

    return emit


def numeric_modulo(gen, vop, values):
    return gen.operation(
        f"call ptr @mule.fast.binary.5(ptr {values[0]}, ptr {values[1]}, i32 0, ptr null, ptr null)"
    )


def allocate(gen, vop, values):
    return gen.operation(f"call ptr @mule_native_new(ptr {values[0]})")


def initialize(gen, vop, values):
    layout = vop.codegen_info
    return gen.compiled_call(layout.methods["__init__"], values)


def invalid_initializer(gen, vop, values):
    gen.line("call i32 @mule_native_initializer_error()")
    return "null"


def read_int64(gen, vop, values):
    payload = gen.operation(f"call ptr @mule_native_payload(ptr {values[0]})")
    gen.check_pointer(payload)
    index = gen.decode(values[1])
    address = gen.operation(f"getelementptr i64, ptr {payload}, i64 {index}")
    value = gen.operation(f"load i64, ptr {address}")
    return gen.operation(f"call ptr @{INT64_BOX_LAYOUT.symbol()}(i64 {value})")


def write_int64(gen, vop, values):
    payload = gen.operation(f"call ptr @mule_native_payload(ptr {values[0]})")
    gen.check_pointer(payload)
    fact = vop.args[2].tn.type or UNKNOWN
    if fact.is_fixnum or vop.args[2].tn.primitive_type is BOXED_I64:
        value = gen.operation(
            f"call i64 @{INT64_BOX_LAYOUT.symbol('load')}(ptr {values[2]})"
        )
    else:
        slot = gen.operation("alloca i64")
        status = gen.operation(f"call i32 @mule_unbox_i64(ptr {values[2]}, ptr {slot})")
        gen.check_status(status)
        value = gen.operation(f"load i64, ptr {slot}")
    index = gen.decode(values[1])
    address = gen.operation(f"getelementptr i64, ptr {payload}, i64 {index}")
    gen.line(f"store i64 {value}, ptr {address}")
    return "@mule.none"


# Raw templates: native bodies over machine representations.

RAW_SCALAR = (SIGNED, BOOLEAN, NULL, DOUBLE)


def _raw_binary(operator, opcode):
    @define_vop(
        f"raw-binary-{operator.name.lower()}",
        translate=fndb.BINARY[operator],
        also_translate=(fndb.INPLACE_BINARY[operator],),
        args=((SIGNED,), (SIGNED,)),
        results=((SIGNED,),),
        ltn_policy=RAW,
    )
    def generator(gen, vop):
        left, right = gen.take(vop.args[0]), gen.take(vop.args[1])
        gen.store(vop.results[0], gen.arithmetic(opcode, left, right))


for _operator, _opcode in _RAW_BINARY.items():
    _raw_binary(_operator, _opcode)


def _raw_float_binary(operator, opcode, restriction):
    """Float arithmetic over ``double`` operands, or a mix with one ``i64``
    operand that is converted first, as Python converts the integer."""
    suffix = "-".join(sc.name for sc in restriction)

    @define_vop(
        f"raw-float-{operator.name.lower()}-{suffix}",
        translate=fndb.BINARY[operator],
        also_translate=(fndb.INPLACE_BINARY[operator],),
        args=tuple((sc,) for sc in restriction),
        results=((DOUBLE,),),
        cost=1 if restriction == (DOUBLE, DOUBLE) else 2,
        ltn_policy=RAW,
    )
    def generator(gen, vop):
        left, right = (gen.double(operand) for operand in vop.args)
        gen.store(vop.results[0], gen.float_arithmetic(opcode, left, right))


_MIXED = ((DOUBLE, DOUBLE), (SIGNED, DOUBLE), (DOUBLE, SIGNED))
for _operator, _opcode in _RAW_FLOAT_BINARY.items():
    for _restriction in _MIXED:
        _raw_float_binary(_operator, _opcode, _restriction)


@define_vop(
    "raw-not",
    translate=fndb.UNARY[ast.UnaryOperator.NOT],
    args=((SIGNED, BOOLEAN, DOUBLE),),
    results=((BOOLEAN,),),
    ltn_policy=RAW,
)
def raw_not(gen, vop):
    operand = vop.args[0]
    truth = gen.truth(operand.tn, gen.take(operand))
    gen.store(vop.results[0], gen.operation(f"xor i1 {truth}, true"))


@define_vop(
    "raw-positive",
    translate=fndb.UNARY[ast.UnaryOperator.POSITIVE],
    args=((SIGNED,),),
    results=((SIGNED,),),
    ltn_policy=RAW,
)
def raw_positive(gen, vop):
    gen.store(vop.results[0], gen.take(vop.args[0]))


@define_vop(
    "raw-negative",
    translate=fndb.UNARY[ast.UnaryOperator.NEGATIVE],
    args=((SIGNED,),),
    results=((SIGNED,),),
    ltn_policy=RAW,
)
def raw_negative(gen, vop):
    gen.store(vop.results[0], gen.arithmetic("sub", "0", gen.take(vop.args[0])))


@define_vop(
    "raw-float-positive",
    translate=fndb.UNARY[ast.UnaryOperator.POSITIVE],
    args=((DOUBLE,),),
    results=((DOUBLE,),),
    cost=1,
    ltn_policy=RAW,
)
def raw_float_positive(gen, vop):
    gen.store(vop.results[0], gen.take(vop.args[0]))


@define_vop(
    "raw-float-negative",
    translate=fndb.UNARY[ast.UnaryOperator.NEGATIVE],
    args=((DOUBLE,),),
    results=((DOUBLE,),),
    cost=1,
    ltn_policy=RAW,
)
def raw_float_negative(gen, vop):
    gen.store(vop.results[0], gen.operation(f"fneg double {gen.take(vop.args[0])}"))


@define_vop(
    "raw-invert",
    translate=fndb.UNARY[ast.UnaryOperator.INVERT],
    args=((SIGNED,),),
    results=((SIGNED,),),
    ltn_policy=RAW,
)
def raw_invert(gen, vop):
    value = gen.take(vop.args[0])
    gen.store(vop.results[0], gen.operation(f"xor i64 {value}, -1"))


def _raw_compare(operator, predicate):
    @define_vop(
        f"raw-compare-{operator.name.lower()}",
        translate=fndb.COMPARE[operator],
        args=((SIGNED,), (SIGNED,)),
        results=((BOOLEAN,),),
        ltn_policy=RAW,
    )
    def generator(gen, vop):
        left, right = gen.take(vop.args[0]), gen.take(vop.args[1])
        gen.store(
            vop.results[0], gen.operation(f"icmp {predicate} i64 {left}, {right}")
        )


for _operator, _predicate in _RAW_COMPARE.items():
    _raw_compare(_operator, _predicate)


def _raw_float_compare(operator, predicate):
    @define_vop(
        f"raw-float-compare-{operator.name.lower()}",
        translate=fndb.COMPARE[operator],
        args=((DOUBLE,), (DOUBLE,)),
        results=((BOOLEAN,),),
        cost=1,
        ltn_policy=RAW,
    )
    def generator(gen, vop):
        left, right = (gen.take(operand) for operand in vop.args)
        gen.store(
            vop.results[0], gen.operation(f"fcmp {predicate} double {left}, {right}")
        )


for _operator, _predicate in _RAW_FLOAT_COMPARE.items():
    _raw_float_compare(_operator, _predicate)

# Python compares an integer with a float exactly, never by converting the
# integer, so a mixed comparison orders the pair through a helper that returns
# -1, 0, 1, or 2 (unordered) and tests the code it needs.
_ORDER_TESTS = {
    ast.ComparisonOperator.EQUAL: "icmp eq i32 {code}, 0",
    ast.ComparisonOperator.NOT_EQUAL: "icmp ne i32 {code}, 0",
    ast.ComparisonOperator.LESS: "icmp eq i32 {code}, -1",
    ast.ComparisonOperator.LESS_EQUAL: "icmp sle i32 {code}, 0",
    ast.ComparisonOperator.GREATER: "icmp eq i32 {code}, 1",
    ast.ComparisonOperator.GREATER_EQUAL: "icmp ult i32 {code}, 2",
}
_SWAPPED = {
    ast.ComparisonOperator.EQUAL: ast.ComparisonOperator.EQUAL,
    ast.ComparisonOperator.NOT_EQUAL: ast.ComparisonOperator.NOT_EQUAL,
    ast.ComparisonOperator.LESS: ast.ComparisonOperator.GREATER,
    ast.ComparisonOperator.LESS_EQUAL: ast.ComparisonOperator.GREATER_EQUAL,
    ast.ComparisonOperator.GREATER: ast.ComparisonOperator.LESS,
    ast.ComparisonOperator.GREATER_EQUAL: ast.ComparisonOperator.LESS_EQUAL,
}


def _raw_mixed_compare(operator, restriction):
    integer_first = restriction[0] is SIGNED
    tested = operator if integer_first else _SWAPPED[operator]

    @define_vop(
        f"raw-mixed-compare-{operator.name.lower()}-{restriction[0].name}",
        translate=fndb.COMPARE[operator],
        args=tuple((sc,) for sc in restriction),
        results=((BOOLEAN,),),
        cost=2,
        ltn_policy=RAW,
    )
    def generator(gen, vop):
        left, right = (gen.take(operand) for operand in vop.args)
        integer, double = (left, right) if integer_first else (right, left)
        code = gen.operation(
            f"call i32 @mule.compare.i64.double(i64 {integer}, double {double})"
        )
        gen.store(vop.results[0], gen.operation(_ORDER_TESTS[tested].format(code=code)))


for _operator in _RAW_FLOAT_COMPARE:
    _raw_mixed_compare(_operator, (SIGNED, DOUBLE))
    _raw_mixed_compare(_operator, (DOUBLE, SIGNED))


@define_vop(
    "raw-getattr",
    translate=fndb.GETATTR,
    args=((INSTANCE,),),
    results=((SIGNED,),),
    ltn_policy=RAW,
)
def raw_getattr(gen, vop):
    receiver = vop.args[0]
    address = gen.field_address(receiver.tn, gen.take(receiver), vop.codegen_info.name)
    gen.store(vop.results[0], gen.operation(f"load i64, ptr {address}"))


@define_vop(
    "raw-setattr",
    translate=fndb.SETATTR,
    args=((INSTANCE,), (SIGNED,)),
    ltn_policy=RAW,
)
def raw_setattr(gen, vop):
    receiver, value = vop.args
    address = gen.field_address(receiver.tn, gen.take(receiver), vop.codegen_info.name)
    gen.line(f"store i64 {gen.take(value)}, ptr {address}")


@define_vop(
    "raw-method-attribute",
    translate=fndb.GETATTR,
    args=((INSTANCE,),),
    results=((REFERENCE,),),
    ltn_policy=RAW,
)
def raw_method_attribute(gen, vop):
    value = gen.bind_method(gen.take(vop.args[0]), vop.codegen_info.name)
    gen.line(f"store ptr {value}, ptr {gen.owner_slot(vop.node)}")
    gen.check_pointer(value)
    gen.store(vop.results[0], value)


@define_vop(
    "raw-resolve-method",
    translate=fndb.RESOLVE_METHOD,
    args=((INSTANCE,),),
    results=((INSTANCE,), None),
    ltn_policy=RAW,
)
def raw_resolve_method(gen, vop):
    gen.store(vop.results[0], gen.take(vop.args[0]))


@define_vop(
    "raw-call-resolved",
    translate=fndb.CALL_RESOLVED,
    results=((*RAW_SCALAR, INSTANCE, CLASS, REFERENCE),),
    ltn_policy=RAW,
)
def raw_call_resolved(gen, vop):
    method, _bound, *arguments = vop.args
    target = gen.plan.method_target(vop.node.args[0])
    plan = gen.module.native_plans[target]
    gen.line("store i1 false, ptr %call.error")
    parameters = ["ptr %globals", "ptr %call.error", f"ptr {gen.take(method)}"]
    parameters.extend(
        f"{argument.tn.sc.llvm_type} {gen.take(argument)}" for argument in arguments
    )
    parameters.extend(
        f"ptr {gen.dependencies[variable]}" for variable in plan.dependencies
    )
    result = gen.operation(
        f"call {plan.returned.llvm_type} {plan.entry}({', '.join(parameters)})"
    )
    if plan.returned.owns_result:
        gen.line(f"store ptr {result}, ptr {gen.owner_slot(vop.node)}")
    failed = gen.operation("load i1, ptr %call.error")
    gen.guard(failed)
    gen.store(vop.results[0], result)


@define_vop("raw-construct", results=((INSTANCE,),), ltn_policy=RAW)
def raw_construct(gen, vop):
    """Allocate a native instance through its compiled constructor and keep
    the owning reference in the body's owner slot."""
    callee, *arguments = vop.args
    allocation = gen.plan.allocation(vop.node)
    boxed = [gen.box(argument.tn, gen.take(argument)) for argument in arguments]
    owned_class = gen.operation(f"call ptr @mule.fast.retain(ptr {gen.take(callee)})")
    parameters = [
        "ptr %globals",
        f"ptr {owned_class}",
        *(f"ptr {value}" for value in boxed),
    ]
    body = gen.module.bodies[allocation.layout.protocols["construct"]]
    instance = gen.operation(f"call ptr {body}({', '.join(parameters)})")
    gen.line(f"store ptr {instance}, ptr {gen.owner_slot(vop.node)}")
    gen.check_pointer(instance)
    gen.store(vop.results[0], instance)


@define_vop(
    "raw-min2",
    translate=fndb.MIN2,
    args=((SIGNED,), (SIGNED,)),
    results=((SIGNED,),),
    ltn_policy=RAW,
)
def raw_min2(gen, vop):
    left, right = gen.take(vop.args[0]), gen.take(vop.args[1])
    cmp = gen.operation(f"icmp sgt i64 {left}, {right}")
    gen.store(
        vop.results[0], gen.operation(f"select i1 {cmp}, i64 {right}, i64 {left}")
    )


@define_vop(
    "raw-max2",
    translate=fndb.MAX2,
    args=((SIGNED,), (SIGNED,)),
    results=((SIGNED,),),
    ltn_policy=RAW,
)
def raw_max2(gen, vop):
    left, right = gen.take(vop.args[0]), gen.take(vop.args[1])
    cmp = gen.operation(f"icmp slt i64 {left}, {right}")
    gen.store(
        vop.results[0], gen.operation(f"select i1 {cmp}, i64 {right}, i64 {left}")
    )


@define_vop(
    "raw-float-min2",
    translate=fndb.MIN2,
    args=((DOUBLE,), (DOUBLE,)),
    results=((DOUBLE,),),
    cost=1,
    ltn_policy=RAW,
)
def raw_float_min2(gen, vop):
    left, right = gen.take(vop.args[0]), gen.take(vop.args[1])
    cmp = gen.operation(f"fcmp ogt double {left}, {right}")
    gen.store(
        vop.results[0], gen.operation(f"select i1 {cmp}, double {right}, double {left}")
    )


@define_vop(
    "raw-float-max2",
    translate=fndb.MAX2,
    args=((DOUBLE,), (DOUBLE,)),
    results=((DOUBLE,),),
    cost=1,
    ltn_policy=RAW,
)
def raw_float_max2(gen, vop):
    left, right = gen.take(vop.args[0]), gen.take(vop.args[1])
    cmp = gen.operation(f"fcmp olt double {left}, {right}")
    gen.store(
        vop.results[0], gen.operation(f"select i1 {cmp}, double {right}, double {left}")
    )


@define_vop(
    "raw-math-sqrt",
    translate=fndb.MATH_SQRT,
    args=((DOUBLE, SIGNED),),
    results=((DOUBLE,),),
    cost=1,
    ltn_policy=RAW,
)
def raw_math_sqrt(gen, vop):
    value = gen.double(vop.args[0])
    gen.store(vop.results[0], gen.float_sqrt(value))


# Runtime helper templates shared by the object templates.

INTRINSICS = """declare {i64, i1} @llvm.sadd.with.overflow.i64(i64, i64)
declare {i64, i1} @llvm.ssub.with.overflow.i64(i64, i64)
declare {i64, i1} @llvm.smul.with.overflow.i64(i64, i64)
declare double @llvm.sqrt.f64(double)
@mule.true = external global i8, align 8
@mule.false = external global i8, align 8
@mule.none = external global i8, align 8
@mule.undefined = external global i8, align 8
"""


# Byte offsets of runtime object storage that compiled attribute sites read;
# `mule/runtime/object.rs` defines them and its tests check them.
DATA_OFFSET = 16
TEXT_TAG = 4
TEXT_KIND_TAG_OFFSET = 24
TEXT_INLINE_LEN_OFFSET = 25
TEXT_INLINE_WORDS_OFFSET = 32
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


def _attribute_cache_helpers() -> str:
    """Inline attribute-cache probes, the compiled half of the runtime's
    per-site caches, as SBCL's PCL emits inline cache checks before a
    generic function's slow path.

    ``@mule.attr.layout`` returns the receiver's class layout address when
    the receiver is a plain instance without an instance ``__dict__``, or
    zero. ``@mule.attr.hit`` returns the cached encoding when the site's
    cache matches that layout and the current attribute epoch, or zero;
    a zero layout never matches because caches store zero for entries that
    are not keyed by one layout."""
    return f"""@mule_attribute_epoch = external global i64

define internal i64 @mule.attr.layout(ptr %value) alwaysinline {{
entry:
  %bits = ptrtoint ptr %value to i64
  %tag = and i64 %bits, 1
  %integer = icmp ne i64 %tag, 0
  %null = icmp eq ptr %value, null
  %skip = or i1 %integer, %null
  br i1 %skip, label %none, label %object
object:
  %tag.addr = getelementptr i8, ptr %value, i64 {DATA_OFFSET}
  %kind = load i32, ptr %tag.addr
  %instance = icmp eq i32 %kind, {INSTANCE_TAG}
  br i1 %instance, label %fields, label %none
fields:
  %attrs.addr = getelementptr i8, ptr %value, i64 {INSTANCE_ATTRIBUTES_OFFSET}
  %attrs = load ptr, ptr %attrs.addr
  %plain = icmp eq ptr %attrs, null
  br i1 %plain, label %owner, label %none
owner:
  %class.addr = getelementptr i8, ptr %value, i64 {INSTANCE_CLASS_OFFSET}
  %class = load ptr, ptr %class.addr
  %layout.addr = getelementptr i8, ptr %class, i64 {CLASS_LAYOUT_OFFSET}
  %layout = load i64, ptr %layout.addr
  ret i64 %layout
none:
  ret i64 0
}}

define internal i64 @mule.attr.hit(ptr %value, ptr %cache) alwaysinline {{
entry:
  %layout = call i64 @mule.attr.layout(ptr %value)
  %cached = load i64, ptr %cache
  %same = icmp eq i64 %layout, %cached
  %known = icmp ne i64 %layout, 0
  %match = and i1 %same, %known
  br i1 %match, label %epoch, label %miss
epoch:
  %epoch.addr = getelementptr i64, ptr %cache, i64 2
  %cache.epoch = load i64, ptr %epoch.addr
  %current = load atomic i64, ptr @mule_attribute_epoch monotonic, align 8
  %fresh = icmp eq i64 %cache.epoch, %current
  br i1 %fresh, label %hit, label %miss
hit:
  %encoded.addr = getelementptr i64, ptr %cache, i64 1
  %encoded = load i64, ptr %encoded.addr
  ret i64 %encoded
miss:
  ret i64 0
}}

define internal i64 @mule.class.layout(ptr %value) alwaysinline {{
entry:
  %bits = ptrtoint ptr %value to i64
  %tag = and i64 %bits, 1
  %integer = icmp ne i64 %tag, 0
  %null = icmp eq ptr %value, null
  %skip = or i1 %integer, %null
  br i1 %skip, label %none, label %object
object:
  %tag.addr = getelementptr i8, ptr %value, i64 {DATA_OFFSET}
  %kind = load i32, ptr %tag.addr
  %class = icmp eq i32 %kind, {CLASS_TAG}
  br i1 %class, label %layout, label %none
layout:
  %layout.addr = getelementptr i8, ptr %value, i64 {CLASS_LAYOUT_OFFSET}
  %address = load i64, ptr %layout.addr
  ret i64 %address
none:
  ret i64 0
}}

define internal ptr @mule.attr.field(ptr %value, i64 %encoded) alwaysinline {{
entry:
  %fields.addr = getelementptr i8, ptr %value, i64 {INSTANCE_FIELDS_OFFSET}
  %fields = load ptr, ptr %fields.addr
  %index = lshr i64 %encoded, {ATTR_SHIFT}
  %slot = getelementptr ptr, ptr %fields, i64 %index
  ret ptr %slot
}}

define internal ptr @mule.fast.isinstance.1(ptr %value, ptr %class) alwaysinline {{
entry:
  %bits = ptrtoint ptr %value to i64
  %tag = and i64 %bits, 1
  %integer = icmp ne i64 %tag, 0
  %null = icmp eq ptr %value, null
  %skip = or i1 %integer, %null
  br i1 %skip, label %slow, label %object
object:
  %tag.addr = getelementptr i8, ptr %value, i64 {DATA_OFFSET}
  %kind = load i32, ptr %tag.addr
  %kind.off = sub i32 %kind, {INSTANCE_TAG}
  %is.inst = icmp ult i32 %kind.off, 2
  br i1 %is.inst, label %inst, label %slow
inst:
  %actual.addr = getelementptr i8, ptr %value, i64 {INSTANCE_CLASS_OFFSET}
  %actual = load ptr, ptr %actual.addr
  %exact = icmp eq ptr %actual, %class
  br i1 %exact, label %hit, label %slow
hit:
  ret ptr @mule.true
slow:
  %res = call ptr @mule_isinstance_single(ptr %value, ptr %class)
  ret ptr %res
}}"""


def _storage_helpers() -> str:
    """Physical vector VOPs, following SBCL's data-vector-ref/set.

    The runtime's authoritative buffer header exposes its data and length.
    Mutable header loads stay local to each operation, before any destructor.
    """
    return f"""define internal i32 @mule.storage.kind(ptr %sequence) alwaysinline {{
entry:
  %bits = ptrtoint ptr %sequence to i64
  %tag = and i64 %bits, 1
  %fixnum = icmp ne i64 %tag, 0
  %null = icmp eq ptr %sequence, null
  %invalid = or i1 %fixnum, %null
  br i1 %invalid, label %unsupported, label %object
object:
  %kind.addr = getelementptr i8, ptr %sequence, i64 {DATA_OFFSET}
  %kind = load i32, ptr %kind.addr
  ret i32 %kind
unsupported:
  ret i32 0
}}

define internal ptr @mule.fast.storage.read.borrow(ptr %sequence, ptr %position) alwaysinline {{
entry:
  %bits = ptrtoint ptr %position to i64
  %tag = and i64 %bits, 1
  %fixnum = icmp ne i64 %tag, 0
  br i1 %fixnum, label %check.kind, label %slow
check.kind:
  %kind = call i32 @mule.storage.kind(ptr %sequence)
  %list = icmp eq i32 %kind, {LIST_TAG}
  %tuple = icmp eq i32 %kind, {ARGS_TAG}
  %supported = or i1 %list, %tuple
  br i1 %supported, label %bounds, label %slow
bounds:
  %length.addr = getelementptr i8, ptr %sequence, i64 {VALUE_BUFFER_LENGTH_OFFSET}
  %length = load i64, ptr %length.addr
  %index = ashr i64 %bits, 1
  %inside = icmp ult i64 %index, %length
  br i1 %inside, label %read, label %slow
read:
  %data.addr = getelementptr i8, ptr %sequence, i64 {VALUE_BUFFER_DATA_OFFSET}
  %data = load ptr, ptr %data.addr
  %slot = getelementptr ptr, ptr %data, i64 %index
  %value = load ptr, ptr %slot
  ret ptr %value
slow:
  %fallback = call ptr @mule_storage_read(ptr %sequence, ptr %position)
  ret ptr %fallback
}}

define internal ptr @mule.fast.storage.read(ptr %sequence, ptr %position) alwaysinline {{
entry:
  %bits = ptrtoint ptr %position to i64
  %tag = and i64 %bits, 1
  %fixnum = icmp ne i64 %tag, 0
  br i1 %fixnum, label %check.kind, label %slow
check.kind:
  %kind = call i32 @mule.storage.kind(ptr %sequence)
  %list = icmp eq i32 %kind, {LIST_TAG}
  %tuple = icmp eq i32 %kind, {ARGS_TAG}
  %supported = or i1 %list, %tuple
  br i1 %supported, label %bounds, label %slow
bounds:
  %length.addr = getelementptr i8, ptr %sequence, i64 {VALUE_BUFFER_LENGTH_OFFSET}
  %length = load i64, ptr %length.addr
  %index = ashr i64 %bits, 1
  %inside = icmp ult i64 %index, %length
  br i1 %inside, label %read, label %slow
read:
  %data.addr = getelementptr i8, ptr %sequence, i64 {VALUE_BUFFER_DATA_OFFSET}
  %data = load ptr, ptr %data.addr
  %slot = getelementptr ptr, ptr %data, i64 %index
  %value = load ptr, ptr %slot
  %owned = call ptr @mule.fast.retain(ptr %value)
  ret ptr %owned
slow:
  %fallback = call ptr @mule_storage_read(ptr %sequence, ptr %position)
  ret ptr %fallback
}}

define internal ptr @mule.fast.storage.write(ptr %sequence, ptr %position, ptr %replacement) alwaysinline {{
entry:
  %bits = ptrtoint ptr %position to i64
  %tag = and i64 %bits, 1
  %fixnum = icmp ne i64 %tag, 0
  br i1 %fixnum, label %check.kind, label %slow
check.kind:
  %kind = call i32 @mule.storage.kind(ptr %sequence)
  %supported = icmp eq i32 %kind, {LIST_TAG}
  br i1 %supported, label %bounds, label %slow
bounds:
  %length.addr = getelementptr i8, ptr %sequence, i64 {VALUE_BUFFER_LENGTH_OFFSET}
  %length = load i64, ptr %length.addr
  %index = ashr i64 %bits, 1
  %inside = icmp ult i64 %index, %length
  br i1 %inside, label %write, label %slow
write:
  %data.addr = getelementptr i8, ptr %sequence, i64 {VALUE_BUFFER_DATA_OFFSET}
  %data = load ptr, ptr %data.addr
  %slot = getelementptr ptr, ptr %data, i64 %index
  %old = load ptr, ptr %slot
  %owned = call ptr @mule.fast.retain(ptr %replacement)
  store ptr %owned, ptr %slot
  call void @mule_storage_track(ptr %sequence, ptr %replacement)
  call void @mule.fast.release(ptr %old)
  ret ptr @mule.none
slow:
  %fallback = call ptr @mule_storage_write(ptr %sequence, ptr %position, ptr %replacement)
  ret ptr %fallback
}}

define internal ptr @mule.fast.storage.length(ptr %sequence) alwaysinline {{
entry:
  %kind = call i32 @mule.storage.kind(ptr %sequence)
  %list = icmp eq i32 %kind, {LIST_TAG}
  %tuple = icmp eq i32 %kind, {ARGS_TAG}
  %supported = or i1 %list, %tuple
  br i1 %supported, label %length, label %slow
length:
  %length.addr = getelementptr i8, ptr %sequence, i64 {VALUE_BUFFER_LENGTH_OFFSET}
  %count = load i64, ptr %length.addr
  %shifted = shl i64 %count, 1
  %tagged = or i64 %shifted, 1
  %result = inttoptr i64 %tagged to ptr
  ret ptr %result
slow:
  %fallback = call ptr @mule_storage_length(ptr %sequence)
  ret ptr %fallback
}}

define internal ptr @mule.fast.tuple.borrow(ptr %tuple, i64 %index) alwaysinline {{
entry:
  %data.addr = getelementptr i8, ptr %tuple, i64 {VALUE_BUFFER_DATA_OFFSET}
  %data = load ptr, ptr %data.addr
  %slot = getelementptr ptr, ptr %data, i64 %index
  %value = load ptr, ptr %slot
  ret ptr %value
}}

define internal ptr @mule.fast.tuple.slot(ptr %tuple, i64 %index) alwaysinline {{
entry:
  %value = call ptr @mule.fast.tuple.borrow(ptr %tuple, i64 %index)
  %owned = call ptr @mule.fast.retain(ptr %value)
  ret ptr %owned
}}

define internal i1 @mule.fast.tuple.shape(ptr %value, i64 %arity) alwaysinline {{
entry:
  %bits = ptrtoint ptr %value to i64
  %tag = and i64 %bits, 1
  %fixnum = icmp ne i64 %tag, 0
  %null = icmp eq ptr %value, null
  %skip = or i1 %fixnum, %null
  br i1 %skip, label %no, label %object
object:
  %kind.addr = getelementptr i8, ptr %value, i64 {DATA_OFFSET}
  %kind = load i32, ptr %kind.addr
  %is.tuple = icmp eq i32 %kind, {ARGS_TAG}
  br i1 %is.tuple, label %check, label %no
check:
  %class.addr = getelementptr i8, ptr %value, i64 {SEQUENCE_CLASS_OFFSET}
  %class = load ptr, ptr %class.addr
  %plain = icmp eq ptr %class, null
  %len.addr = getelementptr i8, ptr %value, i64 {VALUE_BUFFER_LENGTH_OFFSET}
  %len = load i64, ptr %len.addr
  %len.ok = icmp eq i64 %len, %arity
  %ok = and i1 %plain, %len.ok
  ret i1 %ok
no:
  ret i1 false
}}"""


def _ownership() -> str:
    return """define internal ptr @mule.fast.retain(ptr %value) alwaysinline {
entry:
  %bits = ptrtoint ptr %value to i64
  %tag = and i64 %bits, 1
  %integer = icmp ne i64 %tag, 0
  %null = icmp eq ptr %value, null
  %skip = or i1 %integer, %null
  br i1 %skip, label %done, label %check
check:
  %refs = load i64, ptr %value
  %immortal = icmp eq i64 %refs, -1
  br i1 %immortal, label %done, label %heap
heap:
  atomicrmw add ptr %value, i64 1 monotonic
  br label %done
done:
  ret ptr %value
}

define internal void @mule.fast.release(ptr %value) alwaysinline {
entry:
  %bits = ptrtoint ptr %value to i64
  %tag = and i64 %bits, 1
  %integer = icmp ne i64 %tag, 0
  %null = icmp eq ptr %value, null
  %skip = or i1 %integer, %null
  br i1 %skip, label %done, label %check
check:
  %refs = load i64, ptr %value
  %immortal = icmp eq i64 %refs, -1
  br i1 %immortal, label %done, label %check_unique
check_unique:
  %unique = icmp eq i64 %refs, 1
  br i1 %unique, label %free, label %heap
heap:
  %prev = atomicrmw sub ptr %value, i64 1 release
  %last = icmp eq i64 %prev, 1
  br i1 %last, label %free, label %done
free:
  call void @mule_destroy(ptr %value)
  ret void
done:
  ret void
}

define internal ptr @mule.heap.retain(ptr %value) alwaysinline {
entry:
  %null = icmp eq ptr %value, null
  br i1 %null, label %done, label %check
check:
  %refs = load i64, ptr %value
  %immortal = icmp eq i64 %refs, -1
  br i1 %immortal, label %done, label %heap
heap:
  atomicrmw add ptr %value, i64 1 monotonic
  br label %done
done:
  ret ptr %value
}

define internal void @mule.heap.release(ptr %value) alwaysinline {
entry:
  %null = icmp eq ptr %value, null
  br i1 %null, label %done, label %check
check:
  %refs = load i64, ptr %value
  %immortal = icmp eq i64 %refs, -1
  br i1 %immortal, label %done, label %check_unique
check_unique:
  %unique = icmp eq i64 %refs, 1
  br i1 %unique, label %free, label %heap
heap:
  %prev = atomicrmw sub ptr %value, i64 1 release
  %last = icmp eq i64 %prev, 1
  br i1 %last, label %free, label %done
free:
  call void @mule_destroy(ptr %value)
  ret void
done:
  ret void
}

define internal ptr @mule.fast.index_method(ptr %value, ptr %name) alwaysinline {
entry:
  %bits = ptrtoint ptr %value to i64
  %tag = and i64 %bits, 1
  %integer = icmp ne i64 %tag, 0
  br i1 %integer, label %fixnum, label %slow
fixnum:
  ret ptr @mule.none
slow:
  %result = call ptr @mule_lookup_special(ptr %value, ptr %name)
  ret ptr %result
}

define internal ptr @mule.fast.integer_storage(ptr %value) alwaysinline {
entry:
  %bits = ptrtoint ptr %value to i64
  %tag = and i64 %bits, 1
  %integer = icmp ne i64 %tag, 0
  br i1 %integer, label %fixnum, label %slow
fixnum:
  ret ptr %value
slow:
  %result = call ptr @mule_integer_storage(ptr %value)
  ret ptr %result
}"""


def _truth() -> str:
    return """define internal i32 @mule.fast.truth(ptr %value) alwaysinline {
entry:
  %bits = ptrtoint ptr %value to i64
  %tag = and i64 %bits, 1
  %integer = icmp ne i64 %tag, 0
  br i1 %integer, label %fixnum, label %object
fixnum:
  %nonzero = icmp ne i64 %bits, 1
  %truth = zext i1 %nonzero to i32
  ret i32 %truth
object:
  %true = icmp eq ptr %value, @mule.true
  br i1 %true, label %yes, label %other
other:
  %false = icmp eq ptr %value, @mule.false
  %none = icmp eq ptr %value, @mule.none
  %zero = or i1 %false, %none
  br i1 %zero, label %no, label %slow
yes:
  ret i32 1
no:
  ret i32 0
slow:
  %result = call i32 @mule_truth(ptr %value)
  ret i32 %result
}

; A cell never holds NULL: an unbound name's cell holds the undefined object.
define internal ptr @mule.global.unchecked(ptr %cell, ptr %name) alwaysinline {
entry:
  %value = load ptr, ptr %cell, !nonnull !{}
  %owned = call ptr @mule.fast.retain(ptr %value)
  ret ptr %owned
}

define internal ptr @mule.global.checked(ptr %cell, ptr %name) alwaysinline {
entry:
  %value = load ptr, ptr %cell, !nonnull !{}
  %unbound = icmp eq ptr %value, @mule.undefined
  br i1 %unbound, label %slow, label %bound
bound:
  %owned = call ptr @mule.fast.retain(ptr %value)
  ret ptr %owned
slow:
  %result = call ptr @mule_load_global(ptr %cell, ptr %name)
  ret ptr %result
}

define internal ptr @mule.global.peek.live(ptr %cell, ptr %name) {
entry:
  %own.slot = getelementptr inbounds {ptr, i64, ptr, ptr}, ptr %cell, i64 0, i32 1
  %own = load i64, ptr %own.slot
  %bound = icmp ne i64 %own, 0
  br i1 %bound, label %module, label %builtin
module:
  %value = load ptr, ptr %cell, !nonnull !{}
  br label %found
builtin:
  %builtin.slot = getelementptr inbounds {ptr, i64, ptr, ptr}, ptr %cell, i64 0, i32 2
  %builtin.cell = load ptr, ptr %builtin.slot
  %builtin.missing = icmp eq ptr %builtin.cell, null
  br i1 %builtin.missing, label %slow, label %builtin.check
builtin.check:
  %builtin.own.slot = getelementptr inbounds {ptr, i64, ptr, ptr}, ptr %builtin.cell, i64 0, i32 1
  %builtin.own = load i64, ptr %builtin.own.slot
  %builtin.bound = icmp ne i64 %builtin.own, 0
  br i1 %builtin.bound, label %builtin.value, label %slow
builtin.value:
  %live = load ptr, ptr %builtin.cell, !nonnull !{}
  br label %found
found:
  %result.value = phi ptr [%value, %module], [%live, %builtin.value]
  %owned = call ptr @mule.fast.retain(ptr %result.value)
  ret ptr %owned
slow:
  ret ptr @mule.undefined
}

define internal ptr @mule.global.live(ptr %cell, ptr %name) {
entry:
  %value = call ptr @mule.global.peek.live(ptr %cell, ptr %name)
  %missing = icmp eq ptr %value, @mule.undefined
  br i1 %missing, label %slow, label %found
found:
  ret ptr %value
slow:
  %result = call ptr @mule_load_global(ptr %cell, ptr %name)
  ret ptr %result
}

define internal ptr @mule.fast.local(ptr %value, ptr %name) alwaysinline {
entry:
  %missing = icmp eq ptr %value, null
  br i1 %missing, label %slow, label %loaded
loaded:
  %owned = call ptr @mule.fast.retain(ptr %value)
  ret ptr %owned
slow:
  %error = call ptr @mule_load_local(ptr null, ptr %name)
  ret ptr %error
}

define internal ptr @mule.fast.local.move(ptr %value, ptr %name) alwaysinline {
entry:
  %missing = icmp eq ptr %value, null
  br i1 %missing, label %slow, label %loaded
loaded:
  ret ptr %value
slow:
  %error = call ptr @mule_load_local(ptr null, ptr %name)
  ret ptr %error
}

define internal ptr @mule.fast.local.optional(ptr %value) alwaysinline {
entry:
  %missing = icmp eq ptr %value, null
  br i1 %missing, label %unbound, label %loaded
loaded:
  %owned = call ptr @mule.fast.retain(ptr %value)
  ret ptr %owned
unbound:
  %sentinel = call ptr @mule_binding_missing()
  ret ptr %sentinel
}"""


_ENCODE = """  %shifted = shl i64 %result, 1
  %tagged = or i64 %shifted, 1
  %value = inttoptr i64 %tagged to ptr
  ret ptr %value"""
_RANGE = """  %test.shifted = shl i64 %result, 1
  %test.decoded = ashr i64 %test.shifted, 1
  %range = icmp eq i64 %result, %test.decoded"""


def _binary_helper(op: int, *, fixnums: bool = False) -> str:
    if op == 2:
        operation = f"""  %checked = call {{i64, i1}} @llvm.smul.with.overflow.i64(i64 %a, i64 %b)
  %result = extractvalue {{i64, i1}} %checked, 0
  %overflow = extractvalue {{i64, i1}} %checked, 1
{_RANGE}
  %no.overflow = xor i1 %overflow, true
  %fits = and i1 %range, %no.overflow
  br i1 %fits, label %encode, label %slow"""
    elif op == 4:
        operation = f"""  %nonzero = icmp ne i64 %b, 0
  br i1 %nonzero, label %divide, label %slow
divide:
  %quotient = sdiv i64 %a, %b
  %remainder = srem i64 %a, %b
  %has.remainder = icmp ne i64 %remainder, 0
  %sign.bits = xor i64 %a, %b
  %different.signs = icmp slt i64 %sign.bits, 0
  %adjust = and i1 %has.remainder, %different.signs
  %correction = zext i1 %adjust to i64
  %result = sub i64 %quotient, %correction
{_RANGE}
  br i1 %range, label %encode, label %slow"""
    elif op == 5:
        operation = """  %nonzero = icmp ne i64 %b, 0
  br i1 %nonzero, label %divide, label %slow
divide:
  %remainder = srem i64 %a, %b
  %has.remainder = icmp ne i64 %remainder, 0
  %sign.bits = xor i64 %a, %b
  %different.signs = icmp slt i64 %sign.bits, 0
  %adjust = and i1 %has.remainder, %different.signs
  %adjusted.remainder = add i64 %remainder, %b
  %result = select i1 %adjust, i64 %adjusted.remainder, i64 %remainder
  br label %encode"""
    elif op == 8:
        operation = f"""  %nonnegative = icmp sge i64 %b, 0
  %bounded = icmp slt i64 %b, 63
  %valid = and i1 %nonnegative, %bounded
  br i1 %valid, label %shift, label %slow
shift:
  %result = shl i64 %a, %b
  %restored = ashr i64 %result, %b
  %no.overflow = icmp eq i64 %restored, %a
{_RANGE}
  %fits = and i1 %range, %no.overflow
  br i1 %fits, label %encode, label %slow"""
    elif op == 9:
        operation = """  %nonnegative = icmp sge i64 %b, 0
  br i1 %nonnegative, label %shift, label %slow
shift:
  %bounded = icmp slt i64 %b, 63
  %count = select i1 %bounded, i64 %b, i64 63
  %result = ashr i64 %a, %count
  br label %encode"""
    else:
        instruction = {10: "and", 11: "xor", 12: "or"}[op]
        operation = f"  %result = {instruction} i64 %a, %b\n  br label %encode"
    prefix = "fixnum" if fixnums else "fast"
    guard = (
        ""
        if fixnums
        else """  %tags = and i64 %left.bits, %right.bits
  %tag = and i64 %tags, 1
  %integers = icmp ne i64 %tag, 0
  br i1 %integers, label %fixnums, label %slow
fixnums:
"""
    )
    slow = (
        f"""  %has.slot = icmp ne ptr %reuse.slot, null
  br i1 %has.slot, label %slow.store, label %slow.call
slow.store:
  store ptr %reuse, ptr %reuse.slot
  br label %slow.call
slow.call:
  %fallback = call ptr @mule_binary_reuse(i32 {op}, ptr %left, ptr %right, i32 %inplace, ptr %reuse.slot)
  ret ptr %fallback"""
        if op == 2 and not fixnums
        else f"""  %fallback = call ptr @mule_binary_reuse(i32 {op}, ptr %left, ptr %right, i32 %inplace, ptr null)
  ret ptr %fallback"""
    )
    return f"""define internal ptr @mule.{prefix}.binary.{op}(ptr %left, ptr %right, i32 %inplace, ptr %reuse, ptr %reuse.slot) alwaysinline {{
entry:
  %left.bits = ptrtoint ptr %left to i64
  %right.bits = ptrtoint ptr %right to i64
{guard}  %a = ashr i64 %left.bits, 1
  %b = ashr i64 %right.bits, 1
{operation}
encode:
{_ENCODE}
slow:
{slow}
}}"""


def _unary_helper(op: int, *, fixnum: bool = False) -> str:
    if op == 3:
        return """define internal ptr @mule.fast.unary.3(ptr %operand) {
entry:
  %truth = call i32 @mule.fast.truth(ptr %operand)
  %error = icmp slt i32 %truth, 0
  br i1 %error, label %failed, label %done
done:
  %zero = icmp eq i32 %truth, 0
  %value = select i1 %zero, ptr @mule.true, ptr @mule.false
  ret ptr %value
failed:
  ret ptr null
}"""
    if op == 0:
        operation = "  ret ptr %operand"
    elif op == 1:
        operation = f"""  %minimum = icmp eq i64 %decoded, -4611686018427387904
  br i1 %minimum, label %slow, label %negate
negate:
  %result = sub i64 0, %decoded
{_ENCODE}"""
    else:
        operation = f"  %result = xor i64 %decoded, -1\n{_ENCODE}"
    prefix = "fixnum" if fixnum else "fast"
    guard = (
        ""
        if fixnum
        else """  %tag = and i64 %bits, 1
  %integer = icmp ne i64 %tag, 0
  br i1 %integer, label %fixnum, label %slow
fixnum:
"""
    )
    return f"""define internal ptr @mule.{prefix}.unary.{op}(ptr %operand) alwaysinline {{
entry:
  %bits = ptrtoint ptr %operand to i64
{guard}  %decoded = ashr i64 %bits, 1
{operation}
slow:
  %fallback = call ptr @mule_unary(i32 {op}, ptr %operand)
  ret ptr %fallback
}}"""


def _compare_helper(op: int) -> str:
    predicate = ("eq", "ne", "slt", "sle", "sgt", "sge", "eq", "ne")[op]
    operation = f"""  %compared = icmp {predicate} i64 %left.bits, %right.bits
  %value = select i1 %compared, ptr @mule.true, ptr @mule.false
  ret ptr %value"""
    if op in (6, 7):
        return f"""define internal ptr @mule.fast.compare.{op}(ptr %left, ptr %right) alwaysinline {{
entry:
  %left.bits = ptrtoint ptr %left to i64
  %right.bits = ptrtoint ptr %right to i64
{operation}
}}"""
    return f"""define internal ptr @mule.fast.compare.{op}(ptr %left, ptr %right) alwaysinline {{
entry:
  %left.bits = ptrtoint ptr %left to i64
  %right.bits = ptrtoint ptr %right to i64
  %tags = and i64 %left.bits, %right.bits
  %tag = and i64 %tags, 1
  %integers = icmp ne i64 %tag, 0
  br i1 %integers, label %fixnums, label %slow
fixnums:
{operation}
slow:
  %fallback = call ptr @mule_compare(i32 {op}, ptr %left, ptr %right)
  ret ptr %fallback
}}"""


def _compare_bool_helper(op: int) -> str:
    predicate = ("eq", "ne", "slt", "sle", "sgt", "sge")[op]
    return f"""define internal ptr @mule.fast.compare.bool.{op}(ptr %left, ptr %right) alwaysinline {{
entry:
  %left.bits = ptrtoint ptr %left to i64
  %right.bits = ptrtoint ptr %right to i64
  %tags = and i64 %left.bits, %right.bits
  %tag = and i64 %tags, 1
  %integers = icmp ne i64 %tag, 0
  br i1 %integers, label %fixnums, label %slow
fixnums:
  %compared = icmp {predicate} i64 %left.bits, %right.bits
  %value = select i1 %compared, ptr @mule.true, ptr @mule.false
  ret ptr %value
slow:
  %fallback = call ptr @mule_compare_bool(i32 {op}, ptr %left, ptr %right)
  ret ptr %fallback
}}"""


# Exact ordering of a machine integer against a binary64: doubles beyond the
# i64 range order by sign, and in-range doubles split into an integer part,
# which is compared as an integer, and a fraction that breaks ties.
MIXED_COMPARE = """define internal i32 @mule.compare.i64.double(i64 %integer, double %double) {
entry:
  %unordered = fcmp uno double %double, %double
  br i1 %unordered, label %nan, label %ordered
nan:
  ret i32 2
ordered:
  %above = fcmp oge double %double, 0x43E0000000000000
  br i1 %above, label %less, label %bounded.above
less:
  ret i32 -1
bounded.above:
  %below = fcmp olt double %double, 0xC3E0000000000000
  br i1 %below, label %greater, label %bounded
greater:
  ret i32 1
bounded:
  %truncated = fptosi double %double to i64
  %whole = sitofp i64 %truncated to double
  %fraction = fsub double %double, %whole
  %integer.less = icmp slt i64 %integer, %truncated
  %integer.equal = icmp eq i64 %integer, %truncated
  %fraction.positive = fcmp ogt double %fraction, 0.0
  %fraction.zero = fcmp oeq double %fraction, 0.0
  %tie.less = and i1 %integer.equal, %fraction.positive
  %is.less = or i1 %integer.less, %tie.less
  %is.equal = and i1 %integer.equal, %fraction.zero
  %sign = select i1 %is.less, i32 -1, i32 1
  %code = select i1 %is.equal, i32 0, i32 %sign
  ret i32 %code
}"""


LITERAL_HELPERS = """define internal ptr @mule.cached.float(ptr %slot, double %value) alwaysinline {
entry:
  %cached = load ptr, ptr %slot
  %present = icmp ne ptr %cached, null
  br i1 %present, label %ready, label %build
build:
  %built = call ptr @mule_literal_float(double %value)
  store ptr %built, ptr %slot
  br label %ready
ready:
  %result = phi ptr [%cached, %entry], [%built, %build]
  %owned = call ptr @mule.heap.retain(ptr %result)
  ret ptr %owned
}

define internal ptr @mule.cached.text(ptr %slot, ptr %data, i64 %length) alwaysinline {
entry:
  %cached = load ptr, ptr %slot
  %present = icmp ne ptr %cached, null
  br i1 %present, label %ready, label %build
build:
  %built = call ptr @mule_literal_text(ptr %data, i64 %length)
  store ptr %built, ptr %slot
  br label %ready
ready:
  %result = phi ptr [%cached, %entry], [%built, %build]
  %owned = call ptr @mule.heap.retain(ptr %result)
  ret ptr %owned
}

define internal ptr @mule.cached.bytes(ptr %slot, ptr %data, i64 %length) alwaysinline {
entry:
  %cached = load ptr, ptr %slot
  %present = icmp ne ptr %cached, null
  br i1 %present, label %ready, label %build
build:
  %built = call ptr @mule_literal_bytes(ptr %data, i64 %length)
  store ptr %built, ptr %slot
  br label %ready
ready:
  %result = phi ptr [%cached, %entry], [%built, %build]
  %owned = call ptr @mule.heap.retain(ptr %result)
  ret ptr %owned
}

define internal ptr @mule.cached.int(ptr %slot, ptr %integer) alwaysinline {
entry:
  %cached = load ptr, ptr %slot
  %present = icmp ne ptr %cached, null
  br i1 %present, label %ready, label %build
build:
  %built = call ptr @mule_literal_integer(ptr %integer)
  %bad = icmp eq ptr %built, null
  br i1 %bad, label %failed, label %built.ok
built.ok:
  store ptr %built, ptr %slot
  br label %ready
ready:
  %result = phi ptr [%cached, %entry], [%built, %built.ok]
  %owned = call ptr @mule.heap.retain(ptr %result)
  ret ptr %owned
failed:
  ret ptr null
}

define internal ptr @mule.cached.tuple(ptr %slot, i64 %count, ptr %items) alwaysinline {
entry:
  %cached = load ptr, ptr %slot
  %present = icmp ne ptr %cached, null
  br i1 %present, label %ready, label %build
build:
  %tuple = call ptr @mule_sequence_tuple_owned(i64 %count, ptr %items)
  store ptr %tuple, ptr %slot
  %bad = icmp eq ptr %tuple, null
  br i1 %bad, label %failed, label %ready
ready:
  %result = phi ptr [%cached, %entry], [%tuple, %build]
  %owned = call ptr @mule.heap.retain(ptr %result)
  ret ptr %owned
failed:
  ret ptr null
}

define internal ptr @mule.borrowed.float(ptr %slot, double %value) alwaysinline {
entry:
  %cached = load ptr, ptr %slot
  %present = icmp ne ptr %cached, null
  br i1 %present, label %ready, label %build
build:
  %built = call ptr @mule_literal_float(double %value)
  store ptr %built, ptr %slot
  br label %ready
ready:
  %result = phi ptr [%cached, %entry], [%built, %build]
  ret ptr %result
}

define internal ptr @mule.borrowed.text(ptr %slot, ptr %data, i64 %length) alwaysinline {
entry:
  %cached = load ptr, ptr %slot
  %present = icmp ne ptr %cached, null
  br i1 %present, label %ready, label %build
build:
  %built = call ptr @mule_literal_text(ptr %data, i64 %length)
  store ptr %built, ptr %slot
  br label %ready
ready:
  %result = phi ptr [%cached, %entry], [%built, %build]
  ret ptr %result
}

define internal ptr @mule.borrowed.bytes(ptr %slot, ptr %data, i64 %length) alwaysinline {
entry:
  %cached = load ptr, ptr %slot
  %present = icmp ne ptr %cached, null
  br i1 %present, label %ready, label %build
build:
  %built = call ptr @mule_literal_bytes(ptr %data, i64 %length)
  store ptr %built, ptr %slot
  br label %ready
ready:
  %result = phi ptr [%cached, %entry], [%built, %build]
  ret ptr %result
}

define internal ptr @mule.borrowed.int(ptr %slot, ptr %integer) alwaysinline {
entry:
  %cached = load ptr, ptr %slot
  %present = icmp ne ptr %cached, null
  br i1 %present, label %ready, label %build
build:
  %built = call ptr @mule_literal_integer(ptr %integer)
  store ptr %built, ptr %slot
  br label %ready
ready:
  %result = phi ptr [%cached, %entry], [%built, %build]
  ret ptr %result
}

define internal ptr @mule.borrowed.tuple(ptr %slot, i64 %count, ptr %items) alwaysinline {
entry:
  %cached = load ptr, ptr %slot
  %present = icmp ne ptr %cached, null
  br i1 %present, label %ready, label %build
build:
  %tuple = call ptr @mule_sequence_tuple_owned(i64 %count, ptr %items)
  store ptr %tuple, ptr %slot
  %bad = icmp eq ptr %tuple, null
  br i1 %bad, label %failed, label %ready
ready:
  %result = phi ptr [%cached, %entry], [%tuple, %build]
  ret ptr %result
failed:
  ret ptr null
}

define internal ptr @mule.borrowed.tuple.fn(ptr %slot, ptr %builder) alwaysinline {
entry:
  %cached = load ptr, ptr %slot
  %present = icmp ne ptr %cached, null
  br i1 %present, label %ready, label %build
build:
  %built = call ptr %builder()
  call void @mule.heap.release(ptr %built)
  br label %ready
ready:
  %result = phi ptr [%cached, %entry], [%built, %build]
  ret ptr %result
}"""


def helpers(*, bootstrap: bool = False) -> str:
    """Native guarded templates; LLVM removes unused templates after inlining."""
    return "\n\n".join(
        [
            INTRINSICS,
            MIXED_COMPARE,
            LITERAL_HELPERS,
            _ownership(),
            _storage_helpers(),
            *(layout.codec.helpers(layout) for layout in BOX_LAYOUTS),
            _truth(),
            _attribute_cache_helpers(),
            *(_binary_helper(op) for op in (2, 4, 5, 8, 9, 10, 11, 12)),
            *(_binary_helper(op, fixnums=True) for op in (2, 4, 5, 8, 9, 10, 11, 12)),
            *(_unary_helper(op) for op in range(4)),
            *(_unary_helper(op, fixnum=True) for op in range(3)),
            *(_compare_helper(op) for op in range(8)),
            *(_compare_bool_helper(op) for op in range(6)),
        ]
    )


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


INSERT_SAFEPOINT = insert_safepoint
CONSTANT = constant
LOAD_LOCAL = load_local
LOAD_GLOBAL = load_global
MOVE = move
COERCE = coerce
MOVE_ARGUMENT = move_argument
LOAD_CLOSURE = load_closure
LOCAL_ADDRESS = local_address
LOCAL_LOAD_CLOSURE = local_load_closure
LOCAL_CELL_REF = local_cell_ref
LOCAL_CELL_SET = local_cell_set
LOCAL_CELL_CLEAR = local_cell_clear
LOCAL_FUNCTION = local_function
LOCAL_EXTENT_CALL = local_extent_call
VALUE_CELL_REF = value_cell_ref
VALUE_CELL_SET = value_cell_set
VALUE_CELL_CLEAR = value_cell_clear
UNBIND_LOCAL = unbind_local
STORE_GLOBAL = store_global
UNBIND_GLOBAL = unbind_global
DISCARD = discard
TYPE_CHECK = type_check
TEXT_SLICE_COMPARE = text_slice_compare
TEXT_SLICE2_COMPARE = text_slice2_compare
BRANCH = branch
JUMP = jump
RETURN = return_
FULL_CALL = full_call
CALL_RESOLVED = call_resolved
LOCAL_CALL = local_call
FAST_LEN = fast_len
PARSE_INT_TEXT = parse_int_text
PARSE_INT_SLICE = parse_int_slice
RAW_CONSTRUCT = raw_construct

__all__ = [
    "BINARY_OPCODES",
    "BRANCH",
    "COMPARE_OPCODES",
    "CONSTANT",
    "DISCARD",
    "FAST_LEN",
    "FULL_CALL",
    "INSERT_SAFEPOINT",
    "JUMP",
    "LOAD_CLOSURE",
    "LOAD_GLOBAL",
    "LOAD_LOCAL",
    "LOCAL_CALL",
    "MOVE",
    "MOVE_ARGUMENT",
    "OBJECT",
    "PARSE_INT_SLICE",
    "PARSE_INT_TEXT",
    "RAW",
    "RAW_CONSTRUCT",
    "RETURN",
    "STORE_GLOBAL",
    "TERMINATORS",
    "TEXT_SLICE2_COMPARE",
    "TEXT_SLICE_COMPARE",
    "TYPE_CHECK",
    "UNARY_OPCODES",
    "UNBIND_GLOBAL",
    "UNBIND_LOCAL",
    "VALUE_CELL_CLEAR",
    "VALUE_CELL_REF",
    "VALUE_CELL_SET",
    "LiteralEmitter",
    "allocate",
    "compiled_call",
    "helpers",
    "initialize",
    "invalid_initializer",
    "lookup_method",
    "numeric_modulo",
    "primitive",
    "read_int64",
    "render_text",
    "text_access",
    "text_result",
    "write_int64",
]
