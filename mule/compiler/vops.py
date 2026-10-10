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

from mule import ast

from . import fndb
from .ctype import FLOAT, PYTHON_INTEGER, UNKNOWN
from .vmdef import define_vop
from .vop import (
    BOOLEAN,
    DESCRIPTOR,
    IMMEDIATE,
    INSTANCE,
    NULL,
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


def decimal(value: int) -> str:
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


# Fixed templates: emitted by the IR2 converter for IR1 nodes other than
# known calls.


@define_vop("constant", args=(), results=(None,))
def constant(gen, vop):
    result = vop.results[0]
    literal = vop.codegen_info
    if result.tn.sc in (SIGNED, BOOLEAN, NULL):
        gen.store(result, literal.accept(_RawLiteral()))
        return
    gen.pointer_result(result, gen.constant(literal))


class _RawLiteral(ast.Visitor):
    def visit_Node(self, node):
        raise TypeError(f"no raw representation for {node!r}")

    def visit_IntegerLiteral(self, literal):
        return str(literal.value)

    def visit_BooleanLiteral(self, literal):
        return "true" if literal.value else "false"

    def visit_NoneLiteral(self, literal):
        return "0"


@define_vop("load-local", args=(None,), results=(None,))
def load_local(gen, vop):
    source, result = vop.args[0], vop.results[0]
    access = vop.codegen_info
    if source.tn.sc not in OBJECT:
        gen.store(result, gen.load(source))
        return
    value = gen.load(source)
    if access.move:
        gen.line(f"store ptr null, ptr {source.tn.slot}")
    helper = (
        "local.move"
        if access.move or not result.tn.sc.owned or not source.tn.sc.owned
        else "local"
    )
    name = gen.module.text(access.name)
    gen.pointer_result(result, f"call ptr @mule.fast.{helper}(ptr {value}, ptr {name})")


@define_vop("load-global", results=(None,))
def load_global(gen, vop):
    name = gen.module.text(vop.codegen_info)
    gen.pointer_result(
        vop.results[0], f"call ptr @mule_load_global(ptr %globals, ptr {name})"
    )


@define_vop("move", args=(None,), results=(None,))
def move(gen, vop):
    value = gen.take(vop.args[0])
    gen.store(vop.results[0], value)


@define_vop("move-argument", results=(None,))
def move_argument(gen, vop):
    gen.store(vop.results[0], gen.parameter(vop.codegen_info))


@define_vop("load-closure", results=(None,))
def load_closure(gen, vop):
    gen.pointer_result(
        vop.results[0], f"call ptr @mule_arg(ptr %closure, i32 {vop.codegen_info})"
    )


@define_vop("unbind-local", results=(None,))
def unbind_local(gen, vop):
    target = vop.results[0]
    value = gen.load(target)
    gen.line(f"store ptr null, ptr {target.tn.slot}")
    if target.tn.sc.owned:
        gen.release(value)


@define_vop("store-global", args=(None,))
def store_global(gen, vop):
    value = gen.take(vop.args[0])
    name = gen.module.text(vop.codegen_info)
    status = gen.operation(
        f"call i32 @mule_store_global(ptr %globals, ptr {name}, ptr {value})"
    )
    gen.release(value)
    gen.check_status(status)


@define_vop("unbind-global")
def unbind_global(gen, vop):
    name = gen.module.text(vop.codegen_info)
    gen.line(f"call i32 @mule_clear_binding(ptr %globals, ptr {name})")


@define_vop("discard", args=(None,))
def discard(gen, vop):
    operand = vop.args[0]
    if operand.tn.sc in OBJECT:
        gen.release(gen.take(operand))


@define_vop("type-check", args=(None,), results=(None,))
def type_check(gen, vop):
    operand, result = vop.args[0], vop.results[0]
    check = vop.codegen_info
    if operand.tn.sc not in OBJECT:
        gen.store(result, gen.take(operand))
        return
    actual = operand.tn.type or UNKNOWN
    if check.safety and not actual.fits(check.contract.fact):
        # Keep the unrefined operand owned by its original slot until the
        # assertion succeeds, so error cleanup releases its actual value.
        value = gen.load(operand)
        check.contract.accept(_ContractAssertion(gen, value))
    value = gen.take(operand)
    gen.publish(result, value)
    gen.refine(result.tn, value)


class _ContractAssertion:
    def __init__(self, gen, value):
        self.gen = gen
        self.value = value

    def visit_Contract(self, contract):
        raise TypeError(f"unsupported annotation contract {contract}")

    def visit_IntegerContract(self, contract):
        fact = contract.fact
        module = self.gen.module
        minimum = module.text(decimal(fact.minimum))
        maximum = module.text(decimal(fact.maximum))
        name = module.text(contract.display_name)
        status = self.gen.operation(
            f"call i32 @mule_assert_integer(ptr {self.value}, ptr {minimum}, "
            f"ptr {maximum}, ptr {name})"
        )
        self.gen.check_status(status)

    def visit_FloatContract(self, contract):
        name = self.gen.module.text(contract.display_name)
        status = self.gen.operation(
            f"call i32 @mule_assert_float(ptr {self.value}, ptr {name})"
        )
        self.gen.check_status(status)

    def visit_PythonIntegerContract(self, contract):
        gen = self.gen
        true = gen.operation(f"icmp eq ptr {self.value}, @mule.true")
        false = gen.operation(f"icmp eq ptr {self.value}, @mule.false")
        boolean = gen.operation(f"or i1 {true}, {false}")
        integer = gen.fresh("integer.check")[1:]
        done = gen.fresh("integer.checked")[1:]
        gen.line(f"br i1 {boolean}, label %{done}, label %{integer}")
        gen.lines.append(integer + ":")
        status = gen.operation(
            f"call i32 @mule_assert_integer(ptr {self.value}, ptr null, "
            f"ptr null, ptr {gen.module.text(contract.display_name)})"
        )
        gen.check_status(status)
        gen.line(f"br label %{done}")
        gen.lines.append(done + ":")


@define_vop("branch", args=(None,))
def branch(gen, vop):
    operand = vop.args[0]
    targets = vop.codegen_info
    value = gen.take(operand)
    truth = gen.truth(operand.tn, value)
    gen.line(
        f"br i1 {truth}, label %{gen.labels[targets.consequent]}, "
        f"label %{gen.labels[targets.alternative]}"
    )


@define_vop("jump")
def jump(gen, vop):
    gen.line(f"br label %{gen.labels[vop.codegen_info]}")


@define_vop("return", args=(None,))
def return_(gen, vop):
    operand = vop.args[0]
    value = gen.take(operand)
    gen.line(f"store {operand.tn.sc.llvm_type} {value}, ptr %return")
    gen.line("br label %cleanup")


@define_vop("full-call", results=(None,))
def full_call(gen, vop):
    result = vop.results[0]
    site = vop.codegen_info
    function = gen.take(vop.args[0])
    arguments = vop.args[1:]
    values = [gen.take(argument) for argument in arguments]
    gen.argv(values)
    value = gen.call(site, function, arguments, values)
    gen.publish(result, value)
    for operand in (function, *values):
        gen.release(operand)
    gen.check_pointer(value)
    gen.refine(result.tn, value)


TERMINATORS = frozenset({"branch", "jump", "return", "raise", "reraise", "unwind"})

# Known functions over objects.


def _binary_generic(gen, vop, opcode, *, inplace):
    result = vop.results[0]
    left, right = vop.args
    lhs, rhs = gen.take(left), gen.take(right)
    left_type = left.tn.type or UNKNOWN
    if opcode == 5 and not (left_type.fits(PYTHON_INTEGER) or left_type.fits(FLOAT)):
        value = gen.compiled_call("mule_format_modulo", (lhs, rhs))
        gen.publish(result, value)
        gen.release(lhs)
        gen.release(rhs)
        gen.check_pointer(value)
        gen.refine(result.tn, value)
        return
    fixnums = left_type.is_fixnum and (right.tn.type or UNKNOWN).is_fixnum
    result_type = result.tn.type or UNKNOWN
    if fixnums and result_type.is_fixnum and opcode in _PRIMITIVE_BINARY:
        a, b = gen.decode(lhs), gen.decode(rhs)
        integer = gen.operation(f"{_PRIMITIVE_BINARY[opcode]} i64 {a}, {b}")
        gen.native_result(result, gen.encode(integer), (lhs, rhs))
        return
    prefix = "fixnum" if fixnums else "fast"
    reuse = "null"
    info = vop.codegen_info
    if inplace and info is not None and info.reuse_local is not None:
        reuse = gen.local_slot(info.reuse_local)
    if opcode in (0, 1, 2, 4, 5, 8, 9, 10, 11, 12):
        operation = (
            f"call ptr @mule.{prefix}.binary.{opcode}(ptr {lhs}, ptr {rhs}, "
            f"i32 {int(inplace)}, ptr {reuse})"
        )
    else:
        operation = (
            f"call ptr @mule_binary_reuse(i32 {opcode}, ptr {lhs}, ptr {rhs}, "
            f"i32 {int(inplace)}, ptr {reuse})"
        )
    gen.pointer_result(result, operation, (lhs, rhs))


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
        fixnum = (operand.tn.type or UNKNOWN).is_fixnum
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
        prefix = "fixnum" if fixnum and opcode < 3 else "fast"
        gen.pointer_result(
            result, f"call ptr @mule.{prefix}.unary.{opcode}(ptr {value})", (value,)
        )


for _operator, _opcode in UNARY_OPCODES.items():
    _define_unary(_operator, _opcode)


def _define_compare(operator, opcode):
    @define_vop(
        f"compare-{operator.name.lower()}",
        translate=fndb.COMPARE[operator],
        args=(OBJECT, OBJECT),
        results=(OBJECT,),
    )
    def generic(gen, vop):
        result = vop.results[0]
        left, right = vop.args
        lhs, rhs = gen.take(left), gen.take(right)
        if (
            opcode < 6
            and (left.tn.type or UNKNOWN).is_fixnum
            and (right.tn.type or UNKNOWN).is_fixnum
        ):
            a, b = gen.decode(lhs), gen.decode(rhs)
            predicate = ("eq", "ne", "slt", "sle", "sgt", "sge")[opcode]
            condition = gen.operation(f"icmp {predicate} i64 {a}, {b}")
            boolean = gen.operation(
                f"select i1 {condition}, ptr @mule.true, ptr @mule.false"
            )
            gen.native_result(result, boolean, (lhs, rhs))
            return
        operation = (
            f"call ptr @mule.fast.compare.{opcode}(ptr {lhs}, ptr {rhs})"
            if opcode < 8
            else f"call ptr @mule_compare(i32 {opcode}, ptr {lhs}, ptr {rhs})"
        )
        gen.pointer_result(result, operation, (lhs, rhs))


for _operator, _opcode in COMPARE_OPCODES.items():
    _define_compare(_operator, _opcode)


@define_vop("getattr", translate=fndb.GETATTR, args=(OBJECT,), results=(OBJECT,))
def getattr_(gen, vop):
    receiver = gen.take(vop.args[0])
    info = vop.codegen_info
    gen.pointer_result(
        vop.results[0],
        f"call ptr @mule_get_attr_cached(ptr {receiver}, ptr {gen.module.text(info.name)}, "
        f"ptr {gen.module.attribute_cache()})",
        (receiver,),
    )


@define_vop("setattr", translate=fndb.SETATTR, args=(OBJECT, OBJECT))
def setattr_(gen, vop):
    receiver, value = gen.take(vop.args[0]), gen.take(vop.args[1])
    info = vop.codegen_info
    status = gen.operation(
        f"call i32 @mule_set_attr_cached(ptr {receiver}, ptr {gen.module.text(info.name)}, "
        f"ptr {value}, ptr {gen.module.attribute_cache()})"
    )
    gen.release(receiver)
    gen.release(value)
    gen.check_status(status)


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
    # Loop iterations reuse the receiver slot; the previous call normally
    # cleared it.
    old = gen.load(bound)
    gen.line(f"store ptr null, ptr {bound.tn.slot}")
    gen.release(old)
    gen.pointer_result(
        method,
        f"call ptr @mule_resolve_method(ptr {receiver}, ptr {gen.module.text(info.name)}, "
        f"ptr {gen.module.attribute_cache()}, ptr {bound.tn.slot})",
        (receiver,),
    )


@define_vop("call-resolved", translate=fndb.CALL_RESOLVED, results=(OBJECT,))
def call_resolved(gen, vop):
    method, bound, *arguments = vop.args
    function = gen.take(method)
    receiver = gen.load(bound)
    gen.line(f"store ptr null, ptr {bound.tn.slot}")
    values = [gen.take(argument) for argument in arguments]
    gen.argv(values)
    value = gen.call_method(vop.codegen_info, function, receiver, values)
    gen.publish(vop.results[0], value)
    for operand in (function, receiver, *values):
        gen.release(operand)
    gen.check_pointer(value)


@define_vop("make-function", translate=fndb.MAKE_FUNCTION, results=(OBJECT,))
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
    defaults = gen.tuple_of(result, defaults_refs)
    closure = gen.tuple_of(result, closure_refs)
    if defaults is None and target.binding is not None:
        defaults = "null"
    consumed = [value for value in (defaults, closure) if value not in (None, "null")]
    if closure is not None:
        call = (
            f"call ptr @mule_function_closure(ptr %globals, ptr {native}, ptr {name}, "
            f"ptr {defaults or 'null'}, ptr {closure})"
        )
    elif defaults is not None:
        call = (
            f"call ptr @mule_function_defaults(ptr %globals, ptr {native}, ptr {name}, "
            f"ptr {defaults})"
        )
    else:
        call = f"call ptr @mule_function(ptr %globals, ptr {native}, ptr {name})"
    gen.pointer_result(result, call, consumed)


@define_vop("make-class", translate=fndb.MAKE_CLASS, results=(OBJECT,))
def make_class(gen, vop):
    info = vop.codegen_info
    module = gen.module
    operands = list(vop.args)
    base = gen.take(operands.pop(0)) if info.has_base else "null"
    methods = [gen.take(method) for method in operands]
    gen.argv(methods)
    layout = module.layouts.get(info.class_id)
    name = module.text(info.public_name)
    if layout is not None:
        gen.pointer_result(
            vop.results[0],
            f"call ptr @mule_native_class(ptr {name}, "
            f"ptr {layout.schema}, ptr %globals, i64 {len(info.fields)}, ptr {module.names(info.fields)}, "
            f"i64 {len(methods)}, ptr {module.names(info.method_names)}, ptr %argv, "
            f"ptr {layout.constructor}, ptr {layout.getter}, ptr {layout.setter})",
            methods,
        )
        return
    gen.pointer_result(
        vop.results[0],
        f"call ptr @mule_class(ptr {name}, "
        f"ptr {base}, i64 {len(info.fields)}, ptr {module.names(info.fields)}, "
        f"i64 {len(methods)}, ptr {module.names(info.method_names)}, ptr %argv)",
        (base, *methods),
    )


@define_vop("import", translate=fndb.IMPORT, results=(OBJECT,))
def import_(gen, vop):
    index = gen.module.import_index(vop.codegen_info)
    gen.pointer_result(vop.results[0], f"call ptr @mule.import.{index}()")


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
    gen.pointer_result(vop.results[0], f"call ptr @mule_bool(i32 {status})")


def _sequence(gen, vop, primitive):
    values = [gen.take(element) for element in vop.args]
    gen.argv(values)
    gen.pointer_result(
        vop.results[0], f"call ptr @{primitive}(i64 {len(values)}, ptr %argv)", values
    )


@define_vop("build-list", translate=fndb.BUILD_LIST, results=(OBJECT,))
def build_list(gen, vop):
    _sequence(gen, vop, "mule_sequence_list")


@define_vop("build-tuple", translate=fndb.BUILD_TUPLE, results=(OBJECT,))
def build_tuple(gen, vop):
    _sequence(gen, vop, "mule_sequence_tuple")


@define_vop("format", translate=fndb.FORMAT, args=(OBJECT, OBJECT), results=(OBJECT,))
def format_(gen, vop):
    result = vop.results[0]
    value = gen.take(vop.args[0])
    conversion = vop.codegen_info.conversion
    if conversion is not None:
        protocol = {"s": "string", "r": "representation"}[conversion]
        converted = gen.compiled_call("mule_format_" + protocol, [value])
        gen.publish(result, converted)
        gen.release(value)
        gen.check_pointer(converted)
        value = gen.take(result)
    specification = gen.take(vop.args[1])
    formatted = gen.compiled_call("mule_format_format_value", [value, specification])
    gen.publish(result, formatted)
    gen.release(value)
    gen.release(specification)
    gen.check_pointer(formatted)


@define_vop("range-prepare", translate=fndb.RANGE_PREPARE, results=(OBJECT,))
def range_prepare(gen, vop):
    callee, iterator = gen.take(vop.args[0]), gen.take(vop.args[1])
    state = vop.args[2].tn.slot
    module = gen.module
    globals_value = gen.operation("call ptr @mule_protocol_globals()")
    matched = gen.operation(
        f"call i32 @mule_function_matches(ptr {callee}, "
        f"ptr {module.functions[module.sequence_functions['range']]}, ptr {globals_value})"
    )
    stored = gen.operation(f"call i32 @mule_range_storage(ptr {iterator}, ptr {state})")
    accepted = gen.operation(f"and i32 {matched}, {stored}")
    result = gen.operation(f"call ptr @mule_bool(i32 {accepted})")
    gen.publish(vop.results[0], result)
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
    current, stop, step = _range_words(gen, vop.args[0].tn.slot)
    forward = gen.operation(f"icmp sgt i64 {step}, 0")
    below = gen.operation(f"icmp slt i64 {current}, {stop}")
    above = gen.operation(f"icmp sgt i64 {current}, {stop}")
    available = gen.operation(f"select i1 {forward}, i1 {below}, i1 {above}")
    wide = gen.operation(f"zext i1 {available} to i32")
    gen.publish(vop.results[0], gen.operation(f"call ptr @mule_bool(i32 {wide})"))


@define_vop("range-advance", translate=fndb.RANGE_ADVANCE, results=(OBJECT,))
def range_advance(gen, vop):
    state = vop.args[0].tn.slot
    current, _, step = _range_words(gen, state)
    following = gen.operation(f"add nsw i64 {current}, {step}")
    gen.line(f"store i64 {following}, ptr {state}")
    encoded = gen.operation(f"shl i64 {current}, 1")
    tagged = gen.operation(f"or i64 {encoded}, 1")
    result = gen.operation(f"inttoptr i64 {tagged} to ptr")
    gen.publish(vop.results[0], result)
    gen.refine(vop.results[0].tn, result)


def _define_protocol(name, info):
    @define_vop(f"protocol-{name}", translate=info, results=(OBJECT,))
    def protocol(gen, vop):
        arguments = [gen.take(value) for value in vop.args]
        result = gen.sequence_call(name, arguments)
        gen.publish(vop.results[0], result)
        for value in arguments:
            gen.release(value)
        gen.check_pointer(result)


for _name, _info in fndb.PROTOCOLS.items():
    _define_protocol(_name, _info)


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
    message = gen.take(vop.args[0])
    status = gen.operation(f"call i32 @mule_assertion_failure(ptr {message})")
    gen.release(message)
    gen.check_status(status)


@define_vop("raise", translate=fndb.RAISE, args=(OBJECT,))
def raise_(gen, vop):
    value = gen.take(vop.args[0])
    result = gen.sequence_call("raise_value", [value])
    gen.release(value)
    gen.release(result)
    gen.propagate()


@define_vop("reraise", translate=fndb.RERAISE)
def reraise(gen, vop):
    gen.line("call i32 @mule_reraise()")
    gen.propagate()


@define_vop("unwind", translate=fndb.UNWIND)
def unwind(gen, vop):
    gen.propagate()


# Compiled-protocol primitives: the physical operations the compiled stdlib
# modules call by underscored name.


def primitive(name, info, emit):
    """Register ``emit(gen, vop, values)`` as the template of ``info``: the
    operands are consumed, the result published, and the operands released
    before the result is checked."""

    @define_vop(f"primitive-{name}", translate=info, results=(OBJECT,))
    def generator(gen, vop):
        values = [gen.take(value) for value in vop.args]
        result = emit(gen, vop, values)
        gen.publish(vop.results[0], result)
        for value in values:
            gen.release(value)
        gen.check_pointer(result)

    return generator


def text_access(function):
    def emit(gen, vop, values):
        operands = ", ".join(f"ptr {value}" for value in values)
        return gen.operation(f"call ptr @{function}({operands})")

    return emit


def lookup_method(name):
    def emit(gen, vop, values):
        return gen.operation(
            f"call ptr @mule_lookup_special(ptr {values[0]}, ptr {gen.module.text(name)})"
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


def compiled_call(function):
    def emit(gen, vop, values):
        return gen.compiled_call(function, values)

    return emit


def numeric_modulo(gen, vop, values):
    return gen.operation(
        f"call ptr @mule.fast.binary.5(ptr {values[0]}, ptr {values[1]}, i32 0, ptr null)"
    )


def binding_body(gen, vop, values):
    """The external entry's final local call shares the caller's logical frame."""
    target = vop.codegen_info
    globals_value, view, *parameters = values
    owned = [
        gen.operation(f"call ptr @mule.fast.retain(ptr {value})")
        for value in parameters
    ]
    operands = [f"ptr {globals_value}"]
    if gen.module.closure_of(target):
        closure = gen.operation(f"call ptr @mule_closure(ptr {view})")
        operands.append(f"ptr {closure}")
    operands.extend(f"ptr {value}" for value in owned)
    return gen.operation(f"call ptr {gen.module.bodies[target]}({', '.join(operands)})")


def allocate(gen, vop, values):
    return gen.operation(f"call ptr @mule_native_new(ptr {values[0]})")


def initialize(gen, vop, values):
    layout = vop.codegen_info
    target = gen.module.bodies[layout.methods["__init__"]]
    status = gen.operation("call i32 @mule_enter_frame()")
    gen.check_status(status)
    owned = [
        gen.operation(f"call ptr @mule.fast.retain(ptr {value})") for value in values
    ]
    params = ", ".join(["ptr %globals", *(f"ptr {value}" for value in owned)])
    result = gen.operation(f"call ptr {target}({params})")
    gen.line("call void @mule_leave_frame()")
    return result


def invalid_initializer(gen, vop, values):
    gen.line("call i32 @mule_native_initializer_error()")
    return "null"


def read_int64(gen, vop, values):
    payload = gen.operation(f"call ptr @mule_native_payload(ptr {values[0]})")
    gen.check_pointer(payload)
    index = gen.decode(values[1])
    address = gen.operation(f"getelementptr i64, ptr {payload}, i64 {index}")
    value = gen.operation(f"load i64, ptr {address}")
    return gen.operation(f"call ptr @mule_box_i64(i64 {value})")


def write_int64(gen, vop, values):
    payload = gen.operation(f"call ptr @mule_native_payload(ptr {values[0]})")
    gen.check_pointer(payload)
    slot = gen.operation("alloca i64")
    status = gen.operation(f"call i32 @mule_unbox_i64(ptr {values[2]}, ptr {slot})")
    gen.check_status(status)
    value = gen.operation(f"load i64, ptr {slot}")
    index = gen.decode(values[1])
    address = gen.operation(f"getelementptr i64, ptr {payload}, i64 {index}")
    gen.line(f"store i64 {value}, ptr {address}")
    return "@mule.none"


# Raw templates: native bodies over machine representations.

RAW_SCALAR = (SIGNED, BOOLEAN, NULL)


def _raw_binary(operator, opcode):
    @define_vop(
        f"raw-binary-{operator.name.lower()}",
        translate=(fndb.BINARY[operator], fndb.INPLACE_BINARY[operator]),
        args=((SIGNED,), (SIGNED,)),
        results=((SIGNED,),),
        ltn_policy=RAW,
    )
    def generator(gen, vop):
        left, right = gen.take(vop.args[0]), gen.take(vop.args[1])
        gen.store(vop.results[0], gen.arithmetic(opcode, left, right))


for _operator, _opcode in _RAW_BINARY.items():
    _raw_binary(_operator, _opcode)


@define_vop(
    "raw-not",
    translate=fndb.UNARY[ast.UnaryOperator.NOT],
    args=((SIGNED, BOOLEAN),),
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
    results=(RAW_SCALAR,),
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
    result = gen.operation(
        f"call {plan.returned.llvm_type} {plan.entry}({', '.join(parameters)})"
    )
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
    payload = gen.operation(f"call ptr @mule_native_payload(ptr {instance})")
    gen.check_pointer(payload)
    gen.store(vop.results[0], payload)


# Runtime helper templates shared by the object templates.

INTRINSICS = """declare {i64, i1} @llvm.sadd.with.overflow.i64(i64, i64)
declare {i64, i1} @llvm.ssub.with.overflow.i64(i64, i64)
declare {i64, i1} @llvm.smul.with.overflow.i64(i64, i64)
@mule.true = external global i8
@mule.false = external global i8
@mule.none = external global i8
"""


def _ownership() -> str:
    functions = []
    for name in ("retain", "release"):
        result = "ptr" if name == "retain" else "void"
        returned = "ret ptr %value" if name == "retain" else "ret void"
        call = (
            "%owned = call ptr @mule_retain(ptr %value)\n  ret ptr %owned"
            if name == "retain"
            else "call void @mule_release(ptr %value)\n  ret void"
        )
        functions.append(f"""define internal {result} @mule.fast.{name}(ptr %value) alwaysinline {{
entry:
  %bits = ptrtoint ptr %value to i64
  %tag = and i64 %bits, 1
  %integer = icmp ne i64 %tag, 0
  %null = icmp eq ptr %value, null
  %true = icmp eq ptr %value, @mule.true
  %false = icmp eq ptr %value, @mule.false
  %none = icmp eq ptr %value, @mule.none
  %a = or i1 %integer, %null
  %b = or i1 %true, %false
  %c = or i1 %a, %b
  %skip = or i1 %c, %none
  br i1 %skip, label %done, label %heap
heap:
  {call}
done:
  {returned}
}}""")
    return "\n\n".join(functions)


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
}"""


_ENCODE = """  %shifted = shl i64 %result, 1
  %tagged = or i64 %shifted, 1
  %value = inttoptr i64 %tagged to ptr
  ret ptr %value"""
_RANGE = """  %test.shifted = shl i64 %result, 1
  %test.decoded = ashr i64 %test.shifted, 1
  %range = icmp eq i64 %result, %test.decoded"""


def _binary_helper(op: int, *, fixnums: bool = False) -> str:
    if op in (0, 1, 2):
        intrinsic = {0: "sadd", 1: "ssub", 2: "smul"}[op]
        operation = f"""  %checked = call {{i64, i1}} @llvm.{intrinsic}.with.overflow.i64(i64 %a, i64 %b)
  %result = extractvalue {{i64, i1}} %checked, 0
  %overflow = extractvalue {{i64, i1}} %checked, 1
{_RANGE}
  %no.overflow = xor i1 %overflow, true
  %fits = and i1 %range, %no.overflow
  br i1 %fits, label %encode, label %slow"""
    elif op in (4, 5):
        result = "%floor" if op == 4 else "%modulo"
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
  %floor = sub i64 %quotient, %correction
  %adjusted.remainder = add i64 %remainder, %b
  %modulo = select i1 %adjust, i64 %adjusted.remainder, i64 %remainder
  %result = add i64 {result}, 0
{_RANGE}
  br i1 %range, label %encode, label %slow"""
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
    return f"""define internal ptr @mule.{prefix}.binary.{op}(ptr %left, ptr %right, i32 %inplace, ptr %reuse) alwaysinline {{
entry:
  %left.bits = ptrtoint ptr %left to i64
  %right.bits = ptrtoint ptr %right to i64
{guard}
  %a = ashr i64 %left.bits, 1
  %b = ashr i64 %right.bits, 1
{operation}
encode:
{_ENCODE}
slow:
  %fallback = call ptr @mule_binary_reuse(i32 {op}, ptr %left, ptr %right, i32 %inplace, ptr %reuse)
  ret ptr %fallback
}}"""


def _unary_helper(op: int, *, fixnum: bool = False) -> str:
    if op == 3:
        return """define internal ptr @mule.fast.unary.3(ptr %operand) alwaysinline {
entry:
  %truth = call i32 @mule.fast.truth(ptr %operand)
  %error = icmp slt i32 %truth, 0
  br i1 %error, label %failed, label %done
failed:
  ret ptr null
done:
  %zero = icmp eq i32 %truth, 0
  %value = select i1 %zero, ptr @mule.true, ptr @mule.false
  ret ptr %value
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
{guard}
  %decoded = ashr i64 %bits, 1
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


def helpers() -> str:
    """Native guarded templates; LLVM removes unused templates after inlining."""
    return "\n\n".join(
        [
            INTRINSICS,
            _ownership(),
            _truth(),
            *(_binary_helper(op) for op in (0, 1, 2, 4, 5, 8, 9, 10, 11, 12)),
            *(
                _binary_helper(op, fixnums=True)
                for op in (0, 1, 2, 4, 5, 8, 9, 10, 11, 12)
            ),
            *(_unary_helper(op) for op in range(4)),
            *(_unary_helper(op, fixnum=True) for op in range(3)),
            *(_compare_helper(op) for op in range(8)),
        ]
    )


class LiteralEmitter(ast.Visitor):
    """Each literal chooses its native representation without host type checks."""

    def __init__(self, module):
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
        return f"call ptr @mule_literal_int(ptr {self.module.text(decimal(value))})"

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


CONSTANT = constant
LOAD_LOCAL = load_local
LOAD_GLOBAL = load_global
MOVE = move
MOVE_ARGUMENT = move_argument
LOAD_CLOSURE = load_closure
UNBIND_LOCAL = unbind_local
STORE_GLOBAL = store_global
UNBIND_GLOBAL = unbind_global
DISCARD = discard
TYPE_CHECK = type_check
BRANCH = branch
JUMP = jump
RETURN = return_
FULL_CALL = full_call
RAW_CONSTRUCT = raw_construct

__all__ = [
    "BINARY_OPCODES",
    "BRANCH",
    "COMPARE_OPCODES",
    "CONSTANT",
    "DISCARD",
    "FULL_CALL",
    "JUMP",
    "LOAD_CLOSURE",
    "LOAD_GLOBAL",
    "LOAD_LOCAL",
    "MOVE",
    "MOVE_ARGUMENT",
    "OBJECT",
    "RAW",
    "RAW_CONSTRUCT",
    "RETURN",
    "STORE_GLOBAL",
    "TERMINATORS",
    "TYPE_CHECK",
    "UNARY_OPCODES",
    "UNBIND_GLOBAL",
    "UNBIND_LOCAL",
    "LiteralEmitter",
    "allocate",
    "binding_body",
    "compiled_call",
    "decimal",
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
