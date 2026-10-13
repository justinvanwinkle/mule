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
    CLASS,
    DESCRIPTOR,
    DOUBLE,
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
    if result.tn.sc in (SIGNED, BOOLEAN, NULL, DOUBLE):
        gen.store(result, literal.accept(_RawLiteral()))
        return
    literal.accept(_BoxedConstant(gen, result))


class _BoxedConstant(ast.Visitor):
    """Materialize constants using their compiled representation."""

    def __init__(self, gen, result):
        self.gen = gen
        self.result = result

    def visit_Node(self, node):
        self.gen.pointer_result(self.result, self.gen.constant(node))

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
    result = vop.results[0]
    if result.tn.sc is CLASS:
        # A native body's class dependency was loaded and guarded at entry.
        gen.store(result, gen.dependency(vop.codegen_info))
        return
    gen.pointer_result(
        result, gen.module.load_global(vop.codegen_info, gen.lambda_.policy.safety)
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
    var = vop.codegen_info
    status = gen.operation(gen.module.store_global(var.namespace, var.name, value))
    gen.release(value)
    gen.check_status(status)


@define_vop("unbind-global")
def unbind_global(gen, vop):
    gen.line(gen.module.delete_global(vop.codegen_info))


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
    gen.release(function)
    gen.check_pointer(value)
    gen.refine(result.tn, value)


TERMINATORS = frozenset(
    {"branch", "jump", "return", "raise", "reraise", "resume-exception", "unwind"}
)

# Known functions over objects.


def _binary_generic(gen, vop, opcode, *, inplace):
    result = vop.results[0]
    left, right = vop.args
    lhs, rhs = gen.take(left), gen.take(right)
    left_type = left.tn.type or UNKNOWN
    if opcode == 5 and not (left_type.fits(PYTHON_INTEGER) or left_type.fits(FLOAT)):
        value = gen.compiled_call_named("mule_format_modulo", (lhs, rhs))
        gen.publish(result, value)
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
    out = gen.out_pointer()
    value = gen.operation(
        f"call ptr @mule_resolve_method(ptr {receiver}, ptr {gen.module.text(info.name)}, "
        f"ptr {gen.module.attribute_cache()}, ptr {out})"
    )
    gen.publish(method, value)
    gen.publish(bound, gen.operation(f"load ptr, ptr {out}"))
    gen.release(receiver)
    gen.check_pointer(value)
    gen.refine(method.tn, value)


@define_vop("call-resolved", translate=fndb.CALL_RESOLVED, results=(OBJECT,))
def call_resolved(gen, vop):
    method, bound, *arguments = vop.args
    function = gen.take(method)
    receiver = gen.take(bound)
    values = [gen.take(argument) for argument in arguments]
    gen.argv(values)
    value = gen.call_method(vop.codegen_info, function, receiver, values)
    gen.publish(vop.results[0], value)
    gen.release(function)
    gen.check_pointer(value)


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
        cells = module.text(
            "".join("c" if var.indirect else "v" for var in target.environment.closure)
        )
        function = gen.operation(call)
        call = f"call ptr @mule_function_closure(ptr {function}, ptr {closure}, ptr {cells})"
    gen.pointer_result(result, call, consumed)


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
    gen.pointer_result(vop.results[0], f"call ptr @mule_bool(i32 {status})")


@define_vop("current-globals", translate=fndb.CURRENT_GLOBALS, results=(OBJECT,))
def current_globals(gen, vop):
    # The entry's globals argument, retained for the result's owner.
    gen.pointer_result(vop.results[0], "call ptr @mule.fast.retain(ptr %globals)")


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
    values = [gen.take(element) for element in vop.args]
    gen.argv(values)
    gen.pointer_result(
        vop.results[0],
        f"call ptr @mule_sequence_tuple_owned(i64 {len(values)}, ptr %argv)",
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


@define_vop("range-prepare", translate=fndb.RANGE_PREPARE, results=(OBJECT,))
def range_prepare(gen, vop):
    callee, iterator = gen.take(vop.args[0]), gen.take(vop.args[1])
    state = vop.args[2].tn.slot
    # No callable-identity guard: `range` is a class, and the storage check
    # only accepts iterators of the protocol's own RangeIterator class.
    stored = gen.operation(f"call i32 @mule_range_storage(ptr {iterator}, ptr {state})")
    result = gen.operation(f"call ptr @mule_bool(i32 {stored})")
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
        gen.check_pointer(result)


@define_vop("structural-unpack", translate=fndb.PROTOCOLS["unpack"], results=(OBJECT,))
def structural_unpack(gen, vop):
    value, count = [gen.take(argument) for argument in vop.args]
    if vop.codegen_info.proved:
        gen.publish(vop.results[0], value)
        gen.release(count)
        return
    matched = gen.operation(
        f"call i32 @mule_tuple_shape_matches(ptr {value}, i64 {vop.codegen_info.arity})"
    )
    valid = gen.operation(f"icmp ne i32 {matched}, 0")
    fast = gen.fresh("unpack.tuple")[1:]
    slow = gen.fresh("unpack.iterable")[1:]
    done = gen.fresh("unpack.done")[1:]
    gen.line(f"br i1 {valid}, label %{fast}, label %{slow}")
    gen.lines.append(fast + ":")
    gen.release(count)
    gen.line(f"br label %{done}")
    gen.lines.append(slow + ":")
    result = gen.sequence_call("unpack", [value, count])
    unpacked = gen.check_pointer(result)
    gen.line(f"br label %{done}")
    gen.lines.append(done + ":")
    gen.publish(
        vop.results[0],
        gen.operation(f"phi ptr [{value}, %{fast}], [{result}, %{unpacked}]"),
    )


for _name, _info in fndb.PROTOCOLS.items():
    if _name != "unpack":
        _define_protocol(_name, _info)


@define_vop("unpack-ref", translate=fndb.UNPACK_REF, results=(OBJECT,))
def unpack_ref(gen, vop):
    access = vop.codegen_info
    parent = gen.load(vop.args[0]) if access.borrow_parent else gen.take(vop.args[0])
    position = gen.take(vop.args[1])
    if access.proved:
        result = gen.operation(
            f"call ptr @mule_tuple_slot(ptr {parent}, i64 {access.index})"
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


def compiled_call(name):
    def emit(gen, vop, values):
        return gen.compiled_call_named(name, values)

    return emit


def numeric_modulo(gen, vop, values):
    return gen.operation(
        f"call ptr @mule.fast.binary.5(ptr {values[0]}, ptr {values[1]}, i32 0, ptr null)"
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
@mule.undefined = external global i8
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
        # Fixnums and NULL are not objects. Immortal objects, including
        # True, False and None, carry the reference count the runtime
        # leaves unchanged; that check inlines with the runtime under LTO.
        functions.append(f"""define internal {result} @mule.fast.{name}(ptr %value) alwaysinline {{
entry:
  %bits = ptrtoint ptr %value to i64
  %tag = and i64 %bits, 1
  %integer = icmp ne i64 %tag, 0
  %null = icmp eq ptr %value, null
  %skip = or i1 %integer, %null
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

define internal ptr @mule.global.live(ptr %cell, ptr %name) alwaysinline {
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


# Exact ordering of a machine integer against a binary64: doubles beyond the
# i64 range order by sign, and in-range doubles split into an integer part,
# which is compared as an integer, and a fraction that breaks ties.
MIXED_COMPARE = """define internal i32 @mule.compare.i64.double(i64 %integer, double %double) alwaysinline {
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


def helpers() -> str:
    """Native guarded templates; LLVM removes unused templates after inlining."""
    return "\n\n".join(
        [
            INTRINSICS,
            MIXED_COMPARE,
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
        return f"call ptr @mule_literal_int(ptr {self.module.text(decimal(value))})"

    def visit_FloatLiteral(self, node: ast.FloatLiteral) -> str:
        return f"call ptr @mule_literal_float(double {double_constant(node.value)})"

    def visit_TextLiteral(self, node: ast.TextLiteral) -> str:
        encoded = node.value.encode("utf-8", "surrogatepass")
        return (
            f"call ptr @mule_literal_text(ptr {self.module.literal_data(encoded)}, "
            f"i64 {len(encoded)})"
        )

    def visit_BytesLiteral(self, node: ast.BytesLiteral) -> str:
        value = node.value
        return (
            f"call ptr @mule_literal_bytes(ptr {self.module.literal_data(value)}, "
            f"i64 {len(value)})"
        )

    def visit_ComplexLiteral(self, node: ast.ComplexLiteral) -> str:
        return self.visit_Literal(node)

    def visit_EllipsisLiteral(self, node: ast.EllipsisLiteral) -> str:
        return self.visit_Literal(node)

    def visit_Tuple(self, node: ast.Tuple) -> str:
        return f"call ptr {self.module.constant_tuple(node)}()"


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
