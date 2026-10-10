"""Representation selection, following ``src/compiler/represent.lisp``.

Two decisions live here:

- ``select_representations`` assigns every TN of an IR2 component a storage
  class from its primitive type's candidates by the cost of the VOP operands
  that read and write it (``select-tn-representation``). A local whose every
  writer delivers an immortal value becomes an immediate (never released);
  everything else holding an object is an owned descriptor. Raw storage
  classes are chosen when the templates reading a TN restrict it to them.

- ``select_native_plans`` decides which lambdas get a native entry: a body
  whose parameters, locals, and intermediate values all have proven machine
  representations under ``int64``/``bool``/native-class contracts. The plan
  is the representation assignment that entry is generated under, with the
  class layouts it depends on (guarded once at entry) and the allocations it
  owns. SBCL has no such all-or-nothing decision because its types are
  static; Mule guards class identity at runtime, so a native body needs a
  general body to fall back to.
"""

from __future__ import annotations

from dataclasses import dataclass, field

from mule import ast

from .annotation_types import EXPORTS
from .ctype import UNKNOWN
from .ir1util import block_exception_target
from .ltn import primitive_type_of
from .node import (
    CallKind,
    Cast,
    Combination,
    Constant,
    FunctionalKind,
    GlobalVar,
    If,
    KnownFunction,
    Lambda,
    LambdaVar,
    NodeVisitor,
    Ref,
    Return,
    Set,
)
from .vop import (
    BOOLEAN_T,
    CLASS_T,
    FIXNUM,
    IMMORTAL_T,
    NONE_T,
    RANGE,
    RANGE_T,
    SIGNED64,
    IR2Component,
    PrimitiveType,
    T,
    instance_type,
)

IMMORTAL_TYPES = (FIXNUM, BOOLEAN_T, NONE_T, IMMORTAL_T)


# Storage-class selection for one IR2 component.


def select_representations(component: IR2Component) -> None:
    if component.context.plan is None:
        for tn in component.tns:
            if tn.leaf is not None:
                tn.primitive_type = _variable_type(tn, component)
    for tn in component.tns:
        tn.sc = select_tn_representation(tn)


def _variable_type(tn, component) -> PrimitiveType:
    """A local is immortal when every value written into it is."""
    context = component.context
    for ref in tn.writes:
        vop = ref.vop
        name = vop.info.name
        if name == "move":
            written = vop.args[0].tn.primitive_type
        elif name == "move-argument":
            written = primitive_type_of(context.parameters[vop.codegen_info])
        elif name == "unbind-local":
            continue
        else:
            written = T
        if written not in IMMORTAL_TYPES:
            return T
    return IMMORTAL_T


def select_tn_representation(tn):
    """The cheapest storage class among the primitive type's candidates,
    counting one coercion per operand whose restriction excludes it."""
    if tn.primitive_type is RANGE_T:
        return RANGE
    best, best_cost = None, None
    for sc in tn.primitive_type.scs:
        cost = 0
        for ref in (*tn.reads, *tn.writes):
            restriction = _restriction(ref)
            if restriction is not None and sc not in restriction:
                cost += 1
        if best is None or cost < best_cost:
            best, best_cost = sc, cost
    return best


def _restriction(ref):
    vop = ref.vop
    operands = vop.results if ref.write_p else vop.args
    restrictions = vop.info.results if ref.write_p else vop.info.args
    if not restrictions:
        return None
    index = operands.index(ref)
    if index >= len(restrictions):
        return None
    return restrictions[index]


# Native representation plans.


class NotNative(Exception):
    pass


class Deferred(Exception):
    pass


class Representation:
    llvm_type = "i64"
    zero = "0"
    primitive_type = SIGNED64

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

    def truth(self):
        raise NotNative

    def boxable(self):
        raise NotNative

    def parameter(self, gen, value):
        raise NotNative


class Int64(Representation):
    def numeric(self):
        return self

    def python_integer(self):
        return self

    def truth(self):
        return True

    def boxable(self):
        return True

    def parameter(self, gen, value):
        slot = gen.operation("alloca i64")
        status = gen.operation(f"call i32 @mule_unbox_i64(ptr {value}, ptr {slot})")
        gen.check_status(status)
        return gen.operation(f"load i64, ptr {slot}")


class Boolean(Representation):
    llvm_type = "i1"
    zero = "false"
    primitive_type = BOOLEAN_T

    def python_integer(self):
        return self

    def truth(self):
        return True

    def boxable(self):
        return True


class Nothing(Representation):
    primitive_type = NONE_T

    def boxable(self):
        return True


@dataclass(frozen=True)
class Instance(Representation):
    layout_id: str
    layout: object = field(compare=False)
    llvm_type = "ptr"
    zero = "null"

    @property
    def primitive_type(self):
        return instance_type(self.layout)

    def field(self, name):
        if name not in self.layout.info.fields:
            raise NotNative
        return INT64

    def method(self, name):
        target = self.layout.methods.get(name)
        if target is None:
            raise NotNative
        return Method(self, target)

    def parameter(self, gen, value):
        payload = gen.operation(
            f"call ptr @mule_native_instance_payload(ptr {value}, "
            f"ptr {self.layout.schema}, ptr %globals)"
        )
        gen.check_pointer(payload)
        return payload


@dataclass(frozen=True)
class Class(Representation):
    binding: str
    layout_id: str
    layout: object = field(compare=False)
    llvm_type = "ptr"
    zero = "null"
    primitive_type = CLASS_T

    def constructor(self):
        return Instance(self.layout_id, self.layout)


@dataclass(frozen=True)
class Method(Representation):
    receiver: Instance
    target: object
    llvm_type = "ptr"
    zero = "null"

    @property
    def primitive_type(self):
        return self.receiver.primitive_type


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


@dataclass(slots=True)
class NativePlan:
    """The representation assignment of one native entry."""

    function: Lambda
    index: int
    parameters: tuple[Representation, ...]
    locals: dict[LambdaVar, Representation]
    values: dict[object, Representation] = field(default_factory=dict)
    returned: Representation | None = None
    dependencies: dict[str, Class] = field(default_factory=dict)
    calls: set[Lambda] = field(default_factory=set)
    allocations: dict[object, Class] = field(default_factory=dict)
    method_targets: dict[object, Lambda] = field(default_factory=dict)

    @property
    def entry(self):
        return f"@mule.native.{self.index}"

    def lvar_type(self, lvar):
        if len(lvar.uses) != 1:
            return None
        representation = self.values.get(lvar.uses[0])
        return representation.primitive_type if representation is not None else None

    def var_type(self, var):
        representation = self.locals.get(var)
        return representation.primitive_type if representation is not None else None

    def allocation(self, node):
        return self.allocations.get(node)

    def method_target(self, lvar):
        return self.method_targets[lvar.use()]


_BINARY = {
    ast.BinaryOperator.ADD,
    ast.BinaryOperator.SUBTRACT,
    ast.BinaryOperator.MULTIPLY,
    ast.BinaryOperator.BIT_AND,
    ast.BinaryOperator.BIT_OR,
    ast.BinaryOperator.BIT_XOR,
}
_COMPARE = {
    ast.ComparisonOperator.EQUAL,
    ast.ComparisonOperator.NOT_EQUAL,
    ast.ComparisonOperator.LESS,
    ast.ComparisonOperator.LESS_EQUAL,
    ast.ComparisonOperator.GREATER,
    ast.ComparisonOperator.GREATER_EQUAL,
}
_BINARY_NAMES = {
    **{f"%{operator.name.lower()}": operator for operator in ast.BinaryOperator},
    **{f"%i{operator.name.lower()}": operator for operator in ast.BinaryOperator},
}
_UNARY_NAMES = {
    f"%unary_{operator.name.lower()}": operator for operator in ast.UnaryOperator
}
_COMPARE_NAMES = {
    f"%compare_{operator.name.lower()}": operator for operator in ast.ComparisonOperator
}


class _Select(NodeVisitor):
    """Assign representations to one lambda's values (``_Select``)."""

    def __init__(self, plan, layouts, bindings, known_plans):
        self.plan, self.layouts, self.bindings = plan, layouts, bindings
        self.known_plans = known_plans
        self.block = None

    def value(self, lvar):
        if len(lvar.uses) != 1:
            raise NotNative
        value = self.plan.values.get(lvar.uses[0])
        if value is None:
            raise Deferred
        return value

    def visit_Node(self, node):
        raise NotNative

    def visit_Bind(self, node):
        return None

    def visit_Entry(self, node):
        return None

    def visit_Exit(self, node):
        if node.entry is not None:
            raise NotNative

    def visit_Enclose(self, node):
        return None

    def visit_Ref(self, node: Ref):
        leaf = node.leaf
        if isinstance(leaf, Constant):
            return leaf.value.accept(_Literal())
        if isinstance(leaf, LambdaVar):
            if leaf.indirect or leaf.home is not self.plan.function:
                raise NotNative
            result = self.plan.locals.get(leaf)
            if result is None:
                raise Deferred
            return result
        if isinstance(leaf, GlobalVar):
            identity = self.bindings.get(leaf.name)
            if identity is None:
                raise NotNative
            result = Class(leaf.name, identity, self.layouts[identity])
            self.plan.dependencies[leaf.name] = result
            return result
        if isinstance(leaf, (KnownFunction, Lambda)):
            return None
        raise NotNative

    def visit_Set(self, node: Set):
        var = node.var
        if not isinstance(var, LambdaVar) or node.value is None or var.indirect:
            raise NotNative
        value = self.value(node.value)
        old = self.plan.locals.get(var)
        if old is not None and old != value:
            raise NotNative
        self.plan.locals[var] = value

    def visit_Cast(self, node: Cast):
        value = self.value(node.value)
        expected = node.contract.accept(_AssertionContract(value))
        if value != expected:
            raise NotNative
        return expected

    def visit_If(self, node: If):
        self.value(node.test).truth()

    def visit_Return(self, node: Return):
        if not node.result.uses:
            raise NotNative
        for use in node.result.uses:
            value = self.plan.values.get(use)
            if value is None:
                raise Deferred
            # Instance escape requires owned-return representation conversion.
            value.boxable()
            if self.plan.returned is not None and self.plan.returned != value:
                raise NotNative
            self.plan.returned = value

    def visit_Combination(self, node: Combination):
        if node.kind is not CallKind.KNOWN:
            return self.construct(node)
        name = node.fun_info.name
        args = node.args
        if name in _BINARY_NAMES:
            if _BINARY_NAMES[name] not in _BINARY:
                raise NotNative
            self.value(args[0]).numeric()
            self.value(args[1]).numeric()
            return INT64
        if name in _UNARY_NAMES:
            operator = _UNARY_NAMES[name]
            value = self.value(args[0])
            if operator == ast.UnaryOperator.NOT:
                value.truth()
                return BOOL
            value.numeric()
            return INT64
        if name in _COMPARE_NAMES:
            if _COMPARE_NAMES[name] not in _COMPARE:
                raise NotNative
            self.value(args[0]).numeric()
            self.value(args[1]).numeric()
            return BOOL
        if name == "%getattr":
            return self.value(args[0]).field(node.info.name)
        if name == "%setattr":
            if self.value(args[0]).field(node.info.name) != self.value(args[1]):
                raise NotNative
            return None
        if name == "%resolve_method":
            method = self.value(args[0]).method(node.info.name)
            self.plan.method_targets[node] = method.target
            return method
        if name == "%call_resolved":
            return self.call_method(node)
        raise NotNative

    def call_method(self, node):
        if node.info.keywords:
            raise NotNative
        method = self.value(node.args[0])
        target = method.target
        arguments = node.args[1:]
        if not target.signature.positional_body:
            raise NotNative
        if len(target.vars) != len(arguments) + 1:
            raise NotNative
        for argument, contract in zip(
            arguments, target.signature.annotations[1:], strict=True
        ):
            if contract is None or self.value(argument) != contract.accept(_Contract()):
                raise NotNative
        self.plan.calls.add(target)
        if target.signature.return_annotation is not None:
            return target.signature.return_annotation.accept(_Contract())
        known = self.known_plans.get(target)
        if known is None:
            raise Deferred
        return known.returned

    def construct(self, node):
        if node.info is not None and node.info.keywords:
            raise NotNative
        callee = self.value(node.fun)
        instance = callee.constructor()
        initializer = instance.layout.methods["__init__"]
        if not initializer.signature.positional_body:
            raise NotNative
        if len(node.args) != len(initializer.vars) - 1:
            raise NotNative
        for argument, contract in zip(
            node.args, initializer.signature.annotations[1:], strict=True
        ):
            if contract is None or self.value(argument) != contract.accept(_Contract()):
                raise NotNative
        # A reused allocation site in a loop needs an escape/lifetime analysis
        # before borrowed instance references can safely replace owned values.
        pending, visited = list(self.block.succ), set()
        while pending:
            block = pending.pop()
            if block is self.block:
                raise NotNative
            if block not in visited:
                visited.add(block)
                pending.extend(block.succ)
        self.plan.calls.add(initializer)
        self.plan.allocations[node] = callee
        return instance


def class_bindings(lambdas, layouts) -> dict[str, str]:
    """Global names bound to a native class definition anywhere in the
    program, keyed to the class identity."""
    bindings = {}
    for lambda_ in lambdas:
        for block in lambda_.component.blocks():
            for node in block.nodes():
                if not isinstance(node, Set) or not isinstance(node.var, GlobalVar):
                    continue
                if node.value is None or len(node.value.uses) != 1:
                    continue
                producer = node.value.uses[0]
                if (
                    isinstance(producer, Combination)
                    and producer.kind is CallKind.KNOWN
                    and producer.fun_info.name == "%make_class"
                    and producer.info.class_id in layouts
                ):
                    bindings.setdefault(node.var.name, producer.info.class_id)
    return bindings


def select_native_plans(lambdas, facts, layouts) -> dict[Lambda, NativePlan]:
    """Choose native entries; remove any caller whose local edges lack a body."""
    bindings = class_bindings(lambdas, layouts)
    plans: dict[Lambda, NativePlan] = {}
    previous_count = -1
    while len(plans) != previous_count:
        previous_count = len(plans)
        for index, lambda_ in enumerate(lambdas):
            if (
                lambda_.kind is FunctionalKind.TOPLEVEL
                or lambda_.policy.speed < 3
                or lambda_ in plans
            ):
                continue
            try:
                plans[lambda_] = _select_function(
                    lambda_, index, facts[lambda_], layouts, bindings, plans
                )
            except NotNative:
                continue
    # Native recursion requires SBCL's stack/entry checks before local edges can
    # bypass the existing boxed frame guard. Keep recursive components boxed.
    cyclic = set()
    for lambda_, plan in plans.items():
        pending, seen = list(plan.calls), set()
        while pending:
            target = pending.pop()
            if target is lambda_:
                cyclic.add(lambda_)
                break
            if target not in seen and target in plans:
                seen.add(target)
                pending.extend(plans[target].calls)
    for lambda_ in cyclic:
        del plans[lambda_]
    while True:
        invalid = {
            lambda_
            for lambda_, plan in plans.items()
            if any(
                target not in plans or plans[target].dependencies
                for target in plan.calls
            )
        }
        if not invalid:
            return plans
        for lambda_ in invalid:
            del plans[lambda_]


def _select_function(lambda_, index, facts, layouts, bindings, plans) -> NativePlan:
    if lambda_.environment is not None and lambda_.environment.closure:
        raise NotNative
    if any(var.indirect for var in lambda_.locals):
        raise NotNative
    signature = lambda_.signature
    if signature.variadic:
        raise NotNative
    parameters = []
    for ordinal, contract in enumerate(signature.annotations):
        if ordinal == 0 and lambda_.owner_class is not None:
            parameters.append(
                Instance(lambda_.owner_class, layouts[lambda_.owner_class])
            )
        elif contract is not None:
            parameters.append(contract.accept(_Contract()))
        else:
            raise NotNative
    if len(parameters) != len(lambda_.vars):
        raise NotNative
    plan = NativePlan(
        lambda_,
        index,
        tuple(parameters),
        dict(zip(lambda_.vars, parameters, strict=True)),
    )
    by_name = {var.name: var for var in lambda_.locals}
    for name, contract in lambda_.local_annotations.items():
        representation = contract.accept(_LocalContract())
        if representation is not None and name in by_name:
            plan.locals[by_name[name]] = representation
    blocks = list(lambda_.component.blocks())
    for block in blocks:
        if block_exception_target(block) is not None:
            raise NotNative
    selector = _Select(plan, layouts, bindings, plans)
    previous = None
    while True:
        deferred = False
        for block in blocks:
            selector.block = block
            for node in block.nodes():
                try:
                    result = node.accept(selector)
                    if (
                        result is not None
                        and getattr(node, "lvar", None) is not None
                        or result is not None
                    ):
                        plan.values[node] = result
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
    types = facts.types
    constrained = _result_contracts(lambda_, blocks)
    int64 = EXPORTS["int64"].fact
    for block in blocks:
        for node in block.nodes():
            if (
                isinstance(node, Ref)
                and isinstance(node.leaf, LambdaVar)
                and types.get(node, UNKNOWN).may_be_unbound
            ):
                raise NotNative
            if not isinstance(node, Combination) or node.kind is not CallKind.KNOWN:
                continue
            name = node.fun_info.name
            arithmetic = name in _BINARY_NAMES and _BINARY_NAMES[name] not in {
                ast.BinaryOperator.BIT_AND,
                ast.BinaryOperator.BIT_OR,
                ast.BinaryOperator.BIT_XOR,
            }
            negation = (
                name in _UNARY_NAMES and _UNARY_NAMES[name] != ast.UnaryOperator.NOT
            )
            if (arithmetic or negation) and (
                node not in constrained and not types.get(node, UNKNOWN).fits(int64)
            ):
                raise NotNative
    return plan


def _result_contracts(lambda_, blocks) -> set:
    """Producers whose value an ``int64`` contract constrains: cast operands
    and, under an ``int64`` return annotation, returned values."""
    constrained = set()
    for block in blocks:
        for node in block.nodes():
            if isinstance(node, Cast) and node.contract is EXPORTS["int64"]:
                constrained.update(node.value.uses)
    if (
        lambda_.signature.return_annotation is EXPORTS["int64"]
        and lambda_.return_ is not None
    ):
        constrained.update(lambda_.return_.result.uses)
    return constrained


__all__ = [
    "BOOL",
    "INT64",
    "NONE",
    "Boolean",
    "Class",
    "Instance",
    "Int64",
    "Method",
    "NativePlan",
    "NotNative",
    "Nothing",
    "Representation",
    "class_bindings",
    "select_native_plans",
    "select_representations",
    "select_tn_representation",
]
