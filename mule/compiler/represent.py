"""Representation selection, following ``src/compiler/represent.lisp``.

Two decisions live here:

- ``select_representations`` assigns every TN of an IR2 component a storage
  class from its primitive type's candidates by the cost of the VOP operands
  that read and write it (``select-tn-representation``). A local whose every
  writer delivers an immortal value becomes an immediate (never released).
  Float locals and their assignment-connected temporaries can share raw
  storage when every producer and consumer permits it without coercion.
  Object boundaries keep the entire connected group boxed to preserve identity.

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

from collections.abc import Callable
from dataclasses import dataclass, field
from typing import TYPE_CHECKING

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
    LeafVisitor,
    NodeVisitor,
    Ref,
    Return,
    Set,
    linked,
)
from .vop import (
    BOOLEAN_T,
    BOXED_FLOAT,
    CLASS_T,
    DESCRIPTOR,
    DOUBLE,
    FIXNUM,
    FLOAT64,
    FLOAT_BOX_LAYOUT,
    IMMORTAL_T,
    NONE_T,
    RANGE,
    RANGE_T,
    SIGNED64,
    VOP,
    BoxLayout,
    IR2Component,
    PrimitiveType,
    T,
    instance_type,
)

if TYPE_CHECKING:
    from .native_protocol import Layout

IMMORTAL_TYPES = (FIXNUM, BOOLEAN_T, NONE_T, IMMORTAL_T)


# Storage-class selection for one IR2 component.


def select_representations(component: IR2Component) -> None:
    raw = component.context.plan is not None
    reads = component.reads
    writes = component.writes
    if not raw:
        for tn in component.tns:
            if tn.leaf is not None:
                tn.primitive_type = _variable_type(tn, component, writes)
    shared = _select_float_move_representations(component) if not raw else {}
    for tn in component.tns:
        tn.sc = shared.get(tn) or select_tn_representation(
            tn,
            reads=reads.get(tn, ()),
            writes=writes.get(tn, ()),
            raw=raw,
            convention=component.context.convention,
        )


def _variable_type(tn, component, writes) -> PrimitiveType:
    """Preserve float storage candidates or the union of immortal writers."""
    context = component.context
    written_types = set()
    for vop, _ref in writes.get(tn, ()):
        name = vop.info.name
        if name == "move":
            written = vop.args[0].tn.primitive_type
        elif name == "move-argument":
            written = primitive_type_of(context.parameters[vop.codegen_info])
        elif name == "load-closure":
            written = tn.primitive_type
        elif name == "unbind-local":
            continue
        else:
            written = T
        written_types.add(written)
    if written_types == {BOXED_FLOAT}:
        return BOXED_FLOAT
    if written_types.issubset(IMMORTAL_TYPES):
        return IMMORTAL_T
    return T


def _representation_move(vop) -> bool:
    """Whether this VOP copies a value without checking or changing its type."""
    if vop.info.name == "move":
        return True
    if vop.info.name == "load-local":
        access = vop.codegen_info
        return access.bound and not access.allow_unbound
    return False


def _select_float_move_representations(component):
    """Choose one raw storage class for a closed group of float copies.

    As in SBCL's move costing, copies inherit their operands' representation
    rather than imposing an object boundary. Without coercion VOPs, every
    connected TN must admit the same class. Any identity-bearing object use
    therefore keeps all aliases boxed, including copies across loop backedges.
    """
    selected = {}
    visited = set()
    for root in component.tns:
        if (
            root in visited
            or root.leaf is None
            or root.primitive_type is not BOXED_FLOAT
        ):
            continue
        group, candidates = _move_group_representations(component, root)
        visited.update(group)
        if candidates:
            selected.update((tn, candidates[0]) for tn in group)
    return selected


def can_use_double(component, tn, *, ignore=()) -> bool:
    """Whether every value and boundary in a copy group admits raw doubles.

    ``ignore`` omits boundary VOPs whose convention is currently being selected,
    such as the producer of a numeric call result or an argument initialization.
    The caller must replace each omitted boundary with a compatible convention.
    All other uses, including identity-sensitive uses, retain their restrictions.
    """
    _group, candidates = _move_group_representations(component, tn, ignore=ignore)
    return DOUBLE in candidates


def fresh_float_returns(component) -> bool:
    """Whether object returns can be reconstructed without losing aliases."""
    found = False
    for block in component.blocks:
        for vop in block.vops:
            if vop.info.name != "return":
                continue
            found = True
            group, candidates = _move_group_representations(
                component, vop.args[0].tn, ignore=(vop,)
            )
            if DOUBLE not in candidates:
                return False
            for tn in group:
                for writer, _ref in component.writes.get(tn, ()):
                    if _representation_move(writer):
                        continue
                    if not writer.info.produces_fresh(writer):
                        return False
    return found


def fresh_box_returns(component: IR2Component, layout: BoxLayout) -> bool:
    """Whether an object entry returns fresh leaf storage without another escape."""
    if component.context.plan is not None or component.context.convention is not None:
        return False
    found = False
    for block in component.blocks:
        for vop in block.vops:
            if vop.info.name != "return":
                continue
            found = True
            group, _candidates = _move_group_representations(component, vop.args[0].tn)
            if not _private_box_group(
                component, group, layout, None, allow_return=True
            ):
                return False
    return found


def mark_private_boxes(
    component: IR2Component,
    *,
    fresh_result: Callable[[VOP], BoxLayout | None] | None = None,
) -> None:
    """Prove private ownership for complete groups of leaf-box copies.

    Every origin must allocate fresh leaf storage, and every non-copy reader
    must explicitly promise not to capture the reference. A resolver can supply
    an interprocedural fresh-result proof for an otherwise opaque producer.
    Unknown origins and uses retain ordinary shared-object ownership.
    """
    for tn in component.tns:
        tn.private_box = None
    if component.context.plan is not None:
        return
    visited = set()
    for root in component.tns:
        layout = root.primitive_type.boxed_layout
        if root in visited or root.sc is not DESCRIPTOR or layout is None:
            continue
        group, _candidates = _move_group_representations(component, root)
        visited.update(group)
        if _private_box_group(component, group, layout, fresh_result):
            for tn in group:
                tn.private_box = layout


def _private_box_group(
    component, group, layout, fresh_result, *, allow_return=False
) -> bool:
    found_origin = False
    for tn in group:
        if tn.sc is not DESCRIPTOR or tn.primitive_type.boxed_layout is not layout:
            return False
        for reader, ref in component.reads.get(tn, ()):
            if _representation_move(reader):
                continue
            if allow_return and reader.info.name == "return":
                continue
            if reader.info.arg_escapes is None or len(reader.info.arg_escapes) != len(
                reader.args
            ):
                return False
            if reader.info.captures_argument(reader.args.index(ref), vop=reader):
                return False
        for writer, _ref in component.writes.get(tn, ()):
            if _representation_move(writer) or writer.info.name == "unbind-local":
                continue
            if writer.info.produces_fresh(writer):
                found_origin = True
                continue
            if fresh_result is not None and fresh_result(writer) is layout:
                found_origin = True
                continue
            return False
    return found_origin


def _move_group_representations(component, root, *, ignore=()):
    group = set()
    pending = [root]
    ignored = frozenset(ignore)
    candidates = [sc for sc in root.primitive_type.scs if sc.raw]
    convention = component.context.convention
    while pending:
        tn = pending.pop()
        if tn in group:
            continue
        group.add(tn)
        candidates = [sc for sc in candidates if tn.primitive_type.admits(sc)]
        for vop, ref in (
            *component.reads.get(tn, ()),
            *component.writes.get(tn, ()),
        ):
            if vop in ignored:
                continue
            if _representation_move(vop):
                pending.extend(operand.tn for operand in (*vop.args, *vop.results))
                continue
            restriction = _operand_restriction(vop, ref, convention=convention)
            if restriction is not None:
                candidates = [sc for sc in candidates if sc in restriction]
    return group, candidates


def select_tn_representation(tn, *, reads=(), writes=(), raw=False, convention=None):
    """The cheapest storage class among the primitive type's candidates,
    counting one coercion per operand whose restriction excludes it. A native
    body holds every value that has a machine representation in it."""
    if tn.primitive_type is RANGE_T:
        return RANGE
    candidates = tn.primitive_type.scs
    if raw and any(sc.raw for sc in candidates):
        candidates = [sc for sc in candidates if sc.raw]
    if len(candidates) == 1 or (not reads and not writes):
        return candidates[0]
    best, best_cost = None, None
    for sc in candidates:
        cost = 0
        for vop, ref in (*reads, *writes):
            restriction = _operand_restriction(vop, ref, raw=raw, convention=convention)
            if restriction is not None and sc not in restriction:
                cost += 1
        if cost == 0:
            return sc
        if best_cost is None or cost < best_cost:
            best, best_cost = sc, cost
    return best


def _operand_restriction(vop, ref, *, raw=False, convention=None):
    restriction = (
        convention.operand_restriction(vop, ref)
        if convention is not None and not raw
        else None
    )
    if restriction is None:
        restriction = _restriction(vop, ref)
    return ref.tn.primitive_type.operand_restriction(restriction, raw=raw)


def _restriction(vop, ref):
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

    def python_float(self):
        raise NotNative

    def python_real(self):
        raise NotNative

    def field(self, name):
        raise NotNative

    def method(self, name):
        raise NotNative

    def constructor(self):
        raise NotNative

    def truth(self):
        raise NotNative

    def box(self, gen, value):
        """An owned object for a raw value; representations that cannot
        leave a native body raise ``NotNative`` during plan selection."""
        raise NotNative

    def parameter(self, gen, value):
        raise NotNative

    def can_adapt_return(self, component) -> bool:
        return True


class Int64(Representation):
    def python_real(self):
        return self

    def numeric(self):
        return self

    def python_integer(self):
        return self

    def truth(self):
        return True

    def box(self, gen, value):
        return gen.operation(f"call ptr @mule_box_i64(i64 {value})")

    def parameter(self, gen, value):
        slot = gen.operation("alloca i64")
        status = gen.operation(f"call i32 @mule_unbox_i64(ptr {value}, ptr {slot})")
        gen.check_status(status)
        return gen.operation(f"load i64, ptr {slot}")


class Float64(Representation):
    def python_real(self):
        return self

    llvm_type = "double"
    zero = "0.0"
    primitive_type = FLOAT64

    def numeric(self):
        return self

    def python_float(self):
        return self

    def truth(self):
        return True

    def box(self, gen, value):
        return gen.operation(f"call ptr @{FLOAT_BOX_LAYOUT.symbol()}(double {value})")

    def can_adapt_return(self, component) -> bool:
        return fresh_float_returns(component)

    def parameter(self, gen, value):
        slot = gen.operation("alloca double")
        status = gen.operation(f"call i32 @mule_unbox_f64(ptr {value}, ptr {slot})")
        gen.check_status(status)
        return gen.operation(f"load double, ptr {slot}")


class Boolean(Representation):
    def python_real(self):
        return self

    llvm_type = "i1"
    zero = "false"
    primitive_type = BOOLEAN_T

    def python_integer(self):
        return self

    def truth(self):
        return True

    def box(self, gen, value):
        wide = gen.operation(f"zext i1 {value} to i32")
        return gen.operation(f"call ptr @mule_bool(i32 {wide})")


class Nothing(Representation):
    primitive_type = NONE_T

    def box(self, gen, value):
        return "@mule.none"


@dataclass(frozen=True)
class Instance(Representation):
    layout_id: str
    layout: Layout = field(compare=False)
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
    layout: Layout = field(compare=False)
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


INT64, FLOAT, BOOL, NONE = Int64(), Float64(), Boolean(), Nothing()


class _Probe:
    """A generator stand-in for representation checks during planning."""

    def operation(self, text):
        return "0"


class _Contract:
    def visit_Contract(self, contract):
        raise NotNative

    def visit_IntegerContract(self, contract):
        if contract.bits != 64 or not contract.signed:
            raise NotNative
        return INT64

    def visit_FloatContract(self, contract):
        # The runtime stores every float as binary64; narrower widths would
        # need their own rounding at each operation.
        if contract.bits != 64:
            raise NotNative
        return FLOAT

    visit_PythonIntegerContract = visit_Contract
    visit_PythonFloatContract = visit_Contract


class _LocalContract(_Contract):
    def visit_PythonFloatContract(self, contract):
        return None

    def visit_PythonIntegerContract(self, contract):
        # Unbounded Python int is a semantic constraint, not a machine width.
        # Select storage from the local's definitions and arithmetic proofs.
        return None


class _AssertionContract(_Contract):
    def __init__(self, representation):
        self.representation = representation

    def visit_PythonIntegerContract(self, contract):
        return self.representation.python_integer()

    def visit_PythonFloatContract(self, contract):
        return self.representation.python_real()

    def visit_FloatContract(self, contract):
        if contract.bits != 64:
            raise NotNative
        return self.representation.python_float()


class _Literal(ast.Visitor):
    def visit_Node(self, node):
        raise NotNative

    def visit_IntegerLiteral(self, node):
        if not -(1 << 63) <= node.value < (1 << 63):
            raise NotNative
        return INT64

    def visit_FloatLiteral(self, node):
        return FLOAT

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
    dependencies: dict[GlobalVar, Class] = field(default_factory=dict)
    calls: set[Lambda] = field(default_factory=set)
    allocations: dict[object, Class] = field(default_factory=dict)
    method_targets: dict[object, Lambda] = field(default_factory=dict)

    @property
    def entry(self):
        return f"@mule.native.{self.index}"

    def lvar_type(self, lvar):
        """The primitive type every producer of ``lvar`` delivers, or None
        when a producer has no representation or they disagree."""
        types = {self.node_type(use) for use in lvar.uses}
        if len(types) != 1:
            return None
        return types.pop()

    def node_type(self, node):
        representation = self.values.get(node)
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
_FLOAT_BINARY = {
    ast.BinaryOperator.ADD,
    ast.BinaryOperator.SUBTRACT,
    ast.BinaryOperator.MULTIPLY,
    ast.BinaryOperator.DIVIDE,
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


class _Select(NodeVisitor, LeafVisitor):
    """Assign representations to one lambda's values (``_Select``). A
    reference's representation is its leaf's."""

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
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        raise NotNative

    def visit_Constant(self, leaf: Constant):
        return leaf.value.accept(_Literal())

    def visit_LambdaVar(self, leaf: LambdaVar):
        if leaf.indirect or leaf.home is not self.plan.function:
            raise NotNative
        result = self.plan.locals.get(leaf)
        if result is None:
            raise Deferred
        return result

    def visit_GlobalVar(self, leaf: GlobalVar):
        identity = self.bindings.get(leaf.name)
        if identity is None:
            raise NotNative
        result = Class(leaf.name, identity, self.layouts[identity])
        self.plan.dependencies[leaf] = result
        return result

    def visit_KnownFunction(self, leaf: KnownFunction):
        return None

    def visit_Lambda(self, leaf: Lambda):
        return None

    def visit_Set(self, node: Set):
        node.var.accept(_AssignLocal(self, node))

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
            value.box(_Probe(), "0")
            if self.plan.returned is not None and self.plan.returned != value:
                raise NotNative
            self.plan.returned = value

    def visit_Combination(self, node: Combination):
        if node.kind is not CallKind.KNOWN:
            return self.construct(node)
        name = linked(node.fun_info, node, "known function").name
        args = node.args
        if name in _BINARY_NAMES:
            return self.binary(_BINARY_NAMES[name], args)
        if name in _UNARY_NAMES:
            operator = _UNARY_NAMES[name]
            value = self.value(args[0])
            if operator == ast.UnaryOperator.NOT:
                value.truth()
                return BOOL
            value.numeric()
            if operator == ast.UnaryOperator.INVERT and value is FLOAT:
                raise NotNative
            return value if value is FLOAT else INT64
        if name in _COMPARE_NAMES:
            if _COMPARE_NAMES[name] not in _COMPARE:
                raise NotNative
            self.numeric_operands(args)
            return BOOL
        if name == "%getattr":
            return self.value(args[0]).field(linked(node.info, node, "attribute").name)
        if name == "%setattr":
            attribute = linked(node.info, node, "attribute").name
            if self.value(args[0]).field(attribute) != self.value(args[1]):
                raise NotNative
            return None
        if name == "%resolve_method":
            method = self.value(args[0]).method(linked(node.info, node, "method").name)
            self.plan.method_targets[node] = method.target
            return method
        if name == "%call_resolved":
            return self.call_method(node)
        raise NotNative

    def numeric_operands(self, args):
        return self.value(args[0]).numeric(), self.value(args[1]).numeric()

    def binary(self, operator, args):
        left, right = self.numeric_operands(args)
        if FLOAT in (left, right):
            # Python rounds the integer operand to a float before a mixed
            # operation, as ``sitofp`` does.
            if operator not in _FLOAT_BINARY:
                raise NotNative
            return FLOAT
        if operator not in _BINARY:
            raise NotNative
        return INT64

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
        annotation = target.signature.implementation_return_annotation
        if annotation is not None:
            return annotation.accept(_Contract())
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
        pending, visited = list(linked(self.block, self, "current block").succ), set()
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


class _AssignLocal(LeafVisitor):
    """A native body assigns only its own direct locals."""

    def __init__(self, selector: _Select, node: Set):
        self.selector = selector
        self.node = node

    def visit_Leaf(self, leaf):
        raise NotNative

    def visit_LambdaVar(self, leaf: LambdaVar):
        if self.node.value is None or leaf.indirect:
            raise NotNative
        plan = self.selector.plan
        value = self.selector.value(self.node.value)
        old = plan.locals.get(leaf)
        if old is not None and old != value:
            raise NotNative
        plan.locals[leaf] = value


def class_bindings(lambdas, layouts) -> dict[str, str]:
    """Global names bound to a native class definition anywhere in the
    program, keyed to the class identity."""
    if not layouts:
        return {}
    collector = _ClassBindings(layouts)
    for lambda_ in lambdas:
        for block in lambda_.component.blocks():
            for node in block.nodes():
                node.accept(collector)
    return collector.bindings


class _ClassBindings(NodeVisitor, LeafVisitor):
    """Record global assignments whose single producer makes a native class."""

    def __init__(self, layouts):
        self.layouts = layouts
        self.bindings: dict[str, str] = {}
        self.node: Set | None = None

    def visit_Set(self, node: Set):
        self.node = node
        node.var.accept(self)
        self.node = None

    def visit_GlobalVar(self, leaf: GlobalVar):
        node = linked(self.node, self, "assignment")
        value = node.value
        if value is None or len(value.uses) != 1:
            return
        class_id = value.uses[0].accept(self)
        if class_id is not None:
            self.bindings.setdefault(leaf.name, class_id)

    def visit_Combination(self, node: Combination):
        if (
            self.node is None
            or node.kind is not CallKind.KNOWN
            or linked(node.fun_info, node, "known function").name != "%make_class"
        ):
            return None
        class_id = linked(node.info, node, "class definition").class_id
        return class_id if class_id in self.layouts else None


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
                    if result is not None:
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
    proof = _MachineProof(plan, facts.types, _result_contracts(lambda_, blocks))
    for block in blocks:
        for node in block.nodes():
            node.accept(proof)
    return plan


class _MachineProof(NodeVisitor, LeafVisitor):
    """Reject a plan whose locals may be read unbound or whose integer
    arithmetic is not proven to stay within ``int64``."""

    def __init__(self, plan, types, constrained):
        self.plan, self.types, self.constrained = plan, types, constrained
        self.int64 = EXPORTS["int64"].fact

    def visit_Ref(self, node: Ref):
        if self.types.get(node, UNKNOWN).may_be_unbound:
            node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        raise NotNative

    def visit_Combination(self, node: Combination):
        if node.kind is not CallKind.KNOWN:
            return
        name = linked(node.fun_info, node, "known function").name
        arithmetic = name in _BINARY_NAMES and _BINARY_NAMES[name] not in {
            ast.BinaryOperator.BIT_AND,
            ast.BinaryOperator.BIT_OR,
            ast.BinaryOperator.BIT_XOR,
        }
        negation = name in _UNARY_NAMES and _UNARY_NAMES[name] != ast.UnaryOperator.NOT
        if (
            (arithmetic or negation)
            and self.plan.values.get(node) is not FLOAT
            and node not in self.constrained
            and not self.types.get(node, UNKNOWN).fits(self.int64)
        ):
            raise NotNative


class _Int64Operands(NodeVisitor):
    def __init__(self, constrained: set):
        self.constrained = constrained

    def visit_Cast(self, node: Cast):
        if node.contract is EXPORTS["int64"]:
            self.constrained.update(linked(node.value, node, "value").uses)


def _result_contracts(lambda_, blocks) -> set:
    """Producers whose value an ``int64`` contract constrains: cast operands
    and, under an implementation ``int64`` return constraint, returned values."""
    constrained = set()
    operands = _Int64Operands(constrained)
    for block in blocks:
        for node in block.nodes():
            node.accept(operands)
    if (
        lambda_.signature.implementation_return_annotation is EXPORTS["int64"]
        and lambda_.return_ is not None
    ):
        constrained.update(lambda_.return_.result.uses)
    return constrained


__all__ = [
    "BOOL",
    "FLOAT",
    "INT64",
    "NONE",
    "Boolean",
    "Class",
    "Float64",
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
