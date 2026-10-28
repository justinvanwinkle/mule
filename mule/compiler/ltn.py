"""Local template selection, following ``src/compiler/ltn.lisp``.

LTN annotates every lvar with how its value is delivered to IR2 (an
``IR2Lvar`` with the value's primitive type) and chooses, for every known
call, the template that implements it. Candidates come from the template
registry in cost order; the first whose operand restrictions, primitive-type
requirements, policy, and guard accept the call wins. Every known function
has a generic template that accepts any object, so selection cannot fail for
a known the backend supports.

Selection runs once per lambda per entry context: a guarded entry whose
parameters are proven fixnums sees narrower primitive types than the general
entry and therefore picks cheaper templates. The native entry context adds a
representation plan whose primitive types take precedence over the derived
value types (a machine integer local is ``signed64`` even where flow analysis
only knows it is an integer).
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import TYPE_CHECKING

from .ctype import BOTTOM, FIXNUM_MAX, FIXNUM_MIN, UNKNOWN, ValueType, ValueTypeVisitor
from .node import (
    CallKind,
    Cast,
    Combination,
    If,
    Lambda,
    LeafVisitor,
    NodeVisitor,
    Ref,
    Return,
    Set,
    linked,
)
from .seqtran import is_predictable_len_call, known_length_primitive
from .vmdef import operand_restriction_ok, primitive_type_ok, templates_for
from .vop import (
    BOOLEAN_T,
    BOXED_FLOAT,
    BOXED_I64,
    FIXNUM,
    IMMORTAL_T,
    NONE_T,
    IR2Lvar,
    IR2LvarKind,
    PrimitiveType,
    T,
)

if TYPE_CHECKING:
    from .callconv import CallConvention
    from .represent import NativePlan


class NoTemplate(Exception):
    """No registered template translates a known call under its policy."""


@dataclass(frozen=True, slots=True)
class FixnumGuard:
    """A conjunction of full-range fixnum tests on incoming parameters.

    Its rejection means at least one listed argument is not a fixnum. It
    does not narrow any individual argument and does not describe reassigned
    parameter locals. Operand proofs follow only single-definition copies of
    incoming values; reassigned locations do not inherit the predicate.
    """

    parameter_indices: tuple[int, ...]

    def rejects_operands(self, component, operands) -> bool:
        """Whether these operands contain every argument in the rejected test."""
        origins = {
            _incoming_parameter_origin(operand.tn, component.writes)
            for operand in operands
        }
        return bool(self.parameter_indices) and set(self.parameter_indices) <= origins

    def emit(self, builder, parameters) -> str:
        """Emit the complete conjunction over the supplied incoming values."""
        combined = None
        for index in self.parameter_indices:
            bits = builder.operation(f"ptrtoint ptr {parameters[index]} to i64")
            combined = (
                bits
                if combined is None
                else builder.operation(f"and i64 {combined}, {bits}")
            )
        if combined is None:
            raise ValueError("a fixnum guard requires at least one parameter")
        tag = builder.operation(f"and i64 {combined}, 1")
        return builder.operation(f"icmp ne i64 {tag}, 0")

    @classmethod
    def from_parameters(cls, parameters) -> FixnumGuard | None:
        """Return the complete guard only when no other condition is required."""
        indices = []
        for index, fact in enumerate(parameters):
            if fact is UNKNOWN:
                continue
            if (
                not fact.is_fixnum
                or fact.minimum != FIXNUM_MIN
                or fact.maximum != FIXNUM_MAX
            ):
                return None
            indices.append(index)
        return cls(tuple(indices)) if indices else None


def _incoming_parameter_origin(tn, writes) -> int | None:
    """Follow exact IR2 copies without treating mutable locals as SSA values."""
    visited = set()
    while tn not in visited:
        visited.add(tn)
        definitions = writes.get(tn, ())
        if len(definitions) != 1:
            return None
        writer, _ref = definitions[0]
        if writer.info.name == "move-argument":
            return writer.codegen_info
        if writer.info.name == "load-local":
            access = writer.codegen_info
            if not access.bound or access.allow_unbound:
                return None
        elif writer.info.name != "move":
            return None
        tn = writer.args[0].tn
    return None


@dataclass(slots=True, eq=False)
class EntryContext:
    """The facts one body is generated under: value types and predicted
    targets per node, the entry's parameter types, its return type, and the
    optional native body plan and independent internal calling convention."""

    id: str | None
    types: dict = field(default_factory=dict)
    targets: dict = field(default_factory=dict)
    parameters: tuple = ()
    return_type: ValueType = UNKNOWN
    plan: NativePlan | None = None
    convention: CallConvention | None = None
    rejected_fixnum_guard: FixnumGuard | None = None
    borrowed_parameters: frozenset[int] = frozenset()
    borrowed_callees: dict = field(default_factory=dict)
    element_contracts: dict = field(default_factory=dict)
    escape_facts: object = None
    base_escape_facts: object = None
    local_environments: object = None
    codegen_info: dict = field(default_factory=dict)
    in_bounds_nodes: frozenset = frozenset()

    def type_of(self, node) -> ValueType:
        return self.types.get(node, UNKNOWN)


@dataclass(frozen=True, slots=True)
class PhysicalSlotPlan:
    name: str
    index: int | None = None
    identity: str | None = None
    identities: tuple[str, ...] = ()


class _PhysicalLayouts(ValueTypeVisitor):
    def visit_ValueType(self, fact):
        return ()

    def visit_InstanceType(self, fact):
        return (fact.identity,) if not fact.may_be_unbound else ()

    def visit_UnionType(self, fact):
        if fact.may_be_unbound:
            return ()
        members = [member.accept(self) for member in fact.members]
        if not all(members):
            return ()
        return tuple(
            dict.fromkeys(identity for values in members for identity in values)
        )


_PHYSICAL_LAYOUTS = _PhysicalLayouts()


def _physical_slot_template(call, context):
    from . import vops

    name = call.info.name
    receiver = call.args[0].use()
    fact = context.type_of(receiver)
    index = fact.fixed_slot_index(name)
    identity = fact.instance_class()
    direct = False
    if (
        index is not None
        and context.id is not None
        and context.plan is None
        and context.convention is None
    ):
        leaf = receiver.accept(_PHYSICAL_PARAMETER)
        home = call.home_lambda
        if leaf is not None and home is not None and leaf in home.vars:
            position = home.vars.index(leaf)
            direct = (
                position < len(context.parameters)
                and context.parameters[position].fixed_slot_index(name) == index
            )
    identities = fact.accept(_PHYSICAL_LAYOUTS)
    context.codegen_info[call] = PhysicalSlotPlan(name, index, identity, identities)
    write = call.fun_info.name == "%physical_field_set"
    if direct:
        return vops.PHYSICAL_FIELD_WRITE if write else vops.PHYSICAL_FIELD_READ
    if index is not None and identities:
        return (
            vops.PHYSICAL_FIELD_WRITE_GUARDED
            if write
            else vops.PHYSICAL_FIELD_READ_GUARDED
        )
    return vops.PHYSICAL_FIELD_WRITE_NAME if write else vops.PHYSICAL_FIELD_READ_NAME


class _PhysicalParameter(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Ref(self, node):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf):
        return leaf if not leaf.indirect and not leaf.sets else None


_PHYSICAL_PARAMETER = _PhysicalParameter()


def primitive_type_of(value_type: ValueType) -> PrimitiveType:
    """The primitive type of values described by a derived value type
    (``primitive-type`` of a ctype)."""
    if value_type is UNKNOWN:
        return T
    return value_type.accept(_PRIMITIVE_TYPE_OF)


class _PrimitiveTypeOf(ValueTypeVisitor):
    def visit_ValueType(self, fact: ValueType) -> PrimitiveType:
        return IMMORTAL_T if fact.is_immortal else T

    def visit_IntegerType(self, fact: ValueType) -> PrimitiveType:
        if fact.is_fixnum:
            return FIXNUM
        if (
            not fact.may_be_unbound
            and fact.minimum is not None
            and fact.maximum is not None
            and -(1 << 63) <= fact.minimum
            and fact.maximum < 1 << 63
        ):
            return BOXED_I64
        return self.visit_ValueType(fact)

    visit_PythonIntegerType = visit_ValueType

    def visit_BooleanType(self, fact: ValueType) -> PrimitiveType:
        return BOOLEAN_T

    def visit_NoneType(self, fact: ValueType) -> PrimitiveType:
        return NONE_T

    def visit_ImmortalType(self, fact: ValueType) -> PrimitiveType:
        return IMMORTAL_T

    def visit_FloatType(self, fact: ValueType) -> PrimitiveType:
        return T if fact.may_be_unbound else BOXED_FLOAT

    visit_TextType = visit_ValueType
    visit_BytesType = visit_ValueType
    visit_ByteArrayType = visit_ValueType
    visit_FunctionType = visit_ValueType
    visit_TupleType = visit_ValueType


_PRIMITIVE_TYPE_OF = _PrimitiveTypeOf()


def ltn_analyze(lambda_: Lambda, context: EntryContext) -> object:
    """Annotate lvars and select templates under ``context``.

    The returned token identifies annotations from this analysis. The caller
    must keep the IR1 graph and context facts unchanged until IR2 conversion.
    """
    visitor = _Annotate(context)
    for block in linked(lambda_.component, lambda_, "component").blocks():
        for node in block.nodes():
            node.accept(visitor)
    return visitor.token


class _PredictableLenCalleeDest(NodeVisitor):
    def __init__(self, lvar, context: EntryContext):
        self.lvar = lvar
        self.context = context

    def visit_Node(self, node) -> bool:
        return False

    def visit_Combination(self, node: Combination) -> bool:
        if node.fun is not self.lvar or not is_predictable_len_call(node):
            return False
        arg = node.args[0]
        arg_type = self.context.type_of(arg.use()) if len(arg.uses) == 1 else UNKNOWN
        return known_length_primitive(arg_type) is not None


class _Annotate(NodeVisitor, LeafVisitor):
    def __init__(self, context: EntryContext):
        self.context = context
        self.token = object()
        self.lvar = None

    def lvar_types(self, lvar) -> tuple[PrimitiveType, ValueType | None]:
        plan = self.context.plan
        if plan is not None:
            chosen = plan.lvar_type(lvar)
            if chosen is not None:
                return chosen, None
        value_type = BOTTOM
        for producer in lvar.uses:
            value_type = value_type.join(self.context.type_of(producer))
        return primitive_type_of(value_type), value_type

    def lvar_ptype(self, lvar) -> PrimitiveType:
        return self.lvar_types(lvar)[0]

    def annotate_1_value_lvar(self, lvar) -> None:
        if lvar is None or lvar.info is not None:
            return
        primitive_type, value_type = self.lvar_types(lvar)
        lvar.info = IR2Lvar(
            IR2LvarKind.FIXED,
            primitive_type,
            value_type=value_type,
            ltn_token=self.token,
        )

    def annotate_delayed(self, lvar) -> None:
        if lvar is not None and lvar.info is None:
            lvar.info = IR2Lvar(IR2LvarKind.DELAYED, T)

    def visit_Leaf(self, leaf):
        raise TypeError(f"no IR2 delivery for a reference to {leaf!r}")

    # The callee of a known call and the lambda of ``%make_function`` are
    # compile-time operands, never materialized values.
    def visit_KnownFunction(self, leaf):
        self.annotate_delayed(self.lvar)

    def visit_Lambda(self, leaf):
        self.annotate_delayed(self.lvar)

    def visit_Constant(self, leaf):
        self.annotate_1_value_lvar(self.lvar)

    def visit_LambdaVar(self, leaf):
        self.annotate_1_value_lvar(self.lvar)

    def visit_GlobalVar(self, leaf):
        self.annotate_1_value_lvar(self.lvar)

    def visit_Ref(self, node: Ref):
        lvar = node.lvar
        if (
            self.context.plan is None
            and lvar is not None
            and lvar.dest is not None
            and lvar.dest.accept(_PredictableLenCalleeDest(lvar, self.context))
        ):
            self.annotate_delayed(lvar)
            return
        self.lvar = lvar
        node.leaf.accept(self)
        self.lvar = None

    def visit_Set(self, node: Set):
        self.annotate_1_value_lvar(node.lvar)

    def visit_Cast(self, node: Cast):
        self.annotate_1_value_lvar(node.lvar)

    def visit_If(self, node: If):
        return None

    def visit_Return(self, node: Return):
        self.annotate_1_value_lvar(node.result)

    def visit_Combination(self, node: Combination):
        if node.lvar is not None:
            self.annotate_1_value_lvar(node.lvar)
        if node.kind is CallKind.KNOWN:
            node.template = find_template_for_ltn_policy(node, self.context, self)


def find_template_for_ltn_policy(call: Combination, context, annotate) -> object:
    """Choose the cheapest template whose restrictions the call satisfies
    (``find-template-for-ltn-policy``)."""
    fun_info = linked(call.fun_info, call, "fun_info")
    if fun_info.name in ("%physical_field_ref", "%physical_field_set"):
        return _physical_slot_template(call, context)
    candidates = templates_for(fun_info)
    operands = []
    for arg in call.args:
        if arg.info is None:
            annotate.annotate_1_value_lvar(arg)
        if arg.info.kind is not IR2LvarKind.DELAYED:
            operands.append(arg.info.primitive_type)
    lvar = call.lvar
    if context.plan is not None:
        result = context.plan.node_type(call)
    elif lvar is None:
        result = primitive_type_of(context.type_of(call))
    elif lvar.info is not None:
        result = lvar.info.primitive_type
    else:
        result = annotate.lvar_ptype(lvar)
    policy = call.lexenv.policy
    raw = context.plan is not None
    for template in candidates:
        if (template.ltn_policy == "raw") != raw:
            continue
        if template.policy is not None and not template.policy(policy):
            continue
        if template.guard is not None and not template.guard(call):
            continue
        if not template_args_ok(template, operands):
            continue
        if not template_results_ok(template, result):
            continue
        return template
    raise NoTemplate(f"no template translates {fun_info.name} under {policy!r}")


def template_args_ok(template, operands) -> bool:
    if template.args and len(template.args) != len(operands):
        return False
    if template.arg_types and len(template.arg_types) != len(operands):
        return False
    for index, ptype in enumerate(operands):
        if template.args and not operand_restriction_ok(template.args[index], ptype):
            return False
        if template.arg_types and not primitive_type_ok(
            template.arg_types[index], ptype
        ):
            return False
    return True


def template_results_ok(template, result) -> bool:
    if result is None:
        return True
    if template.results and not operand_restriction_ok(template.results[0], result):
        return False
    if not template.result_types:
        return True
    return primitive_type_ok(template.result_types[0], result)


__all__ = [
    "EntryContext",
    "NoTemplate",
    "find_template_for_ltn_policy",
    "ltn_analyze",
    "primitive_type_of",
    "template_results_ok",
]
