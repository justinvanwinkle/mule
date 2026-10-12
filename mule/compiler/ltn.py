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

from .ctype import (
    UNKNOWN,
    BooleanType,
    ImmortalType,
    IntegerType,
    NoneType,
    ValueType,
)
from .node import (
    CallKind,
    Cast,
    Combination,
    Constant,
    GlobalVar,
    If,
    KnownFunction,
    Lambda,
    LambdaVar,
    NodeVisitor,
    Ref,
    Return,
    Set,
    linked,
)
from .vmdef import operand_restriction_ok, primitive_type_ok, templates_for
from .vop import (
    BOOLEAN_T,
    FIXNUM,
    IMMORTAL_T,
    NONE_T,
    IR2Lvar,
    IR2LvarKind,
    PrimitiveType,
    T,
)

if TYPE_CHECKING:
    from .represent import NativePlan


class NoTemplate(Exception):
    """No registered template translates a known call under its policy."""


@dataclass(slots=True, eq=False)
class EntryContext:
    """The facts one body is generated under: value types and predicted
    targets per node, the entry's parameter types, its return type, and the
    optional native representation plan."""

    id: str | None
    types: dict = field(default_factory=dict)
    targets: dict = field(default_factory=dict)
    parameters: tuple = ()
    return_type: ValueType = UNKNOWN
    plan: NativePlan | None = None

    def type_of(self, node) -> ValueType:
        return self.types.get(node, UNKNOWN)


def primitive_type_of(value_type: ValueType) -> PrimitiveType:
    """The primitive type of values described by a derived value type
    (``primitive-type`` of a ctype)."""
    if isinstance(value_type, BooleanType):
        return BOOLEAN_T
    if isinstance(value_type, NoneType):
        return NONE_T
    if isinstance(value_type, IntegerType) and value_type.is_fixnum:
        return FIXNUM
    if isinstance(value_type, ImmortalType) or value_type.is_immortal:
        return IMMORTAL_T
    return T


def ltn_analyze(lambda_: Lambda, context: EntryContext) -> None:
    """Annotate every lvar of ``lambda_`` and select templates for its known
    calls under ``context``."""
    visitor = _Annotate(context)
    for block in linked(lambda_.component, lambda_, "component").blocks():
        for node in block.nodes():
            node.accept(visitor)


class _Annotate(NodeVisitor):
    def __init__(self, context: EntryContext):
        self.context = context

    def lvar_ptype(self, lvar) -> PrimitiveType:
        plan = self.context.plan
        if plan is not None:
            chosen = plan.lvar_type(lvar)
            if chosen is not None:
                return chosen
        uses = lvar.uses
        if len(uses) != 1:
            return T
        return primitive_type_of(self.context.type_of(uses[0]))

    def annotate_1_value_lvar(self, lvar) -> None:
        if lvar is None or lvar.info is not None:
            return
        lvar.info = IR2Lvar(IR2LvarKind.FIXED, self.lvar_ptype(lvar))

    def annotate_delayed(self, lvar) -> None:
        if lvar is not None and lvar.info is None:
            lvar.info = IR2Lvar(IR2LvarKind.DELAYED, T)

    def visit_Ref(self, node: Ref):
        leaf = node.leaf
        if isinstance(leaf, (KnownFunction, Lambda)):
            # The callee of a known call and the lambda of ``%make_function``
            # are compile-time operands, never materialized values.
            self.annotate_delayed(node.lvar)
            return
        if isinstance(leaf, (Constant, LambdaVar, GlobalVar)):
            self.annotate_1_value_lvar(node.lvar)
            return
        raise TypeError(f"no IR2 delivery for a reference to {leaf!r}")

    def visit_Set(self, node: Set):
        self.annotate_1_value_lvar(node.lvar)

    def visit_Cast(self, node: Cast):
        self.annotate_1_value_lvar(node.lvar)

    def visit_If(self, node: If):
        return None

    def visit_Return(self, node: Return):
        self.annotate_1_value_lvar(node.result)

    def visit_Combination(self, node: Combination):
        if node.kind is CallKind.KNOWN:
            node.template = find_template_for_ltn_policy(node, self.context, self)
        if node.lvar is not None:
            self.annotate_1_value_lvar(node.lvar)


def find_template_for_ltn_policy(call: Combination, context, annotate) -> object:
    """Choose the cheapest template whose restrictions the call satisfies
    (``find-template-for-ltn-policy``)."""
    fun_info = linked(call.fun_info, call, "fun_info")
    candidates = templates_for(fun_info)
    operands = [
        annotate.lvar_ptype(arg)
        for arg in call.args
        if arg.info is None or arg.info.kind is not IR2LvarKind.DELAYED
    ]
    result = annotate.lvar_ptype(call.lvar) if call.lvar is not None else None
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
    if result is None or not template.result_types:
        return True
    return primitive_type_ok(template.result_types[0], result)


__all__ = [
    "EntryContext",
    "NoTemplate",
    "find_template_for_ltn_policy",
    "ltn_analyze",
    "primitive_type_of",
]
