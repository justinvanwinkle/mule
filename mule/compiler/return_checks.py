"""Checked definition-derived call results, following SBCL assert-call-type."""

from collections import defaultdict

from . import fndb
from .annotation_types import DeclaredReturnContract, InferredReturnContract
from .ctype import BOTTOM, UNKNOWN, InstanceType, ValueType, ValueTypeVisitor
from .ir1util import insert_cast_after
from .locall import global_bindings, made_function
from .node import CallKind, Cast, Combination, Lambda, LeafVisitor, NodeVisitor


def assert_inferred_results(compilation) -> bool:
    """Expose definition-derived results only through an ordinary checked Cast."""
    definitions = global_bindings(compilation.lambdas, unique_definitions=True)
    method_collector = _ModuleMethods()
    visited_components = set()
    for function in compilation.lambdas:
        comp = function.component
        if comp is None or comp in visited_components:
            continue
        visited_components.add(comp)
        for block in comp.blocks():
            for node in block.nodes():
                node.accept(method_collector)
    changed = False
    for function in compilation.lambdas:
        if not function.policy.infers_return_contracts:
            continue
        visitor = _AssertResult(
            function,
            compilation.facts,
            definitions,
            method_collector.methods,
            method_collector.shadowed,
        )
        for block in list(function.component.blocks()):
            for node in list(block.nodes()):
                node.accept(visitor)
        changed |= visitor.changed
    return changed


class _ModuleMethods(NodeVisitor):
    def __init__(self):
        self.methods: dict[tuple[str, str], list[Lambda]] = defaultdict(list)
        self.shadowed: set[tuple[str, str]] = set()

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is fndb.CLASS_STORE
            and len(node.args) == 2
            and node.info is not None
        ):
            key = (node.lexenv.lambda_.namespace, node.info.name)
            lam = (
                made_function(node.args[1].uses[0])
                if len(node.args[1].uses) == 1
                else None
            )
            if lam is not None and node.info.name not in (
                "__init_subclass__",
                "__class_getitem__",
            ):
                if lam not in self.methods[key]:
                    self.methods[key].append(lam)
            else:
                self.shadowed.add(key)


class _ResolvedMethodCall(NodeVisitor):
    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is fndb.RESOLVE_METHOD
            and len(node.args) == 1
            and node.info is not None
        ):
            return node.args[0], node.info.name
        return None


_RESOLVED_METHOD_CALL = _ResolvedMethodCall()


class _DefinitionTarget(NodeVisitor, LeafVisitor):
    def __init__(self, definitions):
        self.definitions = definitions

    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Ref(self, node):
        return node.leaf.accept(self)

    def visit_GlobalVar(self, leaf):
        return self.definitions.get(leaf.namespace, {}).get(leaf.name)


class _AssertionDestination(NodeVisitor):
    def __init__(self, call):
        self.call = call

    def visit_Node(self, node):
        return False

    def visit_Return(self, node):
        return True

    def visit_Cast(self, node):
        return node.declaration == ("inferred-return", self.call)


def _target_inferred_return_type(
    target: Lambda,
    target_facts,
    arguments: tuple[ValueType, ...],
    keywords: tuple[str | None, ...],
) -> ValueType:
    if not keywords and len(arguments) == len(target.vars):
        inferred = target.derive_return_type(arguments, keywords)
        if inferred is not UNKNOWN and inferred is not BOTTOM:
            return inferred
    if target_facts is not None:
        inferred = target_facts.return_type
        if not keywords:
            for variant in target_facts.variants:
                if (
                    len(arguments) == len(variant.parameters)
                    and all(
                        actual.fits(expected)
                        for actual, expected in zip(
                            arguments, variant.parameters, strict=True
                        )
                    )
                    and variant.return_type is not BOTTOM
                    and (inferred is UNKNOWN or variant.return_type.fits(inferred))
                ):
                    inferred = variant.return_type
        if inferred is not UNKNOWN and inferred is not BOTTOM:
            return inferred
    return target.derive_return_type(arguments, keywords)


def _target_return_contract(
    target: Lambda,
    target_facts,
    arguments: tuple[ValueType, ...],
    keywords: tuple[str | None, ...],
):
    annotation = target.signature.implementation_return_annotation
    declared = None if annotation is None else annotation.definition_contract()
    inferred = _target_inferred_return_type(target, target_facts, arguments, keywords)
    if inferred is BOTTOM or inferred.may_be_unbound:
        inferred = UNKNOWN
    else:
        inferred = inferred.widen_constant()
    if declared is not None:
        return DeclaredReturnContract(target.name, declared, inferred)
    if inferred is UNKNOWN:
        return None
    return InferredReturnContract(target.name, inferred)


class _ReceiverInstanceClasses(ValueTypeVisitor):
    def visit_ValueType(self, fact):
        return None

    def visit_NoneType(self, fact):
        return frozenset() if not fact.may_be_unbound else None

    def visit_InstanceType(self, fact):
        return frozenset({fact.identity}) if not fact.may_be_unbound else None

    def visit_UnionType(self, fact):
        if fact.may_be_unbound:
            return None
        classes: set[str] = set()
        for member in fact.members:
            member_classes = member.accept(self)
            if member_classes is None:
                return None
            classes.update(member_classes)
        return frozenset(classes)


_RECEIVER_INSTANCE_CLASSES = _ReceiverInstanceClasses()


class _AssertResult(NodeVisitor):
    def __init__(self, function, all_facts, definitions, module_methods, shadowed):
        self.function = function
        self.all_facts = all_facts
        self.facts = all_facts[function]
        self.target = _DefinitionTarget(definitions)
        self.module_methods = module_methods
        self.shadowed = shadowed
        self.changed = False

    def lvar_type(self, lvar):
        result = BOTTOM
        for producer in lvar.uses:
            result = result.join(self.facts.types.get(producer, UNKNOWN))
        return result

    def receiver_lvar_type(self, lvar):
        result = self.lvar_type(lvar)
        if (
            result is not UNKNOWN
            or self.function.method_class is None
            or not self.function.vars
        ):
            return result
        joined = BOTTOM
        for producer in lvar.uses:
            cand = self.facts.types.get(producer, UNKNOWN)
            if cand is UNKNOWN:
                for variant in self.facts.variants:
                    if (
                        variant.parameters
                        and variant.parameters[0].instance_class()
                        == self.function.method_class
                    ):
                        v_type = variant.types.get(producer, UNKNOWN)
                        if v_type is not UNKNOWN and v_type is not BOTTOM:
                            cand = v_type
                            break
            joined = joined.join(cand)
        return joined

    def _resolved_method_contract(self, call: Combination):
        if not call.args or len(call.args[0].uses) != 1:
            return None
        resolved = call.args[0].uses[0].accept(_RESOLVED_METHOD_CALL)
        if resolved is None:
            return None
        receiver_lvar, method_name = resolved
        receiver_type = self.receiver_lvar_type(receiver_lvar).widen_constant()
        arg_types = tuple(self.lvar_type(arg).widen_constant() for arg in call.args[1:])
        arguments = (receiver_type, *arg_types)
        keywords = call.info.keywords
        target = self.facts.targets.get(call)
        if target is None:
            variant_targets = {
                v.targets[call] for v in self.facts.variants if call in v.targets
            }
            if len(variant_targets) == 1:
                target = next(iter(variant_targets))
        if target is not None:
            target_lam = target.as_lambda()
            if (
                target_lam is None
                or target_lam.namespace != self.function.namespace
                or not target_lam.policy.infers_return_contracts
            ):
                return None
            return _target_return_contract(
                target_lam, self.all_facts.get(target_lam), arguments, keywords
            )
        method_key = (self.function.namespace, method_name)
        if method_key in self.shadowed:
            return None
        candidates = self.module_methods.get(method_key, ())
        receiver_classes = receiver_type.accept(_RECEIVER_INSTANCE_CLASSES)
        if not candidates or not receiver_classes:
            return None
        joined_inferred = BOTTOM
        shared_declared = None
        first_candidate = True
        for cand in candidates:
            if (
                cand.namespace != self.function.namespace
                or not cand.policy.infers_return_contracts
                or cand.call_order(keywords, len(arg_types), receiver=True) is None
            ):
                return None
            ann = cand.signature.implementation_return_annotation
            cand_declared = None if ann is None else ann.definition_contract()
            if first_candidate:
                shared_declared = cand_declared
                first_candidate = False
            elif cand_declared != shared_declared:
                shared_declared = None
            cand_args = (
                InstanceType(cand.method_class)
                if cand.method_class is not None
                else (
                    receiver_type
                    if receiver_type.instance_class() is not None
                    else UNKNOWN
                ),
                *arg_types,
            )
            cand_inferred = _target_inferred_return_type(
                cand, self.all_facts.get(cand), cand_args, keywords
            )
            if (
                cand_inferred is UNKNOWN
                or cand_inferred is BOTTOM
                or cand_inferred.may_be_unbound
            ):
                joined_inferred = UNKNOWN
            elif joined_inferred is not UNKNOWN:
                joined_inferred = joined_inferred.join(cand_inferred.widen_constant())
        if shared_declared is not None:
            return DeclaredReturnContract(
                method_name,
                shared_declared,
                UNKNOWN if joined_inferred is BOTTOM else joined_inferred,
            )
        if joined_inferred is UNKNOWN or joined_inferred is BOTTOM:
            return None
        return InferredReturnContract(method_name, joined_inferred)

    def visit_Combination(self, call):
        if not call.lexenv.policy.infers_return_contracts:
            return
        result = call.lvar
        if result is None or result.dest is None or len(result.uses) != 1:
            return
        if result.dest.accept(_AssertionDestination(call)):
            return
        if (
            call.kind is CallKind.KNOWN
            and call.fun_info is fndb.CALL_RESOLVED
            and not call.proven
        ):
            contract = self._resolved_method_contract(call)
            if contract is None:
                return
        elif call.kind in (CallKind.FULL, CallKind.GUARDED) and not call.proven:
            if len(call.fun.uses) != 1:
                return
            target = call.fun.use().accept(self.target)
            if (
                target is None
                or target.namespace != self.function.namespace
                or not target.policy.infers_return_contracts
            ):
                return
            location = call.info.location
            if target.code is None or location is None:
                return
            if (
                target.code.filename != location[0]
                or target.code.firstlineno > location[1]
            ):
                return
            arguments = tuple(self.lvar_type(arg).widen_constant() for arg in call.args)
            contract = _target_return_contract(
                target, self.all_facts.get(target), arguments, call.info.keywords
            )
            if contract is None:
                return
        else:
            return
        checked = contract.fact
        if checked is UNKNOWN or checked is BOTTOM or checked.may_be_unbound:
            return

        def make_cast(value):
            return Cast(
                call.lexenv,
                call.source,
                value,
                contract,
                call.info.location,
                int(contract.requires_check(call.lexenv.policy.checks_contracts)),
                declaration=("inferred-return", call),
            )

        insert_cast_after(call, make_cast)
        call.block.component.reoptimize = True
        self.changed = True
