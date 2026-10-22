"""Checked definition-derived call results, following SBCL assert-call-type."""

from .annotation_types import DeclaredReturnContract, InferredReturnContract
from .ctype import BOTTOM, UNKNOWN
from .ir1util import insert_cast_after
from .locall import global_bindings
from .node import CallKind, Cast, LeafVisitor, NodeVisitor


def assert_inferred_results(compilation) -> bool:
    """Expose definition-derived results only through an ordinary checked Cast."""
    definitions = global_bindings(compilation.lambdas, unique_definitions=True)
    changed = False
    for function in compilation.lambdas:
        if not function.policy.infers_return_contracts:
            continue
        visitor = _AssertResult(function, compilation.facts[function], definitions)
        for block in list(function.component.blocks()):
            for node in list(block.nodes()):
                node.accept(visitor)
        changed |= visitor.changed
    return changed


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


class _AssertResult(NodeVisitor):
    def __init__(self, function, facts, definitions):
        self.function = function
        self.facts = facts
        self.target = _DefinitionTarget(definitions)
        self.changed = False

    def lvar_type(self, lvar):
        result = BOTTOM
        for producer in lvar.uses:
            result = result.join(self.facts.types.get(producer, UNKNOWN))
        return result

    def visit_Combination(self, call):
        if call.kind not in (CallKind.FULL, CallKind.GUARDED) or call.proven:
            return
        if not call.lexenv.policy.infers_return_contracts:
            return
        result = call.lvar
        if result is None or result.dest is None or len(result.uses) != 1:
            return
        if result.dest.accept(_AssertionDestination(call)):
            return
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
        if target.code.filename != location[0] or target.code.firstlineno > location[1]:
            return
        annotation = target.signature.implementation_return_annotation
        declared = None if annotation is None else annotation.definition_contract()
        if declared is not None:
            contract = DeclaredReturnContract(target.name, declared)
        else:
            arguments = tuple(self.lvar_type(arg).widen_constant() for arg in call.args)
            inferred = target.derive_return_type(arguments, call.info.keywords)
            if inferred is UNKNOWN or inferred is BOTTOM or inferred.may_be_unbound:
                return
            contract = InferredReturnContract(target.name, inferred)
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
