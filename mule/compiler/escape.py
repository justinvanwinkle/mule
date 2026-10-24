"""Escape analysis: which values cannot outlive the scope that holds them.

SBCL asks this question in ``find-lvar-dynamic-extents``
(``src/compiler/envanal.lisp``) for values declared ``dynamic-extent``:
``ref-good-for-dx-p`` accepts a reference whose destination is a known
function marked ``dx-safe`` or a local call whose corresponding variable's
references do not escape either, and ``leaf-refs-not-escape-elsewhere-p``
checks every other reference of the same leaf. Mule asks it for every value
without a declaration, because reference counting makes each avoided escape
worth a retain and a release. ``spec/native-lowering.md`` records the
departure and its consequences.

The facts are conservative. A value escapes when any of its uses may keep
it: storing it into a variable that escapes, into a global, cell, object, or
container; returning, yielding, or raising it; or passing it to a callee
that may keep its argument. A full call to an unknown function keeps every
argument. Known functions keep their arguments unless the function database
declares otherwise (``FunInfo.borrowed_args``).

Parameters of local functions are solved together: a parameter escapes when
a use of it escapes, and a local call's argument escapes when the callee's
parameter does. Solving starts from "does not escape" and moves monotonically
to "escapes" over the uses of each body. Unknown and guarded calls remain
escaping: a predicted callee does not establish effects for its fallback.
These facts describe capture, not a backend ownership convention; callers
must separately prove an owner's lifetime before selecting a borrowed entry.
"""

from __future__ import annotations

from dataclasses import dataclass

from . import fndb
from .ctype import ListType, TupleType, ValueType, ValueTypeVisitor
from .ir1opt import lvar_type
from .ir1util import made_lambda
from .node import (
    CallKind,
    Combination,
    FunctionalKind,
    Lambda,
    LambdaVar,
    LeafVisitor,
    Lvar,
    NodeVisitor,
    Ref,
    Set,
    linked,
)


class EscapeFacts:
    """The solved escape facts of one compilation."""

    def __init__(self):
        self._escaping: set[object] = set()
        self._allocation_escaping: set[object] = set()
        self._analyzed: set[object] = set()
        self._allocations: set[Combination] = set()
        self._aliases: dict[Combination, frozenset[LambdaVar]] = {}

    def variable_escapes(self, var: LambdaVar) -> bool:
        """Whether a reference held by ``var`` may outlive its scope. A
        variable the analysis never saw is reported as escaping."""
        return var in self._escaping or var not in self._analyzed

    def parameter_escapes(self, lambda_: Lambda, index: int) -> bool:
        return self.variable_escapes(lambda_.vars[index])

    def allocation_escapes(self, node: Combination) -> bool:
        """Whether the newly allocated object can leave its owning activation."""
        if node not in self._allocations:
            return True
        if node.fun_info in (fndb.BUILD_LIST, fndb.BUILD_TUPLE):
            return node in self._allocation_escaping
        return node in self._escaping

    def aliases_for(self, node: Combination) -> frozenset[LambdaVar]:
        return self._aliases.get(node, frozenset())


def _is_local_function_ref(ref: Ref, lambda_: Lambda) -> bool:
    if lambda_.environment is not None and lambda_.environment.closure:
        return True
    if ref.dest is None:
        return False
    maker = ref.dest.accept(_ALLOCATION)
    if maker is None or maker.fun_info is not fndb.MAKE_FUNCTION:
        return True
    if maker.lvar is None or maker.lvar.dest is None:
        return False
    return maker.lvar.dest.accept(
        _LOCAL_BINDING
    ).var is not None or maker.lvar.dest.accept(_DirectFunctionUse(maker, lambda_))


def _expand_lambdas(lambdas) -> tuple[Lambda, ...]:
    ordered = []
    seen = set()
    pending = [lambda_ for lambda_ in lambdas if lambda_.component is not None]
    while pending:
        lambda_ = pending.pop(0)
        if lambda_ in seen or lambda_.component is None:
            continue
        seen.add(lambda_)
        ordered.append(lambda_)
        for ref in lambda_.refs:
            owner = ref.home_lambda
            if (
                owner is not None
                and owner not in seen
                and owner.component is not None
                and _is_local_function_ref(ref, lambda_)
            ):
                pending.append(owner)
        if lambda_.environment is not None:
            for var in lambda_.environment.closure:
                owner = var.home
                if (
                    owner is not None
                    and owner not in seen
                    and owner.component is not None
                ):
                    pending.append(owner)
        for block in lambda_.component.blocks():
            for node in block.nodes():
                callee = node.accept(_REACHABLE_CALLEE)
                if (
                    callee is not None
                    and callee not in seen
                    and callee.component is not None
                ):
                    pending.append(callee)
    return tuple(ordered)


def _expand_local_family(
    lambdas, local_environments: LocalEnvironments
) -> tuple[Lambda, ...]:
    ordered = []
    seen = set()
    pending = [lambda_ for lambda_ in lambdas if lambda_.component is not None]
    while pending:
        lambda_ = pending.pop(0)
        if lambda_ in seen or lambda_.component is None:
            continue
        seen.add(lambda_)
        ordered.append(lambda_)
        entry = local_environments.functions.get(lambda_)
        if (
            entry is not None
            and entry.owner not in seen
            and entry.owner.component is not None
        ):
            pending.append(entry.owner)
        if lambda_.environment is not None:
            for var in lambda_.environment.closure:
                owner = var.home
                if (
                    owner is not None
                    and owner not in seen
                    and owner.component is not None
                ):
                    pending.append(owner)
        for child_entry in local_environments.functions.values():
            if (
                child_entry.owner is lambda_
                and child_entry.function not in seen
                and child_entry.function.component is not None
            ):
                pending.append(child_entry.function)
        for block in lambda_.component.blocks():
            for node in block.nodes():
                allocation = node.accept(_ALLOCATION)
                if allocation is not None and allocation.fun_info is fndb.MAKE_FUNCTION:
                    child = made_lambda(allocation)
                    if child not in seen and child.component is not None:
                        pending.append(child)
    return tuple(ordered)


def analyze_escapes(
    lambdas,
    *,
    local_environments: LocalEnvironments | None = None,
    base_facts: EscapeFacts | None = None,
    expand: bool = True,
) -> EscapeFacts:
    if base_facts is not None and local_environments is not None:
        lambdas = _expand_local_family(lambdas, local_environments)
    elif expand:
        lambdas = _expand_lambdas(lambdas)
    else:
        lambdas = tuple(lambda_ for lambda_ in lambdas if lambda_.component is not None)
    if local_environments is None:
        local_environments = find_local_environments(lambdas)
    facts = EscapeFacts()
    solver = _Solver(facts, local_environments)
    for lambda_ in lambdas:
        facts._analyzed.update(lambda_.locals)
    if local_environments.functions:
        for lambda_ in lambdas:
            for block in lambda_.component.blocks():
                for node in block.nodes():
                    solver.record_call_inputs(node)
    for lambda_ in lambdas:
        solver.add_lambda(lambda_)
    makers = []
    for lambda_ in lambdas:
        for block in lambda_.component.blocks():
            for node in block.nodes():
                allocation = node.accept(_ALLOCATION)
                if allocation is None:
                    continue
                facts._allocations.add(allocation)
                facts._analyzed.add(allocation)
                solver.uses.classify(allocation, allocation.lvar)
                if allocation.fun_info is fndb.MAKE_FUNCTION:
                    makers.append((made_lambda(allocation), allocation))
    for child, maker in makers:
        if child.environment is not None:
            for var in child.environment.closure:
                solver.escapes_with(var, maker)
    solver.solve(base_facts)
    for allocation in facts._allocations:
        aliases, pending = set(), list(solver.depends_on.get(allocation, ()))
        while pending:
            alias = pending.pop()
            if alias in aliases:
                continue
            aliases.add(alias)
            pending.extend(solver.depends_on.get(alias, ()))
        facts._aliases[allocation] = frozenset(
            alias for alias in aliases if alias not in facts._allocations
        )
    return facts


class _Solver:
    """Worklist over variables: each variable lists the conditions under
    which it escapes; a condition is either unconditional or another
    variable escaping."""

    def __init__(
        self,
        facts: EscapeFacts,
        local_environments: LocalEnvironments | None = None,
    ):
        self.facts = facts
        self.local_environments = local_environments or LocalEnvironments()
        # var -> variables whose escape makes ``var`` escape
        self.depends_on: dict[LambdaVar, list[LambdaVar]] = {}
        self.dependents: dict[LambdaVar, list[LambdaVar]] = {}
        self.incoming_args: dict[LambdaVar, list[Lvar]] = {}
        self.pending: list[LambdaVar] = []
        self.allocation_pending: list[LambdaVar] = []
        self.uses = _UseClassifier(self)

    def proven_callee(self, node: Combination) -> Lambda | None:
        return _proven_callee(node, self.local_environments)

    def record_call_inputs(self, node) -> None:
        callee = node.accept(_CALL_NODE)
        if callee is None:
            return
        target = self.proven_callee(callee)
        if target is None or not self.local_environments.is_local(target):
            return
        for var, arg in zip(target.vars, callee.args, strict=True):
            self.incoming_args.setdefault(var, []).append(arg)

    def add_lambda(self, lambda_: Lambda) -> None:
        for var in lambda_.locals:
            self.facts._analyzed.add(var)
            if var.indirect:
                # A cell outlives the frame; its contents escape with it.
                self.escape(var)
                continue
            for ref in var.refs:
                self.uses.classify(var, ref.lvar)

    def escape(self, var: LambdaVar) -> None:
        if var not in self.facts._escaping:
            self.facts._escaping.add(var)
            self.pending.append(var)
        if var not in self.facts._allocation_escaping:
            self.facts._allocation_escaping.add(var)
            self.allocation_pending.append(var)

    def non_container_escape(self, var: LambdaVar) -> None:
        if var not in self.facts._escaping:
            self.facts._escaping.add(var)
            self.pending.append(var)

    def escapes_with(self, var: LambdaVar, other: LambdaVar) -> None:
        """``var`` escapes whenever ``other`` does."""
        if var is other:
            return
        self.depends_on.setdefault(var, []).append(other)
        self.dependents.setdefault(other, []).append(var)

    def solve(self, base_facts: EscapeFacts | None = None) -> None:
        for dependency in self.dependents:
            if dependency not in self.facts._analyzed:
                if base_facts is not None and dependency in base_facts._analyzed:
                    if dependency in base_facts._escaping:
                        self.non_container_escape(dependency)
                    if dependency in base_facts._allocation_escaping:
                        self.escape(dependency)
                else:
                    self.escape(dependency)
        while self.pending:
            other = self.pending.pop()
            for var in self.dependents.get(other, ()):
                self.non_container_escape(var)
        while self.allocation_pending:
            other = self.allocation_pending.pop()
            for var in self.dependents.get(other, ()):
                self.escape(var)


class _UseClassifier(NodeVisitor):
    """Decide what a use of a variable's value does with it. ``classify``
    follows the value from a reference to the node that consumes it."""

    def __init__(self, solver: _Solver):
        self.solver = solver
        self.container_origin = _ContainerOrigin(solver)
        self.var: LambdaVar | None = None
        self.lvar: Lvar | None = None

    @property
    def source(self) -> LambdaVar:
        return linked(self.var, self, "active escape-analysis variable")

    @property
    def operand(self) -> Lvar:
        return linked(self.lvar, self, "active escape-analysis operand")

    def classify(self, var: LambdaVar, lvar: Lvar | None) -> None:
        if lvar is None or lvar.dest is None:
            # The value is computed and discarded.
            return
        previous = self.var, self.lvar
        self.var, self.lvar = var, lvar
        try:
            lvar.dest.accept(self)
        finally:
            self.var, self.lvar = previous

    def visit_Node(self, node):
        # Returns, exits carrying values, and any consumer not classified
        # below may keep the value.
        self.solver.escape(self.source)

    def visit_If(self, node):
        # __bool__ and __len__ may publish their receiver through user code.
        self.solver.escape(self.source)

    def visit_Cast(self, node):
        # A cast passes its operand through: it escapes where the cast's
        # result goes.
        self.classify(self.source, node.lvar)

    def visit_Set(self, node: Set):
        target = node.var.accept(_LOCAL_TARGET)
        if target is None:
            self.solver.escape(self.source)
            return
        self.solver.escapes_with(self.source, target)

    def visit_Combination(self, node: Combination):
        if self.lvar is node.fun:
            if self.solver.proven_callee(node) is not None:
                return
            # An arbitrary callable's __call__ may publish its receiver.
            self.solver.escape(self.source)
            return
        for index, argument in enumerate(node.args):
            if argument is self.operand:
                _CALL_KIND_USES[node.kind](self, node, index)

    def known_call(self, node: Combination, index: int) -> None:
        info = node.fun_info
        if (
            info is fndb.SETATTR
            and node.info.name == "__annotate__"
            and len(node.args[0].uses) == 1
            and self.solver.local_environments.function_for_value(node.args[0].use())
            is not None
        ):
            return
        if (
            index == 0
            and info in _LOCAL_CONTAINER_READERS
            and (
                info not in (fndb.PROTOCOLS["getitem"], fndb.PROTOCOLS["setitem"])
                or (
                    len(node.args) >= 2
                    and not lvar_type(node.args[1]).may_run_user_code
                )
            )
        ):
            if info is fndb.PROTOCOLS["unpack"]:
                self.classify(self.source, node.lvar)
            if not (
                self.source in self.solver.facts._allocations
                or self.source.accept(self.container_origin) is not None
                or _is_safe_container_reader_call(node, info)
            ):
                self.solver.non_container_escape(self.source)
            return
        if info is not None and info.borrows_argument(index):
            # The operation reads its argument; its result may still carry
            # the value onward when the function returns it.
            if info.returns_argument(index):
                self.classify(self.source, node.lvar)
            return
        self.solver.escape(self.source)

    def local_call(self, node: Combination, index: int) -> None:
        callee = self.solver.proven_callee(node)
        if callee is None or index >= len(callee.vars):
            self.solver.escape(self.source)
            return
        self.solver.escapes_with(self.source, callee.vars[index])

    def unknown_call(self, node: Combination, index: int) -> None:
        callee = self.solver.proven_callee(node)
        if callee is not None and index < len(callee.vars):
            self.solver.escapes_with(self.source, callee.vars[index])
            return
        self.solver.escape(self.source)


# How each call kind treats an argument. Unproven GUARDED calls fall back to a
# full call when the prediction fails, so they keep their arguments as one does.
_CALL_KIND_USES = {
    CallKind.KNOWN: _UseClassifier.known_call,
    CallKind.LOCAL: _UseClassifier.local_call,
    CallKind.FULL: _UseClassifier.unknown_call,
    CallKind.GUARDED: _UseClassifier.unknown_call,
    CallKind.ERROR: _UseClassifier.unknown_call,
}


class _LocalTarget(LeafVisitor):
    """The local variable a ``Set`` assigns, or None for any other target."""

    def visit_Leaf(self, leaf):
        return None

    def visit_LambdaVar(self, leaf: LambdaVar):
        return None if leaf.indirect else leaf


_LOCAL_TARGET = _LocalTarget()


class _CalleeLambda(NodeVisitor):
    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(_LAMBDA_LEAF)


_CALLEE_LAMBDA = _CalleeLambda()


class _LambdaLeaf(LeafVisitor):
    def visit_Leaf(self, leaf):
        return None

    def visit_Lambda(self, leaf: Lambda):
        return leaf


_LAMBDA_LEAF = _LambdaLeaf()


def _callee_lambda(node: Combination) -> Lambda | None:
    if len(node.fun.uses) != 1:
        return None
    return node.fun.uses[0].accept(_CALLEE_LAMBDA)


class _CallNode(NodeVisitor):
    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        return node


_CALL_NODE = _CallNode()


class _ReachableCallee(NodeVisitor):
    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if node.kind is CallKind.KNOWN and node.fun_info is fndb.MAKE_FUNCTION:
            return made_lambda(node)
        if node.kind is CallKind.LOCAL:
            return _callee_lambda(node)
        if node.kind is CallKind.GUARDED and node.proven and node.predicted is not None:
            return node.predicted.as_lambda()
        return None


_REACHABLE_CALLEE = _ReachableCallee()


def _proven_callee(
    node: Combination, local_environments: LocalEnvironments | None
) -> Lambda | None:
    if node.kind is CallKind.LOCAL:
        return _callee_lambda(node)
    if node.kind not in (CallKind.FULL, CallKind.GUARDED):
        return None
    if node.info is not None and node.info.keywords:
        return None
    callee = (
        local_environments.call_target(node) if local_environments is not None else None
    )
    if (
        callee is None
        and node.kind is CallKind.GUARDED
        and node.proven
        and node.predicted is not None
    ):
        callee = node.predicted.as_lambda()
    if (
        callee is None
        or callee.kind is not FunctionalKind.NORMAL
        or callee.signature.variadic
        or callee.signature.default_parameters
        or not callee.signature.positional_body
        or (callee.code is not None and callee.code.flags & 0x2A0)
        or len(node.args) != len(callee.vars)
    ):
        return None
    return callee


class _BuiltinContainerType(ValueTypeVisitor):
    def visit_ValueType(self, fact: ValueType) -> bool:
        return False

    def visit_TupleType(self, fact: TupleType) -> bool:
        return not fact.may_be_unbound

    def visit_ListType(self, fact: ListType) -> bool:
        return not fact.may_be_unbound


_BUILTIN_CONTAINER_TYPE = _BuiltinContainerType()


def _is_safe_container_reader_call(node: Combination, info) -> bool:
    if not node.args or not lvar_type(node.args[0]).accept(_BUILTIN_CONTAINER_TYPE):
        return False
    if info in (fndb.PROTOCOLS["getitem"], fndb.PROTOCOLS["setitem"]):
        return len(node.args) >= 2 and not lvar_type(node.args[1]).may_run_user_code
    return True


class _Allocation(NodeVisitor):
    def visit_Node(self, node):
        return None

    def visit_Combination(self, node):
        return (
            node
            if node.kind is CallKind.KNOWN
            and node.fun_info in (fndb.BUILD_LIST, fndb.BUILD_TUPLE, fndb.MAKE_FUNCTION)
            else None
        )


_ALLOCATION = _Allocation()
_LOCAL_CONTAINER_READERS = frozenset(
    (
        fndb.LEN,
        fndb.PROTOCOLS["getitem"],
        fndb.PROTOCOLS["setitem"],
        fndb.PROTOCOLS["unpack"],
        fndb.UNPACK_REF,
    )
)


class _ContainerOrigin(NodeVisitor, LeafVisitor):
    def __init__(self, solver: _Solver | None = None):
        self.solver = solver
        self.active = set()

    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Cast(self, node):
        if len(node.value.uses) != 1:
            return None
        return node.value.use().accept(self)

    def visit_Ref(self, node):
        return node.leaf.accept(self)

    def visit_Combination(self, node):
        if node.kind is not CallKind.KNOWN:
            return None
        if node.fun_info in (fndb.BUILD_LIST, fndb.BUILD_TUPLE):
            return node
        if (
            node.fun_info is fndb.PROTOCOLS["unpack"]
            and node.args
            and len(node.args[0].uses) == 1
        ):
            return node.args[0].use().accept(self)
        return None

    def visit_LambdaVar(self, var):
        if var in self.active:
            return None
        if var in var.home.vars:
            if self.solver is None or not self.solver.local_environments.is_local(
                var.home
            ):
                return None
            values = [
                *(
                    assignment.value
                    for assignment in var.sets
                    if assignment.value is not None
                ),
                *self.solver.incoming_args.get(var, ()),
            ]
        else:
            values = [
                assignment.value
                for assignment in var.sets
                if assignment.value is not None
            ]
        if not values or any(len(value.uses) != 1 for value in values):
            return None
        self.active.add(var)
        try:
            origins = [value.use().accept(self) for value in values]
            return origins[0] if all(origin is not None for origin in origins) else None
        finally:
            self.active.remove(var)


@dataclass(frozen=True, slots=True)
class LocalFunction:
    function: Lambda
    binding: LambdaVar | None
    owner: Lambda
    aliases: tuple = ()


class LocalEnvironments:
    """Local function identities and cells whose lifetime is their home frame."""

    def __init__(self, functions=(), stack_cells=()):
        self.functions = {entry.function: entry for entry in functions}
        self.bindings = {
            var: entry.function
            for entry in functions
            for var in (entry.binding, *entry.aliases)
            if var is not None
        }
        self.stack_cells = frozenset(stack_cells)
        self.discarded_functions = frozenset()

    def is_local(self, function):
        return function in self.functions

    def function_for_binding(self, var):
        return self.bindings.get(var)

    def function_for_value(self, node):
        return node.accept(_LOCAL_BINDING).and_lookup(self.bindings, self.functions)

    def call_target(self, node):
        if len(node.fun.uses) != 1:
            return None
        return self.function_for_value(node.fun.use())

    def discards_function(self, function):
        return function in self.discarded_functions


class _BindingResult:
    def __init__(self, var=None, function=None):
        self.var = var
        self.function = function

    def and_lookup(self, bindings, functions=()):
        if self.function is not None:
            return self.function if self.function in functions else None
        return bindings.get(self.var)


class _LocalBinding(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return _BindingResult()

    def visit_Leaf(self, leaf):
        return _BindingResult()

    def visit_Ref(self, node):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, var):
        return _BindingResult(var)

    def visit_Set(self, node):
        return node.var.accept(self)

    def visit_Combination(self, node):
        if node.kind is CallKind.KNOWN and node.fun_info is fndb.MAKE_FUNCTION:
            return _BindingResult(function=made_lambda(node))
        return _BindingResult()


_LOCAL_BINDING = _LocalBinding()


class _OnlyLocalAnnotation(NodeVisitor):
    """A fresh function is unobservable when only discarded metadata owns it."""

    def __init__(self, environments):
        self.environments = environments
        self.operand = None
        self.active = set()

    def check(self, value):
        if value is None or value.dest is None:
            return True
        previous = self.operand
        self.operand = value
        try:
            return value.dest.accept(self)
        finally:
            self.operand = previous

    def visit_Node(self, node):
        return False

    def visit_Set(self, node):
        binding = node.var.accept(_LOCAL_BINDING).var
        if binding is None or binding in self.active or len(binding.sets) != 1:
            return False
        self.active.add(binding)
        try:
            return all(self.check(ref.lvar) for ref in binding.refs)
        finally:
            self.active.remove(binding)

    def visit_Combination(self, node):
        return (
            node.kind is CallKind.KNOWN
            and node.fun_info is fndb.SETATTR
            and node.info.name == "__annotate__"
            and node.args[1] is self.operand
            and self.environments.function_for_value(node.args[0].use()) is not None
        )


class _DirectFunctionUse(NodeVisitor):
    def __init__(self, ref, function):
        self.ref = ref
        self.function = function

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node):
        if (
            node.fun_info is fndb.SETATTR
            and node.info.name == "__annotate__"
            and node.args[0] is self.ref.lvar
            and len(self.ref.lvar.uses) == 1
        ):
            return True
        return (
            node.fun is self.ref.lvar
            and len(node.fun.uses) == 1
            and node.kind in (CallKind.FULL, CallKind.GUARDED, CallKind.LOCAL)
            and (node.info is None or not node.info.keywords)
            and len(node.args) == len(self.function.vars)
        )


class _SameFunctionDefinition(NodeVisitor, LeafVisitor):
    """Every binding write constructs the same source code or aliases it."""

    def __init__(self, function):
        self.function = function
        self.active = set()

    def visit_Node(self, node):
        return False

    def visit_Leaf(self, leaf):
        return False

    def visit_Ref(self, node):
        return node.leaf.accept(self)

    def visit_Cast(self, node):
        return node.value.use().accept(self)

    def visit_Combination(self, node):
        return (
            node.kind is CallKind.KNOWN
            and node.fun_info is fndb.MAKE_FUNCTION
            and made_lambda(node) is self.function
        )

    def visit_LambdaVar(self, var):
        if var in self.active or var in var.home.vars or not var.sets:
            return False
        self.active.add(var)
        try:
            return all(
                assignment.value is not None and assignment.value.use().accept(self)
                for assignment in var.sets
            )
        finally:
            self.active.remove(var)


def find_local_environments(lambdas) -> LocalEnvironments:
    """Prove closure extent before selecting a non-object environment ABI.

    SBCL separates INDIRECT from EXPLICIT-VALUE-CELL in environment analysis.
    Captured variables remain indirect here; only their physical cell changes.
    """
    lambdas = tuple(lambdas)
    creations = {}
    candidates = {}
    for owner in lambdas:
        for block in owner.component.blocks():
            for node in block.nodes():
                maker = node.accept(_ALLOCATION)
                if (
                    maker is None
                    or maker.fun_info is not fndb.MAKE_FUNCTION
                    or maker.home_lambda is not owner
                ):
                    continue
                function = made_lambda(maker)
                creations.setdefault(function, set()).add(maker)
                if (
                    (owner.code is not None and owner.code.flags & 0x2A0)
                    or function.kind is not FunctionalKind.NORMAL
                    or function.signature.variadic
                    or function.signature.default_parameters
                    or not function.signature.positional_body
                    or (function.code is not None and function.code.flags & 0x2A0)
                    or maker.lvar is None
                    or maker.lvar.dest is None
                ):
                    continue
                binding = maker.lvar.dest.accept(_LOCAL_BINDING).var
                if binding is None and maker.lvar.dest.accept(
                    _DirectFunctionUse(maker, function)
                ):
                    candidates.setdefault(function, {})[maker] = LocalFunction(
                        function, None, owner
                    )
                    continue
                same_definition = _SameFunctionDefinition(function)
                if (
                    binding is None
                    or not binding.accept(same_definition)
                    or not binding.refs
                ):
                    continue
                aliases, pending, references = {binding}, [binding], []
                valid = True
                while pending:
                    alias = pending.pop()
                    for ref in alias.refs:
                        other = (
                            ref.dest.accept(_LOCAL_BINDING).var
                            if ref.dest is not None
                            else None
                        )
                        if other is not None:
                            if not other.accept(same_definition):
                                valid = False
                                break
                            if other not in aliases:
                                aliases.add(other)
                                pending.append(other)
                        else:
                            references.append(ref)
                if not valid:
                    continue
                uses = []
                for ref in references:
                    dest = ref.dest
                    if dest is None or not dest.accept(
                        _DirectFunctionUse(ref, function)
                    ):
                        break
                    # Each possible write denotes this source code. Ordinary
                    # local/capture reads still check dynamic initialization.
                    uses.append(ref)
                else:
                    candidates.setdefault(function, {})[maker] = LocalFunction(
                        function, binding, owner, tuple(aliases - {binding})
                    )
    makers = {}
    for function, sites in candidates.items():
        if sites.keys() != creations[function]:
            continue
        entries = tuple(sites.values())
        owner = entries[0].owner
        if any(entry.owner is not owner for entry in entries):
            continue
        aliases = dict.fromkeys(
            var
            for entry in entries
            for var in (entry.binding, *entry.aliases)
            if var is not None
        )
        binding = next(iter(aliases), None)
        makers[function] = LocalFunction(
            function,
            binding,
            owner,
            tuple(var for var in aliases if var is not binding),
        )
    changed = True
    while changed:
        changed = False
        for function, entry in tuple(makers.items()):
            # A raw function binding cannot be captured by an object closure.
            if any(
                ref.home_lambda is not entry.owner and ref.home_lambda not in makers
                for alias in (entry.binding, *entry.aliases)
                if alias is not None
                for ref in alias.refs
            ):
                del makers[function]
                changed = True
    holders = {}
    for function in lambdas:
        if function.environment is not None:
            for var in function.environment.closure:
                holders.setdefault(var, set()).add(function)
    environments = LocalEnvironments(makers.values())
    proof = _OnlyLocalAnnotation(environments)
    discarded = {
        function
        for function, sites in creations.items()
        if all(proof.check(maker.lvar) for maker in sites)
    }
    environments.discarded_functions = frozenset(discarded)
    environments.stack_cells = frozenset(
        var
        for var, functions in holders.items()
        if var.indirect
        and functions - discarded <= makers.keys()
        and not (var.home.code is not None and var.home.code.flags & 0x2A0)
    )
    return environments
