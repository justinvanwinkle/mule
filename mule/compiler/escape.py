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
        return node not in self._allocations or node in self._escaping

    def aliases_for(self, node: Combination) -> frozenset[LambdaVar]:
        return self._aliases.get(node, frozenset())


def analyze_escapes(lambdas) -> EscapeFacts:
    lambdas = tuple(lambdas)
    facts = EscapeFacts()
    solver = _Solver(facts)
    for lambda_ in lambdas:
        facts._analyzed.update(lambda_.locals)
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
    solver.solve()
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

    def __init__(self, facts: EscapeFacts):
        self.facts = facts
        # var -> variables whose escape makes ``var`` escape
        self.depends_on: dict[LambdaVar, list[LambdaVar]] = {}
        self.dependents: dict[LambdaVar, list[LambdaVar]] = {}
        self.pending: list[LambdaVar] = []
        self.uses = _UseClassifier(self)

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
        if var in self.facts._escaping:
            return
        self.facts._escaping.add(var)
        self.pending.append(var)

    def escapes_with(self, var: LambdaVar, other: LambdaVar) -> None:
        """``var`` escapes whenever ``other`` does."""
        if var is other:
            return
        self.depends_on.setdefault(var, []).append(other)
        self.dependents.setdefault(other, []).append(var)

    def solve(self) -> None:
        for dependency in self.dependents:
            if dependency not in self.facts._analyzed:
                self.escape(dependency)
        while self.pending:
            other = self.pending.pop()
            for var in self.dependents.get(other, ()):
                self.escape(var)


class _UseClassifier(NodeVisitor):
    """Decide what a use of a variable's value does with it. ``classify``
    follows the value from a reference to the node that consumes it."""

    def __init__(self, solver: _Solver):
        self.solver = solver
        self.container_origin = _ContainerOrigin()
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
            # An arbitrary callable's __call__ may publish its receiver.
            self.solver.escape(self.source)
            return
        for index, argument in enumerate(node.args):
            if argument is self.operand:
                _CALL_KIND_USES[node.kind](self, node, index)

    def known_call(self, node: Combination, index: int) -> None:
        info = node.fun_info
        if index == 0 and info in _LOCAL_CONTAINER_READERS:
            if self.source in self.solver.facts._allocations:
                return
            if self.source.accept(self.container_origin) is not None:
                return
        if info is not None and info.borrows_argument(index):
            # The operation reads its argument; its result may still carry
            # the value onward when the function returns it.
            if info.returns_argument(index):
                self.classify(self.source, node.lvar)
            return
        self.solver.escape(self.source)

    def local_call(self, node: Combination, index: int) -> None:
        callee = _callee_lambda(node)
        if callee is None or index >= len(callee.vars):
            self.solver.escape(self.source)
            return
        self.solver.escapes_with(self.source, callee.vars[index])

    def unknown_call(self, node: Combination, index: int) -> None:
        self.solver.escape(self.source)


# How each call kind treats an argument. GUARDED calls fall back to a full
# call when the prediction fails, so they keep their arguments as one does.
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
    def __init__(self):
        self.active = set()

    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Cast(self, node):
        return node.value.use().accept(self)

    def visit_Ref(self, node):
        return node.leaf.accept(self)

    def visit_Combination(self, node):
        return (
            node
            if node.kind is CallKind.KNOWN
            and node.fun_info in (fndb.BUILD_LIST, fndb.BUILD_TUPLE)
            else None
        )

    def visit_LambdaVar(self, var):
        if var in self.active or var in var.home.vars:
            return None
        writes = [assignment for assignment in var.sets if assignment.value is not None]
        if not writes:
            return None
        self.active.add(var)
        try:
            origins = [assignment.value.use().accept(self) for assignment in writes]
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
