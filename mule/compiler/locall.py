"""Local-call analysis, following ``src/compiler/locall.lisp``.

SBCL converts a call whose function is a known lambda into a local call and
splits optional-argument entry points. Python module globals are mutable until
the module has executed, so Mule converts a call through a global name into a
**guarded** call: the predicted lambda is recorded on the combination, code
generation checks the callable's identity at the call site, and the general
path remains. A call whose callee identity is proven by flow analysis (the
binding was stored in the same function and nothing since could run user code)
additionally receives the callee's published return type.

Entry contexts replace SBCL's optional-dispatch entry points for now: each
lambda keeps its general entry (unknown parameters) and may gain a ``fixnum``
entry (every parameter a tagged integer) and an ``observed`` entry (the join of
integer intervals actual call sites supplied). Return types per entry start
empty and grow by union, exactly as ``accumulate-optimistic-arg-types`` /
``publish-optimistic-types`` do; nothing is published before the fixpoint.
Code generation emits one body per entry with that entry's facts; copying the
body (SBCL's inline expansion) becomes necessary once transforms depend on an
entry's facts.
"""

from __future__ import annotations

from collections import defaultdict
from collections.abc import Mapping
from contextlib import contextmanager
from dataclasses import dataclass, field
from threading import local

from mule import ast

from . import ir1opt, loop
from .annotation_types import AnnotationLedger
from .constraint import (
    BOOTSTRAP_BUILTIN_TARGETS,
    BOOTSTRAP_CLASS_TARGETS,
    NO_FACT,
    Body,
    Fact,
    LambdaGraph,
    build_lambda_graph,
    constant_type,
    join_fact,
    propagate,
    widen_fact,
)
from .ctype import (
    ANY_CLASS_TYPE,
    BOOLEAN,
    BOOLEAN_FALSE,
    BOOLEAN_TRUE,
    BOTTOM,
    BUILTIN_CLASS_TYPES,
    BYTEARRAY_TYPE,
    BYTES_TYPE,
    DICT_TYPE,
    EMPTY_LIST_TYPE,
    FIXNUM_MAX,
    FIXNUM_MIN,
    FLOAT,
    INTEGER,
    LIST_TYPE,
    NONE_TYPE,
    PYTHON_INTEGER,
    STORAGE_CLASS_TYPES,
    TEXT_TYPE,
    UNBOUND,
    UNKNOWN,
    ClassType,
    DictType,
    InstanceType,
    IntegerType,
    ListType,
    TupleType,
    UniqueObjectType,
    ValueType,
    ValueTypeVisitor,
)
from .dfo import find_dfo
from .fndb import (
    BUILD_LIST,
    BUILD_TUPLE,
    CLASS_DELETE,
    CLASS_STORE,
    COMPARE,
    GETATTR,
    GETSLICE,
    IMPORT,
    LEN,
    MAKE_CLASS,
    MAKE_FUNCTION,
    PROTOCOLS,
    RANGE_PREPARE,
    RESOLVE_METHOD,
    RESOLVE_SUPER_METHOD,
    SETATTR,
    UNARY,
    UNPACK_REF,
    flow_propagation,
)
from .ir1util import clone_lambda_component, join_blocks_if_possible, made_function
from .node import (
    CallKind,
    ClassConstructorTarget,
    Combination,
    ForeignCallTarget,
    FunctionalKind,
    GlobalVar,
    Lambda,
    LambdaVar,
    LeafVisitor,
    NodeVisitor,
    Ref,
    Set,
    linked,
)
from .seqtran import is_predictable_len_call, known_length_primitive

DELATTR = PROTOCOLS["delete_attribute"]
DELITEM = PROTOCOLS["delitem"]
GETITEM = PROTOCOLS["getitem"]
ITERATE = PROTOCOLS["iterate"]
SETITEM = PROTOCOLS["setitem"]

_SET_DEL_ITEM = frozenset({SETITEM, DELITEM})
_GET_SET_DEL_ATTR = frozenset({GETATTR, SETATTR, DELATTR})
_GET_ITEM_OR_SLICE = frozenset({GETITEM, GETSLICE})
_CONTAINMENT_COMPARE = frozenset(
    {
        COMPARE[ast.ComparisonOperator.IN],
        COMPARE[ast.ComparisonOperator.NOT_IN],
    }
)
_SPECIALIZABLE_CONTAINER_OPS = frozenset(
    {
        GETITEM,
        GETSLICE,
        SETITEM,
        DELITEM,
        RESOLVE_METHOD,
        ITERATE,
    }
)


class _ParameterUse(NodeVisitor, LeafVisitor):
    """Classifies how one parameter (and local copies of it) is used."""

    def __init__(
        self,
        parameter,
        storage_methods: Mapping[tuple[str, str], ValueType] | None = None,
    ):
        self.copies = {parameter}
        self.queue = [parameter]
        self.storage_methods = storage_methods
        self.numeric = False
        self.disqualified = False
        self.text = False
        self.text_disqualified = False
        self.list_seq = False
        self.list_disqualified = False
        self.sequence_disqualified = False
        self.has_len = False
        self.has_getitem = False
        self.current = None

    def analyze(self, contract, full: ValueType, declared=None) -> ValueType:
        effective = contract if contract is not None else declared
        while self.queue and not (
            self.disqualified
            and self.text_disqualified
            and self.list_disqualified
            and self.sequence_disqualified
        ):
            var = self.queue.pop()
            for ref in var.refs:
                if ref.dest is not None and ref.lvar is not None:
                    self.current = ref.lvar
                    ref.dest.accept(self)
        if effective is not None:
            spec_type = effective.speculative_parameter_type(full)
            if spec_type.fits(INTEGER) or spec_type.fits_float(FLOAT):
                return spec_type if not self.disqualified else UNKNOWN
            if spec_type.fits_text(TEXT_TYPE):
                return spec_type if not self.text_disqualified else UNKNOWN
            if spec_type.is_exact_list:
                return spec_type if not self.list_disqualified else UNKNOWN
            if spec_type is not UNKNOWN:
                return spec_type if not self.sequence_disqualified else UNKNOWN
        if (
            self.numeric
            and not self.disqualified
            and (effective is None or not effective.contradicts(full))
        ):
            return full
        if (
            self.text
            and not self.text_disqualified
            and not self.numeric
            and (effective is None or not effective.contradicts(TEXT_TYPE))
        ):
            return TEXT_TYPE
        if (
            (self.list_seq or (self.has_len and self.has_getitem))
            and not self.list_disqualified
            and not self.numeric
            and not self.text
            and (effective is None or not effective.contradicts(LIST_TYPE))
        ):
            return LIST_TYPE
        return UNKNOWN

    def general_type(self, declared) -> ValueType:
        if declared is None:
            return UNKNOWN
        candidate = declared.general_parameter_type()
        if candidate is UNKNOWN:
            return UNKNOWN
        while self.queue and not (self.numeric or self.sequence_disqualified):
            var = self.queue.pop()
            for ref in var.refs:
                if ref.dest is not None and ref.lvar is not None:
                    self.current = ref.lvar
                    ref.dest.accept(self)
        if self.numeric or self.sequence_disqualified:
            return UNKNOWN
        return candidate

    def visit_Set(self, node):
        if node.value is self.current:
            node.var.accept(self)

    def visit_LambdaVar(self, leaf):
        if leaf not in self.copies:
            self.copies.add(leaf)
            self.queue.append(leaf)

    def visit_Combination(self, node):
        lvar = self.current
        if node.fun is lvar:
            self.disqualified = True
            self.text_disqualified = True
            self.list_disqualified = True
            self.sequence_disqualified = True
            return
        info = node.fun_info
        if info is None:
            if is_predictable_len_call(node) and node.args and node.args[0] is lvar:
                self.disqualified = True
                self.has_len = True
            return
        if info is RESOLVE_METHOD and node.args and node.args[0] is lvar:
            self.disqualified = True
            method_name = linked(node.info, node, "info").name
            if self.storage_methods is not None and (
                ("TextStorage", method_name) in self.storage_methods
                or ("ListStorage", method_name) in self.storage_methods
            ):
                if ("TextStorage", method_name) in self.storage_methods:
                    if ("TupleStorage", method_name) not in self.storage_methods:
                        self.text = True
                else:
                    self.text_disqualified = True
                if ("ListStorage", method_name) in self.storage_methods:
                    if ("TupleStorage", method_name) not in self.storage_methods and (
                        "dict",
                        method_name,
                    ) not in self.storage_methods:
                        self.list_seq = True
                else:
                    self.list_disqualified = True
            else:
                self.text_disqualified = True
                self.list_disqualified = True
            return
        if node.args and node.args[0] is lvar and info in _SET_DEL_ITEM:
            self.disqualified = True
            self.text_disqualified = True
            self.list_seq = True
            return
        if node.args and node.args[0] is lvar and info in _GET_SET_DEL_ATTR:
            self.disqualified = True
            self.list_disqualified = True
            return
        if (
            node.args
            and node.args[0] is lvar
            and info.non_integer_first_arg
            and not (
                info is RANGE_PREPARE and node.info is not None and node.info.direct
            )
        ):
            self.disqualified = True
            if info in _GET_ITEM_OR_SLICE:
                self.has_getitem = True
            elif info.derive_type is not None and info.derive_type(node) == IntegerType(
                0, FIXNUM_MAX
            ):
                self.has_len = True
            return
        if info is RESOLVE_SUPER_METHOD and lvar in node.args:
            self.disqualified = True
            self.text_disqualified = True
            self.list_disqualified = True
            self.sequence_disqualified = True
            return
        if len(node.args) > 1 and node.args[1] is lvar and info in _CONTAINMENT_COMPARE:
            self.disqualified = True
            return
        if len(node.args) > 1 and node.args[1] is lvar and info.index_second_arg:
            self.numeric = True
            self.text_disqualified = True
            self.list_disqualified = True
            self.sequence_disqualified = True
            return
        if node.args and node.args[0] is lvar and info in _GET_ITEM_OR_SLICE:
            self.has_getitem = True
            return
        if info.numeric:
            self.numeric = True


class _PredictableGlobalFunInfo(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        if node.lexenv.policy.safety >= 3:
            return None
        home = node.home_lambda
        if home is not None and (home.policy.safety >= 3 or home.is_protocol):
            return None
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return None

    def visit_GlobalVar(self, leaf: GlobalVar):
        return ir1opt._PREDICTABLE_GLOBAL_FUN_INFO.visit_GlobalVar(leaf)


_PREDICTABLE_GLOBAL_FUN_INFO = _PredictableGlobalFunInfo()


class _SpecializesBody(NodeVisitor):
    """Whether ``candidate`` narrows a numeric operation, branch, or transform
    over ``general``."""

    def __init__(
        self, general: dict[object, ValueType], candidate: dict[object, ValueType]
    ):
        self.general = general
        self.candidate = candidate

    def visit_Node(self, node):
        return False

    def visit_If(self, node):
        if len(node.test.uses) != 1:
            return False
        test_use = node.test.uses[0]
        return self.candidate.get(test_use, UNKNOWN) in (
            BOOLEAN_TRUE,
            BOOLEAN_FALSE,
        ) and self.general.get(test_use, UNKNOWN) not in (
            BOOLEAN_TRUE,
            BOOLEAN_FALSE,
        )

    def visit_Combination(self, node):
        cand_type = self.candidate.get(node, UNKNOWN)
        gen_type = self.general.get(node, UNKNOWN)
        if (
            cand_type != gen_type
            and cand_type is not UNKNOWN
            and not gen_type.fits(cand_type)
        ):
            return True
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is not None
            and node.fun_info.numeric
            and node.args
            and all(len(arg.uses) == 1 for arg in node.args)
        ):
            cand_args = [self.candidate.get(arg.uses[0], UNKNOWN) for arg in node.args]
            gen_args = [self.general.get(arg.uses[0], UNKNOWN) for arg in node.args]
            if (
                all(arg.is_fixnum for arg in cand_args)
                and not all(arg.is_fixnum for arg in gen_args)
            ) or (
                all(arg.fits_float(FLOAT) for arg in cand_args)
                and not all(arg.fits_float(FLOAT) for arg in gen_args)
            ):
                return True
        if is_predictable_len_call(node) and len(node.args[0].uses) == 1:
            arg_use = node.args[0].uses[0]
            if known_length_primitive(
                self.candidate.get(arg_use, UNKNOWN)
            ) is not known_length_primitive(self.general.get(arg_use, UNKNOWN)):
                return True
        if (
            node.kind is CallKind.KNOWN
            and (
                node.fun_info in _SPECIALIZABLE_CONTAINER_OPS
                or node.fun_info in _GET_SET_DEL_ATTR
                or node.fun_info is PROTOCOLS["unpack"]
                or node.fun_info is UNPACK_REF
            )
            and node.args
            and len(node.args[0].uses) == 1
        ):
            arg_use = node.args[0].uses[0]
            cand_recv = self.candidate.get(arg_use, UNKNOWN)
            gen_recv = self.general.get(arg_use, UNKNOWN)
            if (
                cand_recv != gen_recv
                and cand_recv is not UNKNOWN
                and not gen_recv.fits(cand_recv)
            ):
                return True
        if (
            node.kind in (CallKind.FULL, CallKind.GUARDED)
            and len(node.fun.uses) == 1
            and not (node.info is not None and node.info.keywords)
            and node.lexenv.policy.safety < 3
        ):
            info = node.fun.uses[0].accept(_PREDICTABLE_GLOBAL_FUN_INFO)
            if info is not None and info.transforms:
                for arg in node.args:
                    if len(arg.uses) == 1:
                        arg_use = arg.uses[0]
                        cand_arg = self.candidate.get(arg_use, UNKNOWN)
                        gen_arg = self.general.get(arg_use, UNKNOWN)
                        if (
                            cand_arg != gen_arg
                            and cand_arg is not UNKNOWN
                            and not gen_arg.fits(cand_arg)
                        ):
                            return True
        return False


def _specializes_body(
    graph: LambdaGraph,
    general: dict[object, ValueType],
    candidate: dict[object, ValueType],
    general_targets: dict[object, Lambda] | None = None,
    candidate_targets: dict[object, Lambda] | None = None,
) -> bool:
    # A context whose argument types resolve calls the general body cannot
    # resolve (methods of a typed receiver) specializes those calls.
    if (
        candidate_targets is not None
        and general_targets is not None
        and any(
            general_targets.get(node) is not target
            for node, target in candidate_targets.items()
        )
    ):
        return True
    visitor = _SpecializesBody(general, candidate)
    for block in graph.blocks:
        for node in graph.block_nodes[block]:
            if node.accept(visitor):
                return True
    return False


@dataclass(slots=True)
class EntryContext:
    """A guarded internal entry: facts valid once its parameter guards pass."""

    id: str
    parameters: tuple[ValueType, ...]
    types: dict[object, ValueType]
    targets: dict[object, Lambda]
    return_type: ValueType
    element_contracts: dict[object, object] = field(default_factory=dict)


@dataclass(slots=True)
class LambdaFacts:
    types: dict[object, ValueType]
    targets: dict[object, Lambda]
    variants: tuple[EntryContext, ...] = ()
    return_type: ValueType = UNKNOWN
    parameters: tuple[ValueType, ...] = ()
    element_contracts: dict[object, object] = field(default_factory=dict)


def global_bindings(
    lambdas, *, unique_definitions=False
) -> dict[str, dict[str, Lambda]]:
    """Per namespace, the lambda each global name is bound to by a store of a
    freshly created function object anywhere in the module."""
    targets = defaultdict(dict)
    definitions = defaultdict(dict)
    for lambda_ in lambdas:
        visitor = _GlobalFunctionStores(
            targets[lambda_.namespace],
            definitions=definitions[lambda_.namespace] if unique_definitions else None,
        )
        for block in lambda_.component.blocks():
            for node in block.nodes():
                node.accept(visitor)
    if not unique_definitions:
        return targets
    return {
        namespace: {
            name: origin
            for name, origins in bindings.items()
            if len(origins) == 1 and (origin := next(iter(origins))) is not None
        }
        for namespace, bindings in definitions.items()
    }


class _GlobalFunctionStores(NodeVisitor):
    def __init__(
        self,
        namespace: dict[str, Lambda],
        counts: dict[str, int] | None = None,
        definitions: dict[str, set[Lambda]] | None = None,
        leaves: dict[str, GlobalVar] | None = None,
    ):
        self.namespace = namespace
        self.counts = counts
        self.definitions = definitions
        self.leaves = leaves

    def visit_Set(self, node):
        node.var.accept(
            _GlobalFunctionStore(
                self.namespace, self.counts, node, self.definitions, self.leaves
            )
        )


class _GlobalFunctionStore(LeafVisitor):
    """Records a store of a fresh function object into a module global; a
    store into a local binds no module name."""

    def __init__(
        self,
        namespace: dict[str, Lambda],
        counts: dict[str, int] | None,
        store: Set,
        definitions: dict[str, set[Lambda]] | None = None,
        leaves: dict[str, GlobalVar] | None = None,
    ):
        self.namespace = namespace
        self.counts = counts
        self.store = store
        self.definitions = definitions
        self.leaves = leaves

    def visit_GlobalVar(self, leaf):
        if self.counts is not None:
            self.counts[leaf.name] = self.counts.get(leaf.name, 0) + 1
        if self.store.value is None:
            if self.definitions is not None:
                self.definitions.setdefault(leaf.name, set()).add(None)
            return
        lambda_ = made_function(self.store.value.use())
        if lambda_ is not None:
            self.namespace[leaf.name] = lambda_
            if self.leaves is not None:
                self.leaves[leaf.name] = leaf
            if self.definitions is not None and lambda_.source_name == leaf.name:
                self.definitions.setdefault(leaf.name, set()).add(lambda_)


_UNARY_CONSTANT_OPS = frozenset(
    {
        UNARY[ast.UnaryOperator.NEGATIVE],
        UNARY[ast.UnaryOperator.POSITIVE],
        UNARY[ast.UnaryOperator.INVERT],
    }
)


class _FoldUnaryConstant(ast.Visitor):
    def __init__(self, info):
        self.info = info

    def visit_Node(self, node):
        return None

    def visit_IntegerLiteral(self, node: ast.IntegerLiteral):
        if self.info is UNARY[ast.UnaryOperator.NEGATIVE]:
            return ast.IntegerLiteral(-node.value)
        if self.info is UNARY[ast.UnaryOperator.POSITIVE]:
            return ast.IntegerLiteral(+node.value)
        if self.info is UNARY[ast.UnaryOperator.INVERT]:
            return ast.IntegerLiteral(~node.value)
        return None


class _MadeConstant(NodeVisitor, LeafVisitor):
    """The constant literal an lvar or node evaluates to, or None."""

    def __init__(self):
        self.visited = set()

    def visit_Lvar(self, lvar):
        return lvar.uses[0].accept(self) if len(lvar.uses) == 1 else None

    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Ref(self, node):
        return node.leaf.accept(self)

    def visit_Constant(self, leaf):
        return leaf.value

    def visit_LambdaVar(self, leaf):
        if leaf in self.visited or len(leaf.sets) != 1:
            return None
        self.visited.add(leaf)
        value = leaf.sets[0].value
        return (
            value.uses[0].accept(self)
            if value is not None and len(value.uses) == 1
            else None
        )

    def visit_Combination(self, node):
        if node.kind is not CallKind.KNOWN or node.fun_info is None:
            return None
        info = node.fun_info
        if info in _UNARY_CONSTANT_OPS:
            if len(node.args) != 1 or len(node.args[0].uses) != 1:
                return None
            operand = node.args[0].uses[0].accept(self)
            if operand is None:
                return None
            return operand.accept(_FoldUnaryConstant(info))
        if info is BUILD_TUPLE:
            elements = []
            for arg in node.args:
                if len(arg.uses) != 1:
                    return None
                item = arg.uses[0].accept(self)
                if item is None:
                    return None
                elements.append(item)
            return ast.Tuple(elements)
        return None


class _WidenUserConstantType(ValueTypeVisitor):
    def visit_ValueType(self, fact):
        return fact

    def visit_BooleanType(self, fact):
        return BOOLEAN

    def visit_IntegerType(self, fact):
        return IntegerType(FIXNUM_MIN, FIXNUM_MAX) if fact.is_fixnum else INTEGER

    def visit_TextType(self, fact):
        return TEXT_TYPE

    def visit_TupleType(self, fact):
        return TupleType(
            fact.arity,
            None
            if fact.elements is None
            else tuple(el.accept(self) for el in fact.elements),
        )


_WIDEN_USER_CONSTANT_TYPE = _WidenUserConstantType()


def _widen_user_constant_type(vtype: ValueType) -> ValueType:
    return vtype.accept(_WIDEN_USER_CONSTANT_TYPE)


class _GlobalCalleeRefName(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_GlobalVar(self, leaf: GlobalVar):
        return leaf.name


_GLOBAL_CALLEE_REF_NAME = _GlobalCalleeRefName()


class _ProtocolSingletonStoreType(NodeVisitor):
    def __init__(
        self,
        var_name: str,
        class_targets: dict[str, ClassConstructorTarget] | None,
    ):
        self.var_name = var_name
        self.class_targets = class_targets

    def visit_Node(self, node):
        return UNKNOWN

    def visit_Combination(self, node: Combination):
        if node.args:
            return UNKNOWN
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is not None
            and node.fun_info.derive_type is not None
        ):
            derived = node.fun_info.derive_type(node)
            return derived.widen_constant() if derived is not None else UNKNOWN
        if (
            node.kind in (CallKind.FULL, CallKind.GUARDED)
            and self.class_targets is not None
            and len(node.fun.uses) == 1
        ):
            callee_name = node.fun.uses[0].accept(_GLOBAL_CALLEE_REF_NAME)
            if callee_name is not None and callee_name in self.class_targets:
                cls_target = self.class_targets[callee_name]
                if cls_target.new_target is None and cls_target.init_target is None:
                    return UniqueObjectType(self.var_name)
        return UNKNOWN


class _GlobalConstantStores(NodeVisitor, LeafVisitor):
    def __init__(
        self,
        constants: dict[GlobalVar, ValueType],
        *,
        is_protocol: bool,
        class_targets: dict[str, ClassConstructorTarget] | None = None,
    ):
        self.constants = constants
        self.is_protocol = is_protocol
        self.class_targets = class_targets
        self._store: Set | None = None

    def visit_Set(self, node: Set):
        self._store = node
        node.var.accept(self)
        self._store = None

    def visit_GlobalVar(self, leaf: GlobalVar):
        store = self._store
        if store is None or store.value is None or len(leaf.sets) != 1:
            return
        if leaf.sets[0] is not store:
            return
        home = store.home_lambda
        if home is None or home.kind is not FunctionalKind.TOPLEVEL:
            return
        literal = store.value.accept(_MadeConstant())
        if literal is not None:
            vtype = constant_type(literal)
        elif (
            self.is_protocol
            and leaf.name.startswith("_")
            and len(store.value.uses) == 1
        ):
            vtype = store.value.uses[0].accept(
                _ProtocolSingletonStoreType(leaf.name, self.class_targets)
            )
        else:
            return
        vtype = vtype.widen_mutable_containers()
        if vtype is UNKNOWN:
            return
        if self.is_protocol:
            if leaf.name.startswith("_"):
                self.constants[leaf] = vtype
            return
        if (
            home.policy.safety < 3
            and leaf.name.startswith("_")
            and leaf.name.isupper()
            and not vtype.fits_none(NONE_TYPE)
        ):
            self.constants[leaf] = _widen_user_constant_type(vtype)


class _ClassBaseNames(NodeVisitor, LeafVisitor):
    """Extract base class global names from the ``bases`` operand of ``%make_class``."""

    def visit_Node(self, node) -> tuple[str, ...]:
        return ()

    def visit_Leaf(self, leaf) -> tuple[str, ...]:
        return ()

    def visit_Ref(self, node: Ref) -> tuple[str, ...]:
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar) -> tuple[str, ...]:
        if (
            len(leaf.sets) == 1
            and leaf.sets[0].value is not None
            and len(leaf.sets[0].value.uses) == 1
        ):
            return leaf.sets[0].value.uses[0].accept(self)
        return ()

    def visit_Combination(self, node: Combination) -> tuple[str, ...]:
        if node.kind is CallKind.KNOWN and node.fun_info is BUILD_TUPLE:
            return tuple(
                base_name
                for arg in node.args
                if len(arg.uses) == 1
                and (base_name := arg.uses[0].accept(_GLOBAL_CALLEE_REF_NAME))
                is not None
            )
        return ()


_CLASS_BASE_NAMES = _ClassBaseNames()


class _ModuleClassCollector(NodeVisitor):
    """Collect module-level class definitions and their method lambdas."""

    def __init__(self):
        self.methods: dict[str, dict[str, Lambda]] = defaultdict(dict)
        self.public_names: dict[str, str] = {}
        self.bases: dict[str, tuple[str, ...]] = {}
        # Identities whose definitions pass class keywords (a metaclass
        # among them), so calling the class may construct anything.
        self.keyword_classes: set[str] = set()
        self.definitions: dict[str, int] = defaultdict(int)
        # Names a class body binds to anything other than a plain function.
        self.other_bindings: dict[str, set[str]] = defaultdict(set)

    def visit_Combination(self, node: Combination):
        if node.kind is not CallKind.KNOWN or node.fun_info is None:
            return
        if node.fun_info is CLASS_STORE and len(node.args) == 2:
            # (class namespace, value): a function stored in a class body.
            info = linked(node.info, node, "info")
            value = node.args[1]
            lam = made_function(value.uses[0]) if len(value.uses) == 1 else None
            if lam is not None and info.name not in (
                "__init_subclass__",
                "__class_getitem__",
            ):
                self.methods[info.identity][info.name] = lam
            else:
                # Any other binding (a decorated function, a descriptor, a
                # rebinding, or an implicit classmethod) shadows a method of
                # the same name.
                self.methods[info.identity].pop(info.name, None)
                self.other_bindings[info.identity].add(info.name)
        elif node.fun_info is CLASS_DELETE:
            info = linked(node.info, node, "info")
            self.methods[info.identity].pop(info.name, None)
        elif node.fun_info is MAKE_CLASS:
            info = linked(node.info, node, "class definition")
            self.public_names[info.identity] = info.public_name
            self.definitions[info.public_name] += 1
            if info.has_keywords:
                self.keyword_classes.add(info.identity)
            self.bases[info.public_name] = (
                node.args[0].uses[0].accept(_CLASS_BASE_NAMES)
                if node.args and len(node.args[0].uses) == 1
                else ()
            )
            if len(self.bases[info.public_name]) != info.base_count:
                # A base the analysis cannot name may define anything.
                self.keyword_classes.add(info.identity)


def _collect_module_classes(
    lambdas,
) -> dict[str, dict[str, ClassConstructorTarget]]:
    return _collect_class_tables(lambdas)[0]


@dataclass(slots=True)
class ClassFacts:
    """What analysis knows about one user class whose instances it types:
    its resolved methods (including inherited ones defined in the module),
    its single-inheritance chain of class identities, the names shadowed on
    the class by non-method bindings, and its guarded instance field types."""

    identity: str
    methods: dict[str, Lambda]
    fields: dict[str, ValueType]
    chain: tuple[str, ...] = ()
    shadowed: frozenset[str] = frozenset()
    public_name: str = ""
    is_protocol: bool = False


class _SanitizeInstanceFieldType(ValueTypeVisitor):
    """Restricts a value type stored into an instance field to shapes whose
    consumers guard their runtime operations (instance/None unions, fixnums/ints,
    strings, and dicts)."""

    def visit_ValueType(self, fact):
        return UNKNOWN

    def visit_BottomType(self, fact):
        return BOTTOM

    def visit_NoneType(self, fact):
        return UNKNOWN if fact.may_be_unbound else NONE_TYPE

    def visit_IntegerType(self, fact):
        if fact.may_be_unbound:
            return UNKNOWN
        if fact.is_fixnum:
            return IntegerType(FIXNUM_MIN, FIXNUM_MAX)
        return PYTHON_INTEGER

    def visit_BooleanType(self, fact):
        return UNKNOWN if fact.may_be_unbound else BOOLEAN

    def visit_PythonIntegerType(self, fact):
        return UNKNOWN if fact.may_be_unbound else PYTHON_INTEGER

    def visit_TextType(self, fact):
        return UNKNOWN if fact.may_be_unbound else TEXT_TYPE

    def visit_InstanceType(self, fact):
        if fact.may_be_unbound:
            return UNKNOWN
        return fact.as_optional()

    def visit_UnionType(self, fact):
        if fact.may_be_unbound:
            return UNKNOWN
        return fact.map(lambda member: member.accept(self))

    def visit_DictType(self, fact):
        if fact.may_be_unbound:
            return UNKNOWN
        if fact.value is BOTTOM:
            return DictType(BOTTOM, BOTTOM, False, fact.proven)
        key = fact.key.accept(self) if fact.key is not BOTTOM else BOTTOM
        val = fact.value.accept(self)
        if val is BOTTOM or val is NONE_TYPE:
            return DICT_TYPE if fact.proven else UNKNOWN
        if val is UNKNOWN and not fact.proven:
            return UNKNOWN
        return DictType(key, val, False, fact.proven)


class _GetattrOnInstance(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        if (
            len(leaf.sets) == 1
            and leaf.sets[0].value is not None
            and len(leaf.sets[0].value.uses) == 1
        ):
            return leaf.sets[0].value.uses[0].accept(self)
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is GETATTR
            and len(node.args) == 1
            and node.info is not None
        ):
            return node.args[0], node.info.name
        return None


_GETATTR_ON_INSTANCE = _GetattrOnInstance()


_SANITIZE_INSTANCE_FIELD_TYPE = _SanitizeInstanceFieldType()


class _SanitizeProtocolFieldType(_SanitizeInstanceFieldType):
    def visit_IntegerType(self, fact):
        if fact.may_be_unbound:
            return UNKNOWN
        if fact.is_fixnum:
            return fact
        return PYTHON_INTEGER

    def visit_ListType(self, fact):
        if fact.may_be_unbound or fact.element is BOTTOM:
            return UNKNOWN
        elem = fact.element.accept(self)
        return ListType(elem)


_SANITIZE_PROTOCOL_FIELD_TYPE = _SanitizeProtocolFieldType()


class _StorageReadFromInstanceField(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        if (
            len(leaf.sets) == 1
            and leaf.sets[0].value is not None
            and len(leaf.sets[0].value.uses) == 1
        ):
            return leaf.sets[0].value.uses[0].accept(self)
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is not None
            and node.fun_info.name == "_storage_read"
            and len(node.args) == 2
        ):
            attr = node.args[0].use().accept(_GETATTR_ON_INSTANCE)
            if attr is not None:
                return attr[1]
        return None


_STORAGE_READ_FROM_INSTANCE_FIELD = _StorageReadFromInstanceField()


class _EmptyListCopiedFromField(NodeVisitor, LeafVisitor):
    def __init__(self, field_name: str):
        self.field_name = field_name

    def visit_Node(self, node):
        return False

    def visit_Leaf(self, leaf):
        return False

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        if (
            len(leaf.sets) != 1
            or leaf.sets[0].value is None
            or len(leaf.sets[0].value.uses) != 1
        ):
            return False
        init_node = leaf.sets[0].value.uses[0]
        if not init_node.accept(_IS_EMPTY_BUILD_LIST):
            return False
        saw_append = False
        for ref in leaf.refs:
            if ref.lvar is None or ref.lvar.dest is None:
                continue
            dest = ref.lvar.dest
            copied = dest.accept(_AppendsFromField(ref.lvar, self.field_name))
            if copied is False:
                return False
            if copied is True:
                saw_append = True
        return saw_append


class _IsEmptyBuildList(NodeVisitor):
    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        return (
            node.kind is CallKind.KNOWN
            and node.fun_info is BUILD_LIST
            and len(node.args) == 0
        )


_IS_EMPTY_BUILD_LIST = _IsEmptyBuildList()


class _AppendsFromField(NodeVisitor):
    def __init__(self, list_lvar, field_name: str):
        self.list_lvar = list_lvar
        self.field_name = field_name

    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is not None
            and node.fun_info.name == "_storage_append"
            and len(node.args) == 2
            and node.args[0] is self.list_lvar
        ):
            src_field = node.args[1].use().accept(_STORAGE_READ_FROM_INSTANCE_FIELD)
            return src_field == self.field_name
        return None


class _DependsOnInstanceField(NodeVisitor, LeafVisitor):
    def __init__(self, field_name: str):
        self.field_name = field_name
        self.seen: set[LambdaVar] = set()

    def visit_Node(self, node):
        return False

    def visit_Leaf(self, leaf):
        return False

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        if leaf in self.seen:
            return False
        self.seen.add(leaf)
        if (
            len(leaf.sets) == 1
            and leaf.sets[0].value is not None
            and len(leaf.sets[0].value.uses) == 1
        ):
            return leaf.sets[0].value.uses[0].accept(self)
        return False

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is GETATTR
            and len(node.args) == 1
            and node.info is not None
        ):
            return node.info.name == self.field_name
        return any(use.accept(self) for arg in node.args for use in arg.uses)


_TABLE_FIELD_NAMES = frozenset({"_indices", "_hashes", "_keys", "_values", "_used"})


class _CollectSetattrSites(NodeVisitor):
    def __init__(
        self,
        sites: list[Combination],
        setitems: list[Combination],
        list_mutations: list[Combination] | None = None,
    ):
        self.sites = sites
        self.setitems = setitems
        self.list_mutations = list_mutations if list_mutations is not None else []

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is SETATTR
            and len(node.args) == 2
            and node.info is not None
        ):
            self.sites.append(node)
        elif (
            node.kind is CallKind.KNOWN
            and node.fun_info is PROTOCOLS.get("setitem")
            and len(node.args) == 3
        ):
            self.setitems.append(node)
        elif (
            node.kind is CallKind.KNOWN
            and node.fun_info is not None
            and node.fun_info.name in ("_storage_append", "_storage_write")
            and len(node.args) in (2, 3)
        ):
            self.list_mutations.append(node)


def _update_class_field_types(
    class_facts: dict[str, ClassFacts],
    setattr_sites: list[tuple[Lambda, Combination]],
    setitem_sites: list[tuple[Lambda, Combination]],
    bodies: dict[tuple[Lambda, str], Body],
    parameters: dict[tuple[Lambda, str], tuple[ValueType, ...]],
    list_mutation_sites: list[tuple[Lambda, Combination]] = (),
) -> bool:
    if not class_facts or not setattr_sites:
        return False
    table_cls_id = next(
        (cid for cid, cf in class_facts.items() if cf.public_name == "_Table"),
        None,
    )
    direct_stores: dict[str, dict[str, ValueType]] = defaultdict(dict)
    ambiguous_fields: set[str] = set()
    for lam, node in setattr_sites:
        field_name = node.info.name
        recv_use = node.args[0].use()
        val_use = node.args[1].use()
        found_context = False
        saw_typed_receiver = False
        best_store: tuple[str, ValueType] | None = None
        for ctx_name in ("observed", "fixnum", "general"):
            body = bodies.get((lam, ctx_name))
            if body is None:
                continue
            recv_fact = body.facts.get(recv_use, NO_FACT)
            val_body = body
            if (ctx_name == "observed" and lam.signature.default_parameters) or (
                ctx_name == "fixnum"
                and lam.vars
                and parameters.get((lam, "fixnum"), ())[1:]
                != parameters.get((lam, "general"), ())[1:]
            ):
                val_body = bodies.get((lam, "general")) or body
            val_fact = val_body.facts.get(val_use, NO_FACT)
            if recv_fact.type is BOTTOM or val_fact.type is BOTTOM:
                continue
            found_context = True
            recv_cls = recv_fact.type.instance_class()
            if (
                recv_cls is None
                and lam.is_protocol
                and table_cls_id is not None
                and field_name in _TABLE_FIELD_NAMES
            ):
                recv_cls = table_cls_id
            if recv_cls is not None and recv_cls in class_facts:
                saw_typed_receiver = True
                if best_store is None or best_store[1] is UNKNOWN:
                    best_store = (recv_cls, val_fact.type)
        if best_store is not None:
            recv_cls, stored_type = best_store
            cur_type = class_facts[recv_cls].fields.get(field_name, BOTTOM)
            if stored_type is UNKNOWN and cur_type is not BOTTOM:
                stored_type = cur_type
            elif (
                stored_type == LIST_TYPE
                and cur_type is BOTTOM
                and val_use.accept(_EmptyListCopiedFromField(field_name))
            ):
                stored_type = EMPTY_LIST_TYPE
            if stored_type is not UNKNOWN or (
                val_use.accept(_GETATTR_ON_INSTANCE) is None
                and not (
                    cur_type is BOTTOM
                    and val_use.accept(_DependsOnInstanceField(field_name))
                )
            ):
                prev = direct_stores[recv_cls].get(field_name, BOTTOM)
                direct_stores[recv_cls][field_name] = prev.join(stored_type)
        if (
            found_context
            and not saw_typed_receiver
            and not (lam.method_class is not None and lam.method_class in class_facts)
        ):
            ambiguous_fields.add(field_name)

    for lam, node in setitem_sites:
        attr = node.args[0].use().accept(_GETATTR_ON_INSTANCE)
        if attr is None:
            continue
        inst_lvar, field_name = attr
        inst_use = inst_lvar.use()
        key_use = node.args[1].use()
        val_use = node.args[2].use()
        for ctx_name in ("observed", "fixnum", "general"):
            body = bodies.get((lam, ctx_name))
            if body is None:
                continue
            inst_fact = body.facts.get(inst_use, NO_FACT)
            key_fact = body.facts.get(key_use, NO_FACT)
            val_fact = body.facts.get(val_use, NO_FACT)
            if (
                inst_fact.type is BOTTOM
                or key_fact.type is BOTTOM
                or val_fact.type is BOTTOM
            ):
                continue
            inst_cls = inst_fact.type.instance_class()
            if inst_cls is not None and inst_cls in class_facts:
                cur_field = direct_stores[inst_cls].get(field_name, BOTTOM)
                if cur_field.fits_dict():
                    direct_stores[inst_cls][field_name] = cur_field.with_entry(
                        key_fact.type, val_fact.type
                    )

    for lam, node in list_mutation_sites:
        attr = node.args[0].use().accept(_GETATTR_ON_INSTANCE)
        if attr is None:
            continue
        inst_lvar, field_name = attr
        inst_use = inst_lvar.use()
        val_use = node.args[-1].use()
        for ctx_name in ("observed", "fixnum", "general"):
            body = bodies.get((lam, ctx_name))
            if body is None:
                continue
            inst_fact = body.facts.get(inst_use, NO_FACT)
            val_fact = body.facts.get(val_use, NO_FACT)
            if inst_fact.type is BOTTOM or val_fact.type is BOTTOM:
                continue
            inst_cls = inst_fact.type.instance_class()
            if (
                inst_cls is None
                and lam.is_protocol
                and table_cls_id is not None
                and field_name in _TABLE_FIELD_NAMES
            ):
                inst_cls = table_cls_id
            if inst_cls is not None and inst_cls in class_facts:
                cur_field = direct_stores[inst_cls].get(field_name, BOTTOM)
                if cur_field.fits_list():
                    direct_stores[inst_cls][field_name] = cur_field.with_element(
                        val_fact.type
                    )

    changed = False
    for class_id, facts in class_facts.items():
        new_fields: dict[str, ValueType] = {}
        chain = facts.chain or (class_id,)
        sanitizer = (
            _SANITIZE_PROTOCOL_FIELD_TYPE
            if facts.is_protocol
            else _SANITIZE_INSTANCE_FIELD_TYPE
        )
        candidate_names = {
            name
            for anc_id in chain
            for name in direct_stores.get(anc_id, ())
            if name not in ambiguous_fields
            and name not in facts.methods
            and name not in facts.shadowed
        }
        for name in candidate_names:
            joined = BOTTOM
            for anc_id in chain:
                anc_type = direct_stores.get(anc_id, {}).get(name, BOTTOM)
                if anc_type is not BOTTOM:
                    joined = joined.join(anc_type)
            sanitized = joined.accept(sanitizer)
            if (
                sanitized is not UNKNOWN
                and sanitized is not BOTTOM
                and sanitized is not NONE_TYPE
                and sanitized != DictType(BOTTOM, BOTTOM, False)
            ):
                new_fields[name] = sanitized
        if new_fields != facts.fields:
            facts.fields = new_fields
            changed = True
    return changed


def _collect_class_tables(lambdas):
    """Constructor targets by namespace and name, and the facts of every
    class whose instances analysis types, by identity."""
    by_ns: dict[str, dict[str, ClassConstructorTarget]] = defaultdict(dict)
    typed: dict[str, ClassFacts] = {}
    collectors: dict[str, _ModuleClassCollector] = defaultdict(_ModuleClassCollector)
    visited_components = set()
    for lambda_ in lambdas:
        component = lambda_.component
        if component is None or (lambda_.namespace, component) in visited_components:
            continue
        visited_components.add((lambda_.namespace, component))
        collector = collectors[lambda_.namespace]
        for block in component.blocks():
            for node in block.nodes():
                node.accept(collector)
    is_protocol = any(lam.is_protocol for lam in lambdas)
    for ns, collector in collectors.items():
        all_class_ids = list(
            dict.fromkeys([*collector.public_names, *collector.methods])
        )
        for class_id in all_class_ids:
            cls_name = collector.public_names.get(class_id, class_id)
            methods = collector.methods.get(class_id, {})
            base_target = BOOTSTRAP_CLASS_TARGETS.get(cls_name)
            if base_target is not None:
                base_type = base_target.base_type
                cls_type = base_target.cls_type
                records_type_origin = base_target.records_type_origin
            else:
                base_type = STORAGE_CLASS_TYPES.get(
                    cls_name,
                    InstanceType(class_id)
                    if _constructs_own_instances(collector, class_id, cls_name)
                    else UNKNOWN,
                )
                cls_type = BUILTIN_CLASS_TYPES.get(
                    cls_name,
                    ClassType((base_type,))
                    if base_type is not UNKNOWN
                    else ANY_CLASS_TYPE,
                )
                records_type_origin = False
            by_ns[ns][cls_name] = ClassConstructorTarget(
                base_type=base_type,
                cls_type=cls_type,
                new_target=methods.get("__new__"),
                init_target=methods.get("__init__"),
                records_type_origin=records_type_origin,
            )
            if base_target is None and _constructs_own_instances(
                collector, class_id, cls_name
            ):
                methods_map, chain, shadowed = _resolved_class_info(
                    collector, class_id, cls_name
                )
                typed[class_id] = ClassFacts(
                    class_id,
                    methods_map,
                    {},
                    chain,
                    shadowed,
                    public_name=cls_name,
                    is_protocol=is_protocol,
                )
    return by_ns, typed


def _resolved_class_info(
    collector, class_id: str, cls_name: str
) -> tuple[dict[str, Lambda], tuple[str, ...], frozenset[str]]:
    """The class's methods in method-resolution order over its module
    bases, its single-inheritance chain of class identities, and the
    non-method attribute names bound on the chain."""
    identities = {name: identity for identity, name in collector.public_names.items()}
    chain = [class_id]
    current = cls_name
    while True:
        bases = [base for base in collector.bases.get(current, ()) if base != "object"]
        if len(bases) != 1 or bases[0] not in identities:
            if bases:
                chain = [class_id]
            break
        current = bases[0]
        chain.append(identities[current])
    resolved: dict[str, Lambda] = {}
    shadowed: set[str] = set()
    for identity in reversed(chain):
        for name in collector.other_bindings.get(identity, ()):
            resolved.pop(name, None)
            shadowed.add(name)
        for name, lam in collector.methods.get(identity, {}).items():
            shadowed.discard(name)
            resolved[name] = lam
    return resolved, tuple(chain), frozenset(shadowed)


def _constructs_own_instances(collector, class_id: str, cls_name: str) -> bool:
    """Whether calling the class returns an exact instance of it, as
    ``type.__call__`` does through ``object.__new__``: one definition under
    the name, no class keywords or metaclass, and no ``__new__`` on the class
    or on a base defined in the module. Bases imported from elsewhere are
    not inspected, so a class with such a base is excluded."""
    if collector.definitions.get(cls_name, 0) != 1:
        return False
    if class_id in collector.keyword_classes:
        return False
    if "__new__" in collector.methods.get(class_id, {}):
        return False
    identities = {name: identity for identity, name in collector.public_names.items()}
    pending = list(collector.bases.get(cls_name, ()))
    seen = set()
    while pending:
        base = pending.pop()
        if base == "object" or base in seen:
            continue
        seen.add(base)
        base_id = identities.get(base)
        if (
            base_id is None
            or base_id in collector.keyword_classes
            or "__new__" in collector.methods.get(base_id, {})
        ):
            return False
        pending.extend(collector.bases.get(base, ()))
    return True


def collect_global_constants(
    target,
    class_targets_by_ns: dict[str, dict[str, ClassConstructorTarget]] | None = None,
) -> dict[GlobalVar, ValueType]:
    """Unreassigned module-level private constant globals and their value types."""
    try:
        lambdas = list(target)
    except TypeError:
        lambdas = list(target.lambdas)
    seen = set(lambdas)
    for lambda_ in list(lambdas):
        current = lambda_
        while current.refs:
            parent = current.refs[0].home_lambda
            if parent is None or parent in seen:
                break
            seen.add(parent)
            lambdas.append(parent)
            current = parent
    is_protocol = any(lambda_.is_protocol for lambda_ in lambdas)
    if class_targets_by_ns is None and is_protocol:
        class_targets_by_ns = _collect_module_classes(lambdas)
    constants: dict[GlobalVar, ValueType] = {}
    visited_components = set()
    for lambda_ in lambdas:
        component = lambda_.component
        if component is None or component in visited_components:
            continue
        visited_components.add(component)
        ns_classes = (
            class_targets_by_ns.get(lambda_.namespace)
            if class_targets_by_ns is not None
            else None
        )
        visitor = _GlobalConstantStores(
            constants, is_protocol=is_protocol, class_targets=ns_classes
        )
        for block in component.blocks():
            for node in block.nodes():
                node.accept(visitor)
    return constants


def _noop():
    return None


def _with_defined_arguments(arguments, continuation):
    for argument in arguments:
        defined = argument.when_defined(_noop)
        if defined is not None:
            return defined
    return continuation()


class _ClosedMakeFunctionCall(NodeVisitor, LeafVisitor):
    def __init__(self, target: Lambda, ref: Ref):
        self.target = target
        self.ref = ref

    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.kind is not CallKind.KNOWN
            or node.fun_info is not MAKE_FUNCTION
            or len(node.args) != 1
            or node.args[0] is not self.ref.lvar
            or node.lvar is None
            or len(node.lvar.uses) != 1
        ):
            return None
        dest = node.lvar.dest
        if dest is None:
            return None
        return dest.accept(_ClosedFunctionBinding(self.target))


class _ClosedFunctionBinding(NodeVisitor, LeafVisitor):
    def __init__(self, target: Lambda):
        self.target = target

    def visit_Node(self, node):
        return None

    def visit_Set(self, node: Set):
        if node.lvar is not None and node.lvar.dest is not None:
            return None
        return node.var.accept(self)

    def visit_Leaf(self, leaf):
        return None

    def visit_LambdaVar(self, leaf: LambdaVar):
        if len(leaf.sets) != 1 or (leaf.home is not None and leaf in leaf.home.vars):
            return None
        final_refs = [ref for ref in leaf.refs if ref.final]
        if (
            len(final_refs) == 1
            and final_refs[0].lvar is not None
            and len(final_refs[0].lvar.uses) == 1
            and final_refs[0].dest is not None
        ):
            forwarded = final_refs[0].dest.accept(self)
            if forwarded is not None:
                return forwarded
        if not leaf.refs:
            return None
        call_refs = []
        for ref in leaf.refs:
            if (
                ref.lvar is None
                or len(ref.lvar.uses) != 1
                or ref.dest is None
                or not ref.dest.accept(_IsClosedPositionalCall(self.target, ref.lvar))
            ):
                return None
            call_refs.append(ref)
        if not any(ref.home_lambda is not self.target for ref in call_refs):
            return None
        return call_refs


class _IsClosedPositionalCall(NodeVisitor):
    def __init__(self, target: Lambda, fun_lvar):
        self.target = target
        self.fun_lvar = fun_lvar

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        return (
            node.fun is self.fun_lvar
            and node.kind in (CallKind.FULL, CallKind.GUARDED)
            and (node.info is None or not node.info.keywords)
            and len(node.args) == len(self.target.vars)
        )


def _collect_closed_lambdas(lambdas) -> dict[Lambda, tuple[Ref, ...]]:
    """Return nested functions whose function object never escapes and whose
    every reference is the callee of a direct positional call."""
    closed: dict[Lambda, tuple[Ref, ...]] = {}
    for lambda_ in lambdas:
        if (
            lambda_.kind is not FunctionalKind.NORMAL
            or not lambda_.vars
            or not lambda_.signature.positional_body
            or lambda_.signature.default_parameters
            or len(lambda_.refs) != 1
        ):
            continue
        ref = lambda_.refs[0]
        if ref.lvar is None or len(ref.lvar.uses) != 1 or ref.dest is None:
            continue
        call_refs = ref.dest.accept(_ClosedMakeFunctionCall(lambda_, ref))
        if call_refs is not None:
            closed[lambda_] = tuple(call_refs)
    return closed


class _ImportedModuleOrigin(NodeVisitor, LeafVisitor):
    """Resolve the module name an imported module object lvar originates from."""

    def __init__(self):
        self._active_aliases: set[LambdaVar] = set()

    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Combination(self, node: Combination):
        if node.kind is not CallKind.KNOWN or node.fun_info is None:
            return None
        if node.fun_info is IMPORT:
            return linked(node.info, node, "info").module_name
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        if leaf.home is not None and leaf in leaf.home.vars:
            return None
        if leaf in self._active_aliases:
            return None
        value_sets = [s for s in leaf.sets if s.value is not None]
        if len(value_sets) != 1:
            return None
        val = value_sets[0].value
        if val is None or len(val.uses) != 1:
            return None
        self._active_aliases.add(leaf)
        try:
            return val.uses[0].accept(self)
        finally:
            self._active_aliases.remove(leaf)


class _ImportBindingStores(NodeVisitor, LeafVisitor):
    """Collect ``from mod import orig as name`` global bindings in module bodies."""

    def __init__(self, imports: dict[str, tuple[str, str]]):
        self.imports = imports
        self._store: Set | None = None
        self._active_aliases: set[LambdaVar] = set()

    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Set(self, node: Set):
        self._store = node
        node.var.accept(self)
        self._store = None

    def visit_GlobalVar(self, leaf: GlobalVar):
        store = self._store
        self._store = None
        if store is None or store.value is None or len(leaf.sets) != 1:
            return
        if leaf.sets[0] is not store:
            return
        home = store.home_lambda
        if home is None or home.kind is not FunctionalKind.TOPLEVEL:
            return
        if len(store.value.uses) != 1:
            return
        origin = store.value.uses[0].accept(self)
        if origin is not None:
            self.imports[leaf.name] = origin

    def visit_Combination(self, node: Combination):
        if node.kind is not CallKind.KNOWN or node.fun_info is None:
            return None
        if node.fun_info is GETATTR and len(node.args) == 1:
            if len(node.args[0].uses) != 1:
                return None
            mod_name = node.args[0].uses[0].accept(_ImportedModuleOrigin())
            if mod_name is not None:
                return mod_name, linked(node.info, node, "info").name
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf: LambdaVar):
        store = self._store
        self._store = None
        # Parameter stores may be annotation assertions of the incoming value,
        # not definitions. Alias cycles likewise establish no import origin.
        if leaf.home is not None and leaf in leaf.home.vars:
            return None
        if leaf in self._active_aliases:
            return None
        self._active_aliases.add(leaf)
        try:
            if store is not None:
                value_sets = [s for s in leaf.sets if s.value is not None]
                if (
                    store.value is None
                    or len(value_sets) != 1
                    or value_sets[0] is not store
                ):
                    return None
                if len(store.value.uses) != 1:
                    return None
                origin = store.value.uses[0].accept(self)
                if origin is not None:
                    self.imports[leaf.name] = origin
                return None
            value_sets = [s for s in leaf.sets if s.value is not None]
            if len(value_sets) != 1:
                return None
            val = value_sets[0].value
            if val is None or len(val.uses) != 1:
                return None
            return val.uses[0].accept(self)
        finally:
            self._active_aliases.remove(leaf)


class _ActiveLocallState(local):
    def __init__(self):
        self.derivations: set[tuple[Lambda, str]] = set()
        self.specialized_contexts: set[tuple[Lambda, object]] = set()


_ACTIVE_STATE = _ActiveLocallState()
_MAX_DERIVATION_DEPTH = 6


class _MakeFunctionDefaults(NodeVisitor):
    def __init__(self, target: Lambda, ref: Ref):
        self.target = target
        self.ref = ref

    def visit_Node(self, node):
        return ()

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is MAKE_FUNCTION
            and len(node.args) > 1
            and node.args[0] is self.ref.lvar
        ):
            return tuple(node.args[1:])
        return ()


class _MutatesFunctionDefaults(NodeVisitor):
    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        if node.kind is not CallKind.KNOWN or node.fun_info is None:
            return False
        if node.fun_info is SETATTR:
            return node.info.name in ("__defaults__", "__kwdefaults__")
        if node.fun_info is GETATTR:
            return node.info.name == "__kwdefaults__"
        return node.fun_info is DELATTR


_MUTATES_FUNCTION_DEFAULTS = _MutatesFunctionDefaults()


def analyze(
    lambdas,
    annotations: AnnotationLedger,
    *,
    imported_function_types: Mapping[tuple[str, str], ValueType] | None = None,
    imported_targets: (
        Mapping[tuple[str, str], Lambda | ClassConstructorTarget | ForeignCallTarget]
        | None
    ) = None,
    storage_methods: Mapping[tuple[str, str], ValueType] | None = None,
    builtin_return_types: Mapping[str, ValueType] | None = None,
    builtin_targets: (
        Mapping[str, Lambda | ClassConstructorTarget | ForeignCallTarget] | None
    ) = None,
    state_cache: dict | None = None,
) -> dict[Lambda, LambdaFacts]:
    """Solve local-call arguments and results, then publish entry contexts.

    General entries model externally supplied values. Observed integer
    contexts and an all-fixnum context describe optional guarded entries, never
    changes to a function's public contract. Return summaries start empty, grow
    by union, and widen cyclic intervals before publication.
    """
    lambdas = list(lambdas)
    is_protocol_unit = any(lam.is_protocol for lam in lambdas)
    if not is_protocol_unit:
        if (
            storage_methods is None
            or imported_function_types is None
            or builtin_return_types is None
        ):
            from .sequence_protocol import stdlib_type_summary

            exported, methods, builtins = stdlib_type_summary()
            if imported_function_types is None:
                imported_function_types = exported
            if storage_methods is None:
                storage_methods = methods
            if builtin_return_types is None:
                builtin_return_types = builtins
        if imported_targets is None or builtin_targets is None:
            from .sequence_protocol import stdlib_target_summary

            exp_targets, b_targets = stdlib_target_summary()
            if imported_targets is None:
                imported_targets = exp_targets
            if builtin_targets is None:
                builtin_targets = b_targets
    else:
        if imported_targets is None:
            from .protocols import NATIVE_MODULE_TARGETS

            imported_targets = NATIVE_MODULE_TARGETS
        if builtin_targets is None:
            builtin_targets = BOOTSTRAP_BUILTIN_TARGETS
    prev_graphs: dict[Lambda, LambdaGraph] = (
        state_cache.get("graphs", {}) if state_cache is not None else {}
    )
    prev_revisions: dict[Lambda, int] = (
        state_cache.get("revisions", {}) if state_cache is not None else {}
    )
    graphs: dict[Lambda, LambdaGraph] = {}
    unchanged_lambdas: set[Lambda] = set()
    for lambda_ in lambdas:
        comp = lambda_.component
        current_blocks = tuple(comp.blocks()) if comp is not None else ()
        current_nodes = {block: tuple(block.nodes()) for block in current_blocks}
        prev_graph = prev_graphs.get(lambda_)
        if (
            prev_graph is not None
            and prev_revisions.get(lambda_) == lambda_.ir1_revision
            and prev_graph.blocks == current_blocks
            and prev_graph.block_nodes == current_nodes
        ):
            graphs[lambda_] = prev_graph
            unchanged_lambdas.add(lambda_)
        else:
            graphs[lambda_] = build_lambda_graph(lambda_)
    closed_lambdas = _collect_closed_lambdas(lambdas)
    class_targets_by_ns, class_facts = _collect_class_tables(lambdas)
    global_constants = collect_global_constants(lambdas, class_targets_by_ns)
    old_constant_ref_types: dict[Ref, ValueType | None] = {}
    for leaf, vtype in global_constants.items():
        for ref in leaf.refs:
            home = ref.home_lambda
            if (
                home is not None
                and home.kind is not FunctionalKind.TOPLEVEL
                and home.policy.safety < 3
                and ref.lexenv.policy.safety < 3
            ):
                old_constant_ref_types[ref] = ref.derived_type
                ref.derived_type = vtype
    global_targets = defaultdict(dict)
    global_counts: dict[str, dict[str, int]] = defaultdict(dict)
    global_leaves: dict[str, dict[str, GlobalVar]] = defaultdict(dict)
    import_bindings_by_ns: dict[str, dict[str, tuple[str, str]]] = defaultdict(dict)
    for lambda_ in lambdas:
        visitor = _GlobalFunctionStores(
            global_targets[lambda_.namespace],
            global_counts[lambda_.namespace],
            leaves=global_leaves[lambda_.namespace],
        )
        imp_visitor = _ImportBindingStores(import_bindings_by_ns[lambda_.namespace])
        graph = graphs[lambda_]
        for block in graph.blocks:
            for node in graph.block_nodes[block]:
                node.accept(visitor)
                node.accept(imp_visitor)
    lambda_set = set(lambdas)
    static_callees: dict[Lambda, list[Lambda]] = defaultdict(list)
    for target in lambdas:
        for ref in target.refs:
            caller = ref.home_lambda
            if caller is not None and caller is not target and caller in lambda_set:
                static_callees[caller].append(target)
    for target, call_refs in closed_lambdas.items():
        for ref in call_refs:
            caller = ref.home_lambda
            if caller is not None and caller is not target and caller in lambda_set:
                static_callees[caller].append(target)
    for ns, ns_targets in global_targets.items():
        ns_leaves = global_leaves[ns]
        for name, target_obj in ns_targets.items():
            target = target_obj.as_lambda()
            gvar = ns_leaves.get(name)
            if target is not None and gvar is not None and target in lambda_set:
                for ref in gvar.refs:
                    caller = ref.home_lambda
                    if (
                        caller is not None
                        and caller is not target
                        and caller in lambda_set
                    ):
                        static_callees[caller].append(target)
    ordered_lambdas: list[Lambda] = [
        lam for lam in lambdas if lam.kind is FunctionalKind.TOPLEVEL
    ]
    visited_order: set[Lambda] = set(ordered_lambdas)
    for root in lambdas:
        if root in visited_order:
            continue
        stack: list[tuple[Lambda, bool]] = [(root, False)]
        while stack:
            cur, done = stack.pop()
            if done:
                ordered_lambdas.append(cur)
                continue
            if cur in visited_order:
                continue
            visited_order.add(cur)
            stack.append((cur, True))
            for callee in reversed(static_callees.get(cur, ())):
                if callee not in visited_order:
                    stack.append((callee, False))
    lambda_order = {lam: idx for idx, lam in enumerate(ordered_lambdas)}
    imported_returns_by_ns: dict[str, dict[str, ValueType]] = defaultdict(dict)
    imported_targets_by_ns: dict[
        str, dict[str, Lambda | ClassConstructorTarget | ForeignCallTarget]
    ] = defaultdict(dict)
    for ns, bindings in import_bindings_by_ns.items():
        ns_returns = imported_returns_by_ns[ns]
        ns_targets = imported_targets_by_ns[ns]
        ns_counts = global_counts[ns]
        for local_name, (mod_name, orig_name) in bindings.items():
            if ns_counts.get(local_name, 0) > 1:
                continue
            if (
                imported_function_types is not None
                and (mod_name, orig_name) in imported_function_types
            ):
                ns_returns[local_name] = imported_function_types[mod_name, orig_name]
            if (
                imported_targets is not None
                and (mod_name, orig_name) in imported_targets
            ):
                ns_targets[local_name] = imported_targets[mod_name, orig_name]
    for ns, cls_targets in class_targets_by_ns.items():
        ns_targets = imported_targets_by_ns[ns]
        ns_counts = global_counts[ns]
        for cls_name, cls_target in cls_targets.items():
            if ns_counts.get(cls_name, 0) <= 1 and cls_name not in global_targets[ns]:
                ns_targets[cls_name] = cls_target
    proven_by_namespace: dict[str, frozenset[str]] = {
        ns: frozenset(
            name
            for name, target in {
                **imported_targets_by_ns.get(ns, {}),
                **targets,
            }.items()
            if (
                global_counts[ns].get(name, 0) <= 1
                and (fn := target.as_lambda()) is not None
                and fn.is_protocol
            )
        )
        for ns, targets in global_targets.items()
    }
    rebound_by_namespace: dict[str, frozenset[str]] = {
        ns: frozenset(counts) for ns, counts in global_counts.items()
    }
    namespaces = {lambda_.namespace for lambda_ in lambdas}
    global_constants_by_ns: dict[str, dict[str, ValueType]] = {}
    for ns in namespaces:
        rebound = rebound_by_namespace.get(ns, frozenset())
        ns_consts: dict[str, ValueType] = {
            cls_name: cls_type
            for cls_name, cls_type in BUILTIN_CLASS_TYPES.items()
            if cls_name not in rebound
        }
        ns_counts = global_counts[ns]
        for cls_name, cls_target in class_targets_by_ns.get(ns, {}).items():
            if ns_counts.get(cls_name, 0) <= 1:
                ns_consts[cls_name] = cls_target.cls_type
        for local_name, (mod_name, orig_name) in import_bindings_by_ns.get(
            ns, {}
        ).items():
            if ns_counts.get(local_name, 0) > 1:
                continue
            if (mod_name, orig_name) == ("typing", "TYPE_CHECKING"):
                ns_consts[local_name] = BOOLEAN_FALSE
                continue
            if (
                imported_targets is not None
                and (mod_name, orig_name) in imported_targets
            ):
                cls_const = imported_targets[mod_name, orig_name].global_constant_type()
                if cls_const is not UNKNOWN:
                    ns_consts[local_name] = cls_const
            if (
                orig_name.startswith("_")
                and imported_function_types is not None
                and (mod_name, orig_name) in imported_function_types
                and (
                    imported_targets is None
                    or (mod_name, orig_name) not in imported_targets
                )
            ):
                ns_consts[local_name] = imported_function_types[mod_name, orig_name]
        global_constants_by_ns[ns] = ns_consts
    for leaf, vtype in global_constants.items():
        if leaf.sets and leaf.sets[0].home_lambda is not None:
            ns = leaf.sets[0].home_lambda.namespace
            global_constants_by_ns.setdefault(ns, {})[leaf.name] = vtype
    defaults_mutated = any(
        node.accept(_MUTATES_FUNCTION_DEFAULTS)
        for graph in graphs.values()
        for block in graph.blocks
        for node in graph.block_nodes[block]
    )
    if not defaults_mutated:
        for lambda_ in lambdas:
            if (
                lambda_.signature.default_parameters
                and len(lambda_.refs) == 1
                and lambda_.refs[0].dest is not None
            ):
                default_lvars = lambda_.refs[0].dest.accept(
                    _MakeFunctionDefaults(lambda_, lambda_.refs[0])
                )
                if default_lvars:
                    ns_consts = global_constants_by_ns.get(lambda_.namespace, {})
                    def_types: list[ValueType] = []
                    for def_lvar in default_lvars:
                        lit = def_lvar.accept(_MadeConstant())
                        if lit is not None:
                            def_types.append(constant_type(lit))
                        elif (
                            len(def_lvar.uses) == 1
                            and (
                                gname := def_lvar.uses[0].accept(
                                    _GLOBAL_CALLEE_REF_NAME
                                )
                            )
                            is not None
                        ):
                            def_types.append(ns_consts.get(gname, UNKNOWN))
                        else:
                            def_types.append(UNKNOWN)
                    lambda_.default_types = tuple(def_types)
    captured_vars: list = []
    seen_captured: set = set()
    for lambda_ in lambdas:
        if lambda_.environment is not None:
            for var in lambda_.environment.closure:
                if var not in seen_captured:
                    seen_captured.add(var)
                    captured_vars.append(var)
    closed_captures = {
        var
        for var in captured_vars
        if all(
            lambda_ in closed_lambdas
            for lambda_ in lambdas
            if lambda_.environment is not None and var in lambda_.environment.closure
        )
    }
    for var in captured_vars:
        if (
            (var.indirect and var not in closed_captures)
            or not var.sets
            or (var in var.home.vars and var.name not in var.home.local_annotations)
        ):
            var.optimistic_type = UNKNOWN
        else:
            var.optimistic_type = BOTTOM
    var_changes = defaultdict(int)
    full = IntegerType(FIXNUM_MIN, FIXNUM_MAX)
    parameters: dict[tuple[Lambda, str], tuple[ValueType, ...]] = {}
    # External entries construct these exact containers before entering the
    # body; their contents and, for tuples, arity remain unknown.
    binding_types = {
        ast.ParameterKind.VAR_POSITIONAL: TupleType(),
        ast.ParameterKind.VAR_KEYWORD: DICT_TYPE,
    }
    for lambda_ in lambdas:
        signature = lambda_.signature
        if lambda_.vars and not signature.variadic:
            gen_params = [
                _ParameterUse(var, storage_methods).general_type(
                    lambda_.local_annotations.get(var.name)
                )
                if contract is None
                and ("local", lambda_, var.name) not in annotations.ignored
                else UNKNOWN
                for var, contract in zip(
                    lambda_.vars, signature.annotations, strict=True
                )
            ]
            if gen_params[0] is UNKNOWN:
                self_type = lambda_.protocol_self_type()
                if self_type is not None:
                    gen_params[0] = self_type
            parameters[lambda_, "general"] = tuple(gen_params)
            is_typed_method = (
                lambda_.method_class is not None
                and lambda_.method_class in class_facts
                and lambda_ in class_facts[lambda_.method_class].methods.values()
                and signature.kinds[0]
                in (
                    ast.ParameterKind.POSITIONAL_ONLY,
                    ast.ParameterKind.POSITIONAL_OR_KEYWORD,
                )
                and signature.annotations[0] is None
                and lambda_.vars[0].name not in lambda_.local_annotations
            )
            if is_typed_method:
                spec_params = (
                    InstanceType(lambda_.method_class),
                    *(
                        _ParameterUse(var, storage_methods).analyze(
                            contract,
                            full,
                            lambda_.local_annotations.get(var.name)
                            if ("local", lambda_, var.name) not in annotations.ignored
                            else None,
                        )
                        for var, contract in zip(
                            lambda_.vars[1:],
                            signature.annotations[1:],
                            strict=True,
                        )
                    ),
                )
                parameters[lambda_, "fixnum"] = spec_params
            else:
                spec_params = tuple(
                    _ParameterUse(var, storage_methods).analyze(
                        contract,
                        full,
                        lambda_.local_annotations.get(var.name)
                        if ("local", lambda_, var.name) not in annotations.ignored
                        else None,
                    )
                    for var, contract in zip(
                        lambda_.vars, signature.annotations, strict=True
                    )
                )
                if all(param is not UNKNOWN for param in spec_params):
                    parameters[lambda_, "fixnum"] = spec_params
                else:
                    partial_params = tuple(
                        param
                        if param is not UNKNOWN
                        and (
                            contract is not None
                            or (
                                var.name in lambda_.local_annotations
                                and ("local", lambda_, var.name)
                                not in annotations.ignored
                            )
                            or lambda_.vars[0].name != "self"
                        )
                        else UNKNOWN
                        for var, param, contract in zip(
                            lambda_.vars,
                            spec_params,
                            signature.annotations,
                            strict=True,
                        )
                    )
                    if any(param is not UNKNOWN for param in partial_params):
                        parameters[lambda_, "fixnum"] = partial_params
        else:
            gen_params = [binding_types.get(kind, UNKNOWN) for kind in signature.kinds]
            if lambda_.vars and gen_params[0] is UNKNOWN:
                self_type = lambda_.protocol_self_type()
                if self_type is not None:
                    gen_params[0] = self_type
            parameters[lambda_, "general"] = tuple(gen_params)
    summaries: dict[tuple[Lambda, str], ValueType] = dict.fromkeys(parameters, BOTTOM)
    return_changes = defaultdict(int)
    argument_changes = defaultdict(int)
    closed_seeded: set[Lambda] = set()
    bodies: dict[tuple[Lambda, str], Body] = {}
    # What each body's last propagation read: its parameters and, for every
    # Each entry records ``(expected_parameters, consulted_calls, closure_types,
    # consulted_targets)`` for the most recent propagation of ``key``. A body
    # whose inputs and resolved call return types are unchanged would propagate
    # identically.
    inputs: dict[tuple[Lambda, str], tuple] = {}
    deriver_cache: dict[tuple[Lambda, tuple[ValueType, ...]], Body] = {}
    setattr_sites: list[tuple[Lambda, Combination]] = []
    setitem_sites: list[tuple[Lambda, Combination]] = []
    list_mutation_sites: list[tuple[Lambda, Combination]] = []
    if class_facts:
        for lambda_ in lambdas:
            graph = graphs[lambda_]
            site_collector = _CollectSetattrSites([], [], [])
            for block in graph.blocks:
                for node in graph.block_nodes[block]:
                    node.accept(site_collector)
            for site in site_collector.sites:
                setattr_sites.append((lambda_, site))
            for site in site_collector.setitems:
                setitem_sites.append((lambda_, site))
            for site in site_collector.list_mutations:
                list_mutation_sites.append((lambda_, site))
    field_changes = 0
    initial_pending_reset: set[tuple[Lambda, str]] = set()
    must_recompute: set[Lambda] = set(lambdas)
    if (
        state_cache is not None
        and "bodies" in state_cache
        and state_cache.get("ignored") == set(annotations.ignored)
    ):
        prev_bodies = state_cache["bodies"]
        prev_inputs = state_cache["inputs"]
        prev_summaries = state_cache["summaries"]
        prev_parameters = state_cache["parameters"]
        prev_deriver_cache = state_cache["deriver_cache"]
        prev_captured_types = state_cache.get("captured_types", {})
        prev_class_fields = state_cache.get("class_fields", {})
        directly_changed = set(lambdas) - unchanged_lambdas
        while True:
            added_direct: set[Lambda] = set()
            for target, call_refs in closed_lambdas.items():
                if target not in directly_changed and any(
                    ref.home_lambda in directly_changed for ref in call_refs
                ):
                    added_direct.add(target)
            for var in captured_vars:
                if var not in prev_captured_types or any(
                    store.home_lambda in directly_changed for store in var.sets
                ):
                    for lam in lambdas:
                        if (
                            lam not in directly_changed
                            and lam.environment is not None
                            and var in lam.environment.closure
                        ):
                            added_direct.add(lam)
            if not added_direct:
                break
            directly_changed.update(added_direct)
        affected = set(directly_changed)
        while True:
            added = {
                key[0]
                for key, recorded in prev_inputs.items()
                if key[0] not in affected and affected.intersection(recorded[3])
            }
            for target, call_refs in closed_lambdas.items():
                if target not in affected and any(
                    ref.home_lambda in affected for ref in call_refs
                ):
                    added.add(target)
            for var in captured_vars:
                if var not in prev_captured_types or any(
                    store.home_lambda in affected for store in var.sets
                ):
                    for lam in lambdas:
                        if (
                            lam not in affected
                            and lam.environment is not None
                            and var in lam.environment.closure
                        ):
                            added.add(lam)
            if not added:
                break
            affected.update(added)
        back_edge_affected = {
            callee
            for k, rec in prev_inputs.items()
            if k[0] in affected
            for callee in rec[3]
            if callee in affected
            and lambda_order.get(k[0], 0) <= lambda_order.get(callee, 0)
        }
        must_recompute = directly_changed | back_edge_affected
        if prev_class_fields:
            for cls_id, fields_map in prev_class_fields.items():
                if cls_id in class_facts:
                    class_facts[cls_id].fields = dict(fields_map)
        for var in captured_vars:
            if var in prev_captured_types and all(
                store.home_lambda not in must_recompute for store in var.sets
            ):
                var.optimistic_type = prev_captured_types[var]
        affected_observed_targets = {
            target
            for (caller, _), prev_body in prev_bodies.items()
            if caller in must_recompute
            for call in prev_body.calls
            if call.target is not None
            and (target := call.target.as_lambda()) is not None
        }
        for key, prev_body in prev_bodies.items():
            lam, ctx_name = key
            if lam not in must_recompute:
                if ctx_name == "observed" and lam in affected_observed_targets:
                    continue
                if key not in parameters and key in prev_parameters:
                    parameters[key] = prev_parameters[key]
                elif (
                    ctx_name == "general"
                    and lam in closed_lambdas
                    and key in prev_parameters
                ):
                    parameters[key] = prev_parameters[key]
                    closed_seeded.add(lam)
                if key in parameters and parameters[key] == prev_parameters.get(key):
                    bodies[key] = prev_body
                    inputs[key] = prev_inputs[key]
                    summaries[key] = prev_summaries[key]
                    if lam in affected:
                        initial_pending_reset.add(key)
        if not affected:
            deriver_cache.update(prev_deriver_cache)
        else:
            for d_key, d_body in prev_deriver_cache.items():
                if d_key[0] not in affected:
                    deriver_cache[d_key] = d_body

    def lookup_return_type(
        target, arguments, context_parameters, context_summaries
    ) -> ValueType:
        obs = context_parameters.get((target, "observed"))
        fix = context_parameters.get((target, "fixnum"))
        gen = context_parameters.get((target, "general"))
        order_contexts = (
            ("fixnum", "observed", "general")
            if obs is not None
            and fix is not None
            and all(a.fits(b) or a is UNKNOWN for a, b in zip(obs, fix, strict=True))
            and not all(a.fits(b) for a, b in zip(obs, fix, strict=True))
            else ("observed", "fixnum", "general")
        )
        for context in order_contexts:
            key = target, context
            expected = context_parameters.get(key)
            if expected is not None and all(
                actual.fits(wanted)
                for actual, wanted in zip(arguments, expected, strict=True)
            ):
                if (
                    context != "general"
                    and gen is not None
                    and all(
                        actual.fits(wanted)
                        for actual, wanted in zip(arguments, gen, strict=True)
                    )
                    and all(g.fits(e) for g, e in zip(gen, expected, strict=True))
                ):
                    return context_summaries[target, "general"]
                return context_summaries[key]
        return UNKNOWN

    def closure_inputs(lambda_):
        if lambda_.environment is None:
            return ()
        return tuple(var.optimistic_type for var in lambda_.environment.closure)

    post_graphs: dict[Lambda, LambdaGraph] = {}
    analysis_finished = False

    def _current_graph(target_lam: Lambda) -> LambdaGraph:
        if not analysis_finished:
            return graphs[target_lam]
        comp = target_lam.component
        cached_graph = post_graphs.get(target_lam)
        if cached_graph is not None and (comp is None or not comp.reoptimize):
            return cached_graph
        current_blocks = tuple(comp.blocks()) if comp is not None else ()
        current_nodes = {block: tuple(block.nodes()) for block in current_blocks}
        base_graph = cached_graph or graphs.get(target_lam)
        if (
            base_graph is not None
            and base_graph.blocks == current_blocks
            and base_graph.block_nodes == current_nodes
        ):
            post_graphs[target_lam] = base_graph
            return base_graph
        # Inserting a checked result changes def-use edges within existing
        # blocks. Cached propagation must include the new Cast producers.
        cached_graph = build_lambda_graph(target_lam)
        post_graphs[target_lam] = cached_graph
        deriver_cache.clear()
        return cached_graph

    def _resolve_deriver_call(
        callee: Fact, arguments, location=None, keywords=()
    ) -> ValueType:
        if not callee.proven or callee.target is None:
            return UNKNOWN
        return callee.target.derive_return_type(arguments, keywords)

    def _propagate_for_deriver(
        target_lam: Lambda, param_types: tuple[ValueType, ...], mode: str
    ) -> Body | None:
        graph = _current_graph(target_lam)
        if graph is graphs.get(target_lam):
            cur_clos = closure_inputs(target_lam)
            for ctx_name in ("general", "fixnum", "observed"):
                ctx_key = (target_lam, ctx_name)
                if (
                    param_types == parameters.get(ctx_key)
                    and (ctx_body := bodies.get(ctx_key)) is not None
                    and (rec := inputs.get(ctx_key)) is not None
                    and rec[2] == cur_clos
                ):
                    return ctx_body
        cache_key = (target_lam, param_types)
        cached = deriver_cache.get(cache_key)
        if cached is not None:
            return cached
        active_derivations = _ACTIVE_STATE.derivations
        active_key = (target_lam, mode)
        if (
            active_key in active_derivations
            or len(active_derivations) >= _MAX_DERIVATION_DEPTH
        ):
            return None
        active_derivations.add(active_key)
        try:
            proven_globals = (
                proven_by_namespace.get(target_lam.namespace, frozenset())
                if target_lam.kind is not FunctionalKind.TOPLEVEL
                else frozenset()
            )
            with flow_propagation():
                body = propagate(
                    target_lam,
                    global_targets[target_lam.namespace],
                    param_types,
                    _resolve_deriver_call,
                    AnnotationLedger(),
                    graph=graph,
                    proven_globals=proven_globals,
                    rebound_globals=rebound_by_namespace.get(
                        target_lam.namespace, frozenset()
                    ),
                    global_constants=global_constants_by_ns.get(target_lam.namespace),
                    imported_returns=(
                        imported_returns_by_ns.get(target_lam.namespace)
                        if target_lam.policy.safety < 3
                        else None
                    ),
                    imported_targets=(
                        imported_targets_by_ns.get(target_lam.namespace)
                        if target_lam.policy.safety < 3
                        else None
                    ),
                    module_returns=(
                        imported_function_types
                        if target_lam.policy.safety < 3
                        else None
                    ),
                    module_targets=(
                        imported_targets if target_lam.policy.safety < 3 else None
                    ),
                    storage_methods=storage_methods,
                    builtin_return_types=builtin_return_types,
                    builtin_targets=builtin_targets,
                    class_facts=class_facts,
                )
            if len(deriver_cache) > 4096:
                deriver_cache.clear()
            deriver_cache[cache_key] = body
            return body
        finally:
            active_derivations.discard(active_key)

    def _make_derivers(target_lam: Lambda):
        def _derive_ret(param_types: tuple[ValueType, ...]) -> ValueType:
            gen_summary = summaries.get((target_lam, "general"), UNKNOWN)
            if not any(
                c is not None and c.contradicts(p)
                for c, p in zip(target_lam.signature.annotations, param_types)
            ) and (
                gen_summary in (BYTES_TYPE, BYTEARRAY_TYPE, FLOAT, NONE_TYPE)
                or gen_summary.instance_class() is not None
                or (
                    gen_summary == TEXT_TYPE
                    and not any(
                        p.fits_text(TEXT_TYPE) and p != TEXT_TYPE for p in param_types
                    )
                )
            ):
                rec = inputs.get((target_lam, "general"))
                if rec is not None and rec[2] == closure_inputs(target_lam):
                    return gen_summary
            body = _propagate_for_deriver(target_lam, param_types, "ret")
            if body is None:
                return gen_summary.normalized_bottom()
            return body.return_type.normalized_bottom()

        def _derive_self(param_types: tuple[ValueType, ...]) -> ValueType | None:
            body = _propagate_for_deriver(target_lam, param_types, "self")
            if body is None:
                return None
            return body.self_type

        def _derive_branch(
            param_types: tuple[ValueType, ...], truth: bool
        ) -> tuple[ValueType, ...] | None:
            body = _propagate_for_deriver(target_lam, param_types, "branch")
            if body is None or not body.return_states:
                return None
            active_derivations = _ACTIVE_STATE.derivations
            active_key = (target_lam, "branch_narrow")
            if (
                active_key in active_derivations
                or len(active_derivations) >= _MAX_DERIVATION_DEPTH
            ):
                return None
            active_derivations.add(active_key)
            try:
                from .constraint import _Walk

                walk = _Walk(
                    target_lam,
                    global_targets[target_lam.namespace],
                    param_types,
                    _resolve_deriver_call,
                    AnnotationLedger(),
                    graph=_current_graph(target_lam),
                    proven_globals=proven_by_namespace.get(
                        target_lam.namespace, frozenset()
                    ),
                    rebound_globals=rebound_by_namespace.get(
                        target_lam.namespace, frozenset()
                    ),
                    global_constants=global_constants_by_ns.get(target_lam.namespace),
                    imported_returns=imported_returns_by_ns.get(target_lam.namespace),
                    imported_targets=imported_targets_by_ns.get(target_lam.namespace),
                    module_returns=imported_function_types,
                    module_targets=imported_targets,
                    storage_methods=storage_methods,
                    builtin_return_types=builtin_return_types,
                    builtin_targets=builtin_targets,
                    class_facts=class_facts,
                )
                return walk.narrow_return_parameters(body, truth)
            finally:
                active_derivations.discard(active_key)

        return _derive_ret, _derive_self, _derive_branch

    for lambda_ in lambdas:
        if lambda_.kind is FunctionalKind.NORMAL:
            ret_d, self_d, branch_d = _make_derivers(lambda_)
            lambda_.type_deriver = ret_d
            lambda_.self_type_deriver = self_d
            lambda_.branch_deriver = branch_d

    # The canonical (callee, arguments) queries and return types consulted
    # during the current propagation.
    consulted: dict[tuple[Lambda, tuple], ValueType] = {}
    pending_reset: set[tuple[Lambda, str]] = set(initial_pending_reset)

    while True:
        changed = False
        narrowed_targets: set[Lambda] = set()
        previous_parameters = dict(parameters)

        def resolve(
            callee: Fact,
            arguments,
            location=None,
            keywords=(),
            context_parameters=previous_parameters,
            context_summaries=summaries,
        ) -> ValueType:
            return callee.type.when_defined(
                lambda: _with_defined_arguments(
                    arguments,
                    lambda: resolve_defined(
                        callee,
                        arguments,
                        context_parameters,
                        context_summaries,
                        location,
                        keywords,
                    ),
                )
            )

        def resolve_defined(
            callee: Fact,
            arguments,
            context_parameters,
            context_summaries,
            location,
            keywords,
        ):
            # A name's predicted target is never sufficient to publish a return
            # type. Only an already-loaded value with proven identity is local.
            if not callee.proven or callee.target is None:
                return UNKNOWN
            target = callee.target.as_lambda()
            if target is None:
                return callee.target.derive_return_type(arguments, keywords)
            order = target.call_order(keywords, len(arguments))
            if order is None:
                if target.signature.variadic:
                    gen = context_parameters.get((target, "general"))
                    if gen is not None:
                        result = context_summaries.get((target, "general"), UNKNOWN)
                        consulted[target, gen] = result
                        return result
                return UNKNOWN
            arguments = tuple(
                arguments[index] if index >= 0 else UNKNOWN for index in order
            )
            for index, (actual, contract, var) in enumerate(
                zip(arguments, target.signature.annotations, target.vars, strict=True)
            ):
                if contract is not None:
                    annotations.check(
                        contract, actual, location, ("local", target, var.name)
                    )
                else:
                    declared = target.local_annotations.get(var.name)
                    if declared is not None and declared.contradicts(actual):
                        annotations.ignored.add(("local", target, var.name))
                        gen = parameters.get((target, "general"))
                        if gen is not None and gen[index] is not UNKNOWN:
                            updated = list(gen)
                            updated[index] = UNKNOWN
                            parameters[target, "general"] = tuple(updated)
            result = lookup_return_type(
                target, arguments, context_parameters, context_summaries
            )
            if result is UNKNOWN and arguments != context_parameters.get(
                (target, "general")
            ):
                derived = target.derive_return_type(arguments)
                if derived is not UNKNOWN and derived is not BOTTOM:
                    result = derived
            consulted[target, arguments] = result
            return result

        for lambda_ in ordered_lambdas:
            for ctx_name in ("observed", "fixnum", "general"):
                key = lambda_, ctx_name
                expected = previous_parameters.get(key)
                if expected is None:
                    continue
                recorded = inputs.get(key)
                current_closure = closure_inputs(lambda_)
                if (
                    recorded is not None
                    and recorded[0] == expected
                    and recorded[2] == current_closure
                    and all(
                        (
                            new_res := lookup_return_type(
                                target, args, previous_parameters, summaries
                            )
                        )
                        == result
                        or (
                            new_res is UNKNOWN
                            and args != previous_parameters.get((target, "general"))
                            and result is not UNKNOWN
                            and result is not BOTTOM
                            and target.derive_return_type(args) == result
                        )
                        for (target, args), result in recorded[1].items()
                    )
                ):
                    body = bodies[key]
                    pending_reset.discard(key)
                    old_summary = summaries[key]
                else:
                    consulted.clear()
                    proven_globals = (
                        proven_by_namespace.get(lambda_.namespace, frozenset())
                        if lambda_.kind is not FunctionalKind.TOPLEVEL
                        else frozenset()
                    )
                    with flow_propagation():
                        body = propagate(
                            lambda_,
                            global_targets[lambda_.namespace],
                            expected,
                            resolve,
                            annotations,
                            graph=graphs[lambda_],
                            proven_globals=proven_globals,
                            rebound_globals=rebound_by_namespace.get(
                                lambda_.namespace, frozenset()
                            ),
                            global_constants=global_constants_by_ns.get(
                                lambda_.namespace
                            ),
                            imported_returns=(
                                imported_returns_by_ns.get(lambda_.namespace)
                                if lambda_.policy.safety < 3
                                else None
                            ),
                            imported_targets=(
                                imported_targets_by_ns.get(lambda_.namespace)
                                if lambda_.policy.safety < 3
                                else None
                            ),
                            module_returns=(
                                imported_function_types
                                if lambda_.policy.safety < 3
                                else None
                            ),
                            module_targets=(
                                imported_targets if lambda_.policy.safety < 3 else None
                            ),
                            storage_methods=storage_methods,
                            builtin_return_types=builtin_return_types,
                            builtin_targets=builtin_targets,
                            class_facts=class_facts,
                        )
                    bodies[key] = body
                    inputs[key] = (
                        expected,
                        dict(consulted),
                        current_closure,
                        frozenset(target for target, _ in consulted),
                    )
                    old_summary = summaries[key]
                    if key in pending_reset:
                        pending_reset.discard(key)
                        summaries[key] = BOTTOM
                        return_changes.pop(key, None)
                returned = summaries[key].join(body.return_type)
                if returned != summaries[key]:
                    summaries[key] = returned
                    if returned != old_summary:
                        return_changes[key] += 1
                        if return_changes[key] > 3:
                            summaries[key] = widen_fact(
                                Fact(old_summary), Fact(returned)
                            ).type
                        if summaries[key] != old_summary:
                            changed = True

        for var in captured_vars:
            parameter = var in var.home.vars
            if var.indirect and var not in closed_captures:
                # Escaping cells expose replacement and deletion through
                # cell_contents, including replacement of nested containers.
                continue
            if parameter and var not in closed_captures:
                continue
            if not parameter and not var.sets:
                continue
            # Parameter initialization is a definition too. Only nonescaping
            # closures can use it without an environment-specific guard: an
            # escaping function exposes its cells to arbitrary external writes.
            joined = (
                parameters[var.home, "general"][var.home.vars.index(var)]
                if parameter
                else BOTTOM
            )
            for store in var.sets:
                home_body = bodies.get((store.home_lambda, "general"))
                if home_body is None:
                    continue
                if store.value is None:
                    joined = joined.join(UNBOUND)
                else:
                    joined = joined.join(
                        home_body.facts.get(store.value.use(), NO_FACT).type
                    )
            if joined != var.optimistic_type:
                var_changes[var] += 1
                if var_changes[var] > 3:
                    joined = widen_fact(Fact(var.optimistic_type), Fact(joined)).type
                if joined != var.optimistic_type:
                    previous_type = var.optimistic_type
                    var.optimistic_type = joined
                    deriver_cache.clear()
                    if (
                        previous_type is None
                        or previous_type is BOTTOM
                        or not previous_type.fits(joined)
                    ):
                        narrowed_targets.update(
                            lambda_
                            for lambda_ in lambdas
                            if lambda_.environment is not None
                            and var in lambda_.environment.closure
                        )
                    changed = True

        # The broad guard context is not evidence that callers actually supplied
        # that broad interval. Propagate observations through general/observed
        # entries, including recursive calls, independently of that context.
        seeds: dict[Lambda, tuple] = {}
        for target, call_refs in closed_lambdas.items():
            external_calls = []
            all_calls = []
            missing_external = False
            for ref in call_refs:
                caller = ref.home_lambda
                caller_body = bodies.get((caller, "general"))
                if caller_body is None:
                    if caller is not target:
                        missing_external = True
                    continue
                matching = [
                    call
                    for call in caller_body.calls
                    if call.target is target
                    and call.proven
                    and len(call.arguments) == len(target.vars)
                    and all(arg is not BOTTOM for arg in call.arguments)
                ]
                if not matching:
                    if caller is not target:
                        missing_external = True
                    continue
                all_calls.extend(matching)
                if caller is not target:
                    external_calls.extend(matching)
            if missing_external or not external_calls:
                continue
            source_calls = external_calls if target not in closed_seeded else all_calls
            joined_args = source_calls[0].arguments
            for call in source_calls[1:]:
                joined_args = tuple(
                    old.join(new)
                    for old, new in zip(joined_args, call.arguments, strict=True)
                )
            gen_key = target, "general"
            old_gen = parameters[gen_key]
            candidate_gen = []
            for arg_type, contract, var in zip(
                joined_args,
                target.signature.annotations,
                target.vars,
                strict=True,
            ):
                if arg_type is BOTTOM or arg_type is UNBOUND or arg_type.may_be_unbound:
                    arg_type = UNKNOWN
                if contract is not None and contract.contradicts(arg_type):
                    arg_type = UNKNOWN
                declared = (
                    target.local_annotations.get(var.name)
                    if contract is None
                    and ("local", target, var.name) not in annotations.ignored
                    else None
                )
                if declared is not None and declared.contradicts(arg_type):
                    arg_type = UNKNOWN
                candidate_gen.append(arg_type)
            new_gen = tuple(candidate_gen)
            if target not in closed_seeded:
                closed_seeded.add(target)
                merged_gen = tuple(
                    b if a is UNKNOWN or b.fits(a) else a.join(b)
                    for a, b in zip(old_gen, new_gen, strict=True)
                )
            else:
                merged_gen = tuple(
                    a.join(b) for a, b in zip(old_gen, new_gen, strict=True)
                )
                if merged_gen != old_gen:
                    argument_changes[gen_key] += 1
                    if argument_changes[gen_key] > 3:
                        merged_gen = tuple(
                            widen_fact(Fact(a), Fact(b)).type
                            for a, b in zip(old_gen, merged_gen, strict=True)
                        )
            if merged_gen != old_gen:
                if not all(a.fits(b) for a, b in zip(old_gen, merged_gen, strict=True)):
                    narrowed_targets.add(target)
                parameters[gen_key] = merged_gen
                changed = True
        for key, body in bodies.items():
            if (key[0], "observed") in bodies and key[1] in ("fixnum", "general"):
                continue
            if key[1] == "fixnum" and any(param.is_fixnum for param in parameters[key]):
                continue
            if (
                key[1] == "general"
                and (key[0], "fixnum") in bodies
                and not any(param.is_fixnum for param in parameters[key[0], "fixnum"])
            ):
                continue
            for call in body.calls:
                if any(arg is BOTTOM for arg in call.arguments):
                    continue
                target = call.target.as_lambda() if call.target is not None else None
                if (
                    target is None
                    or target not in graphs
                    or not target.signature.positional_body
                    or not target.vars
                    or len(call.arguments) != len(target.vars)
                ):
                    # Imported targets still predict guarded calls, but their
                    # defining analysis owns their argument and entry contexts.
                    continue
                for index, (actual, contract, var) in enumerate(
                    zip(
                        call.arguments,
                        target.signature.annotations,
                        target.vars,
                        strict=True,
                    )
                ):
                    if contract is None:
                        declared = target.local_annotations.get(var.name)
                        if declared is not None and declared.contradicts(actual):
                            annotations.ignored.add(("local", target, var.name))
                            gen = parameters.get((target, "general"))
                            if gen is not None and gen[index] is not UNKNOWN:
                                updated = list(gen)
                                updated[index] = UNKNOWN
                                parameters[target, "general"] = tuple(updated)
                                narrowed_targets.add(target)
                                changed = True
                raw_domains = tuple(
                    domain
                    if (domain := argument.argument_domain().dispatch_domain())
                    is not None
                    and (contract is None or not contract.contradicts(domain))
                    else UNKNOWN
                    for argument, contract in zip(
                        call.argument_facts,
                        target.signature.annotations,
                        strict=True,
                    )
                )
                if all(domain is UNKNOWN for domain in raw_domains):
                    continue
                fix_params = parameters.get((target, "fixnum"))
                domains = tuple(
                    domain
                    if domain is not UNKNOWN
                    else (
                        fix_param
                        if fix_params is not None
                        and (
                            (fix_param := fix_params[idx])
                            in (LIST_TYPE, TEXT_TYPE, BYTES_TYPE, BYTEARRAY_TYPE)
                            or fix_param.instance_class() is not None
                        )
                        else UNKNOWN
                    )
                    for idx, domain in enumerate(raw_domains)
                )
                if target not in seeds:
                    seeds[target] = domains
                else:
                    seeds[target] = tuple(
                        old.join(new).dispatch_domain() or UNKNOWN
                        for old, new in zip(seeds[target], domains, strict=True)
                    )
        for target, seed in seeds.items():
            key = target, "observed"
            old = parameters.get(key)
            if old is not None:
                seed = tuple(
                    b
                    if a is UNKNOWN and b is not UNKNOWN and argument_changes[key] == 0
                    else (a.join(b).dispatch_domain() or UNKNOWN)
                    for a, b in zip(old, seed, strict=True)
                )
                if old != seed:
                    argument_changes[key] += 1
                    if argument_changes[key] > 3:
                        seed = tuple(
                            widen_fact(Fact(a), Fact(b)).type.dispatch_domain()
                            or UNKNOWN
                            for a, b in zip(old, seed, strict=True)
                        )
            if old != seed:
                if old is None or not all(
                    a.fits(b) for a, b in zip(old, seed, strict=True)
                ):
                    narrowed_targets.add(target)
                parameters[key] = seed
                summaries.setdefault(key, BOTTOM)
                changed = True
        if narrowed_targets:
            # Propagate invalidation transitively along the recorded call graph
            # so only functions that depend on narrowed arguments or captures
            # reset their provisional return summary.
            affected = set(narrowed_targets)
            while True:
                added = {
                    key[0]
                    for key, recorded in inputs.items()
                    if key[0] not in affected and affected.intersection(recorded[3])
                }
                if not added:
                    break
                affected.update(added)
            back_edge_targets = {
                callee
                for k, rec in inputs.items()
                if k[0] in affected
                for callee in rec[3]
                if callee in affected and lambda_order[k[0]] <= lambda_order[callee]
            }
            for key, expected in parameters.items():
                if key == (key[0], "observed") and key[0] in narrowed_targets:
                    summaries[key] = BOTTOM
                    return_changes.pop(key, None)
                    pending_reset.discard(key)
                elif key[0] in affected:
                    recorded = inputs.get(key)
                    if (
                        recorded is None
                        or recorded[0] != expected
                        or recorded[2] != closure_inputs(key[0])
                        or key[0] in back_edge_targets
                    ):
                        summaries[key] = BOTTOM
                        return_changes.pop(key, None)
                        pending_reset.discard(key)
                    elif affected.intersection(recorded[3]):
                        pending_reset.add(key)
        if field_changes < 3 and _update_class_field_types(
            class_facts,
            setattr_sites,
            setitem_sites,
            bodies,
            parameters,
            list_mutation_sites,
        ):
            field_changes += 1
            inputs.clear()
            deriver_cache.clear()
            pending_reset.update(parameters)
            changed = True
        if not changed:
            break

    for var in captured_vars:
        if var.optimistic_type is None or var.optimistic_type is BOTTOM:
            var.optimistic_type = UNKNOWN
        else:
            var.optimistic_type = var.optimistic_type.normalized_bottom()
    if state_cache is not None:
        state_cache["captured_types"] = {
            var: var.optimistic_type for var in captured_vars
        }

    for ref, old_type in old_constant_ref_types.items():
        ref.derived_type = old_type

    prev_results: dict[Lambda, LambdaFacts] = (
        state_cache.get("results", {}) if state_cache is not None else {}
    )
    prev_bodies_ref: dict[tuple[Lambda, str], Body] = (
        state_cache.get("bodies", {}) if state_cache is not None else {}
    )
    prev_summaries_ref: dict[tuple[Lambda, str], ValueType] = (
        state_cache.get("summaries", {}) if state_cache is not None else {}
    )
    prev_parameters_ref: dict[tuple[Lambda, str], tuple[ValueType, ...]] = (
        state_cache.get("parameters", {}) if state_cache is not None else {}
    )
    results: dict[Lambda, LambdaFacts] = {}
    for lambda_ in lambdas:
        if (
            lambda_ not in must_recompute
            and lambda_ in prev_results
            and all(
                bodies.get((lambda_, ctx)) is prev_bodies_ref.get((lambda_, ctx))
                and summaries.get((lambda_, ctx))
                == prev_summaries_ref.get((lambda_, ctx))
                and parameters.get((lambda_, ctx))
                == prev_parameters_ref.get((lambda_, ctx))
                for ctx in ("general", "fixnum", "observed")
            )
        ):
            results[lambda_] = prev_results[lambda_]
            continue
        graph = graphs[lambda_]
        general = bodies[lambda_, "general"]
        facts = LambdaFacts(
            general.types(),
            general.targets(),
            return_type=summaries[lambda_, "general"].normalized_bottom(),
            parameters=parameters[lambda_, "general"],
            element_contracts=dict(general.element_contracts),
        )
        entries = []
        obs_params = parameters.get((lambda_, "observed"))
        fix_params = parameters.get((lambda_, "fixnum"))
        variant_order = (
            ("fixnum", "observed")
            if obs_params is not None
            and fix_params is not None
            and all(
                a.fits(b) or a is UNKNOWN
                for a, b in zip(obs_params, fix_params, strict=True)
            )
            and not all(a.fits(b) for a, b in zip(obs_params, fix_params, strict=True))
            else ("observed", "fixnum")
        )
        for identifier in variant_order:
            key = lambda_, identifier
            if key not in parameters:
                continue
            body = bodies[key]
            candidate_types = body.types()
            ret_type = summaries[key].normalized_bottom()
            if (
                identifier == "fixnum"
                and body.element_contracts == facts.element_contracts
                and not _specializes_body(
                    graph,
                    facts.types,
                    candidate_types,
                    facts.targets,
                    body.targets(),
                )
            ):
                continue
            if (
                identifier == "observed"
                and not any(param.is_fixnum for param in parameters[key])
                and body.element_contracts == facts.element_contracts
                and not _specializes_body(
                    graph,
                    facts.types,
                    candidate_types,
                    facts.targets,
                    body.targets(),
                )
                and ret_type == facts.return_type
            ):
                continue
            if (
                identifier == "observed"
                and (lambda_, "fixnum") in parameters
                and (
                    bodies[lambda_, "fixnum"].element_contracts
                    != facts.element_contracts
                    or _specializes_body(
                        graph,
                        facts.types,
                        bodies[lambda_, "fixnum"].types(),
                        facts.targets,
                        bodies[lambda_, "fixnum"].targets(),
                    )
                )
                and (
                    all(
                        obs_p is UNKNOWN or obs_p == fix_p
                        for obs_p, fix_p in zip(
                            parameters[key],
                            parameters[lambda_, "fixnum"],
                            strict=True,
                        )
                    )
                    or (
                        all(
                            obs_p.fits(fix_p)
                            for obs_p, fix_p in zip(
                                parameters[key],
                                parameters[lambda_, "fixnum"],
                                strict=True,
                            )
                        )
                        and not any(
                            obs_p.is_fixnum and not fix_p.is_fixnum
                            for obs_p, fix_p in zip(
                                parameters[key],
                                parameters[lambda_, "fixnum"],
                                strict=True,
                            )
                        )
                        and body.element_contracts
                        == bodies[lambda_, "fixnum"].element_contracts
                        and not _specializes_body(
                            graph,
                            bodies[lambda_, "fixnum"].types(),
                            candidate_types,
                            bodies[lambda_, "fixnum"].targets(),
                            body.targets(),
                        )
                        and ret_type == summaries[lambda_, "fixnum"].normalized_bottom()
                    )
                )
            ):
                continue
            entries.append(
                EntryContext(
                    identifier,
                    parameters[key],
                    candidate_types,
                    body.targets(),
                    ret_type,
                    dict(body.element_contracts),
                )
            )
        facts.variants = tuple(entries)
        results[lambda_] = facts
        _mark_guarded_calls(graph, general)
        _publish_derived_types(lambda_, graph, facts.types)
    analysis_finished = True
    if state_cache is not None:
        state_cache["graphs"] = graphs
        state_cache["revisions"] = {lam: lam.ir1_revision for lam in lambdas}
        state_cache["bodies"] = dict(bodies)
        state_cache["inputs"] = dict(inputs)
        state_cache["summaries"] = dict(summaries)
        state_cache["parameters"] = dict(parameters)
        state_cache["deriver_cache"] = dict(deriver_cache)
        state_cache["ignored"] = set(annotations.ignored)
        state_cache["results"] = dict(results)
        state_cache["class_fields"] = {
            cls_id: dict(cf.fields) for cls_id, cf in class_facts.items()
        }
    return results


def _lvar_current_type(lvar) -> ValueType | None:
    if lvar.derived_type is not None:
        return lvar.derived_type
    if not lvar.uses or any(use.derived_type is None for use in lvar.uses):
        return None
    derived = lvar.uses[0].derived_type
    for use in lvar.uses[1:]:
        derived = derived.join(use.derived_type)
    return derived


def _recompute_lvar_type(lvar) -> ValueType:
    if not lvar.uses:
        lvar.derived_type = BOTTOM
        return BOTTOM
    derived = (
        lvar.uses[0].derived_type if lvar.uses[0].derived_type is not None else UNKNOWN
    )
    for use in lvar.uses[1:]:
        use_type = use.derived_type if use.derived_type is not None else UNKNOWN
        derived = derived.join(use_type)
    lvar.derived_type = derived
    return derived


class _PublishNodeTypes(NodeVisitor):
    def __init__(self, types: dict[object, ValueType]):
        self.types = types
        self.old_lvar_types: dict[object, ValueType | None] = {}
        self.lvars: list = []

    def visit_ValuedNode(self, node):
        lvar = node.lvar
        if lvar is not None and lvar not in self.old_lvar_types:
            self.old_lvar_types[lvar] = _lvar_current_type(lvar)
            self.lvars.append(lvar)
        node.derived_type = self.types.get(node, UNKNOWN)


class _ReoptimizeNarrowedKnownCalls(NodeVisitor):
    def __init__(
        self,
        lambda_: Lambda,
        old_lvar_types: dict[object, ValueType | None],
    ):
        self.lambda_ = lambda_
        self.old_lvar_types = old_lvar_types
        self.reoptimized = False

    def visit_If(self, node):
        new_type = _recompute_lvar_type(node.test)
        if new_type != self.old_lvar_types.get(node.test) and (
            new_type.truth_value() is not None
            or ir1opt.constant_lvar_value(node.test) is not None
        ):
            node.reoptimize = True
            node.block.reoptimize = True
            if self.lambda_.component is not None:
                self.lambda_.component.reoptimize = True
            self.reoptimized = True

    def visit_Combination(self, node):
        if node.kind is CallKind.KNOWN and node.fun_info is not None:
            info = node.fun_info
        elif is_predictable_len_call(node):
            info = LEN
        elif (
            node.kind in (CallKind.FULL, CallKind.GUARDED)
            and len(node.fun.uses) == 1
            and not (node.info is not None and node.info.keywords)
            and node.lexenv.policy.safety < 3
        ):
            info = node.fun.uses[0].accept(_PREDICTABLE_GLOBAL_FUN_INFO)
            if info is None:
                return
        else:
            return
        if not (
            info.transforms or (info.optimizer is not None and info is not GETITEM)
        ):
            return
        checked_args = (
            (*node.args, *node.info.arguments)
            if info is RANGE_PREPARE and node.info is not None and not node.info.direct
            else node.args
        )
        if any(
            (new_type := _recompute_lvar_type(arg)) != self.old_lvar_types.get(arg)
            and new_type is not UNKNOWN
            for arg in checked_args
        ):
            node.reoptimize = True
            node.block.reoptimize = True
            if self.lambda_.component is not None:
                self.lambda_.component.reoptimize = True
            self.reoptimized = True


def _publish_derived_types(
    lambda_: Lambda, graph: LambdaGraph, types: dict[object, ValueType]
) -> bool:
    """Write ``general`` flow-derived types back onto IR1 nodes and lvars, and
    request IR1 reoptimization when a known call with transforms receives a
    narrowed operand."""
    publisher = _PublishNodeTypes(types)
    for block in graph.blocks:
        for node in graph.block_nodes[block]:
            node.accept(publisher)
    for lvar in publisher.lvars:
        _recompute_lvar_type(lvar)
    reoptimizer = _ReoptimizeNarrowedKnownCalls(lambda_, publisher.old_lvar_types)
    for block in graph.blocks:
        for node in graph.block_nodes[block]:
            node.accept(reoptimizer)
    return reoptimizer.reoptimized


def _would_reoptimize_for_context(lambda_: Lambda, context: EntryContext) -> bool:
    component = lambda_.component
    if component is None or context.id not in ("fixnum", "observed"):
        return False
    for block in component.blocks():
        for node in block.nodes():
            if node.accept(_NodeHasContextTransform(context)):
                return True
    return False


class _NodeHasContextTransform(NodeVisitor):
    def __init__(self, context: EntryContext):
        self.context = context

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        if node.kind is CallKind.KNOWN and node.fun_info is not None:
            info = node.fun_info
        elif is_predictable_len_call(node):
            info = LEN
        elif (
            node.kind in (CallKind.FULL, CallKind.GUARDED)
            and len(node.fun.uses) == 1
            and not (node.info is not None and node.info.keywords)
            and node.lexenv.policy.safety < 3
        ):
            info = node.fun.uses[0].accept(_PREDICTABLE_GLOBAL_FUN_INFO)
            if info is None:
                return False
        else:
            return False
        if not (
            info.transforms
            or (info.optimizer is not None and info is not GETITEM)
            or (
                loop.is_hoistable_op(info)
                and node.block.loop is not None
                and node.block.loop.is_natural
            )
        ):
            return False
        checked_args = (
            (*node.args, *node.info.arguments)
            if info is RANGE_PREPARE and node.info is not None and not node.info.direct
            else node.args
        )
        for arg in checked_args:
            if not arg.uses:
                continue
            cand_arg = self.context.types.get(arg.uses[0], UNKNOWN)
            for use in arg.uses[1:]:
                cand_arg = cand_arg.join(self.context.types.get(use, UNKNOWN))
            gen_arg = _lvar_current_type(arg) or UNKNOWN
            if (
                cand_arg is not UNKNOWN
                and cand_arg != gen_arg
                and not gen_arg.fits(cand_arg)
            ):
                return True
        return False


class _CollectNodeTypesAndTargets(NodeVisitor):
    def __init__(
        self,
        base_types: dict[object, ValueType],
        base_targets: dict[object, Lambda],
    ):
        self.types = dict(base_types)
        self.targets = dict(base_targets)

    def visit_ValuedNode(self, node):
        if node not in self.types and node.derived_type is not None:
            self.types[node] = node.derived_type

    def visit_Combination(self, node: Combination):
        self.visit_ValuedNode(node)
        if (
            node not in self.targets
            and node.kind is CallKind.GUARDED
            and node.predicted is not None
        ):
            self.targets[node] = node.predicted


@contextmanager
def specialized_ir1_context(lambda_: Lambda, context: EntryContext):
    """Run entry-context-specific IR1 optimization on a cloned component for
    ``context`` and restore the canonical IR1 component on exit."""
    active_contexts = _ACTIVE_STATE.specialized_contexts
    active_key = (lambda_, context.id)
    if active_key in active_contexts or not _would_reoptimize_for_context(
        lambda_, context
    ):
        yield
        return
    active_contexts.add(active_key)
    cloned_component, node_map, restore = clone_lambda_component(lambda_)
    old_types = context.types
    old_targets = context.targets
    old_element_contracts = context.element_contracts
    old_escape_facts = context.escape_facts
    old_base_escape_facts = context.base_escape_facts
    try:
        cloned_types = {
            node_map.get(node, node): vtype for node, vtype in old_types.items()
        }
        cloned_targets = {
            node_map.get(node, node): target for node, target in old_targets.items()
        }
        cloned_element_contracts = {
            node_map.get(node, node): contract
            for node, contract in old_element_contracts.items()
        }
        graph = build_lambda_graph(lambda_)
        reopt = _publish_derived_types(lambda_, graph, cloned_types)
        if reopt:
            ir1opt.ir1_optimize_until_done(cloned_component)
            join_blocks_if_possible(cloned_component)
            find_dfo(cloned_component)
            loop.analyze(cloned_component)
        hoisted = loop.hoist_loop_invariants(cloned_component)
        if reopt or hoisted:
            if hoisted:
                ir1opt.ir1_optimize_until_done(cloned_component)
                join_blocks_if_possible(cloned_component)
                find_dfo(cloned_component)
                loop.analyze(cloned_component)
            collector = _CollectNodeTypesAndTargets(cloned_types, cloned_targets)
            for block in cloned_component.blocks():
                for node in block.nodes():
                    node.accept(collector)
            cloned_types = collector.types
            cloned_targets = collector.targets
        context.types = cloned_types
        context.targets = cloned_targets
        context.element_contracts = cloned_element_contracts
        context.base_escape_facts = old_escape_facts or old_base_escape_facts
        context.escape_facts = None
        yield
    finally:
        active_contexts.discard(active_key)
        context.types = old_types
        context.targets = old_targets
        context.element_contracts = old_element_contracts
        context.escape_facts = old_escape_facts
        context.base_escape_facts = old_base_escape_facts
        restore()


def _mark_guarded_calls(graph: LambdaGraph, body: Body) -> None:
    """Record predicted targets on full calls so later phases and code
    generation can guard them; the general path always remains."""
    visitor = _MarkGuardedCall(body)
    for block in graph.blocks:
        for node in graph.block_nodes[block]:
            node.accept(visitor)


class _MarkGuardedCall(NodeVisitor):
    def __init__(self, body: Body):
        self.body = body

    def visit_Combination(self, node):
        if node.kind not in (CallKind.FULL, CallKind.GUARDED):
            return
        fact = self.body.facts.get(node.fun.use())
        if fact is None or fact.target is None:
            node.kind = CallKind.FULL
            node.predicted = None
            node.proven = False
            return
        node.kind = CallKind.GUARDED
        node.predicted = fact.target
        node.proven = fact.proven


__all__ = [
    "EntryContext",
    "LambdaFacts",
    "analyze",
    "collect_global_constants",
    "global_bindings",
    "join_fact",
    "specialized_ir1_context",
]
