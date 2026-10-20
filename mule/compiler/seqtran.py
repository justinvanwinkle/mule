"""Sequence IR1 transforms, following ``src/compiler/seqtran.lisp``.

Transforms registered via ``@deftransform`` fold constant-index tuple
subscripting when dropped elements are effect-free and non-raising, and
simplify truthiness negation of newly constructed empty tuples and lists.
"""

from __future__ import annotations

from dataclasses import replace

from mule import ast

from . import fndb
from .ctype import (
    BOOLEAN_TRUE,
    PYTHON_INTEGER,
    UNKNOWN,
    BytesType,
    IntegerType,
    ListType,
    TextType,
    TupleType,
    ValueType,
    ValueTypeVisitor,
)
from .ir1opt import (
    _flush_operand,
    constant_literal,
    lvar_type,
    replace_combination_with_constant,
)
from .ir1util import (
    add_lvar_use,
    delete_lvar_use,
    delete_ref,
    delete_set,
    insert_node_after,
    insert_node_before,
    reoptimize_lvar,
    replace_combination_with_lvar,
    retarget_known_combination,
    substitute_lvar_uses,
    unlink_node,
    use_lvar,
)
from .knownfun import KNOWN, FunInfo, deftransform
from .node import (
    CallKind,
    Combination,
    Constant,
    GlobalVar,
    If,
    KnownFunction,
    Lambda,
    LambdaVar,
    LeafVisitor,
    Lvar,
    Node,
    NodeVisitor,
    Ref,
    Set,
    linked,
)


class _ConstantIndex(ast.Visitor):
    """Extract an integer index from a constant literal, or None."""

    def visit_Node(self, node):
        return None

    def visit_IntegerLiteral(self, node: ast.IntegerLiteral) -> int:
        return node.value

    def visit_BooleanLiteral(self, node: ast.BooleanLiteral) -> int:
        return int(node.value)


_CONSTANT_INDEX = _ConstantIndex()


class _ExactIntegerLiteral(ast.Visitor):
    """Extract the value of an ``ast.IntegerLiteral``, or None."""

    def visit_Node(self, node):
        return None

    def visit_IntegerLiteral(self, node: ast.IntegerLiteral) -> int:
        return node.value


_EXACT_INTEGER_LITERAL = _ExactIntegerLiteral()


class _ConstantTupleElements(ast.Visitor):
    """Extract the element literals of a constant ``ast.Tuple``, or None."""

    def visit_Node(self, node):
        return None

    def visit_Tuple(self, node: ast.Tuple):
        return node.elements


_CONSTANT_TUPLE_ELEMENTS = _ConstantTupleElements()


class _ConstantIndexable(ast.Visitor):
    """Fold constant-index subscripting on constant tuple, text, or bytes literals."""

    def __init__(self, raw_index: int, span: ast.Span | None):
        self.raw_index = raw_index
        self.span = span

    def visit_Node(self, node):
        return None

    def visit_Tuple(self, node: ast.Tuple):
        n = len(node.elements)
        idx = self.raw_index + n if self.raw_index < 0 else self.raw_index
        if not (0 <= idx < n):
            return None
        selected = node.elements[idx]
        if self.span is not None:
            return replace(selected, span=self.span)
        return selected

    def visit_TextLiteral(self, node: ast.TextLiteral):
        n = len(node.value)
        idx = self.raw_index + n if self.raw_index < 0 else self.raw_index
        if not (0 <= idx < n):
            return None
        return ast.TextLiteral(node.value[idx], span=self.span)

    def visit_BytesLiteral(self, node: ast.BytesLiteral):
        n = len(node.value)
        idx = self.raw_index + n if self.raw_index < 0 else self.raw_index
        if not (0 <= idx < n):
            return None
        return ast.IntegerLiteral(node.value[idx], span=self.span)


class _SliceBound(ast.Visitor):
    """Extract ``(valid, bound)`` from a constant slice bound literal."""

    def visit_Node(self, node) -> tuple[bool, int | None]:
        return False, None

    def visit_NoneLiteral(self, node: ast.NoneLiteral) -> tuple[bool, None]:
        return True, None

    def visit_IntegerLiteral(self, node: ast.IntegerLiteral) -> tuple[bool, int]:
        return True, node.value

    def visit_BooleanLiteral(self, node: ast.BooleanLiteral) -> tuple[bool, int]:
        return True, int(node.value)


_SLICE_BOUND = _SliceBound()


class _ConstantSliceable(ast.Visitor):
    """Fold constant slicing on bounded constant text, bytes, or tuple literals."""

    def __init__(self, s: slice, span: ast.Span | None):
        self.slice = s
        self.span = span

    def visit_Node(self, node):
        return None

    def visit_TextLiteral(self, node: ast.TextLiteral):
        if len(node.value) <= 256:
            return ast.TextLiteral(node.value[self.slice], span=self.span)
        return None

    def visit_BytesLiteral(self, node: ast.BytesLiteral):
        if len(node.value) <= 256:
            return ast.BytesLiteral(node.value[self.slice], span=self.span)
        return None

    def visit_Tuple(self, node: ast.Tuple):
        if len(node.elements) <= 256:
            return ast.Tuple(list(node.elements[self.slice]), span=self.span)
        return None


class _BuildTupleProducer(NodeVisitor):
    """Return ``node`` when it is a known ``%build_tuple`` combination."""

    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if node.kind is CallKind.KNOWN and node.fun_info is fndb.BUILD_TUPLE:
            return node
        return None


_BUILD_TUPLE_PRODUCER = _BuildTupleProducer()


class _BuildSequenceProducer(NodeVisitor):
    """Return ``node`` when it is a known ``%build_tuple`` or ``%build_list``."""

    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if node.kind is CallKind.KNOWN and node.fun_info in (
            fndb.BUILD_TUPLE,
            fndb.BUILD_LIST,
        ):
            return node
        return None


_BUILD_SEQUENCE_PRODUCER = _BuildSequenceProducer()


class _EmptyContainerProducer(NodeVisitor):
    """Return ``node`` when it is an empty ``%build_tuple`` or ``%build_list``."""

    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info in (fndb.BUILD_TUPLE, fndb.BUILD_LIST)
            and len(node.args) == 0
        ):
            return node
        return None


_EMPTY_CONTAINER_PRODUCER = _EmptyContainerProducer()


class _SetToVar(NodeVisitor):
    """Whether a node is a binding store to ``var``."""

    def __init__(self, var: LambdaVar):
        self.var = var

    def visit_Node(self, node) -> bool:
        return False

    def visit_Set(self, node: Set) -> bool:
        return node.var is self.var and node.value is not None


class _ExpressionTempStore(NodeVisitor, LeafVisitor):
    """Extract ``(Set, temporary)`` when ``node`` stores into a single-set
    expression temporary ``LambdaVar``."""

    def visit_Node(self, node):
        return None

    def visit_Set(self, node: Set):
        if node.value is None or node.lvar is not None:
            return None
        temp = node.var.accept(self)
        if temp is None:
            return None
        return node, temp

    def visit_Leaf(self, leaf):
        return None

    def visit_LambdaVar(self, leaf: LambdaVar):
        if leaf.expression_temporary and len(leaf.sets) == 1:
            return leaf
        return None


_EXPRESSION_TEMP_STORE = _ExpressionTempStore()


class _LambdaVarTargetStore(NodeVisitor, LeafVisitor):
    """Return ``node`` when it is a simple ``Set`` to a ``LambdaVar``."""

    def visit_Node(self, node):
        return None

    def visit_Set(self, node: Set):
        if node.value is not None and node.lvar is None and node.var.accept(self):
            return node
        return None

    def visit_Leaf(self, leaf) -> bool:
        return False

    def visit_LambdaVar(self, leaf: LambdaVar) -> bool:
        return True


_LAMBDA_VAR_TARGET_STORE = _LambdaVarTargetStore()


class _TempRefCheck(NodeVisitor):
    """Match a ``Ref`` to ``temporary`` with the expected ``final`` flag."""

    def __init__(self, temporary: LambdaVar, *, final: bool):
        self.temporary = temporary
        self.final = final

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        if node.leaf is self.temporary and node.final is self.final:
            return node
        return None


class _UnpackRefCall(NodeVisitor):
    """Return ``node`` when it is a ``%unpack_ref`` call on ``(loaded, pos)``."""

    def __init__(self, loaded: Lvar, index: int):
        self.loaded = loaded
        self.index = index

    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.kind is not CallKind.KNOWN
            or node.fun_info is not fndb.UNPACK_REF
            or node.lvar is None
            or len(node.args) != 2
            or node.args[0] is not self.loaded
            or len(node.fun.uses) != 1
        ):
            return None
        pos_literal = constant_literal(node.args[1])
        if (
            pos_literal is None
            or pos_literal.accept(_EXACT_INTEGER_LITERAL) != self.index
        ):
            return None
        return node


def _following_node_in_block(node: Node) -> Node | None:
    ctran = node.next
    if ctran is None or ctran.block is not node.block:
        return None
    return ctran.next


def _lambda_var_bound_at(leaf: LambdaVar, ref: Ref) -> bool:
    if any(store.value is None for store in leaf.sets):
        return False
    if leaf.home is not None and leaf in leaf.home.vars:
        return True
    checker = _SetToVar(leaf)
    for node in ref.block.nodes():
        if node is ref:
            return False
        if node.accept(checker):
            return True
    return False


class _SafeDroppedElement(NodeVisitor, LeafVisitor):
    """Whether an unselected tuple element is a single-use, non-raising ``Ref``."""

    def __init__(self, lvar: Lvar):
        self.lvar = lvar
        self._ref: Ref | None = None

    def visit_Node(self, node) -> bool:
        return False

    def visit_Ref(self, node: Ref) -> bool:
        if node.final or lvar_type(self.lvar).may_be_unbound:
            return False
        self._ref = node
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf) -> bool:
        return False

    def visit_Constant(self, leaf: Constant) -> bool:
        return True

    def visit_KnownFunction(self, leaf: KnownFunction) -> bool:
        return True

    def visit_Lambda(self, leaf: Lambda) -> bool:
        return True

    def visit_LambdaVar(self, leaf: LambdaVar) -> bool:
        if leaf.type is not None:
            return not leaf.type.may_be_unbound
        ref = linked(self._ref, leaf, "reference")
        if ref.derived_type is not None and ref.derived_type is not UNKNOWN:
            return not ref.derived_type.may_be_unbound
        return _lambda_var_bound_at(leaf, ref)


def _is_safe_dropped_element(lvar: Lvar) -> bool:
    if len(lvar.uses) != 1:
        return False
    return lvar.uses[0].accept(_SafeDroppedElement(lvar))


class _DeletePureRef(NodeVisitor):
    def visit_Ref(self, node: Ref) -> None:
        delete_ref(node)
        unlink_node(node)


_DELETE_PURE_REF = _DeletePureRef()


def _delete_pure_ref(lvar: Lvar) -> None:
    uses = list(lvar.uses)
    lvar.dest = None
    for use in uses:
        use.lvar = None
    lvar.uses = []
    for use in uses:
        if use.prev is not None:
            use.accept(_DELETE_PURE_REF)


@deftransform("%unpack")
def _transform_unpack(node: Combination) -> bool:
    if node.lvar is None or len(node.args) != 2 or len(node.fun.uses) != 1:
        return False
    seq_lvar, count_lvar = node.args
    if len(seq_lvar.uses) != 1 or len(count_lvar.uses) != 1:
        return False
    count_literal = constant_literal(count_lvar)
    if count_literal is None:
        return False
    n = count_literal.accept(_EXACT_INTEGER_LITERAL)
    if n is None or n < 0:
        return False

    seq_producer = seq_lvar.uses[0]
    count_ref = count_lvar.uses[0]
    fun_ref = node.fun.uses[0]
    if (
        seq_producer.prev is None
        or count_ref.prev is None
        or fun_ref.prev is None
        or seq_producer.block is not node.block
        or _following_node_in_block(seq_producer) is not count_ref
        or _following_node_in_block(count_ref) is not fun_ref
        or _following_node_in_block(fun_ref) is not node
    ):
        return False

    seq_literal = constant_literal(seq_lvar)
    const_elements = None
    build_node = None
    if seq_literal is not None:
        const_elements = seq_literal.accept(_CONSTANT_TUPLE_ELEMENTS)
        if const_elements is None or len(const_elements) != n:
            return False
    else:
        build_node = seq_producer.accept(_BUILD_SEQUENCE_PRODUCER)
        if build_node is None or len(build_node.args) != n:
            return False

    dest = node.lvar.dest
    if dest is None or _following_node_in_block(node) is not dest:
        return False
    temp_store_info = dest.accept(_EXPRESSION_TEMP_STORE)
    if temp_store_info is None:
        return False
    store_temp, temporary = temp_store_info
    if store_temp.value is not node.lvar:
        return False

    current: Node = store_temp
    loads: list[Ref] = []
    ref_calls: list[Combination] = []
    for index in range(n):
        next_node = _following_node_in_block(current)
        if next_node is None:
            return False
        load_i = next_node.accept(_TempRefCheck(temporary, final=False))
        if load_i is None or load_i.lvar is None or len(load_i.lvar.uses) != 1:
            return False
        pos_i = _following_node_in_block(load_i)
        if pos_i is None:
            return False
        fun_i = _following_node_in_block(pos_i)
        if fun_i is None:
            return False
        call_node = _following_node_in_block(fun_i)
        if call_node is None:
            return False
        ref_call_i = call_node.accept(_UnpackRefCall(load_i.lvar, index))
        if (
            ref_call_i is None
            or ref_call_i.args[1].uses != [pos_i]
            or ref_call_i.fun.uses != [fun_i]
        ):
            return False
        store_node = _following_node_in_block(ref_call_i)
        if store_node is None:
            return False
        target_store_i = store_node.accept(_LAMBDA_VAR_TARGET_STORE)
        if (
            target_store_i is None
            or target_store_i.value is not ref_call_i.lvar
            or ref_call_i.lvar.dest is not target_store_i
            or len(ref_call_i.lvar.uses) != 1
        ):
            return False
        loads.append(load_i)
        ref_calls.append(ref_call_i)
        current = target_store_i

    after_stores = _following_node_in_block(current)
    if after_stores is None:
        return False
    discard_ref = after_stores.accept(_TempRefCheck(temporary, final=True))
    if discard_ref is None or discard_ref.lvar is not None:
        return False
    if temporary.refs != [*loads, discard_ref]:
        return False

    if build_node is not None:
        source_lvars = list(build_node.args)
        build_node.args = []
        for i in range(n):
            ref_call_i = ref_calls[i]
            elem_lvar = linked(ref_call_i.lvar, ref_call_i, "lvar")
            src_lvar = source_lvars[i]
            delete_lvar_use(ref_call_i)
            src_lvar.dest = None
            substitute_lvar_uses(elem_lvar, src_lvar)
            _flush_operand(ref_call_i.fun)
            _delete_pure_ref(ref_call_i.args[0])
            _delete_pure_ref(ref_call_i.args[1])
            ref_call_i.args = []
            unlink_node(ref_call_i)
        _flush_operand(build_node.fun)
        delete_lvar_use(build_node)
        unlink_node(build_node)
    else:
        assert const_elements is not None
        for i in range(n):
            ref_call_i = ref_calls[i]
            load_i = loads[i]
            elem_lit = const_elements[i]
            if ref_call_i.source is not None:
                elem_lit = replace(elem_lit, span=ref_call_i.source.span)
            replace_combination_with_constant(ref_call_i, elem_lit)
            delete_ref(load_i)
            unlink_node(load_i)
        _delete_pure_ref(seq_lvar)

    delete_ref(discard_ref)
    unlink_node(discard_ref)

    node.lvar.dest = None
    store_temp.value = None
    delete_set(store_temp)
    unlink_node(store_temp)
    home_locals = node.home_lambda.locals
    if not temporary.refs and not temporary.sets and temporary in home_locals:
        home_locals.remove(temporary)

    seq_lvar.dest = None
    _flush_operand(node.fun)
    _flush_operand(count_lvar)
    node.args = []
    unlink_node(node)
    return True


@deftransform("%getitem")
def _transform_tuple_getitem(node: Combination) -> bool:
    if node.lvar is None or len(node.args) != 2:
        return False
    seq_lvar, index_lvar = node.args
    index_literal = constant_literal(index_lvar)
    if index_literal is None or index_lvar.uses[0].block is not node.block:
        return False
    raw_index = index_literal.accept(_CONSTANT_INDEX)
    if raw_index is None:
        return False

    seq_literal = constant_literal(seq_lvar)
    if seq_literal is not None and seq_lvar.uses[0].block is node.block:
        span = node.source.span if node.source is not None else None
        folded = seq_literal.accept(_ConstantIndexable(raw_index, span))
        if folded is None:
            return False
        replace_combination_with_constant(node, folded)
        return True

    if len(seq_lvar.uses) != 1:
        return False
    tuple_node = seq_lvar.uses[0].accept(_BUILD_TUPLE_PRODUCER)
    if (
        tuple_node is None
        or tuple_node.prev is None
        or tuple_node.block is not node.block
    ):
        return False
    n = len(tuple_node.args)
    idx = raw_index + n if raw_index < 0 else raw_index
    if not (0 <= idx < n):
        return False
    for i, elem in enumerate(tuple_node.args):
        if i != idx and not _is_safe_dropped_element(elem):
            return False

    selected = tuple_node.args[idx]
    for i, elem in enumerate(tuple_node.args):
        if i != idx:
            _delete_pure_ref(elem)
    tuple_node.args = []
    _flush_operand(tuple_node.fun)
    unlink_node(tuple_node)
    seq_lvar.dest = None
    node.args[0] = selected
    selected.dest = node
    replace_combination_with_lvar(node, selected)
    return True


@deftransform("%getslice")
def _transform_constant_getslice(node: Combination) -> bool:
    if node.lvar is None or len(node.args) != 4:
        return False
    for arg in node.args:
        if len(arg.uses) != 1 or arg.uses[0].block is not node.block:
            return False
    seq_lvar, lower_lvar, upper_lvar, step_lvar = node.args
    seq_literal = constant_literal(seq_lvar)
    lower_literal = constant_literal(lower_lvar)
    upper_literal = constant_literal(upper_lvar)
    step_literal = constant_literal(step_lvar)
    if (
        seq_literal is None
        or lower_literal is None
        or upper_literal is None
        or step_literal is None
    ):
        return False
    lower_ok, lower = lower_literal.accept(_SLICE_BOUND)
    upper_ok, upper = upper_literal.accept(_SLICE_BOUND)
    step_ok, step = step_literal.accept(_SLICE_BOUND)
    if not (lower_ok and upper_ok and step_ok) or step == 0:
        return False
    span = node.source.span if node.source is not None else None
    folded = seq_literal.accept(_ConstantSliceable(slice(lower, upper, step), span))
    if folded is None:
        return False
    replace_combination_with_constant(node, folded)
    return True


@deftransform("%unary_not")
def _transform_empty_container_not(node: Combination) -> bool:
    if node.lvar is None or len(node.args) != 1 or len(node.fun.uses) != 1:
        return False
    arg = node.args[0]
    arg_literal = constant_literal(arg)
    if arg_literal is not None:
        elements = arg_literal.accept(_CONSTANT_TUPLE_ELEMENTS)
        if elements is not None and len(elements) == 0:
            folded = ast.BooleanLiteral(True)
            folded.span = node.source.span if node.source is not None else None
            replace_combination_with_constant(node, folded)
            return True
        return False

    if len(arg.uses) != 1:
        return False
    build_node = arg.uses[0].accept(_EMPTY_CONTAINER_PRODUCER)
    fun_ref = node.fun.uses[0]
    if (
        build_node is None
        or build_node.prev is None
        or fun_ref.prev is None
        or fun_ref.next is not node.prev
        or build_node.next is not fun_ref.prev
    ):
        return False
    folded = ast.BooleanLiteral(True)
    folded.span = node.source.span if node.source is not None else None
    replace_combination_with_constant(node, folded)
    _flush_operand(build_node.fun)
    unlink_node(build_node)
    return True


_PROTOCOL_PREFIXES = (
    "mule_sequences_",
    "mule_format_",
    "mule_arguments_",
    "mule_native_class_",
    "mule_print_",
)


class _KnownLengthPrimitive(ValueTypeVisitor):
    """Select the direct length primitive for a proven container or text type."""

    def visit_ValueType(self, fact: ValueType) -> FunInfo | None:
        return None

    def visit_TextType(self, fact: TextType) -> FunInfo | None:
        if fact.may_be_unbound:
            return None
        return KNOWN["_text_length"]

    def visit_BytesType(self, fact: BytesType) -> FunInfo | None:
        if fact.may_be_unbound:
            return None
        return KNOWN["_storage_length"]

    def visit_ListType(self, fact: ListType) -> FunInfo | None:
        if fact.may_be_unbound:
            return None
        return KNOWN["_storage_length"]

    def visit_TupleType(self, fact: TupleType) -> FunInfo | None:
        if fact.may_be_unbound:
            return None
        return KNOWN["_storage_length"]


_KNOWN_LENGTH_PRIMITIVE = _KnownLengthPrimitive()


class _ConcatLengthKind(ValueTypeVisitor):
    """Select the length primitive for a bound text or bytes concatenation."""

    def visit_ValueType(self, fact: ValueType) -> FunInfo | None:
        return None

    def visit_TextType(self, fact: TextType) -> FunInfo | None:
        if fact.may_be_unbound:
            return None
        return KNOWN["_text_length"]

    def visit_BytesType(self, fact: BytesType) -> FunInfo | None:
        if fact.may_be_unbound:
            return None
        return KNOWN["_storage_length"]


_CONCAT_LENGTH_KIND = _ConcatLengthKind()


class _ConstantConcatLength(ast.Visitor):
    """Extract the length of a constant text or bytes literal, or None."""

    def visit_Node(self, node) -> int | None:
        return None

    def visit_TextLiteral(self, node: ast.TextLiteral) -> int:
        return len(node.value)

    def visit_BytesLiteral(self, node: ast.BytesLiteral) -> int:
        return len(node.value)


_CONSTANT_CONCAT_LENGTH = _ConstantConcatLength()


class _RetargetConstantRef(NodeVisitor):
    """Replace a constant ``Ref``'s literal in place."""

    def __init__(self, literal: ast.IntegerLiteral, vtype: ValueType):
        self.literal = literal
        self.vtype = vtype

    def visit_Ref(self, node: Ref) -> None:
        if node in node.leaf.refs:
            node.leaf.refs.remove(node)
        leaf = Constant(self.literal)
        node.leaf = leaf
        leaf.refs.append(node)
        node.derived_type = self.vtype


class _ConcatAddProducer(NodeVisitor):
    """Return ``node`` when it is a known ``%add`` concatenation for ``primitive``."""

    def __init__(self, primitive: FunInfo, block):
        self.primitive = primitive
        self.block = block

    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.kind is not CallKind.KNOWN
            or node.fun_info is not fndb.BINARY[ast.BinaryOperator.ADD]
            or node.lvar is None
            or len(node.args) != 2
            or node.args[0] is node.args[1]
            or len(node.fun.uses) != 1
            or node.prev is None
            or node.block is not self.block
            or node.lexenv.policy.safety >= 3
        ):
            return None
        left, right = node.args
        if (
            lvar_type(left).accept(_CONCAT_LENGTH_KIND) is not self.primitive
            or lvar_type(right).accept(_CONCAT_LENGTH_KIND) is not self.primitive
        ):
            return None
        if any(use.prev is None for use in (*left.uses, *right.uses)):
            return None
        return node


class _LocalConcatVarRef(NodeVisitor, LeafVisitor):
    """Match a non-final ``Ref`` to an unannotated local ``LambdaVar``."""

    def __init__(self, home_lambda: Lambda | None):
        self.home_lambda = home_lambda
        self._ref: Ref | None = None

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        if node.final:
            return None
        self._ref = node
        var = node.leaf.accept(self)
        if var is None:
            return None
        return node, var

    def visit_Leaf(self, leaf):
        return None

    def visit_LambdaVar(self, leaf: LambdaVar):
        if (
            leaf.indirect
            or leaf.home is not self.home_lambda
            or leaf.home is None
            or leaf in leaf.home.vars
            or leaf.name in leaf.home.local_annotations
            or leaf.type is not None
        ):
            return None
        return leaf


def _match_concat_local_store(
    arg_lvar: Lvar,
    call: Combination,
    positions: dict[Node, int],
    block_nodes: list[Node],
) -> tuple[LambdaVar, Set, Ref, bool] | None:
    if len(arg_lvar.uses) != 1:
        return None
    local_info = arg_lvar.uses[0].accept(_LocalConcatVarRef(call.home_lambda))
    if local_info is None:
        return None
    arg_use, var = local_info
    if arg_use not in positions:
        return None
    if not var.sets or not var.refs:
        return None
    for store in var.sets:
        if store.value is None or store.lvar is not None or store.prev is None:
            return None
    for ref in var.refs:
        if ref.prev is None or ref.final:
            return None

    var_sets = set(var.sets)
    var_refs = set(var.refs)
    blocks = {s.block for s in var.sets} | {r.block for r in var.refs}
    matched_store: Set | None = None
    for blk in blocks:
        active_store: Set | None = None
        active_ref: Ref | None = None
        for node in blk.nodes():
            if node in var_sets:
                active_store = node
                active_ref = None
            elif node in var_refs:
                if active_store is None or active_ref is not None:
                    return None
                active_ref = node
                if node is arg_use:
                    matched_store = active_store

    if matched_store is None or matched_store not in positions:
        return None
    store_pos = positions[matched_store]
    ref_pos = positions[arg_use]
    call_pos = positions[call]
    if not (store_pos < ref_pos < call_pos):
        return None

    fun_use = call.fun.uses[0]
    intervening = block_nodes[store_pos + 1 : call_pos]
    only_safe_refs = all(
        node is arg_use
        or node is fun_use
        or (node.value_lvar is not None and _is_safe_dropped_element(node.value_lvar))
        for node in intervening
    )
    if only_safe_refs:
        return var, matched_store, arg_use, True
    if len(var.sets) == 1 and len(var.refs) == 1:
        return var, matched_store, arg_use, False
    return None


def _wrap_lvar_in_length_primitive(
    add_node: Combination,
    operand_lvar: Lvar,
    primitive: FunInfo,
    positions: dict[Node, int],
) -> None:
    orig_type = lvar_type(operand_lvar)
    value = Lvar()
    for use in list(operand_lvar.uses):
        operand_lvar.uses.remove(use)
        use.lvar = None
        add_lvar_use(use, value)
    value.derived_type = orig_type

    fun_leaf = KnownFunction(primitive)
    fun_ref = Ref(add_node.lexenv, add_node.source, fun_leaf)
    fun_leaf.refs.append(fun_ref)
    fun_lvar = Lvar()
    use_lvar(fun_ref, fun_lvar)

    len_call = Combination(add_node.lexenv, add_node.source, fun_lvar)
    len_call.kind = CallKind.KNOWN
    len_call.fun_info = primitive
    fun_lvar.dest = len_call
    len_call.args = [value]
    value.dest = len_call

    if (
        len(value.uses) == 1
        and value.uses[0] in positions
        and positions[value.uses[0]] < positions[add_node]
    ):
        producer = value.uses[0]
        insert_node_after(producer, fun_ref)
        insert_node_after(fun_ref, len_call)
    else:
        insert_node_before(add_node, fun_ref)
        insert_node_before(add_node, len_call)

    use_lvar(len_call, operand_lvar)
    len_type = (
        primitive.derive_type(len_call)
        if primitive.derive_type is not None
        else UNKNOWN
    )
    len_call.derived_type = len_type
    operand_lvar.derived_type = len_type


def _fuse_concat_length(call: Combination, primitive: FunInfo) -> bool:
    """Rewrite ``len(a + b)`` on proven ``str`` or ``bytes`` into
    ``_text_length(a) + _text_length(b)`` (or ``_storage_length``)."""
    if (
        call.lvar is None
        or len(call.args) != 1
        or len(call.fun.uses) != 1
        or call.prev is None
        or call.lexenv.policy.safety >= 3
    ):
        return False
    home = call.home_lambda
    if home is not None and (
        home.policy.safety >= 3 or home.name.startswith(_PROTOCOL_PREFIXES)
    ):
        return False
    arg_lvar = call.args[0]
    if lvar_type(arg_lvar).accept(_CONCAT_LENGTH_KIND) is not primitive:
        return False

    block = call.block
    block_nodes = list(block.nodes())
    positions = {node: idx for idx, node in enumerate(block_nodes)}
    adder = _ConcatAddProducer(primitive, block)

    local_match: tuple[LambdaVar, Set, Ref, bool] | None = None
    root_add: Combination | None = None
    root_lvar: Lvar | None = None

    if len(arg_lvar.uses) == 1:
        direct_add = arg_lvar.uses[0].accept(adder)
        if direct_add is not None:
            root_add = direct_add
            root_lvar = arg_lvar

    if root_add is None:
        local_match = _match_concat_local_store(arg_lvar, call, positions, block_nodes)
        if local_match is None:
            return False
        _, matched_store, _, _ = local_match
        store_val = linked(matched_store.value, matched_store, "value")
        if len(store_val.uses) != 1:
            return False
        cand_add = store_val.uses[0].accept(adder)
        if cand_add is None:
            return False
        root_add = cand_add
        root_lvar = store_val

    assert root_add is not None and root_lvar is not None
    add_nodes_postorder: list[Combination] = []
    leaf_operands: list[tuple[Combination, Lvar]] = []
    seen_adds: set[Combination] = set()

    def collect(add_node: Combination, limit_pos: int) -> bool:
        if add_node in seen_adds:
            return False
        node_pos = positions.get(add_node)
        if node_pos is None or node_pos >= limit_pos:
            return False
        seen_adds.add(add_node)
        for operand_lvar in add_node.args:
            if operand_lvar.dest is not add_node or not operand_lvar.uses:
                return False
            if len(operand_lvar.uses) == 1:
                child_add = operand_lvar.uses[0].accept(adder)
                if child_add is not None:
                    if not collect(child_add, node_pos):
                        return False
                    continue
                use_pos = positions.get(operand_lvar.uses[0])
                if use_pos is not None and use_pos >= node_pos:
                    return False
            leaf_operands.append((add_node, operand_lvar))
        add_nodes_postorder.append(add_node)
        return True

    bound_pos = (
        positions[local_match[1]] if local_match is not None else positions[call]
    )
    if not collect(root_add, bound_pos):
        return False

    for add_node, operand_lvar in leaf_operands:
        if len(operand_lvar.uses) == 1 and operand_lvar.uses[0] in positions:
            literal = constant_literal(operand_lvar)
            if literal is not None:
                const_len = literal.accept(_CONSTANT_CONCAT_LENGTH)
                if const_len is not None:
                    int_lit = ast.IntegerLiteral(const_len, span=literal.span)
                    int_type = IntegerType(const_len, const_len)
                    operand_lvar.uses[0].accept(_RetargetConstantRef(int_lit, int_type))
                    operand_lvar.derived_type = int_type
                    reoptimize_lvar(operand_lvar)
                    continue
        _wrap_lvar_in_length_primitive(add_node, operand_lvar, primitive, positions)

    for add_node in add_nodes_postorder:
        left_type = lvar_type(add_node.args[0])
        right_type = lvar_type(add_node.args[1])
        new_type = left_type.binary(ast.BinaryOperator.ADD, right_type)
        add_node.derived_type = new_type
        add_node.reoptimize = True
        if add_node.lvar is not None:
            add_node.lvar.derived_type = new_type
            reoptimize_lvar(add_node.lvar)

    if local_match is not None:
        var, matched_store, arg_use, eliminate_store = local_match
        if eliminate_store:
            matched_store.value = None
            root_lvar.dest = None
            delete_set(matched_store)
            unlink_node(matched_store)
            delete_lvar_use(arg_use)
            delete_ref(arg_use)
            unlink_node(arg_use)
            substitute_lvar_uses(arg_lvar, root_lvar)
            home_locals = call.home_lambda.locals
            if not var.refs and not var.sets and var in home_locals:
                home_locals.remove(var)
        else:
            new_root_type = lvar_type(root_lvar)
            matched_store.derived_type = new_root_type
            arg_use.derived_type = new_root_type
            arg_lvar.derived_type = new_root_type

    replace_combination_with_lvar(call, arg_lvar)
    return True


_LENGTH_PRIMITIVE_TRANSFORMS_REGISTERED = False


def _transform_known_length_primitive(node: Combination) -> bool:
    if node.kind is not CallKind.KNOWN or node.fun_info is None:
        return False
    return _fuse_concat_length(node, node.fun_info)


def _ensure_length_primitive_transforms() -> None:
    global _LENGTH_PRIMITIVE_TRANSFORMS_REGISTERED
    if _LENGTH_PRIMITIVE_TRANSFORMS_REGISTERED:
        return
    if "_text_length" not in KNOWN or "_storage_length" not in KNOWN:
        return
    deftransform("_text_length", fun=_transform_known_length_primitive)
    deftransform("_storage_length", fun=_transform_known_length_primitive)
    _LENGTH_PRIMITIVE_TRANSFORMS_REGISTERED = True


def known_length_primitive(vtype: ValueType) -> FunInfo | None:
    """Return the direct length primitive for ``vtype``, or None."""
    _ensure_length_primitive_transforms()
    return vtype.accept(_KNOWN_LENGTH_PRIMITIVE)


class _PredictableLenGlobal(NodeVisitor, LeafVisitor):
    """Match a ``Ref`` to an unmutated ``GlobalVar('len')`` outside protocols."""

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        if node.lexenv.policy.safety >= 3:
            return None
        home = node.home_lambda
        if home is not None and (
            home.policy.safety >= 3 or home.name.startswith(_PROTOCOL_PREFIXES)
        ):
            return None
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return None

    def visit_GlobalVar(self, leaf: GlobalVar):
        if leaf.name == "len" and not leaf.sets:
            return leaf
        return None


_PREDICTABLE_LEN_GLOBAL = _PredictableLenGlobal()


def is_predictable_len_call(node: Combination) -> bool:
    """Whether ``node`` is a 1-argument positional call to builtin ``len``."""
    _ensure_length_primitive_transforms()
    if (
        node.kind not in (CallKind.FULL, CallKind.GUARDED)
        or len(node.args) != 1
        or len(node.fun.uses) != 1
        or (node.info is not None and node.info.keywords)
        or node.lexenv.policy.safety >= 3
    ):
        return False
    home = node.home_lambda
    if home is not None and (
        home.policy.safety >= 3 or home.name.startswith(_PROTOCOL_PREFIXES)
    ):
        return False
    return node.fun.uses[0].accept(_PREDICTABLE_LEN_GLOBAL) is not None


@deftransform("len")
def _transform_len(node: Combination) -> bool:
    if not is_predictable_len_call(node):
        return False
    primitive = known_length_primitive(lvar_type(node.args[0]))
    if primitive is None:
        return False
    retarget_known_combination(node, primitive, [node.args[0]])
    _fuse_concat_length(node, primitive)
    return True


class _AttributeName(ast.Visitor):
    """The text of a constant attribute name, or None."""

    def visit_Node(self, node):
        return None

    def visit_TextLiteral(self, node: ast.TextLiteral) -> str | None:
        return node.value if node.value.isidentifier() else None


_ATTRIBUTE_NAME = _AttributeName()


def _constant_attribute_name(lvar: Lvar) -> str | None:
    literal = constant_literal(lvar)
    return None if literal is None else literal.accept(_ATTRIBUTE_NAME)


def _retarget_attribute_builtin(
    node: Combination, info: FunInfo, name: str, args: list[Lvar]
) -> None:
    from .ir1tran import AttributeInfo

    location = node.info.location if node.info is not None else None
    retarget_known_combination(node, info, args)
    node.info = AttributeInfo(name, location)


@deftransform("getattr")
def _transform_getattr(node: Combination) -> bool:
    """``getattr(x, "name", default)`` with a constant name becomes a lookup
    with its own call-site cache (SBCL's ``slot-value`` with a constant slot
    name compiles to the slot accessor). The builtin name check is the
    predictable-global test ``_OptimizeNode`` already made."""
    if len(node.args) != 3:
        return False
    name = _constant_attribute_name(node.args[1])
    if name is None:
        return False
    _retarget_attribute_builtin(
        node, fndb.GETATTR_DEFAULT, name, [node.args[0], node.args[2]]
    )
    return True


@deftransform("hasattr")
def _transform_hasattr(node: Combination) -> bool:
    if len(node.args) != 2:
        return False
    name = _constant_attribute_name(node.args[1])
    if name is None:
        return False
    _retarget_attribute_builtin(node, fndb.HASATTR, name, [node.args[0]])
    return True


class _IfTestingLvar(NodeVisitor):
    """Return ``node`` when it is an ``If`` testing ``test``."""

    def __init__(self, test: Lvar):
        self.test = test

    def visit_Node(self, node):
        return None

    def visit_If(self, node: If) -> If | None:
        if node.test is self.test:
            return node
        return None


class _UnmutatedRangeGlobalRef(NodeVisitor, LeafVisitor):
    """Match a ``Ref`` to an unmutated ``GlobalVar('range')``."""

    def visit_Node(self, node) -> bool:
        return False

    def visit_Ref(self, node: Ref) -> bool:
        if node.lexenv.policy.safety >= 3:
            return False
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf) -> bool:
        return False

    def visit_GlobalVar(self, leaf: GlobalVar) -> bool:
        return leaf.name == "range" and not leaf.sets


_UNMUTATED_RANGE_GLOBAL_REF = _UnmutatedRangeGlobalRef()


class _PredictableRangeCalleeRef(NodeVisitor, LeafVisitor):
    """Match a ``Ref`` to a single-assignment ``LambdaVar`` bound to builtin ``range``."""

    def visit_Node(self, node) -> bool:
        return False

    def visit_Ref(self, node: Ref) -> bool:
        if node.lexenv.policy.safety >= 3:
            return False
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf) -> bool:
        return False

    def visit_LambdaVar(self, leaf: LambdaVar) -> bool:
        if len(leaf.sets) != 1:
            return False
        store = leaf.sets[0]
        if store.value is None or len(store.value.uses) != 1:
            return False
        return store.value.uses[0].accept(_UNMUTATED_RANGE_GLOBAL_REF)


_PREDICTABLE_RANGE_CALLEE_REF = _PredictableRangeCalleeRef()


@deftransform("%range_prepare")
def _transform_range_prepare(node: Combination) -> bool:
    if (
        node.lvar is None
        or len(node.lvar.uses) != 1
        or len(node.args) != 2
        or node.lexenv.policy.safety >= 3
    ):
        return False
    home = node.home_lambda
    if home is not None and (
        home.policy.safety >= 3 or home.name.startswith(_PROTOCOL_PREFIXES)
    ):
        return False
    dest = node.lvar.dest
    if dest is None:
        return False
    if_node = dest.accept(_IfTestingLvar(node.lvar))
    if if_node is None or if_node.prev is None:
        return False
    callee_lvar = node.args[0]
    if len(callee_lvar.uses) != 1 or not callee_lvar.uses[0].accept(
        _PREDICTABLE_RANGE_CALLEE_REF
    ):
        return False
    state = node.info
    if state is None or not (1 <= len(state.arguments) <= 3):
        return False
    for arg in state.arguments:
        arg_type = lvar_type(arg)
        if not (
            arg_type.fits(PYTHON_INTEGER)
            and arg_type.is_fixnum
            and not arg_type.may_be_unbound
        ):
            return False

    span = node.source.span if node.source is not None else None
    leaf = Constant(ast.BooleanLiteral(True, span=span))
    true_ref = Ref(node.lexenv, node.source, leaf)
    true_ref.derived_type = BOOLEAN_TRUE
    leaf.refs.append(true_ref)
    insert_node_before(if_node, true_ref)
    test_lvar = node.lvar
    delete_lvar_use(node)
    use_lvar(true_ref, test_lvar)
    reoptimize_lvar(test_lvar)
    return True


__all__ = [
    "_transform_constant_getslice",
    "_transform_empty_container_not",
    "_transform_len",
    "_transform_range_prepare",
    "_transform_tuple_getitem",
    "_transform_unpack",
    "is_predictable_len_call",
    "known_length_primitive",
]
