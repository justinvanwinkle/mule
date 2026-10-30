"""Sequence IR1 transforms, following ``src/compiler/seqtran.lisp``.

Transforms registered via ``@deftransform`` fold constant-index tuple
subscripting when dropped elements are effect-free and non-raising, and
simplify truthiness negation of newly constructed empty tuples and lists.
"""

from __future__ import annotations

from dataclasses import dataclass, replace
from threading import Lock

from mule import ast

from . import fndb
from .ctype import (
    BOOLEAN_TRUE,
    BYTEARRAY_TYPE,
    BYTES_TYPE,
    FIXNUM_MIN,
    INTEGER,
    LIST_TYPE,
    NONE_TYPE,
    PYTHON_INTEGER,
    TEXT_TYPE,
    UNKNOWN,
    ByteArrayType,
    BytesType,
    DictType,
    IntegerType,
    ListType,
    SetType,
    TextType,
    TupleType,
    ValueType,
    ValueTypeVisitor,
)
from .ir1opt import (
    _flush_dead_operands,
    _flush_operand,
    _is_effectless_known_call,
    constant_literal,
    constant_lvar_value,
    lvar_matches,
    lvar_type,
    replace_combination_with_constant,
    splice_fun_args,
)
from .ir1tran import find_constant_leaf
from .ir1util import (
    delete_lvar_use,
    delete_ref,
    delete_set,
    insert_node_before,
    is_flushable_ref,
    is_pure_bound_ref,
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
    Cast,
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
from .srctran import _is_slice_bound_lvar, _is_unit_slice_step


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


@dataclass(eq=False)
class ByteIteration:
    """IR1 cursor shared by setup, availability, and element production.

    LTN selects byte or generic operations independently for each entry context.
    The hidden iterator local retains the sequence in a typed byte entry and
    retains an ordinary Python iterator in an unknown entry.
    """

    source: Lvar

    def clone(self, map_lvar):
        return ByteIteration(map_lvar(self.source))


class _IteratorStore(NodeVisitor):
    def visit_Node(self, node):
        return None

    def visit_Set(self, node):
        return node.var.accept(_HiddenIteratorVar(node))


class _HiddenIteratorVar(LeafVisitor):
    def __init__(self, store):
        self.store = store

    def visit_Leaf(self, leaf):
        return None

    def visit_LambdaVar(self, leaf):
        if leaf.hidden and leaf.sets == [self.store] and not leaf.indirect:
            return leaf
        return None


@deftransform("%iterate")
def _transform_byte_iteration(node):
    if node.lvar is None or len(node.args) != 1 or len(node.args[0].uses) != 1:
        return False
    if (
        node.lexenv.policy.safety < 3
        and node.args[0].uses[0].accept(_ENUMERATE_CALL_CHECK) is not None
    ):
        return False
    home = node.home_lambda
    if home is None or home.coroutine_body or home.name.startswith(_PROTOCOL_PREFIXES):
        return False
    store = node.lvar.dest
    iterator = None if store is None else store.accept(_IteratorStore())
    if iterator is None:
        return False
    available = []
    for ref in iterator.refs:
        if ref.prev is None or ref.lvar is None:
            continue
        dest = ref.lvar.dest
        if dest is None or not dest.accept(_SequenceAvailableOnLvar(ref.lvar)):
            return False
        available.append(dest)
    if len(available) != 1:
        return False
    steps = [
        step
        for block in home.component.blocks()
        for step in block.nodes()
        if step.accept(_ByteNextItem(iterator))
    ]
    if len(steps) != 1:
        return False
    from .byteops import AVAILABLE, ITERATE, NEXT

    state = ByteIteration(node.args[0])
    retarget_known_combination(node, ITERATE, list(node.args))
    node.info = state
    avail = available[0]
    retarget_known_combination(avail, AVAILABLE, list(avail.args))
    avail.info = state
    step = steps[0]
    ref = Ref(step.lexenv, step.source, iterator)
    iterator.refs.append(ref)
    owner = Lvar()
    use_lvar(ref, owner)
    insert_node_before(step, ref)
    retarget_known_combination(step, NEXT, [owner])
    step.info = state
    return True


class _ByteNextItem(NodeVisitor):
    def __init__(self, iterator):
        self.iterator = iterator

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node):
        return (
            node.fun_info is fndb.PROTOCOLS["next_item"] and node.info is self.iterator
        )


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


@deftransform("%multiply")
def _transform_singleton_repeat(node: Combination) -> bool:
    """Replace fresh singleton repetition by allocation and compiled vector fill."""
    if node.lvar is None or len(node.args) != 2:
        return False
    for sequence_index in (0, 1):
        sequence = node.args[sequence_index]
        count = node.args[1 - sequence_index]
        if not lvar_type(count).fits_integer(INTEGER) or len(sequence.uses) != 1:
            continue
        build = sequence.uses[0].accept(_BUILD_SEQUENCE_PRODUCER)
        if (
            build is None
            or build.fun_info is not fndb.BUILD_LIST
            or len(build.args) != 1
            or build.block is not node.block
        ):
            continue
        item = build.args[0]
        # Keep operand producers in place: count evaluation can run user code.
        # Transfer the element's lvar to the fill call instead of flushing it
        # with the now-unobservable singleton list.
        build.args = []
        sequence.dest = None
        _flush_operand(build.fun)
        unlink_node(build)
        retarget_known_combination(node, fndb.PROTOCOLS["_list_fill"], [item, count])
        return True
    return False


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


_is_safe_dropped_element = is_pure_bound_ref
_delete_pure_ref = _flush_operand


class _UnmutatedEnumerateGlobalRef(NodeVisitor, LeafVisitor):
    """Match a ``Ref`` to an unmutated ``GlobalVar('enumerate')``."""

    def visit_Node(self, node) -> bool:
        return False

    def visit_Ref(self, node: Ref) -> bool:
        if node.lexenv.policy.safety >= 3:
            return False
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf) -> bool:
        return False

    def visit_GlobalVar(self, leaf: GlobalVar) -> bool:
        return leaf.name == "enumerate" and not leaf.sets


_UNMUTATED_ENUMERATE_GLOBAL_REF = _UnmutatedEnumerateGlobalRef()


class _EnumerateCallCheck(NodeVisitor):
    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.prev is not None
            and node.kind in (CallKind.FULL, CallKind.GUARDED)
            and 1 <= len(node.args) <= 2
            and not (node.info is not None and node.info.keywords)
            and len(node.fun.uses) == 1
            and node.fun.uses[0].accept(_UNMUTATED_ENUMERATE_GLOBAL_REF)
        ):
            return node
        return None


_ENUMERATE_CALL_CHECK = _EnumerateCallCheck()


class _EnumerateIterateCheck(NodeVisitor):
    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.prev is not None
            and node.kind is CallKind.KNOWN
            and node.fun_info is fndb.PROTOCOLS["iterate"]
            and len(node.args) == 1
            and len(node.args[0].uses) == 1
        ):
            enum_call = node.args[0].uses[0].accept(_ENUMERATE_CALL_CHECK)
            if enum_call is not None and enum_call.block is node.block:
                return node, enum_call
        return None


_ENUMERATE_ITERATE_CHECK = _EnumerateIterateCheck()


class _SequenceAvailableOnLvar(NodeVisitor):
    def __init__(self, target_lvar: Lvar):
        self.target_lvar = target_lvar

    def visit_Node(self, node) -> bool:
        return False

    def visit_Combination(self, node: Combination) -> bool:
        return (
            node.kind is CallKind.KNOWN
            and node.fun_info
            in (
                fndb.SEQUENCE_AVAILABLE,
                fndb.PROTOCOLS.get("sequence_available"),
            )
            and len(node.args) >= 1
            and node.args[0] is self.target_lvar
        )


class _EnumerateIteratorRef(NodeVisitor, LeafVisitor):
    def __init__(self):
        self._ref: Ref | None = None

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        self._ref = node
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return None

    def visit_LambdaVar(self, leaf: LambdaVar):
        if len(leaf.sets) != 1:
            return None
        store = leaf.sets[0]
        if store.prev is None or store.value is None or len(store.value.uses) != 1:
            return None
        matched = store.value.uses[0].accept(_ENUMERATE_ITERATE_CHECK)
        if matched is None:
            return None
        iterate_call, enum_call = matched
        if store.block is not iterate_call.block:
            return None
        return self._ref, leaf, store, iterate_call, enum_call


class _NextItemEnumerateProducer(NodeVisitor):
    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is fndb.PROTOCOLS["next_item"]
            and len(node.args) == 1
            and len(node.args[0].uses) == 1
        ):
            setup = None
            if node.info is not None and hasattr(node.info, "accept"):
                setup = node.info.accept(_EnumerateIteratorRef())
            if setup is None:
                setup = node.args[0].uses[0].accept(_EnumerateIteratorRef())
            if setup is not None:
                return (node, *setup)
        return None


_NEXT_ITEM_ENUMERATE_PRODUCER = _NextItemEnumerateProducer()


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
        or _following_node_in_block(count_ref) is not fun_ref
        or _following_node_in_block(fun_ref) is not node
    ):
        return False

    seq_literal = constant_literal(seq_lvar)
    const_elements = None
    build_node = None
    enum_setup = None
    if seq_literal is not None:
        const_elements = seq_literal.accept(_CONSTANT_TUPLE_ELEMENTS)
        if const_elements is None or len(const_elements) != n:
            return False
    else:
        build_node = seq_producer.accept(_BUILD_SEQUENCE_PRODUCER)
        if build_node is not None:
            if len(build_node.args) != n:
                return False
        elif n == 2 and node.lexenv.policy.safety < 3:
            home = node.home_lambda
            if home is not None and (
                home.policy.safety < 3 and not home.name.startswith(_PROTOCOL_PREFIXES)
            ):
                enum_setup = seq_producer.accept(_NEXT_ITEM_ENUMERATE_PRODUCER)
                if enum_setup is not None:
                    (
                        _next_call,
                        next_iter_ref,
                        iterator_var,
                        iter_set,
                        iterate_call,
                        enum_call,
                    ) = enum_setup
                    start_val = 0
                    if len(enum_call.args) == 2:
                        start_lit = constant_literal(enum_call.args[1])
                        s_val = (
                            None
                            if start_lit is None
                            else start_lit.accept(_EXACT_INTEGER_LITERAL)
                        )
                        if (
                            s_val is None
                            or not is_pure_bound_ref(enum_call.args[1])
                            or not (FIXNUM_MIN <= s_val < (1 << 60))
                        ):
                            enum_setup = None
                        else:
                            start_val = s_val
                    if enum_setup is not None:
                        valid_refs = True
                        saw_avail = False
                        for ref in iterator_var.refs:
                            if ref is next_iter_ref:
                                continue
                            if ref.lvar is None:
                                continue
                            if (
                                len(ref.lvar.uses) == 1
                                and ref.lvar.dest is not None
                                and ref.lvar.dest.accept(
                                    _SequenceAvailableOnLvar(ref.lvar)
                                )
                            ):
                                saw_avail = True
                                continue
                            valid_refs = False
                            break
                        if not valid_refs or not saw_avail:
                            enum_setup = None
            if enum_setup is None:
                return False
        else:
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

    ref_calls: list[Combination | None] = [None] * n
    discard_ref: Ref | None = None
    for ref in temporary.refs:
        if ref.prev is None:
            return False
        if ref.accept(_TempRefCheck(temporary, final=True)) is not None:
            if discard_ref is not None or ref.lvar is not None:
                return False
            discard_ref = ref
            continue
        loaded = ref.lvar
        if loaded is None or loaded.uses != [ref] or loaded.dest is None:
            return False
        call = loaded.dest
        # The temporary's value edges identify extraction independently of the
        # target. Receiver/index evaluation and nested assignments stay in place.
        for index in range(n):
            matched = call.accept(_UnpackRefCall(loaded, index))
            if matched is not None:
                if ref_calls[index] is not None:
                    return False
                ref_calls[index] = matched
                break
        else:
            return False
    if discard_ref is None or any(call is None for call in ref_calls):
        return False

    if enum_setup is not None:
        (
            _next_call,
            _next_iter_ref,
            iterator_var,
            iter_set,
            iterate_call,
            enum_call,
        ) = enum_setup
        start_val = 0
        if len(enum_call.args) == 2:
            start_lit = constant_literal(enum_call.args[1])
            assert start_lit is not None
            start_val = start_lit.accept(_EXACT_INTEGER_LITERAL)
            assert start_val is not None
            _delete_pure_ref(enum_call.args[1])
        seq_arg = enum_call.args[0]
        enum_call.args = []
        seq_arg.dest = iterate_call
        enum_res_lvar = iterate_call.args[0]
        enum_res_lvar.dest = None
        delete_lvar_use(enum_call)
        iterate_call.args[0] = seq_arg
        _flush_operand(enum_call.fun)
        unlink_node(enum_call)

        home = node.home_lambda
        idx_var = LambdaVar(f"$enum_idx_{iterator_var.name}")
        idx_var.home = home
        idx_var.type = IntegerType(start_val, (1 << 60) - 1)
        home.locals.append(idx_var)

        init_lvar = _emit_const_int_before(iter_set, start_val)
        init_set = Set(iter_set.lexenv, iter_set.source, idx_var, init_lvar)
        init_lvar.dest = init_set
        idx_var.sets.append(init_set)
        insert_node_before(iter_set, init_set)

        ref_call_0 = ref_calls[0]
        ref_call_1 = ref_calls[1]
        assert ref_call_0 is not None and ref_call_1 is not None

        idx_out_lvar = linked(ref_call_0.lvar, ref_call_0, "lvar")
        cur_idx_ref = Ref(ref_call_0.lexenv, ref_call_0.source, idx_var)
        cur_idx_ref.derived_type = idx_var.type
        idx_var.refs.append(cur_idx_ref)
        insert_node_before(ref_call_0, cur_idx_ref)
        delete_lvar_use(ref_call_0)
        use_lvar(cur_idx_ref, idx_out_lvar)
        reoptimize_lvar(idx_out_lvar)

        inc_read_ref = Ref(ref_call_0.lexenv, ref_call_0.source, idx_var)
        inc_read_ref.derived_type = idx_var.type
        idx_var.refs.append(inc_read_ref)
        inc_read_lvar = Lvar()
        use_lvar(inc_read_ref, inc_read_lvar)
        inc_read_lvar.derived_type = idx_var.type
        insert_node_before(ref_call_0, inc_read_ref)

        one_lvar = _emit_const_int_before(ref_call_0, 1)
        add_info = fndb.BINARY[ast.BinaryOperator.ADD]
        add_res_lvar = _emit_known_call_before(
            ref_call_0, add_info, [inc_read_lvar, one_lvar]
        )
        inc_set = Set(ref_call_0.lexenv, ref_call_0.source, idx_var, add_res_lvar)
        add_res_lvar.dest = inc_set
        idx_var.sets.append(inc_set)
        insert_node_before(ref_call_0, inc_set)

        _flush_operand(ref_call_0.fun)
        _delete_pure_ref(ref_call_0.args[0])
        _delete_pure_ref(ref_call_0.args[1])
        ref_call_0.args = []
        unlink_node(ref_call_0)

        val_out_lvar = linked(ref_call_1.lvar, ref_call_1, "lvar")
        delete_lvar_use(ref_call_1)
        seq_lvar.dest = None
        substitute_lvar_uses(val_out_lvar, seq_lvar)
        _flush_operand(ref_call_1.fun)
        _delete_pure_ref(ref_call_1.args[0])
        _delete_pure_ref(ref_call_1.args[1])
        ref_call_1.args = []
        unlink_node(ref_call_1)
    elif build_node is not None:
        source_lvars = list(build_node.args)
        build_node.args = []
        for i in range(n):
            ref_call_i = ref_calls[i]
            assert ref_call_i is not None
            elem_lvar = linked(ref_call_i.lvar, ref_call_i, "lvar")
            src_lvar = source_lvars[i]
            # Keep the already evaluated element as an Lvar temporary. Its
            # ordinary IR2 lifetime protects it across earlier target effects
            # and releases it if a target raises before this element is stored.
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
            assert ref_call_i is not None
            elem_lit = const_elements[i]
            if ref_call_i.source is not None:
                elem_lit = replace(elem_lit, span=ref_call_i.source.span)
            replace_combination_with_constant(ref_call_i, elem_lit)
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
    if not is_pure_bound_ref(index_lvar) or index_lvar.uses[0].block is not node.block:
        return False
    index_literal = constant_lvar_value(index_lvar)
    if index_literal is None:
        return False
    raw_index = index_literal.accept(_CONSTANT_INDEX)
    if raw_index is None:
        return False

    seq_literal = (
        constant_lvar_value(seq_lvar)
        if is_pure_bound_ref(seq_lvar) and seq_lvar.uses[0].block is node.block
        else None
    )
    if seq_literal is not None:
        span = node.source.span if node.source is not None else None
        folded = seq_literal.accept(_ConstantIndexable(raw_index, span))
        if folded is None:
            return False
        replace_combination_with_constant(node, folded)
        return True

    tuple_node = splice_fun_args(seq_lvar, ("%build_tuple",), splice=False)
    if tuple_node is None:
        return False
    n = len(tuple_node.args)
    idx = raw_index + n if raw_index < 0 else raw_index
    if not (0 <= idx < n):
        return False
    for i, elem in enumerate(tuple_node.args):
        if i != idx and not _is_safe_dropped_element(elem):
            return False

    selected = tuple_node.args[idx]
    splice_fun_args(seq_lvar, ("%build_tuple",))
    replace_combination_with_lvar(node, selected)
    return True


@deftransform("%getslice")
def _transform_constant_getslice(node: Combination) -> bool:
    if node.lvar is None or len(node.args) != 4:
        return False
    for arg in node.args:
        if not is_pure_bound_ref(arg) or arg.uses[0].block is not node.block:
            return False
    seq_lvar, lower_lvar, upper_lvar, step_lvar = node.args
    seq_literal = constant_lvar_value(seq_lvar)
    lower_literal = constant_lvar_value(lower_lvar)
    upper_literal = constant_lvar_value(upper_lvar)
    step_literal = constant_lvar_value(step_lvar)
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
    if node.lvar is None or len(node.args) != 1:
        return False
    arg = node.args[0]
    arg_literal = constant_lvar_value(arg) if is_pure_bound_ref(arg) else None
    if arg_literal is not None:
        elements = arg_literal.accept(_CONSTANT_TUPLE_ELEMENTS)
        if elements is not None and len(elements) == 0:
            folded = ast.BooleanLiteral(True)
            folded.span = node.source.span if node.source is not None else None
            replace_combination_with_constant(node, folded)
            return True
        return False

    if splice_fun_args(arg, ("%build_tuple", "%build_list", "%empty_set"), 0) is None:
        return False
    folded = ast.BooleanLiteral(True)
    folded.span = node.source.span if node.source is not None else None
    replace_combination_with_constant(node, folded)
    return True


@deftransform("set", ())
def _transform_empty_set(node: Combination) -> bool:
    retarget_known_combination(node, fndb.PROTOCOLS["empty_set"], [])
    return True


@deftransform("%build_set")
def _transform_empty_build_set(node: Combination) -> bool:
    if len(node.args) != 1:
        return False
    items_lvar = node.args[0]
    if is_pure_bound_ref(items_lvar):
        literal = constant_lvar_value(items_lvar)
        if literal is not None:
            elements = literal.accept(_CONSTANT_TUPLE_ELEMENTS)
            if elements is not None and len(elements) == 0:
                retarget_known_combination(node, fndb.PROTOCOLS["empty_set"], [])
                return True
    if splice_fun_args(items_lvar, ("%build_tuple",), 0) is not None:
        retarget_known_combination(node, fndb.PROTOCOLS["empty_set"], [])
        return True
    return False


_SET_MUTATION_PROTOCOLS = {
    "add": "set_add",
    "discard": "set_discard",
    "remove": "set_remove",
}


class _ResolveByteAppendProducer(NodeVisitor):
    def __init__(self, block):
        self.block = block

    def visit_Node(self, node):
        return None

    def visit_Combination(self, node):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is fndb.RESOLVE_METHOD
            and node.info is not None
            and node.info.name == "append"
            and len(node.args) == 1
            and node.prev is not None
            and node.block is self.block
            and lvar_type(node.args[0]).fits_bytearray()
        ):
            return node
        return None


@deftransform("%call_resolved")
def _transform_bytearray_append_byte(node):
    if (
        len(node.args) != 2
        or (node.info is not None and node.info.keywords)
        or node.lexenv.policy.safety >= 3
    ):
        return False
    from .byteops import APPEND_BYTE

    resolved, value = node.args
    if len(resolved.uses) != 1:
        return False
    producer = resolved.uses[0].accept(_ResolveByteAppendProducer(node.block))
    if producer is None:
        return False
    receiver = producer.args[0]
    producer.args = []
    _flush_operand(producer.fun)
    unlink_node(producer)
    resolved.uses.clear()
    resolved.dest = None
    retarget_known_combination(node, APPEND_BYTE, [receiver, value])
    return True


class _ResolveSetMethodProducer(NodeVisitor):
    """Match a ``%resolve_method`` for ``add``, ``discard``, or ``remove`` on a mutable set."""

    def __init__(self, block):
        self.block = block

    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.kind is not CallKind.KNOWN
            or node.fun_info is not fndb.RESOLVE_METHOD
            or node.info is None
            or node.info.name not in _SET_MUTATION_PROTOCOLS
            or len(node.args) != 1
            or node.prev is None
            or node.block is not self.block
            or node.lexenv.policy.safety >= 3
        ):
            return None
        recv_lvar = node.args[0]
        if not lvar_type(recv_lvar).fits_mutable_set():
            return None
        return node, _SET_MUTATION_PROTOCOLS[node.info.name]


@deftransform("%call_resolved")
def _transform_call_resolved_set_method(node: Combination) -> bool:
    if (
        len(node.args) != 2
        or (node.info is not None and node.info.keywords)
        or node.lexenv.policy.safety >= 3
        or (
            node.home_lambda is not None
            and (
                node.home_lambda.policy.safety >= 3
                or node.home_lambda.name.startswith(_PROTOCOL_PREFIXES)
            )
        )
    ):
        return False
    resolved, arg = node.args
    if len(resolved.uses) != 1:
        return False
    matched = resolved.uses[0].accept(_ResolveSetMethodProducer(node.block))
    if matched is None:
        return False
    resolve_node, protocol_name = matched
    recv_lvar = resolve_node.args[0]
    resolve_node.args = []
    _flush_operand(resolve_node.fun)
    unlink_node(resolve_node)
    resolved.uses.clear()
    resolved.dest = None
    node.args = [recv_lvar, arg]
    recv_lvar.dest = node
    retarget_known_combination(node, fndb.PROTOCOLS[protocol_name], [recv_lvar, arg])
    return True


@deftransform("%bit_and")
def _transform_set_and(node: Combination) -> bool:
    if len(node.args) != 2:
        return False
    left, right = node.args
    if lvar_type(left).fits_mutable_set() and lvar_type(right).fits_set():
        retarget_known_combination(node, fndb.PROTOCOLS["set_and"], [left, right])
        return True
    return False


@deftransform("%bit_or")
def _transform_set_or(node: Combination) -> bool:
    if len(node.args) != 2:
        return False
    left, right = node.args
    if lvar_type(left).fits_mutable_set() and lvar_type(right).fits_set():
        retarget_known_combination(node, fndb.PROTOCOLS["set_or"], [left, right])
        return True
    return False


@deftransform("%subtract")
def _transform_set_sub(node: Combination) -> bool:
    if len(node.args) != 2:
        return False
    left, right = node.args
    if lvar_type(left).fits_mutable_set() and lvar_type(right).fits_set():
        retarget_known_combination(node, fndb.PROTOCOLS["set_sub"], [left, right])
        return True
    return False


@deftransform("%bit_xor")
def _transform_set_xor(node: Combination) -> bool:
    if len(node.args) != 2:
        return False
    left, right = node.args
    if lvar_type(left).fits_mutable_set() and lvar_type(right).fits_set():
        retarget_known_combination(node, fndb.PROTOCOLS["set_xor"], [left, right])
        return True
    return False


def _match_text_slice(lvar: Lvar) -> Combination | None:
    slice_node = splice_fun_args(lvar, ("%getslice",), 4, splice=False)
    if slice_node is None:
        return None
    seq_lvar, start_lvar, stop_lvar, step_lvar = slice_node.args
    if (
        not lvar_type(seq_lvar).fits_text(TEXT_TYPE)
        or not _is_slice_bound_lvar(start_lvar)
        or not _is_slice_bound_lvar(stop_lvar)
        or not _is_unit_slice_step(step_lvar)
    ):
        return None
    return slice_node


def _transform_text_slice_compare(node: Combination, *, invert: bool) -> bool:
    if len(node.args) != 2:
        return False
    left, right = node.args
    left_slice = _match_text_slice(left)
    right_slice = _match_text_slice(right)
    if (
        left_slice is not None
        and right_slice is not None
        and left_slice is not right_slice
    ):
        seq1, start1, stop1, _ = left_slice.args
        seq2, start2, stop2, _ = right_slice.args
        splice_fun_args(left, ("%getslice",), 4)
        splice_fun_args(right, ("%getslice",), 4)
        retarget_known_combination(
            node,
            fndb.TEXT_SLICE2_COMPARE,
            [seq1, start1, stop1, seq2, start2, stop2],
        )
        node.info = invert
        return True
    if left_slice is not None:
        seq1, start1, stop1, _ = left_slice.args
        splice_fun_args(left, ("%getslice",), 4)
        retarget_known_combination(
            node, fndb.TEXT_SLICE_COMPARE, [seq1, start1, stop1, right]
        )
        node.info = invert
        return True
    if right_slice is not None:
        seq2, start2, stop2, _ = right_slice.args
        splice_fun_args(right, ("%getslice",), 4)
        retarget_known_combination(
            node, fndb.TEXT_SLICE_COMPARE, [seq2, start2, stop2, left]
        )
        node.info = invert
        return True
    return False


@deftransform("%compare_equal", (TEXT_TYPE, TEXT_TYPE))
def _transform_text_slice_compare_equal(node: Combination) -> bool:
    return _transform_text_slice_compare(node, invert=False)


@deftransform("%compare_not_equal", (TEXT_TYPE, TEXT_TYPE))
def _transform_text_slice_compare_not_equal(node: Combination) -> bool:
    return _transform_text_slice_compare(node, invert=True)


@deftransform("%text_slice_compare")
def _transform_text_slice_compare_target(node: Combination) -> bool:
    if len(node.args) != 4:
        return False
    seq1, start1, stop1, target = node.args
    other_slice = _match_text_slice(target)
    if other_slice is None:
        return False
    seq2, start2, stop2, _ = other_slice.args
    invert = bool(node.info)
    splice_fun_args(target, ("%getslice",), 4)
    retarget_known_combination(
        node,
        fndb.TEXT_SLICE2_COMPARE,
        [seq1, start1, stop1, seq2, start2, stop2],
    )
    node.info = invert
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

    def visit_ByteArrayType(self, fact: ByteArrayType) -> FunInfo | None:
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

    def visit_DictType(self, fact: DictType) -> FunInfo | None:
        if not fact.proven or fact.may_be_unbound:
            return None
        return fndb.PROTOCOLS["dict_len"]

    def visit_SetType(self, fact: SetType) -> FunInfo | None:
        if not fact.proven or fact.may_be_unbound:
            return None
        return fndb.PROTOCOLS["table_len"]


_KNOWN_LENGTH_PRIMITIVE = _KnownLengthPrimitive()


_TUPLE_TYPE = TupleType()
_SEQUENCE_LENGTH_PRODUCERS = frozenset(
    {"%add", "_text_slice", "%getslice", "%_list_fill", "%multiply"}
)


class _IsMutableSeqType(ValueTypeVisitor):
    def visit_ValueType(self, fact: ValueType) -> bool:
        return False

    def visit_ByteArrayType(self, fact: ByteArrayType) -> bool:
        return True

    def visit_ListType(self, fact: ListType) -> bool:
        return True


_IS_MUTABLE_SEQ_TYPE = _IsMutableSeqType()


class _ConstantSeqLength(ast.Visitor):
    """Extract the length of a constant sequence literal, or None."""

    def visit_Node(self, node) -> int | None:
        return None

    def visit_TextLiteral(self, node: ast.TextLiteral) -> int:
        return len(node.value)

    def visit_BytesLiteral(self, node: ast.BytesLiteral) -> int:
        return len(node.value)

    def visit_Tuple(self, node: ast.Tuple) -> int:
        return len(node.elements)


_CONSTANT_SEQ_LENGTH = _ConstantSeqLength()


class _ConstantRefLeaf(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        if node.final:
            return None
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return None

    def visit_Constant(self, leaf: Constant) -> Constant:
        return leaf


_CONSTANT_REF_LEAF = _ConstantRefLeaf()


class _LocalLambdaVarRef(NodeVisitor, LeafVisitor):
    def __init__(self, home_lambda: Lambda | None):
        self.home_lambda = home_lambda
        self._ref: Ref | None = None

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        if node.final or node.allow_unbound:
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
            or leaf.home.coroutine_body
            or any(s.value is None for s in leaf.sets)
        ):
            return None
        return leaf


class _NonMutatingStep(NodeVisitor, LeafVisitor):
    def visit_Node(self, node) -> bool:
        return False

    def visit_Ref(self, node: Ref) -> bool:
        return is_flushable_ref(node)

    def visit_Cast(self, node: Cast) -> bool:
        return node.value is not None and (
            not node.type_check
            or not node.safety
            or lvar_type(node.value).fits(node.contract.checked_type)
        )

    def visit_Set(self, node: Set) -> bool:
        return node.value is not None and node.var.accept(self)

    def visit_Leaf(self, leaf) -> bool:
        return False

    def visit_LambdaVar(self, leaf: LambdaVar) -> bool:
        return not leaf.indirect and leaf.home is not None

    def visit_Combination(self, node: Combination) -> bool:
        return _is_effectless_known_call(node)


_NON_MUTATING_STEP = _NonMutatingStep()


def _node_precedes_in_block(first: Node, second: Node) -> bool:
    if first.prev is None or second.prev is None or first.block is not second.block:
        return False
    seen_first = False
    for node in first.block.nodes():
        if node is first:
            if seen_first or first is second:
                return False
            seen_first = True
        elif node is second:
            return seen_first
    return False


def _no_intervening_mutation(prod: Combination, call: Combination) -> bool:
    if prod.prev is None or call.prev is None or prod.block is not call.block:
        return False
    in_range = False
    for node in prod.block.nodes():
        if node is prod:
            in_range = True
            continue
        if node is call:
            return in_range
        if in_range and not node.accept(_NON_MUTATING_STEP):
            return False
    return False


def _var_unmodified_between(var: LambdaVar, ref_node: Ref, call: Combination) -> bool:
    if not var.sets:
        return True
    if ref_node.prev is None or call.prev is None:
        return False
    if ref_node.block is call.block:
        in_range = False
        var_sets = set(var.sets)
        for node in call.block.nodes():
            if node is ref_node:
                in_range = True
                continue
            if node is call:
                return in_range
            if in_range and node in var_sets:
                return False
        return False
    return (
        len(var.sets) == 1
        and var.sets[0].prev is not None
        and var.sets[0].block is ref_node.block
        and _node_precedes_in_block(var.sets[0], ref_node)
    )


def _match_sequence_producer(arg_lvar: Lvar, call: Combination) -> Combination | None:
    prod = lvar_matches(arg_lvar, _SEQUENCE_LENGTH_PRODUCERS)
    if prod is not None:
        if prod.prev is None or prod.lexenv.policy.safety >= 3:
            return None
        if prod.block is call.block and not _node_precedes_in_block(prod, call):
            return None
        return prod
    if len(arg_lvar.uses) != 1:
        return None
    local_info = arg_lvar.uses[0].accept(_LocalLambdaVarRef(call.home_lambda))
    if local_info is None:
        return None
    ref_node, var = local_info
    if (
        var in var.home.vars
        or ref_node.prev is None
        or ref_node.block is not call.block
        or not _node_precedes_in_block(ref_node, call)
    ):
        return None
    var_sets = set(var.sets)
    matched_store: Set | None = None
    for node in call.block.nodes():
        if node is ref_node:
            break
        if node in var_sets:
            matched_store = node
    if matched_store is None or matched_store.value is None:
        return None
    prod = lvar_matches(
        matched_store.value, _SEQUENCE_LENGTH_PRODUCERS, local_alias=False
    )
    if (
        prod is None
        or prod.prev is None
        or prod.lexenv.policy.safety >= 3
        or prod.block is not call.block
        or not _node_precedes_in_block(prod, matched_store)
    ):
        return None
    return prod


def _can_capture_producer_operand(
    prod: Combination, index: int, call: Combination
) -> bool:
    op_lvar = prod.args[index]
    if lvar_type(op_lvar).may_be_unbound or not op_lvar.uses:
        return False
    if any(use.prev is None for use in op_lvar.uses):
        return False
    if len(op_lvar.uses) == 1:
        use = op_lvar.uses[0]
        if use.accept(_CONSTANT_REF_LEAF) is not None:
            return True
        local_info = use.accept(_LocalLambdaVarRef(call.home_lambda))
        if local_info is not None:
            ref_node, var = local_info
            if _var_unmodified_between(var, ref_node, call):
                return True
    home = call.home_lambda
    return home is not None and not home.coroutine_body


def _capture_producer_operand(prod: Combination, index: int, call: Combination) -> Lvar:
    op_lvar = prod.args[index]
    op_type = lvar_type(op_lvar)
    if len(op_lvar.uses) == 1:
        use = op_lvar.uses[0]
        const_leaf = use.accept(_CONSTANT_REF_LEAF)
        if const_leaf is not None:
            ref = Ref(call.lexenv, call.source, const_leaf)
            const_leaf.refs.append(ref)
            ref.derived_type = op_type
            lvar = Lvar()
            use_lvar(ref, lvar)
            lvar.derived_type = op_type
            insert_node_before(call, ref)
            return lvar
        local_info = use.accept(_LocalLambdaVarRef(call.home_lambda))
        if local_info is not None:
            ref_node, var = local_info
            if _var_unmodified_between(var, ref_node, call):
                ref = Ref(call.lexenv, call.source, var)
                var.refs.append(ref)
                ref.derived_type = op_type
                lvar = Lvar()
                use_lvar(ref, lvar)
                lvar.derived_type = op_type
                insert_node_before(call, ref)
                return lvar
    home = linked(call.home_lambda, call, "home_lambda")
    tmp = LambdaVar(f"$seq_op_{id(op_lvar):x}_{index}", hidden=True)
    tmp.home = home
    tmp.type = op_type
    home.locals.append(tmp)
    set_node = Set(prod.lexenv, prod.source, tmp, op_lvar)
    op_lvar.dest = set_node
    tmp.sets.append(set_node)
    insert_node_before(prod, set_node)
    ref1 = Ref(prod.lexenv, prod.source, tmp)
    tmp.refs.append(ref1)
    ref1.derived_type = op_type
    lvar1 = Lvar()
    use_lvar(ref1, lvar1)
    lvar1.derived_type = op_type
    lvar1.dest = prod
    prod.args[index] = lvar1
    insert_node_before(prod, ref1)
    ref2 = Ref(call.lexenv, call.source, tmp)
    tmp.refs.append(ref2)
    ref2.derived_type = op_type
    lvar2 = Lvar()
    use_lvar(ref2, lvar2)
    lvar2.derived_type = op_type
    insert_node_before(call, ref2)
    return lvar2


def _emit_const_int_before(before_node: Node, value: int) -> Lvar:
    span = before_node.source.span if before_node.source is not None else None
    literal = ast.IntegerLiteral(value, span=span)
    block = before_node.block
    leaf = (
        find_constant_leaf(block.component.constants, literal)
        if block.component is not None
        else Constant(literal)
    )
    vtype = IntegerType(value, value)
    ref = Ref(before_node.lexenv, before_node.source, leaf)
    ref.derived_type = vtype
    leaf.refs.append(ref)
    lvar = Lvar()
    use_lvar(ref, lvar)
    lvar.derived_type = vtype
    insert_node_before(before_node, ref)
    return lvar


def _emit_known_call_before(before_node: Node, info: FunInfo, args: list[Lvar]) -> Lvar:
    fun_leaf = KnownFunction(info)
    fun_ref = Ref(before_node.lexenv, before_node.source, fun_leaf)
    fun_leaf.refs.append(fun_ref)
    fun_lvar = Lvar()
    use_lvar(fun_ref, fun_lvar)
    comb = Combination(before_node.lexenv, before_node.source, fun_lvar)
    comb.kind = CallKind.KNOWN
    comb.fun_info = info
    fun_lvar.dest = comb
    comb.args = list(args)
    for arg in args:
        arg.dest = comb
    out_lvar = Lvar()
    use_lvar(comb, out_lvar)
    insert_node_before(before_node, fun_ref)
    insert_node_before(before_node, comb)
    derived = info.derive_type(comb) if info.derive_type is not None else None
    comb.derived_type = derived if derived is not None else UNKNOWN
    out_lvar.derived_type = comb.derived_type
    comb.reoptimize = True
    return out_lvar


def _emit_seq_length_before(
    before_node: Node, seq_lvar: Lvar, primitive: FunInfo
) -> Lvar:
    if is_pure_bound_ref(seq_lvar):
        literal = constant_literal(seq_lvar)
        if literal is not None:
            const_len = literal.accept(_CONSTANT_SEQ_LENGTH)
            if const_len is not None:
                _flush_operand(seq_lvar)
                return _emit_const_int_before(before_node, const_len)
        exact_len = lvar_type(seq_lvar).exact_length()
        if exact_len is not None:
            _flush_operand(seq_lvar)
            return _emit_const_int_before(before_node, exact_len)
    return _emit_known_call_before(before_node, primitive, [seq_lvar])


def _same_sequence_family(left: ValueType, right: ValueType, result: ValueType) -> bool:
    return (
        (
            left.fits_text(TEXT_TYPE)
            and right.fits_text(TEXT_TYPE)
            and result.fits_text(TEXT_TYPE)
        )
        or (
            (left.fits(BYTES_TYPE) or left.fits(BYTEARRAY_TYPE))
            and (right.fits(BYTES_TYPE) or right.fits(BYTEARRAY_TYPE))
            and (result.fits(BYTES_TYPE) or result.fits(BYTEARRAY_TYPE))
        )
        or (left.fits(LIST_TYPE) and right.fits(LIST_TYPE) and result.fits(LIST_TYPE))
        or (
            left.fits_tuple(_TUPLE_TYPE)
            and right.fits_tuple(_TUPLE_TYPE)
            and result.fits_tuple(_TUPLE_TYPE)
        )
    )


class _TransformInnerLength(NodeVisitor):
    def visit_Node(self, node) -> bool:
        return False

    def visit_Combination(self, node: Combination) -> bool:
        return _transform_known_length_primitive(node)


_TRANSFORM_INNER_LENGTH = _TransformInnerLength()


def _finalize_length_retarget(
    call: Combination,
    info: FunInfo,
    args: list[Lvar],
    *,
    nested_len_lvars: tuple[Lvar, ...] = (),
) -> bool:
    retarget_known_combination(call, info, args)
    derived = info.derive_type(call) if info.derive_type is not None else None
    if derived is not None:
        call.derived_type = derived
        if call.lvar is not None:
            call.lvar.derived_type = derived
    if call.block is not None and call.block.flush_p:
        call.block.flush_p = False
        _flush_dead_operands(call.block)
    for len_lvar in nested_len_lvars:
        if (
            len(len_lvar.uses) == 1
            and len_lvar.uses[0].prev is not None
            and len_lvar.uses[0].accept(_TRANSFORM_INNER_LENGTH)
            and call.prev is not None
            and call.fun_info is not None
        ):
            new_derived = (
                call.fun_info.derive_type(call)
                if call.fun_info.derive_type is not None
                else None
            )
            if new_derived is not None:
                call.derived_type = new_derived
                if call.lvar is not None:
                    call.lvar.derived_type = new_derived
    return True


def _finalize_length_replace_lvar(call: Combination, new_lvar: Lvar) -> bool:
    for old_arg in list(call.args):
        if old_arg is not new_lvar:
            _flush_operand(old_arg)
    call.args = [new_lvar]
    new_lvar.dest = call
    block = call.block
    replace_combination_with_lvar(call, new_lvar)
    if block is not None and block.flush_p:
        block.flush_p = False
        _flush_dead_operands(block)
    return True


def _transform_known_length_primitive(call: Combination) -> bool:
    """Rewrite ``_text_length(x)`` or ``_storage_length(x)`` when ``x`` is
    produced by a sequence concatenation, slice, or repetition."""
    if (
        call.kind is not CallKind.KNOWN
        or call.fun_info is None
        or call.lvar is None
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
    primitive = call.fun_info
    arg_lvar = call.args[0]
    arg_t = lvar_type(arg_lvar)
    if known_length_primitive(arg_t) is not primitive:
        return False

    prod = _match_sequence_producer(arg_lvar, call)
    if prod is None or prod.fun_info is None:
        return False
    if arg_t.accept(_IS_MUTABLE_SEQ_TYPE) and not _no_intervening_mutation(prod, call):
        return False

    prod_name = prod.fun_info.name
    add_info = fndb.BINARY[ast.BinaryOperator.ADD]
    sub_info = fndb.BINARY[ast.BinaryOperator.SUBTRACT]
    mul_info = fndb.BINARY[ast.BinaryOperator.MULTIPLY]

    if prod_name == "%add" and len(prod.args) == 2:
        left_t = lvar_type(prod.args[0])
        right_t = lvar_type(prod.args[1])
        if (
            known_length_primitive(left_t) is not primitive
            or known_length_primitive(right_t) is not primitive
            or not _same_sequence_family(left_t, right_t, arg_t)
            or not _can_capture_producer_operand(prod, 0, call)
            or not _can_capture_producer_operand(prod, 1, call)
        ):
            return False
        left_op = _capture_producer_operand(prod, 0, call)
        right_op = _capture_producer_operand(prod, 1, call)
        len_left = _emit_seq_length_before(call, left_op, primitive)
        len_right = _emit_seq_length_before(call, right_op, primitive)
        return _finalize_length_retarget(
            call,
            add_info,
            [len_left, len_right],
            nested_len_lvars=(len_left, len_right),
        )

    if prod_name == "_text_slice" and len(prod.args) == 3:
        start_t = lvar_type(prod.args[1])
        stop_t = lvar_type(prod.args[2])
        if (
            start_t.may_be_unbound
            or stop_t.may_be_unbound
            or not start_t.fits(INTEGER)
            or not stop_t.fits(INTEGER)
            or not _can_capture_producer_operand(prod, 1, call)
            or not _can_capture_producer_operand(prod, 2, call)
        ):
            return False
        start_op = _capture_producer_operand(prod, 1, call)
        stop_op = _capture_producer_operand(prod, 2, call)
        if start_t.exact_integer() == 0:
            _flush_operand(start_op)
            return _finalize_length_replace_lvar(call, stop_op)
        return _finalize_length_retarget(call, sub_info, [stop_op, start_op])

    if prod_name == "%getslice" and len(prod.args) == 4:
        seq_t = lvar_type(prod.args[0])
        start_t = lvar_type(prod.args[1])
        stop_t = lvar_type(prod.args[2])
        step_t = lvar_type(prod.args[3])
        if (
            known_length_primitive(seq_t) is not primitive
            or start_t.may_be_unbound
            or stop_t.may_be_unbound
            or step_t.may_be_unbound
            or not (step_t.fits_none(NONE_TYPE) or step_t.exact_integer() == 1)
            or not _can_capture_producer_operand(prod, 0, call)
        ):
            return False
        start_is_zero = start_t.fits_none(NONE_TYPE) or start_t.exact_integer() == 0
        start_nonneg = start_is_zero or (
            start_t.fits(INTEGER)
            and start_t.minimum is not None
            and start_t.minimum >= 0
        )
        stop_is_none = stop_t.fits_none(NONE_TYPE)
        stop_nonneg = (
            not stop_is_none
            and stop_t.fits(INTEGER)
            and stop_t.minimum is not None
            and stop_t.minimum >= 0
        )
        if not start_nonneg or not (stop_is_none or stop_nonneg):
            return False
        if not start_is_zero and not _can_capture_producer_operand(prod, 1, call):
            return False
        if not stop_is_none and not _can_capture_producer_operand(prod, 2, call):
            return False
        seq_op = _capture_producer_operand(prod, 0, call)
        if start_is_zero and stop_is_none:
            seq_len = _emit_seq_length_before(call, seq_op, primitive)
            return _finalize_length_replace_lvar(call, seq_len)
        if start_is_zero and stop_nonneg:
            stop_op = _capture_producer_operand(prod, 2, call)
            seq_len = _emit_seq_length_before(call, seq_op, primitive)
            seq_len_t = lvar_type(seq_len)
            if (
                stop_t.maximum is not None
                and seq_len_t.minimum is not None
                and stop_t.maximum <= seq_len_t.minimum
            ):
                _flush_operand(seq_len)
                return _finalize_length_replace_lvar(call, stop_op)
            return _finalize_length_retarget(
                call, fndb.MIN2, [seq_len, stop_op], nested_len_lvars=(seq_len,)
            )
        if not start_is_zero and stop_is_none:
            start_op = _capture_producer_operand(prod, 1, call)
            seq_len = _emit_seq_length_before(call, seq_op, primitive)
            seq_len_t = lvar_type(seq_len)
            if (
                start_t.maximum is not None
                and seq_len_t.minimum is not None
                and start_t.maximum <= seq_len_t.minimum
            ):
                return _finalize_length_retarget(
                    call, sub_info, [seq_len, start_op], nested_len_lvars=(seq_len,)
                )
            diff = _emit_known_call_before(call, sub_info, [seq_len, start_op])
            zero = _emit_const_int_before(call, 0)
            return _finalize_length_retarget(
                call, fndb.MAX2, [zero, diff], nested_len_lvars=(seq_len,)
            )
        start_op = _capture_producer_operand(prod, 1, call)
        stop_op = _capture_producer_operand(prod, 2, call)
        seq_len = _emit_seq_length_before(call, seq_op, primitive)
        seq_len_t = lvar_type(seq_len)
        if (
            stop_t.maximum is not None
            and seq_len_t.minimum is not None
            and stop_t.maximum <= seq_len_t.minimum
        ):
            _flush_operand(seq_len)
            clamped_stop = stop_op
        else:
            clamped_stop = _emit_known_call_before(call, fndb.MIN2, [seq_len, stop_op])
        clamped_t = lvar_type(clamped_stop)
        if (
            start_t.maximum is not None
            and clamped_t.minimum is not None
            and start_t.maximum <= clamped_t.minimum
        ):
            return _finalize_length_retarget(
                call,
                sub_info,
                [clamped_stop, start_op],
                nested_len_lvars=(seq_len,),
            )
        diff = _emit_known_call_before(call, sub_info, [clamped_stop, start_op])
        zero = _emit_const_int_before(call, 0)
        return _finalize_length_retarget(
            call, fndb.MAX2, [zero, diff], nested_len_lvars=(seq_len,)
        )

    if prod_name == "%_list_fill" and len(prod.args) == 2:
        elem_t = lvar_type(prod.args[0])
        count_t = lvar_type(prod.args[1])
        if (
            elem_t.may_be_unbound
            or count_t.may_be_unbound
            or not count_t.fits(INTEGER)
            or count_t.minimum is None
            or count_t.maximum is None
            or count_t.minimum < -(1 << 63)
            or count_t.maximum >= (1 << 60)
            or not _can_capture_producer_operand(prod, 1, call)
        ):
            return False
        count_op = _capture_producer_operand(prod, 1, call)
        if count_t.minimum >= 0:
            return _finalize_length_replace_lvar(call, count_op)
        zero = _emit_const_int_before(call, 0)
        return _finalize_length_retarget(call, fndb.MAX2, [zero, count_op])

    if prod_name == "%multiply" and len(prod.args) == 2:
        t0 = lvar_type(prod.args[0])
        t1 = lvar_type(prod.args[1])
        if known_length_primitive(t0) is primitive and t1.fits(INTEGER):
            seq_idx, cnt_idx, cnt_t = 0, 1, t1
        elif known_length_primitive(t1) is primitive and t0.fits(INTEGER):
            seq_idx, cnt_idx, cnt_t = 1, 0, t0
        else:
            return False
        if (
            cnt_t.may_be_unbound
            or cnt_t.minimum is None
            or cnt_t.maximum is None
            or cnt_t.minimum < -(1 << 63)
            or cnt_t.maximum > (1 << 30)
            or not _can_capture_producer_operand(prod, seq_idx, call)
            or not _can_capture_producer_operand(prod, cnt_idx, call)
        ):
            return False
        seq_op = _capture_producer_operand(prod, seq_idx, call)
        cnt_op = _capture_producer_operand(prod, cnt_idx, call)
        seq_len = _emit_seq_length_before(call, seq_op, primitive)
        if cnt_t.minimum >= 0:
            nonneg_cnt = cnt_op
        else:
            zero = _emit_const_int_before(call, 0)
            nonneg_cnt = _emit_known_call_before(call, fndb.MAX2, [zero, cnt_op])
        return _finalize_length_retarget(
            call,
            mul_info,
            [seq_len, nonneg_cnt],
            nested_len_lvars=(seq_len,),
        )

    return False


_LENGTH_PRIMITIVE_TRANSFORMS_REGISTERED = False
_LENGTH_PRIMITIVE_LOCK = Lock()


def _ensure_length_primitive_transforms() -> None:
    global _LENGTH_PRIMITIVE_TRANSFORMS_REGISTERED
    if _LENGTH_PRIMITIVE_TRANSFORMS_REGISTERED:
        return
    with _LENGTH_PRIMITIVE_LOCK:
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
    _transform_known_length_primitive(node)
    return True


class _ConstantOrdValue(ast.Visitor):
    """Extract the codepoint or byte of a 1-element constant str or bytes literal."""

    def visit_Node(self, node) -> int | None:
        return None

    def visit_TextLiteral(self, node: ast.TextLiteral) -> int | None:
        return ord(node.value) if len(node.value) == 1 else None

    def visit_BytesLiteral(self, node: ast.BytesLiteral) -> int | None:
        return node.value[0] if len(node.value) == 1 else None


_CONSTANT_ORD_VALUE = _ConstantOrdValue()


@deftransform("ord")
def _transform_ord(node: Combination) -> bool:
    """Fold constant ``ord('a')`` / ``ord(b'a')`` and fuse ``ord(s[i])`` into
    direct codepoint extraction when ``s`` is a proven builtin ``str`` and ``i``
    is a nonnegative integer."""
    if len(node.args) != 1:
        return False
    arg = node.args[0]
    if is_pure_bound_ref(arg):
        lit = constant_lvar_value(arg)
        if lit is not None:
            code = lit.accept(_CONSTANT_ORD_VALUE)
            if code is not None:
                folded = ast.IntegerLiteral(code)
                folded.span = node.source.span if node.source is not None else None
                replace_combination_with_constant(node, folded)
                return True
    getitem_node = splice_fun_args(arg, ("%getitem",), 2, splice=False)
    if getitem_node is not None:
        seq_lvar, idx_lvar = getitem_node.args
        seq_type = lvar_type(seq_lvar)
        idx_type = lvar_type(idx_lvar)
        if seq_type.exact_text and idx_type.fits_python_integer(PYTHON_INTEGER):
            splice_fun_args(arg, ("%getitem",), 2)
            retarget_known_combination(
                node, KNOWN["_text_codepoint"], [seq_lvar, idx_lvar]
            )
            return True
    return False


class _AttributeName(ast.Visitor):
    """The text of a constant attribute name, or None."""

    def visit_Node(self, node):
        return None

    def visit_TextLiteral(self, node: ast.TextLiteral) -> str | None:
        return node.value if node.value.isidentifier() else None


_ATTRIBUTE_NAME = _AttributeName()


def _constant_attribute_name(lvar: Lvar) -> str | None:
    if not is_pure_bound_ref(lvar):
        return None
    literal = constant_lvar_value(lvar)
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
    """``getattr`` with a constant name is attribute access (SBCL's
    ``slot-value`` with a constant slot name compiles to the slot accessor):
    ``getattr(x, "name")`` is ``x.name``, and ``getattr(x, "name", default)``
    is the same cached lookup with a miss returning ``default``. The name is
    used as written: ``getattr`` never applies private-name mangling. The
    builtin name check is the predictable-global test ``_OptimizeNode``
    already made."""
    if len(node.args) not in (2, 3):
        return False
    name = _constant_attribute_name(node.args[1])
    if name is None:
        return False
    if len(node.args) == 2:
        _retarget_attribute_builtin(node, fndb.GETATTR, name, [node.args[0]])
        return True
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


@deftransform("isinstance")
def _transform_instance_check(node: Combination) -> bool:
    """``isinstance`` with a class operand or with the tuple built at the call
    site becomes ``%isinstance`` with the
    classes as separate operands, so the tuple is never allocated (SBCL's
    ``typep`` with a constant type specifier is open-coded per type). The
    tuple's elements are already evaluated in order before the call; only
    the allocation goes away."""
    if len(node.args) != 2:
        return False
    value, classinfo = node.args
    if lvar_type(classinfo).fits_class():
        retarget_known_combination(node, fndb.ISINSTANCE, [value, classinfo])
        return True
    tuple_node = splice_fun_args(classinfo, ("%build_tuple",), splice=False)
    if tuple_node is None or not tuple_node.args:
        return False
    classes = list(tuple_node.args)
    splice_fun_args(classinfo, ("%build_tuple",))
    retarget_known_combination(node, fndb.ISINSTANCE, [value, *classes])
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


class _RangeCallCheck(NodeVisitor):
    def __init__(self, block, arguments):
        self.block = block
        self.arguments = arguments

    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.prev is not None
            and node.block is self.block
            and tuple(node.args) == self.arguments
            and len(node.fun.uses) == 1
        ):
            return node
        return None


class _IterateCallCheck(NodeVisitor):
    def __init__(self, block, arguments):
        self.block = block
        self.arguments = arguments

    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.prev is not None
            and node.block is self.block
            and node.fun_info is fndb.PROTOCOLS["iterate"]
            and len(node.args) == 1
            and len(node.args[0].uses) == 1
        ):
            range_call = (
                node.args[0].uses[0].accept(_RangeCallCheck(self.block, self.arguments))
            )
            if range_call is not None:
                return node, range_call
        return None


class _RangeIteratorRef(NodeVisitor, LeafVisitor):
    def __init__(self, block, arguments):
        self.block = block
        self.arguments = arguments
        self._ref: Ref | None = None

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        self._ref = node
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return None

    def visit_LambdaVar(self, leaf: LambdaVar):
        if len(leaf.sets) != 1:
            return None
        store = leaf.sets[0]
        if (
            store.prev is None
            or store.block is not self.block
            or store.value is None
            or len(store.value.uses) != 1
        ):
            return None
        matched = store.value.uses[0].accept(
            _IterateCallCheck(self.block, self.arguments)
        )
        if matched is None:
            return None
        iterate_call, range_call = matched
        return self._ref, store, iterate_call, range_call


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

    if len(state.arguments) <= 2:
        step_sign = 1
    else:
        step_type = lvar_type(state.arguments[2])
        if step_type.minimum is not None and step_type.minimum > 0:
            step_sign = 1
        elif step_type.maximum is not None and step_type.maximum < 0:
            step_sign = -1
        else:
            step_sign = 0

    if step_sign != 0 and len(node.args[1].uses) == 1:
        setup = (
            node.args[1].uses[0].accept(_RangeIteratorRef(node.block, state.arguments))
        )
        if setup is not None:
            iterator_ref, iterator_set, iterate_call, range_call = setup
            callee_ref = callee_lvar.uses[0]
            callee_var = callee_ref.leaf
            callee_ref_1 = range_call.fun.uses[0]

            callee_lvar.dest = None
            delete_ref(callee_ref)
            unlink_node(callee_ref)

            iterator_var = iterator_ref.leaf
            iterator_lvar = node.args[1]
            iterator_lvar.dest = None
            delete_ref(iterator_ref)
            unlink_node(iterator_ref)
            for ref in list(iterator_var.refs):
                if ref.lvar is None and ref.prev is not None:
                    delete_ref(ref)
                    unlink_node(ref)

            range_call.args = []
            for arg_lvar in state.arguments:
                arg_lvar.dest = node
            node.args = list(state.arguments)
            state.direct = True
            state.step_sign = step_sign

            iterated_lvar = iterator_set.value
            if iterated_lvar is not None:
                iterated_lvar.dest = None
            delete_set(iterator_set)
            unlink_node(iterator_set)

            _flush_operand(iterate_call.fun)
            range_val_lvar = iterate_call.args[0]
            range_val_lvar.dest = None
            iterate_call.args = []
            unlink_node(iterate_call)

            range_call.fun.dest = None
            delete_ref(callee_ref_1)
            unlink_node(callee_ref_1)
            unlink_node(range_call)

            if not callee_var.refs and len(callee_var.sets) == 1:
                callee_set = callee_var.sets[0]
                if (
                    callee_set.prev is not None
                    and callee_set.value is not None
                    and len(callee_set.value.uses) == 1
                ):
                    range_global_ref = callee_set.value.uses[0]
                    callee_set.value.dest = None
                    delete_ref(range_global_ref)
                    unlink_node(range_global_ref)
                    delete_set(callee_set)
                    unlink_node(callee_set)

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
    "_PREDICTABLE_RANGE_CALLEE_REF",
    "_RangeIteratorRef",
    "_transform_constant_getslice",
    "_transform_empty_container_not",
    "_transform_len",
    "_transform_range_prepare",
    "_transform_tuple_getitem",
    "_transform_unpack",
    "is_predictable_len_call",
    "known_length_primitive",
]
