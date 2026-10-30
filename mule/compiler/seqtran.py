"""Sequence IR1 transforms, following ``src/compiler/seqtran.lisp``.

Transforms registered via ``@deftransform`` fold constant-index tuple
subscripting when dropped elements are effect-free and non-raising, and
simplify truthiness negation of newly constructed empty tuples and lists.
"""

from __future__ import annotations

from dataclasses import dataclass, replace

from mule import ast

from . import fndb
from .ctype import (
    FIXNUM_MIN,
    INTEGER,
    TEXT_TYPE,
    IntegerType,
)
from .ir1opt import (
    _flush_operand,
    constant_literal,
    constant_lvar_value,
    lvar_type,
    replace_combination_with_constant,
    splice_fun_args,
)
from .ir1util import (
    delete_lvar_use,
    delete_ref,
    delete_set,
    insert_node_before,
    is_pure_bound_ref,
    reoptimize_lvar,
    replace_combination_with_lvar,
    retarget_known_combination,
    substitute_lvar_uses,
    unlink_node,
    use_lvar,
)
from .knownfun import deftransform
from .node import (
    CallKind,
    Combination,
    GlobalVar,
    LambdaVar,
    LeafVisitor,
    Lvar,
    Node,
    NodeVisitor,
    Ref,
    Set,
    linked,
)
from .seqtran_length import (
    _PREDICTABLE_RANGE_CALLEE_REF,
    _PROTOCOL_PREFIXES,
    _UNMUTATED_RANGE_GLOBAL_REF,
    _emit_const_int_before,
    _emit_known_call_before,
    _RangeIteratorRef,
    _transform_len,
    _transform_range_prepare,
    is_predictable_len_call,
    known_length_primitive,
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
    if node.lexenv.policy.safety < 3 and (
        node.args[0].uses[0].accept(_ENUMERATE_CALL_CHECK) is not None
        or node.args[0].uses[0].accept(_EnumerateHeldRef()) is not None
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


class _EnumerateHeldRef(NodeVisitor, LeafVisitor):
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
        if len(leaf.sets) != 1 or len(leaf.refs) != 1 or leaf.refs[0] is not self._ref:
            return None
        store = leaf.sets[0]
        if store.prev is None or store.value is None or len(store.value.uses) != 1:
            return None
        enum_call = store.value.uses[0].accept(_ENUMERATE_CALL_CHECK)
        if enum_call is None or enum_call.block is not store.block:
            return None
        return self._ref, leaf, store, enum_call


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
            use = node.args[0].uses[0]
            enum_call = use.accept(_ENUMERATE_CALL_CHECK)
            if enum_call is not None and enum_call.block is node.block:
                return node, enum_call, None
            held_info = use.accept(_EnumerateHeldRef())
            if held_info is not None:
                held_ref, held_var, held_set, enum_call = held_info
                if held_set.block is node.block:
                    return node, enum_call, (held_ref, held_var, held_set)
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
        iterate_call, enum_call, held_info = matched
        if store.block is not iterate_call.block:
            return None
        return self._ref, leaf, store, iterate_call, enum_call, held_info


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
                        _held_info,
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
            held_info,
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
        if held_info is not None:
            _held_ref, _held_var, held_set = held_info
            seq_arg.dest = held_set
            enum_res_lvar = held_set.value
            assert enum_res_lvar is not None
            enum_res_lvar.dest = None
            delete_lvar_use(enum_call)
            held_set.value = seq_arg
            reoptimize_lvar(iterate_call.args[0])
        else:
            seq_arg.dest = iterate_call
            enum_res_lvar = iterate_call.args[0]
            enum_res_lvar.dest = None
            delete_lvar_use(enum_call)
            iterate_call.args[0] = seq_arg
            reoptimize_lvar(seq_arg)
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

    if splice_fun_args(arg, ("%build_tuple", "%build_list"), 0) is None:
        return False
    folded = ast.BooleanLiteral(True)
    folded.span = node.source.span if node.source is not None else None
    replace_combination_with_constant(node, folded)
    return True


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


__all__ = [
    "_PREDICTABLE_RANGE_CALLEE_REF",
    "_UNMUTATED_RANGE_GLOBAL_REF",
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
