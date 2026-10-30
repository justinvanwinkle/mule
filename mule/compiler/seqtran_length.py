"""Sequence length, range, attribute, and codepoint comparison IR1 transforms."""

from __future__ import annotations

from threading import Lock

from mule import ast

from . import fndb
from .ctype import (
    BOOLEAN_TRUE,
    BYTEARRAY_TYPE,
    BYTES_TYPE,
    INTEGER,
    LIST_TYPE,
    NONE_TYPE,
    PYTHON_INTEGER,
    TEXT_TYPE,
    UNKNOWN,
    ByteArrayType,
    BytesType,
    IntegerType,
    ListType,
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


CODEPOINT_TYPE = IntegerType(0, 0x10FFFF)
CODEPOINT_COMPARE_NAMES = (
    "%compare_equal",
    "%compare_not_equal",
    "%compare_less",
    "%compare_less_equal",
    "%compare_greater",
    "%compare_greater_equal",
)


class SingleCharCodepoint(ast.Visitor):
    """Extract the Unicode codepoint of a 1-character ``ast.TextLiteral``."""

    def visit_Node(self, node):
        return None

    def visit_TextLiteral(self, node: ast.TextLiteral) -> int | None:
        return ord(node.value) if len(node.value) == 1 else None


SINGLE_CHAR_CODEPOINT = SingleCharCodepoint()


class CodepointConstantRef(NodeVisitor):
    """Match a single-use constant ``Ref`` holding a 1-character ``ast.TextLiteral``."""

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref) -> tuple[Ref, int] | None:
        if node.prev is None or node.lvar is None or len(node.lvar.uses) != 1:
            return None
        literal = constant_literal(node.lvar)
        if literal is None:
            return None
        code = literal.accept(SINGLE_CHAR_CODEPOINT)
        if code is None:
            return None
        return node, code


CODEPOINT_CONSTANT_REF = CodepointConstantRef()


def match_codepoint_comparison(
    node: Combination, char_lvar: Lvar
) -> tuple[Combination, Ref, int] | None:
    if (
        node.kind is not CallKind.KNOWN
        or node.fun_info is None
        or node.fun_info.name not in CODEPOINT_COMPARE_NAMES
        or len(node.args) != 2
    ):
        return None
    if node.args[0] is char_lvar and node.args[1] is not char_lvar:
        other_lvar = node.args[1]
    elif node.args[1] is char_lvar and node.args[0] is not char_lvar:
        other_lvar = node.args[0]
    else:
        return None
    if len(other_lvar.uses) != 1:
        return None
    const_match = other_lvar.uses[0].accept(CODEPOINT_CONSTANT_REF)
    if const_match is None:
        return None
    const_ref, code = const_match
    return node, const_ref, code


def store_dominates_ref(home_lambda: Lambda, store: Set, ref: Ref) -> bool:
    store_block = store.block
    ref_block = ref.block
    if store_block is None or ref_block is None:
        return False
    if store_block is ref_block:
        for node in store_block.nodes():
            if node is store:
                return True
            if node is ref:
                return False
        return False
    if home_lambda.bind is None or home_lambda.bind.block is None:
        return False
    entry_block = home_lambda.bind.block
    if store_block is entry_block:
        return True
    visited = {entry_block, store_block}
    worklist = [entry_block]
    while worklist:
        cur = worklist.pop()
        for succ in cur.succ:
            if succ is ref_block:
                return False
            if succ not in visited:
                visited.add(succ)
                worklist.append(succ)
    return True


class CodepointConsumerCheck(NodeVisitor, LeafVisitor):
    """Verify that a character producer's value is only consumed by 1-character
    literal comparisons (directly or through local single-source variables)."""

    def __init__(self, home_lambda: Lambda):
        self.home_lambda = home_lambda
        self.active_lvar: Lvar | None = None
        self.visited_vars: set[LambdaVar] = set()
        self.comparisons: list[tuple[Combination, Ref, int]] = []

    def check_lvar(self, lvar: Lvar) -> bool:
        if lvar.dest is None:
            return False
        prev_lvar = self.active_lvar
        self.active_lvar = lvar
        ok = lvar.dest.accept(self)
        self.active_lvar = prev_lvar
        return ok

    def visit_Node(self, node) -> bool:
        return False

    def visit_Combination(self, node: Combination) -> bool:
        if self.active_lvar is None:
            return False
        matched = match_codepoint_comparison(node, self.active_lvar)
        if matched is None:
            return False
        self.comparisons.append(matched)
        return True

    def visit_Set(self, node: Set) -> bool:
        if node.value is not self.active_lvar:
            return False
        return node.var.accept(self)

    def visit_Leaf(self, leaf) -> bool:
        return False

    def visit_LambdaVar(self, leaf: LambdaVar) -> bool:
        if leaf in self.visited_vars:
            return True
        if (
            leaf.indirect
            or leaf.home is not self.home_lambda
            or leaf.home.coroutine_body
            or leaf in leaf.home.vars
            or len(leaf.sets) != 1
            or not leaf.refs
        ):
            return False
        store = leaf.sets[0]
        self.visited_vars.add(leaf)
        has_active_ref = False
        for ref in leaf.refs:
            if ref.prev is None:
                continue
            if ref.lvar is None:
                if leaf.hidden and ref.final:
                    continue
                return False
            if ref.allow_unbound:
                return False
            if (
                ref.derived_type is None or ref.derived_type.may_be_unbound
            ) and not store_dominates_ref(self.home_lambda, store, ref):
                return False
            has_active_ref = True
            if not self.check_lvar(ref.lvar):
                return False
        return has_active_ref


def is_valid_codepoint_producer(node: Combination) -> bool:
    if (
        node.kind is not CallKind.KNOWN
        or node.fun_info is None
        or node.lvar is None
        or len(node.lvar.uses) != 1
        or node.prev is None
        or node.lexenv.policy.safety >= 3
    ):
        return False
    home = node.home_lambda
    if (
        home is None
        or home.policy.safety >= 3
        or home.name.startswith(_PROTOCOL_PREFIXES)
    ):
        return False
    name = node.fun_info.name
    if name == "%getitem":
        if len(node.args) != 2:
            return False
        seq_type = lvar_type(node.args[0])
        idx_type = lvar_type(node.args[1])
        return seq_type.fits_text(TEXT_TYPE) and idx_type.fits_python_integer(
            PYTHON_INTEGER
        )
    if name == "%byte_next":
        if len(node.args) != 1 or node.info is None:
            return False
        if node.derived_type is not None and node.derived_type.is_fixnum:
            return False
        return lvar_type(node.info.source).fits_text(TEXT_TYPE)
    return False


def rewrite_constant_ref_to_codepoint(const_ref: Ref, code: int) -> None:
    old_leaf = const_ref.leaf
    if const_ref in old_leaf.refs:
        old_leaf.refs.remove(const_ref)
    span = const_ref.source.span if const_ref.source is not None else None
    literal = ast.IntegerLiteral(code, span=span)
    block = const_ref.block
    new_leaf = (
        find_constant_leaf(block.component.constants, literal)
        if block is not None and block.component is not None
        else Constant(literal)
    )
    const_ref.leaf = new_leaf
    new_leaf.refs.append(const_ref)
    vtype = IntegerType(code, code)
    const_ref.derived_type = vtype
    if const_ref.lvar is not None:
        const_ref.lvar.derived_type = vtype


from .byteops import NEXT as BYTE_NEXT  # noqa: F401


@deftransform("%getitem")
@deftransform("%byte_next")
def transform_text_codepoint_producer(node: Combination) -> bool:
    if not is_valid_codepoint_producer(node):
        return False
    checker = CodepointConsumerCheck(node.home_lambda)
    if not checker.check_lvar(node.lvar) or not checker.comparisons:
        return False
    if node.fun_info.name == "%getitem":
        retarget_known_combination(node, KNOWN["_text_codepoint"], list(node.args))
    node.derived_type = CODEPOINT_TYPE
    node.lvar.derived_type = CODEPOINT_TYPE
    reoptimize_lvar(node.lvar)
    for var in checker.visited_vars:
        for store in var.sets:
            if store.value is not None:
                store.value.derived_type = CODEPOINT_TYPE
        for ref in var.refs:
            if ref.prev is not None:
                ref.derived_type = CODEPOINT_TYPE
                if ref.lvar is not None:
                    ref.lvar.derived_type = CODEPOINT_TYPE
                    reoptimize_lvar(ref.lvar)
    for comp_node, const_ref, code in checker.comparisons:
        rewrite_constant_ref_to_codepoint(const_ref, code)
        if comp_node.prev is not None and comp_node.block is not None:
            comp_node.reoptimize = True
            comp_node.block.reoptimize = True
    return True


class FindCodepointProducer(NodeVisitor, LeafVisitor):
    """Trace a comparison operand back to its ``%getitem`` or ``%byte_next``
    producer through single-assignment local variables."""

    def __init__(self):
        self.seen_vars: set[LambdaVar] = set()

    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is not None
            and node.fun_info.name in ("%getitem", "%byte_next")
        ):
            return node
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return None

    def visit_LambdaVar(self, leaf: LambdaVar):
        if leaf in self.seen_vars or len(leaf.sets) != 1 or leaf.indirect:
            return None
        self.seen_vars.add(leaf)
        store = leaf.sets[0]
        if store.value is None or len(store.value.uses) != 1:
            return None
        return store.value.uses[0].accept(self)


for compare_name in CODEPOINT_COMPARE_NAMES:

    @deftransform(compare_name)
    def transform_text_codepoint_compare(node: Combination) -> bool:
        if len(node.args) != 2:
            return False
        for idx in (0, 1):
            arg = node.args[idx]
            if len(arg.uses) != 1:
                continue
            producer = arg.uses[0].accept(FindCodepointProducer())
            if producer is not None and transform_text_codepoint_producer(producer):
                return True
        return False
