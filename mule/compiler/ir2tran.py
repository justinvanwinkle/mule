"""IR1 to IR2 conversion, following ``src/compiler/ir2tran.lisp``.

Each IR1 block becomes an ``IR2Block`` of VOPs. Every lvar delivers its value
through the TN that LTN's annotation designates; every local variable has a
TN of its own; captured variables live in hidden TNs holding their cells or
copied values. Known calls become instances of the template LTN selected;
full calls, references, assignments, branches, casts, and returns become the
fixed templates the backend defines for them.

Conversion runs once per lambda per entry context. A context carries the
facts a body is generated under; its TNs record the derived value type so
code generation can refine representations and skip releases of immortal
values.

Python adaptations (``spec/python-changes.md``):

- A ``Bind`` converts to the entry prologue: parameter moves, closure loads,
  and cell allocation for the lambda's own captured-and-assigned variables.
  SBCL's XEP conventions are code generation's wrappers instead.
- A never-returning known call (``%raise``, ``%reraise``, ``%unwind``) ends
  its block the way SBCL's ``%unwind`` templates do; the error edge to the
  handler block is implicit in the block's cleanup.
- A resolved method delivers two locations: the method and the receiver.
"""

from __future__ import annotations

from dataclasses import dataclass, replace
from typing import TYPE_CHECKING

from mule import ast

from . import fndb, vops

if TYPE_CHECKING:
    from .annotation_types import Contract
from .ctype import (
    BOOLEAN,
    BOOLEAN_FALSE,
    BOTTOM,
    NONE_TYPE,
    PYTHON_INTEGER,
    REAL,
    TEXT_TYPE,
    UNKNOWN,
    IntegerType,
    NoneType,
    TupleType,
    ValueType,
)
from .ir1util import block_exception_target, made_lambda
from .ltn import EntryContext, primitive_type_of, template_results_ok
from .node import (
    Bind,
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
    NodeVisitor,
    Ref,
    Return,
    Set,
    linked,
)
from .seqtran import (
    _PREDICTABLE_RANGE_CALLEE_REF,
    _RangeIteratorRef,
    is_predictable_len_call,
    known_length_primitive,
)
from .vmdef import templates_for
from .vop import (
    BOOLEAN_T,
    FIXNUM,
    RANGE_T,
    REFERENCE_T,
    VOP,
    IR2Block,
    IR2Component,
    IR2Environment,
    IR2LvarKind,
    T,
    TNRef,
)


@dataclass(frozen=True, slots=True)
class LocalAccess:
    """Code generation data of a local read: the name for the unbound-local
    error, whether the read is the last use (ownership moves), whether
    the variable is proven bound, and whether the consumer borrows the slot."""

    name: str
    move: bool
    bound: bool = False
    borrow: bool = False
    allow_unbound: bool = False


_BORROWING_VOPS = frozenset(
    {
        "getattr",
        "getattr-default",
        "hasattr",
        "isinstance",
        "setattr",
        "resolve-method",
        "resolve-super-method",
        "branch",
        "store-global",
        "build-list",
        "unpack-ref",
        "has-binding",
        "class-store",
        "class-delete",
        "enter-handler",
        "assertion-failure",
        "unary-not",
        "compare-equal",
        "compare-not_equal",
        "compare-less",
        "compare-less_equal",
        "compare-greater",
        "compare-greater_equal",
        "compare-is",
        "compare-is_not",
        "binary-matrix_multiply",
        "binary-divide",
        "binary-floor_divide",
        "binary-power",
        "binary-left_shift",
        "binary-right_shift",
        "binary-bit_and",
        "binary-bit_xor",
        "binary-bit_or",
        "inplace-divide",
        "inplace-floor_divide",
        "inplace-modulo",
        "inplace-power",
        "inplace-left_shift",
        "inplace-right_shift",
        "inplace-bit_and",
        "inplace-bit_xor",
        "inplace-bit_or",
        "sequence-available",
        "protocol-iterate",
        "protocol-getitem",
        "protocol-getslice",
        "protocol-setitem",
        "fast-len",
        "text-slice-compare",
        "text-slice2-compare",
        "parse-int-text",
        "parse-int-slice",
    }
)
_RIGHT_BORROWING_VOPS = frozenset(
    {
        "binary-add",
        "binary-subtract",
        "binary-multiply",
        "inplace-add",
        "inplace-subtract",
        "inplace-multiply",
    }
)
_PURE_FIXNUM_VOPS = frozenset(
    {
        "binary-add",
        "binary-subtract",
        "binary-multiply",
        "binary-floor_divide",
        "binary-modulo",
        "binary-bit_and",
        "binary-bit_or",
        "binary-bit_xor",
        "binary-left_shift",
        "binary-right_shift",
        "inplace-add",
        "inplace-subtract",
        "inplace-multiply",
        "inplace-floor_divide",
        "inplace-modulo",
        "inplace-bit_and",
        "inplace-bit_or",
        "inplace-bit_xor",
        "inplace-left_shift",
        "inplace-right_shift",
    }
)


_NON_BORROWING_PRIMITIVES = frozenset(
    {
        "primitive-_string_protocol",
        "primitive-_decimal_protocol",
        "primitive-_percent_decimal_protocol",
        "primitive-_format_value_protocol",
        "primitive-_representation_protocol",
        "primitive-_hex_protocol",
        "primitive-_initialize",
        "primitive-_read_int64",
        "primitive-_write_int64",
    }
)


class _BorrowableConstant(ast.Visitor):
    def visit_Node(self, node):
        return False

    def visit_TextLiteral(self, node):
        return True

    def visit_BytesLiteral(self, node):
        return True

    def visit_FloatLiteral(self, node):
        return True

    def visit_IntegerLiteral(self, node):
        return True

    def visit_Tuple(self, node):
        return all(element.accept(self) for element in node.elements)


_BORROWABLE_CONSTANT = _BorrowableConstant()


def _borrows_operand(vop: VOP, ref: TNRef, source) -> bool:
    name = vop.info.name
    if name in _BORROWING_VOPS:
        return True
    if (
        name in ("binary-add", "binary-subtract", "binary-multiply")
        and len(vop.args) == 2
        and ref is vop.args[0]
    ):
        lhs_type = ref.tn.type or UNKNOWN
        return lhs_type.is_fixnum or lhs_type.fits_text(TEXT_TYPE)
    if name == "binary-modulo" and len(vop.args) == 2:
        return (vop.args[0].tn.type or UNKNOWN).fits(REAL)
    if name in _RIGHT_BORROWING_VOPS and len(vop.args) == 2 and ref is vop.args[1]:
        info = vop.codegen_info
        return (
            info is None
            or source is None
            or source.leaf is None
            or info.reuse_local != source.leaf.name
        )
    if name in ("compare-in", "compare-not_in") and len(vop.args) == 2:
        return (vop.args[1].tn.type or UNKNOWN).fits_dict()
    if name in ("full-call", "local-extent-call") and vop.args:
        return ref is vop.args[0] or any(
            ref is vop.args[index + 1]
            for index in linked(vop.codegen_info, vop, "call site").borrowable_arguments
        )
    if name == "call-resolved":
        info = vop.codegen_info
        if info is not None and (
            info.exact_text_find
            or info.exact_text_split
            or info.exact_text_partition
            or info.exact_text_affix
            or info.exact_dict_get
            or info.exact_bytearray_append
        ):
            return True
    if name == "range-prepare":
        info = vop.codegen_info
        if info is not None and (info.direct or info.fused):
            return True
    return name.startswith("primitive-") and name not in _NON_BORROWING_PRIMITIVES


def _safe_borrow_step(step: VOP, source) -> bool:
    if source is not None and any(ref.tn is source for ref in step.results):
        return False
    name = step.info.name
    if name in vops.TERMINATORS or name == "yield":
        return False
    if name == "load-local":
        info = step.codegen_info
        return not (
            source is not None
            and step.args[0].tn is source
            and info is not None
            and info.move
        )
    if (
        source is not None
        and source.leaf is not None
        and not source.leaf.indirect
        and not source.leaf.expression_temporary
    ):
        if name.startswith(("inplace-", "boxed-float-")):
            info = step.codegen_info
            if info is not None and info.reuse_local == source.leaf.name:
                return False
        return True
    if name == "constant":
        return True
    if name in _PURE_FIXNUM_VOPS and step.results:
        return (step.results[0].tn.type or UNKNOWN).is_fixnum and all(
            (arg.tn.type or UNKNOWN).is_fixnum for arg in step.args
        )
    return False


@dataclass(frozen=True, slots=True)
class UnpackCheck:
    arity: int
    proved: bool


@dataclass(frozen=True, slots=True)
class UnpackSlot:
    index: int
    proved: bool
    borrow_parent: bool
    borrow_element: bool = False


@dataclass(frozen=True, slots=True)
class CallSite:
    """Code generation data of a full call: the keyword names per argument,
    the source location, and the predicted target when flow analysis found
    one (``GUARDED``), with whether the identity was proven."""

    keywords: tuple
    location: tuple | None
    target: Lambda | None
    proven: bool
    borrowable_arguments: frozenset[int] = frozenset()
    local_environment: bool = False


@dataclass(frozen=True, slots=True)
class MethodResolution:
    name: str
    exact_list_append: bool = False
    exact_list_pop: bool = False
    exact_text_find: bool = False
    exact_text_split: bool = False
    exact_text_partition: bool = False
    exact_text_affix: bool = False
    exact_dict_get: bool = False
    exact_bytearray_append: bool = False

    @property
    def is_exact(self) -> bool:
        return (
            self.exact_list_append
            or self.exact_list_pop
            or self.exact_text_find
            or self.exact_text_split
            or self.exact_text_partition
            or self.exact_text_affix
            or self.exact_dict_get
            or self.exact_bytearray_append
        )


@dataclass(frozen=True, slots=True)
class MethodCall:
    keywords: tuple
    name: str
    exact_list_append: bool = False
    exact_list_pop: bool = False
    exact_text_find: bool = False
    exact_text_split: bool = False
    exact_text_partition: bool = False
    exact_text_affix: bool = False
    exact_dict_get: bool = False
    exact_bytearray_append: bool = False
    # The method the receiver's class resolves the name to, when analysis
    # typed the receiver; code generation guards the resolved function's
    # identity and calls this method's body directly.
    target: Lambda | None = None
    # The receiver's analyzed type, for selecting the target's entry context.
    receiver_type: ValueType = UNKNOWN

    @property
    def is_exact(self) -> bool:
        return (
            self.exact_list_append
            or self.exact_list_pop
            or self.exact_text_find
            or self.exact_text_split
            or self.exact_text_partition
            or self.exact_text_affix
            or self.exact_dict_get
            or self.exact_bytearray_append
        )


@dataclass(frozen=True, slots=True)
class FunctionObject:
    lambda_: Lambda
    info: object


@dataclass(frozen=True, slots=True)
class TypeCheck:
    contract: Contract
    location: tuple | None
    safety: int


@dataclass(frozen=True, slots=True)
class BranchTargets:
    consequent: str
    alternative: str


def ir2_convert(lambda_: Lambda, context: EntryContext) -> IR2Component:
    """Convert ``lambda_`` under ``context`` into VOPs."""
    environment = IR2Environment(lambda_, context)
    component = IR2Component(lambda_, context, environment)
    converter = _Converter(component)
    converter.prepare_environment()
    blocks = list(linked(lambda_.component, lambda_, "component").blocks())
    for index, block in enumerate(blocks):
        ir2_block = IR2Block(block, index)
        component.ir2_blocks[block] = ir2_block
        component.blocks.append(ir2_block)
    for block in blocks:
        converter.convert_block(block)
    converter.close()
    borrowed_names = {lambda_.vars[index].name for index in context.borrowed_parameters}
    for block in component.blocks:
        for vop in block.vops:
            if (
                vop.info.reuses_local
                and vop.codegen_info is not None
                and vop.codegen_info.reuse_local in borrowed_names
            ):
                vop.codegen_info = replace(vop.codegen_info, reuse_local=None)
    _fuse_text_codepoint_comparisons(component)
    _mark_last_local_reads(component)
    if _propagate_local_aliases(component):
        _mark_last_local_reads(component)
    _fuse_text_slice_compares(component)
    _mark_boolean_comparisons(component)
    _mark_borrowed_local_reads(component)
    _specialize_list_getitems(component)
    _clear_lvar_info(lambda_)
    return component


_CODEPOINT_TYPE = IntegerType(0, 0x10FFFF)
_CODEPOINT_COMPARISON_VOPS = frozenset(
    {
        "compare-equal",
        "compare-not_equal",
        "compare-less",
        "compare-less_equal",
        "compare-greater",
        "compare-greater_equal",
    }
)


class _SingleCharCodepoint(ast.Visitor):
    """The Unicode codepoint of a 1-character ``ast.TextLiteral``, or None."""

    def visit_Node(self, node):
        return None

    def visit_TextLiteral(self, node: ast.TextLiteral) -> int | None:
        return ord(node.value) if len(node.value) == 1 else None


_SINGLE_CHAR_CODEPOINT = _SingleCharCodepoint()


def _single_char_constant_vop(tn, reads, writes) -> tuple[VOP, int] | None:
    tn_writes = writes.get(tn)
    tn_reads = reads.get(tn)
    if (
        tn_writes is None
        or len(tn_writes) != 1
        or tn_reads is None
        or len(tn_reads) != 1
    ):
        return None
    writer, _ = tn_writes[0]
    if writer.info is not vops.CONSTANT:
        return None
    code = writer.codegen_info.accept(_SINGLE_CHAR_CODEPOINT)
    if code is None:
        return None
    return writer, code


def _codepoint_comparison_constant(
    reader_vop: VOP, reader_ref: TNRef, reads, writes
) -> tuple[VOP, int] | None:
    if (
        reader_vop.info.name not in _CODEPOINT_COMPARISON_VOPS
        or len(reader_vop.args) != 2
    ):
        return None
    other_ref = (
        reader_vop.args[1] if reader_vop.args[0] is reader_ref else reader_vop.args[0]
    )
    if other_ref.tn is reader_ref.tn:
        return None
    return _single_char_constant_vop(other_ref.tn, reads, writes)


def _fuse_text_codepoint_comparisons(component: IR2Component) -> None:
    """Fuse ``text[i]`` with 1-character literal comparisons into codepoint
    extraction and fixnum comparisons when ``text`` is proven ``str``, ``i`` is
    a Python integer, and the indexed character never escapes to any
    non-comparison use."""
    if component.context.plan is not None:
        return
    reads = component.reads
    writes = component.writes
    getitem_vops: set[VOP] = set()
    for block in component.blocks:
        for vop in block.vops:
            if vop.info.name != "protocol-getitem":
                continue
            seq_type = vop.args[0].tn.type or UNKNOWN
            idx_type = vop.args[1].tn.type or UNKNOWN
            res_tn = vop.results[0].tn
            if (
                seq_type.fits_text(TEXT_TYPE)
                and idx_type.fits_python_integer(PYTHON_INTEGER)
                and len(writes.get(res_tn, ())) == 1
                and reads.get(res_tn)
            ):
                getitem_vops.add(vop)
    if not getitem_vops:
        return

    local_tns = {
        reader.results[0].tn
        for vop in getitem_vops
        for reader, _ in reads.get(vop.results[0].tn, ())
        if reader.info is vops.MOVE
    }

    def valid_value_reads(tn) -> bool:
        tn_reads = reads.get(tn)
        if not tn_reads:
            return False
        has_active_use = False
        for reader, ref in tn_reads:
            if reader.info is vops.DISCARD:
                continue
            if reader.info is vops.MOVE and reader.results[0].tn in local_tns:
                has_active_use = True
                continue
            if _codepoint_comparison_constant(reader, ref, reads, writes) is not None:
                has_active_use = True
                continue
            return False
        return has_active_use

    changed = True
    while changed:
        changed = False
        getitem_result_tns = {vop.results[0].tn for vop in getitem_vops}
        for local_tn in list(local_tns):
            tn_writes = writes.get(local_tn)
            tn_reads = reads.get(local_tn)
            if (
                not tn_writes
                or not tn_reads
                or any(
                    writer.info is not vops.MOVE
                    or writer.args[0].tn not in getitem_result_tns
                    for writer, _ in tn_writes
                )
            ):
                local_tns.remove(local_tn)
                changed = True
                continue
            has_comparison = False
            valid_local = True
            for reader, _ in tn_reads:
                if (
                    reader.info is not vops.LOAD_LOCAL
                    or reader.codegen_info is None
                    or not reader.codegen_info.bound
                ):
                    valid_local = False
                    break
                load_res = reader.results[0].tn
                if len(writes.get(load_res, ())) != 1:
                    valid_local = False
                    break
                load_reads = reads.get(load_res)
                if not load_reads:
                    valid_local = False
                    break
                for consumer, cref in load_reads:
                    if consumer.info is vops.DISCARD:
                        continue
                    if (
                        _codepoint_comparison_constant(consumer, cref, reads, writes)
                        is not None
                    ):
                        has_comparison = True
                        continue
                    valid_local = False
                    break
                if not valid_local:
                    break
            if not valid_local or not has_comparison:
                local_tns.remove(local_tn)
                changed = True
        for vop in list(getitem_vops):
            if not valid_value_reads(vop.results[0].tn):
                getitem_vops.remove(vop)
                changed = True

    if not getitem_vops:
        return

    value_tns = [vop.results[0].tn for vop in getitem_vops]
    for vop in getitem_vops:
        vop.codegen_info = True
        res_tn = vop.results[0].tn
        res_tn.primitive_type = FIXNUM
        res_tn.type = _CODEPOINT_TYPE

    for local_tn in local_tns:
        local_tn.primitive_type = FIXNUM
        local_tn.type = _CODEPOINT_TYPE
        for writer, _ in writes.get(local_tn, ()):
            writer.codegen_info = False
        for reader, _ in reads.get(local_tn, ()):
            load_res = reader.results[0].tn
            load_res.primitive_type = FIXNUM
            load_res.type = _CODEPOINT_TYPE
            value_tns.append(load_res)

    for tn in value_tns:
        for reader, ref in reads.get(tn, ()):
            constant_match = _codepoint_comparison_constant(reader, ref, reads, writes)
            if constant_match is None:
                continue
            const_vop, code = constant_match
            literal = linked(const_vop.codegen_info, const_vop, "constant literal")
            const_vop.codegen_info = ast.IntegerLiteral(code, span=literal.span)
            const_tn = const_vop.results[0].tn
            const_tn.primitive_type = FIXNUM
            const_tn.type = IntegerType(code, code)


def _is_valid_text_slice_getslice(vop: VOP) -> bool:
    if vop.info.name != "protocol-getslice" or len(vop.args) != 4 or not vop.results:
        return False
    seq_type = vop.args[0].tn.type or UNKNOWN
    start_type = vop.args[1].tn.type or UNKNOWN
    stop_type = vop.args[2].tn.type or UNKNOWN
    step_type = vop.args[3].tn.type or UNKNOWN
    return (
        seq_type.fits_text(seq_type)
        and (
            start_type.fits_python_integer(start_type)
            or start_type.fits_none(start_type)
        )
        and (stop_type.fits_python_integer(stop_type) or stop_type.fits_none(stop_type))
        and (step_type.fits_none(step_type) or step_type.exact_integer() == 1)
    )


def _safe_slice_compare_intervening_step(vop: VOP) -> bool:
    name = vop.info.name
    if name in ("constant", "move"):
        return True
    if name == "type-check":
        check: TypeCheck = linked(vop.codegen_info, vop, "type-check info")
        actual = vop.args[0].tn.type or UNKNOWN
        return not check.safety or actual.fits(check.contract.fact)
    if name == "load-local":
        access: LocalAccess = linked(vop.codegen_info, vop, "local access")
        return bool(access.bound)
    if name in _PURE_FIXNUM_VOPS and vop.results:
        return (vop.results[0].tn.type or UNKNOWN).is_fixnum and all(
            (arg.tn.type or UNKNOWN).is_fixnum for arg in vop.args
        )
    return False


def _remove_vop_from_component(component: IR2Component, vop: VOP) -> None:
    reads = component.reads
    writes = component.writes
    for ref in vop.args:
        ref_list = reads.get(ref.tn)
        if ref_list is not None and (vop, ref) in ref_list:
            ref_list.remove((vop, ref))
    for ref in vop.results:
        ref_list = writes.get(ref.tn)
        if ref_list is not None and (vop, ref) in ref_list:
            ref_list.remove((vop, ref))


def _slice_step_writer(
    slice_vop: VOP, reads, writes, positions: dict[VOP, int]
) -> VOP | None:
    step_tn = slice_vop.args[3].tn
    step_writes = writes.get(step_tn)
    step_reads = reads.get(step_tn)
    if (
        step_writes is None
        or len(step_writes) != 1
        or step_reads is None
        or len(step_reads) != 1
    ):
        return None
    writer = step_writes[0][0]
    if writer.info is not vops.CONSTANT or writer not in positions:
        return None
    return writer


def _trace_slice_to_compare(
    slice_index: int,
    slice_vop: VOP,
    reads,
    writes,
    positions: dict[VOP, int],
    local_candidates: set,
) -> tuple[VOP | None, VOP | None, VOP | None, TNRef | None]:
    slice_res = slice_vop.results[0].tn
    slice_res_writes = writes.get(slice_res)
    slice_res_reads = reads.get(slice_res)
    if (
        slice_res_writes is None
        or len(slice_res_writes) != 1
        or slice_res_reads is None
        or len(slice_res_reads) != 1
    ):
        return None, None, None, None
    consumer, consumer_ref = slice_res_reads[0]
    if consumer.info.name in ("compare-equal", "compare-not_equal"):
        if consumer in positions and positions[consumer] > slice_index:
            return None, None, consumer, consumer_ref
    elif (
        consumer.info is vops.MOVE
        and consumer in positions
        and positions[consumer] == slice_index + 1
    ):
        local_tn = consumer.results[0].tn
        local_writes = writes.get(local_tn)
        local_reads = reads.get(local_tn)
        if (
            local_tn in local_candidates
            and local_writes is not None
            and len(local_writes) == 1
            and local_reads is not None
            and len(local_reads) == 1
        ):
            reader, _ = local_reads[0]
            if (
                reader.info is vops.LOAD_LOCAL
                and reader.codegen_info is not None
                and reader.codegen_info.move
                and reader.codegen_info.bound
                and reader in positions
                and positions[reader] > positions[consumer]
            ):
                loaded_tn = reader.results[0].tn
                loaded_writes = writes.get(loaded_tn)
                loaded_reads = reads.get(loaded_tn)
                if (
                    loaded_writes is not None
                    and len(loaded_writes) == 1
                    and loaded_reads is not None
                    and len(loaded_reads) == 1
                ):
                    cmp_candidate, cmp_ref = loaded_reads[0]
                    if (
                        cmp_candidate.info.name
                        in ("compare-equal", "compare-not_equal")
                        and cmp_candidate in positions
                        and positions[cmp_candidate] > positions[reader]
                    ):
                        return consumer, reader, cmp_candidate, cmp_ref
    return None, None, None, None


def _trace_operand_to_slice(
    ref: TNRef,
    compare_vop: VOP,
    reads,
    writes,
    positions: dict[VOP, int],
    removed: set[VOP],
    local_candidates: set,
) -> tuple[VOP, VOP, VOP | None, VOP | None] | None:
    tn = ref.tn
    tn_writes = writes.get(tn)
    tn_reads = reads.get(tn)
    if (
        tn_writes is None
        or len(tn_writes) != 1
        or tn_reads is None
        or len(tn_reads) != 1
        or tn_reads[0][0] is not compare_vop
    ):
        return None
    writer = tn_writes[0][0]
    if writer in removed or writer not in positions:
        return None
    if _is_valid_text_slice_getslice(writer):
        step_w = _slice_step_writer(writer, reads, writes, positions)
        if step_w is not None and step_w not in removed:
            return writer, step_w, None, None
        return None
    if (
        writer.info is vops.LOAD_LOCAL
        and writer.codegen_info is not None
        and writer.codegen_info.move
        and writer.codegen_info.bound
    ):
        local_tn = writer.args[0].tn
        local_writes = writes.get(local_tn)
        local_reads = reads.get(local_tn)
        if (
            local_tn in local_candidates
            and local_writes is not None
            and len(local_writes) == 1
            and local_reads is not None
            and len(local_reads) == 1
        ):
            move_w = local_writes[0][0]
            if (
                move_w.info is vops.MOVE
                and move_w not in removed
                and move_w in positions
                and positions[move_w] < positions[writer]
            ):
                src_tn = move_w.args[0].tn
                src_writes = writes.get(src_tn)
                src_reads = reads.get(src_tn)
                if (
                    src_writes is not None
                    and len(src_writes) == 1
                    and src_reads is not None
                    and len(src_reads) == 1
                ):
                    slice_w = src_writes[0][0]
                    if (
                        slice_w not in removed
                        and slice_w in positions
                        and positions[slice_w] + 1 == positions[move_w]
                        and _is_valid_text_slice_getslice(slice_w)
                    ):
                        step_w = _slice_step_writer(slice_w, reads, writes, positions)
                        if step_w is not None and step_w not in removed:
                            return slice_w, step_w, move_w, writer
    return None


def _fuse_text_slice_compares(component: IR2Component) -> None:
    """Fuse ``text[start:stop] == target`` and
    ``left[start1:stop1] == right[start2:stop2]`` (and ``!=``) into an in-place
    ``text-slice-compare`` or ``text-slice2-compare`` VOP when the sliced
    sequences and target are proven ``str`` and the substring slices are not
    read anywhere else."""
    reads = component.reads
    writes = component.writes
    local_candidates = {
        tn
        for var in component.lambda_.locals
        if not var.indirect
        and not var.expression_temporary
        and (tn := component.environment.vars.get(var)) is not None
    }
    for block in component.blocks:
        vop_list = block.vops
        positions = {step: pos for pos, step in enumerate(vop_list)}
        removed: set[VOP] = set()
        replacements: dict[VOP, list[VOP]] = {}

        for slice_index, slice_vop in enumerate(vop_list):
            if slice_vop in removed or not _is_valid_text_slice_getslice(slice_vop):
                continue
            step_writer = _slice_step_writer(slice_vop, reads, writes, positions)
            if step_writer is None:
                continue
            move_vop, load_vop, compare_vop, compare_ref = _trace_slice_to_compare(
                slice_index,
                slice_vop,
                reads,
                writes,
                positions,
                local_candidates,
            )
            if compare_vop is None or compare_ref is None or compare_vop in removed:
                continue
            compare_index = positions[compare_vop]
            if len(compare_vop.args) != 2 or not compare_vop.results:
                continue
            other_ref = (
                compare_vop.args[1]
                if compare_ref is compare_vop.args[0]
                else compare_vop.args[0]
            )
            other_type = other_ref.tn.type or UNKNOWN
            if not other_type.fits_text(other_type):
                continue

            slice_operand_tns = {
                slice_vop.args[0].tn,
                slice_vop.args[1].tn,
                slice_vop.args[2].tn,
            }
            if other_ref.tn in slice_operand_tns:
                continue
            if any(
                len(writes.get(tn, ())) != 1 or len(reads.get(tn, ())) != 1
                for tn in slice_operand_tns
            ):
                continue

            other_slice_info = _trace_operand_to_slice(
                other_ref,
                compare_vop,
                reads,
                writes,
                positions,
                removed,
                local_candidates,
            )
            if other_slice_info is not None:
                other_slice, other_step, other_move, other_load = other_slice_info
                other_slice_index = positions[other_slice]
                if other_slice is not slice_vop and other_slice_index > slice_index:
                    other_operand_tns = {
                        other_slice.args[0].tn,
                        other_slice.args[1].tn,
                        other_slice.args[2].tn,
                    }
                    if not (slice_operand_tns & other_operand_tns) and all(
                        len(writes.get(tn, ())) == 1 and len(reads.get(tn, ())) == 1
                        for tn in other_operand_tns
                    ):
                        allowed_steps = {
                            move_vop,
                            load_vop,
                            other_step,
                            other_slice,
                            other_move,
                            other_load,
                        }
                        intervening_two_ok = True
                        for pos in range(slice_index + 1, compare_index):
                            step = vop_list[pos]
                            if step in allowed_steps:
                                continue
                            if (
                                step in removed
                                or not _safe_slice_compare_intervening_step(step)
                            ):
                                intervening_two_ok = False
                                break
                            step_tns = {ref.tn for ref in (*step.args, *step.results)}
                            if step_tns & slice_operand_tns:
                                intervening_two_ok = False
                                break
                            if pos > other_slice_index and (
                                step_tns & other_operand_tns
                            ):
                                intervening_two_ok = False
                                break
                        if intervening_two_ok:
                            invert = compare_vop.info.name == "compare-not_equal"
                            for dead_vop in (
                                step_writer,
                                slice_vop,
                                move_vop,
                                load_vop,
                                other_step,
                                other_slice,
                                other_move,
                                other_load,
                                compare_vop,
                            ):
                                if dead_vop is not None:
                                    _remove_vop_from_component(component, dead_vop)
                                    removed.add(dead_vop)
                            fused_args = [
                                TNRef(slice_vop.args[0].tn, False),
                                TNRef(slice_vop.args[1].tn, False),
                                TNRef(slice_vop.args[2].tn, False),
                                TNRef(other_slice.args[0].tn, False),
                                TNRef(other_slice.args[1].tn, False),
                                TNRef(other_slice.args[2].tn, False),
                            ]
                            fused_results = [TNRef(compare_vop.results[0].tn, True)]
                            fused_vop = VOP(
                                vops.TEXT_SLICE2_COMPARE,
                                compare_vop.node,
                                fused_args,
                                fused_results,
                            )
                            fused_vop.codegen_info = invert
                            for ref in fused_args:
                                reads[ref.tn].append((fused_vop, ref))
                            for ref in fused_results:
                                writes[ref.tn].append((fused_vop, ref))
                            replacements[compare_vop] = [fused_vop]
                            continue

            intervening_ok = True
            for step in vop_list[slice_index + 1 : compare_index]:
                if step in (move_vop, load_vop):
                    continue
                if step in removed or not _safe_slice_compare_intervening_step(step):
                    intervening_ok = False
                    break
                if any(
                    ref.tn in slice_operand_tns for ref in (*step.args, *step.results)
                ):
                    intervening_ok = False
                    break
            if not intervening_ok:
                continue

            other_writes = writes.get(other_ref.tn)
            other_reads = reads.get(other_ref.tn)
            hoist_constant: VOP | None = None
            place_at_slice = False
            if (
                other_writes is not None
                and len(other_writes) == 1
                and other_reads is not None
                and other_reads[0][0] is compare_vop
                and len(other_reads) == 1
                and other_writes[0][0] in positions
            ):
                other_writer = other_writes[0][0]
                writer_pos = positions[other_writer]
                if writer_pos < slice_index:
                    place_at_slice = True
                elif other_writer.info is vops.CONSTANT and writer_pos < compare_index:
                    hoist_constant = other_writer
                    place_at_slice = True

            invert = compare_vop.info.name == "compare-not_equal"
            for dead_vop in (step_writer, slice_vop, move_vop, load_vop, compare_vop):
                if dead_vop is not None:
                    _remove_vop_from_component(component, dead_vop)
                    removed.add(dead_vop)
            if hoist_constant is not None:
                removed.add(hoist_constant)

            fused_args = [
                TNRef(slice_vop.args[0].tn, False),
                TNRef(slice_vop.args[1].tn, False),
                TNRef(slice_vop.args[2].tn, False),
                TNRef(other_ref.tn, False),
            ]
            fused_results = [TNRef(compare_vop.results[0].tn, True)]
            fused_vop = VOP(
                vops.TEXT_SLICE_COMPARE,
                compare_vop.node,
                fused_args,
                fused_results,
            )
            fused_vop.codegen_info = invert
            for ref in fused_args:
                reads[ref.tn].append((fused_vop, ref))
            for ref in fused_results:
                writes[ref.tn].append((fused_vop, ref))
            if place_at_slice:
                replacements[slice_vop] = (
                    [hoist_constant, fused_vop]
                    if hoist_constant is not None
                    else [fused_vop]
                )
            else:
                replacements[compare_vop] = [fused_vop]

        if removed:
            dead_tns = {
                ref.tn for step in removed for ref in (*step.args, *step.results)
            }
            new_vops: list[VOP] = []
            for step in vop_list:
                if step in replacements:
                    new_vops.extend(replacements[step])
                elif step not in removed:
                    new_vops.append(step)
            block.vops = new_vops
            component.tns = [
                tn
                for tn in component.tns
                if tn not in dead_tns or reads.get(tn) or writes.get(tn)
            ]


def _mark_boolean_comparisons(component: IR2Component) -> None:
    """Mark rich-comparison VOPs whose result is only tested for truthiness so
    the slow path coerces custom comparison results to ``bool`` once."""
    if component.context.plan is not None:
        return
    reads = component.reads
    writes = component.writes
    for block in component.blocks:
        for vop in block.vops:
            if vop.info.name not in _CODEPOINT_COMPARISON_VOPS or not vop.results:
                continue
            res_tn = vop.results[0].tn
            if res_tn.leaf is not None:
                continue
            res_writes = writes.get(res_tn)
            res_reads = reads.get(res_tn)
            if (
                res_writes is None
                or len(res_writes) != 1
                or not res_reads
                or not all(
                    reader.info is vops.BRANCH or reader.info.name == "unary-not"
                    for reader, _ in res_reads
                )
            ):
                continue
            vop.codegen_info = True
            res_tn.type = BOOLEAN
            res_tn.primitive_type = BOOLEAN_T


def _propagate_local_aliases(component: IR2Component) -> bool:
    """Substitute stable, uncaptured local copies before ownership selection.

    A local copy has the same value as its source, but may keep that value
    alive independently. The lifetime proof must cover its consumers and every
    reachable owner transfer, not just the textual assignment and last read.
    """
    if component.context.plan is not None:
        return False
    changed = False
    while True:
        proof = _BorrowedLifetime(component)
        removed = set()
        for block in component.blocks:
            for acquisition in block.vops:
                if acquisition.info is not vops.LOAD_LOCAL:
                    continue
                access = acquisition.codegen_info
                if not access.bound:
                    continue
                source = acquisition.args[0].tn
                if not _plain_local(source):
                    continue
                value = acquisition.results[0].tn
                uses = component.reads.get(value, ())
                if len(uses) != 1:
                    continue
                assignment, _ = uses[0]
                if assignment.info is not vops.MOVE:
                    continue
                alias = assignment.results[0].tn
                if (
                    alias is source
                    or not _plain_local(alias)
                    or len(component.writes.get(alias, ())) != 1
                ):
                    continue
                loads = {reader for reader, _ in component.reads.get(alias, ())}
                if not loads or any(
                    reader.info is not vops.LOAD_LOCAL or not reader.codegen_info.bound
                    for reader in loads
                ):
                    continue
                consumers = {assignment, *loads}
                ends = set()
                for load in loads:
                    aliases, uses = proof.consumers(load.results[0].tn, source)
                    if not aliases:
                        break
                    consumers.update(uses)
                    if load.codegen_info.move:
                        readers = component.reads.get(load.results[0].tn, ())
                        if len(readers) == 1 and readers[0][0].info is not vops.MOVE:
                            ends.add(readers[0][0])
                else:
                    if not proof.protected(acquisition, consumers, source):
                        continue
                    if not proof.stable_after(acquisition, consumers, source, ends):
                        continue
                    for load in loads:
                        load.args[0] = TNRef(source, False)
                        if source.borrowed_from is source and load.codegen_info.move:
                            load.codegen_info = replace(load.codegen_info, move=False)
                    removed.update((acquisition, assignment))
                    break
            if removed:
                break
        if not removed:
            return changed
        changed = True
        component.reads.clear()
        component.writes.clear()
        for block in component.blocks:
            block.vops = [vop for vop in block.vops if vop not in removed]
            for vop in block.vops:
                for ref in vop.args:
                    component.reads[ref.tn].append((vop, ref))
                for ref in vop.results:
                    component.writes[ref.tn].append((vop, ref))
        _mark_last_local_reads(component)


def _plain_local(tn) -> bool:
    return (
        tn.leaf is not None
        and not tn.leaf.indirect
        and not tn.leaf.expression_temporary
    )


def _mark_borrowed_local_reads(component: IR2Component) -> None:
    """Elide owned copies whose uses remain protected by a stable local root.

    The CFG region includes exception successors and loop backedges. It is
    deliberately conservative about mutable owner locations and operations
    that can exploit reference-count uniqueness through an alias.
    """
    if component.context.plan is not None:
        return
    proof = _BorrowedLifetime(component)
    reads = component.reads
    writes = component.writes
    for block in component.blocks:
        for vop in block.vops:
            is_load = vop.info is vops.LOAD_LOCAL
            is_const = vop.info is vops.CONSTANT
            is_global = vop.info is vops.LOAD_GLOBAL
            if not is_load and not is_const and not is_global:
                continue
            result = vop.results[0].tn
            result_reads = reads.get(result)
            result_writes = writes.get(result)
            if result_writes is None or len(result_writes) != 1 or result_reads is None:
                continue
            if is_load:
                access: LocalAccess = linked(vop.codegen_info, vop, "local access")
                if not access.bound or access.move:
                    continue
                source = vop.args[0].tn
            elif is_global:
                access = None
                if (
                    result.leaf is not None
                    or result.primitive_type is not T
                    or len(result_reads) != 1
                ):
                    continue
                reader, ref = result_reads[0]
                if (
                    reader.info is not vops.FULL_CALL
                    or not reader.args
                    or ref is not reader.args[0]
                    or not reader.codegen_info.proven
                    or reader.codegen_info.keywords
                ):
                    continue
                source = None
            else:
                access = None
                if (
                    result.leaf is not None
                    or result.primitive_type is not T
                    or vop.codegen_info is None
                    or not vop.codegen_info.accept(_BORROWABLE_CONSTANT)
                ):
                    continue
                source = None
            aliases, consumers = proof.consumers(result, source)
            if not aliases or not proof.protected(vop, consumers, source):
                continue
            root = source if source is not None else result
            for alias in aliases:
                alias.borrowed_from = root
            if access is not None:
                vop.codegen_info = replace(access, borrow=True)


class _BorrowedLifetime:
    """Definition-sensitive borrow regions over IR2's existing control graph."""

    def __init__(self, component):
        self.component = component
        self.positions = {
            vop: (block, index)
            for block in component.blocks
            for index, vop in enumerate(block.vops)
        }
        self.predecessors = {block: set() for block in component.blocks}
        for block in component.blocks:
            for successor in block.block.succ:
                following = component.ir2_blocks.get(successor)
                if following is not None:
                    self.predecessors[following].add((block, len(block.vops)))
            handler = component.ir2_blocks.get(block_exception_target(block.block))
            if handler is not None:
                # An error can leave before the block's normal transfer. Include
                # the throwing operation itself in each conservative predecessor.
                self.predecessors[handler].update(
                    (block, index + 1) for index in range(len(block.vops))
                )

    def consumers(self, result, source):
        aliases, pending, consumers = set(), [result], set()
        while pending:
            tn = pending.pop()
            if tn in aliases:
                continue
            if len(self.component.writes.get(tn, ())) != 1:
                return (), ()
            aliases.add(tn)
            for reader, ref in self.component.reads.get(tn, ()):
                if reader.info is vops.MOVE:
                    target = reader.results[0].tn
                    if target.leaf is not None or target is source:
                        return (), ()
                    pending.append(target)
                    consumers.add(reader)
                    continue
                if not _borrows_operand(reader, ref, source):
                    return (), ()
                consumers.add(reader)
                # Exact method resolution can forward the borrowed receiver
                # without a new owner; its invocation must be in the region.
                if (
                    reader.info.name == "resolve-method"
                    and reader.codegen_info is not None
                    and reader.codegen_info.is_exact
                ):
                    bound = reader.results[1].tn
                    uses = self.component.reads.get(bound, ())
                    if len(uses) != 1:
                        return (), ()
                    pending.append(bound)
                    consumers.add(uses[0][0])
        return aliases, consumers

    def protected(self, acquisition, consumers, source):
        if not consumers:
            return False
        start, start_index = self.positions[acquisition]
        boundary = start, start_index + 1
        local_only = source is not None and (
            source.leaf is None
            or source.leaf.indirect
            or source.leaf.expression_temporary
        )
        pending = []
        for consumer in consumers:
            block, index = self.positions[consumer]
            if block is start and index <= start_index:
                return False
            pending.append((block, index + 1))
        visited = set()
        while pending:
            point = pending.pop()
            if point == boundary or point in visited:
                continue
            visited.add(point)
            block, index = point
            if local_only and block is not start:
                return False
            if index == 0:
                previous = self.predecessors[block]
                if not previous or block is self.component.blocks[0]:
                    return False
                pending.extend(previous)
                continue
            step = block.vops[index - 1]
            if step not in consumers or step.info is not vops.MOVE:
                if not self.safe_step(
                    step, source, consumer=step in consumers
                ) and not (self.transfers_to_consumer(step, source, consumers)):
                    return False
                if (
                    local_only
                    and step not in consumers
                    and not _safe_borrow_step(step, source)
                ):
                    return False
            pending.append((block, index - 1))
        return True

    def transfers_to_consumer(self, step, source, consumers):
        """A moved owner still protects borrows consumed by the same operation."""
        if (
            step.info is not vops.LOAD_LOCAL
            or step.args[0].tn is not source
            or not step.codegen_info.move
            or not step.codegen_info.bound
        ):
            return False
        moved = step.results[0].tn
        if moved.leaf is not None or len(self.component.writes.get(moved, ())) != 1:
            return False
        uses = self.component.reads.get(moved, ())
        if len(uses) != 1:
            return False
        terminal, ref = uses[0]
        if not (
            terminal in consumers
            and terminal.info not in (vops.MOVE, vops.LOAD_LOCAL)
            and _borrows_operand(terminal, ref, source)
        ):
            return False
        block, index = self.positions[step]
        end_block, end = self.positions[terminal]
        if block is not end_block or index >= end:
            return False
        if any(
            not _safe_borrow_step(intermediate, None)
            or not self.copy_safe_step(intermediate)
            for intermediate in block.vops[index + 1 : end]
        ):
            return False
        pending = [(block, end + 1)]
        handler = self.component.ir2_blocks.get(block_exception_target(block.block))
        if handler is not None:
            pending.append((handler, 0))
        visited = set()
        while pending:
            following, position = pending.pop()
            if (following, position) in visited:
                continue
            visited.add((following, position))
            if position == len(following.vops):
                for successor in following.block.succ:
                    target = self.component.ir2_blocks.get(successor)
                    if target is not None:
                        pending.append((target, 0))
                continue
            if following.vops[position] in consumers:
                return False
            handler = self.component.ir2_blocks.get(
                block_exception_target(following.block)
            )
            if handler is not None:
                pending.append((handler, 0))
            pending.append((following, position + 1))
        return True

    def copy_safe_step(self, step):
        """Prove that no implicit error exit can outlive an eliminated copy.

        Stable-root borrowing alone permits throwing operations: the root still
        owns the value on their error paths. Removing an independently owned
        local also changes cleanup order, so it needs a nonthrowing region.
        Selected operations without an established contract remain barriers.
        """
        if step.info is vops.LOAD_LOCAL:
            return step.codegen_info.bound
        if step.info is vops.MOVE:
            return len(self.component.writes.get(step.results[0].tn, ())) == 1
        if step.info is vops.CONSTANT:
            return all(
                not storage.owned
                for result in step.results
                for storage in result.tn.primitive_type.scs
            )
        name = step.info.name
        if name in ("compare-is", "compare-is_not", "jump"):
            return True
        if name == "branch":
            condition = step.args[0].tn.type
            return (
                condition is not None
                and not condition.may_be_unbound
                and not condition.may_run_user_code
            )
        return False

    def stable_after(self, acquisition, consumers, source, ends):
        """Keep the original owner valid on paths without an alias read, too."""
        if not ends:
            return False
        start, index = self.positions[acquisition]
        pending = [(start, index + 1, True)]
        visited = set()
        while pending:
            block, index, live = pending.pop()
            if (block, index, live) in visited:
                continue
            visited.add((block, index, live))
            if not live:
                continue
            if index == len(block.vops):
                if live and not block.block.succ:
                    return False
                for successor in block.block.succ:
                    following = self.component.ir2_blocks.get(successor)
                    if following is not None:
                        pending.append((following, 0, live))
                continue
            step = block.vops[index]
            if (step is acquisition or not self.copy_safe_step(step)) or (
                not self.safe_step(step, source, consumer=step in consumers)
                and not self.transfers_to_consumer(step, source, consumers)
            ):
                return False
            handler = self.component.ir2_blocks.get(block_exception_target(block.block))
            if handler is not None:
                pending.append((handler, 0, live and step not in ends))
            pending.append((block, index + 1, live and step not in ends))
        return True

    @staticmethod
    def safe_step(step, source, *, consumer=False):
        name = step.info.name
        if name == "yield":
            return False
        if source is not None and any(ref.tn is source for ref in step.results):
            return False
        if name == "load-local":
            return not (step.args[0].tn is source and step.codegen_info.move)
        if consumer:
            # Every operand use was checked by _borrows_operand. Arithmetic
            # borrows only an immutable left operand or a nonconsumed right
            # operand; unrelated arithmetic remains a uniqueness barrier.
            return True
        # These can mutate an object selected through another alias. Borrowed
        # temporaries must not make a uniqueness test observe fewer aliases.
        if name.startswith(("inplace-", "boxed-float-")) or name in (
            "binary-add",
            "binary-subtract",
            "binary-multiply",
        ):
            return False
        if name in vops.TERMINATORS:
            return True
        return _safe_borrow_step(step, source)


_BORROWED_LIST_GETITEM_READERS = _CODEPOINT_COMPARISON_VOPS | frozenset(
    {
        "compare-is",
        "compare-is_not",
        "protocol-iterate",
        "fast-len",
        "primitive-_storage_length",
        "primitive-_text_length",
    }
)

_PURE_BINARY_ARITH_VOPS = frozenset(
    {
        "binary-add",
        "binary-subtract",
        "binary-multiply",
        "binary-floor_divide",
        "binary-modulo",
        "binary-bit_and",
        "binary-bit_or",
        "binary-bit_xor",
        "binary-left_shift",
        "binary-right_shift",
        "inplace-add",
        "inplace-subtract",
        "inplace-multiply",
        "inplace-floor_divide",
        "inplace-modulo",
        "inplace-bit_and",
        "inplace-bit_or",
        "inplace-bit_xor",
        "inplace-left_shift",
        "inplace-right_shift",
    }
)


def _can_borrow_list_getitem(reader: VOP, reader_ref: TNRef | None = None) -> bool:
    name = reader.info.name
    if name in _BORROWED_LIST_GETITEM_READERS:
        return True
    if name in _PURE_BINARY_ARITH_VOPS and len(reader.args) == 2:
        return all(
            not (arg.tn.type or UNKNOWN).may_run_user_code for arg in reader.args
        )
    if name == "protocol-getitem" and len(reader.args) == 2:
        seq_type = reader.args[0].tn.type or UNKNOWN
        return (seq_type.is_exact_list or seq_type.is_exact_tuple) and (
            reader.args[1].tn.type or UNKNOWN
        ).is_fixnum
    if name == "primitive-_storage_read" and len(reader.args) == 2:
        return True
    if name == "primitive-_storage_write" and len(reader.args) == 3:
        return reader_ref in reader.args[:2]
    if name == "protocol-setitem" and len(reader.args) == 3:
        return (
            reader_ref in reader.args[:2]
            and (reader.args[0].tn.type or UNKNOWN).is_exact_list
            and (reader.args[1].tn.type or UNKNOWN).is_fixnum
        )
    return False


def _safe_list_borrow_step(step: VOP, seq_local, root_local=None) -> bool:
    if any(ref.tn in (seq_local, root_local) for ref in step.results):
        return False
    name = step.info.name
    if name == "constant":
        return True
    if name == "load-local":
        access: LocalAccess = linked(step.codegen_info, step, "local access")
        return bool(access.bound) and not (
            step.args[0].tn in (seq_local, root_local) and access.move
        )
    if name in _PURE_FIXNUM_VOPS and step.results:
        return (step.results[0].tn.type or UNKNOWN).is_fixnum and all(
            (arg.tn.type or UNKNOWN).is_fixnum for arg in step.args
        )
    if name == "protocol-getitem" and len(step.args) == 2:
        seq_type = step.args[0].tn.type or UNKNOWN
        return (seq_type.is_exact_list or seq_type.is_exact_tuple) and (
            step.args[1].tn.type or UNKNOWN
        ).is_fixnum
    if name == "primitive-_storage_read" and len(step.args) == 2:
        return (step.args[1].tn.type or UNKNOWN).is_fixnum
    return False


def _local_may_run_user_code(tn, component: IR2Component) -> bool:
    if tn.type is not None:
        return tn.type.may_run_user_code
    writers = component.writes.get(tn)
    if not writers:
        return True
    for writer, _ in writers:
        name = writer.info.name
        if name == "unbind-local":
            continue
        if name == "move" and writer.args:
            if (writer.args[0].tn.type or UNKNOWN).may_run_user_code:
                return True
        elif name == "move-argument":
            if component.context.parameters[writer.codegen_info].may_run_user_code:
                return True
        else:
            return True
    return False


def _has_local_aggregate_origin(tn, component: IR2Component) -> bool:
    writes = component.writes
    escape_facts = component.context.escape_facts
    pending = [tn]
    seen = set()
    while pending:
        cur = pending.pop()
        if cur in seen:
            continue
        seen.add(cur)
        for writer, _ in writes.get(cur, ()):
            if (
                writer.info.name in ("build-list", "build-tuple")
                and len(writer.args) > 0
                and not escape_facts.allocation_escapes(writer.node)
            ):
                return True
            if writer.info in (vops.MOVE, vops.LOAD_LOCAL) and writer.args:
                pending.append(writer.args[0].tn)
    return False


def _try_borrow_unpacked_getitem(
    component: IR2Component,
    block: IR2Block,
    index: int,
    vop: VOP,
    borrow_info: str,
    seq_local,
    root_local,
    writer_index: int,
) -> bool:
    reads = component.reads
    writes = component.writes
    if _has_local_aggregate_origin(root_local, component):
        return False
    vops_list = block.vops
    n_vops = len(vops_list)
    if index + 3 >= n_vops:
        return False
    if any(
        not _safe_list_borrow_step(step, seq_local, root_local)
        for step in vops_list[writer_index + 1 : index]
    ):
        return False
    count_vop = vops_list[index + 1]
    unpack_vop = vops_list[index + 2]
    move_vop = vops_list[index + 3]
    if (
        count_vop.info is not vops.CONSTANT
        or unpack_vop.info.name != "structural-unpack"
        or not unpack_vop.codegen_info.proved
        or move_vop.info is not vops.MOVE
    ):
        return False
    arity = unpack_vop.codegen_info.arity
    if arity is None or arity < 1:
        return False
    res_tn = vop.results[0].tn
    count_tn = count_vop.results[0].tn
    unpack_res_tn = unpack_vop.results[0].tn
    temp_tn = move_vop.results[0].tn
    if (
        unpack_vop.args[0].tn is not res_tn
        or unpack_vop.args[1].tn is not count_tn
        or move_vop.args[0].tn is not unpack_res_tn
        or len(writes.get(count_tn, ())) != 1
        or len(reads.get(count_tn, ())) != 1
        or len(writes.get(unpack_res_tn, ())) != 1
        or len(reads.get(unpack_res_tn, ())) != 1
        or temp_tn.leaf is None
        or not temp_tn.leaf.expression_temporary
        or len(writes.get(temp_tn, ())) != 1
        or len(reads.get(temp_tn, ())) != arity + 1
    ):
        return False
    slot_records = []
    cursor = index + 4
    for slot_idx in range(arity):
        if cursor + 2 >= n_vops:
            return False
        pos_vop = vops_list[cursor]
        ref_vop = vops_list[cursor + 1]
        if (
            pos_vop.info is not vops.CONSTANT
            or ref_vop.info.name != "unpack-ref"
            or not ref_vop.codegen_info.proved
            or not ref_vop.codegen_info.borrow_parent
            or ref_vop.codegen_info.index != slot_idx
            or ref_vop.args[0].tn is not temp_tn
            or ref_vop.args[1].tn is not pos_vop.results[0].tn
            or len(writes.get(pos_vop.results[0].tn, ())) != 1
            or len(reads.get(pos_vop.results[0].tn, ())) != 1
        ):
            return False
        orig_ref_res_tn = ref_vop.results[0].tn
        ref_res_tn = orig_ref_res_tn
        if len(writes.get(ref_res_tn, ())) != 1 or len(reads.get(ref_res_tn, ())) != 1:
            return False
        cursor += 2
        next_vop = vops_list[cursor]
        checked_res_tn = None
        if next_vop.info is vops.TYPE_CHECK:
            check = next_vop.codegen_info
            actual = next_vop.args[0].tn.type or UNKNOWN
            if (
                next_vop.args[0].tn is not ref_res_tn
                or (check.safety and not actual.fits(check.contract.checked_type))
                or cursor + 1 >= n_vops
            ):
                return False
            ref_res_tn = next_vop.results[0].tn
            checked_res_tn = ref_res_tn
            if (
                len(writes.get(ref_res_tn, ())) != 1
                or len(reads.get(ref_res_tn, ())) != 1
            ):
                return False
            cursor += 1
        store_vop = vops_list[cursor]
        slot_target = store_vop.results[0].tn if store_vop.results else None
        if (
            store_vop.info is not vops.MOVE
            or store_vop.args[0].tn is not ref_res_tn
            or slot_target in (None, temp_tn, seq_local, root_local)
            or (
                store_vop.codegen_info is not False
                and _local_may_run_user_code(slot_target, component)
            )
        ):
            return False
        slot_records.append(
            (ref_vop, orig_ref_res_tn, checked_res_tn, store_vop, cursor)
        )
        cursor += 1
    if cursor + 1 >= n_vops:
        return False
    discard_load_vop = vops_list[cursor]
    discard_vop = vops_list[cursor + 1]
    if (
        discard_load_vop.info is not vops.LOAD_LOCAL
        or discard_load_vop.args[0].tn is not temp_tn
        or not discard_load_vop.codegen_info.move
        or discard_vop.info is not vops.DISCARD
    ):
        return False
    discard_tn = discard_load_vop.results[0].tn
    if (
        discard_vop.args[0].tn is not discard_tn
        or len(writes.get(discard_tn, ())) != 1
        or len(reads.get(discard_tn, ())) != 1
    ):
        return False
    vop.codegen_info = borrow_info
    res_tn.borrowed_from = root_local
    unpack_res_tn.borrowed_from = root_local
    temp_tn.borrowed_from = root_local
    move_vop.codegen_info = False
    discard_load_vop.codegen_info = replace(
        discard_load_vop.codegen_info, move=False, borrow=True
    )
    discard_tn.borrowed_from = root_local
    proof = _BorrowedLifetime(component)
    for (
        ref_vop,
        _orig_ref_res_tn,
        checked_res_tn,
        store_vop,
        store_pos,
    ) in slot_records:
        if (
            _try_borrow_container_element_local(
                component,
                proof,
                block,
                index,
                ref_vop,
                replace(ref_vop.codegen_info, borrow_element=True),
                seq_local,
                root_local,
                writer_index,
                store_vop,
                move_index=store_pos,
                allow_unused=True,
            )
            and checked_res_tn is not None
        ):
            checked_res_tn.borrowed_from = root_local
    return True


def _safe_container_local_region_step(
    component: IR2Component,
    step: VOP,
    local_tn,
    seq_local,
    root_local,
    elem_type: ValueType,
    *,
    is_consumer: bool = False,
    from_storage_read: bool = False,
) -> bool:
    for ref in step.results:
        if ref.tn in (local_tn, seq_local, root_local):
            return False
    name = step.info.name
    if name in ("constant", "jump", "insert-safepoint", "discard"):
        return True
    if name == "unpack-ref":
        return step.codegen_info is not None and bool(step.codegen_info.proved)
    if name == "type-check":
        check = step.codegen_info
        return check is not None and (
            not check.safety
            or (step.args[0].tn.type or UNKNOWN).fits(check.contract.checked_type)
        )
    if name == "branch":
        cond_type = (step.args[0].tn.type or UNKNOWN) if step.args else UNKNOWN
        return not cond_type.may_be_unbound and not cond_type.may_run_user_code
    if name == "load-local":
        access: LocalAccess = linked(step.codegen_info, step, "local access")
        return bool(access.bound) and not (
            step.args[0].tn in (seq_local, root_local) and access.move
        )
    if name == "move":
        target = step.results[0].tn
        return (
            step.codegen_info is False
            or target.borrowed_from is not None
            or not _local_may_run_user_code(target, component)
        )
    if name in (
        "compare-is",
        "compare-is_not",
        "fast-len",
        "primitive-_storage_length",
        "primitive-_text_length",
        "range-prepare",
        "range-available",
        "range-advance",
    ):
        return True
    if name in _CODEPOINT_COMPARISON_VOPS:
        return (
            is_consumer
            or from_storage_read
            or all(not (arg.tn.type or UNKNOWN).may_run_user_code for arg in step.args)
        )
    if name in _PURE_BINARY_ARITH_VOPS and len(step.args) == 2:
        return all(
            not (arg.tn.type or UNKNOWN).may_run_user_code
            and (
                not name.startswith("inplace-")
                or not (arg.tn.type or UNKNOWN).is_exact_list
            )
            for arg in step.args
        )
    if name == "protocol-getitem" and len(step.args) == 2:
        seq_t = step.args[0].tn.type or UNKNOWN
        return (seq_t.is_exact_list or seq_t.is_exact_tuple) and (
            step.args[1].tn.type or UNKNOWN
        ).is_fixnum
    if name == "primitive-_storage_read" and len(step.args) == 2:
        return (step.args[1].tn.type or UNKNOWN).is_fixnum
    if name == "primitive-_storage_write" and len(step.args) == 3:
        return is_consumer
    if name == "protocol-setitem" and len(step.args) == 3:
        seq_t = step.args[0].tn.type or UNKNOWN
        idx_t = step.args[1].tn.type or UNKNOWN
        item_t = step.args[2].tn.type or UNKNOWN
        if not (
            seq_t.is_exact_list
            and idx_t.is_fixnum
            and not item_t.may_run_user_code
            and not seq_t.getitem_type(idx_t).may_run_user_code
        ):
            return False
        if is_consumer:
            return True
        elem_cls = elem_type.runtime_class_type()
        item_cls = item_t.runtime_class_type()
        old_cls = seq_t.getitem_type(idx_t).runtime_class_type()
        return (
            elem_cls is not UNKNOWN
            and item_cls is not UNKNOWN
            and old_cls is not UNKNOWN
            and elem_cls.comparison(ast.ComparisonOperator.IS, item_cls)
            is BOOLEAN_FALSE
            and elem_cls.comparison(ast.ComparisonOperator.IS, old_cls) is BOOLEAN_FALSE
        )
    return False


def _try_borrow_container_element_local(
    component: IR2Component,
    proof: _BorrowedLifetime,
    block: IR2Block,
    index: int,
    vop: VOP,
    borrow_info,
    seq_local,
    root_local,
    writer_index: int,
    move_vop: VOP,
    *,
    move_index: int | None = None,
    allow_unused: bool = False,
) -> bool:
    reads = component.reads
    writes = component.writes
    res_tn = vop.results[0].tn
    res_type = res_tn.type or UNKNOWN
    if res_type.is_fixnum or res_type.is_immortal:
        return False
    local_tn = move_vop.results[0].tn
    if (
        not _plain_local(local_tn)
        or local_tn in (seq_local, root_local)
        or local_tn.borrowed_from is not None
        or local_tn.leaf in component.lambda_.vars
        or local_tn.leaf in component.environment.cells
        or local_tn.leaf in component.environment.copies
        or component.lambda_.coroutine_body
        or len(writes.get(local_tn, ())) != 1
        or _has_local_aggregate_origin(root_local, component)
    ):
        return False
    if any(
        not _safe_list_borrow_step(step, seq_local, root_local)
        for step in block.vops[writer_index + 1 : index]
    ):
        return False
    local_reads = reads.get(local_tn, ())
    if not local_reads:
        if not allow_unused:
            return False
        vop.codegen_info = borrow_info
        res_tn.borrowed_from = root_local
        local_tn.borrowed_from = root_local
        move_vop.codegen_info = False
        return True
    from_storage_read = vop.info.name == "primitive-_storage_read"
    borrowing_loads: list[VOP] = []
    consumers: set[VOP] = set()
    for load_vop, _ in local_reads:
        if (
            load_vop.info is not vops.LOAD_LOCAL
            or load_vop.codegen_info is None
            or not load_vop.codegen_info.bound
        ):
            return False
        load_res_tn = load_vop.results[0].tn
        if load_res_tn.leaf is not None or len(writes.get(load_res_tn, ())) != 1:
            return False
        load_res_reads = reads.get(load_res_tn, ())
        if len(load_res_reads) != 1:
            return False
        consumer_vop, consumer_ref = load_res_reads[0]
        load_block, load_pos = proof.positions[load_vop]
        cons_block, cons_pos = proof.positions[consumer_vop]
        if (
            cons_block is load_block
            and cons_pos > load_pos
            and _can_borrow_list_getitem(consumer_vop, consumer_ref)
        ):
            borrowing_loads.append(load_vop)
            consumers.add(consumer_vop)
        else:
            return False
    if not borrowing_loads:
        return False
    single_consumer = len(consumers) == 1
    if move_index is None:
        move_index = index + 1
    boundary = (block, move_index + 1)
    pending = []
    for consumer in consumers:
        c_block, c_index = proof.positions[consumer]
        if c_block is block and c_index <= move_index:
            return False
        pending.append((c_block, c_index + 1))
    visited = set()
    while pending:
        point = pending.pop()
        if point == boundary or point in visited:
            continue
        visited.add(point)
        b, idx = point
        if idx == 0:
            prev = proof.predecessors[b]
            if not prev or b is component.blocks[0]:
                return False
            pending.extend(prev)
            continue
        step = b.vops[idx - 1]
        if not _safe_container_local_region_step(
            component,
            step,
            local_tn,
            seq_local,
            root_local,
            res_type,
            is_consumer=single_consumer and step in consumers,
            from_storage_read=from_storage_read,
        ):
            return False
        pending.append((b, idx - 1))
    vop.codegen_info = borrow_info
    res_tn.borrowed_from = root_local
    local_tn.borrowed_from = root_local
    move_vop.codegen_info = False
    for load_vop in borrowing_loads:
        load_vop.codegen_info = replace(load_vop.codegen_info, move=False, borrow=True)
        load_vop.results[0].tn.borrowed_from = root_local
    return True


def _specialize_list_getitems(component: IR2Component) -> None:
    """Specialize ``protocol-getitem`` and ``primitive-_storage_read`` on proven
    containers with fixnum indices, and borrow elements across protected
    non-mutating regions."""
    if component.context.plan is not None:
        return
    reads = component.reads
    writes = component.writes
    proof: _BorrowedLifetime | None = None
    for block in component.blocks:
        positions: dict[VOP, int] | None = None
        for index, vop in enumerate(block.vops):
            name = vop.info.name
            if vop.codegen_info is not None or len(vop.args) != 2 or not vop.results:
                continue
            seq_type = vop.args[0].tn.type or UNKNOWN
            idx_type = vop.args[1].tn.type or UNKNOWN
            if not idx_type.is_fixnum:
                continue
            if name == "protocol-getitem":
                if seq_type.is_exact_list:
                    vop.codegen_info = "list_owned"
                    borrow_info = "list_borrow"
                elif seq_type.is_exact_tuple:
                    vop.codegen_info = "tuple_owned"
                    borrow_info = "tuple_borrow"
                else:
                    continue
            elif name == "primitive-_storage_read":
                borrow_info = "storage_borrow"
            else:
                continue
            res_tn = vop.results[0].tn
            if res_tn.leaf is not None or (
                res_tn.name is not None and res_tn.name.startswith("$")
            ):
                continue
            res_writes = writes.get(res_tn)
            res_reads = reads.get(res_tn)
            if (
                res_writes is None
                or len(res_writes) != 1
                or res_reads is None
                or len(res_reads) != 1
            ):
                continue
            seq_tn = vop.args[0].tn
            seq_writes = writes.get(seq_tn)
            if seq_writes is None or len(seq_writes) != 1:
                continue
            seq_writer, _ = seq_writes[0]
            if (
                seq_writer.info is not vops.LOAD_LOCAL
                or seq_writer.codegen_info is None
                or not seq_writer.codegen_info.bound
                or seq_writer.codegen_info.move
            ):
                continue
            seq_local = seq_writer.args[0].tn
            if not _plain_local(seq_local):
                continue
            root_local = (
                seq_local.borrowed_from
                if seq_local.borrowed_from is not None
                else seq_local
            )
            if positions is None:
                positions = {step: pos for pos, step in enumerate(block.vops)}
            writer_index = positions.get(seq_writer)
            reader, reader_ref = res_reads[0]
            reader_index = positions.get(reader)
            if (
                writer_index is None
                or writer_index >= index
                or reader_index is None
                or reader_index <= index
            ):
                continue
            if (
                name == "protocol-getitem"
                and reader.info.name == "structural-unpack"
                and reader_index == index + 2
                and _try_borrow_unpacked_getitem(
                    component,
                    block,
                    index,
                    vop,
                    borrow_info,
                    seq_local,
                    root_local,
                    writer_index,
                )
            ):
                continue
            if reader.info is vops.MOVE and reader_index == index + 1:
                if proof is None:
                    proof = _BorrowedLifetime(component)
                _try_borrow_container_element_local(
                    component,
                    proof,
                    block,
                    index,
                    vop,
                    borrow_info,
                    seq_local,
                    root_local,
                    writer_index,
                    reader,
                )
                continue
            if not _can_borrow_list_getitem(reader, reader_ref):
                continue
            if any(
                step is not vop
                and not _safe_list_borrow_step(step, seq_local, root_local)
                for step in block.vops[writer_index + 1 : reader_index]
            ):
                continue
            vop.codegen_info = borrow_info
            res_tn.borrowed_from = root_local


_SUSPENSION_VOPS = frozenset(
    {
        "yield",
        "protocol-yield_from",
        "protocol-await_value",
        "protocol-async_generator_yield",
    }
)


def _mark_last_local_reads(component: IR2Component) -> None:
    """Mark ``LOAD_LOCAL`` VOPs that are the final read of their local on all
    normal and exception paths before the local is overwritten or the body exits."""
    environment = component.environment
    candidates = {
        tn
        for var in component.lambda_.locals
        if not var.indirect
        and not var.expression_temporary
        and (tn := environment.vars.get(var)) is not None
        and tn.borrowed_from is not tn
    }
    visible_candidates = (
        {
            tn
            for var in component.lambda_.locals
            if not var.indirect
            and not var.hidden
            and not var.name.startswith("$")
            and (tn := environment.vars.get(var)) is not None
            and tn.borrowed_from is not tn
        }
        if component.lambda_.coroutine_body
        else set()
    )
    for block in component.blocks:
        for vop in block.vops:
            if vop.info is vops.LOAD_LOCAL and (
                vop.args[0].tn in candidates
                or vop.args[0].tn.borrowed_from is vop.args[0].tn
            ):
                vop.codegen_info = replace(vop.codegen_info, move=False)
    if not candidates:
        return
    blocks = component.blocks
    if (
        len(blocks) == 1
        and not blocks[0].block.succ
        and block_exception_target(blocks[0].block) is None
    ):
        live = set()
        for vop in reversed(blocks[0].vops):
            if visible_candidates and vop.info.name in _SUSPENSION_VOPS:
                live.update(visible_candidates)
            if vop.info is vops.LOAD_LOCAL:
                tn = vop.args[0].tn
                access: LocalAccess = linked(vop.codegen_info, vop, "local access")
                if tn in candidates and tn not in live and not access.move:
                    vop.codegen_info = replace(access, move=True)
            for ref in vop.results:
                live.discard(ref.tn)
            for ref in vop.args:
                if ref.tn in candidates:
                    live.add(ref.tn)
        return
    ir2_blocks = component.ir2_blocks
    normal_succs = {
        block: [ir2_blocks[succ] for succ in block.block.succ if succ in ir2_blocks]
        for block in blocks
    }
    handlers = {
        block: ir2_blocks.get(block_exception_target(block.block)) for block in blocks
    }
    block_gen: dict[IR2Block, set] = {}
    block_kill: dict[IR2Block, set] = {}
    for block in blocks:
        gen = set()
        kill = set()
        for vop in reversed(block.vops):
            if visible_candidates and vop.info.name in _SUSPENSION_VOPS:
                gen.update(visible_candidates)
            for ref in vop.results:
                tn = ref.tn
                if tn in candidates:
                    gen.discard(tn)
                    kill.add(tn)
            for ref in vop.args:
                tn = ref.tn
                if tn in candidates:
                    gen.add(tn)
        block_gen[block] = gen
        block_kill[block] = kill
    live_in = {block: set() for block in blocks}
    changed = True
    while changed:
        changed = False
        for block in reversed(blocks):
            live = set()
            for succ in normal_succs[block]:
                live |= live_in[succ]
            if live:
                live.difference_update(block_kill[block])
            live |= block_gen[block]
            handler = handlers[block]
            if handler is not None:
                live |= live_in[handler]
            if live != live_in[block]:
                live_in[block] = live
                changed = True
    for block in blocks:
        live = set()
        for succ in normal_succs[block]:
            live |= live_in[succ]
        handler = handlers[block]
        handler_live = live_in[handler] if handler is not None else ()
        for vop in reversed(block.vops):
            if handler_live:
                live.update(handler_live)
            if visible_candidates and vop.info.name in _SUSPENSION_VOPS:
                live.update(visible_candidates)
            if vop.info is vops.LOAD_LOCAL:
                tn = vop.args[0].tn
                access: LocalAccess = linked(vop.codegen_info, vop, "local access")
                if tn in candidates and tn not in live and not access.move:
                    vop.codegen_info = replace(access, move=True)
            for ref in vop.results:
                live.discard(ref.tn)
            for ref in vop.args:
                if ref.tn in candidates:
                    live.add(ref.tn)


def _clear_lvar_info(lambda_: Lambda) -> None:
    """LTN annotations belong to one conversion; the next context annotates
    afresh with its own primitive types."""
    for block in linked(lambda_.component, lambda_, "component").blocks():
        for node in block.nodes():
            node.accept(_CLEAR_LVAR_INFO)


class _ClearLvarInfo(NodeVisitor):
    def visit_ValuedNode(self, node):
        if node.lvar is not None:
            node.lvar.info = None

    def visit_Return(self, node: Return):
        node.result.info = None


class _ConstantIndex(NodeVisitor, LeafVisitor):
    """The fixed nonnegative index an operand delivers: a reference to an
    integer literal constant."""

    def visit_Node(self, node):
        raise ValueError("structural unpack requires a fixed nonnegative index")

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return self.visit_Node(leaf)

    def visit_Constant(self, leaf: Constant):
        return leaf.value.accept(_INTEGER_INDEX)


class _IntegerIndex(ast.Visitor):
    def visit_Node(self, node):
        raise ValueError("structural unpack requires a fixed nonnegative index")

    def visit_IntegerLiteral(self, node: ast.IntegerLiteral):
        if node.value < 0:
            return self.visit_Node(node)
        return node.value


class _ProvedSlotRead(NodeVisitor):
    """Whether the consumer of a tuple of type ``shape`` reads a slot the
    shape proves present: a known ``%unpack_ref`` call."""

    def __init__(self, shape):
        self.shape = shape

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        if node.kind is not CallKind.KNOWN or node.fun_info is not fndb.UNPACK_REF:
            return False
        index = _Converter.constant_index(node.args[1])
        return self.shape.proves_tuple_index(index)


class _LoadLeaf(LeafVisitor):
    """Convert a ``Ref`` by the kind of leaf it references."""

    def __init__(self, converter: _Converter):
        self.converter = converter
        self.node: Ref | None = None

    def visit_Leaf(self, leaf):
        raise TypeError(f"no IR2 conversion for a reference to {leaf!r}")

    def visit_KnownFunction(self, leaf: KnownFunction):
        return None

    def visit_Lambda(self, leaf: Lambda):
        return None

    def visit_Constant(self, leaf: Constant):
        self.load(vops.CONSTANT, info=leaf.value)

    def visit_GlobalVar(self, leaf: GlobalVar):
        self.load(vops.LOAD_GLOBAL, info=leaf)

    def visit_LambdaVar(self, leaf: LambdaVar):
        converter = self.converter
        node = linked(self.node, self, "reference node")
        if converter.borrows_unpack_capture(node, leaf):
            # The capture dominates this infallible slot read and remains owned
            # until the assignment's explicit discard or exception transfer.
            assert node.lvar is not None and node.lvar.info is not None
            node.lvar.info.locs.append(converter.var_tn(leaf))
            return
        result, discard = converter.result_tn(node)
        environment = converter.environment
        local = converter.context.local_environments
        if local is not None and local.function_for_binding(leaf) is not None:
            result.primitive_type = REFERENCE_T
        if leaf in converter.stack_cells:
            converter.emit(
                vops.LOCAL_CELL_REF,
                node,
                args=(converter.stack_cells[leaf],),
                results=(result,),
                info=node.allow_unbound,
            )
        elif leaf in environment.cells:
            converter.emit(
                vops.VALUE_CELL_REF,
                node,
                args=(environment.cells[leaf],),
                results=(result,),
                info=node.allow_unbound,
            )
        else:
            source = environment.copies.get(leaf) or converter.var_tn(leaf)
            types = converter.component.context.types
            bound = (
                leaf in environment.copies
                or (leaf.hidden and node.lvar is None and node.final)
                or (
                    not node.allow_unbound
                    and node in types
                    and not types[node].may_be_unbound
                )
            )
            converter.emit(
                vops.LOAD_LOCAL,
                node,
                args=(source,),
                results=(result,),
                info=LocalAccess(
                    leaf.name,
                    node.final and source.borrowed_from is not source,
                    bound,
                    allow_unbound=node.allow_unbound,
                ),
            )
        if discard:
            converter.emit_discard(node, result)

    def load(self, template, info):
        converter = self.converter
        node = linked(self.node, self, "reference node")
        result, discard = converter.result_tn(node)
        converter.emit(template, node, results=(result,), info=info)
        if discard:
            converter.emit_discard(node, result)


class _StoreVar(LeafVisitor):
    """Convert a ``Set`` by the kind of variable it assigns."""

    def __init__(self, converter: _Converter):
        self.converter = converter
        self.node: Set | None = None

    def visit_Leaf(self, leaf):
        raise TypeError(f"no IR2 conversion for an assignment to {leaf!r}")

    def visit_LambdaVar(self, leaf: LambdaVar):
        converter = self.converter
        node = linked(self.node, self, "assignment node")
        if leaf in converter.stack_cells:
            local = converter.context.local_environments
            owned = local.function_for_binding(leaf) is None
            if node.value is None:
                converter.emit(
                    vops.LOCAL_CELL_CLEAR,
                    node,
                    args=(converter.stack_cells[leaf],),
                    info=owned,
                )
            else:
                converter.emit(
                    vops.LOCAL_CELL_SET,
                    node,
                    args=(converter.stack_cells[leaf], converter.lvar_tn(node.value)),
                    info=owned,
                )
            return
        if leaf in converter.environment.cells:
            cell = converter.environment.cells[leaf]
            if node.value is None:
                converter.emit(
                    vops.VALUE_CELL_CLEAR,
                    node,
                    args=(cell,),
                )
            else:
                converter.emit(
                    vops.VALUE_CELL_SET,
                    node,
                    args=(cell, converter.lvar_tn(node.value)),
                )
            return
        target = converter.var_tn(leaf)
        release = converter.context.type_of(node).needs_release
        if node.value is None:
            converter.emit(vops.UNBIND_LOCAL, node, results=(target,), info=release)
        else:
            converter.emit(
                vops.MOVE,
                node,
                args=(converter.lvar_tn(node.value),),
                results=(target,),
                info=release,
            )

    def visit_GlobalVar(self, leaf: GlobalVar):
        converter = self.converter
        node = linked(self.node, self, "assignment node")
        if node.value is None:
            converter.emit(vops.UNBIND_GLOBAL, node, info=(leaf, node.missing_ok))
        else:
            converter.emit(
                vops.STORE_GLOBAL,
                node,
                args=(converter.lvar_tn(node.value),),
                info=leaf,
            )


class _MethodResolution(NodeVisitor):
    """The ``%resolve_method`` combination delivering a resolved method."""

    def __init__(self, resolved):
        self.resolved = resolved

    def visit_Node(self, node):
        raise TypeError(f"{self.resolved!r} is not delivered by a method resolution")

    def visit_Combination(self, node: Combination) -> Combination:
        return node


class _ExactListAppendCall(NodeVisitor):
    """Whether the consumer of a resolved method is a 1-argument positional call."""

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        return (
            node.kind is CallKind.KNOWN
            and node.fun_info is fndb.CALL_RESOLVED
            and len(node.args) == 2
            and not linked(node.info, node, "call info").keywords
        )


class _ExactListPopCall(NodeVisitor):
    """Whether the consumer of a resolved method is a 0-argument positional call."""

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        return (
            node.kind is CallKind.KNOWN
            and node.fun_info is fndb.CALL_RESOLVED
            and len(node.args) == 1
            and not linked(node.info, node, "call info").keywords
        )


class _ExactTextFindCall(NodeVisitor):
    """Whether the consumer of a resolved ``str.find``/``str.rfind`` method is
    a 1-, 2-, or 3-argument positional call with exact ``str`` substring and
    fixnum or ``None`` bounds."""

    def __init__(self, context: EntryContext):
        self.context = context

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        if (
            node.kind is not CallKind.KNOWN
            or node.fun_info is not fndb.CALL_RESOLVED
            or not (2 <= len(node.args) <= 4)
            or linked(node.info, node, "call info").keywords
        ):
            return False
        sub = (
            self.context.type_of(node.args[1].uses[0])
            if len(node.args[1].uses) == 1
            else UNKNOWN
        )
        if not sub.fits_text(TEXT_TYPE):
            return False
        for bound_arg in node.args[2:]:
            bound = (
                self.context.type_of(bound_arg.uses[0])
                if len(bound_arg.uses) == 1
                else UNKNOWN
            )
            if not (bound.is_fixnum or bound.fits_none(NONE_TYPE)):
                return False
        return True


class _ExactTextSplitCall(NodeVisitor):
    """Whether the consumer of a resolved ``str.split`` method is a 1- or
    2-argument positional call with exact ``str`` separator and optional integer
    ``maxsplit``."""

    def __init__(self, context: EntryContext):
        self.context = context

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        if (
            node.kind is not CallKind.KNOWN
            or node.fun_info is not fndb.CALL_RESOLVED
            or not (2 <= len(node.args) <= 3)
            or linked(node.info, node, "call info").keywords
        ):
            return False
        sep = (
            self.context.type_of(node.args[1].uses[0])
            if len(node.args[1].uses) == 1
            else UNKNOWN
        )
        if not sep.fits_text(TEXT_TYPE):
            return False
        if len(node.args) == 3:
            maxsplit = (
                self.context.type_of(node.args[2].uses[0])
                if len(node.args[2].uses) == 1
                else UNKNOWN
            )
            if not maxsplit.fits_python_integer(PYTHON_INTEGER):
                return False
        return True


class _ExactTextPartitionCall(NodeVisitor):
    """Whether the consumer of a resolved ``str.partition``/``str.rpartition``
    method is a 1-argument positional call with exact ``str`` separator."""

    def __init__(self, context: EntryContext):
        self.context = context

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        if (
            node.kind is not CallKind.KNOWN
            or node.fun_info is not fndb.CALL_RESOLVED
            or len(node.args) != 2
            or linked(node.info, node, "call info").keywords
        ):
            return False
        sep = (
            self.context.type_of(node.args[1].uses[0])
            if len(node.args[1].uses) == 1
            else UNKNOWN
        )
        return sep.fits_text(TEXT_TYPE)


class _ExactTextAffixCall(NodeVisitor):
    """Whether the consumer of a resolved ``str.startswith``/``str.endswith``
    method is a 1-, 2-, or 3-argument positional call with exact ``str`` or
    tuple affix and fixnum or ``None`` bounds."""

    def __init__(self, context: EntryContext):
        self.context = context

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        if (
            node.kind is not CallKind.KNOWN
            or node.fun_info is not fndb.CALL_RESOLVED
            or not (2 <= len(node.args) <= 4)
            or linked(node.info, node, "call info").keywords
        ):
            return False
        affix = (
            self.context.type_of(node.args[1].uses[0])
            if len(node.args[1].uses) == 1
            else UNKNOWN
        )
        if not (affix.fits_text(TEXT_TYPE) or affix.fits_tuple(TupleType())):
            return False
        for bound_arg in node.args[2:]:
            bound = (
                self.context.type_of(bound_arg.uses[0])
                if len(bound_arg.uses) == 1
                else UNKNOWN
            )
            if not (bound.is_fixnum or bound.fits_none(NONE_TYPE)):
                return False
        return True


class _UnmutatedIntRef(NodeVisitor, LeafVisitor):
    """Return ``node`` when it is a ``Ref`` to an unmutated ``GlobalVar('int')``."""

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        if node.leaf.accept(self):
            return node
        return None

    def visit_Leaf(self, leaf):
        return False

    def visit_GlobalVar(self, leaf: GlobalVar):
        return leaf.name == "int" and not leaf.sets


class _NoneLiteralCheck(ast.Visitor):
    def visit_Node(self, node):
        return False

    def visit_NoneLiteral(self, node: ast.NoneLiteral):
        return True


class _NoneConstantRef(NodeVisitor, LeafVisitor):
    """Return ``node`` when it is a ``Ref`` to ``Constant(NoneLiteral)``."""

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        if node.leaf.accept(self):
            return node
        return None

    def visit_Leaf(self, leaf):
        return False

    def visit_Constant(self, leaf: Constant):
        return leaf.value.accept(_NONE_LITERAL_CHECK)


class _GetsliceProducer(NodeVisitor):
    """Return ``node`` when it is a known ``%getslice`` combination."""

    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.kind is CallKind.KNOWN
            and node.fun_info is fndb.GETSLICE
            and len(node.args) == 4
        ):
            return node
        return None


class _ScanIntParse(NodeVisitor):
    def __init__(self, converter: _Converter):
        self.converter = converter

    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        self.converter.note_int_parse(node)
        self.converter.note_range_prepare(node)


class _ExactDictGetCall(NodeVisitor):
    """Whether the consumer of a resolved ``dict.get`` method is a 1- or
    2-argument positional call."""

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node: Combination):
        return (
            node.kind is CallKind.KNOWN
            and node.fun_info is fndb.CALL_RESOLVED
            and 2 <= len(node.args) <= 3
            and not linked(node.info, node, "call info").keywords
        )


_PROTOCOL_PREFIXES = (
    "mule_sequences_",
    "mule_format_",
    "mule_arguments_",
    "mule_native_class_",
    "mule_print_",
)
_CLEAR_LVAR_INFO = _ClearLvarInfo()
_INTEGER_INDEX = _IntegerIndex()
_CONSTANT_INDEX = _ConstantIndex()
_EXACT_LIST_APPEND_CALL = _ExactListAppendCall()
_EXACT_LIST_POP_CALL = _ExactListPopCall()
_EXACT_DICT_GET_CALL = _ExactDictGetCall()
_UNMUTATED_INT_REF = _UnmutatedIntRef()
_NONE_LITERAL_CHECK = _NoneLiteralCheck()
_NONE_CONSTANT_REF = _NoneConstantRef()
_GETSLICE_PRODUCER = _GetsliceProducer()


class _Converter(NodeVisitor):
    def __init__(self, component: IR2Component):
        self.component = component
        self.environment = component.environment
        self.context = component.context
        self.lambda_ = component.lambda_
        self.current: IR2Block | None = None
        self._load_leaf: _LoadLeaf | None = _LoadLeaf(self)
        self._store_var: _StoreVar | None = _StoreVar(self)
        self._scan_int_parse: _ScanIntParse | None = _ScanIntParse(self)
        self._skipped_nodes: set = set()
        self._fused_int_calls: dict[Combination, Combination | None] = {}
        self._fused_range_prepares: dict[Combination, tuple] = {}
        self.stack_cells = {}

    def close(self) -> None:
        self._load_leaf = None
        self._store_var = None
        self._scan_int_parse = None
        self._skipped_nodes.clear()
        self._fused_int_calls.clear()
        self._fused_range_prepares.clear()

    # TNs

    def var_tn(self, var: LambdaVar):
        tn = self.environment.vars.get(var)
        if tn is None:
            plan = self.context.plan
            ptype = plan.var_type(var) if plan is not None else None
            local = self.context.local_environments
            reference = (
                local is not None and local.function_for_binding(var) is not None
            )
            tn = self.component.tn(
                REFERENCE_T if reference else (ptype or T),
                leaf=None if reference else var,
                name=f"$function:{var.name}" if reference else var.name,
            )
            self.environment.vars[var] = tn
        return tn

    def prepare_environment(self):
        """Create the TNs of locals, captured cells and copies, and range
        states before any block refers to them."""
        lambda_ = self.lambda_
        environment = self.environment
        for var in lambda_.locals:
            self.var_tn(var)
        for index in self.context.borrowed_parameters:
            root = self.var_tn(lambda_.vars[index])
            root.borrowed_from = root
        closure = lambda_.environment.closure if lambda_.environment else ()
        environment.captured = list(closure)
        for var in closure:
            local = self.context.local_environments
            if local is not None and var in local.stack_cells:
                self.stack_cells[var] = self.component.tn(
                    REFERENCE_T, name=f"$slot:{var.name}"
                )
            elif var.indirect:
                environment.cells[var] = self.component.tn(T, name=f"$cell:{var.name}")
            else:
                vtype = var.optimistic_type or UNKNOWN
                reference = (
                    local is not None and local.function_for_binding(var) is not None
                )
                tn = self.component.tn(
                    REFERENCE_T if reference else primitive_type_of(vtype),
                    leaf=None if reference else var,
                    name=f"$copy:{var.name}",
                )
                tn.type = vtype
                environment.copies[var] = tn
        for var in lambda_.locals:
            local = self.context.local_environments
            if var.indirect and not (local is not None and var in local.stack_cells):
                environment.cells[var] = self.component.tn(T, name=f"$cell:{var.name}")
                environment.home_cells.append(var)

    def lvar_tn(self, lvar):
        """The TN delivering ``lvar``'s value (``lvar-tn``)."""
        info = lvar.info
        if info is None:
            raise ValueError(f"{lvar!r} was not annotated by LTN")
        if info.kind is IR2LvarKind.DELAYED:
            raise ValueError(f"{lvar!r} is a compile-time operand")
        if not info.locs:
            tn = self.component.tn(info.primitive_type)
            value_type = BOTTOM
            for producer in lvar.uses:
                value_type = value_type.join(self.context.type_of(producer))
            tn.type = value_type
            info.locs.append(tn)
        return info.locs[0]

    def temporary(self, primitive_type=T, value_type=UNKNOWN):
        tn = self.component.tn(primitive_type)
        tn.type = value_type
        return tn

    def result_tn(self, node):
        """The TN a valued node writes: the lvar's, or a fresh temporary whose
        value is discarded."""
        if node.lvar is not None:
            return self.lvar_tn(node.lvar), False
        value_type = self.context.type_of(node)
        plan = self.context.plan
        ptype = plan.node_type(node) if plan is not None else None
        return self.temporary(ptype or primitive_type_of(value_type), value_type), True

    def range_tn(self, state):
        tn = self.environment.ranges.get(id(state))
        if tn is None:
            tn = self.environment.ranges[id(state)] = self.component.tn(
                RANGE_T, name=f"$range{len(self.environment.ranges)}"
            )
        return tn

    # VOP emission

    def emit(self, template, node, args=(), results=(), info=None) -> VOP:
        arg_refs = [TNRef(tn, False) for tn in args] if args else []
        result_refs = [TNRef(tn, True) for tn in results] if results else []
        vop = VOP(template, node, arg_refs, result_refs)
        vop.codegen_info = info
        if arg_refs:
            reads = self.component.reads
            for ref in arg_refs:
                reads[ref.tn].append((vop, ref))
        if result_refs:
            writes = self.component.writes
            for ref in result_refs:
                writes[ref.tn].append((vop, ref))
        self.current.vops.append(vop)
        return vop

    def emit_discard(self, node, tn) -> None:
        self.emit(vops.DISCARD, node, args=(tn,))

    def convert_block(self, block) -> None:
        self.current = self.component.ir2_blocks[block]
        # SBCL ir2-convert: Bind emits its own entry poll, while the loop
        # analysis identifies natural and strange loop heads here.
        if block.loop is not None and block.loop.is_header(block):
            self.emit(vops.INSERT_SAFEPOINT, block.start.next)
        self._skipped_nodes.clear()
        self._fused_int_calls.clear()
        self._fused_range_prepares.clear()
        nodes = tuple(block.nodes())
        scan = linked(self._scan_int_parse, self, "int-parse scanner")
        for node in nodes:
            node.accept(scan)
        for node in nodes:
            if node not in self._skipped_nodes:
                node.accept(self)
        self.convert_transfer(block)

    def lvar_fits_text(self, lvar) -> bool:
        return bool(lvar.uses) and all(
            (vtype := self.context.type_of(use)).fits_text(TEXT_TYPE)
            and not vtype.may_be_unbound
            for use in lvar.uses
        )

    def lvar_fits_slice_bound(self, lvar) -> bool:
        return bool(lvar.uses) and all(
            (
                (vtype := self.context.type_of(use)).fits(PYTHON_INTEGER)
                or vtype.fits_none(NONE_TYPE)
            )
            and not vtype.may_be_unbound
            for use in lvar.uses
        )

    def lvar_fits_none(self, lvar) -> bool:
        return bool(lvar.uses) and all(
            (vtype := self.context.type_of(use)).fits_none(NONE_TYPE)
            and not vtype.may_be_unbound
            for use in lvar.uses
        )

    def note_int_parse(self, node: Combination) -> None:
        if (
            node.kind is not CallKind.FULL
            or self.context.plan is not None
            or self.lambda_.policy.safety >= 3
            or self.lambda_.name.startswith(_PROTOCOL_PREFIXES)
            or len(node.args) != 1
            or (node.info is not None and node.info.keywords)
            or len(node.fun.uses) != 1
            or node.fun.dest is not node
        ):
            return
        fun_ref = node.fun.use().accept(_UNMUTATED_INT_REF)
        if fun_ref is None or fun_ref.block is not node.block:
            return
        arg_lvar = node.args[0]
        if len(arg_lvar.uses) != 1 or arg_lvar.dest is not node:
            return
        arg_use = arg_lvar.use()
        arg_type = self.context.type_of(arg_use)
        if not (arg_type.fits_text(TEXT_TYPE) and not arg_type.may_be_unbound):
            return
        slice_node = arg_use.accept(_GETSLICE_PRODUCER)
        if (
            slice_node is not None
            and slice_node.block is node.block
            and slice_node.next is node.prev
        ):
            seq_lvar, start_lvar, stop_lvar, step_lvar = slice_node.args
            if len(step_lvar.uses) == 1 and step_lvar.dest is slice_node:
                step_ref = step_lvar.use().accept(_NONE_CONSTANT_REF)
                if (
                    step_ref is not None
                    and step_ref.block is node.block
                    and self.lvar_fits_text(seq_lvar)
                    and self.lvar_fits_slice_bound(start_lvar)
                    and self.lvar_fits_slice_bound(stop_lvar)
                    and self.lvar_fits_none(step_lvar)
                ):
                    self._skipped_nodes.add(fun_ref)
                    self._skipped_nodes.add(step_ref)
                    self._skipped_nodes.add(slice_node)
                    self._fused_int_calls[node] = slice_node
                    return
        self._skipped_nodes.add(fun_ref)
        self._fused_int_calls[node] = None

    def note_range_prepare(self, node: Combination) -> None:
        if (
            node.kind is not CallKind.KNOWN
            or node.fun_info is not fndb.RANGE_PREPARE
            or self.context.plan is not None
            or self.lambda_.policy.safety >= 3
            or node.lexenv.policy.safety >= 3
            or self.lambda_.name.startswith(_PROTOCOL_PREFIXES)
        ):
            return
        state = node.info
        if (
            state is None
            or state.direct
            or len(node.args) != 2
            or not (1 <= len(state.arguments) <= 3)
        ):
            return
        callee_lvar = node.args[0]
        if len(callee_lvar.uses) != 1 or not callee_lvar.uses[0].accept(
            _PREDICTABLE_RANGE_CALLEE_REF
        ):
            return
        if len(node.args[1].uses) != 1:
            return
        setup = (
            node.args[1].uses[0].accept(_RangeIteratorRef(node.block, state.arguments))
        )
        if setup is None:
            return
        iterator_ref, iterator_set, iterate_call, range_call = setup
        iterator_var = iterator_ref.leaf
        if (
            iterator_var in self.stack_cells
            or iterator_var in self.environment.cells
            or iterator_var in self.environment.copies
        ):
            return
        if any(
            len(arg.uses) != 1 or arg.dest is not range_call for arg in state.arguments
        ):
            return
        if len(iterate_call.fun.uses) != 1 or len(range_call.fun.uses) != 1:
            return
        callee_ref_2 = callee_lvar.uses[0]
        callee_var = callee_ref_2.leaf
        callee_ref_1 = range_call.fun.uses[0]
        iterate_ref = iterate_call.fun.uses[0]
        if (
            callee_ref_1.block is not node.block
            or iterate_ref.block is not node.block
            or callee_ref_2.block is not node.block
            or iterator_ref.block is not node.block
        ):
            return
        if len(callee_var.refs) != 2 or len(callee_var.sets) != 1:
            return
        callee_set = callee_var.sets[0]
        if (
            callee_set.block is not node.block
            or callee_set.value is None
            or len(callee_set.value.uses) != 1
        ):
            return
        range_global_ref = callee_set.value.uses[0]
        if range_global_ref.block is not node.block:
            return
        self._skipped_nodes.update(
            (
                range_global_ref,
                callee_set,
                callee_ref_1,
                range_call,
                iterate_ref,
                iterate_call,
                iterator_set,
                callee_ref_2,
                iterator_ref,
            )
        )
        self._fused_range_prepares[node] = (iterator_var, range_global_ref.leaf)

    def convert_transfer(self, block) -> None:
        """A block that does not end in a branch, return, or raise jumps to
        its single successor."""
        current = linked(self.current, self, "current block")
        last = current.vops[-1] if current.vops else None
        if last is not None and last.info.name in vops.TERMINATORS:
            return
        successors = [succ for succ in block.succ if succ.start is not None]
        if len(successors) != 1:
            raise ValueError(f"{block!r} has no IR2 control transfer")
        self.emit(
            vops.JUMP, block.last, info=self.component.ir2_blocks[successors[0]].label
        )

    # Nodes

    def visit_Bind(self, node: Bind):
        environment = self.environment
        for index, var in enumerate(self.lambda_.vars):
            self.emit(vops.MOVE_ARGUMENT, node, results=(self.var_tn(var),), info=index)
        for index, var in enumerate(environment.captured):
            target = (
                self.stack_cells.get(var)
                or environment.cells.get(var)
                or environment.copies[var]
            )
            local = self.context.local_environments
            template = (
                vops.LOCAL_LOAD_CLOSURE
                if local is not None and local.is_local(self.lambda_)
                else vops.LOAD_CLOSURE
            )
            self.emit(
                template,
                node,
                results=(target,),
                info=(index, var in self.stack_cells)
                if template is vops.LOCAL_LOAD_CLOSURE
                else index,
            )
        for var in environment.home_cells:
            cell = self.temporary()
            self.emit(
                templates_for(fndb.PROTOCOLS["cell_new"])[0], node, results=(cell,)
            )
            self.emit(
                vops.MOVE,
                node,
                args=(cell,),
                results=(environment.cells[var],),
                info=False,
            )
            if var in self.lambda_.vars:
                # A captured parameter's cell starts out holding its argument.
                value = self.temporary()
                self.emit(
                    vops.LOAD_LOCAL,
                    node,
                    args=(self.var_tn(var),),
                    results=(value,),
                    info=LocalAccess(var.name, True, True),
                )
                self.emit(
                    vops.VALUE_CELL_SET,
                    node,
                    args=(environment.cells[var], value),
                )

        self.emit(vops.INSERT_SAFEPOINT, node)

    def cell(self, node, var):
        """Load the cell object of a captured-and-assigned variable."""
        loaded = self.temporary()
        self.emit(
            vops.LOAD_LOCAL,
            node,
            args=(self.environment.cells[var],),
            results=(loaded,),
            info=LocalAccess(var.name, False, True),
        )
        return loaded

    def captured_value(self, node, var):
        """The value a nested function's closure holds for ``var``."""
        environment = self.environment
        if var in environment.cells:
            return self.cell(node, var)
        source = environment.copies.get(var) or self.var_tn(var)
        local = self.context.local_environments
        reference = local is not None and local.function_for_binding(var) is not None
        loaded = self.temporary(REFERENCE_T if reference else T)
        self.emit(
            vops.LOAD_LOCAL,
            node,
            args=(source,),
            results=(loaded,),
            info=LocalAccess(
                var.name, False, var in environment.copies or var in self.lambda_.vars
            ),
        )
        return loaded

    def visit_Ref(self, node: Ref):
        if (
            node.lvar is not None
            and node.lvar.info is not None
            and node.lvar.info.kind is IR2LvarKind.DELAYED
        ):
            return
        load_leaf = linked(self._load_leaf, self, "leaf loader")
        load_leaf.node = node
        node.leaf.accept(load_leaf)
        load_leaf.node = None

    def borrows_unpack_capture(self, node: Ref, leaf: LambdaVar):
        if not (
            leaf.expression_temporary
            and node.lvar is not None
            and len(node.lvar.uses) == 1
        ):
            return False
        consumer = node.lvar.dest
        if consumer is None:
            return False
        return consumer.accept(_ProvedSlotRead(self.context.type_of(node)))

    @staticmethod
    def constant_index(lvar):
        return lvar.use().accept(_CONSTANT_INDEX)

    def visit_Set(self, node: Set):
        store_var = linked(self._store_var, self, "variable storer")
        store_var.node = node
        node.var.accept(store_var)
        store_var.node = None

    def visit_Cast(self, node: Cast):
        operand = self.lvar_tn(node.value)
        actual = operand.type or UNKNOWN
        checked = (
            node.type_check
            and bool(node.safety)
            and not actual.fits(node.contract.checked_type)
        )
        if (
            not checked
            and self.context.plan is None
            and node.lvar is not None
            and len(node.lvar.uses) == 1
            and not node.lvar.info.locs
            and operand.leaf is None
            and node not in self.context.element_contracts
        ):
            target_type = self.context.type_of(node)
            target_ptype = node.lvar.info.primitive_type
            if actual.fits(target_type) and operand.primitive_type is target_ptype:
                node.lvar.info.locs.append(operand)
                return
            writes = self.component.writes.get(operand, ())
            if (
                (not target_type.is_fixnum or actual.is_fixnum)
                and len(writes) == 1
                and not self.component.reads.get(operand)
                and writes[0][0].info.translate is not None
                and template_results_ok(writes[0][0].info, target_ptype)
            ):
                operand.type = target_type
                operand.primitive_type = target_ptype
                node.lvar.info.locs.append(operand)
                return
        result, discard = self.result_tn(node)
        self.emit(
            vops.TYPE_CHECK,
            node,
            args=(operand,),
            results=(result,),
            info=TypeCheck(
                node.contract, node.location, node.safety if node.type_check else 0
            ),
        )
        if discard:
            self.emit_discard(node, result)

    def visit_If(self, node: If):
        blocks = self.component.ir2_blocks
        self.emit(
            vops.BRANCH,
            node,
            args=(self.lvar_tn(node.test),),
            info=BranchTargets(
                blocks[node.consequent].label, blocks[node.alternative].label
            ),
        )

    def visit_Return(self, node: Return):
        self.emit(vops.RETURN, node, args=(self.lvar_tn(node.result),))

    def visit_Combination(self, node: Combination):
        if node.kind is not CallKind.KNOWN:
            self.convert_full_call(node)
            return
        name = linked(node.fun_info, node, "known function").name
        if name == "%setattr" and node.info.name == "__annotate__":
            local = self.context.local_environments
            receiver = node.args[0].use()
            target = local.function_for_value(receiver) if local is not None else None
            if target is not None:
                for arg in node.args:
                    self.emit_discard(node, self.lvar_tn(arg))
                return
        if name == "%make_function":
            self.convert_make_function(node)
        elif name in ("%resolve_method", "%resolve_super_method"):
            self.convert_resolve_method(node)
        elif name == "%call_resolved":
            self.convert_call_resolved(node)
        elif name in ("%range_prepare", "%range_available", "%range_advance"):
            self.convert_range(node)
        elif name == "%unpack":
            arity = self.constant_index(node.args[1])
            shape = self.context.type_of(node.args[0].use())
            self.convert_template(
                node,
                [self.lvar_tn(arg) for arg in node.args],
                UnpackCheck(arity, shape.fits(TupleType(arity))),
            )
        elif name == "%unpack_ref":
            index = self.constant_index(node.args[1])
            shape = self.context.type_of(node.args[0].use())
            args = [self.lvar_tn(arg) for arg in node.args]
            self.convert_template(
                node,
                args,
                UnpackSlot(
                    index,
                    shape.proves_tuple_index(index),
                    args[0].leaf is not None and args[0].leaf.expression_temporary,
                ),
            )
        else:
            self.convert_template(node, [self.lvar_tn(arg) for arg in node.args])

    def convert_template(self, node, args, info=None):
        """Emit the template LTN selected (``ir2-convert-template``)."""
        template = node.template
        if template is None:
            raise ValueError(f"{node!r} has no template")
        if linked(node.fun_info, node, "known function").result:
            result, discard = self.result_tn(node)
            self.emit(
                template,
                node,
                args=args,
                results=(result,),
                info=node.info if info is None else info,
            )
            if discard:
                self.emit_discard(node, result)
        else:
            self.emit(
                template, node, args=args, info=node.info if info is None else info
            )

    def convert_full_call(self, node: Combination):
        local = self.context.local_environments
        target = local.call_target(node) if local is not None else None
        if target is not None:
            result, discard = self.result_tn(node)
            self.emit(
                vops.LOCAL_EXTENT_CALL,
                node,
                args=(
                    self.lvar_tn(node.fun),
                    *(self.lvar_tn(arg) for arg in node.args),
                ),
                results=(result,),
                info=CallSite(
                    (),
                    node.info.location if node.info is not None else None,
                    target,
                    True,
                    self.context.borrowed_callees.get(target, frozenset()),
                    local_environment=True,
                ),
            )
            if discard:
                self.emit_discard(node, result)
            return
        if node in self._fused_int_calls:
            slice_node = self._fused_int_calls[node]
            result, discard = self.result_tn(node)
            if slice_node is not None:
                seq_lvar, start_lvar, stop_lvar, _ = slice_node.args
                self.emit(
                    vops.PARSE_INT_SLICE,
                    node,
                    args=(
                        self.lvar_tn(seq_lvar),
                        self.lvar_tn(start_lvar),
                        self.lvar_tn(stop_lvar),
                    ),
                    results=(result,),
                )
            else:
                self.emit(
                    vops.PARSE_INT_TEXT,
                    node,
                    args=(self.lvar_tn(node.args[0]),),
                    results=(result,),
                )
            if discard:
                self.emit_discard(node, result)
            return
        if self.context.plan is None and is_predictable_len_call(node):
            arg = node.args[0]
            arg_type = (
                self.context.type_of(arg.use()) if len(arg.uses) == 1 else UNKNOWN
            )
            primitive = known_length_primitive(arg_type)
            template = (
                templates_for(primitive)[0] if primitive is not None else vops.FAST_LEN
            )
            result, discard = self.result_tn(node)
            self.emit(
                template,
                node,
                args=(self.lvar_tn(arg),),
                results=(result,),
            )
            if discard:
                self.emit_discard(node, result)
            return
        fun = node.fun
        target = self.context.targets.get(fun.use()) if len(fun.uses) == 1 else None
        code_target = (
            self.context.type_of(fun.use()).callable_target()
            if len(fun.uses) == 1
            else None
        )
        proven = target is not None and (
            code_target is target or (node.kind is CallKind.GUARDED and node.proven)
        )
        info = node.info
        site = CallSite(
            info.keywords if info is not None else (),
            info.location if info is not None else None,
            target,
            proven,
            self.context.borrowed_callees.get(target, frozenset())
            if target is not None
            and (info is None or not info.keywords)
            and target is not None
            and target in self.context.borrowed_callees
            and target.call_order((), len(node.args)) == tuple(range(len(node.args)))
            else frozenset(),
        )
        result, discard = self.result_tn(node)
        plan = self.context.plan
        template = vops.FULL_CALL
        if plan is not None and plan.allocation(node) is not None:
            template = vops.RAW_CONSTRUCT
        self.emit(
            template,
            node,
            args=(self.lvar_tn(fun), *(self.lvar_tn(arg) for arg in node.args)),
            results=(result,),
            info=site,
        )
        if discard:
            self.emit_discard(node, result)

    def convert_make_function(self, node: Combination):
        lambda_ = made_lambda(node)
        local = self.context.local_environments
        if local is not None and local.discards_function(lambda_):
            # Defaults were evaluated before construction; discarding metadata
            # must not discard their effects or execute lazy annotations.
            for argument in node.args[1:]:
                self.emit_discard(node, self.lvar_tn(argument))
            result, discard = self.result_tn(node)
            self.emit(
                vops.CONSTANT, node, results=(result,), info=ast.NoneLiteral(None)
            )
            if discard:
                self.emit_discard(node, result)
            return
        if local is not None and local.is_local(lambda_):
            captured = []
            closure = lambda_.environment.closure if lambda_.environment else ()
            for var in closure:
                if var in self.stack_cells:
                    captured.append(self.stack_cells[var])
                    continue
                if var in local.stack_cells:
                    source = self.var_tn(var)
                else:
                    value = self.captured_value(node, var)
                    reference = local.function_for_binding(var) is not None
                    source = self.component.tn(
                        REFERENCE_T if reference else T,
                        name=f"$capture:{lambda_.name}:{var.name}",
                    )
                    self.emit(vops.MOVE, node, args=(value,), results=(source,))
                address = self.temporary(REFERENCE_T)
                self.emit(vops.LOCAL_ADDRESS, node, args=(source,), results=(address,))
                captured.append(address)
            result, discard = self.result_tn(node)
            result.primitive_type = REFERENCE_T
            self.emit(
                vops.LOCAL_FUNCTION,
                node,
                args=captured,
                results=(result,),
                info=lambda_,
            )
            if discard:
                self.emit_discard(node, result)
            return
        closure = lambda_.environment.closure if lambda_.environment else ()
        captured = [self.captured_value(node, var) for var in closure]
        defaults = [self.lvar_tn(arg) for arg in node.args[1:]]
        result, discard = self.result_tn(node)
        self.emit(
            node.template,
            node,
            args=[*defaults, *captured],
            results=(result, self.temporary(), self.temporary()),
            info=FunctionObject(lambda_, node.info),
        )
        if discard:
            self.emit_discard(node, result)

    def is_exact_list_append(self, resolve: Combination, call) -> bool:
        if resolve.fun_info is not fndb.RESOLVE_METHOD:
            return False
        receiver = self.context.type_of(resolve.args[0].use())
        info = linked(resolve.info, resolve, "method info")
        return (
            receiver.is_exact_list
            and info.name == "append"
            and call is not None
            and call.accept(_EXACT_LIST_APPEND_CALL)
        )

    def is_exact_list_pop(self, resolve: Combination, call) -> bool:
        if resolve.fun_info is not fndb.RESOLVE_METHOD:
            return False
        receiver = self.context.type_of(resolve.args[0].use())
        info = linked(resolve.info, resolve, "method info")
        return (
            receiver.is_exact_list
            and info.name == "pop"
            and call is not None
            and call.accept(_EXACT_LIST_POP_CALL)
        )

    def is_exact_text_find(self, resolve: Combination, call) -> bool:
        if resolve.fun_info is not fndb.RESOLVE_METHOD:
            return False
        receiver = self.context.type_of(resolve.args[0].use())
        info = linked(resolve.info, resolve, "method info")
        return (
            receiver.fits_text(TEXT_TYPE)
            and info.name in ("find", "rfind")
            and call is not None
            and call.accept(_ExactTextFindCall(self.context))
        )

    def is_exact_text_split(self, resolve: Combination, call) -> bool:
        if resolve.fun_info is not fndb.RESOLVE_METHOD:
            return False
        receiver = self.context.type_of(resolve.args[0].use())
        info = linked(resolve.info, resolve, "method info")
        return (
            receiver.fits_text(TEXT_TYPE)
            and info.name == "split"
            and call is not None
            and call.accept(_ExactTextSplitCall(self.context))
        )

    def is_exact_text_partition(self, resolve: Combination, call) -> bool:
        if resolve.fun_info is not fndb.RESOLVE_METHOD:
            return False
        receiver = self.context.type_of(resolve.args[0].use())
        info = linked(resolve.info, resolve, "method info")
        return (
            receiver.fits_text(TEXT_TYPE)
            and info.name in ("partition", "rpartition")
            and call is not None
            and call.accept(_ExactTextPartitionCall(self.context))
        )

    def is_exact_text_affix(self, resolve: Combination, call) -> bool:
        if resolve.fun_info is not fndb.RESOLVE_METHOD:
            return False
        receiver = self.context.type_of(resolve.args[0].use())
        info = linked(resolve.info, resolve, "method info")
        return (
            receiver.fits_text(TEXT_TYPE)
            and info.name in ("startswith", "endswith")
            and call is not None
            and call.accept(_ExactTextAffixCall(self.context))
        )

    def is_exact_dict_get(self, resolve: Combination, call) -> bool:
        if resolve.fun_info is not fndb.RESOLVE_METHOD:
            return False
        receiver = self.context.type_of(resolve.args[0].use())
        info = linked(resolve.info, resolve, "method info")
        return (
            receiver.fits_dict()
            and info.name == "get"
            and call is not None
            and call.accept(_EXACT_DICT_GET_CALL)
        )

    def is_exact_bytearray_append(self, resolve: Combination, call) -> bool:
        if resolve.fun_info is not fndb.RESOLVE_METHOD:
            return False
        receiver = self.context.type_of(resolve.args[0].use())
        info = linked(resolve.info, resolve, "method info")
        return (
            receiver.fits_bytearray()
            and info.name == "append"
            and call is not None
            and call.accept(_EXACT_LIST_APPEND_CALL)
        )

    def convert_resolve_method(self, node: Combination):
        args = tuple(self.lvar_tn(arg) for arg in node.args)
        lvar = node.lvar
        if lvar is None:
            raise ValueError("a resolved method is always consumed by its call")
        exact_list_append = self.is_exact_list_append(node, lvar.dest)
        exact_list_pop = self.is_exact_list_pop(node, lvar.dest)
        exact_text_find = self.is_exact_text_find(node, lvar.dest)
        exact_text_split = self.is_exact_text_split(node, lvar.dest)
        exact_text_partition = self.is_exact_text_partition(node, lvar.dest)
        exact_text_affix = self.is_exact_text_affix(node, lvar.dest)
        exact_dict_get = self.is_exact_dict_get(node, lvar.dest)
        exact_bytearray_append = self.is_exact_bytearray_append(node, lvar.dest)
        method = self.lvar_tn(lvar)
        bound = self.component.tn(T, name="receiver")
        info = linked(node.info, node, "method info")
        resolution = MethodResolution(
            info.name,
            exact_list_append,
            exact_list_pop,
            exact_text_find,
            exact_text_split,
            exact_text_partition,
            exact_text_affix,
            exact_dict_get,
            exact_bytearray_append,
        )
        if resolution.is_exact:
            method.type = NoneType()
            bound.type = args[0].type
        else:
            bound.type = UNKNOWN
        linked(lvar.info, lvar, "LTN annotation").locs.append(bound)
        self.emit(
            node.template,
            node,
            args=args,
            results=(method, bound),
            info=resolution,
        )

    def convert_call_resolved(self, node: Combination):
        resolved = node.args[0]
        method, bound = linked(resolved.info, resolved, "LTN annotation").locs
        resolve = resolved.use().accept(_MethodResolution(resolved))
        info = linked(node.info, node, "call info")
        exact_list_append = self.is_exact_list_append(resolve, node)
        exact_list_pop = self.is_exact_list_pop(resolve, node)
        exact_text_find = self.is_exact_text_find(resolve, node)
        exact_text_split = self.is_exact_text_split(resolve, node)
        exact_text_partition = self.is_exact_text_partition(resolve, node)
        exact_text_affix = self.is_exact_text_affix(resolve, node)
        exact_dict_get = self.is_exact_dict_get(resolve, node)
        exact_bytearray_append = self.is_exact_bytearray_append(resolve, node)
        result, discard = self.result_tn(node)
        self.emit(
            node.template,
            node,
            args=(method, bound, *(self.lvar_tn(arg) for arg in node.args[1:])),
            results=(result,),
            info=MethodCall(
                info.keywords,
                linked(resolve.info, resolve, "method info").name,
                exact_list_append,
                exact_list_pop,
                exact_text_find,
                exact_text_split,
                exact_text_partition,
                exact_text_affix,
                exact_dict_get,
                exact_bytearray_append,
                self.context.targets.get(node),
                self.context.type_of(resolve.args[0].use())
                if len(resolve.args[0].uses) == 1
                else UNKNOWN,
            ),
        )
        if discard:
            self.emit_discard(node, result)

    def convert_range(self, node: Combination):
        info = node.info
        state = self.range_tn(info)
        step_sign = info.step_sign if info is not None else 0
        if (
            info is not None
            and step_sign == 0
            and len(info.arguments) == 3
            and len(info.arguments[2].uses) == 1
        ):
            step_type = self.context.type_of(info.arguments[2].use())
            if step_type.minimum is not None and step_type.minimum > 0:
                step_sign = 1
            elif step_type.maximum is not None and step_type.maximum < 0:
                step_sign = -1
        if node in self._fused_range_prepares:
            iterator_var, callee_leaf = self._fused_range_prepares[node]
            arg_tns = [self.lvar_tn(arg) for arg in info.arguments]
            result, discard = self.result_tn(node)
            iterator_tn = self.var_tn(iterator_var)
            fused_info = replace(
                info,
                step_sign=step_sign,
                fused=True,
                callee_leaf=callee_leaf,
            )
            self.emit(
                node.template,
                node,
                args=(*arg_tns, state),
                results=(result, iterator_tn),
                info=fused_info,
            )
            if discard:
                self.emit_discard(node, result)
            return
        args = [self.lvar_tn(arg) for arg in node.args]
        codegen_info = (
            replace(info, step_sign=step_sign)
            if info is not None and step_sign != info.step_sign
            else info
        )
        self.convert_template(node, [*args, state], codegen_info)

    def visit_Node(self, node):
        return None


__all__ = [
    "BranchTargets",
    "CallSite",
    "FunctionObject",
    "LocalAccess",
    "MethodCall",
    "TypeCheck",
    "ir2_convert",
]
