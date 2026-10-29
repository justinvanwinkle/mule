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
from .annotation_types import InferredReturnContract
from .callconv import TupleDelivery, tuple_return_convention

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
    ValueTypeVisitor,
)
from .ir1util import block_exception_target, block_start_cleanup, made_lambda
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
    DOUBLE,
    FIXNUM,
    RANGE_T,
    REFERENCE_T,
    SIGNED,
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
        "protocol-dict_contains",
        "protocol-dict_not_contains",
        "protocol-dict_len",
        "protocol-table_len",
        "protocol-set_add",
        "protocol-set_discard",
        "protocol-set_remove",
        "protocol-set_contains",
        "protocol-set_not_contains",
        "protocol-text_contains",
        "protocol-text_not_contains",
        "protocol-set_and",
        "protocol-set_or",
        "protocol-set_sub",
        "protocol-set_xor",
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
_FIXNUM_BINARY_VOPS = frozenset(
    {
        "fixnum-add",
        "fixnum-subtract",
        "fixnum-multiply",
        "fixnum-bit_and",
        "fixnum-bit_or",
        "fixnum-bit_xor",
        "fixnum-left_shift",
        "fixnum-right_shift",
        "fixnum-floor_divide",
        "fixnum-modulo",
    }
)
_FIXNUM_UNARY_VOPS = frozenset(
    {
        "fixnum-unary-positive",
        "fixnum-unary-negative",
        "fixnum-unary-invert",
    }
)
_FIXNUM_COMPARISON_VOPS = frozenset(
    {
        "fixnum-compare-equal",
        "fixnum-compare-not_equal",
        "fixnum-compare-less",
        "fixnum-compare-less_equal",
        "fixnum-compare-greater",
        "fixnum-compare-greater_equal",
    }
)
_PURE_FIXNUM_VOPS = (
    _FIXNUM_BINARY_VOPS
    | _FIXNUM_UNARY_VOPS
    | frozenset(
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
    if vop.info is vops.MOVE and vop.codegen_info is DEAD_LOCAL_STORE:
        return True
    name = vop.info.name
    if name in (
        "physical-field-read",
        "physical-field-write",
        "physical-field-borrow-end",
    ):
        return True
    if name in _FIXNUM_BINARY_VOPS | _FIXNUM_UNARY_VOPS | _FIXNUM_COMPARISON_VOPS:
        return True
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
        rhs_type = vop.args[1].tn.type or UNKNOWN
        return rhs_type.fits_dict() or rhs_type.fits_set()
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
    if name in _FIXNUM_COMPARISON_VOPS:
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


@dataclass(frozen=True, slots=True)
class DirectSequenceIteration:
    seq_type: ValueType
    iterator_local: object
    index_tn: object
    seq_local: object | None = None
    borrow_element: bool = False


def ir2_convert(
    lambda_: Lambda, context: EntryContext, *, ltn_token: object | None = None
) -> IR2Component:
    """Convert ``lambda_`` under ``context`` into VOPs.

    A token from LTN permits reuse of its derived types only while the graph
    and context facts remain unchanged since that analysis.
    """
    environment = IR2Environment(lambda_, context)
    component = IR2Component(lambda_, context, environment)
    component.tuple_return = tuple_return_convention(lambda_, context)
    converter = _Converter(component, ltn_token=ltn_token)
    converter.prepare_environment()
    blocks = list(linked(lambda_.component, lambda_, "component").blocks())
    for index, block in enumerate(blocks):
        ir2_block = IR2Block(block, index, block_exception_target(block))
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
    _specialize_sequence_iterations(component)
    _mark_last_local_reads(component)
    _propagate_local_aliases(component)
    _mark_boolean_comparisons(component)
    _mark_borrowed_local_reads(component)
    _specialize_list_getitems(component)
    _borrow_fixed_slot_reads(component)
    _specialize_list_getitems(component)
    _clear_lvar_info(lambda_)
    return component


def select_tuple_delivery(component, arities, method_targets):
    """Deliver unpacked values through fixed locations using IR2 def-use edges.

    A region owns the unconsumed slots from acquisition through each projection.
    Regions cannot overlap because this ABI uses one output buffer per frame.
    Target instructions are consumers, not part of the matching grammar.
    """
    plan = component.tuple_delivery = TupleDelivery()
    if component.context.plan is not None:
        return
    reads, writes = component.reads, component.writes
    positions = {
        op: (block, i) for block in component.blocks for i, op in enumerate(block.vops)
    }

    def definition(tn):
        defs = writes.get(tn, ())
        return defs[0][0] if len(defs) == 1 else None

    def sole_reader(tn):
        uses = reads.get(tn, ())
        return uses[0][0] if len(uses) == 1 else None

    for block in component.blocks:
        occupied = set()
        for unpack in block.vops:
            if unpack.info.name != "structural-unpack":
                continue
            arity = unpack.codegen_info.arity
            if arity is None or not 1 <= arity <= 8:
                continue
            captured = sole_reader(unpack.results[0].tn)
            if captured is None or captured.info is not vops.MOVE:
                continue
            root = captured.results[0].tn
            if (
                root.leaf is None
                or not root.leaf.expression_temporary
                or len(writes.get(root, ())) != 1
            ):
                continue
            projections = []
            discards = []
            valid = True
            for reader, _ in reads.get(root, ()):
                if (
                    reader.info.name == "unpack-ref"
                    and reader.codegen_info.borrow_parent
                ):
                    projections.append(reader)
                elif reader.info is vops.LOAD_LOCAL and reader.codegen_info.move:
                    discard = sole_reader(reader.results[0].tn)
                    if discard is None or discard.info is not vops.DISCARD:
                        valid = False
                        break
                    discards.append(reader)
                else:
                    valid = False
                    break
            if not valid or len(discards) != 1 or len(projections) != arity:
                continue
            if any(
                positions.get(op, (None,))[0] is not block
                for op in [captured, *projections, *discards]
            ):
                continue
            projections.sort(key=lambda op: positions[op][1])
            if [op.codegen_info.index for op in projections] != list(range(arity)):
                continue
            if not (
                positions[unpack][1]
                < positions[captured][1]
                < positions[projections[0]][1]
                and positions[projections[-1]][1] < positions[discards[0]][1]
            ):
                continue
            producer = definition(unpack.args[0].tn)
            assertion = None
            if producer is not None and producer.info is vops.TYPE_CHECK:
                assertion = producer
                if sole_reader(assertion.results[0].tn) is not unpack:
                    continue
                producer = definition(producer.args[0].tn)
                if not vops.checks_unpacked_return(
                    assertion.codegen_info.contract, arity
                ):
                    continue
            call = None
            if producer is not None and sole_reader(producer.results[0].tn) in (
                assertion,
                unpack,
            ):
                if producer.info is vops.FULL_CALL:
                    site = producer.codegen_info
                    if (
                        arities.get(site.target) == arity
                        and site.target.call_order(
                            site.keywords, len(producer.args) - 1
                        )
                        is not None
                    ):
                        call = producer
                elif producer.info is vops.LOCAL_EXTENT_CALL:
                    target = producer.codegen_info.target
                    if (
                        arities.get(target) == arity
                        and target.signature.return_annotation is None
                    ):
                        call = producer
                elif (
                    producer.info is vops.CALL_RESOLVED
                    and producer.codegen_info.exact_text_partition
                    and arity == 3
                ):
                    call = producer
            if assertion is not None and call is None:
                continue
            start = producer if call is not None else unpack
            if positions.get(start, (None,))[0] is not block:
                continue
            first, last = positions[start][1], positions[discards[0]][1]
            interval = set(range(first, last + 1))
            if interval & occupied:
                continue
            # Unknown tuple/iterable results can use the same slot ABI. This
            # helper validates and extracts all children before any target runs.
            if call is not None:
                plan.calls[call] = arity
                plan.unpacks.add(unpack)
                call.results[0].tn.type = NONE_TYPE
                if assertion is not None:
                    plan.call_assertions[call] = assertion
                    assertion.results[0].tn.type = NONE_TYPE
            else:
                plan.slot_unpacks[unpack] = arity
                if (
                    producer is not None
                    and producer.info.name == "protocol-next_item"
                    and (producer.args[0].tn.type or UNKNOWN).fits_none(NONE_TYPE)
                    and len(block.block.pred) == 1
                ):
                    predecessor = component.ir2_blocks.get(block.block.pred[0])
                    if predecessor is not None:
                        branches = [
                            op
                            for op in predecessor.vops
                            if op.info is vops.BRANCH
                            and op.codegen_info.consequent == block.label
                        ]
                        if len(branches) == 1:
                            advance = definition(branches[0].args[0].tn)
                            if (
                                advance is not None
                                and advance.info.name == "sequence-available"
                                and advance.codegen_info is None
                            ):
                                plan.sequence_available[advance] = arity
            occupied.update(interval)
            plan.moves.add(captured)
            plan.discard_loads.add(discards[0])
            plan.temp_tns.add(root)
            plan.max_arity = max(plan.max_arity, arity)
            for tn in (unpack.results[0].tn, root, discards[0].results[0].tn):
                tn.type = NONE_TYPE
            remaining = tuple(range(arity))
            for op in block.vops[first + 1 : last + 1]:
                plan.remaining[op] = remaining
                if op in projections:
                    slot = op.codegen_info.index
                    plan.refs[op] = (slot, arity)
                    remaining = tuple(index for index in remaining if index != slot)

    # Fold return checks into direct-call boundaries using the same def-use index.
    for block in component.blocks:
        for call in block.vops:
            if call in plan.calls or call.info not in (
                vops.FULL_CALL,
                vops.LOCAL_EXTENT_CALL,
                vops.CALL_RESOLVED,
            ):
                continue
            assertion = sole_reader(call.results[0].tn)
            if assertion is None or assertion.info is not vops.TYPE_CHECK:
                continue
            if call.info is vops.CALL_RESOLVED:
                info = call.codegen_info
                if info.is_exact:
                    continue
                target = info.target
                count = len(call.args) - 2
                direct = (
                    target is not None
                    and not info.keywords
                    and target.call_order((), count + 1) is not None
                )
                direct = direct or any(
                    candidate.call_order(info.keywords, count, receiver=bound)
                    is not None
                    for candidate in method_targets.get(info.name, ())
                    for bound in (True, False)
                )
            else:
                site = call.codegen_info
                direct = (
                    site.target is not None
                    and site.target.call_order(site.keywords, len(call.args) - 1)
                    is not None
                )
            if direct:
                plan.direct_assertions[call] = assertion


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


def _enclosing_exception_target(block):
    cleanup = block_start_cleanup(block)
    if cleanup is not None:
        cleanup = cleanup.parent
    while cleanup is not None:
        if cleanup.target is not None:
            return cleanup.target
        cleanup = cleanup.parent
    return None


def _prune_unreachable_ir2_blocks(component: IR2Component) -> None:
    if not component.blocks:
        return
    ir2_blocks = component.ir2_blocks
    reachable: set[IR2Block] = set()
    pending = [component.blocks[0]]
    while pending:
        cur = pending.pop()
        if cur in reachable:
            continue
        reachable.add(cur)
        for succ in cur.block.succ:
            succ_ir2 = ir2_blocks.get(succ)
            if succ_ir2 is not None and succ_ir2 not in reachable:
                pending.append(succ_ir2)
        exc_ir2 = ir2_blocks.get(cur.exception_target)
        if exc_ir2 is not None and exc_ir2 not in reachable:
            pending.append(exc_ir2)
    if len(reachable) == len(component.blocks):
        return
    component.blocks = [b for b in component.blocks if b in reachable]
    component.ir2_blocks = {
        b: ir2_b for b, ir2_b in ir2_blocks.items() if ir2_b in reachable
    }
    component.reads.clear()
    component.writes.clear()
    for block in component.blocks:
        for vop in block.vops:
            for ref in vop.args:
                component.reads[ref.tn].append((vop, ref))
            for ref in vop.results:
                component.writes[ref.tn].append((vop, ref))
    component.tns = [
        tn
        for tn in component.tns
        if tn.leaf is not None
        or (tn.name is not None and tn.name.startswith("$"))
        or tn in component.reads
        or tn in component.writes
    ]


def _specialize_sequence_iterations(component: IR2Component) -> None:
    """Specialize ``for`` loops over proven exact ``list``, ``tuple``, and ``bytes``
    sequences to iterate directly over the sequence buffer with a stack-allocated
    ``i64`` index instead of heap-allocating a ``SeqIter`` iterator object."""
    if component.context.plan is not None or component.lambda_.coroutine_body:
        return
    reads = component.reads
    writes = component.writes
    vop_blocks = {vop: block for block in component.blocks for vop in block.vops}
    pruned_handlers = False
    for block in component.blocks:
        for vop in block.vops:
            if (
                vop.info.name != "protocol-iterate"
                or len(vop.args) != 1
                or not vop.results
            ):
                continue
            seq_tn = vop.args[0].tn
            seq_type = seq_tn.type or UNKNOWN
            if seq_type.may_be_unbound or not (
                seq_type.is_exact_list or seq_type.is_exact_tuple
            ):
                continue
            iter_res = vop.results[0].tn
            iter_reads = reads.get(iter_res, ())
            if len(iter_reads) != 1:
                continue
            move_vop, _ = iter_reads[0]
            if move_vop.info is not vops.MOVE or not move_vop.results:
                continue
            iterator_local = move_vop.results[0].tn
            if (
                not _plain_local(iterator_local)
                or iterator_local.leaf is None
                or not iterator_local.leaf.hidden
                or iterator_local.leaf in component.environment.cells
                or iterator_local.leaf in component.environment.copies
                or len(writes.get(iterator_local, ())) != 1
            ):
                continue
            local_reads = reads.get(iterator_local, ())
            avail_vops = []
            valid = True
            for load_vop, _ in local_reads:
                if (
                    load_vop.info is not vops.LOAD_LOCAL
                    or load_vop.codegen_info is None
                    or not load_vop.codegen_info.bound
                    or not load_vop.results
                ):
                    valid = False
                    break
                loaded_tn = load_vop.results[0].tn
                loaded_reads = reads.get(loaded_tn, ())
                if len(loaded_reads) != 1:
                    valid = False
                    break
                consumer, _ = loaded_reads[0]
                if consumer.info.name == "sequence-available":
                    avail_vops.append(consumer)
                elif consumer.info is not vops.DISCARD:
                    valid = False
                    break
            if not valid or len(avail_vops) != 1:
                continue
            next_vops = [
                step
                for b in component.blocks
                for step in b.vops
                if step.info.name == "protocol-next_item"
                and step.node is not None
                and step.node.info is iterator_local.leaf
            ]
            if len(next_vops) != 1:
                continue
            seq_local = None
            seq_writes = writes.get(seq_tn, ())
            if len(seq_writes) == 1:
                seq_writer, _ = seq_writes[0]
                if (
                    seq_writer.info is vops.LOAD_LOCAL
                    and seq_writer.codegen_info is not None
                    and seq_writer.codegen_info.bound
                    and _plain_local(seq_writer.args[0].tn)
                ):
                    seq_local = seq_writer.args[0].tn
            index_tn = component.tn(
                FIXNUM, name=f"$seq_iter_idx.{iterator_local.number}"
            )
            index_tn.type = IntegerType(0, (1 << 62) - 1)
            index_tn.sc = SIGNED
            iter_res.type = seq_type
            iterator_local.type = seq_type
            for load_vop, _ in local_reads:
                load_vop.results[0].tn.type = seq_type
            info = DirectSequenceIteration(
                seq_type=seq_type,
                iterator_local=iterator_local,
                index_tn=index_tn,
                seq_local=seq_local,
            )
            vop.codegen_info = info
            avail_vop = avail_vops[0]
            avail_vop.codegen_info = info
            next_vops[0].codegen_info = info
            avail_block = vop_blocks.get(avail_vop)
            if avail_block is not None and all(
                step.info.name
                in ("insert-safepoint", "load-local", "sequence-available", "branch")
                for step in avail_block.vops
            ):
                avail_block.exception_target = _enclosing_exception_target(
                    avail_block.block
                )
                pruned_handlers = True
    if pruned_handlers:
        _prune_unreachable_ir2_blocks(component)


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


def _propagate_local_aliases(component: IR2Component) -> None:
    """Substitute stable, uncaptured local copies before ownership selection.

    A local copy has the same value as its source, but may keep that value
    alive independently. The lifetime proof must cover its consumers and every
    reachable owner transfer, not just the textual assignment and last read.
    Each rewrite refreshes last-read information before the next proof.
    """
    if component.context.plan is not None:
        return
    while True:
        proof = None
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
                if proof is None:
                    proof = _BorrowedLifetime(component)
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
            return
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
            handler = component.ir2_blocks.get(block.exception_target)
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
                if reader.info is vops.MOVE and reader.codegen_info is DEAD_LOCAL_STORE:
                    consumers.add(reader)
                    continue
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

    def protected(
        self,
        acquisition,
        consumers,
        source,
        extra_boundaries=(),
        *,
        container_borrow: bool = False,
    ):
        if not consumers:
            return False
        start, start_index = self.positions[acquisition]
        boundary = start, start_index + 1
        boundaries = (
            {
                boundary,
                *(
                    (b, idx + 1)
                    for step in extra_boundaries
                    for b, idx in (self.positions[step],)
                ),
            }
            if extra_boundaries
            else {boundary}
        )
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
            if point in boundaries or point in visited:
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
                    step,
                    source,
                    consumer=step in consumers,
                    container_borrow=container_borrow,
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
        handler = self.component.ir2_blocks.get(block.exception_target)
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
            handler = self.component.ir2_blocks.get(following.exception_target)
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
            handler = self.component.ir2_blocks.get(block.exception_target)
            if handler is not None:
                pending.append((handler, 0, live and step not in ends))
            pending.append((block, index + 1, live and step not in ends))
        return True

    @staticmethod
    def safe_step(step, source, *, consumer=False, container_borrow=False):
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
        if (
            name.startswith("inplace-")
            or (
                name.startswith("boxed-float-")
                and step.codegen_info is not None
                and step.codegen_info.reuse_local is not None
            )
            or name
            in (
                "binary-add",
                "binary-subtract",
                "binary-multiply",
            )
        ):
            return bool(
                len(step.args) == 2
                and all((arg.tn.type or UNKNOWN).fits(REAL) for arg in step.args)
            )
        if name in vops.TERMINATORS:
            return True
        return _safe_borrow_step(step, source)


_BORROWED_LIST_GETITEM_READERS = (
    _CODEPOINT_COMPARISON_VOPS | _FIXNUM_COMPARISON_VOPS | _FIXNUM_UNARY_VOPS
) | frozenset(
    {
        "compare-is",
        "compare-is_not",
        "protocol-iterate",
        "fast-len",
        "primitive-_storage_length",
        "primitive-_text_length",
        "boxed-float-compare-equal",
        "boxed-float-compare-not_equal",
        "boxed-float-compare-less",
        "boxed-float-compare-less_equal",
        "boxed-float-compare-greater",
        "boxed-float-compare-greater_equal",
        "boxed-float-add",
        "boxed-float-subtract",
        "boxed-float-multiply",
        "boxed-float-divide",
        "boxed-float-power",
        "mixed-float-add-left",
        "mixed-float-add-right",
        "mixed-float-subtract-left",
        "mixed-float-subtract-right",
        "mixed-float-multiply-left",
        "mixed-float-multiply-right",
        "mixed-float-divide-left",
        "mixed-float-divide-right",
        "mixed-real-float-add-left",
        "mixed-real-float-add-right",
        "mixed-real-float-subtract-left",
        "mixed-real-float-subtract-right",
        "mixed-real-float-multiply-left",
        "mixed-real-float-multiply-right",
        "mixed-real-float-divide-left",
        "mixed-real-float-divide-right",
    }
)

_PURE_FLOAT_STEP_VOPS = frozenset(
    {
        "boxed-float-negative",
        "boxed-float-add",
        "boxed-float-subtract",
        "boxed-float-multiply",
        "boxed-float-divide",
        "boxed-float-power",
        "mixed-float-add-left",
        "mixed-float-add-right",
        "mixed-float-subtract-left",
        "mixed-float-subtract-right",
        "mixed-float-multiply-left",
        "mixed-float-multiply-right",
        "mixed-float-divide-left",
        "mixed-float-divide-right",
        "mixed-real-float-add-left",
        "mixed-real-float-add-right",
        "mixed-real-float-subtract-left",
        "mixed-real-float-subtract-right",
        "mixed-real-float-multiply-left",
        "mixed-real-float-multiply-right",
        "mixed-real-float-divide-left",
        "mixed-real-float-divide-right",
        "boxed-float-compare-equal",
        "boxed-float-compare-not_equal",
        "boxed-float-compare-less",
        "boxed-float-compare-less_equal",
        "boxed-float-compare-greater",
        "boxed-float-compare-greater_equal",
    }
)

_PURE_BINARY_ARITH_VOPS = _FIXNUM_BINARY_VOPS | frozenset(
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
    if name in (
        "physical-field-read",
        "physical-field-read-name",
        "physical-field-read-guarded",
        "primitive-_instance_field",
    ):
        return (reader_ref is None or reader_ref is reader.args[0]) and all(
            (arg.tn.type or UNKNOWN).accept(_INERT_LOCAL_VALUE)
            for arg in reader.args[1:]
        )
    if name in _BORROWED_LIST_GETITEM_READERS:
        if name.startswith("boxed-float-"):
            return (
                reader.codegen_info is None or reader.codegen_info.reuse_local is None
            )
        return True
    if name == "coerce" and reader.results:
        result = reader.results[0].tn
        return result.sc is DOUBLE or (
            result.sc is SIGNED and result.primitive_type is FIXNUM
        )
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
    if name == "primitive-_storage_append" and len(reader.args) == 2:
        return reader_ref is reader.args[0]
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
    if name == "physical-field-borrow-end":
        # This marker only extends the receiver's SSA lifetime.
        return True
    if name in (
        "physical-field-read",
        "physical-field-read-name",
        "physical-field-read-guarded",
        "primitive-_instance_field",
    ):
        # An unrelated read may destroy its last receiver or its result.
        # Those releases can run callbacks even though the lookup is physical.
        return False
    if name == "move":
        target = step.results[0].tn
        return (
            step.codegen_info is False
            or target.borrowed_from is not None
            or (target.type or UNKNOWN).accept(_INERT_LOCAL_VALUE)
        )
    if name in ("constant", "coerce"):
        return True
    if name == "discard":
        return bool(step.args and step.args[0].tn.borrowed_from is not None)
    if name in ("unpack-ref", "structural-unpack"):
        return step.codegen_info is not None and bool(step.codegen_info.proved)
    if name == "type-check":
        check: TypeCheck = linked(step.codegen_info, step, "type-check info")
        actual = step.args[0].tn.type or UNKNOWN
        return not check.safety or actual.fits(check.contract.checked_type)
    if name == "load-local":
        access: LocalAccess = linked(step.codegen_info, step, "local access")
        return bool(access.bound) and not (
            step.args[0].tn in (seq_local, root_local) and access.move
        )
    if name in _PURE_FLOAT_STEP_VOPS:
        return step.codegen_info is None or step.codegen_info.reuse_local is None
    if name in _FIXNUM_COMPARISON_VOPS:
        return True
    if name in _PURE_FIXNUM_VOPS and step.results:
        return (step.results[0].tn.type or UNKNOWN).is_fixnum and all(
            (arg.tn.type or UNKNOWN).is_fixnum for arg in step.args
        )
    if name == "protocol-next_item":
        info = step.codegen_info
        return info is not None and (
            info.seq_type.is_exact_list or info.seq_type.is_exact_tuple
        )
    if name == "protocol-getitem" and len(step.args) == 2:
        seq_type = step.args[0].tn.type or UNKNOWN
        return (seq_type.is_exact_list or seq_type.is_exact_tuple) and (
            step.args[1].tn.type or UNKNOWN
        ).is_fixnum
    if name == "primitive-_storage_read" and len(step.args) == 2:
        return (step.args[1].tn.type or UNKNOWN).is_fixnum
    return name == "primitive-_storage_length" and len(step.args) == 1


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
        coerced_res_tn = None
        if vops_list[cursor].info is vops.COERCE:
            coerce_vop = vops_list[cursor]
            if (
                coerce_vop.args[0].tn is not ref_res_tn
                or not coerce_vop.results
                or coerce_vop.results[0].tn.sc not in (DOUBLE, SIGNED)
                or cursor + 1 >= n_vops
            ):
                return False
            ref_res_tn = coerce_vop.results[0].tn
            coerced_res_tn = ref_res_tn
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
            (
                ref_vop,
                orig_ref_res_tn,
                checked_res_tn,
                coerced_res_tn,
                store_vop,
                cursor,
            )
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
        orig_ref_res_tn,
        checked_res_tn,
        coerced_res_tn,
        store_vop,
        store_pos,
    ) in slot_records:
        if coerced_res_tn is not None:
            ref_vop.codegen_info = replace(ref_vop.codegen_info, borrow_element=True)
            orig_ref_res_tn.borrowed_from = root_local
            if checked_res_tn is not None:
                checked_res_tn.borrowed_from = root_local
            continue
        slot_target = store_vop.results[0].tn
        slot_reads = reads.get(slot_target, ())
        if (
            slot_target.leaf is not None
            and slot_target.leaf.expression_temporary
            and len(writes.get(slot_target, ())) == 1
            and len(slot_reads) == 1
            and slot_reads[0][0].info is vops.LOAD_LOCAL
            and slot_reads[0][0].codegen_info is not None
            and slot_reads[0][0].codegen_info.bound
        ):
            load_vop = slot_reads[0][0]
            load_pos_info = proof.positions.get(load_vop)
            if (
                load_pos_info is not None
                and load_pos_info[0] is block
                and load_pos_info[1] > store_pos
                and _try_borrow_unpacked_getitem(
                    component,
                    block,
                    load_pos_info[1],
                    load_vop,
                    replace(load_vop.codegen_info, move=False, borrow=True),
                    seq_local,
                    root_local,
                    writer_index,
                )
            ):
                ref_vop.codegen_info = replace(
                    ref_vop.codegen_info, borrow_element=True
                )
                orig_ref_res_tn.borrowed_from = root_local
                slot_target.borrowed_from = root_local
                store_vop.codegen_info = False
                if checked_res_tn is not None:
                    checked_res_tn.borrowed_from = root_local
                continue
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
    if name in (
        "constant",
        "coerce",
        "jump",
        "insert-safepoint",
        "discard",
        "physical-field-borrow-end",
    ):
        return True
    if name in _PURE_FLOAT_STEP_VOPS:
        return step.codegen_info is None or step.codegen_info.reuse_local is None
    if name in _FIXNUM_COMPARISON_VOPS | _FIXNUM_UNARY_VOPS:
        return True
    if name in ("unpack-ref", "structural-unpack"):
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
        "primitive-_text_codepoint",
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
    if name == "protocol-next_item":
        info = step.codegen_info
        return info is not None and (
            info.seq_type.is_exact_list or info.seq_type.is_exact_tuple
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
            if not vop.results:
                continue
            if name == "protocol-next_item":
                info = vop.codegen_info
                if (
                    info is None
                    or len(vop.args) != 1
                    or not (info.seq_type.is_exact_list or info.seq_type.is_exact_tuple)
                ):
                    continue
                if info.borrow_element:
                    continue
                borrow_info = replace(info, borrow_element=True)
                seq_local = (
                    info.seq_local
                    if info.seq_local is not None
                    else info.iterator_local
                )
                root_local = (
                    info.iterator_local.borrowed_from
                    if info.iterator_local.borrowed_from is not None
                    else info.iterator_local
                )
                if positions is None:
                    positions = {step: pos for pos, step in enumerate(block.vops)}
                writer_index = index
            else:
                if (
                    vop.codegen_info not in (None, "list_owned", "tuple_owned")
                    or len(vop.args) != 2
                ):
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
                seq_tn = vop.args[0].tn
                seq_writes = writes.get(seq_tn)
                if seq_writes is None or len(seq_writes) != 1:
                    continue
                seq_writer, _ = seq_writes[0]
                if (
                    seq_writer.info is vops.LOAD_LOCAL
                    and seq_writer.codegen_info is not None
                    and seq_writer.codegen_info.bound
                    and not seq_writer.codegen_info.move
                ):
                    seq_local = seq_writer.args[0].tn
                    if not _plain_local(seq_local):
                        continue
                    root_local = (
                        seq_local.borrowed_from
                        if seq_local.borrowed_from is not None
                        else seq_local
                    )
                elif (
                    seq_writer.info.name in ("getattr", "physical-field-read-borrow")
                    and seq_tn.borrowed_from is not None
                    and _plain_local(seq_tn.borrowed_from)
                ):
                    seq_local = seq_tn.borrowed_from
                    root_local = seq_local
                else:
                    continue
                if positions is None:
                    positions = {step: pos for pos, step in enumerate(block.vops)}
                writer_index = positions.get(seq_writer)
                if writer_index is None or writer_index >= index:
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
            reader, reader_ref = res_reads[0]
            reader_index = positions.get(reader)
            if reader_index is None or reader_index <= index:
                continue
            if (
                name in ("protocol-getitem", "protocol-next_item")
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
            if reader.info is vops.MOVE and all(
                step.info.name == "physical-field-borrow-end"
                for step in block.vops[index + 1 : reader_index]
            ):
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
                    move_index=reader_index,
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


def _safe_fixed_slot_region_step(component, step, value, root) -> bool:
    if step.info.name == "move":
        if not _safe_container_local_region_step(
            component, step, value, root, None, value.type or UNKNOWN
        ):
            return False
        target = step.results[0].tn
        # A dead store still releases the previous binding. Its weak-reference
        # callbacks can replace the borrowed field, even for function values.
        return (
            step.codegen_info is False
            or target.borrowed_from is not None
            or (target.type or UNKNOWN).accept(_INERT_LOCAL_VALUE)
        )
    return _safe_list_borrow_step(step, root)


def _borrow_fixed_slot_reads(component: IR2Component) -> None:
    """Borrow temporary slot values across a proven callback-free region.

    A nullable second result owns descriptor fallback values. The explicit end
    operation keeps both that owner and the receiver alive through the consumer.
    Local copies retain their existing ownership unless earlier alias analysis
    already proved they can disappear without changing destruction order.
    """
    if component.context.plan is not None:
        return
    for block in component.blocks:
        positions = {step: index for index, step in enumerate(block.vops)}
        endings = []
        for index, step in enumerate(block.vops):
            physical = step.info.name == "physical-field-read"
            if (step.info.name != "getattr" and not physical) or not step.results:
                continue
            value = step.results[0].tn
            receiver = step.args[0].tn
            attribute_name = (
                step.codegen_info.name
                if physical
                else linked(step.codegen_info, step, "attribute access").name
            )
            if (
                (receiver.type or UNKNOWN).fixed_slot_index(attribute_name) is None
                or value.leaf is not None
                or value.borrowed_from is not None
            ):
                continue
            uses = component.reads.get(value, ())
            origins = component.writes.get(receiver, ())
            if len(uses) != 1 or len(origins) != 1:
                continue
            consumer, ref = uses[0]
            end = positions.get(consumer)
            load, _ = origins[0]
            if end is None or end <= index or load.info is not vops.LOAD_LOCAL:
                continue
            access: LocalAccess = linked(load.codegen_info, load, "local access")
            if not access.bound:
                continue
            root = load.args[0].tn
            if not _plain_local(root):
                continue
            physical_read = (
                consumer.info.name
                in (
                    "primitive-_storage_read",
                    "primitive-_storage_length",
                    "physical-field-read",
                    "physical-field-read-name",
                    "physical-field-read-guarded",
                    "primitive-_instance_field",
                )
                and ref is consumer.args[0]
                and (
                    _can_borrow_list_getitem(consumer, ref)
                    if consumer.info.name.startswith("physical-field-read")
                    or consumer.info.name == "primitive-_instance_field"
                    else _safe_list_borrow_step(consumer, root)
                )
            )
            # Physical reads do not invoke the stored value's Python methods.
            # These physical mutations finish every array access before
            # releasing an old element or a failed moved append argument.
            terminal_storage = ref is consumer.args[0] and (
                (
                    consumer.info.name == "primitive-_storage_write"
                    and len(consumer.args) == 3
                    and (consumer.args[1].tn.type or UNKNOWN).is_fixnum
                )
                or (
                    consumer.info.name == "primitive-_storage_append"
                    and len(consumer.args) == 2
                )
            )
            if (
                not physical_read
                and not terminal_storage
                and consumer.info.name not in ("compare-is", "compare-is_not")
                and (
                    (value.type or UNKNOWN).may_run_user_code
                    or not _can_borrow_list_getitem(consumer, ref)
                )
            ):
                continue
            if any(
                not _safe_fixed_slot_region_step(component, other, value, root)
                for other in block.vops[index + 1 : end]
            ):
                continue
            value.borrowed_from = root
            # Keep the existing local owner until the consumer instead of
            # moving its final read into a short-lived receiver temporary.
            if access.move:
                load.codegen_info = replace(access, move=False, borrow=True)
                receiver.borrowed_from = root
            if physical:
                step.info = vops.PHYSICAL_FIELD_READ_BORROW
                ending = VOP(
                    vops.PHYSICAL_FIELD_BORROW_END,
                    consumer.node,
                    [TNRef(root, False)],
                    [],
                )
            else:
                owner = component.tn(T, name="slot.fallback.owner")
                owner.type = UNKNOWN
                step.info = vops.GETATTR_BORROW
                step.results.append(TNRef(owner, True))
                ending = VOP(
                    vops.SLOT_BORROW_END,
                    consumer.node,
                    [TNRef(owner, False), TNRef(root, False)],
                    [],
                )
            endings.append((end + 1, ending))
        for index, ending in reversed(endings):
            block.vops.insert(index, ending)
    # Later ownership, packing and representation passes consume these indices.
    component.reads.clear()
    component.writes.clear()
    for block in component.blocks:
        for step in block.vops:
            for ref in step.args:
                component.reads[ref.tn].append((step, ref))
            for ref in step.results:
                component.writes[ref.tn].append((step, ref))


DEAD_LOCAL_STORE = object()


class _InertLocalValue(ValueTypeVisitor):
    """Values whose earlier destruction cannot invoke finalizers or weakrefs.

    Operation purity does not imply destruction safety: functions, classes,
    and iterators can expose finalization through weak references. Mutable
    containers also require a lifetime-wide element proof, not a current type.
    """

    def visit_ValueType(self, fact):
        return False

    def _bound_scalar(self, fact):
        return not fact.may_be_unbound

    visit_IntegerType = _bound_scalar
    visit_PythonIntegerType = _bound_scalar
    visit_BooleanType = _bound_scalar
    visit_FloatType = _bound_scalar
    visit_NoneType = _bound_scalar
    visit_ImmortalType = _bound_scalar
    visit_TextType = _bound_scalar
    visit_BytesType = _bound_scalar
    visit_ByteArrayType = _bound_scalar

    def visit_TupleType(self, fact):
        return (
            fact.is_exact_tuple
            and fact.elements is not None
            and all(element.accept(self) for element in fact.elements)
        )

    def visit_UnionType(self, fact):
        return not fact.may_be_unbound and all(
            member.accept(self) for member in fact.members
        )


_INERT_LOCAL_VALUE = _InertLocalValue()


def _mark_dead_local_store(vop: VOP, live: set, candidates: set) -> None:
    if (
        vop.info is vops.MOVE
        and vop.results[0].tn in candidates
        and vop.results[0].tn not in live
        and (vop.args[0].tn.type or UNKNOWN).accept(_INERT_LOCAL_VALUE)
    ):
        # Releasing an arbitrary value here could invoke __del__ before the
        # binding's original overwrite or exit. Inert values have no such effect.
        vop.codegen_info = DEAD_LOCAL_STORE


def _mark_last_local_reads(component: IR2Component) -> None:
    """Mark ownership transfers and dead stores from instruction-level liveness.

    Handler reads, closed-over slots, borrowed roots, and suspended frame
    bindings retain the ownership required by their observable lifetimes.
    """
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
    borrowed_roots = {
        tn.borrowed_from for tn in component.tns if tn.borrowed_from is not None
    }
    dead_store_candidates = {
        tn
        for tn in candidates
        if tn.borrowed_from is None
        and tn not in borrowed_roots
        and tn.leaf not in environment.cells
        and tn.leaf not in environment.copies
        and tn not in visible_candidates
    }
    for block in component.blocks:
        for vop in block.vops:
            if vop.info is vops.MOVE and vop.codegen_info is DEAD_LOCAL_STORE:
                vop.codegen_info = True
            elif vop.info is vops.LOAD_LOCAL and (
                vop.args[0].tn in candidates
                or vop.args[0].tn.borrowed_from is vop.args[0].tn
            ):
                access: LocalAccess = linked(vop.codegen_info, vop, "local access")
                if access.move:
                    vop.codegen_info = replace(access, move=False)
    if not candidates:
        return
    blocks = component.blocks
    if (
        len(blocks) == 1
        and not blocks[0].block.succ
        and blocks[0].exception_target is None
    ):
        live = set()
        for vop in reversed(blocks[0].vops):
            if visible_candidates and vop.info.name in _SUSPENSION_VOPS:
                live.update(visible_candidates)
            _mark_dead_local_store(vop, live, dead_store_candidates)
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
    handlers = {block: ir2_blocks.get(block.exception_target) for block in blocks}
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
            _mark_dead_local_store(vop, live, dead_store_candidates)
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
                info=(leaf, node.final_binding),
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


class _ScanIntParse(NodeVisitor):
    def __init__(self, converter: _Converter):
        self.converter = converter

    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
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


class _Converter(NodeVisitor):
    def __init__(self, component: IR2Component, *, ltn_token: object | None = None):
        self.component = component
        self.ltn_token = ltn_token
        self.environment = component.environment
        self.context = component.context
        self.lambda_ = component.lambda_
        self.current: IR2Block | None = None
        self._load_leaf: _LoadLeaf | None = _LoadLeaf(self)
        self._store_var: _StoreVar | None = _StoreVar(self)
        self._scan_int_parse: _ScanIntParse | None = (
            _ScanIntParse(self)
            if self.context.plan is None
            and self.lambda_.policy.safety < 3
            and not self.lambda_.name.startswith(_PROTOCOL_PREFIXES)
            else None
        )
        self._skipped_nodes: set = set()
        self._fused_range_prepares: dict[Combination, tuple] = {}
        self.stack_cells = {}

    def close(self) -> None:
        self._load_leaf = None
        self._store_var = None
        self._scan_int_parse = None
        self._skipped_nodes.clear()
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
            if (
                self.ltn_token is not None
                and info.ltn_token is self.ltn_token
                and info.value_type is not None
            ):
                value_type = info.value_type
            else:
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
        self._fused_range_prepares.clear()
        nodes = tuple(block.nodes())
        if self._scan_int_parse is not None:
            for node in nodes:
                node.accept(self._scan_int_parse)
        for node in nodes:
            if node not in self._skipped_nodes:
                node.accept(self)
        self.convert_transfer(block)

    def note_range_prepare(self, node: Combination) -> None:
        if (
            node.kind is not CallKind.KNOWN
            or node.fun_info is not fndb.RANGE_PREPARE
            or node.lexenv.policy.safety >= 3
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
        target_type = self.context.type_of(node)
        checked = (
            node.type_check
            and bool(node.safety)
            and not actual.fits(node.contract.checked_type)
        )
        if (
            not checked
            and self.context.plan is None
            and node not in self.context.element_contracts
        ):
            if (
                node.lvar is not None
                and len(node.lvar.uses) == 1
                and not node.lvar.info.locs
                and operand.leaf is None
            ):
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
            if actual.fits(target_type):
                result, discard = self.result_tn(node)
                if operand is not result:
                    self.emit(vops.MOVE, node, args=(operand,), results=(result,))
                if discard:
                    self.emit_discard(node, result)
                return
        contract = node.contract
        if (
            checked
            and node.declaration is not None
            and node.declaration[0] == "inferred-return"
            and contract.checked_type is PYTHON_INTEGER
            and target_type.is_fixnum
        ):
            contract = InferredReturnContract(
                contract.name, target_type.widen_constant()
            )
        result, discard = self.result_tn(node)
        self.emit(
            vops.TYPE_CHECK,
            node,
            args=(operand,),
            results=(result,),
            info=TypeCheck(
                contract, node.location, node.safety if node.type_check else 0
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
        elif name == "%next_item":
            self.emit_template(node, [self.lvar_tn(arg) for arg in node.args], None)
        else:
            self.convert_template(node, [self.lvar_tn(arg) for arg in node.args])

    def convert_template(self, node, args, info=None):
        """Emit the template LTN selected (``ir2-convert-template``)."""
        if info is None:
            info = self.context.codegen_info.get(node, node.info)
        self.emit_template(node, args, node.info if info is None else info)

    def emit_template(self, node, args, info):
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
                info=info,
            )
            if discard:
                self.emit_discard(node, result)
        else:
            self.emit(template, node, args=args, info=info)

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
        keywords = info.keywords if info is not None else ()
        order = (
            target.call_order(keywords, len(node.args))
            if target is not None and target in self.context.borrowed_callees
            else None
        )
        borrowable = (
            frozenset(
                arg_idx
                for param_idx, arg_idx in enumerate(order)
                if arg_idx is not None
                and arg_idx >= 0
                and param_idx in self.context.borrowed_callees[target]
            )
            if order is not None
            else frozenset()
        )
        site = CallSite(
            keywords,
            info.location if info is not None else None,
            target,
            proven,
            borrowable,
        )
        result, discard = self.result_tn(node)
        if discard and not proven:
            result.type = UNKNOWN
            result.primitive_type = T
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
