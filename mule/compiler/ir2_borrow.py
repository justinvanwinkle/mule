"""IR2 codegen records, local lifetime, alias propagation, and borrow specialization passes."""

from __future__ import annotations

from dataclasses import dataclass, replace
from typing import TYPE_CHECKING

from mule import ast

from . import vops

if TYPE_CHECKING:
    from .annotation_types import Contract
from .ctype import (
    BOOLEAN_FALSE,
    REAL,
    TEXT_TYPE,
    UNKNOWN,
    ValueType,
    ValueTypeVisitor,
)
from .node import (
    Lambda,
    LeafVisitor,
    NodeVisitor,
    linked,
)
from .vop import (
    DOUBLE,
    FIXNUM,
    SIGNED,
    VOP,
    IR2Block,
    IR2Component,
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
    known = vop.info.translate
    if known is not None and any(
        index < len(vop.args) and ref is vop.args[index]
        for index in known.borrowed_args
    ):
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
    if name in ("full-call", "local-extent-call") and vop.args:
        site = linked(vop.codegen_info, vop, "call site")
        offset = 1 if site.callee_required else 0
        return (site.callee_required and ref is vop.args[0]) or any(
            ref is vop.args[index + offset] for index in site.borrowable_arguments
        )
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
    final_binding: bool = False
    callee_required: bool = True


def _final_call_needs_object(target, call, caller):
    if target.takes_closure or target.namespace != caller.namespace:
        return True
    keywords = call.info.keywords if call.info is not None else ()
    order = target.call_order(keywords, len(call.args))
    return order is None or any(index < 0 for index in order)


class _FinalBindingRef(NodeVisitor, LeafVisitor):
    def __init__(self, context):
        self.context = context

    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Ref(self, node):
        return node.leaf.accept(self)

    def visit_GlobalVar(self, leaf):
        return leaf.final_function if leaf in self.context.final_bindings else None


class _ObjectFreeFinalUse(NodeVisitor):
    def __init__(self, lvar, target, caller):
        self.lvar, self.target, self.caller = lvar, target, caller

    def visit_Node(self, node):
        return False

    def visit_Combination(self, node):
        return node.fun is self.lvar and not _final_call_needs_object(
            self.target, node, self.caller
        )


@dataclass(frozen=True, slots=True)
class MethodResolution:
    name: str


@dataclass(frozen=True, slots=True)
class MethodCall:
    keywords: tuple
    name: str
    # The method the receiver's class resolves the name to, when analysis
    # typed the receiver; code generation guards the resolved function's
    # identity and calls this method's body directly.
    target: Lambda | None = None
    # The receiver's analyzed type, for selecting the target's entry context.
    receiver_type: ValueType = UNKNOWN


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
            if component.context.plan is not None and result.primitive_type is not T:
                continue
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
                    or not reader.codegen_info.callee_required
                    or ref is not reader.args[0]
                    or not (
                        reader.codegen_info.proven
                        or (
                            (
                                component.lambda_.policy.safety == 0
                                or vop.codegen_info in component.context.final_bindings
                            )
                            and not vop.codegen_info.sets
                        )
                    )
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
        "primitive-_text_codepoint",
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
    if name.startswith(
        (
            "byte-cursor-mutable-list-",
            "byte-cursor-immutable-tuple-",
            "byte-cursor-immutable-bytes-",
            "byte-cursor-immutable-text-",
            "byte-cursor-mutable-bytearray-",
        )
    ):
        return True
    if name == "protocol-getitem" and len(step.args) == 2:
        seq_type = step.args[0].tn.type or UNKNOWN
        return (seq_type.is_exact_list or seq_type.is_exact_tuple) and (
            step.args[1].tn.type or UNKNOWN
        ).is_fixnum
    if (
        name in ("primitive-_storage_read", "primitive-_text_codepoint")
        and len(step.args) == 2
    ):
        return (step.args[1].tn.type or UNKNOWN).is_fixnum
    return (
        name in ("primitive-_storage_length", "primitive-_text_length")
        and len(step.args) == 1
    )


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
    if name.startswith(
        (
            "byte-cursor-mutable-list-",
            "byte-cursor-immutable-tuple-",
            "byte-cursor-immutable-bytes-",
            "byte-cursor-immutable-text-",
            "byte-cursor-mutable-bytearray-",
        )
    ):
        return True
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
                if name.startswith(
                    (
                        "byte-cursor-mutable-list-%byte_next-",
                        "byte-cursor-immutable-tuple-%byte_next-",
                    )
                ):
                    borrow_info = "cursor_borrow"
                else:
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
                name
                in (
                    "protocol-getitem",
                    "protocol-next_item",
                    "byte-cursor-mutable-list-%byte_next-False",
                    "byte-cursor-immutable-tuple-%byte_next-False",
                )
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
                    "primitive-_text_codepoint",
                    "primitive-_text_length",
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
