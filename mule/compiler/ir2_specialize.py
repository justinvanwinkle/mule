"""IR2 tuple delivery, sequence iteration, and boolean comparison specialization passes."""

from __future__ import annotations

from dataclasses import dataclass

from . import vops
from .callconv import TupleDelivery
from .ctype import (
    BOOLEAN,
    NONE_TYPE,
    UNKNOWN,
    IntegerType,
    ValueType,
)
from .ir1util import block_start_cleanup
from .ir2_borrow import _CODEPOINT_COMPARISON_VOPS, _plain_local
from .vop import (
    BOOLEAN_T,
    FIXNUM,
    SIGNED,
    IR2Block,
    IR2Component,
)


@dataclass(frozen=True, slots=True)
class DirectSequenceIteration:
    seq_type: ValueType
    iterator_local: object
    index_tn: object
    seq_local: object | None = None
    borrow_element: bool = False


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
                    arg_count = len(producer.args) - (1 if site.callee_required else 0)
                    if (
                        arities.get(site.target) == arity
                        and site.target.call_order(site.keywords, arg_count) is not None
                    ):
                        call = producer
                elif producer.info is vops.LOCAL_EXTENT_CALL:
                    target = producer.codegen_info.target
                    if (
                        arities.get(target) == arity
                        and target.signature.return_annotation is None
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
                arg_count = len(call.args) - (1 if site.callee_required else 0)
                direct = (
                    site.target is not None
                    and site.target.call_order(site.keywords, arg_count) is not None
                )
            if direct:
                plan.direct_assertions[call] = assertion


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
        if (
            block.exception_target is not None
            and any(
                step.info.name.startswith(
                    (
                        "byte-cursor-mutable-list-%byte_available-",
                        "byte-cursor-immutable-tuple-%byte_available-",
                        "byte-cursor-immutable-bytes-%byte_available-",
                        "byte-cursor-immutable-text-%byte_available-",
                        "byte-cursor-mutable-bytearray-%byte_available-",
                    )
                )
                for step in block.vops
            )
            and all(
                step.info.name in ("insert-safepoint", "load-local", "branch")
                or step.info.name.startswith(
                    (
                        "byte-cursor-mutable-list-%byte_available-",
                        "byte-cursor-immutable-tuple-%byte_available-",
                        "byte-cursor-immutable-bytes-%byte_available-",
                        "byte-cursor-immutable-text-%byte_available-",
                        "byte-cursor-mutable-bytearray-%byte_available-",
                    )
                )
                for step in block.vops
            )
        ):
            block.exception_target = _enclosing_exception_target(block.block)
            pruned_handlers = True
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
