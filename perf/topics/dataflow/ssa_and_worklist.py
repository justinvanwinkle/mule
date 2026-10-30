"""Iterative CFG dataflow worklist convergence and SSA/VOP def-use block rewriting.

Models compiler constraint propagation (constraint.py) and IR2 instruction
transformation (ir2tran.py) using collections.deque worklists, interval and
frozenset lattice facts, and VOP/TNRef def-use maps.
"""

import sys
import time
from collections import deque

DEFAULT_SIZE = 2000
DEFAULT_LOOPS = 1

_MIN_BOUND = -32768
_MAX_BOUND = 32767


class CFGInstruction:
    def __init__(
        self,
        op: str,
        dst_var: int,
        src_a: int,
        src_b: int,
        imm: int,
        tag_mask: int,
    ) -> None:
        self.op = op
        self.dst_var = dst_var
        self.src_a = src_a
        self.src_b = src_b
        self.imm = imm
        self.tag_mask = tag_mask


class CFGBlock:
    def __init__(self, block_id: int) -> None:
        self.block_id = block_id
        self.preds: list[int] = []
        self.succs: list[int] = []
        self.insts: list[CFGInstruction] = []
        # Edge refinement: (cond_var, branch_kind, bound_val, refined_tag_mask)
        self.edge_constraints: dict[int, tuple[int, str, int, int]] = {}


def _build_cfg_function(seed: int, num_vars: int = 12) -> tuple[list[CFGBlock], int]:
    """Construct an 8-block CFG with a loop header, diamond branch, and exit."""
    blocks = [CFGBlock(i) for i in range(8)]

    def link(u: int, v: int) -> None:
        blocks[u].succs.append(v)
        blocks[v].preds.append(u)

    # 0 -> 1 (loop header)
    # 1 -> 2 (loop body), 1 -> 6 (loop exit)
    # 2 -> 3 (left diamond), 2 -> 4 (right diamond)
    # 3 -> 5, 4 -> 5 (diamond merge)
    # 5 -> 1 (backedge), 5 -> 6 (early exit)
    # 6 -> 7 (return block)
    link(0, 1)
    link(1, 2)
    link(1, 6)
    link(2, 3)
    link(2, 4)
    link(3, 5)
    link(4, 5)
    link(5, 1)
    link(5, 6)
    link(6, 7)

    s = seed
    c0 = (s & 15) + 1
    c1 = ((s >> 4) & 31) + 5
    limit = 60 + ((s >> 9) & 63)

    # Block 0: entry initializations
    blocks[0].insts = [
        CFGInstruction("const", 0, -1, -1, 0, 0x01),
        CFGInstruction("const", 1, -1, -1, c0, 0x01),
        CFGInstruction("const", 2, -1, -1, c1, 0x03),
        CFGInstruction("alloc_nonnull", 3, 1, -1, 4, 0x04),
        CFGInstruction("const", 4, -1, -1, -10, 0x01),
    ]

    # Block 1: loop header
    blocks[1].insts = [
        CFGInstruction("add_imm", 5, 0, -1, 1, 0x01),
        CFGInstruction("bit_and", 6, 1, -1, 0x7F, 0x01),
    ]
    blocks[1].edge_constraints[2] = (0, "lt", limit, 0x01)
    blocks[1].edge_constraints[6] = (0, "ge", limit, 0x01)

    # Block 2: diamond dispatch
    blocks[2].insts = [
        CFGInstruction("add", 7, 5, 6, 0, 0x01),
        CFGInstruction("tag_narrow", 8, 2, -1, 0, 0x01),
    ]
    blocks[2].edge_constraints[3] = (7, "le", limit // 2, 0x01)
    blocks[2].edge_constraints[4] = (7, "gt", limit // 2, 0x02)

    # Block 3: then branch
    blocks[3].insts = [
        CFGInstruction("mul_imm", 9, 7, -1, 2, 0x01),
        CFGInstruction("alloc_nonnull", 10, 9, -1, 8, 0x04),
        CFGInstruction("add_imm", 1, 9, -1, 3, 0x01),
    ]

    # Block 4: else branch
    blocks[4].insts = [
        CFGInstruction("sub_imm", 9, 7, -1, 4, 0x01),
        CFGInstruction("alloc_nonnull", 10, 3, -1, 2, 0x04),
        CFGInstruction("bit_or", 1, 9, -1, 0x0F, 0x03),
    ]

    # Block 5: merge & backedge update
    blocks[5].insts = [
        CFGInstruction("add", 11, 9, 6, 0, 0x03),
        CFGInstruction("clamp", 0, 5, -1, limit + 4, 0x01),
        CFGInstruction("add", 4, 11, 8, 0, 0x03),
    ]
    blocks[5].edge_constraints[1] = (0, "lt", limit, 0x01)
    blocks[5].edge_constraints[6] = (0, "ge", limit // 2, 0x01)

    # Block 6: post-loop summary
    blocks[6].insts = [
        CFGInstruction("add", 11, 0, 1, 0, 0x03),
        CFGInstruction("bit_and", 8, 4, -1, 0xFF, 0x01),
    ]

    # Block 7: exit block
    blocks[7].insts = [
        CFGInstruction("add", 0, 11, 8, 0, 0x01),
    ]

    return blocks, num_vars


def _join_intervals(a: tuple[int, int], b: tuple[int, int]) -> tuple[int, int]:
    lo = min(b[0], a[0])
    hi = max(b[1], a[1])
    lo = max(lo, _MIN_BOUND)
    hi = min(hi, _MAX_BOUND)
    return (lo, hi)


def _widen_interval(
    old_iv: tuple[int, int], new_iv: tuple[int, int], step: int
) -> tuple[int, int]:
    lo = old_iv[0]
    hi = old_iv[1]
    if new_iv[0] < lo:
        lo = -128 if (new_iv[0] >= -128 and step < 4) else _MIN_BOUND
    if new_iv[1] > hi:
        hi = 256 if (new_iv[1] <= 256 and step < 4) else _MAX_BOUND
    return (lo, hi)


def _transfer_block(
    blk: CFGBlock,
    in_iv: dict[int, tuple[int, int]],
    in_nonnull: frozenset[int],
    in_tags: dict[int, int],
) -> tuple[dict[int, tuple[int, int]], frozenset[int], dict[int, int]]:
    iv = dict(in_iv)
    nonnull_mut = set(in_nonnull)
    tags = dict(in_tags)

    for inst in blk.insts:
        op = inst.op
        dst = inst.dst_var
        if op == "const":
            iv[dst] = (inst.imm, inst.imm)
            tags[dst] = inst.tag_mask
            if inst.imm != 0:
                nonnull_mut.add(dst)
        elif op == "alloc_nonnull":
            iv[dst] = (1, inst.imm)
            tags[dst] = inst.tag_mask
            nonnull_mut.add(dst)
        elif op == "add_imm":
            sa = iv.get(inst.src_a, (_MIN_BOUND, _MAX_BOUND))
            lo = sa[0] + inst.imm
            hi = sa[1] + inst.imm
            iv[dst] = (
                max(_MIN_BOUND, lo),
                min(_MAX_BOUND, hi),
            )
            tags[dst] = tags.get(inst.src_a, 0x07) | inst.tag_mask
            if inst.src_a in nonnull_mut and inst.imm > 0:
                nonnull_mut.add(dst)
        elif op == "sub_imm":
            sa = iv.get(inst.src_a, (_MIN_BOUND, _MAX_BOUND))
            lo = sa[0] - inst.imm
            hi = sa[1] - inst.imm
            iv[dst] = (
                max(_MIN_BOUND, lo),
                min(_MAX_BOUND, hi),
            )
            tags[dst] = tags.get(inst.src_a, 0x07) | inst.tag_mask
        elif op == "mul_imm":
            sa = iv.get(inst.src_a, (_MIN_BOUND, _MAX_BOUND))
            p1 = sa[0] * inst.imm
            p2 = sa[1] * inst.imm
            lo = min(p2, p1)
            hi = max(p1, p2)
            iv[dst] = (
                max(_MIN_BOUND, lo),
                min(_MAX_BOUND, hi),
            )
            tags[dst] = tags.get(inst.src_a, 0x07)
        elif op == "add":
            sa = iv.get(inst.src_a, (_MIN_BOUND, _MAX_BOUND))
            sb = iv.get(inst.src_b, (_MIN_BOUND, _MAX_BOUND))
            lo = sa[0] + sb[0]
            hi = sa[1] + sb[1]
            iv[dst] = (
                max(_MIN_BOUND, lo),
                min(_MAX_BOUND, hi),
            )
            tags[dst] = (
                tags.get(inst.src_a, 0x07) | tags.get(inst.src_b, 0x07) | inst.tag_mask
            )
            if inst.src_a in nonnull_mut and inst.src_b in nonnull_mut:
                nonnull_mut.add(dst)
        elif op == "bit_and":
            iv[dst] = (0, inst.imm)
            tags[dst] = inst.tag_mask
        elif op == "bit_or":
            sa = iv.get(inst.src_a, (0, 255))
            hi = (max(0, sa[1])) | inst.imm
            iv[dst] = (0, min(_MAX_BOUND, hi))
            tags[dst] = tags.get(inst.src_a, 0x07) | inst.tag_mask
            nonnull_mut.add(dst)
        elif op == "tag_narrow":
            sa = iv.get(inst.src_a, (_MIN_BOUND, _MAX_BOUND))
            iv[dst] = sa
            tags[dst] = tags.get(inst.src_a, 0x07) & inst.tag_mask
            if inst.src_a in nonnull_mut:
                nonnull_mut.add(dst)
        elif op == "clamp":
            sa = iv.get(inst.src_a, (0, inst.imm))
            lo = max(0, sa[0])
            hi = min(inst.imm, sa[1])
            lo = min(lo, hi)
            iv[dst] = (lo, hi)
            tags[dst] = inst.tag_mask

    return iv, frozenset(nonnull_mut), tags


def _refine_for_edge(
    blk: CFGBlock,
    succ_id: int,
    out_iv: dict[int, tuple[int, int]],
    out_nonnull: frozenset[int],
    out_tags: dict[int, int],
) -> tuple[dict[int, tuple[int, int]], frozenset[int], dict[int, int]]:
    constraint = blk.edge_constraints.get(succ_id)
    if constraint is None:
        return out_iv, out_nonnull, out_tags

    cvar, kind, bound_val, mask = constraint
    cur_iv = out_iv.get(cvar)
    if cur_iv is None:
        return out_iv, out_nonnull, out_tags

    lo, hi = cur_iv
    if kind == "lt":
        if hi >= bound_val:
            hi = bound_val - 1
    elif kind == "le":
        hi = min(hi, bound_val)
    elif kind == "gt":
        if lo <= bound_val:
            lo = bound_val + 1
    elif kind == "ge":
        lo = max(lo, bound_val)
    lo = min(lo, hi)

    refined_iv = dict(out_iv)
    refined_iv[cvar] = (lo, hi)
    refined_tags = dict(out_tags)
    refined_tags[cvar] = (out_tags.get(cvar, 0x07) & mask) or mask
    refined_nn = out_nonnull | frozenset([cvar]) if lo > 0 else out_nonnull
    return refined_iv, refined_nn, refined_tags


# ==============================================================================
# Scenario 2: VOP / TNRef Def-Use Block Rewriting (ir2tran.py style)
# ==============================================================================


class TN:
    """Temporary Name representing a virtual register or constant operand."""

    def __init__(self, tn_id: int, kind: str, const_val: int = 0) -> None:
        self.tn_id = tn_id
        self.kind = kind  # "temp" or "const"
        self.const_val = const_val


class TNRef:
    """Doubly-referenced operand slot linking a VOP instruction to a TN."""

    def __init__(self, tn: TN, role: str, slot_idx: int) -> None:
        self.tn = tn
        self.role = role  # "read" or "write"
        self.slot_idx = slot_idx


class VOPInst:
    """Low-level IR2 instruction holding argument and result TNRefs."""

    def __init__(
        self, inst_id: int, opcode: str, args: list[TNRef], results: list[TNRef]
    ) -> None:
        self.inst_id = inst_id
        self.opcode = opcode
        self.args = args
        self.results = results
        self.live = True


class IR2Block:
    """Maintains instruction list alongside exact reads/writes def-use tables."""

    def __init__(self, block_id: int) -> None:
        self.block_id = block_id
        self.insts: list[VOPInst] = []
        self.reads: dict[int, list[tuple[VOPInst, TNRef]]] = {}
        self.writes: dict[int, list[tuple[VOPInst, TNRef]]] = {}
        self.tns: dict[int, TN] = {}

    def get_or_make_temp(self, tn_id: int) -> TN:
        tn = self.tns.get(tn_id)
        if tn is None:
            tn = TN(tn_id, "temp", 0)
            self.tns[tn_id] = tn
        return tn

    def make_const(self, tn_id: int, val: int) -> TN:
        tn = TN(tn_id, "const", val)
        self.tns[tn_id] = tn
        return tn

    def append_vop(
        self,
        inst_id: int,
        opcode: str,
        arg_tns: list[TN],
        res_tns: list[TN],
    ) -> VOPInst:
        arg_refs = [TNRef(t, "read", idx) for idx, t in enumerate(arg_tns)]
        res_refs = [TNRef(t, "write", idx) for idx, t in enumerate(res_tns)]
        inst = VOPInst(inst_id, opcode, arg_refs, res_refs)
        self.insts.append(inst)
        for r in arg_refs:
            bucket = self.reads.get(r.tn.tn_id)
            if bucket is None:
                bucket = []
                self.reads[r.tn.tn_id] = bucket
            bucket.append((inst, r))
        for w in res_refs:
            bucket = self.writes.get(w.tn.tn_id)
            if bucket is None:
                bucket = []
                self.writes[w.tn.tn_id] = bucket
            bucket.append((inst, w))
        return inst

    def retarget_read_ref(self, inst: VOPInst, ref: TNRef, new_tn: TN) -> None:
        old_id = ref.tn.tn_id
        old_bucket = self.reads.get(old_id)
        if old_bucket is not None:
            self.reads[old_id] = [
                pair for pair in old_bucket if not (pair[0] is inst and pair[1] is ref)
            ]
        ref.tn = new_tn
        new_bucket = self.reads.get(new_tn.tn_id)
        if new_bucket is None:
            new_bucket = []
            self.reads[new_tn.tn_id] = new_bucket
        new_bucket.append((inst, ref))

    def unlink_instruction(self, inst: VOPInst) -> None:
        if not inst.live:
            return
        inst.live = False
        for r in inst.args:
            b = self.reads.get(r.tn.tn_id)
            if b is not None:
                self.reads[r.tn.tn_id] = [pair for pair in b if pair[0] is not inst]
        for w in inst.results:
            b = self.writes.get(w.tn.tn_id)
            if b is not None:
                self.writes[w.tn.tn_id] = [pair for pair in b if pair[0] is not inst]


def _build_ir2_block(block_id: int, seed: int) -> IR2Block:
    blk = IR2Block(block_id)
    c_zero = blk.make_const(1000, 0)
    c_one = blk.make_const(1001, 1)
    c_two = blk.make_const(1002, 2)
    c_four = blk.make_const(1004, 4)
    c_eight = blk.make_const(1008, 8)
    c_imm = blk.make_const(1010, (seed & 0x3F) + 3)

    t0 = blk.get_or_make_temp(1)
    t1 = blk.get_or_make_temp(2)
    t2 = blk.get_or_make_temp(3)
    t3 = blk.get_or_make_temp(4)
    t4 = blk.get_or_make_temp(5)
    t5 = blk.get_or_make_temp(6)
    t6 = blk.get_or_make_temp(7)
    t7 = blk.get_or_make_temp(8)
    t8 = blk.get_or_make_temp(9)
    t9 = blk.get_or_make_temp(10)
    t_dead1 = blk.get_or_make_temp(11)
    t_dead2 = blk.get_or_make_temp(12)

    # Sequence with copies, power-of-two multiplies, identity adds, and dead defs
    blk.append_vop(1, "load_arg", [c_imm], [t0])
    blk.append_vop(2, "move", [t0], [t1])
    blk.append_vop(3, "mul", [t1, c_four], [t2])
    blk.append_vop(4, "add", [t2, c_zero], [t3])
    blk.append_vop(5, "move", [t3], [t4])
    blk.append_vop(6, "mul", [t4, c_eight], [t5])
    blk.append_vop(7, "mul", [t5, c_one], [t6])
    blk.append_vop(8, "add", [t1, c_imm], [t_dead1])
    blk.append_vop(9, "mul", [t_dead1, c_two], [t_dead2])
    blk.append_vop(10, "sub", [t6, t2], [t7])
    blk.append_vop(11, "move", [t7], [t8])
    blk.append_vop(12, "add", [t8, c_zero], [t9])
    blk.append_vop(13, "return", [t9], [])
    return blk


def prepare(
    size: int,
) -> tuple[list[tuple[list[CFGBlock], int]], list[int]]:
    cfg_list: list[tuple[list[CFGBlock], int]] = []
    ir2_seeds: list[int] = []
    state = 77182931
    for i in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        cfg_list.append(_build_cfg_function(state ^ i))
        ir2_seeds.append(state)
    return cfg_list, ir2_seeds


def scenario_worklist_type_propagation(
    cfg_list: list[tuple[list[CFGBlock], int]],
) -> tuple[int, int, int, int]:
    """Propagate interval, non-null frozenset, and tag masks over CFGs to fixed point."""
    total_iterations = 0
    narrowed_vars = 0
    nonnull_facts = 0
    bound_span_sum = 0

    for _ in range(35):
        for blocks, _num_vars in cfg_list:
            n_blks = len(blocks)
            in_iv: list[dict[int, tuple[int, int]]] = [{} for _ in range(n_blks)]
            out_iv: list[dict[int, tuple[int, int]]] = [{} for _ in range(n_blks)]
            in_nn: list[frozenset[int]] = [frozenset() for _ in range(n_blks)]
            out_nn: list[frozenset[int]] = [frozenset() for _ in range(n_blks)]
            in_tags: list[dict[int, int]] = [{} for _ in range(n_blks)]
            out_tags: list[dict[int, int]] = [{} for _ in range(n_blks)]
            visit_counts = [0] * n_blks

            worklist: deque[int] = deque([0])
            in_queue: set[int] = {0}

            while worklist:
                bid = worklist.popleft()
                in_queue.remove(bid)
                blk = blocks[bid]
                visit_counts[bid] += 1
                total_iterations += 1

                if blk.preds:
                    merged_iv: dict[int, tuple[int, int]] = {}
                    merged_tags: dict[int, int] = {}
                    nn_accum: frozenset[int] | None = None

                    for pid in blk.preds:
                        pred_blk = blocks[pid]
                        if not out_iv[pid] and pid != 0:
                            continue
                        e_iv, e_nn, e_tags = _refine_for_edge(
                            pred_blk, bid, out_iv[pid], out_nn[pid], out_tags[pid]
                        )
                        for var_id, iv_pair in e_iv.items():
                            existing = merged_iv.get(var_id)
                            if existing is None:
                                merged_iv[var_id] = iv_pair
                            else:
                                joined = _join_intervals(existing, iv_pair)
                                if pid >= bid and visit_counts[bid] > 2:
                                    joined = _widen_interval(
                                        existing, joined, visit_counts[bid]
                                    )
                                merged_iv[var_id] = joined
                        for var_id, tmask in e_tags.items():
                            merged_tags[var_id] = merged_tags.get(var_id, 0) | tmask
                        if nn_accum is None:
                            nn_accum = e_nn
                        else:
                            nn_accum = nn_accum & e_nn

                    in_iv[bid] = merged_iv
                    in_nn[bid] = nn_accum if nn_accum is not None else frozenset()
                    in_tags[bid] = merged_tags

                new_out_iv, new_out_nn, new_out_tags = _transfer_block(
                    blk, in_iv[bid], in_nn[bid], in_tags[bid]
                )

                if (
                    new_out_iv != out_iv[bid]
                    or new_out_nn != out_nn[bid]
                    or new_out_tags != out_tags[bid]
                ):
                    out_iv[bid] = new_out_iv
                    out_nn[bid] = new_out_nn
                    out_tags[bid] = new_out_tags
                    for sid in blk.succs:
                        if sid not in in_queue:
                            in_queue.add(sid)
                            worklist.append(sid)

            exit_iv = out_iv[n_blks - 1]
            exit_nn = out_nn[n_blks - 1]
            exit_tags = out_tags[n_blks - 1]
            nonnull_facts += len(exit_nn)
            for var_id, (lo, hi) in exit_iv.items():
                bound_span_sum = (bound_span_sum + (hi - lo)) & 0x7FFFFFFF
                if exit_tags.get(var_id, 0x07) == 0x01:
                    narrowed_vars += 1

    return total_iterations, narrowed_vars, nonnull_facts, bound_span_sum


def scenario_def_use_block_rewriting(ir2_seeds: list[int]) -> tuple[int, int, int, int]:
    """Rewrite IR2 blocks with copy propagation, strength reduction, and dead code elimination."""
    copies_propagated = 0
    strength_reduced = 0
    dead_eliminated = 0
    live_inst_checksum = 0

    shift_table = {2: 1, 4: 2, 8: 3, 16: 4}

    for round_idx in range(246):
        for idx, seed in enumerate(ir2_seeds):
            blk = _build_ir2_block(idx, seed ^ round_idx)

            # Pass 1: Strength reduction (mul by power of 2 -> shl, add 0 / mul 1 -> move)
            for inst in blk.insts:
                if not inst.live:
                    continue
                if inst.opcode == "mul" and len(inst.args) == 2:
                    rhs = inst.args[1].tn
                    if rhs.kind == "const":
                        if rhs.const_val == 1:
                            inst.opcode = "move"
                            old_ref = inst.args.pop()
                            b = blk.reads.get(old_ref.tn.tn_id)
                            if b is not None:
                                blk.reads[old_ref.tn.tn_id] = [
                                    p for p in b if p[0] is not inst
                                ]
                            strength_reduced += 1
                        elif rhs.const_val in shift_table:
                            sh_amt = shift_table[rhs.const_val]
                            sh_tn = blk.make_const(2000 + inst.inst_id, sh_amt)
                            inst.opcode = "shl"
                            blk.retarget_read_ref(inst, inst.args[1], sh_tn)
                            strength_reduced += 1
                elif inst.opcode == "add" and len(inst.args) == 2:
                    rhs = inst.args[1].tn
                    if rhs.kind == "const" and rhs.const_val == 0:
                        inst.opcode = "move"
                        old_ref = inst.args.pop()
                        b = blk.reads.get(old_ref.tn.tn_id)
                        if b is not None:
                            blk.reads[old_ref.tn.tn_id] = [
                                p for p in b if p[0] is not inst
                            ]
                        strength_reduced += 1

            # Pass 2: Copy propagation via def-use chains
            for inst in blk.insts:
                if not inst.live or inst.opcode != "move":
                    continue
                src_tn = inst.args[0].tn
                dst_tn = inst.results[0].tn
                dst_writes = blk.writes.get(dst_tn.tn_id, [])
                if len(dst_writes) == 1:
                    readers = list(blk.reads.get(dst_tn.tn_id, []))
                    for user_inst, user_ref in readers:
                        blk.retarget_read_ref(user_inst, user_ref, src_tn)
                        copies_propagated += 1

            # Pass 3: Backward worklist dead instruction elimination
            dce_queue: deque[VOPInst] = deque()
            for inst in blk.insts:
                if inst.live and inst.results:
                    res_tn = inst.results[0].tn
                    if not blk.reads.get(res_tn.tn_id):
                        dce_queue.append(inst)

            while dce_queue:
                dead_inst = dce_queue.popleft()
                if not dead_inst.live:
                    continue
                operand_tns = [r.tn for r in dead_inst.args]
                blk.unlink_instruction(dead_inst)
                dead_eliminated += 1
                for op_tn in operand_tns:
                    if op_tn.kind == "temp" and not blk.reads.get(op_tn.tn_id):
                        for writer_inst, _wref in blk.writes.get(op_tn.tn_id, []):
                            if writer_inst.live and writer_inst.results:
                                dce_queue.append(writer_inst)

            # Compact block & accumulate checksum
            blk.insts = [ins for ins in blk.insts if ins.live]
            for ins in blk.insts:
                live_inst_checksum = (
                    live_inst_checksum
                    + ins.inst_id * 17
                    + len(ins.opcode) * 7
                    + len(ins.args) * 3
                ) & 0x7FFFFFFF

    return copies_propagated, strength_reduced, dead_eliminated, live_inst_checksum


def main() -> None:
    size = DEFAULT_SIZE
    loops = DEFAULT_LOOPS
    scenario = "all"
    args = sys.argv[1:]
    i = 0
    while i < len(args):
        if args[i] == "--size" and i + 1 < len(args):
            size = int(args[i + 1])
            i += 2
        elif args[i] == "--loops" and i + 1 < len(args):
            loops = int(args[i + 1])
            i += 2
        elif args[i] == "--scenario" and i + 1 < len(args):
            scenario = args[i + 1]
            i += 2
        else:
            raise ValueError(f"Unknown argument: {args[i]}")

    t0 = time.perf_counter()
    cfg_list, ir2_seeds = prepare(size)
    prep_s = time.perf_counter() - t0

    expected = None
    wl_s = 0.0
    du_s = 0.0
    for _ in range(loops):
        r1: tuple[int, ...] = ()
        r2: tuple[int, ...] = ()
        if scenario in ("all", "worklist_type_propagation"):
            s0 = time.perf_counter()
            r1 = scenario_worklist_type_propagation(cfg_list)
            wl_s += time.perf_counter() - s0
        if scenario in ("all", "def_use_block_rewriting"):
            s1 = time.perf_counter()
            r2 = scenario_def_use_block_rewriting(ir2_seeds)
            du_s += time.perf_counter() - s1
        combined = r1 + r2
        if expected is not None and combined != expected:
            raise AssertionError("Result mismatch across loops")
        expected = combined

    total_s = (wl_s + du_s) / loops
    print("topic\tdataflow/ssa_and_worklist")
    print(f"size\t{size}")
    print(f"loops\t{loops}")
    print(f"result\t{expected}")
    print(f"prepare\t{prep_s:.6f}")
    if scenario in ("all", "worklist_type_propagation"):
        print(f"worklist_type_propagation\t{wl_s / loops:.6f}")
    if scenario in ("all", "def_use_block_rewriting"):
        print(f"def_use_block_rewriting\t{du_s / loops:.6f}")
    print(f"total\t{total_s:.6f}")


if __name__ == "__main__":
    main()
