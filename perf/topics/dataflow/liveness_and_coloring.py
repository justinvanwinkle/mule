"""Bigint bitset backward liveness analysis and Chaitin-Briggs graph coloring.

Models register allocation (pack.py) using arbitrary-precision Python int bitsets
(>256 variables per function, exercising bigint |, &, ~, ^, and bit_length()),
set cross-verification, conservative move coalescing, and simplify/select/spill coloring.
"""

import sys
import time

DEFAULT_SIZE = 2000
DEFAULT_LOOPS = 1

_NUM_VARS_PER_FUNC = 300
_NUM_PHYSICAL_REGS = 8


class LivenessBlock:
    def __init__(
        self,
        block_id: int,
        succs: list[int],
        use_bits: int,
        def_bits: int,
        use_set: set[int],
        def_set: set[int],
    ) -> None:
        self.block_id = block_id
        self.succs = succs
        self.use_bits = use_bits
        self.def_bits = def_bits
        self.use_set = use_set
        self.def_set = def_set


class InterferenceKernel:
    def __init__(
        self,
        num_nodes: int,
        live_cliques: list[tuple[int, ...]],
        move_pairs: list[tuple[int, int]],
        use_weights: list[int],
    ) -> None:
        self.num_nodes = num_nodes
        self.live_cliques = live_cliques
        self.move_pairs = move_pairs
        self.use_weights = use_weights


def _build_liveness_cfg(seed: int) -> list[LivenessBlock]:
    """Build a 10-block CFG with >256 virtual registers represented as bigint bitsets."""
    succ_map: list[list[int]] = [
        [1, 2],  # 0: entry
        [3],  # 1: setup A
        [3],  # 2: setup B
        [4, 5],  # 3: loop header
        [6],  # 4: branch left
        [6, 7],  # 5: branch right
        [3, 8],  # 6: loop latch (backedge to 3)
        [8],  # 7: side exit
        [9],  # 8: epilogue
        [],  # 9: return
    ]

    blocks: list[LivenessBlock] = []
    state = seed
    for bid in range(10):
        use_bits = 0
        def_bits = 0
        use_set: set[int] = set()
        def_set: set[int] = set()

        # Populate variables across the 0..299 range so bitsets exceed 256 bits
        base_offset = (bid * 27 + (seed & 0x1F)) % _NUM_VARS_PER_FUNC
        for k in range(18):
            state = (state * 1664525 + 1013904223) & 0x7FFFFFFF
            u_var = (base_offset + k * 13 + (state & 7)) % _NUM_VARS_PER_FUNC
            d_var = (base_offset + k * 17 + ((state >> 4) & 15)) % _NUM_VARS_PER_FUNC

            # High-numbered variables above bit 260 always participate
            hi_u = 260 + ((state >> 9) % 40)
            hi_d = 260 + ((state >> 15) % 40)

            for uv in (u_var, hi_u):
                if uv not in def_set:
                    use_bits |= 1 << uv
                    use_set.add(uv)
            for dv in (d_var, hi_d):
                def_bits |= 1 << dv
                def_set.add(dv)

        blocks.append(
            LivenessBlock(bid, succ_map[bid], use_bits, def_bits, use_set, def_set)
        )
    return blocks


def _build_interference_kernel(seed: int) -> InterferenceKernel:
    num_nodes = 28
    state = seed
    cliques: list[tuple[int, ...]] = []
    moves: list[tuple[int, int]] = []
    weights: list[int] = []

    for v in range(num_nodes):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        weights.append(10 + (state & 0x3F) + (v & 7) * 5)

    for _ in range(16):
        state = (state * 1664525 + 1013904223) & 0x7FFFFFFF
        c_size = 4 + (state % 5)
        members: list[int] = []
        for j in range(c_size):
            node = ((state >> (j * 4)) + j * 5) % num_nodes
            if node not in members:
                members.append(node)
        if len(members) >= 2:
            cliques.append(tuple(members))

    for _ in range(12):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        u = state % num_nodes
        v = ((state >> 6) + 3) % num_nodes
        if u != v:
            moves.append((u, v))

    return InterferenceKernel(num_nodes, cliques, moves, weights)


def _popcount_bigint(bits: int) -> int:
    count = 0
    cur = bits
    while cur:
        cur &= cur - 1
        count += 1
    return count


def prepare(
    size: int,
) -> tuple[list[list[LivenessBlock]], list[InterferenceKernel]]:
    cfgs: list[list[LivenessBlock]] = []
    kernels: list[InterferenceKernel] = []
    state = 55192841
    for i in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        cfgs.append(_build_liveness_cfg(state ^ i))
        kernels.append(_build_interference_kernel((state >> 3) ^ (i * 31)))
    return cfgs, kernels


def scenario_bigint_bitset_liveness(
    cfgs: list[list[LivenessBlock]],
) -> tuple[int, int, int, int]:
    """Compute backward CFG liveness with >256-bit int bitsets and set verification."""
    sweeps = 0
    max_bit_len_sum = 0
    interference_xor_bits = 0
    set_verified_blocks = 0

    for _ in range(164):
        for idx, blocks in enumerate(cfgs):
            n = len(blocks)
            live_in = [0] * n
            live_out = [0] * n

            changed = True
            while changed:
                changed = False
                sweeps += 1
                # Backward postorder traversal
                for bid in range(n - 1, -1, -1):
                    blk = blocks[bid]
                    out_b = 0
                    for s in blk.succs:
                        out_b |= live_in[s]
                    in_b = blk.use_bits | (out_b & ~blk.def_bits)
                    if out_b != live_out[bid] or in_b != live_in[bid]:
                        live_out[bid] = out_b
                        live_in[bid] = in_b
                        changed = True

            for bid in range(n):
                in_b = live_in[bid]
                out_b = live_out[bid]
                max_bit_len_sum += in_b.bit_length() + out_b.bit_length()
                diff_bits = (in_b ^ out_b) & ((1 << _NUM_VARS_PER_FUNC) - 1)
                interference_xor_bits += diff_bits.bit_length()

            # Cross-verify entry & loop header blocks against Python set liveness
            if (idx & 7) == 0:
                set_in: list[set[int]] = [set() for _ in range(n)]
                set_out: list[set[int]] = [set() for _ in range(n)]
                s_changed = True
                while s_changed:
                    s_changed = False
                    for bid in range(n - 1, -1, -1):
                        blk = blocks[bid]
                        new_out: set[int] = set()
                        for s in blk.succs:
                            new_out |= set_in[s]
                        new_in = blk.use_set | (new_out - blk.def_set)
                        if new_out != set_out[bid] or new_in != set_in[bid]:
                            set_out[bid] = new_out
                            set_in[bid] = new_in
                            s_changed = True

                entry_pop = _popcount_bigint(live_in[0])
                hdr_pop = _popcount_bigint(live_out[3])
                if entry_pop == len(set_in[0]) and hdr_pop == len(set_out[3]):
                    set_verified_blocks += 2

    return sweeps, max_bit_len_sum, interference_xor_bits, set_verified_blocks


def scenario_interference_graph_coloring(
    kernels: list[InterferenceKernel],
) -> tuple[int, int, int, int]:
    """Build interference graphs, coalesce moves (Briggs/George), and run Chaitin-Briggs coloring."""
    coalesced_moves = 0
    colored_nodes = 0
    spilled_nodes = 0
    color_assignment_hash = 0

    k_regs = _NUM_PHYSICAL_REGS

    for _ in range(54):
        for kern in kernels:
            n = kern.num_nodes
            adj: dict[int, set[int]] = {i: set() for i in range(n)}

            for clique in kern.live_cliques:
                c_len = len(clique)
                for i in range(c_len):
                    u = clique[i]
                    u_set = adj[u]
                    for j in range(i + 1, c_len):
                        v = clique[j]
                        u_set.add(v)
                        adj[v].add(u)

            alias: dict[int, int] = {i: i for i in range(n)}
            active_nodes: set[int] = set(range(n))

            def get_rep(x: int, alias_map: dict[int, int] = alias) -> int:
                curr = x
                while alias_map[curr] != curr:
                    curr = alias_map[curr]
                return curr

            # Conservative Briggs move coalescing: merge (u, v) if non-interfering
            # and combined neighbors have < k_regs nodes of degree >= k_regs
            for raw_u, raw_v in kern.move_pairs:
                u = get_rep(raw_u)
                v = get_rep(raw_v)
                if u == v or v in adj[u]:
                    continue
                union_nbrs = adj[u] | adj[v]
                high_deg_count = 0
                for nb in union_nbrs:
                    deg = len(adj[nb])
                    if nb in adj[u] and nb in adj[v]:
                        deg -= 1
                    if deg >= k_regs:
                        high_deg_count += 1
                if high_deg_count < k_regs:
                    # Coalesce v into u
                    for nb in list(adj[v]):
                        adj[nb].discard(v)
                        if nb != u:
                            adj[nb].add(u)
                            adj[u].add(nb)
                    del adj[v]
                    active_nodes.remove(v)
                    alias[v] = u
                    coalesced_moves += 1

            # Copy adjacency for simplify phase
            work_adj: dict[int, set[int]] = {u: set(nbrs) for u, nbrs in adj.items()}
            select_stack: list[tuple[int, bool]] = []
            remaining: set[int] = set(active_nodes)

            while remaining:
                low_deg_node = -1
                for u in sorted(remaining):
                    if len(work_adj[u]) < k_regs:
                        low_deg_node = u
                        break

                if low_deg_node != -1:
                    chosen = low_deg_node
                    optimistic_spill = False
                else:
                    # Pick spill candidate minimizing (use_weight // (degree + 1))
                    best_u = -1
                    best_score = 0x7FFFFFFF
                    for u in sorted(remaining):
                        deg = len(work_adj[u])
                        score = (kern.use_weights[u] * 16) // (deg + 1)
                        if score < best_score:
                            best_score = score
                            best_u = u
                    chosen = best_u
                    optimistic_spill = True

                remaining.remove(chosen)
                for nb in work_adj[chosen]:
                    if nb in remaining:
                        work_adj[nb].discard(chosen)
                select_stack.append((chosen, optimistic_spill))

            # Select phase: pop stack and assign lowest available register or spill slot
            colors: dict[int, int] = {}
            spill_slots: dict[int, int] = {}
            next_spill_slot = 0

            while select_stack:
                u, _was_spill_cand = select_stack.pop()
                used_regs = 0
                for nb in adj[u]:
                    c = colors.get(nb, -1)
                    if c >= 0:
                        used_regs |= 1 << c

                assigned = -1
                for reg_id in range(k_regs):
                    if not (used_regs & (1 << reg_id)):
                        assigned = reg_id
                        break

                if assigned >= 0:
                    colors[u] = assigned
                    colored_nodes += 1
                    color_assignment_hash = (
                        color_assignment_hash + (u + 1) * (assigned + 3)
                    ) & 0x7FFFFFFF
                else:
                    colors[u] = -1
                    spill_slots[u] = next_spill_slot
                    next_spill_slot += 1
                    spilled_nodes += 1
                    color_assignment_hash = (
                        color_assignment_hash + (u + 1) * 97 + next_spill_slot
                    ) & 0x7FFFFFFF

    return coalesced_moves, colored_nodes, spilled_nodes, color_assignment_hash


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
    cfgs, kernels = prepare(size)
    prep_s = time.perf_counter() - t0

    expected = None
    live_s = 0.0
    col_s = 0.0
    for _ in range(loops):
        r1: tuple[int, ...] = ()
        r2: tuple[int, ...] = ()
        if scenario in ("all", "bigint_bitset_liveness"):
            s0 = time.perf_counter()
            r1 = scenario_bigint_bitset_liveness(cfgs)
            live_s += time.perf_counter() - s0
        if scenario in ("all", "interference_graph_coloring"):
            s1 = time.perf_counter()
            r2 = scenario_interference_graph_coloring(kernels)
            col_s += time.perf_counter() - s1
        combined = r1 + r2
        if expected is not None and combined != expected:
            raise AssertionError("Result mismatch across loops")
        expected = combined

    total_s = (live_s + col_s) / loops
    print("topic\tdataflow/liveness_and_coloring")
    print(f"size\t{size}")
    print(f"loops\t{loops}")
    print(f"result\t{expected}")
    print(f"prepare\t{prep_s:.6f}")
    if scenario in ("all", "bigint_bitset_liveness"):
        print(f"bigint_bitset_liveness\t{live_s / loops:.6f}")
    if scenario in ("all", "interference_graph_coloring"):
        print(f"interference_graph_coloring\t{col_s / loops:.6f}")
    print(f"total\t{total_s:.6f}")


if __name__ == "__main__":
    main()
