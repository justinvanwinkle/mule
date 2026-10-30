"""Structural pattern matching (`match`/`case`) and nested unpacking workloads.

Covers heterogeneous AST/protocol event matching with sequence, mapping,
class, OR (`|`), and guarded patterns alongside deep loop unpacking, starred
assignments, and multi-clause list/dict/set comprehensions.
"""

import sys
import time

DEFAULT_SIZE = 240
DEFAULT_LOOPS = 1


class ExprNode:
    __match_args__ = ("op", "lhs", "rhs", "line")

    def __init__(self, op: str, lhs: object, rhs: object, line: int) -> None:
        self.op = op
        self.lhs = lhs
        self.rhs = rhs
        self.line = line


class CallNode:
    __match_args__ = ("func", "args", "flags")

    def __init__(self, func: str, args: list[int], flags: int) -> None:
        self.func = func
        self.args = args
        self.flags = flags


class ConstLeaf:
    __match_args__ = ("value", "ty")

    def __init__(self, value: int, ty: str) -> None:
        self.value = value
        self.ty = ty


_OPS = ("ADD", "SUB", "MUL", "BITAND", "CMP_EQ", "SHIFT_L")
_FUNCS = ("fast_hash", "bounds_check", "coerce_int", "alloc_frame", "trace_hook")
_KINDS = ("metric", "audit", "route", "drop")
_TAGS = ("alpha", "beta", "gamma", "delta", "omega")


def prepare(
    size: int,
) -> tuple[
    list[object],
    list[tuple[str, tuple[int, ...], dict[str, int]]],
]:
    events: list[object] = []
    records: list[tuple[str, tuple[int, ...], dict[str, int]]] = []
    state = 271828183

    for i in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        variant = state % 6
        v1 = (state >> 4) % 200 - 40
        v2 = (state >> 11) % 200 - 20
        v3 = (state >> 18) % 90 + 1

        if variant == 0:
            op = _OPS[(state >> 6) % len(_OPS)]
            events.append(
                ExprNode(
                    op,
                    ConstLeaf(v1, "int" if (v1 & 1) == 0 else "index"),
                    ConstLeaf(v2, "int"),
                    i + 1,
                )
            )
        elif variant == 1:
            op = _OPS[(state >> 7) % len(_OPS)]
            inner = ExprNode("ADD" if (v1 & 1) else "SUB", ConstLeaf(v1, "int"), ConstLeaf(v3, "int"), i)
            events.append(ExprNode(op, inner, ConstLeaf(v2, "int"), i + 2))
        elif variant == 2:
            fn = _FUNCS[(state >> 5) % len(_FUNCS)]
            events.append(CallNode(fn, [v1, v2, v3, (v1 ^ v2) & 0x3F], state & 0x7))
        elif variant == 3:
            seq_cmd = " fold " if (v1 & 1) else "scan"
            events.append([seq_cmd.strip(), v1, v2, v3, (v1 + v2) & 0xFF, i & 0x1F])
        elif variant == 4:
            kind = _KINDS[(state >> 8) % len(_KINDS)]
            events.append(
                {
                    "type": kind,
                    "meta": {"cost": abs(v1) + 3, "retries": v3 & 3, "flag": v2 & 1},
                    "payload": [v1, v2, v3],
                }
            )
        else:
            events.append(("frames", (v1, v2), [v3, v3 + 4, v3 + 9]))

        tag = _TAGS[(state >> 9) % len(_TAGS)]
        tuple_len = 4 + (state % 4)
        nums = tuple(((state >> (k * 3)) % 97) - 15 for k in range(tuple_len))
        meta = {
            "weight": 1 + ((state >> 15) % 9),
            "tier": (state >> 19) % 4,
            "bias": ((state >> 21) % 21) - 10,
        }
        records.append((tag, nums, meta))

    return events, records


def _eval_event(ev: object) -> tuple[int, int, int]:
    match ev:
        case ExprNode(
            "ADD" | "SUB" as op,
            ExprNode(inner_op, ConstLeaf(a, "int"), ConstLeaf(b, "int"), _),
            ConstLeaf(c, "int"),
            line,
        ) if a + b > 0:
            base = (a + b) if inner_op == "ADD" else (a - b)
            res = (base + c) if op == "ADD" else (base - c)
            return res + line, 2, 1

        case ExprNode(
            "ADD" | "MUL" | "BITAND" as op,
            ConstLeaf(x, "int" | "index"),
            ConstLeaf(y, "int"),
            line,
        ):
            if op == "ADD":
                val = x + y
            elif op == "MUL":
                val = x * y
            else:
                val = x & y
            return val + (line & 0xF), 1, 0

        case ExprNode(_, ConstLeaf(x, _), ConstLeaf(y, _), line):
            return (x ^ y) + line, 1, 0

        case CallNode("fast_hash" | "bounds_check" as fn, [first, second, *rest], flags) if flags & 1:
            acc = first * 3 + second + sum(rest)
            return acc if fn == "fast_hash" else (acc ^ flags), 3, len(rest)

        case CallNode(_, [first, *middle, last], flags):
            return first + last + sum(middle) + flags, 2, len(middle)

        case ["fold" | "scan" as mode, lhs, rhs, *tail] if len(tail) >= 2:
            head_part = (lhs + rhs) if mode == "fold" else (lhs - rhs)
            return head_part + tail[0] * 2 - tail[-1], 4, len(tail)

        case {
            "type": "metric" | "audit" as kind,
            "meta": {"cost": int(cost), "retries": int(retries)},
            "payload": [p0, *prest],
        } if cost > 10:
            mult = 2 if kind == "audit" else 1
            return (cost * mult + retries * 5 + p0 + sum(prest)), 5, len(prest)

        case {"type": str(kind), "meta": {"cost": int(cost)}}:
            return cost + len(kind), 1, 0

        case ("frames", (int(x), int(y)), [first_slot, *other_slots]):
            return x + y + first_slot + sum(other_slots), 3, len(other_slots)

        case _:
            return 1, 0, 0


def scenario_structural_pattern_matching(events: list[object]) -> tuple[int, int, int, int]:
    """Dispatch heterogeneous AST and protocol records through deep `match`/`case` blocks."""
    score_sum = 0
    depth_sum = 0
    tail_items = 0
    guarded_hits = 0

    for _ in range(71000):
        for ev in events:
            val, depth, extra = _eval_event(ev)
            score_sum = (score_sum + val) & 0x3FFFFFFF
            depth_sum += depth
            tail_items += extra
            if depth >= 3:
                guarded_hits += 1

    return score_sum, depth_sum, tail_items, guarded_hits


def scenario_nested_unpacking_and_comprehensions(
    records: list[tuple[str, tuple[int, ...], dict[str, int]]],
) -> tuple[int, int, int, int]:
    """Exercise nested loop unpacking, starred targets, and multi-clause comprehensions."""
    unpacked_acc = 0
    comp_list_sum = 0
    comp_dict_sum = 0
    comp_set_score = 0

    for round_idx in range(28500):
        for idx, (tag, (first, second, *middle, last), meta) in enumerate(records):
            w, tier, bias = meta["weight"], meta["tier"], meta["bias"]
            head_pair, *rest_mid = [first + bias, second - tier, *middle]
            mid_total = sum(rest_mid) if rest_mid else 0
            if tag in ("alpha", "gamma"):
                unpacked_acc += (head_pair * w + last + mid_total + (idx & 7))
            else:
                unpacked_acc += (head_pair - last * tier + mid_total + w)

        # Multi-clause list comprehension with nested unpacking & filtering
        matrix_scores = [
            (idx + slot_i) * val * meta["weight"] + a - b
            for idx, (tag, (a, b, *rest), meta) in enumerate(records)
            if meta["tier"] != 2 and (a + b + round_idx) > -10
            for slot_i, val in enumerate(rest)
            if (val + slot_i) % 2 == 0
        ]
        comp_list_sum += sum(matrix_scores)

        # Dict comprehension with tuple key/value unpacking
        bucket_map = {
            (tag, idx & 15): (a * meta["weight"] + b * meta["tier"] + len(extra))
            for idx, (tag, (a, b, *extra), meta) in enumerate(records)
            if meta["weight"] >= 3 and (idx + round_idx) % 3 != 0
        }
        comp_dict_sum += sum(k_idx + v for (_t, k_idx), v in bucket_map.items())

        # Set comprehension with cross-clause filtering
        signature_set = {
            ((a ^ x) + meta["bias"] * (tier_step + 1)) & 0xFFF
            for _tag, (a, _b, *extra), meta in records
            if extra
            for x in extra
            for tier_step in range(meta["tier"] + 1)
            if (x + tier_step) > 0
        }
        comp_set_score += sum(signature_set)

    return unpacked_acc, comp_list_sum, comp_dict_sum, comp_set_score


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
    events, records = prepare(size)
    prep_s = time.perf_counter() - t0

    expected = None
    match_s = 0.0
    unpack_s = 0.0
    for _ in range(loops):
        r1: tuple[int, ...] = ()
        r2: tuple[int, ...] = ()
        if scenario in ("all", "structural_pattern_matching"):
            s0 = time.perf_counter()
            r1 = scenario_structural_pattern_matching(events)
            match_s += time.perf_counter() - s0
        if scenario in ("all", "nested_unpacking_and_comprehensions"):
            s1 = time.perf_counter()
            r2 = scenario_nested_unpacking_and_comprehensions(records)
            unpack_s += time.perf_counter() - s1
        combined = r1 + r2
        if expected is not None and combined != expected:
            raise AssertionError("Result mismatch across loops")
        expected = combined

    total_s = (match_s + unpack_s) / loops
    print("topic\titeration/unpacking_and_matching")
    print(f"size\t{size}")
    print(f"loops\t{loops}")
    print(f"result\t{expected}")
    print(f"prepare\t{prep_s:.6f}")
    if scenario in ("all", "structural_pattern_matching"):
        print(f"structural_pattern_matching\t{match_s / loops:.6f}")
    if scenario in ("all", "nested_unpacking_and_comprehensions"):
        print(f"nested_unpacking_and_comprehensions\t{unpack_s / loops:.6f}")
    print(f"total\t{total_s:.6f}")


if __name__ == "__main__":
    main()
