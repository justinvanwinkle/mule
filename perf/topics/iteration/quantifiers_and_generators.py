"""Quantifier (`any`/`all`/`sum`) and generator/coroutine pipeline workloads.

Models compiler IR instruction scanning (`ir2tran.py` and `constraint.py`
operand/block quantifier checks) alongside multi-stage generator and coroutine
streaming pipelines with `yield`, `yield from`, `.send()`, and `itertools`.
"""

import itertools
import sys
import time

DEFAULT_SIZE = 200
DEFAULT_LOOPS = 1


class OperandType:
    def __init__(
        self,
        name: str,
        may_escape: bool,
        is_unboxed: bool,
        rank: int,
        nullable: bool,
    ) -> None:
        self.name = name
        self.may_escape = may_escape
        self.is_unboxed = is_unboxed
        self.rank = rank
        self.nullable = nullable


DEFAULT_TYPE = OperandType("object", True, False, 0, True)

_TYPE_PALETTE = (
    OperandType("fixnum", False, True, 3, False),
    OperandType("float64", False, True, 3, False),
    OperandType("bool", False, True, 4, False),
    OperandType("str_exact", False, False, 2, False),
    OperandType("list_known", True, False, 1, False),
    OperandType("dict_known", True, False, 1, False),
    OperandType("opt_fixnum", False, False, 2, True),
    OperandType("escaped_box", True, False, 0, True),
)


class Operand:
    def __init__(
        self,
        reg_id: int,
        op_type: OperandType | None,
        is_const: bool,
        uses: int,
    ) -> None:
        self.reg_id = reg_id
        self.type = op_type
        self.is_const = is_const
        self.uses = uses


class InstructionStep:
    def __init__(
        self,
        opcode: str,
        args: list[Operand],
        has_side_effect: bool,
        cost: int,
        dom_depth: int,
    ) -> None:
        self.opcode = opcode
        self.args = args
        self.has_side_effect = has_side_effect
        self.cost = cost
        self.dom_depth = dom_depth


class BasicBlock:
    def __init__(
        self,
        block_id: int,
        steps: list[InstructionStep],
        preds: list[int],
        loop_depth: int,
    ) -> None:
        self.block_id = block_id
        self.steps = steps
        self.preds = preds
        self.loop_depth = loop_depth


_OPCODES = (
    "LOAD_FAST",
    "BINARY_ADD",
    "CALL_DIRECT",
    "STORE_SUBSCR",
    "GUARD_TYPE",
    "ALLOC_BOX",
    "PHI_MERGE",
    "UNBOX_INT",
)


def prepare(
    size: int,
) -> tuple[list[BasicBlock], list[tuple[str, int, int, str]]]:
    blocks: list[BasicBlock] = []
    stream_events: list[tuple[str, int, int, str]] = []
    state = 918273645

    for b_idx in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        num_steps = 12 + (state % 13)
        steps: list[InstructionStep] = []
        for s_idx in range(num_steps):
            state = (state * 1103515245 + 12345) & 0x7FFFFFFF
            opcode = _OPCODES[state % len(_OPCODES)]
            arity = 1 + ((state >> 4) % 4)
            args: list[Operand] = []
            for a_idx in range(arity):
                state = (state * 1103515245 + 12345) & 0x7FFFFFFF
                if (state & 0xF) == 0:
                    op_type = None
                else:
                    op_type = _TYPE_PALETTE[(state >> 5) % len(_TYPE_PALETTE)]
                is_const = ((state >> 9) & 3) == 0
                uses = 1 + ((state >> 11) % 6)
                args.append(Operand((b_idx * 32 + s_idx * 4 + a_idx) & 0xFF, op_type, is_const, uses))
            has_side_effect = opcode in ("CALL_DIRECT", "STORE_SUBSCR") or ((state >> 15) & 7) == 0
            cost = 1 + ((state >> 18) % 8)
            dom_depth = (b_idx % 5) + (s_idx % 3)
            steps.append(InstructionStep(opcode, args, has_side_effect, cost, dom_depth))

        preds = [max(0, b_idx - 1), max(0, b_idx - 2)] if b_idx > 0 else []
        blocks.append(BasicBlock(b_idx, steps, preds, (b_idx % 4)))

        for k in range(6):
            state = (state * 1103515245 + 12345) & 0x7FFFFFFF
            cat = _OPCODES[(state >> 3) % len(_OPCODES)]
            val = (state >> 7) % 500 - 120
            weight = 1 + ((state >> 16) % 15)
            tag = "hot" if (state & 3) == 0 else ("warm" if (state & 1) else "cold")
            stream_events.append((cat, val, weight, tag))

    return blocks, stream_events


def scenario_nested_any_all_quantifiers(blocks: list[BasicBlock]) -> tuple[int, int, int, int]:
    """Scan IR basic block slices with nested `all`/`any`, `sum`, and `filter`/`map`."""
    pure_windows = 0
    unboxed_chains = 0
    escape_hazards = 0
    weighted_cost_sum = 0

    for _ in range(1870):
        for block in blocks:
            steps = block.steps
            n = len(steps)
            for start in range(0, n - 3, 2):
                end = min(n, start + 6)
                window = steps[start:end]

                # Models ir2tran.py / constraint.py quantifier checks over instruction windows
                if all(
                    not step.has_side_effect
                    and all(not (arg.type or DEFAULT_TYPE).may_escape for arg in step.args)
                    for step in window
                ):
                    pure_windows += 1

                if any(
                    step.opcode in ("BINARY_ADD", "UNBOX_INT", "GUARD_TYPE")
                    and all((arg.type or DEFAULT_TYPE).is_unboxed or arg.is_const for arg in step.args)
                    for step in window
                ):
                    unboxed_chains += 1

                if any(
                    any(
                        (arg.type or DEFAULT_TYPE).may_escape
                        and (arg.type or DEFAULT_TYPE).nullable
                        and arg.uses > 3
                        for arg in step.args
                    )
                    for step in window
                ):
                    escape_hazards += 1

                active_steps = filter(lambda s: len(s.args) >= 2 and s.cost >= 2, window)
                step_scores = map(
                    lambda s: s.cost * sum((a.type or DEFAULT_TYPE).rank + a.uses for a in s.args),
                    active_steps,
                )
                weighted_cost_sum += sum(
                    score for score in step_scores if score > 6
                )

            if all(s.dom_depth >= 1 for s in steps[:4]) and any(
                s.opcode == "PHI_MERGE" and any(a.uses >= 4 for a in s.args)
                for s in steps
            ):
                pure_windows += block.loop_depth + 1

    return pure_windows, unboxed_chains, escape_hazards, weighted_cost_sum


def _leaf_token_generator(events: list[tuple[str, int, int, str]], start: int, stop: int):
    for idx in range(start, stop):
        cat, val, weight, tag = events[idx]
        if val < -60:
            continue
        yield (cat, val + weight, weight, tag)
        if tag == "hot" and (val & 1) == 0:
            yield (cat, (val // 2) + weight * 2, weight + 1, "boost")


def _delegating_stage(events: list[tuple[str, int, int, str]], chunk_size: int):
    n = len(events)
    for pos in range(0, n, chunk_size):
        yield from _leaf_token_generator(events, pos, min(n, pos + chunk_size))


def _window_accumulator_coroutine(scale: int):
    running = 0
    count = 0
    bonus = 0
    while True:
        item = yield (running, count, bonus)
        if item is None:
            break
        _cat, val, weight, tag = item
        count += 1
        delta = val * weight + scale
        if tag == "boost":
            running += delta * 2
            bonus += weight
        elif tag == "hot":
            running += delta + 5
        else:
            running += delta - 2


def scenario_coroutine_stream_pipeline(
    stream_events: list[tuple[str, int, int, str]],
) -> tuple[int, int, int, int]:
    """Drive generator delegation (`yield from`), `.send()` coroutines, and `itertools`."""
    coro_total = 0
    coro_bonus = 0
    grouped_score = 0
    top_ranked_sum = 0

    n = len(stream_events)
    half = max(1, n // 2)

    for round_idx in range(13100):
        coro = _window_accumulator_coroutine(round_idx & 7)
        next(coro)

        stream_a = _delegating_stage(stream_events[:half], 16)
        stream_b = _delegating_stage(stream_events[half:], 24)
        chained = itertools.chain(stream_a, stream_b)
        sliced = itertools.islice(chained, 0, n + 80)

        filtered_items: list[tuple[int, str, int, int, str]] = []
        for seq_idx, item in enumerate(sliced):
            running, _cnt, bonus = coro.send(item)
            if (seq_idx & 3) == 0:
                coro_total = (coro_total + running) & 0x3FFFFFFF
                coro_bonus += bonus
            filtered_items.append((seq_idx, item[0], item[1], item[2], item[3]))

        by_cat = sorted(filtered_items, key=lambda rec: (rec[1], -rec[3], rec[2]))
        for _cat_key, group_iter in itertools.groupby(by_cat, key=lambda rec: rec[1]):
            grp_list = list(group_iter)
            grouped_score += len(grp_list) * (grp_list[0][3] + grp_list[-1][3])

        paired = zip(by_cat[::2], by_cat[1::2])
        ranked_deltas = sorted(
            (abs(left[2] - right[2]) + left[3] * right[3] for left, right in paired),
            key=lambda x: (x & 0x1F, x),
        )
        top_ranked_sum += sum(itertools.islice(ranked_deltas, 0, 20))

    return coro_total, coro_bonus, grouped_score, top_ranked_sum


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
    blocks, stream_events = prepare(size)
    prep_s = time.perf_counter() - t0

    expected = None
    quant_s = 0.0
    pipe_s = 0.0
    for _ in range(loops):
        r1: tuple[int, ...] = ()
        r2: tuple[int, ...] = ()
        if scenario in ("all", "nested_any_all_quantifiers"):
            s0 = time.perf_counter()
            r1 = scenario_nested_any_all_quantifiers(blocks)
            quant_s += time.perf_counter() - s0
        if scenario in ("all", "coroutine_stream_pipeline"):
            s1 = time.perf_counter()
            r2 = scenario_coroutine_stream_pipeline(stream_events)
            pipe_s += time.perf_counter() - s1
        combined = r1 + r2
        if expected is not None and combined != expected:
            raise AssertionError("Result mismatch across loops")
        expected = combined

    total_s = (quant_s + pipe_s) / loops
    print("topic\titeration/quantifiers_and_generators")
    print(f"size\t{size}")
    print(f"loops\t{loops}")
    print(f"result\t{expected}")
    print(f"prepare\t{prep_s:.6f}")
    if scenario in ("all", "nested_any_all_quantifiers"):
        print(f"nested_any_all_quantifiers\t{quant_s / loops:.6f}")
    if scenario in ("all", "coroutine_stream_pipeline"):
        print(f"coroutine_stream_pipeline\t{pipe_s / loops:.6f}")
    print(f"total\t{total_s:.6f}")


if __name__ == "__main__":
    main()
