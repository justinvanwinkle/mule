"""Stream integer signals through chained yield generators and run-length coalescing.

Exercises generator frame suspension and resumption, yield pipelines, stateful
sliding-window iteration, and run-length aggregation.
"""

from perfutils import run_case

NAME = "generator_pipeline"
DESCRIPTION = (
    "Chained yield generators with sliding-window deltas and run-length coalescing"
)
DEFAULT_SIZE = 75_000
DEFAULT_SEED = 42


def prepare(size: int, seed: int) -> list[int]:
    if size < 1:
        raise ValueError("size must be positive")
    state = seed & 0x7FFFFFFF
    values: list[int] = []
    for _ in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        values.append(((state >> 8) % 2001) - 1000)
    return values


def iter_filtered(values: list[int], limit: int, bias: int):
    for idx, raw in enumerate(values):
        val = raw + bias
        if -limit <= val <= limit:
            yield idx, val
            if val % 7 == 0:
                yield idx, val // 3


def iter_deltas(stream):
    have_prev1 = False
    have_prev2 = False
    prev1 = 0
    prev2 = 0
    for idx, val in stream:
        d1 = val - prev1 if have_prev1 else val
        d2 = (val - 2 * prev1 + prev2) if have_prev2 else d1
        prev2 = prev1
        prev1 = val
        have_prev2 = have_prev1
        have_prev1 = True
        yield idx, val, d1, d2


def iter_runs(delta_stream):
    cur_sign = 0
    run_len = 0
    run_sum = 0
    max_curv = 0
    for _idx, val, d1, d2 in delta_stream:
        sign = 1 if d1 > 0 else (-1 if d1 < 0 else 0)
        curv = d2 if d2 >= 0 else -d2
        if run_len == 0:
            cur_sign = sign
            run_len = 1
            run_sum = val
            max_curv = curv
        elif sign == cur_sign and run_len < 16:
            run_len += 1
            run_sum += val
            max_curv = max(max_curv, curv)
        else:
            yield cur_sign, run_len, run_sum, max_curv
            cur_sign = sign
            run_len = 1
            run_sum = val
            max_curv = curv
    if run_len > 0:
        yield cur_sign, run_len, run_sum, max_curv


def run(data: list[int]) -> tuple[int, int, int, int]:
    run_count = 0
    total_len = 0
    weighted_sum = 0
    curv_digest = 0

    for round_idx in range(4):
        limit = 750 + round_idx * 60
        bias = (round_idx - 1) * 15
        pipeline = iter_runs(iter_deltas(iter_filtered(data, limit, bias)))
        for sign, length, rsum, mcurv in pipeline:
            run_count += 1
            total_len += length
            weighted_sum += sign * rsum + length * 5
            curv_digest = ((curv_digest * 31) + mcurv + length) & 0x3FFFFFFF

    return run_count, total_len, weighted_sum, curv_digest


def main():
    run_case(NAME, DESCRIPTION, DEFAULT_SIZE, DEFAULT_SEED, prepare, run)


if __name__ == "__main__":
    main()
