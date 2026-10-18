"""Sort, merge, and query weighted integer intervals via binary search.

Exercises list sorting of tuples, tuple unpacking, and binary search over
coalesced intervals.
"""

from perfutils import run_case

NAME = "interval_merging"
DESCRIPTION = "Interval sorting, coalescing, and binary-search point queries"
DEFAULT_SIZE = 50_000
DEFAULT_SEED = 42


def prepare(size: int, seed: int) -> tuple[list[tuple[int, int, int]], list[int]]:
    if size < 1:
        raise ValueError("size must be positive")
    state = seed & 0x7FFFFFFF
    intervals: list[tuple[int, int, int]] = []
    queries: list[int] = []
    span = size * 10
    for _ in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        start = (state >> 4) % span
        length = ((state >> 16) % 35) + 1
        weight = ((state >> 8) % 19) + 1
        intervals.append((start, start + length, weight))
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        queries.append((state >> 4) % span)
    return intervals, queries


def run(
    data: tuple[list[tuple[int, int, int]], list[int]],
) -> tuple[int, int, int]:
    raw_intervals, queries = data
    ordered = sorted(raw_intervals)
    merged: list[tuple[int, int, int]] = []
    cur_start, cur_end, cur_weight = ordered[0]
    for start, end, weight in ordered[1:]:
        if start <= cur_end:
            cur_end = max(cur_end, end)
            cur_weight += weight
        else:
            merged.append((cur_start, cur_end, cur_weight))
            cur_start = start
            cur_end = end
            cur_weight = weight
    merged.append((cur_start, cur_end, cur_weight))

    rounds = max(1, len(queries) // 465)
    covered_queries = 0
    weight_sum = 0
    num_merged = len(merged)
    for round_idx in range(rounds):
        shift = (round_idx * 7) & 63
        for point in queries:
            target = point + shift
            lo = 0
            hi = num_merged - 1
            matched_weight = 0
            while lo <= hi:
                mid = (lo + hi) >> 1
                m_start, m_end, m_weight = merged[mid]
                if target < m_start:
                    hi = mid - 1
                elif target > m_end:
                    lo = mid + 1
                else:
                    matched_weight = m_weight
                    break
            if matched_weight > 0:
                covered_queries += 1
                weight_sum = (weight_sum + matched_weight) & 0x3FFFFFFF
    return num_merged, covered_queries, weight_sum


def main():
    run_case(NAME, DESCRIPTION, DEFAULT_SIZE, DEFAULT_SEED, prepare, run)


if __name__ == "__main__":
    main()
