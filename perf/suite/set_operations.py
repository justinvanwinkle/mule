"""Maintain rotating integer sets with membership probes and set algebra.

Exercises set construction, add, discard, membership testing, and binary set
algebra (&, |, -, ^) with order-independent reductions.
"""

from perfutils import run_case

NAME = "set_operations"
DESCRIPTION = "Mutable set updates, membership probes, and binary set algebra"
DEFAULT_SIZE = 30_000
DEFAULT_SEED = 42


def prepare(size: int, seed: int) -> list[tuple[int, int, int]]:
    if size < 1:
        raise ValueError("size must be positive")
    state = seed & 0x7FFFFFFF
    ops: list[tuple[int, int, int]] = []
    for _ in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        bucket = (state >> 4) & 7
        key = (state >> 8) & 511
        kind = (state >> 18) & 3
        ops.append((bucket, key, kind))
    return ops


def run(data: list[tuple[int, int, int]]) -> tuple[int, int, int, int]:
    buckets: list[set[int]] = []
    for b in range(8):
        initial: set[int] = set()
        base = b * 16
        for k in range(16):
            initial.add(base + k)
        buckets.append(initial)

    hits = 0
    algebra_card = 0

    for bucket, key, kind in data:
        s = buckets[bucket]
        partner = buckets[(bucket + 1) & 7]
        if kind == 0:
            s.add(key)
            if key in partner:
                hits += 1
        elif kind == 1:
            if key in s:
                hits += 1
            s.discard(key)
            s.add((key + 37) & 511)
        elif kind == 2:
            common = s & partner
            diff = s - partner
            algebra_card += len(common) + len(diff)
            if len(common) > 0:
                s.add((key ^ len(common)) & 511)
        else:
            sym = s ^ partner
            merged = s | partner
            algebra_card += len(sym) + len(merged)
            if len(s) > 96:
                buckets[bucket] = diff_keep(s, partner, key)

    final_members = 0
    element_sum = 0
    for b in range(8):
        s = buckets[b]
        weight = b + 1
        final_members += len(s)
        for val in s:
            element_sum += val * weight

    return hits, algebra_card, final_members, element_sum


def diff_keep(left: set[int], right: set[int], extra: int) -> set[int]:
    trimmed = left - right
    trimmed.add(extra)
    return trimmed


def main():
    run_case(NAME, DESCRIPTION, DEFAULT_SIZE, DEFAULT_SEED, prepare, run)


if __name__ == "__main__":
    main()
