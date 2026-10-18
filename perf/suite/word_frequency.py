"""Count token frequencies, bucket values, and reduce top bucket summaries.

Exercises dictionary probing with both string and integer keys alongside dynamic
list accumulation and comprehension filtering.
"""

from perfutils import run_case

NAME = "word_frequency"
DESCRIPTION = "Dictionary frequency counting and bucketed list reduction"
DEFAULT_SIZE = 80_000
DEFAULT_SEED = 42

_VOCAB = (
    "alpha",
    "beta",
    "gamma",
    "delta",
    "epsilon",
    "zeta",
    "eta",
    "theta",
    "iota",
    "kappa",
    "lambda",
    "mu",
    "nu",
    "xi",
    "omicron",
    "pi",
)


def prepare(size: int, seed: int) -> list[tuple[str, int]]:
    if size < 0:
        raise ValueError("size must be nonnegative")
    state = seed & 0x7FFFFFFF
    items: list[tuple[str, int]] = []
    for i in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        word = _VOCAB[(state >> 5) & 15]
        suffix = str((state >> 12) % 32)
        weight = ((state >> 18) % 97) - 30 + (i & 7)
        items.append((word + "_" + suffix, weight))
    return items


def run(data: list[tuple[str, int]]) -> tuple[int, int, int]:
    counts: dict[str, int] = {}
    totals: dict[str, int] = {}
    rounds = max(1, len(data) // 198)
    bucket_sum = 0
    for round_idx in range(rounds):
        bias = (round_idx & 7) - 3
        positive_weights: list[int] = []
        for token, weight in data:
            adj = weight + bias
            counts[token] = counts.get(token, 0) + 1
            totals[token] = totals.get(token, 0) + adj
            if adj > 0:
                positive_weights.append(adj)

        filtered = [w * 3 + 1 for w in positive_weights if (w & 1) == 1]
        for value in filtered:
            bucket_sum = (bucket_sum + value) & 0x3FFFFFFF

    freq_checksum = 0
    distinct = 0
    for token, count in counts.items():
        distinct += 1
        freq_checksum = (
            freq_checksum + count * 17 + totals[token] + len(token)
        ) & 0x3FFFFFFF

    return distinct, freq_checksum, bucket_sum


def main():
    run_case(NAME, DESCRIPTION, DEFAULT_SIZE, DEFAULT_SEED, prepare, run)


if __name__ == "__main__":
    main()
