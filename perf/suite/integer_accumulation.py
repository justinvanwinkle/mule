"""Accumulate varied signed integers and count positive values.

The generated values depend on runtime input. Every list element contributes
to the result; preparation is separate from the measured kernels.
"""

from perfutils import run_case

NAME = "integer_accumulation"
DESCRIPTION = "Signed integer accumulation with a data-dependent branch"
DEFAULT_SIZE = 1_000_000
DEFAULT_SEED = 42


def prepare(size, seed):
    """Build repeatable signed values without an external random generator."""
    if size < 0:
        raise ValueError("size must be nonnegative")
    state = seed & 0xFFFFFFFF
    values = []
    for _ in range(size):
        state = (state * 1664525 + 1013904223) & 0xFFFFFFFF
        values.append((state >> 8) - 0x800000)
    return values


def untyped(data):
    total = 0
    positives = 0
    for value in data:
        total += value
        if value > 0:
            positives += 1
    return total, positives


def partial(data: list[int]):
    total = 0
    positives = 0
    for value in data:
        total += value
        if value > 0:
            positives += 1
    return total, positives


def typed(data: list[int]) -> tuple[int, int]:
    total: int = 0
    positives: int = 0
    for value in data:
        total += value
        if value > 0:
            positives += 1
    return total, positives


def main():
    run_case(
        NAME,
        DESCRIPTION,
        DEFAULT_SIZE,
        DEFAULT_SEED,
        prepare,
        {"untyped": untyped, "partial": partial, "typed": typed},
    )


if __name__ == "__main__":
    main()
