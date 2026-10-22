"""Accumulate varied signed integers with Mule int64 annotations and speed=3, safety=0.

The generated values depend on runtime input. Every list element contributes
to the result; preparation is separate from the measured kernels.
"""

from perfutils import run_case

from mule import int64, optimize

__speed__ = 3
__safety__ = 0
__debug_level__ = 0

NAME = "integer_accumulation_typed"
DESCRIPTION = "Signed integer accumulation with int64 annotations and speed=3, safety=0"
DEFAULT_SIZE = 1_000_000
DEFAULT_SEED = 42


@optimize(speed=3, safety=0, debug=0)
def prepare(size: int64, seed: int64):
    """Build repeatable signed values without an external random generator."""
    if size < 0:
        raise ValueError("size must be nonnegative")
    state: int64 = seed & 0xFFFFFFFF
    values = []
    for _ in range(size):
        state = (state * 1664525 + 1013904223) & 0xFFFFFFFF
        values.append((state >> 8) - 0x800000)
    return values


@optimize(speed=3, safety=0, debug=0)
def run(data: list[int]) -> tuple[int, int]:
    total: int64 = 0
    positives: int64 = 0
    value: int64
    for value in data:
        total += value
        if value > 0:
            positives += 1
    return total, positives


def main():
    run_case(NAME, DESCRIPTION, DEFAULT_SIZE, DEFAULT_SEED, prepare, run)


if __name__ == "__main__":
    main()
