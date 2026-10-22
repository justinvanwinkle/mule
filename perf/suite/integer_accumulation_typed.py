"""Accumulate varied signed integers with Mule int64 annotations and speed=3, safety=0.

The generated values and algorithm match integer_accumulation, adding explicit
Mule int64 type annotations and high-speed/zero-safety compiler policy.
"""

from perfutils import run_case

from mule import int64, optimize

__speed__ = 3
__safety__ = 0

NAME = "integer_accumulation_typed"
DESCRIPTION = "Signed integer accumulation with int64 annotations and speed=3, safety=0"
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


@optimize(speed=3, safety=0)
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
