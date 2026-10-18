"""Compose and invoke stateful lexical closures across a signal pipeline.

Exercises nonlocal cell reads and writes alongside higher-order function
calls.
"""

from perfutils import run_case

NAME = "closure_pipeline"
DESCRIPTION = "Higher-order function pipelines with mutable lexical closure state"
DEFAULT_SIZE = 100_000
DEFAULT_SEED = 42


def make_stepper(seed: int, mul: int):
    acc = seed

    def step(x: int) -> int:
        nonlocal acc
        acc = (acc * mul + x) & 0x3FFFFFFF
        return acc

    return step


def apply_pipeline(val: int, s1, s2, s3) -> int:
    return s3(s2(s1(val)))


def prepare(size: int, seed: int) -> list[int]:
    if size < 0:
        raise ValueError("size must be nonnegative")
    state = seed & 0x7FFFFFFF
    values: list[int] = []
    for _ in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        values.append((state >> 6) & 0xFFFF)
    return values


def run(data: list[int]) -> tuple[int, int]:
    s1 = make_stepper(19, 31)
    s2 = make_stepper(47, 17)
    s3 = make_stepper(83, 13)
    rounds = max(1, len(data) // 580)
    total = 0
    odd_count = 0
    for round_idx in range(rounds):
        bias = round_idx & 15
        for val in data:
            out = apply_pipeline(val + bias, s1, s2, s3)
            total = (total + out) & 0x3FFFFFFF
            if (out & 1) == 1:
                odd_count += 1
    return total, odd_count


def main():
    run_case(NAME, DESCRIPTION, DEFAULT_SIZE, DEFAULT_SEED, prepare, run)


if __name__ == "__main__":
    main()
