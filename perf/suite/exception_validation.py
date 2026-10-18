"""Validate structured integer records using exceptions for rejected inputs.

Exercises exception construction, raising, stack unwinding, and typed handler
dispatch across nested calls.
"""

from perfutils import run_case

NAME = "exception_validation"
DESCRIPTION = "Nested validation raising and catching domain exceptions"
DEFAULT_SIZE = 50_000
DEFAULT_SEED = 42


class ValidationError(ValueError):
    def __init__(self, code: int):
        super().__init__("validation error")
        self.code = code


class RangeError(ValidationError):
    def __init__(self, code: int, penalty: int):
        super().__init__(code)
        self.penalty = penalty


def check_field(raw: int, bound: int) -> int:
    mode = raw & 7
    if mode == 0:
        raise ValidationError(raw & 63)
    if mode == 5:
        raise RangeError(raw & 31, (raw >> 4) & 15)
    value = (raw * 17 + 11) % bound
    return value


def validate_record(a: int, b: int) -> int:
    first = check_field(a, 997)
    second = check_field(b, 991)
    return first * 3 + second


def prepare(size: int, seed: int) -> list[tuple[int, int]]:
    if size < 0:
        raise ValueError("size must be nonnegative")
    state = seed & 0x7FFFFFFF
    pairs: list[tuple[int, int]] = []
    for _ in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        a = (state >> 5) & 0x3FFF
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        b = (state >> 5) & 0x3FFF
        pairs.append((a, b))
    return pairs


def run(data: list[tuple[int, int]]) -> tuple[int, int, int]:
    rounds = max(1, len(data) // 225)
    valid_sum = 0
    range_errors = 0
    other_errors = 0
    for round_idx in range(rounds):
        salt = round_idx & 7
        for a, b in data:
            try:
                valid_sum = (
                    valid_sum + validate_record(a ^ salt, b + salt)
                ) & 0x3FFFFFFF
            except RangeError as err:
                range_errors += 1
                valid_sum = (valid_sum + err.code + err.penalty) & 0x3FFFFFFF
            except ValidationError as err:
                other_errors += 1
                valid_sum = (valid_sum + err.code) & 0x3FFFFFFF
    return valid_sum, range_errors, other_errors


def main():
    run_case(NAME, DESCRIPTION, DEFAULT_SIZE, DEFAULT_SEED, prepare, run)


if __name__ == "__main__":
    main()
