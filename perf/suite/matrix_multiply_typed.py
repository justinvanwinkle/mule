"""Multiply dense integer matrices with Mule int64 annotations and speed=3, safety=0.

The generated matrices and algorithm match matrix_multiply, adding explicit
Mule int64 annotations and high-speed/zero-safety compiler policy.
"""

from perfutils import run_case

from mule import int64, optimize

__speed__ = 3
__safety__ = 0

NAME = "matrix_multiply_typed"
DESCRIPTION = (
    "Dense 2D matrix multiplication with int64 annotations and speed=3, safety=0"
)
DEFAULT_SIZE = 490
DEFAULT_SEED = 42


def prepare(size: int, seed: int) -> tuple[list[list[int]], list[list[int]]]:
    if size < 1:
        raise ValueError("size must be positive")
    state = seed & 0x7FFFFFFF
    left: list[list[int]] = []
    right: list[list[int]] = []
    for _ in range(size):
        row_a: list[int] = []
        row_b: list[int] = []
        for _ in range(size):
            state = (state * 1103515245 + 12345) & 0x7FFFFFFF
            row_a.append(((state >> 8) % 61) - 30)
            state = (state * 1103515245 + 12345) & 0x7FFFFFFF
            row_b.append(((state >> 8) % 61) - 30)
        left.append(row_a)
        right.append(row_b)
    return left, right


@optimize(speed=3, safety=0)
def run(data: tuple[list[list[int]], list[list[int]]]) -> tuple[int, int]:
    left, right = data
    n: int64 = len(left)
    transposed: list[list[int]] = []
    col: int64
    row: int64
    for col in range(n):
        t_row: list[int] = []
        for row in range(n):
            t_row.append(right[row][col])
        transposed.append(t_row)

    trace: int64 = 0
    frobenius: int64 = 0
    i: int64
    j: int64
    k: int64
    for i in range(n):
        row_a = left[i]
        for j in range(n):
            col_b = transposed[j]
            acc: int64 = 0
            for k in range(n):
                va: int64 = row_a[k]
                vb: int64 = col_b[k]
                acc += va * vb
            if i == j:
                trace += acc
            frobenius = (frobenius + acc * acc) & 0x3FFFFFFF
    return trace, frobenius


def main():
    run_case(NAME, DESCRIPTION, DEFAULT_SIZE, DEFAULT_SEED, prepare, run)


if __name__ == "__main__":
    main()
