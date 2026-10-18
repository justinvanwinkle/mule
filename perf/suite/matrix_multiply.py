"""Multiply dense integer matrices and reduce diagonal and Frobenius norms.

Exercises nested loops, 2D list indexing, list pre-transposition, and bounded
integer arithmetic.
"""

from perfutils import run_case

NAME = "matrix_multiply"
DESCRIPTION = "Dense 2D matrix multiplication and norm reduction"
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


def run(data: tuple[list[list[int]], list[list[int]]]) -> tuple[int, int]:
    left, right = data
    n = len(left)
    transposed: list[list[int]] = []
    for col in range(n):
        t_row: list[int] = []
        for row in range(n):
            t_row.append(right[row][col])
        transposed.append(t_row)

    trace = 0
    frobenius = 0
    for i in range(n):
        row_a = left[i]
        for j in range(n):
            col_b = transposed[j]
            acc = 0
            for k in range(n):
                acc += row_a[k] * col_b[k]
            if i == j:
                trace += acc
            frobenius = (frobenius + acc * acc) & 0x3FFFFFFF
    return trace, frobenius


def main():
    run_case(NAME, DESCRIPTION, DEFAULT_SIZE, DEFAULT_SEED, prepare, run)


if __name__ == "__main__":
    main()
