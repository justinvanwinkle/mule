"""Allocate, traverse, and discard recursive binary trees alongside a long-lived tree.

Exercises recursive function calls, tuple/node allocation and deallocation
churn (reference-count traffic vs GC), and pointer chasing.
"""

from perfutils import run_case

NAME = "binary_trees"
DESCRIPTION = "Recursive binary tree allocation, traversal, and deallocation churn"
DEFAULT_SIZE = 3_500
DEFAULT_SEED = 42


def prepare(size: int, seed: int) -> list[tuple[int, int]]:
    if size < 1:
        raise ValueError("size must be positive")
    state = seed & 0x7FFFFFFF
    requests: list[tuple[int, int]] = []
    for _ in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        depth = 4 + ((state >> 8) % 5)
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        root_val = ((state >> 8) % 201) - 100
        requests.append((depth, root_val))
    return requests


def make_tree(item: int, depth: int):
    if depth <= 0:
        return (item, None, None)
    next_depth = depth - 1
    return (
        item,
        make_tree(item * 2 - 1, next_depth),
        make_tree(item * 2 + 1, next_depth),
    )


def check_tree(node) -> int:
    item, left, right = node
    if left is None:
        return item
    return item + check_tree(left) - check_tree(right)


def run(data: list[tuple[int, int]]) -> tuple[int, int, int]:
    stretch_check = check_tree(make_tree(1, 10))
    long_lived = make_tree(3, 9)

    total_check = 0
    total_nodes = 0
    for depth, root_val in data:
        pos_tree = make_tree(root_val, depth)
        neg_tree = make_tree(-root_val, depth)
        total_check += check_tree(pos_tree) + check_tree(neg_tree)
        total_nodes += ((1 << (depth + 1)) - 1) * 2

    long_check = check_tree(long_lived) + stretch_check
    return total_check, long_check, total_nodes


def main():
    run_case(NAME, DESCRIPTION, DEFAULT_SIZE, DEFAULT_SEED, prepare, run)


if __name__ == "__main__":
    main()
