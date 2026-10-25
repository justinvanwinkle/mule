# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# contributed by Antoine Pitrou
# modified by Dominique Wahli
# modified by Heinrich Acker
# 2to3
# *reset*
#
# Signed-int64-annotated copy of binarytrees/binarytrees.python3-2.py.
# The tree representation and algorithm are unchanged.

import sys

from mule_types import int64


def make_tree(depth: int64):
    if not depth:
        return None, None
    depth -= 1
    return make_tree(depth), make_tree(depth)


def check_tree(node) -> int64:
    (left, right) = node
    if not left:
        return 1
    return 1 + check_tree(left) + check_tree(right)


min_depth: int64 = 4
max_depth: int64 = max(min_depth + 2, int(sys.argv[1]))
stretch_depth: int64 = max_depth + 1

print(
    "stretch tree of depth %d\t check:" % stretch_depth,
    check_tree(make_tree(stretch_depth)),
)

long_lived_tree = make_tree(max_depth)

iterations: int64 = 2**max_depth
depth: int64
i: int64

for depth in range(min_depth, stretch_depth, 2):
    check: int64 = 0
    for i in range(1, iterations + 1):
        check += check_tree(make_tree(depth))

    print("%d\t trees of depth %d\t check:" % (iterations, depth), check)
    iterations //= 4

print(
    "long lived tree of depth %d\t check:" % max_depth,
    check_tree(long_lived_tree),
)
