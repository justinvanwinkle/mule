"""Evaluate recursive expression trees with lexical Let bindings.

Preparation builds a seeded DAG of expression nodes and a sequence of runtime
variable pairs; the kernel evaluates the expression tree for each pair.
"""

from perfutils import run_case

NAME = "expression_tree"
DESCRIPTION = "Recursive AST evaluation with polymorphic nodes and dict environments"
DEFAULT_SIZE = 40_000
DEFAULT_SEED = 42


class Expr:
    def eval_node(self, env: dict[str, int]) -> int:
        return 0


class LitExpr(Expr):
    def __init__(self, value: int):
        self.value = value

    def eval_node(self, env: dict[str, int]) -> int:
        return self.value


class VarExpr(Expr):
    def __init__(self, name: str):
        self.name = name

    def eval_node(self, env: dict[str, int]) -> int:
        return env.get(self.name, 0)


class AddExpr(Expr):
    def __init__(self, left: Expr, right: Expr):
        self.left = left
        self.right = right

    def eval_node(self, env: dict[str, int]) -> int:
        return (self.left.eval_node(env) + self.right.eval_node(env)) & 0x3FFFFFFF


class MulExpr(Expr):
    def __init__(self, left: Expr, right: Expr):
        self.left = left
        self.right = right

    def eval_node(self, env: dict[str, int]) -> int:
        return (self.left.eval_node(env) * self.right.eval_node(env)) & 0x3FFFFFFF


class LetExpr(Expr):
    def __init__(self, name: str, bound: Expr, body: Expr):
        self.name = name
        self.bound = bound
        self.body = body

    def eval_node(self, env: dict[str, int]) -> int:
        val = self.bound.eval_node(env)
        had_old = self.name in env
        old_val = env.get(self.name, 0)
        env[self.name] = val
        res = self.body.eval_node(env)
        if had_old:
            env[self.name] = old_val
        else:
            del env[self.name]
        return res


def build_tree(depth: int, seed: int) -> Expr:
    if depth <= 0:
        kind = seed % 3
        if kind == 0:
            return LitExpr((seed % 19) + 1)
        if kind == 1:
            return VarExpr("x")
        return VarExpr("y")
    s1 = (seed * 1103515245 + 12345) & 0x7FFFFFFF
    s2 = (s1 * 1103515245 + 12345) & 0x7FFFFFFF
    mode = seed % 3
    if mode == 0:
        return AddExpr(build_tree(depth - 1, s1), build_tree(depth - 1, s2))
    if mode == 1:
        return MulExpr(build_tree(depth - 1, s1), build_tree(depth - 1, s2))
    var = "x" if (seed & 4) == 0 else "y"
    return LetExpr(
        var,
        AddExpr(build_tree(depth - 2, s1), LitExpr(3)),
        build_tree(depth - 1, s2),
    )


def prepare(size: int, seed: int) -> list[tuple[int, int, int]]:
    if size < 0:
        raise ValueError("size must be nonnegative")
    state = seed & 0x7FFFFFFF
    inputs: list[tuple[int, int, int]] = []
    for _ in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        tree_idx = (state >> 4) & 3
        x_val = (state >> 8) & 31
        y_val = (state >> 13) & 31
        inputs.append((tree_idx, x_val, y_val))
    return inputs


def run(data: list[tuple[int, int, int]]) -> tuple[int, int]:
    trees = [
        build_tree(4, 17),
        build_tree(4, 53),
        build_tree(4, 97),
        build_tree(4, 131),
    ]
    env: dict[str, int] = {"x": 0, "y": 0}
    rounds = max(1, len(data) // 465)
    checksum = 0
    high_count = 0
    for round_idx in range(rounds):
        bias = round_idx & 7
        for tree_idx, x_val, y_val in data:
            env["x"] = x_val + bias
            env["y"] = y_val ^ bias
            value = trees[(tree_idx + round_idx) & 3].eval_node(env)
            checksum = (checksum + value) & 0x3FFFFFFF
            if value > 1000:
                high_count += 1
    return checksum, high_count


def main():
    run_case(NAME, DESCRIPTION, DEFAULT_SIZE, DEFAULT_SEED, prepare, run)


if __name__ == "__main__":
    main()
