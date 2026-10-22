"""Construct, algebraically simplify, and evaluate AST records with match/case.

Exercises slotted record classes with __match_args__, structural pattern
matching (nested class patterns, OR patterns, and guards), and functional tree
rewriting that allocates simplified AST nodes.
"""

from perfutils import run_case

NAME = "dataclass_ast"
DESCRIPTION = (
    "Slotted AST records, match/case structural pattern matching, and tree rewriting"
)
DEFAULT_SIZE = 4_000
DEFAULT_SEED = 42


class Lit:
    __slots__ = ("value",)
    __match_args__ = ("value",)

    def __init__(self, value: int):
        self.value = value


class Var:
    __slots__ = ("slot",)
    __match_args__ = ("slot",)

    def __init__(self, slot: int):
        self.slot = slot


class Unary:
    __slots__ = ("op", "operand")
    __match_args__ = ("op", "operand")

    def __init__(self, op: int, operand):
        self.op = op
        self.operand = operand


class Binary:
    __slots__ = ("left", "op", "right")
    __match_args__ = ("op", "left", "right")

    def __init__(self, op: int, left, right):
        self.op = op
        self.left = left
        self.right = right


class Select:
    __slots__ = ("cond", "else_branch", "then_branch")
    __match_args__ = ("cond", "then_branch", "else_branch")

    def __init__(self, cond, then_branch, else_branch):
        self.cond = cond
        self.then_branch = then_branch
        self.else_branch = else_branch


def build_tree(depth: int, state: int) -> tuple[object, int]:
    state = (state * 1103515245 + 12345) & 0x7FFFFFFF
    if depth <= 0:
        if (state & 3) == 0:
            return Var((state >> 4) & 3), state
        return Lit(((state >> 8) % 19) - 9), state

    kind = (state >> 4) % 7
    if kind == 0:
        inner, state = build_tree(depth - 1, state)
        return Unary(1, Unary(1, inner)), state
    if kind == 1:
        left, state = build_tree(depth - 1, state)
        return Binary(1, left, Lit(0)), state
    if kind == 2:
        right, state = build_tree(depth - 1, state)
        return Binary(3, Lit(1), right), state
    if kind == 3:
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        a = ((state >> 8) % 15) - 7
        b = ((state >> 16) % 15) - 7
        sub, state = build_tree(depth - 1, state)
        return Binary(1, Binary(1, Lit(a), Lit(b)), sub), state
    if kind == 4:
        cond_val = (state >> 9) & 1
        then_b, state = build_tree(depth - 1, state)
        else_b, state = build_tree(depth - 1, state)
        return Select(Lit(cond_val), then_b, else_b), state
    if kind == 5:
        cond, state = build_tree(depth - 1, state)
        then_b, state = build_tree(depth - 1, state)
        else_b, state = build_tree(depth - 1, state)
        return Select(cond, then_b, else_b), state

    op = ((state >> 8) % 3) + 1
    left, state = build_tree(depth - 1, state)
    right, state = build_tree(depth - 1, state)
    return Binary(op, left, right), state


def prepare(size: int, seed: int) -> tuple[list[int], list[tuple[int, int, int, int]]]:
    if size < 1:
        raise ValueError("size must be positive")
    state = seed & 0x7FFFFFFF
    tree_seeds: list[int] = []
    for _ in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        tree_seeds.append(state)
    envs: list[tuple[int, int, int, int]] = []
    for _ in range(8):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        v0 = ((state >> 4) % 21) - 10
        v1 = ((state >> 10) % 21) - 10
        v2 = ((state >> 16) % 21) - 10
        v3 = ((state >> 22) % 21) - 10
        envs.append((v0, v1, v2, v3))
    return tree_seeds, envs


def simplify(node):
    match node:
        case Lit(_) | Var(_):
            return node
        case Unary(op, operand):
            inner = simplify(operand)
            match inner:
                case Lit(v) if op == 1:
                    return Lit(-v)
                case Lit(v) if op == 2:
                    return Lit(v if v >= 0 else -v)
                case Unary(1, core) if op == 1:
                    return core
                case _:
                    return Unary(op, inner)
        case Binary(op, left_raw, right_raw):
            left = simplify(left_raw)
            right = simplify(right_raw)
            match (op, left, right):
                case (1, Lit(a), Lit(b)):
                    return Lit(a + b)
                case (2, Lit(a), Lit(b)):
                    return Lit(a - b)
                case (3, Lit(a), Lit(b)):
                    return Lit(a * b)
                case (1, expr, Lit(0)) | (1, Lit(0), expr):
                    return expr
                case (2, expr, Lit(0)):
                    return expr
                case (2, Lit(0), expr):
                    return simplify(Unary(1, expr))
                case (3, _, Lit(0)) | (3, Lit(0), _):
                    return Lit(0)
                case (3, expr, Lit(1)) | (3, Lit(1), expr):
                    return expr
                case _:
                    return Binary(op, left, right)
        case Select(cond_raw, then_raw, else_raw):
            cond = simplify(cond_raw)
            match cond:
                case Lit(c):
                    return simplify(then_raw) if c != 0 else simplify(else_raw)
                case _:
                    return Select(cond, simplify(then_raw), simplify(else_raw))
        case _:
            return node


def node_count(node) -> int:
    match node:
        case Lit(_) | Var(_):
            return 1
        case Unary(_, operand):
            return 1 + node_count(operand)
        case Binary(_, left, right):
            return 1 + node_count(left) + node_count(right)
        case Select(cond, then_b, else_b):
            return 1 + node_count(cond) + node_count(then_b) + node_count(else_b)
        case _:
            return 0


def eval_ast(node, env: tuple[int, int, int, int]) -> int:
    match node:
        case Lit(v):
            return v
        case Var(slot):
            return env[slot]
        case Unary(1, operand):
            return -eval_ast(operand, env)
        case Unary(_, operand):
            val = eval_ast(operand, env)
            return val if val >= 0 else -val
        case Binary(1, left, right):
            return eval_ast(left, env) + eval_ast(right, env)
        case Binary(2, left, right):
            return eval_ast(left, env) - eval_ast(right, env)
        case Binary(_, left, right):
            return (eval_ast(left, env) * eval_ast(right, env)) % 100_003
        case Select(cond, then_b, else_b):
            if eval_ast(cond, env) != 0:
                return eval_ast(then_b, env)
            return eval_ast(else_b, env)
        case _:
            return 0


def run(
    data: tuple[list[int], list[tuple[int, int, int, int]]],
) -> tuple[int, int, int, int]:
    tree_seeds, envs = data
    raw_nodes = 0
    simplified_nodes = 0
    eval_sum = 0
    digest = 0

    for tree_seed in tree_seeds:
        tree, _ = build_tree(4, tree_seed)
        raw_nodes += node_count(tree)
        rewritten = simplify(tree)
        simplified_nodes += node_count(rewritten)
        for env in envs:
            val = eval_ast(rewritten, env)
            eval_sum += val
            digest = ((digest * 33) + (val & 0xFFFF)) & 0x3FFFFFFF

    return raw_nodes, simplified_nodes, eval_sum, digest


def main():
    run_case(NAME, DESCRIPTION, DEFAULT_SIZE, DEFAULT_SEED, prepare, run)


if __name__ == "__main__":
    main()
