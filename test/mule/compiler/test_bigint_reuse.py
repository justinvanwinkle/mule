"""Bigint storage reuse preserves aliases, evaluation order, and ownership."""

import pytest

from mule.compiler.main import compile_source
from mule.compiler.node import CallKind, Combination
from test.mule.compiler.test_native import run_native

BIG = 2**100 + 7


@pytest.mark.parametrize(
    ("operation", "right", "answer"),
    [
        ("*", 3, BIG * 3),
        ("*", -3, BIG * -3),
        ("*", 0, 0),
        ("*", BIG, BIG * BIG),
        ("+", -BIG, 0),
        ("+", BIG, BIG + BIG),
        ("-", BIG - 1, 1),
        ("-", -BIG, BIG + BIG),
    ],
)
@pytest.mark.parametrize("alias", [False, True])
def test_local_augmented_bigint_preserves_saved_values(
    tmp_path, operation, right, answer, alias
):
    saved = " saved = x\n" if alias else ""
    printed = "print(x, saved)" if alias else "print(x)"
    source = (
        f"def run():\n x = {BIG}\n{saved} x {operation}= {right}\n {printed}\nrun()\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == (f"{answer} {BIG}\n" if alias else f"{answer}\n")
    assert result.stderr == ""


@pytest.mark.parametrize(
    ("body", "expected"),
    [
        ("x *= x\n print(x)", f"{BIG * BIG}\n"),
        ("y = x * 3\n print(x, y)", f"{BIG} {BIG * 3}\n"),
        ("x *= (x := 3)\n print(x)", f"{BIG * 3}\n"),
        ("x *= (saved := x)\n print(x, saved)", f"{BIG * BIG} {BIG}\n"),
        ("x *= 0\n x += 7\n print(x)", "7\n"),
    ],
)
def test_bigint_reuse_obeys_operand_evaluation(tmp_path, body, expected):
    result = run_native(tmp_path, f"def run():\n x = {BIG}\n {body}\nrun()\n")
    assert result.returncode == 0, result.stderr
    assert result.stdout == expected
    assert result.stderr == ""


def inplace_operations(lambda_):
    return [
        node
        for block in lambda_.component.blocks()
        for node in block.nodes()
        if isinstance(node, Combination)
        and node.kind is CallKind.KNOWN
        and node.fun_info.name == "%imultiply"
    ]


def test_frontend_reuses_only_local_augmented_assignment():
    compilation = compile_source(
        "x = 1\nx *= 2\ndef f():\n x = 1\n x *= 2\n return x\n"
    )
    global_operation = inplace_operations(compilation.lambdas[0])[0]
    local_operation = inplace_operations(compilation.lambdas[1])[0]
    assert global_operation.info.reuse_local is None
    assert local_operation.info.reuse_local == "x"
