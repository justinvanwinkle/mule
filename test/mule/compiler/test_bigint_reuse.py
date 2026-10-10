"""Bigint storage reuse preserves aliases, evaluation order, and ownership."""

import pytest

from mule import ast
from mule.compiler import ir, lower
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


def test_reuse_hint_requires_immediate_store_to_same_local():
    module = ir.Module(
        [
            ir.Function(
                "entry",
                (),
                ("x",),
                [
                    ir.Block(
                        "entry",
                        [
                            ir.Constant("left", BIG),
                            ir.Constant("right", 2),
                            ir.Binary(
                                "product",
                                ast.BinaryOperator.MULTIPLY,
                                "left",
                                "right",
                                True,
                                "x",
                            ),
                        ],
                        ir.Return("product"),
                    )
                ],
            )
        ],
        "entry",
    )
    with pytest.raises(ValueError, match="immediate assignment"):
        ir.verify(module)


def test_frontend_reuses_only_local_augmented_assignment():
    module = lower("x = 1\nx *= 2\ndef f():\n x = 1\n x *= 2\n return x\n")
    global_operation = module.functions[0].blocks[0].instructions[4]
    local_operation = module.functions[1].blocks[0].instructions[4]
    assert global_operation.reuse_local is None
    assert local_operation.reuse_local == "x"
