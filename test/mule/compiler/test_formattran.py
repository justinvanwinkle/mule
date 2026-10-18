"""Tests for ``%format`` IR1 transforms in ``mule/compiler/formattran.py``."""

import pytest

from mule import ast
from mule.compiler.ctype import (
    BOOLEAN,
    BYTES_TYPE,
    INTEGER,
    NONE_TYPE,
    TEXT_TYPE,
    UNKNOWN,
    TextType,
)
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1opt import ir1_optimize_until_done
from mule.compiler.ir1util import known_calls
from mule.compiler.knownfun import KNOWN
from mule.compiler.main import compile_source, compile_toplevel
from mule.compiler.node import Constant, LambdaVar, Ref, Return

_BEFORE_FORMATTRAN = {name: info.transforms for name, info in KNOWN.items()}
from mule.compiler import formattran  # noqa: F401

_WITH_FORMATTRAN = {name: info.transforms for name, info in KNOWN.items()}
for _name, _transforms in _BEFORE_FORMATTRAN.items():
    object.__setattr__(KNOWN[_name], "transforms", _transforms)


@pytest.fixture(autouse=True)
def _activate_formattran_transforms():
    saved = {name: info.transforms for name, info in KNOWN.items()}
    for name, transforms in _WITH_FORMATTRAN.items():
        object.__setattr__(KNOWN[name], "transforms", transforms)
    try:
        yield
    finally:
        for name, transforms in saved.items():
            object.__setattr__(KNOWN[name], "transforms", transforms)


def _compile_typed(source: str, param_types: dict[str, object]):
    compilation = compile_toplevel(source)
    fn = compilation.lambdas[1]
    for var in fn.vars:
        if var.name in param_types:
            var.type = param_types[var.name]
    changed = ir1_optimize_until_done(fn.component)
    check_ir1_consistency(compilation.components)
    return compilation, fn, changed


@pytest.mark.parametrize(
    ("expr", "expected"),
    [
        ("f'{42}'", "42"),
        ("f'{42!s}'", "42"),
        ("f'{42!r}'", "42"),
        ("f'{-17}'", "-17"),
        ("f'{True}'", "True"),
        ("f'{False}'", "False"),
        ("f'{True!s}'", "True"),
        ("f'{False!r}'", "False"),
        ("f'{None}'", "None"),
        ("f'{None!s}'", "None"),
        ("f'{None!r}'", "None"),
        ("f'{\"hello\"}'", "hello"),
        ("f'{\"hello\"!s}'", "hello"),
    ],
)
def test_format_constant_folds_literal_operands_with_empty_spec(expr, expected):
    compilation = compile_source(f"def f():\n return {expr}\n")
    fn = compilation.lambdas[1]
    assert list(known_calls(fn, "%format")) == []
    ret = fn.return_
    assert isinstance(ret, Return)
    use = ret.result.use()
    assert isinstance(use, Ref)
    assert isinstance(use.leaf, Constant)
    assert isinstance(use.leaf.value, ast.TextLiteral)
    assert use.leaf.value.value == expected
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize("expr", ["f'{s}'", "f'{s!s}'"])
@pytest.mark.parametrize("param_type", [TEXT_TYPE, TextType("abc")])
def test_format_identity_reduction_for_exact_text_type(expr, param_type):
    _, fn, changed = _compile_typed(
        f"def f(s: str):\n return {expr}\n", {"s": param_type}
    )
    assert changed
    assert list(known_calls(fn, "%format")) == []
    ret = fn.return_
    assert isinstance(ret, Return)
    use = ret.result.use()
    assert isinstance(use, Ref)
    assert isinstance(use.leaf, LambdaVar)
    assert use.leaf is fn.vars[0]
    assert use.leaf.name == "s"


def test_format_identity_reduction_via_flow_analysis():
    compilation = compile_source(
        "def f(c):\n s = 'yes' if c else 'no'\n return f'{s}', f'{s!s}'\n"
    )
    fn = compilation.lambdas[1]
    assert list(known_calls(fn, "%format")) == []
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize(
    ("sig", "expr", "param_types"),
    [
        ("s: str", "f'{s!r}'", {"s": TEXT_TYPE}),
        ("s: str", "f'{s!a}'", {"s": TEXT_TYPE}),
        ("s: str", "f'{s:>10}'", {"s": TEXT_TYPE}),
        ("s: str", "f'{s}'", {"s": TextType(may_be_unbound=True)}),
        ("x: int", "f'{x}'", {"x": INTEGER}),
        ("x: int", "f'{x!r}'", {"x": INTEGER}),
        ("x", "f'{x}'", {"x": UNKNOWN}),
        ("x", "f'{x}'", {"x": BOOLEAN}),
        ("x", "f'{x}'", {"x": NONE_TYPE}),
        ("x", "f'{x}'", {"x": BYTES_TYPE}),
        ("", "f'{\"hello\"!r}'", {}),
        ("", "f'{\"hello\"!a}'", {}),
        ("", "f'{42!a}'", {}),
        ("", "f'{True!a}'", {}),
        ("", "f'{None!a}'", {}),
        ("", f"f'{{{1 << 128}}}'", {}),
        ("", f"f'{{{1 << 128}!r}}'", {}),
    ],
)
def test_format_kept_when_not_trivial_or_not_constant_foldable(sig, expr, param_types):
    _, fn, _ = _compile_typed(f"def f({sig}):\n return {expr}\n", param_types)
    assert len(list(known_calls(fn, "%format"))) == 1
