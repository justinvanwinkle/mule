"""Mixed object/numeric storage preserves float values and observable aliases."""

import pytest

from mule.compiler import emit_llvm
from mule.compiler.codegen import compile_component
from mule.compiler.ctype import UNKNOWN
from mule.compiler.ltn import EntryContext
from mule.compiler.main import compile_source
from mule.compiler.vop import DESCRIPTOR, DOUBLE
from test.mule.compiler.test_exception_groups import compare
from test.mule.compiler.test_llvm_boxing import function_body


def test_numeric_copies_use_doubles_without_replacing_the_observable_object():
    compilation = compile_source(
        """
def calculate(n):
    original = 1.25
    saved = [original]
    value = original
    for index in range(n):
        value = value * 1.5
    print(saved[0] is original)
    return value > 2.0
"""
    )
    function = compilation.lambdas[1]
    assert compilation.facts is not None
    facts = compilation.facts[function]
    context = EntryContext(
        None,
        facts.types,
        facts.targets,
        facts.parameters or (UNKNOWN,) * len(function.vars),
        facts.return_type,
    )
    component = compile_component(function, context)
    locals_by_name = {
        variable.name: tn for variable, tn in component.environment.vars.items()
    }
    assert locals_by_name["original"].sc is DESCRIPTOR
    assert locals_by_name["value"].sc is DOUBLE

    operations = [vop for block in component.blocks for vop in block.vops]
    assert any(
        len(vop.args) == len(vop.results) == 1
        and vop.args[0].tn.sc is DESCRIPTOR
        and vop.results[0].tn.sc is DOUBLE
        for vop in operations
    )
    # Conversion must not replace local-read or overwrite ownership semantics.
    for vop in operations:
        if vop.info.name in ("move", "load-local"):
            assert vop.args[0].tn.sc.llvm_type == vop.results[0].tn.sc.llvm_type


def test_numeric_recurrence_extracts_the_escaped_origin_once_before_the_loop():
    llvm = emit_llvm(
        """
def calculate(n, label):
    original = 1.25
    saved = [original]
    value = original
    for index in range(n):
        value = value * 1.5 + 0.25
    print(label, saved[0] is original)
    return value > 2.0
"""
    )
    body = function_body(llvm, '"mule_fn_calculate.body"')
    assert "fmul double" in body
    assert "fadd double" in body
    assert "call ptr @mule.box.f64(" not in body
    extraction = "call double @mule.box.f64.load("
    assert body.count(extraction) == 1
    # The only object extraction initializes the recurrence, not an iteration.
    assert body.index(extraction) < body.index("fmul double")


def test_self_recursive_and_guarded_fixnum_numeric_entries():
    llvm = emit_llvm(
        """
def masked_step(x: int) -> int:
    total = 0
    for i in range(4):
        total = (total + x + i) & 0xFFFF
    return total

def outer(n: int) -> float:
    def rec_countdown(k, acc):
        if k <= 0:
            return acc & 0xFFFF
        return rec_countdown((k - 1) & 0xFFFF, (acc + masked_step(k)) & 0xFFFF) & 0xFFFF

    def rec_scale(k, x):
        if k <= 0:
            return x * 1.5
        return rec_scale((k - 1) & 0xFFFF, x) + 0.25

    c = rec_countdown(n & 0xFF, 0) & 0xFFFF
    return rec_scale(c, 2.5) + 1.0
"""
    )
    assert "define internal i64 @mule_fn_masked_step.numeric." in llvm
    assert (
        'define internal i64 @"mule_fn_outer.<locals>.rec_countdown.numeric.observed"('
        "ptr %globals, ptr %error.flag, ptr %closure, i64 %parameter.0, i64 %parameter.1)"
    ) in llvm
    assert (
        'define internal double @"mule_fn_outer.<locals>.rec_scale.numeric.general"('
        "ptr %globals, ptr %error.flag, ptr %closure, ptr %parameter.0, double %parameter.1)"
    ) in llvm
    rec_numeric = llvm.split(
        'define internal i64 @"mule_fn_outer.<locals>.rec_countdown.numeric.observed"'
    )[1].split("\n}")[0]
    assert 'call i64 @"mule.numeric.mule_fn_masked_step.' in rec_numeric
    assert (
        'call i64 @"mule_fn_outer.<locals>.rec_countdown.numeric.observed"('
        in rec_numeric
    )
    scale_numeric = llvm.split(
        'define internal double @"mule_fn_outer.<locals>.rec_scale.numeric.general"'
    )[1].split("\n}")[0]
    assert (
        'call double @"mule_fn_outer.<locals>.rec_scale.numeric.general"('
        in scale_numeric
    )


@pytest.mark.parametrize(
    "source",
    [
        """
def calculate(text, n):
    original = float(text)
    saved = [original]
    value = original
    for index in range(n):
        value = value * 1.5
    print(saved[0] is original, value > 2.0)
for text in ('1.25', '-0.0', 'inf', 'nan'):
    calculate(text, 0)
    calculate(text, 3)
""",
        """
def aliases(text):
    original = float(text)
    value = original * 1.5
    alias = value
    saved = [value, alias]
    print(saved[0] is saved[1], +value is alias, saved[0].hex())
for text in ('1.25', '-0.0', 'inf', 'nan'):
    aliases(text)
""",
        """
def choose(text, fresh):
    original = float(text)
    if fresh:
        value = original * 1.5
    else:
        value = original
    alias = value
    saved = [value, alias]
    numeric = value
    numeric = numeric * 2.0
    print(saved[0] is saved[1], value is original, numeric > 0.0)
choose('1.25', False)
choose('1.25', True)
choose('-0.0', False)
choose('-0.0', True)
""",
        """
def retained(n):
    value = 0.5
    saved = []
    for index in range(n):
        alias = value
        saved.append((value, alias))
        value += 1.5
    for pair in saved:
        print(pair[0] is pair[1], pair[0], pair[0] is value)
retained(0)
retained(3)
""",
        """
def failing(n):
    original = -0.0
    saved = [original]
    value = original
    for index in range(n):
        value += 1.5
    alias = value
    try:
        value /= -0.0
    except ZeroDivisionError:
        print(value is alias, value.hex(), saved[0] is original)
    print(saved[0].hex())
failing(0)
failing(3)
""",
        """
def mixed(flag):
    original = 1.5
    value = original
    if flag:
        value = 'other'
    print(value, value is original)
mixed(False)
mixed(True)
""",
        """
class Special(float):
    pass
def positive(text):
    original = Special(text)
    saved = [original]
    value = +original
    alias = value
    numeric = value * 1.5
    print(saved[0] is original, value is alias, value is original, numeric.hex())
positive('1.25')
positive('-0.0')
""",
        """
def masked_step(x: int) -> int:
    total = 0
    for i in range(4):
        total = (total + x + i) & 0xFFFF
    return total

def outer(n: int) -> float:
    def rec_countdown(k, acc):
        if k <= 0:
            return acc & 0xFFFF
        return rec_countdown((k - 1) & 0xFFFF, (acc + masked_step(k)) & 0xFFFF) & 0xFFFF

    def rec_scale(k, x):
        if k <= 0:
            return x * 1.5
        return rec_scale((k - 1) & 0xFFFF, x) + 0.25

    c = rec_countdown(n & 0xFF, 0) & 0xFFFF
    return rec_scale(c, 2.5) + 1.0

def check_str_eq(a: str, b: str):
    return (
        a == '',
        a != '',
        a == 'x',
        a != 'x',
        a == 'alpha_beta',
        a == b,
        a != b,
    )

print(outer(0), outer(3))
for s in ('', 'x', 'y', 'alpha_beta', 'alpha_beta_gamma_delta_epsilon', 'λ'):
    print(check_str_eq(s, 'alpha_beta'), check_str_eq(s, s))
""",
    ],
    ids=[
        "escaped-origin-numeric-copy",
        "fresh-result-aliases",
        "branch-origin-identity",
        "loop-retained-aliases",
        "failed-assignment-identity",
        "mixed-type-destination",
        "float-subclass-positive",
        "self-recursive-numeric-and-inline-str-eq",
    ],
)
def test_mixed_representations_preserve_python_float_semantics(tmp_path, source):
    compare(tmp_path, source)
