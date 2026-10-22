"""Constant branches disappear without discarding runtime record processing."""

import re
from textwrap import dedent

import pytest

from mule.compiler import UnsupportedSyntax
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.main import compile_source
from mule.compiler.node import If
from test.mule.compiler.rewrite_support import build_case

CONDITIONS = [
    pytest.param("True", True, id="true"),
    pytest.param("False", False, id="false"),
    pytest.param("None", False, id="none"),
    pytest.param("0", False, id="integer-zero"),
    pytest.param("-7", True, id="negative-integer"),
    pytest.param("0.0", False, id="positive-float-zero"),
    pytest.param("-0.0", False, id="negative-float-zero"),
    pytest.param("1.25", True, id="float"),
    pytest.param("1e999", True, id="infinity"),
    pytest.param("''", False, id="empty-text"),
    pytest.param("'λ'", True, id="unicode-text"),
    pytest.param("b''", False, id="empty-bytes"),
    pytest.param("b'\\x00'", True, id="nul-byte"),
    pytest.param("3 * 7 == 21", True, id="folded-comparison"),
    pytest.param("(9 & 3) - 1", False, id="folded-arithmetic"),
    pytest.param("'a' > 'z'", False, id="folded-text-comparison"),
]


def _record_kernel(condition, truth, name="kernel"):
    selected = "return len(record)"
    rejected = "return reject_record(record)"
    consequent, alternative = (selected, rejected) if truth else (rejected, selected)
    return (
        f"def {name}(record):\n"
        f"    if {condition}:\n"
        f"        {consequent}\n"
        "    else:\n"
        f"        {alternative}\n"
    )


@pytest.mark.parametrize("condition,truth", CONDITIONS)
def test_constant_record_branch_normal_pipeline_removes_if(condition, truth):
    compilation = compile_source(_record_kernel(condition, truth))
    kernel = next(fn for fn in compilation.lambdas if fn.source_name == "kernel")
    assert not any(
        isinstance(node, If)
        for block in kernel.component.blocks()
        for node in block.nodes()
    )
    check_ir1_consistency(compilation.components)


@pytest.mark.integration
def test_constant_record_branches_optimized_and_native_oracle(tmp_path):
    # Every folded predicate controls an actual length calculation on stdin.
    # Combining cases avoids a separate runtime build per truth-domain boundary.
    kernels = []
    calls = []
    for index, parameter in enumerate(CONDITIONS):
        condition, truth = parameter.values
        name = f"record_kernel_{index}"
        kernels.append(_record_kernel(condition, truth, name))
        calls.append(f"print({name}(record))")
    source = (
        "def reject_record(record):\n"
        "    print('REJECTED', record)\n"
        "    raise ValueError(record)\n"
        + "\n".join(kernels)
        + "\nrecord = input()\n"
        + "\n".join(calls)
        + "\n"
    )
    case = build_case(tmp_path, source)
    for index in range(len(CONDITIONS)):
        body = case.function_ir(rf"^mule_fn_record_kernel_{index}\.body$")
        assert "@mule_truth" not in body
        assert "reject_record" not in body
        # Non-vacuous: selected code still computes the input record's length.
        assert re.search(r"\bload\b|@mule_(?:text_length|storage_length|len)", body)
        assert re.search(r"\bret\b", body)
    case.compare("\n")
    case.compare("λ:payload\n")


def test_constant_empty_arms_preserve_shared_continuation():
    compilation = compile_source(
        "def kernel(record):\n"
        "    if True:\n"
        "        pass\n"
        "    else:\n"
        "        pass\n"
        "    return len(record)\n"
    )
    kernel = next(fn for fn in compilation.lambdas if fn.source_name == "kernel")
    assert not any(
        isinstance(node, If)
        for block in kernel.component.blocks()
        for node in block.nodes()
    )
    check_ir1_consistency(compilation.components)


@pytest.mark.integration
def test_constant_nested_branches_preserve_effects_aliases_and_errors(tmp_path):
    case = build_case(
        tmp_path,
        dedent("""\
        events = []

        def note(value):
            events.append(value)
            return value

        def process(record):
            alias = record
            note('before')
            if 2 * 3 == 6:
                if False:
                    note('dead-inner')
                else:
                    record.append(note('live'))
            else:
                note('dead-outer')
            note('after')
            print(alias is record, alias)
            try:
                if None:
                    note('dead-error')
                else:
                    raise ValueError(note('error'))
            except ValueError as error:
                print(str(error))
            return record

        record = [input()]
        result = process(record)
        result.append('caller')
        print(result is record, record, events)
        """),
    )
    case.compare("runtime-record\n")
    case.compare("\n")


@pytest.mark.integration
def test_dynamic_truth_near_misses_preserve_conversion_effects_and_nan(tmp_path):
    # NaN has no Python literal spelling. Its runtime construction must retain
    # truth conversion, unlike the literal signed-zero cases above.
    case = build_case(
        tmp_path,
        dedent("""\
        events = []

        class Decision:
            def __init__(self, value):
                self.value = value
            def __bool__(self):
                events.append('bool')
                if self.value == 'error':
                    raise ValueError('truth')
                return self.value != ''

        def process(record, decision):
            if decision:
                return record
            return None

        def effectful_true():
            events.append('condition')
            return True

        record = input()
        for value in [None, '', 'x', b'', b'x', 0, -0.0, float('nan')]:
            print(process(record, value) is record)
        for value in ['', record, 'error']:
            try:
                print(process(record, Decision(value)) is record)
            except ValueError as error:
                print(str(error))
        if effectful_true():
            print(record)
        print(events)
        """),
    )
    case.compare("row\n")
    case.compare("\n")


@pytest.mark.parametrize("condition", ["False", "None", "0", "3 * 7 != 21"])
def test_constant_dead_branch_does_not_hide_unsupported_syntax(condition):
    source = (
        "def kernel(record):\n"
        f"    if {condition}:\n"
        "        class Rejected(*record):\n"
        "            pass\n"
        "    return record\n"
    )
    with pytest.raises(UnsupportedSyntax):
        compile_source(source, "dead_record_branch.py")
