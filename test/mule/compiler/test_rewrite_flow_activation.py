"""Flow feedback must reach real bodies without contaminating general entries."""

import re

import pytest

from mule.compiler.codegen import compile_component
from mule.compiler.ctype import INTEGER, TEXT_TYPE, UNKNOWN
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import known_calls
from mule.compiler.ltn import EntryContext
from mule.compiler.main import compile_source
from mule.compiler.node import ValuedNode
from test.mule.compiler.rewrite_support import build_case

FLOW_SOURCE = """\
import mule_types

def report(n: mule_types.int64, raw):
    def step(value: mule_types.int64) -> mule_types.int64:
        return value

    def render(value, stride, label):
        return (value + 0) * stride, str(label)

    label = 'enabled' if raw else 'off'
    zero = 0
    number = step(n + zero) + zero
    count = len(label)
    rendered = render(number & 1023, 8, label)
    return rendered, str(label), str(count), number

number = int(input())
flag = input()
print(report(number, flag))
"""


ENTRY_SOURCE = """\
import mule_types

def format_sample(value, label):
    return (value + 0) * 8, str(label)

def report(n: mule_types.int64, raw):
    label = 'enabled' if raw else 'off'
    result = format_sample(n & 1023, label)
    print(result, result[1] is label)

events = []

class Number:
    def __add__(self, other):
        events.append('add')
        return self

    def __mul__(self, other):
        events.append('multiply')
        return 91

class Label:
    def __str__(self):
        events.append('str')
        return 'custom'

class Broken:
    def __add__(self, other):
        events.append('error')
        raise ValueError('broken')

report(int(input()), input())
# A container lookup obscures the target instead of adding nonnumeric facts
# to the direct call whose observed entry is under examination.
escaped = [format_sample]
print(escaped[0](Number(), Label()))
try:
    escaped[0](Broken(), Label())
except ValueError:
    print('caught')
print(events)
print(escaped[0](2 ** 90, 'large'))
"""


def _lambda(compilation, name):
    return next(fn for fn in compilation.lambdas if fn.source_name == name)


def _assert_reduced_render(fn):
    assert not list(known_calls(fn, "%add"))
    assert not list(known_calls(fn, "%multiply"))
    assert list(known_calls(fn, "%left_shift"))


def _assert_native_shift_without_original_dispatch(body):
    # LLVM can combine the arithmetic shift and result tagging, and attach
    # wrap flags. Those choices do not change the native-work requirement.
    assert re.search(r"\bshl(?:\s+(?:nuw|nsw))*\s+i64\b", body)
    assert not re.search(r'@"?mule\.fast\.binary\.(?:0|2)["(]', body)
    assert not re.search(r"@mule_binary(?:_reuse)?\(i32 (?:0|2),", body)
    # str is the sole unary dynamic call in the selected source path. Tuple
    # construction, printing and the guarded two-argument mismatch can remain.
    assert not re.search(r'@"?mule\.call\.1["(]', body)
    assert not re.search(r"@mule_call_owned\([^\n]*, i64 1,", body)


def test_flow_feedback_preserves_local_call_results_and_promotes_closed_helper():
    compilation = compile_source(FLOW_SOURCE)
    report = _lambda(compilation, "report")
    render = _lambda(compilation, "render")
    _assert_reduced_render(render)
    # The second addition depends on the proven local step result surviving
    # reoptimization of its argument. A fresh UNKNOWN would block this rewrite.
    assert not list(known_calls(report, "%add"))
    assert list(known_calls(report, "_decimal_protocol"))
    assert compilation.facts is not None
    facts = compilation.facts[render]
    assert facts.parameters[0].fits(INTEGER)
    assert facts.parameters[2] == TEXT_TYPE
    assert compilation.facts[_lambda(compilation, "step")].return_type.fits(INTEGER)
    check_ir1_consistency(compilation.components)


@pytest.mark.integration
def test_flow_feedback_emits_native_work_for_runtime_numeric_text_report(tmp_path):
    case = build_case(tmp_path, FLOW_SOURCE)
    for stdin in ("17\nyes\n", "-17\n\n", "9223372036854775807\nyes\n"):
        case.compare(stdin=stdin)
    caller = case.function_ir(r"^mule_fn_report\.body$")
    assert re.search(r'call[^\n]*@"?mule\.direct\.[^\n(]*render\.', caller)
    # The bounded observed render entry is inlined into its actual direct-call
    # helper. The broader fixnum entry and rejected-guard fallback are distinct.
    body = case.function_ir(r"^mule\.direct\..*render\.")
    _assert_native_shift_without_original_dispatch(body)


@pytest.mark.integration
def test_observed_entry_reduces_runtime_values_and_keeps_general_protocols(tmp_path):
    case = build_case(tmp_path, ENTRY_SOURCE)
    for stdin in ("37\nyes\n", "-37\n\n"):
        case.compare(stdin=stdin)
    # LLVM inlines the observed entry into report's guarded direct arm. Inspect
    # that runtime-input computation, not the remaining full-fixnum variant,
    # whose unknown label correctly retains str and whose shift can overflow.
    specialized = case.function_ir(r"^mule_fn_report\.body$")
    assert "@mule_function_matches" in specialized
    _assert_native_shift_without_original_dispatch(specialized)
    # The open entry must still perform user operations. Runtime observations
    # above establish their order, the exception short circuit and large ints.
    general = case.function_ir(r"format_sample\.body(?:\.fallback\.fixnum)?$")
    assert "mule_binary" in general or "mule.fast.binary" in general


class EmissionFailure(Exception):
    pass


def _reject_call_selection(component):
    # Use the backend's normal callback boundary to fail after specialization,
    # without replacing an analysis or modifying compiler registries.
    assert component.blocks
    _assert_reduced_render(component.lambda_)
    raise EmissionFailure("stop after specialized IR2 conversion")


def _accept_call_selection(component):
    _assert_reduced_render(component.lambda_)
    return False


@pytest.mark.parametrize("fail", [False, True])
def test_entry_emission_restores_canonical_graph_and_facts_after_success_or_error(fail):
    compilation = compile_source(ENTRY_SOURCE)
    fn = _lambda(compilation, "format_sample")
    assert compilation.facts is not None
    facts = compilation.facts[fn]
    variant = next(context for context in facts.variants if context.id == "observed")
    context = EntryContext(
        variant.id,
        variant.types,
        variant.targets,
        variant.parameters,
        variant.return_type,
    )
    canonical = fn.component
    blocks = tuple(canonical.blocks())
    nodes = tuple(tuple(block.nodes()) for block in blocks)
    derived_types = {
        node: node.derived_type
        for block_nodes in nodes
        for node in block_nodes
        if isinstance(node, ValuedNode)
    }
    published_types = dict(facts.types)
    published_targets = dict(facts.targets)
    specialized_types = context.types
    specialized_targets = context.targets
    parameters = facts.parameters
    assert parameters == (UNKNOWN, UNKNOWN)
    assert list(known_calls(fn, "%add"))
    assert list(known_calls(fn, "%multiply"))

    # Repeated emissions test that restoration also clears activation state.
    for _ in range(2):
        if fail:
            with pytest.raises(EmissionFailure, match="specialized IR2"):
                compile_component(fn, context, select_calls=_reject_call_selection)
        else:
            emitted = compile_component(
                fn, context, select_calls=_accept_call_selection
            )
            assert emitted.blocks
        assert fn.component is canonical
        assert tuple(canonical.blocks()) == blocks
        assert tuple(tuple(block.nodes()) for block in blocks) == nodes
        assert all(node.derived_type == fact for node, fact in derived_types.items())
        assert context.types is specialized_types
        assert context.targets is specialized_targets
        assert facts.types == published_types
        assert facts.targets == published_targets
        assert facts.parameters == parameters
        assert list(known_calls(fn, "%add"))
        assert list(known_calls(fn, "%multiply"))
        check_ir1_consistency(compilation.components)

    # A later unrestricted emission must not inherit the specialized rewrite.
    general = EntryContext(
        None, facts.types, facts.targets, parameters, facts.return_type
    )
    assert compile_component(fn, general).blocks
    assert list(known_calls(fn, "%add"))
    assert list(known_calls(fn, "%multiply"))
    check_ir1_consistency(compilation.components)
