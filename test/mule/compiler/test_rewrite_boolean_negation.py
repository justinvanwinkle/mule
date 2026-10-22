"""Normal-pipeline Boolean rewrites and observable predicate evaluation."""

import re

import pytest

from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case


@pytest.mark.parametrize(
    ("expression", "expected", "removed"),
    [
        ("not (not bool(item))", None, None),
        ("not (item is sentinel)", "%compare_is_not", "%compare_is"),
        ("not (item is not sentinel)", "%compare_is", "%compare_is_not"),
        ("not (item in sentinel)", "%compare_not_in", "%compare_in"),
        ("not (item not in sentinel)", "%compare_in", "%compare_not_in"),
    ],
)
def test_filter_predicate_normal_facts_remove_negation(expression, expected, removed):
    compilation = compile_source(
        "def filter_items(items, sentinel):\n"
        " selected = []\n"
        " for item in items:\n"
        f"  verdict = {expression}\n"
        "  if verdict:\n"
        "   selected.append(item)\n"
        " return selected\n"
    )
    function = next(
        fn for fn in compilation.lambdas if fn.source_name == "filter_items"
    )
    assert not list(known_calls(function, "%unary_not"))
    if expected is not None:
        assert list(known_calls(function, expected))
        assert not list(known_calls(function, removed))
    check_ir1_consistency(compilation.components)


def test_dynamic_operand_double_not_retains_boolean_conversion():
    compilation = compile_source(
        "def normalize(item):\n return not (not item)\n"
        "def shadowed(item, bool):\n return not (not bool(item))\n"
    )
    for name in ("normalize", "shadowed"):
        function = next(fn for fn in compilation.lambdas if fn.source_name == name)
        assert list(known_calls(function, "%unary_not"))
    check_ir1_consistency(compilation.components)


PROGRAM = """
events = []

class Verdict:
    def __init__(self, value):
        self.value = value

    def __bool__(self):
        events.append(self.value)
        if self.value == 7:
            raise ValueError("truth")
        if self.value == 8:
            return 1
        return self.value > 0

class Bag:
    def __contains__(self, item):
        events.append("contains")
        if item == 9:
            raise RuntimeError("contains")
        return Verdict(item)

def normalize(item):
    return not (not bool(item))

def dynamic_normalize(item):
    return not (not item)

def different(item, sentinel):
    return not (item is sentinel)

def same(item, sentinel):
    return not (item is not sentinel)

def absent(item, container):
    return not (item in container)

def present(item, container):
    return not (item not in container)

def shadowed(item, bool):
    return not (not bool(item))

def passthrough(item):
    return item

def ordered_item(value):
    events.append("item")
    return value

def ordered_bag():
    events.append("bag")
    return Bag()

def filter_records(items, sentinel):
    selected = []
    for item in items:
        if not (item is sentinel):
            selected.append(item)
    return selected

n = int(input())
sentinel = [n]
other = [n]
selected = filter_records([sentinel, other, sentinel, other], sentinel)
print(len(selected), selected[0] is other, selected[1] is other)
print(different(sentinel, sentinel), different(other, sentinel))
print(same(sentinel, sentinel), same(other, sentinel))
for value in [n, 0, -1, 7, 8, 9]:
    for operation in [normalize, dynamic_normalize]:
        try:
            answer = operation(Verdict(value))
            print(answer, answer is True, answer is False)
        except ValueError:
            print("truth-error")
        except TypeError:
            print("truth-type-error")
    for operation in [absent, present]:
        try:
            answer = operation(ordered_item(value), ordered_bag())
            print(answer, answer is True, answer is False)
        except ValueError:
            print("contains-truth-error")
        except TypeError:
            print("contains-truth-type-error")
        except RuntimeError:
            print("contains-error")
    print(events)
    events.clear()
print(shadowed(Verdict(n), passthrough))
print(events)
"""


@pytest.mark.integration
def test_boolean_filter_rewrites_preserve_effects_errors_and_identity(tmp_path):
    case = build_case(tmp_path, PROGRAM)
    for value in (2, -3):
        case.compare(stdin=f"{value}\n")

    # Identity inversion must reach pointer comparison, not a truth protocol.
    # These roots have unknown operands and cannot fold to a constant result.
    for name, equal_result in (("different", "false"), ("same", "true")):
        body = case.function_ir(rf"^mule_fn_{name}\.body$")
        operands = re.findall(r"ptr\s+(%[-\w.]+)", body.splitlines()[0])[-2:]
        comparisons = re.finditer(
            r"(%[-\w.]+) = icmp (eq|ne) ptr (%[-\w.]+), (%[-\w.]+)", body
        )
        comparisons = [
            match for match in comparisons if set(match.group(3, 4)) == set(operands)
        ]
        assert comparisons, body
        # LLVM may use eq with exchanged select arms instead of ne. Follow
        # the operand comparison directly to its singleton result either way.
        for comparison in comparisons:
            condition, predicate = comparison.group(1, 2)
            first = (
                equal_result
                if predicate == "eq"
                else ("true" if equal_result == "false" else "false")
            )
            second = "true" if first == "false" else "false"
            selected = re.search(
                rf"(%[-\w.]+) = select i1 {re.escape(condition)}, "
                rf"ptr @mule\.{first}, ptr @mule\.{second}",
                body,
            )
            assert selected, body
            assert re.search(
                rf"(?:ret ptr |phi ptr .*\[ ){re.escape(selected.group(1))}(?:,|\s)",
                body,
            ), body
        assert "@mule_truth(" not in body
        assert "@mule_compare(" not in body
        assert "@mule_unary(" not in body

    # Check the real compiled membership algorithm as well as the caller.
    # Membership still requires user __contains__ and bool dispatch, but an
    # already-Boolean result must not re-enter the generic unary operation.
    for name in ("absent", "present"):
        body = case.function_ir(rf"^mule_fn_{name}\.body$")
        assert "@mule_unary(" not in body
        assert "@mule_truth(" not in body
    contains = case.function_ir(r"contains.*\.body$", unit="builtins")
    assert "mule_storage_read" in contains, contains
    assert "mule_lookup_special" in contains or "optional_special" in contains
    assert "@mule_unary(" not in contains
