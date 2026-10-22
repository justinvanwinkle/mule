"""Singleton identity rewrites through inferred record-processing values."""

import re

import pytest

from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from mule.compiler.node import If
from test.mule.compiler.rewrite_support import build_case


def _function(compilation, name):
    return next(fn for fn in compilation.lambdas if fn.source_name == name)


@pytest.mark.parametrize("expression", ["str(raw)", "len(raw)", "[raw]", "(raw,)"])
def test_bound_record_values_remove_impossible_none_tests(expression):
    source = f"""
def record_size(raw: str):
    value = {expression}
    if value is None:
        return -1
    if None is not value:
        return len(raw) + 7
    return -2
"""
    compilation = compile_source(source)
    fn = _function(compilation, "record_size")
    assert not list(known_calls(fn, "%compare_is"))
    assert not list(known_calls(fn, "%compare_is_not"))
    assert fn.component is not None
    assert not any(
        isinstance(node, If)
        for block in fn.component.blocks()
        for node in block.nodes()
    )
    check_ir1_consistency(compilation.components)


SINGLETON_SOURCE = """
def record_size(raw: str):
    raw = str(raw)
    missing = None
    absent = None
    accepted = True
    rejected = False
    size = len(raw)
    if missing is not absent:
        return -1
    if accepted is rejected:
        return -2
    if accepted is not True:
        return -3
    if rejected is not False:
        return -4
    if size is None:
        return -5
    return size + 7

print(record_size(input()))
"""


@pytest.mark.integration
def test_singleton_record_predicates_remove_tests_but_keep_runtime_work(tmp_path):
    compilation = compile_source(SINGLETON_SOURCE)
    fn = _function(compilation, "record_size")
    assert not list(known_calls(fn, "%compare_is"))
    assert not list(known_calls(fn, "%compare_is_not"))
    assert fn.component is not None
    assert not any(
        isinstance(node, If)
        for block in fn.component.blocks()
        for node in block.nodes()
    )
    check_ir1_consistency(compilation.components)

    case = build_case(tmp_path, SINGLETON_SOURCE)
    body = case.function_ir(r"^mule_fn_record_size\.body$")
    # Error and overflow edges remain valid. The language-level predicate tests
    # must disappear, while the result must still depend on the runtime record.
    assert "mule_truth" not in body
    assert "mule_compare" not in body
    assert re.search(r"\badd\b[^\n]*\bi64\b", body)
    assert "mule_text_length" in body or re.search(r"\bload i64\b", body)
    for raw in ("", "message", "naïve λ record"):
        result = case.compare(raw + "\n")
        assert result.stdout == f"{len(raw) + 7}\n"


def test_nullable_and_runtime_boolean_predicates_retain_identity_tests():
    compilation = compile_source("""
def optional_size(raw: str, present):
    value = raw if present else None
    return value is None, value is not None

def predicate(raw):
    flag = bool(raw)
    return flag is True, flag is False
""")
    optional = _function(compilation, "optional_size")
    predicate = _function(compilation, "predicate")
    assert list(known_calls(optional, "%compare_is"))
    assert list(known_calls(optional, "%compare_is_not"))
    assert list(known_calls(predicate, "%compare_is"))
    check_ir1_consistency(compilation.components)


BOUNDARY_SOURCE = """
class Record:
    def __init__(self, value):
        self.value = value

    @property
    def field(self):
        print('read field')
        if self.value == 'raise':
            raise ValueError('field')
        return self.value

class Verdict:
    def __eq__(self, other):
        print('unexpected equality')
        return True

def optional_size(raw, present):
    value = raw if present else None
    if value is None:
        return -1
    return len(value)

def unbound(present):
    if present:
        value = None
    try:
        print('bound', value is None)
    except UnboundLocalError:
        print('unbound')

def produce(raw):
    print('produce', raw)
    if raw == 'raise':
        raise ValueError('producer')
    return len(raw)

raw = input()
print(optional_size(raw, False), optional_size(raw, True))
for flag in (False, True):
    print('predicate', flag is True, flag is False)
    unbound(flag)
try:
    print('produced none', produce(raw) is None)
except ValueError:
    print('producer error')
record = Record(None)
print('descriptor none', record.field is None)
record.value = raw
try:
    print('descriptor none', record.field is None)
except ValueError:
    print('descriptor error')
verdict = Verdict()
alias = verdict
print('identity', verdict is alias, verdict is Verdict(), verdict is None)
zero = 0
one = 1
print('numeric boundary', zero is False, one is True, zero == False, one == True)
values = [raw]
alias_values = values
print('list identity', values is alias_values, values is [raw])
"""


@pytest.mark.integration
def test_identity_preserves_nullable_unbound_descriptor_and_effect_boundaries(tmp_path):
    case = build_case(tmp_path, BOUNDARY_SOURCE)
    for raw in ("", "payload", "raise"):
        result = case.compare(raw + "\n")
        assert "unexpected equality" not in result.stdout
        assert "identity True False False\n" in result.stdout
        assert "numeric boundary False False True True\n" in result.stdout
        assert "list identity True False\n" in result.stdout
