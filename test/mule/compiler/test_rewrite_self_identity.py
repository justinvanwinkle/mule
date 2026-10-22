"""Same-local identity folding through normal source and object processing."""

import re
import textwrap

import pytest

from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case

_PROCESS_RECORDS = textwrap.dedent("""\
    def same_record(record):
        return record is record

    def different_record(record):
        return record is not record

    class Record:
        def __init__(self, value):
            self.value = value

        def __eq__(self, other):
            raise RuntimeError('identity must not call equality')

    def process_records(values):
        retained = []
        for value in values:
            record = Record(value)
            alias = record
            print(same_record(record), different_record(record))
            retained.append(alias)
            record.value = record.value + 1
            del record
            print(alias.value, same_record(alias))
            del alias
        for record in retained:
            print(record.value, different_record(record))
        return retained

    values = [int(part) for part in input().split()]
    records = process_records(values)
    records[0].value = records[0].value + 10
    print(records[0].value)
""")


@pytest.mark.parametrize(
    ("function", "operation"),
    [("same_record", "%compare_is"), ("different_record", "%compare_is_not")],
)
def test_same_local_identity_folds_in_record_processing_ir1(function, operation):
    compilation = compile_source(_PROCESS_RECORDS)
    target = next(fn for fn in compilation.lambdas if fn.source_name == function)
    assert list(known_calls(target, operation)) == []
    # The record itself must not be read merely to evaluate its known identity.
    record = next(var for var in target.vars if var.name == "record")
    assert record.refs == []
    check_ir1_consistency(compilation.components)


@pytest.mark.integration
def test_same_local_identity_removes_test_and_preserves_alias_lifetimes(tmp_path):
    case = build_case(tmp_path, _PROCESS_RECORDS)
    for function, singleton in [("same_record", "true"), ("different_record", "false")]:
        body = case.function_ir(rf"^mule_fn_{function}\.body$")
        expected = "@mule." + singleton
        returns = re.findall(r"\bret ptr ([^\s,]+)", body)
        assert returns, body
        for result in returns:
            if result == expected:
                continue
            # A safepoint failure returns null. The shared exit can merge that
            # exception result with the constant normal result through a phi.
            phi = re.search(
                rf"^\s*{re.escape(result)} = phi ptr (.+)$", body, re.MULTILINE
            )
            assert phi is not None, body
            incoming = re.findall(r"\[\s*([^\s,]+)\s*,", phi.group(1))
            assert set(incoming) == {expected, "null"}, body
        pointer_checks = re.findall(
            r"\bicmp\s+(?:eq|ne)\s+ptr\s+([^,\n]+),\s*([^\s,]+)", body
        )
        # Argument cleanup still checks null before releasing a heap object.
        # No comparison between object values may remain for the identity test.
        assert all("null" in operands for operands in pointer_checks), body
        assert "@mule_compare" not in body
        assert "@mule.fast.compare" not in body
        # Constant normal-return dataflow forbids delegating the identity work.
        assert not re.search(r"\bselect\s+i1\b", body)
    for stdin in ["4 -7 0\n", "123456789012345678901234567890 5\n"]:
        case.compare(stdin=stdin)


_IDENTITY_BOUNDARIES = textwrap.dedent("""\
    def conditional_record(flag, seed):
        if flag:
            record = [seed]
        return record is record

    def deleted_record(seed):
        record = [seed]
        del record
        return record is not record

    def changed_record(seed):
        record = [seed]
        original = record
        def replace():
            nonlocal record
            record = [seed + 1]
            print('replace', original[0], record[0])
            return record
        result = record is replace()
        print(original[0], record[0])
        return result

    class Source:
        def __init__(self, seed):
            self.seed = seed

        @property
        def record(self):
            self.seed = self.seed + 1
            print('read', self.seed)
            return [self.seed]

    def repeated_lookup(source):
        return source.record is source.record, source.record is not source.record

    def compare_aliases(left, right):
        return left is right, left is not right

    seed = int(input())
    for flag in (False, True):
        try:
            print('conditional', conditional_record(flag, seed))
        except UnboundLocalError:
            print('conditional unbound')
    try:
        print(deleted_record(seed))
    except UnboundLocalError:
        print('deleted unbound')
    print('changed', changed_record(seed))
    print('lookup', repeated_lookup(Source(seed)))
    original = [seed]
    alias = original
    print(compare_aliases(original, alias))
    alias = [seed]
    print(compare_aliases(original, alias))
    original.append(seed + 1)
    print(original, alias)
""")


@pytest.mark.parametrize(
    ("function", "operation"),
    [
        ("conditional_record", "%compare_is"),
        ("deleted_record", "%compare_is_not"),
        ("changed_record", "%compare_is"),
        ("repeated_lookup", "%compare_is"),
        ("repeated_lookup", "%compare_is_not"),
        ("compare_aliases", "%compare_is"),
        ("compare_aliases", "%compare_is_not"),
    ],
)
def test_identity_near_misses_keep_runtime_comparison_ir1(function, operation):
    compilation = compile_source(_IDENTITY_BOUNDARIES)
    target = next(fn for fn in compilation.lambdas if fn.source_name == function)
    assert list(known_calls(target, operation))
    check_ir1_consistency(compilation.components)


@pytest.mark.integration
def test_identity_near_misses_preserve_binding_lookup_and_mutation_effects(tmp_path):
    case = build_case(tmp_path, _IDENTITY_BOUNDARIES)
    # Distinct runtime arguments require an actual pointer comparison, not a
    # constant result, even when some callers happen to supply aliases.
    body = case.function_ir(r"^mule_fn_compare_aliases\.body$")
    assert re.search(r"\bicmp\s+(?:eq|ne)\s+ptr\b", body)
    for stdin in ["7\n", "-123456789012345678901234567890\n"]:
        case.compare(stdin=stdin)
