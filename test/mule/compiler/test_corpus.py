"""Run each independent output contract against CPython and a native Mule binary."""

import pytest

from mule.compiler import build
from test.corpus.cases import discover_cases

CASES = discover_cases()


def expected_gap(case, request):
    if request.config.getoption("--corpus-strict"):
        pytest.fail(case.reason)
    pytest.xfail(case.reason)


@pytest.mark.parametrize("case", CASES, ids=lambda case: case.id)
def test_reference_output(case, tmp_path):
    assert case.run_reference(tmp_path) == case.expected


@pytest.mark.parametrize("case", CASES, ids=lambda case: case.id)
def test_native_output(case, tmp_path, request):
    executable = tmp_path / "program"
    try:
        build(case.path, executable, optimize=False)
    except SyntaxError as error:
        if case.matches_compile_gap(error):
            expected_gap(case, request)
        raise
    assert executable.read_bytes()[:4] == b"\x7fELF"
    result = case.run([str(executable)], tmp_path)
    if case.status == "pending" and case.phase == "run" and result != case.expected:
        assert result == case.failure
        expected_gap(case, request)
    assert result == case.expected
