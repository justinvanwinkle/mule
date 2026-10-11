"""Compare native Mule executables with CPython across the conformance programs."""

import pytest

from test.conformance.cases import discover_programs, load_gaps

PROGRAMS = discover_programs()
GAPS = load_gaps()


def expected_gap(gap, request):
    if request.config.getoption("--conformance-strict"):
        pytest.fail(gap.reason)
    pytest.xfail(gap.reason)


@pytest.mark.parametrize("program", PROGRAMS, ids=lambda program: program.id)
def test_reference_output_is_deterministic(program, tmp_path):
    first = program.run_reference(tmp_path, hash_seed="0")
    second = program.run_reference(tmp_path, hash_seed="1")
    assert first == second


@pytest.mark.parametrize("program", PROGRAMS, ids=lambda program: program.id)
def test_native_output_matches_cpython(program, tmp_path, request):
    reference = program.run_reference(tmp_path)
    gap = GAPS.get(program.id)
    executable = tmp_path / "program"
    failure = program.compile(executable)
    if failure is not None:
        if gap is not None and gap.matches_compile(failure, program.path):
            expected_gap(gap, request)
        pytest.fail(f"unrecorded compile failure: {failure}")
    native = program.run([str(executable)], tmp_path)
    if native.matches(reference):
        return
    if gap is not None and gap.matches_run(native):
        expected_gap(gap, request)
    assert native == reference
