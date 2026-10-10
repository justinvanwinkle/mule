"""Attempt every upstream suite, tracking missing Mule support as strict xfails."""

import pytest

from tools.run_stdlib_tests import discover_entries, run_entry


@pytest.mark.parametrize("entry", discover_entries(), ids=lambda entry: entry.name)
@pytest.mark.xfail(
    reason="Mule cannot compile the upstream stdlib suites yet",
    strict=True,
    raises=AssertionError,
)
def test_upstream_suite(entry, tmp_path):
    outcome = run_entry(entry, tmp_path)
    assert outcome.status == "passed", (
        f"{entry.name}: {outcome.phase}\n{outcome.stderr}\n{outcome.stdout}"
    )
