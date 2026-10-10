"""Attempt every upstream suite, tracking missing Mule support as strict xfails."""

import pytest

from tools.run_stdlib_tests import discover_entries, run_entry


@pytest.mark.parametrize(
    "entry",
    [
        pytest.param(
            entry,
            id=entry.name,
            marks=[]
            if entry.name == "test_int_literal"
            else pytest.mark.xfail(
                reason="Mule does not fully support this upstream stdlib suite yet",
                strict=True,
                raises=AssertionError,
            ),
        )
        for entry in discover_entries()
    ],
)
def test_upstream_suite(entry, tmp_path):
    outcome = run_entry(entry, tmp_path)
    assert outcome.status == "passed", (
        f"{entry.name}: {outcome.phase}\n{outcome.stderr}\n{outcome.stdout}"
    )
