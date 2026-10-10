"""Every upstream suite is a real Mule regression test, without skips or xfails."""

import pytest

from tools.run_stdlib_tests import discover_entries, run_entry


@pytest.mark.parametrize("entry", discover_entries(), ids=lambda entry: entry.name)
def test_upstream_suite(entry, tmp_path):
    outcome = run_entry(entry, tmp_path)
    assert outcome.status == "passed", (
        f"{entry.name}: {outcome.phase}\n{outcome.stderr}\n{outcome.stdout}"
    )
