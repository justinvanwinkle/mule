"""Flow-dependent result derivation belongs to the current compiler worker."""

import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor
from threading import Event

import pytest

from mule.compiler.ctype import BOOLEAN
from mule.compiler.fndb import RANGE_PREPARE, flow_propagation
from mule.compiler.node import Combination, Lvar


def _derive_range_type():
    # This hook depends only on the current phase, not the operation's operands.
    assert RANGE_PREPARE.derive_type is not None
    operation = Combination(None, None, Lvar())
    return RANGE_PREPARE.derive_type(operation)


def _wait(event):
    assert event.wait(10), "compiler worker synchronization timed out"


def test_flow_scope_does_not_suppress_another_workers_result_derivation():
    active = Event()
    observed = Event()

    def propagating():
        with flow_propagation():
            assert _derive_range_type() is None
            active.set()
            _wait(observed)

    def ordinary():
        _wait(active)
        try:
            return _derive_range_type()
        finally:
            observed.set()

    with ThreadPoolExecutor(max_workers=2) as workers:
        first = workers.submit(propagating)
        second = workers.submit(ordinary)
        derived = second.result(timeout=15)
        first.result(timeout=15)

    assert derived is BOOLEAN
    assert _derive_range_type() is BOOLEAN


def _overlapping_derivations():
    first_active = Event()
    second_active = Event()
    first_exited = Event()
    second_exited = Event()

    def first():
        try:
            with flow_propagation():
                first_active.set()
                _wait(second_active)
        finally:
            first_exited.set()
        _wait(second_exited)

    def second():
        _wait(first_active)
        try:
            with flow_propagation():
                second_active.set()
                _wait(first_exited)
                return _derive_range_type()
        finally:
            second_exited.set()

    with ThreadPoolExecutor(max_workers=2) as workers:
        first_result = workers.submit(first)
        second_result = workers.submit(second)
        inside = second_result.result(timeout=15)
        first_result.result(timeout=15)
    return inside, _derive_range_type()


def test_overlapping_flow_scopes_preserve_each_worker_and_restore_normal_derivation():
    # The broken process-wide mode leaks after this ordering; isolate the probe
    # so its failure cannot change subsequent compiler tests.
    result = subprocess.run(
        [
            sys.executable,
            "-c",
            (
                "from test.mule.compiler.test_fndb_flow_context import "
                "_overlapping_derivations\n"
                "from mule.compiler.ctype import BOOLEAN\n"
                "inside, after = _overlapping_derivations()\n"
                "print('active worker:', inside, 'after both scopes:', after)\n"
                "assert inside is None and after is BOOLEAN\n"
            ),
        ],
        capture_output=True,
        text=True,
        check=False,
        timeout=30,
    )
    assert result.returncode == 0, result.stdout + result.stderr


def test_nested_flow_scopes_restore_the_outer_phase():
    assert _derive_range_type() is BOOLEAN
    with flow_propagation():
        assert _derive_range_type() is None
        with flow_propagation():
            assert _derive_range_type() is None
        assert _derive_range_type() is None
    assert _derive_range_type() is BOOLEAN


def test_exception_restores_the_previous_derivation_phase():
    with pytest.raises(ValueError, match="probe"), flow_propagation():
        assert _derive_range_type() is None
        raise ValueError("probe")
    assert _derive_range_type() is BOOLEAN


def test_worker_started_inside_flow_begins_with_normal_derivation():
    with flow_propagation():
        with ThreadPoolExecutor(max_workers=1) as workers:
            derived = workers.submit(_derive_range_type).result(timeout=15)
        assert derived is BOOLEAN
        assert _derive_range_type() is None
    assert _derive_range_type() is BOOLEAN
