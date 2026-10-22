import threading

import pytest

from perf.suite.perfutils.cpu import (
    CpuCounters,
    CpuMeasurementError,
    CpuMonitor,
    CpuSnapshot,
    LinuxCpuProbe,
    parse_cpu_stat,
    summarize_cpu,
)


def snapshot(seconds, user=0, idle=0, own=0, iowait=0, steal=0):
    return CpuSnapshot(
        seconds,
        CpuCounters((user, 0, 0, idle, iowait, 0, 0, steal, 0, 0)),
        own,
    )


def test_four_benchmark_cores_are_not_external_noise():
    # Child CPU appears in os.times only after reaping. An intermediate reading
    # must not use its incomplete child total to classify external activity.
    readings = [
        snapshot(10),
        snapshot(15, user=2000, idle=14000),
        snapshot(20, user=4000, idle=28000, own=40),
    ]
    result = summarize_cpu(readings, ticks_per_second=100, cpu_count=32)
    assert result["external_cpu_cores"] == 0
    assert result["benchmark_cpu_seconds"] == 40
    assert result["system_busy_cpu_cores"] == 4
    assert result["system_busy_percent"] == 12.5
    assert result["system_intervals"][0]["system_busy_cpu_cores"] == 4
    assert result["snapshots"][-1]["own_cpu_seconds"] == 40


def test_external_cpu_is_measured_in_cores_not_machine_percentage():
    readings = [snapshot(10), snapshot(20, user=5000, idle=27000, own=40)]
    result = summarize_cpu(readings, ticks_per_second=100, cpu_count=32)
    assert result["external_cpu_cores"] == 1
    assert result["system_busy_cpu_cores"] == 5
    assert result["system_busy_percent"] == 15.625


def test_linux_iowait_decrease_does_not_invalidate_busy_cpu_measurement():
    readings = [
        snapshot(10, iowait=100),
        snapshot(20, user=4000, idle=28000, own=40, iowait=90),
    ]
    result = summarize_cpu(readings, ticks_per_second=100, cpu_count=32)
    assert result["external_cpu_cores"] == 0
    assert result["iowait_cpu_seconds"] == pytest.approx(-0.1)
    assert result["system_busy_percent"] == 12.5


def test_guest_ticks_are_not_counted_twice():
    counters = parse_cpu_stat(
        "cpu 100 20 30 400 50 6 7 8 90 10\ncpu0 1 0 0 2 0 0 0 0 0 0\n"
    )
    assert counters.total == 621
    assert counters.busy == 171
    assert counters.evidence()["guest"] == 90


def test_iowait_is_separate_and_steal_is_busy_cpu_evidence():
    readings = [
        snapshot(10),
        snapshot(20, user=4000, idle=27000, own=40, iowait=600, steal=400),
    ]
    result = summarize_cpu(readings, ticks_per_second=100, cpu_count=32)
    assert result["iowait_cpu_seconds"] == 6
    assert result["steal_cpu_seconds"] == 4
    assert result["system_busy_cpu_seconds"] == 44
    assert result["external_cpu_cores"] == 0.4


@pytest.mark.parametrize(
    "text",
    [
        "",
        "intr 100",
        "cpu 1 2 3",
        "cpu x 0 0 0 0 0 0 0 0 0",
        "cpu -1 0 0 0 0 0 0 0 0 0",
        "cpu 1 0 0 0 0 0 0 0 2 0",
    ],
)
def test_malformed_cpu_readings_fail_with_source_context(text):
    with pytest.raises(CpuMeasurementError, match="fixture-stat"):
        parse_cpu_stat(text, source="fixture-stat")


def test_missing_cpu_file_does_not_become_zero_load(tmp_path):
    probe = LinuxCpuProbe(tmp_path / "absent-stat")
    with pytest.raises(CpuMeasurementError, match="absent-stat"):
        probe.read()


@pytest.mark.parametrize(
    ("readings", "message"),
    [
        ([snapshot(10, user=10), snapshot(20, user=9)], "user"),
        ([snapshot(10), snapshot(10, user=10)], "clock"),
        ([snapshot(10), snapshot(20)], "did not advance"),
        ([snapshot(10, own=1), snapshot(20, user=10)], "reaped-child"),
    ],
)
def test_inconsistent_cpu_evidence_fails_instead_of_approving_timings(
    readings, message
):
    with pytest.raises(CpuMeasurementError, match=message):
        summarize_cpu(readings, ticks_per_second=100, cpu_count=32)


def test_rounding_clamp_preserves_signed_cpu_evidence():
    readings = [snapshot(10), snapshot(20, user=4000, idle=28000, own=40.01)]
    result = summarize_cpu(readings, ticks_per_second=100, cpu_count=32)
    assert result["external_cpu_seconds_raw"] == pytest.approx(-0.01)
    assert result["external_cpu_cores"] == 0


def test_large_accounting_disagreement_cannot_approve_timings():
    readings = [snapshot(10), snapshot(20, user=4000, idle=28000, own=41)]
    with pytest.raises(CpuMeasurementError, match="exceeds system busy"):
        summarize_cpu(readings, ticks_per_second=100, cpu_count=32)


class EndpointProbe:
    ticks_per_second = 100
    cpu_count = 32

    def __init__(self):
        self.readings = iter(
            [snapshot(10), snapshot(20, user=4000, idle=28000, own=40)]
        )

    def read(self):
        return next(self.readings)


def test_monitor_produces_evidence_after_stopping_its_sampling_thread():
    with (
        CpuMonitor(EndpointProbe(), interval=3600) as monitor,
        pytest.raises(RuntimeError, match="context exit"),
    ):
        monitor.result()
    assert monitor.result()["external_cpu_cores"] == 0
    assert not monitor._thread.is_alive()


def test_monitor_stops_when_measurement_body_fails():
    with (
        pytest.raises(ValueError, match="benchmark failed"),
        CpuMonitor(EndpointProbe(), interval=3600) as monitor,
    ):
        raise ValueError("benchmark failed")
    assert not monitor._thread.is_alive()


class FailingPeriodicProbe:
    ticks_per_second = 100
    cpu_count = 32

    def __init__(self):
        self.failed = threading.Event()
        self.owner = threading.current_thread()
        self.readings = iter(
            [snapshot(10), snapshot(20, user=4000, idle=28000, own=40)]
        )

    def read(self):
        if threading.current_thread() is not self.owner:
            self.failed.set()
            raise CpuMeasurementError("periodic reading failed")
        return next(self.readings)


def test_sampling_failure_is_propagated_after_joining_thread():
    probe = FailingPeriodicProbe()
    with (
        pytest.raises(CpuMeasurementError, match="CPU monitoring failed") as error,
        CpuMonitor(probe, interval=0.001) as monitor,
    ):
        assert probe.failed.wait(2), "CPU sampler did not run"
    assert str(error.value.__cause__) == "periodic reading failed"
    assert not monitor._thread.is_alive()
    with pytest.raises(CpuMeasurementError):
        monitor.result()
