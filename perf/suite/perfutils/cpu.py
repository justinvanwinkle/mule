"""Linux CPU accounting for complete, quiescent benchmark measurement phases.

Enter before launching measurement workers and exit only after their subprocesses
are reaped. ``os.times`` child totals are process-wide, so one monitor must cover
the whole phase, not one monitor per concurrent worker. Periodic readings describe
system utilization. Only the phase endpoints support subtracting benchmark CPU.
"""

import math
import os
import threading
import time
from dataclasses import dataclass
from itertools import pairwise
from pathlib import Path

_FIELDS = (
    "user",
    "nice",
    "system",
    "idle",
    "iowait",
    "irq",
    "softirq",
    "steal",
    "guest",
    "guest_nice",
)


class CpuMeasurementError(RuntimeError):
    """CPU evidence is unavailable or inconsistent and cannot approve timings."""


@dataclass(frozen=True)
class CpuCounters:
    ticks: tuple[int, ...]

    @property
    def total(self):
        # guest and guest_nice are already included in user and nice.
        return sum(self.ticks[:8])

    @property
    def busy(self):
        return self.total - self.ticks[3] - self.ticks[4]

    def evidence(self):
        return dict(zip(_FIELDS, self.ticks, strict=True))


def parse_cpu_stat(text, source="/proc/stat"):
    """Read the aggregate Linux CPU line without counting guest ticks twice."""
    lines = text.splitlines()
    fields = lines[0].split() if lines else []
    if len(fields) != 11 or fields[0] != "cpu":
        raise CpuMeasurementError(f"{source}: expected aggregate cpu and ten counters")
    try:
        ticks = tuple(int(value) for value in fields[1:])
    except ValueError as error:
        raise CpuMeasurementError(f"{source}: noninteger CPU counter") from error
    if any(value < 0 for value in ticks):
        raise CpuMeasurementError(f"{source}: negative CPU counter")
    if ticks[8] > ticks[0] or ticks[9] > ticks[1]:
        raise CpuMeasurementError(f"{source}: guest ticks exceed user or nice ticks")
    return CpuCounters(ticks)


@dataclass(frozen=True)
class CpuSnapshot:
    monotonic_seconds: float
    counters: CpuCounters
    own_cpu_seconds: float

    def evidence(self):
        return {
            "monotonic_seconds": self.monotonic_seconds,
            "system_ticks": self.counters.evidence(),
            "own_cpu_seconds": self.own_cpu_seconds,
        }


class LinuxCpuProbe:
    def __init__(self, path=Path("/proc/stat")):
        self.path = Path(path)
        self.ticks_per_second = os.sysconf("SC_CLK_TCK")
        self.cpu_count = os.cpu_count()
        if self.ticks_per_second <= 0 or self.cpu_count is None or self.cpu_count < 1:
            raise CpuMeasurementError("Cannot determine Linux CPU accounting units")

    def read(self):
        try:
            counters = parse_cpu_stat(self.path.read_text(), str(self.path))
        except OSError as error:
            raise CpuMeasurementError(
                f"Cannot read CPU counters from {self.path}"
            ) from error
        process = os.times()
        own = (
            process.user
            + process.system
            + process.children_user
            + process.children_system
        )
        return CpuSnapshot(time.monotonic(), counters, own)


def _interval(start, end):
    elapsed = end.monotonic_seconds - start.monotonic_seconds
    if not math.isfinite(elapsed) or elapsed <= 0:
        raise CpuMeasurementError("CPU sampling monotonic clock did not advance")
    for field, before, after in zip(
        _FIELDS, start.counters.ticks, end.counters.ticks, strict=True
    ):
        # Linux documents that iowait can decrease. It is not busy CPU time.
        if field != "iowait" and after < before:
            raise CpuMeasurementError(
                f"Linux CPU counter decreased: {field} ({before} -> {after})"
            )
    own = end.own_cpu_seconds - start.own_cpu_seconds
    if not math.isfinite(own) or own < 0:
        raise CpuMeasurementError(
            "Process and reaped-child CPU total decreased or is nonfinite"
        )
    return elapsed, own


def summarize_cpu(snapshots, ticks_per_second, cpu_count):
    """Subtract process-tree CPU only after every measurement child is reaped."""
    if len(snapshots) < 2:
        raise CpuMeasurementError("CPU measurement requires start and end snapshots")
    if ticks_per_second <= 0 or cpu_count < 1:
        raise CpuMeasurementError("Invalid Linux CPU accounting units")
    intervals = []
    for before, after in pairwise(snapshots):
        elapsed, _ = _interval(before, after)
        busy = (after.counters.busy - before.counters.busy) / ticks_per_second
        total = _elapsed_cpu_ticks(before, after) / ticks_per_second
        intervals.append(
            {
                "elapsed_seconds": elapsed,
                "system_busy_cpu_cores": busy / elapsed,
                "system_busy_percent": busy / total * 100 if total else 0.0,
            }
        )
    start, end = snapshots[0], snapshots[-1]
    elapsed, own = _interval(start, end)
    total = _elapsed_cpu_ticks(start, end) / ticks_per_second
    busy = (end.counters.busy - start.counters.busy) / ticks_per_second
    if total <= 0:
        raise CpuMeasurementError("Linux aggregate CPU counters did not advance")
    external = busy - own
    # Each endpoint reads independently rounded per-CPU tick counters. Retain
    # the signed difference so clamping never conceals the original evidence.
    rounding = 2 * cpu_count / ticks_per_second
    if external < -rounding:
        raise CpuMeasurementError(
            f"Benchmark CPU exceeds system busy CPU by {-external:.6g}s "
            f"(tick rounding allowance {rounding:.6g}s)"
        )
    return {
        "elapsed_seconds": elapsed,
        "system_busy_cpu_seconds": busy,
        "system_total_cpu_seconds": total,
        "benchmark_cpu_seconds": own,
        "external_cpu_seconds_raw": external,
        "external_cpu_cores": max(0.0, external) / elapsed,
        "system_busy_cpu_cores": busy / elapsed,
        "system_busy_percent": busy / total * 100,
        "iowait_cpu_seconds": (end.counters.ticks[4] - start.counters.ticks[4])
        / ticks_per_second,
        "steal_cpu_seconds": (end.counters.ticks[7] - start.counters.ticks[7])
        / ticks_per_second,
        "ticks_per_second": ticks_per_second,
        "cpu_count": cpu_count,
        "rounding_allowance_seconds": rounding,
        "snapshots": [snapshot.evidence() for snapshot in snapshots],
        "system_intervals": intervals,
    }


def _elapsed_cpu_ticks(start, end):
    busy = end.counters.busy - start.counters.busy
    idle = end.counters.ticks[3] - start.counters.ticks[3]
    iowait = end.counters.ticks[4] - start.counters.ticks[4]
    # A kernel iowait correction must not reduce the utilization denominator.
    # The report retains the signed iowait delta separately.
    return busy + idle + max(0, iowait)


class CpuMonitor:
    """Own a sampling thread for one whole measurement phase.

    An injected probe implements ``read()``, ``ticks_per_second`` and
    ``cpu_count``. Readings during worker execution are system evidence only.
    Call ``result()`` after leaving the context and joining all benchmark work.
    """

    def __init__(self, probe=None, interval=1.0):
        if not math.isfinite(interval) or interval <= 0:
            raise ValueError("CPU sample interval must be finite and positive")
        self._probe = LinuxCpuProbe() if probe is None else probe
        self._interval = interval
        self._stop = threading.Event()
        self._thread = None
        self._snapshots = []
        self._error = None
        self._finished = False

    def __enter__(self):
        if self._thread is not None:
            raise RuntimeError("A CpuMonitor can measure only one phase")
        self._snapshots.append(self._probe.read())
        self._thread = threading.Thread(
            target=self._sample, name="benchmark-cpu-monitor"
        )
        self._thread.start()
        return self

    def _sample(self):
        try:
            while not self._stop.wait(self._interval):
                self._snapshots.append(self._probe.read())
        except Exception as error:  # noqa: BLE001 - propagate thread failures to its owner.
            self._error = error

    def __exit__(self, exc_type, error, traceback):
        self._stop.set()
        self._thread.join()
        try:
            self._snapshots.append(self._probe.read())
        except Exception as probe_error:  # noqa: BLE001 - preserve body errors during cleanup.
            if self._error is None:
                self._error = probe_error
        self._finished = True
        if self._error is not None:
            if error is not None:
                error.add_note(f"CPU monitoring also failed: {self._error}")
            else:
                raise CpuMeasurementError(
                    f"CPU monitoring failed: {self._error}"
                ) from self._error
        return False

    def result(self):
        if not self._finished:
            raise RuntimeError("CPU measurement is available only after context exit")
        if self._error is not None:
            raise CpuMeasurementError(
                f"CPU monitoring failed: {self._error}"
            ) from self._error
        return summarize_cpu(
            self._snapshots, self._probe.ticks_per_second, self._probe.cpu_count
        )
