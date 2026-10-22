"""Append-only history of complete suite runs, for comparison over time.

Each line of the history file is one JSON object summarizing a run of every
scenario against a configured set of runtimes: medians, time ratios, status, and
the checkout revision of each runtime. The file is committed with the code so
the trend survives the ignored per-run result directories.
"""

import json

SCHEMA_VERSION = 2


def _milliseconds(row, field):
    return row["summary"][field]["median"] * 1000 if row["status"] == "ok" else None


def entry(report):
    """The history line for ``report``, the full ``results.json`` object."""
    return {
        "schema_version": SCHEMA_VERSION,
        "created_utc": report["created_utc"],
        "platform": report["platform"],
        "baseline": report["baseline"],
        "options": {
            key: report["options"][key]
            for key in ("samples", "size", "seed", "loops", "warmups")
        },
        "runtimes": [
            {
                "name": runtime["name"],
                "commit": (runtime.get("revision") or {}).get("commit"),
                "tracked_diff_sha256": (runtime.get("revision") or {}).get(
                    "tracked_diff_sha256"
                ),
                "version": (runtime.get("version") or "").strip() or None,
            }
            for runtime in report["runtimes"]
        ],
        "results": [
            {
                "case": row["case"],
                "runtime": row["runtime"],
                "status": row["status"],
                "kernel_ms": _milliseconds(row, "kernel_seconds"),
                "process_ms": _milliseconds(row, "process_seconds"),
                "preparation_ms": _milliseconds(row, "preparation_seconds"),
                "kernel_time_ratio": row.get("time_ratio", {}).get("kernel_seconds"),
                "process_time_ratio": row.get("time_ratio", {}).get("process_seconds"),
            }
            for row in report["rows"]
        ],
    }


def record(report, path):
    """Append ``report`` to the history file at ``path``."""
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as history:
        history.write(json.dumps(entry(report), sort_keys=True) + "\n")


def load(path):
    if not path.is_file():
        return []
    return [
        json.loads(line)
        for line in path.read_text(encoding="utf-8").splitlines()
        if line.strip()
    ]


# An empty diff of the tracked tree has this digest; anything else is dirty.
_CLEAN_DIFF = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"


def _revision(run, runtime_name):
    runtime = next((r for r in run["runtimes"] if r["name"] == runtime_name), None)
    if runtime is None or not runtime.get("commit"):
        return "unknown"
    dirty = runtime.get("tracked_diff_sha256") not in (None, _CLEAN_DIFF)
    return runtime["commit"][:9] + ("+" if dirty else "")


def _candidates(run):
    """The runtimes a run compares against its baseline: every runtime that is
    not the baseline, so all non-baseline runtimes appear in the table."""
    candidates = [
        runtime["name"]
        for runtime in run["runtimes"]
        if runtime["name"] != run["baseline"]
    ]
    return candidates or [run["baseline"]]


def table(runs, runtime_name=None, cases=None, limit=None):
    """Lines of a text table: one row per run/runtime, one column per scenario,
    each cell the kernel median and elapsed-time ratio of ``runtime_name``
    (default: each run's non-baseline runtimes) divided by the run's baseline."""
    if limit is not None:
        runs = runs[-limit:]
    if not runs:
        return ["no history"]
    if cases is None:
        cases = []
        for run in runs:
            for result in run["results"]:
                if result["case"] not in cases:
                    cases.append(result["case"])
    header = ["run (UTC)", "runtime", "revision", "samples", *cases]
    rows = []
    for run in runs:
        selected_runtimes = [runtime_name] if runtime_name else _candidates(run)
        for selected in selected_runtimes:
            results = {
                result["case"]: result
                for result in run["results"]
                if result["runtime"] == selected
            }
            cells = []
            for case in cases:
                result = results.get(case)
                if result is None:
                    cells.append("-")
                elif result["status"] != "ok":
                    cells.append(result["status"])
                else:
                    time_ratio = result["kernel_time_ratio"]
                    ratio = f" {time_ratio:.3g}" if time_ratio is not None else " —"
                    cells.append(f"{result['kernel_ms']:.3g} ms{ratio}")
            rows.append(
                [
                    run["created_utc"],
                    selected,
                    _revision(run, selected),
                    str(run["options"]["samples"]),
                    *cells,
                ]
            )
    widths = [max(len(row[i]) for row in [header, *rows]) for i in range(len(header))]
    lines = [
        "kernel median and elapsed-time ratio (runtime / baseline; lower is better)",
        "  ".join(
            cell.ljust(width) for cell, width in zip(header, widths, strict=True)
        ),
    ]
    lines.extend(
        "  ".join(cell.ljust(width) for cell, width in zip(row, widths, strict=True))
        for row in rows
    )
    return lines
