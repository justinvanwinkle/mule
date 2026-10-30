"""Runner and historical regression tracker for perf/topics benchmarks.

Runs Mule sequentially (never concurrently) against stored CPython baselines in
perf/topics/baseline.json so CPython does not need to be re-run every time.
"""

from __future__ import annotations

from datetime import datetime, timezone
import json
from pathlib import Path
import subprocess
import sys

TOPICS_DIR = Path(__file__).resolve().parent
REPO_ROOT = TOPICS_DIR.parent.parent
BASELINE_PATH = TOPICS_DIR / "baseline.json"
HISTORY_PATH = TOPICS_DIR / "history.jsonl"
RESULTS_ROOT = TOPICS_DIR.parent / "results"


def _load_baseline() -> dict:
    return json.loads(BASELINE_PATH.read_text(encoding="utf-8"))


def _load_history() -> list[dict]:
    if not HISTORY_PATH.exists():
        return []
    entries: list[dict] = []
    for line in HISTORY_PATH.read_text(encoding="utf-8").splitlines():
        line = line.strip()
        if line:
            entries.append(json.loads(line))
    return entries


def _git_identity() -> tuple[str, bool]:
    try:
        sha = subprocess.check_output(
            ["git", "-C", str(REPO_ROOT), "rev-parse", "--short", "HEAD"],
            text=True,
        ).strip()
        status = subprocess.check_output(
            ["git", "-C", str(REPO_ROOT), "status", "--porcelain", "--untracked-files=no"],
            text=True,
        ).strip()
        return sha, bool(status)
    except Exception:
        return "unknown", False


def _parse_topic_output(stdout: str) -> dict[str, str]:
    parsed: dict[str, str] = {}
    for line in stdout.splitlines():
        if "\t" in line:
            k, v = line.split("\t", 1)
            parsed[k.strip()] = v.strip()
    return parsed


def _matches_filter(topic_key: str, scen_name: str, filters: list[str]) -> bool:
    if not filters:
        return True
    full_key = f"{topic_key}:{scen_name}"
    for flt in filters:
        if (
            flt == topic_key
            or flt == full_key
            or flt == scen_name
            or topic_key.startswith(flt.rstrip("/") + "/")
            or flt in full_key
        ):
            return True
    return False


def cmd_list(args: list[str]) -> int:
    baseline = _load_baseline()
    topics = baseline["topics"]
    print(f"{'Scenario':<56} {'DefSize':>7} {'QuickSz':>7} {'CPython~10s':>12}")
    print("-" * 86)
    for topic_key, meta in topics.items():
        for scen_name, cp10 in meta["default_cpython_s"].items():
            if not _matches_filter(topic_key, scen_name, args):
                continue
            full_id = f"{topic_key}:{scen_name}"
            print(
                f"{full_id:<56} {meta['default_size']:>7} {meta['quick_size']:>7} {cp10:>11.2f}s"
            )
    return 0


def cmd_history(args: list[str]) -> int:
    last_n = 10
    filters: list[str] = []
    i = 0
    while i < len(args):
        if args[i] == "--last" and i + 1 < len(args):
            last_n = int(args[i + 1])
            i += 2
        else:
            filters.append(args[i])
            i += 1

    entries = _load_history()
    if not entries:
        print("No history entries recorded in perf/topics/history.jsonl")
        return 0

    shown = entries[-last_n:]
    # Collect matching scenario keys across shown runs
    scen_keys: list[str] = []
    seen: set[str] = set()
    for entry in shown:
        for full_key in entry.get("scenarios", {}):
            topic_key, scen_name = full_key.split(":", 1)
            if full_key not in seen and _matches_filter(topic_key, scen_name, filters):
                seen.add(full_key)
                scen_keys.append(full_key)

    for entry in shown:
        commit_tag = entry["commit"] + ("+" if entry.get("dirty") else "")
        print(
            f"=== {entry['timestamp_utc']}  commit={commit_tag}  mode={entry.get('mode', 'quick')} ==="
        )
        print(
            f"{'Scenario':<54} {'CPy~10s':>8} {'Mule~10s':>9} {'Mule/CPy':>9} {'Status':>8}"
        )
        print("-" * 92)
        scens = entry.get("scenarios", {})
        for k in scen_keys:
            row = scens.get(k)
            if not row:
                continue
            cp10 = row.get("norm10_cpython_s")
            mu10 = row.get("norm10_mule_s")
            ratio = row.get("ratio")
            status = row.get("status", "ok")
            cp_str = f"{cp10:.2f}s" if cp10 is not None else "   —"
            mu_str = f"{mu10:.2f}s" if mu10 is not None else "    —"
            rat_str = f"{ratio:.2f}x" if ratio is not None else "    —"
            print(f"{k:<54} {cp_str:>8} {mu_str:>9} {rat_str:>9} {status:>8}")
        print()
    return 0


def cmd_run(args: list[str]) -> int:
    mode = "quick"
    record_history = True
    refresh_baseline = False
    threshold = 1.05
    filters: list[str] = []

    i = 0
    while i < len(args):
        if args[i] == "--full":
            mode = "full"
            i += 1
        elif args[i] == "--quick":
            mode = "quick"
            i += 1
        elif args[i] == "--no-history":
            record_history = False
            i += 1
        elif args[i] == "--refresh-baseline":
            refresh_baseline = True
            i += 1
        elif args[i] == "--threshold" and i + 1 < len(args):
            threshold = float(args[i + 1])
            i += 2
        else:
            filters.append(args[i])
            i += 1

    baseline = _load_baseline()
    topics = baseline["topics"]
    history = _load_history()
    prev_scenarios: dict[str, dict] = {}
    for entry in history:
        if entry.get("mode") == mode:
            prev_scenarios.update(entry.get("scenarios", {}))

    commit_sha, is_dirty = _git_identity()
    stamp = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    run_scenarios: dict[str, dict] = {}
    regressions: list[tuple[str, float, float, float]] = []
    failures: list[str] = []

    print(
        f"Running perf/topics sequentially (mode={mode}, commit={commit_sha}{'+' if is_dirty else ''})"
    )
    print(
        f"{'Scenario':<52} {'CPy~10s':>8} {'Mule~10s':>9} {'Mule/CPy':>9} {'vs Prev':>9}"
    )
    print("-" * 92)

    for topic_key, meta in topics.items():
        all_scens = list(meta["default_cpython_s"].keys())
        selected_scens = [
            s for s in all_scens if _matches_filter(topic_key, s, filters)
        ]
        if not selected_scens:
            continue

        script_path = REPO_ROOT / meta["path"]
        run_size = meta["default_size"] if mode == "full" else meta["quick_size"]
        scale_factor = meta["default_size"] / run_size

        if refresh_baseline:
            cp_proc = subprocess.run(
                [sys.executable, str(script_path), "--size", str(run_size)],
                cwd=str(REPO_ROOT),
                capture_output=True,
                text=True,
                check=False,
            )
            if cp_proc.returncode == 0:
                cp_parsed = _parse_topic_output(cp_proc.stdout)
                if mode == "quick":
                    meta["quick_result"] = cp_parsed.get("result", meta["quick_result"])
                    for s in all_scens:
                        if s in cp_parsed:
                            val = float(cp_parsed[s])
                            meta["quick_cpython_s"][s] = val
                            meta["default_cpython_s"][s] = round(val * scale_factor, 6)
                else:
                    for s in all_scens:
                        if s in cp_parsed:
                            meta["default_cpython_s"][s] = float(cp_parsed[s])

        # Run whole file if all sub-scenarios are selected, otherwise run per-scenario
        run_whole = len(selected_scens) == len(all_scens)
        invocations = [("all", selected_scens)] if run_whole else [(s, [s]) for s in selected_scens]

        for scen_arg, target_scens in invocations:
            cmd = ["uv", "run", "mule", str(script_path), "--size", str(run_size)]
            if scen_arg != "all":
                cmd.extend(["--scenario", scen_arg])
            proc = subprocess.run(
                cmd,
                cwd=str(REPO_ROOT),
                capture_output=True,
                text=True,
                check=False,
            )
            parsed = _parse_topic_output(proc.stdout) if proc.returncode == 0 else {}
            result_ok = True
            if (
                proc.returncode == 0
                and scen_arg == "all"
                and mode == "quick"
                and "quick_result" in meta
            ):
                result_ok = parsed.get("result") == meta["quick_result"]

            for s in target_scens:
                full_id = f"{topic_key}:{s}"
                cp_s = (
                    meta["default_cpython_s"][s]
                    if mode == "full"
                    else meta["quick_cpython_s"][s]
                )
                cp10 = meta["default_cpython_s"][s]

                if proc.returncode != 0 or not result_ok or s not in parsed:
                    status = "mismatch" if (proc.returncode == 0 and not result_ok) else "fail"
                    failures.append(full_id)
                    run_scenarios[full_id] = {
                        "mule_s": None,
                        "cpython_s": cp_s,
                        "ratio": None,
                        "norm10_mule_s": None,
                        "norm10_cpython_s": cp10,
                        "status": status,
                    }
                    print(f"{full_id:<52} {cp10:>7.2f}s {'FAIL':>9} {'—':>9} {status:>9}")
                    continue

                mu_s = float(parsed[s])
                mu10 = round(mu_s * scale_factor, 6)
                ratio = round(mu_s / cp_s, 6) if cp_s > 0 else 0.0

                prev_row = prev_scenarios.get(full_id)
                vs_prev_str = "—"
                if prev_row and prev_row.get("ratio"):
                    prev_ratio = float(prev_row["ratio"])
                    rel = ratio / prev_ratio
                    pct = (rel - 1.0) * 100.0
                    vs_prev_str = f"{pct:+.1f}%"
                    if rel > threshold:
                        regressions.append((full_id, prev_ratio, ratio, pct))

                run_scenarios[full_id] = {
                    "mule_s": mu_s,
                    "cpython_s": cp_s,
                    "ratio": ratio,
                    "norm10_mule_s": mu10,
                    "norm10_cpython_s": cp10,
                    "status": "ok",
                }
                print(
                    f"{full_id:<52} {cp10:>7.2f}s {mu10:>8.2f}s {ratio:>8.2f}x {vs_prev_str:>9}"
                )

    if refresh_baseline:
        BASELINE_PATH.write_text(
            json.dumps(baseline, indent=2) + "\n", encoding="utf-8"
        )

    entry = {
        "timestamp_utc": stamp,
        "commit": commit_sha,
        "dirty": is_dirty,
        "mode": mode,
        "scenarios": run_scenarios,
    }

    out_dir = RESULTS_ROOT / f"topics-{stamp.replace(':', '').replace('-', '')}"
    out_dir.mkdir(parents=True, exist_ok=True)
    (out_dir / "results.json").write_text(
        json.dumps(entry, indent=2) + "\n", encoding="utf-8"
    )

    if record_history and run_scenarios:
        with HISTORY_PATH.open("a", encoding="utf-8") as f:
            f.write(json.dumps(entry) + "\n")

    if regressions:
        print("\nREGRESSIONS DETECTED (> " + f"{(threshold - 1.0) * 100:.1f}%):")
        for full_id, prev_r, cur_r, pct in regressions:
            print(f"  {full_id}: {prev_r:.2f}x -> {cur_r:.2f}x ({pct:+.1f}%)")

    return 1 if (failures or regressions) else 0


def main() -> None:
    args = sys.argv[1:]
    if not args or args[0] in ("-h", "--help", "help"):
        print("Usage: uv run python -m perf.topics <list|run|history> [options] [filters...]")
        print("  list [filter...]                       List topic sub-scenarios and CPython ~10s baselines")
        print("  run [--quick|--full] [filter...]       Run Mule sequentially, compare to CPython & prev run, record history")
        print("  history [--last N] [filter...]         Display recorded history and ratios from perf/topics/history.jsonl")
        return

    subcmd = args[0]
    rest = args[1:]
    if subcmd == "list":
        sys.exit(cmd_list(rest))
    elif subcmd == "history":
        sys.exit(cmd_history(rest))
    elif subcmd == "run":
        sys.exit(cmd_run(rest))
    else:
        raise ValueError(f"Unknown subcommand: {subcmd}")


if __name__ == "__main__":
    main()
