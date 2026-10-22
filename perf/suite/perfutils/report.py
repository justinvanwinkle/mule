"""Dependency-free HTML dashboard and standalone SVG comparison charts."""

import html
import json
import math
import re
from datetime import datetime
from pathlib import Path

from . import history

COLORS = ("#c14c32", "#267a77", "#71558a", "#a47116")
_TYPED_SUFFIX = "_typed"


def _paired_panels(groups):
    """Pair each ``<case>`` with ``<case>_typed`` when both are present."""
    panels = []
    for case, case_rows in groups.items():
        if case.endswith(_TYPED_SUFFIX) and case[: -len(_TYPED_SUFFIX)] in groups:
            continue
        typed_case = f"{case}{_TYPED_SUFFIX}"
        typed_rows = groups.get(typed_case)
        panels.append(
            (
                case,
                case_rows,
                typed_case if typed_rows is not None else None,
                typed_rows,
            )
        )
    return panels


def _ordered_dashboard_rows(report):
    """Keep untyped and typed rows for each runtime adjacent in the table."""
    groups = {}
    case_order = []
    runtime_order = [runtime["name"] for runtime in report.get("runtimes", ())]
    for row in report["rows"]:
        case = row["case"]
        runtime = row["runtime"]
        if case not in groups:
            groups[case] = {}
            case_order.append(case)
        groups[case][runtime] = row
        if runtime not in runtime_order:
            runtime_order.append(runtime)
    ordered = []
    seen_cases = set()
    for case in case_order:
        if case in seen_cases:
            continue
        if case.endswith(_TYPED_SUFFIX) and case[: -len(_TYPED_SUFFIX)] in groups:
            continue
        seen_cases.add(case)
        typed_case = f"{case}{_TYPED_SUFFIX}"
        if typed_case in groups:
            seen_cases.add(typed_case)
            for runtime in runtime_order:
                untyped_row = groups[case].get(runtime)
                typed_row = groups[typed_case].get(runtime)
                if untyped_row is not None:
                    ordered.append((untyped_row, case, "untyped", None))
                if typed_row is not None:
                    ordered.append((typed_row, case, "typed", untyped_row))
        else:
            for runtime in runtime_order:
                row = groups[case].get(runtime)
                if row is not None:
                    ordered.append((row, None, None, None))
    return ordered


def _ratio(row, metric):
    value = row.get("time_ratio", {}).get(metric)
    if row["status"] != "ok" or value is None:
        return None
    return value if math.isfinite(value) and value >= 0 else None


def _milliseconds(row, metric):
    if row is None or row["status"] != "ok":
        return None
    seconds = row.get("summary", {}).get(metric, {}).get("median")
    if seconds is None or not math.isfinite(seconds) or seconds < 0:
        return None
    value = seconds * 1000
    return value if math.isfinite(value) else None


def _runtime_label(name):
    if name.lower().startswith("cpython"):
        return "CPython" + name[7:]
    if name.lower().startswith("mule"):
        return "Mule" + name[4:].replace("-o", " -O")
    if name.lower().startswith("sbcl"):
        return "SBCL" + name[4:]
    return name


def chart(report, metric):
    """Compare elapsed medians with linear, zero-based axes in separate panels."""
    runtimes = [runtime["name"] for runtime in report["runtimes"]]
    groups = {}
    for row in report["rows"]:
        groups.setdefault(row["case"], {})[row["runtime"]] = row
        if row["runtime"] not in runtimes:
            runtimes.append(row["runtime"])
    # The baseline always comes first; other runtimes retain configuration order.
    if report["baseline"] in runtimes:
        runtimes.remove(report["baseline"])
        runtimes.insert(0, report["baseline"])
    colors = {
        name: "#788582"
        if name == report["baseline"]
        else COLORS[0]
        if name.lower().startswith("mule")
        else COLORS[1]
        if name.lower().startswith("sbcl")
        else COLORS[2 + index % 2]
        for index, name in enumerate(runtimes)
    }
    panels = _paired_panels(groups)
    has_paired = any(typed_rows is not None for _, _, _, typed_rows in panels)
    panel_width = max(
        640 if has_paired else 532,
        len(runtimes) * (190 if has_paired else 140) + 88,
        max((len(case) for case in groups), default=20) * 10 + 48,
    )
    panel_height = 330
    gap, margin, top = 24, 28, 170
    width = 2 * panel_width + gap + 2 * margin
    height = top + max(1, math.ceil(len(panels) / 2)) * (panel_height + gap) + 40
    title = (
        "Kernel elapsed time"
        if metric == "kernel_seconds"
        else "Whole-process elapsed time"
    )
    stamp = report["created_utc"]
    samples = report.get("options", {}).get("samples", "?")
    svg = [
        f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {width} {height}" role="img" aria-labelledby="title description">',
        f'<title id="title">{title}: median milliseconds by scenario and runtime</title>',
        '<desc id="description">Vertical bars show elapsed time. Shorter means faster. Every panel uses an independent linear scale starting at zero. Runtime labels and measured times identify each bar. Failed or unavailable results have no bar.</desc>',
        f'<rect width="{width}" height="{height}" fill="#faf8f3"/>',
        '<g font-family="system-ui,sans-serif" fill="#202b2b" font-size="14">',
        '<text x="36" y="35" font-size="11" font-weight="700" letter-spacing="2" fill="#c14c32">MULE / PERFORMANCE</text>',
        f'<text x="36" y="76" font-size="30" font-weight="650">{title}</text>',
        f'<text x="36" y="105" fill="#596564">{html.escape(stamp)} · {html.escape(str(samples))} samples per runtime and scenario</text>',
        '<text x="36" y="135" font-weight="600">Shorter bars are faster · Linear axes start at zero · Each panel has its own scale</text>',
    ]
    for index, (case, case_rows, typed_case, typed_rows) in enumerate(panels):
        px = margin + (index % 2) * (panel_width + gap)
        py = top + (index // 2) * (panel_height + gap)
        left, right = px + 70, px + panel_width - 24
        zero, plot_height = py + 265, 164
        all_rows = (
            (*case_rows.values(), *typed_rows.values())
            if typed_rows is not None
            else tuple(case_rows.values())
        )
        values = [_milliseconds(row, metric) for row in all_rows]
        maximum = max((value for value in values if value is not None), default=0)
        upper = maximum * 1.2 if maximum else 1.0
        if not math.isfinite(upper):
            upper = maximum
        typed_attr = (
            f' data-typed-case="{html.escape(typed_case)}"'
            if typed_case is not None
            else ""
        )
        subtitle = (
            "UNTYPED VS TYPED · MILLISECONDS · INDEPENDENT SCALE"
            if typed_rows is not None
            else "MILLISECONDS · INDEPENDENT SCALE"
        )
        svg.extend(
            [
                f'<g data-case="{html.escape(case)}"{typed_attr}>',
                f'<rect x="{px}" y="{py}" width="{panel_width}" height="{panel_height}" fill="#fffdf8" stroke="#dedfd8"/>',
                f'<text x="{px + 22}" y="{py + 34}" font-size="18" font-weight="650">{html.escape(case.replace("_", " "))}</text>',
                f'<text x="{px + 22}" y="{py + 58}" font-size="11" fill="#596564">{subtitle}</text>',
            ]
        )
        if typed_rows is not None:
            svg.extend(
                [
                    f'<rect x="{right - 150}" y="{py + 22}" width="12" height="12" fill="#596564"/>',
                    f'<text x="{right - 132}" y="{py + 32}" font-size="11" fill="#596564">untyped</text>',
                    f'<rect x="{right - 74}" y="{py + 22}" width="12" height="12" fill="#596564" fill-opacity="0.45" stroke="#596564" stroke-width="1.5"/>',
                    f'<text x="{right - 56}" y="{py + 32}" font-size="11" fill="#596564">typed</text>',
                ]
            )
        for tick in range(5):
            value = upper * (tick / 4)
            y = zero - plot_height * (tick / 4)
            svg.append(
                f'<line x1="{left}" y1="{y:.2f}" x2="{right}" y2="{y:.2f}" stroke="#e2e3dc"/>'
            )
            svg.append(
                f'<text x="{left - 10}" y="{y + 4:.2f}" font-size="11" fill="#596564" text-anchor="end">{value:.3g}</text>'
            )
        svg.append(
            f'<line data-zero-y="{zero}" x1="{left}" y1="{zero}" x2="{right}" y2="{zero}" stroke="#788582"/>'
        )
        lane = (right - left) / max(1, len(runtimes))
        if typed_rows is None:
            bar_width = min(70, lane * 0.55)
            for runtime_index, runtime in enumerate(runtimes):
                center = left + lane * (runtime_index + 0.5)
                row = case_rows.get(runtime)
                value = _milliseconds(row, metric)
                if value is None:
                    status = (
                        "missing"
                        if row is None
                        else row["status"]
                        if row["status"] != "ok"
                        else "not available"
                    )
                    svg.append(
                        f'<text x="{center:.2f}" y="{zero - 20}" text-anchor="middle" font-size="11" fill="#8d4232">{html.escape(status)}</text>'
                    )
                else:
                    bar_height = (value / upper) * plot_height
                    y = zero - bar_height
                    svg.append(
                        f'<rect data-runtime="{html.escape(runtime)}" data-ms="{value:.17g}" x="{center - bar_width / 2:.2f}" y="{y:.2f}" width="{bar_width:.2f}" height="{bar_height:.2f}" fill="{colors[runtime]}"><title>{html.escape(runtime)}: {value:.6g} ms</title></rect>'
                    )
                    svg.append(
                        f'<text x="{center:.2f}" y="{y - 10:.2f}" text-anchor="middle" font-size="13" font-weight="650">{value:.6g} ms</text>'
                    )
                svg.append(
                    f'<text data-runtime="{html.escape(runtime)}" x="{center:.2f}" y="{zero + 26}" text-anchor="middle" font-size="13" font-weight="600">{html.escape(_runtime_label(runtime))}</text>'
                )
        else:
            sub_width = min(46, lane * 0.34)
            sub_gap = max(6, min(12, lane * 0.06))
            offset = (sub_width + sub_gap) / 2
            for runtime_index, runtime in enumerate(runtimes):
                center = left + lane * (runtime_index + 0.5)
                variants = (
                    ("untyped", case, case_rows.get(runtime), center - offset, ""),
                    (
                        "typed",
                        typed_case,
                        typed_rows.get(runtime),
                        center + offset,
                        f' fill-opacity="0.52" stroke="{colors[runtime]}" stroke-width="1.5"',
                    ),
                )
                for variant, vcase, row, sub_center, style_extra in variants:
                    value = _milliseconds(row, metric)
                    if value is None:
                        status = (
                            "missing"
                            if row is None
                            else row["status"]
                            if row["status"] != "ok"
                            else "not available"
                        )
                        svg.append(
                            f'<text x="{sub_center:.2f}" y="{zero - 20}" text-anchor="middle" font-size="10" fill="#8d4232">{html.escape(status)}</text>'
                        )
                    else:
                        bar_height = (value / upper) * plot_height
                        y = zero - bar_height
                        svg.append(
                            f'<rect data-runtime="{html.escape(runtime)}" data-variant="{variant}" data-case="{html.escape(vcase)}" data-ms="{value:.17g}" x="{sub_center - sub_width / 2:.2f}" y="{y:.2f}" width="{sub_width:.2f}" height="{bar_height:.2f}" fill="{colors[runtime]}"{style_extra}><title>{html.escape(runtime)} ({variant}): {value:.6g} ms</title></rect>'
                        )
                        svg.append(
                            f'<text x="{sub_center:.2f}" y="{y - 8:.2f}" text-anchor="middle" font-size="11" font-weight="650">{value:.6g} ms</text>'
                        )
                    svg.append(
                        f'<text x="{sub_center:.2f}" y="{zero + 15}" text-anchor="middle" font-size="10" fill="#596564">{variant}</text>'
                    )
                svg.append(
                    f'<text data-runtime="{html.escape(runtime)}" x="{center:.2f}" y="{zero + 34}" text-anchor="middle" font-size="13" font-weight="600">{html.escape(_runtime_label(runtime))}</text>'
                )
        svg.append("</g>")
    if not panels:
        svg.append(
            f'<text x="36" y="{top + 35}">No scenario measurements in this report.</text>'
        )
    svg.append(
        f'<text x="36" y="{height - 18}" font-size="11" fill="#596564">Compare runtimes within a panel, not bar heights across panels. Failures are not timings.</text>'
    )
    svg.append("</g></svg>")
    return "\n".join(svg)


def _history_milliseconds(result, metric):
    if result is None or result.get("status") != "ok":
        return None
    value = result.get(metric)
    if value is None or not math.isfinite(value) or value < 0:
        return None
    return value


def _trend_runs(report, history_runs=None):
    runs = [dict(run) for run in (history_runs or ())]
    if report.get("rows"):
        current = history.entry(report)
        existing = next(
            (
                i
                for i, run in enumerate(runs)
                if run.get("created_utc") == current["created_utc"]
            ),
            None,
        )
        if existing is not None:
            runs[existing] = current
        else:
            runs.append(current)
    return runs


def _short_run_label(run):
    stamp = str(run.get("created_utc", ""))
    try:
        dt = datetime.strptime(stamp, "%Y%m%dT%H%M%S.%fZ")
        date_part = f"{dt.strftime('%b')} {dt.day}"
    except ValueError:
        date_part = stamp[:10] or "run"
    for runtime in run.get("runtimes", ()):
        if runtime.get("name", "").lower().startswith("mule") and runtime.get("commit"):
            return f"{date_part} ({runtime['commit'][:7]})"
    for runtime in run.get("runtimes", ()):
        if runtime.get("commit"):
            return f"{date_part} ({runtime['commit'][:7]})"
    return date_part


def trend_chart(report, history_runs=None, metric="kernel_ms"):
    """Plot median elapsed milliseconds across recorded runs for each scenario."""
    runs = _trend_runs(report, history_runs)
    runtimes = [runtime["name"] for runtime in report.get("runtimes", ())]
    groups = {}
    for row in report.get("rows", ()):
        groups.setdefault(row["case"], {})[row["runtime"]] = row
        if row["runtime"] not in runtimes:
            runtimes.append(row["runtime"])
    if not groups:
        for run in runs:
            for res in run.get("results", ()):
                groups.setdefault(res["case"], {})
                if res["runtime"] not in runtimes:
                    runtimes.append(res["runtime"])
    if report.get("baseline") in runtimes:
        runtimes.remove(report["baseline"])
        runtimes.insert(0, report["baseline"])
    colors = {
        name: "#788582"
        if name == report.get("baseline")
        else COLORS[0]
        if name.lower().startswith("mule")
        else COLORS[1]
        if name.lower().startswith("sbcl")
        else COLORS[2 + index % 2]
        for index, name in enumerate(runtimes)
    }
    panels = _paired_panels(groups)
    has_paired = any(typed_rows is not None for _, _, _, typed_rows in panels)
    panel_width = max(
        640 if has_paired else 532,
        len(runtimes) * (190 if has_paired else 140) + 88,
        max((len(case) for case in groups), default=20) * 10 + 48,
    )
    panel_height = 340
    gap, margin, top = 24, 28, 196
    width = 2 * panel_width + gap + 2 * margin
    height = top + max(1, math.ceil(len(panels) / 2)) * (panel_height + gap) + 40
    title = (
        "Kernel elapsed time trend"
        if metric == "kernel_ms"
        else "Whole-process elapsed time trend"
    )
    stamp = report.get("created_utc", "")
    samples = report.get("options", {}).get("samples", "?")
    run_count_word = "run" if len(runs) == 1 else "runs"
    svg = [
        f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {width} {height}" role="img" aria-labelledby="title description">',
        f'<title id="title">{title}: median milliseconds over recorded runs by scenario and runtime</title>',
        '<desc id="description">Trend lines and points show median elapsed time in milliseconds across recorded runs from oldest (left) to newest (right). Lower means faster. Every panel uses an independent linear scale starting at zero. Failed or unavailable results have no point.</desc>',
        f'<rect width="{width}" height="{height}" fill="#faf8f3"/>',
        '<g font-family="system-ui,sans-serif" fill="#202b2b" font-size="14">',
        '<text x="36" y="35" font-size="11" font-weight="700" letter-spacing="2" fill="#c14c32">MULE / PERFORMANCE TREND</text>',
        f'<text x="36" y="76" font-size="30" font-weight="650">{title}</text>',
        f'<text x="36" y="105" fill="#596564">{html.escape(stamp)} · {len(runs)} recorded {run_count_word} · {html.escape(str(samples))} samples per runtime and scenario</text>',
        '<text x="36" y="135" font-weight="600">Lower points are faster · Linear axes start at zero · Oldest run on left, latest on right</text>',
    ]
    for r_idx, runtime in enumerate(runtimes):
        lx = 36 + r_idx * 170
        color = colors[runtime]
        svg.extend(
            [
                f'<line x1="{lx}" y1="160" x2="{lx + 24}" y2="160" stroke="{color}" stroke-width="2.5"/>',
                f'<circle cx="{lx + 12}" cy="160" r="3.5" fill="{color}"/>',
                f'<text x="{lx + 32}" y="164" font-size="13" font-weight="600">{html.escape(_runtime_label(runtime))}</text>',
            ]
        )
    for index, (case, _, typed_case, typed_rows) in enumerate(panels):
        px = margin + (index % 2) * (panel_width + gap)
        py = top + (index // 2) * (panel_height + gap)
        left, right = px + 70, px + panel_width - 24
        zero, plot_height = py + 260, 160
        target_cases = {case} if typed_case is None else {case, typed_case}
        panel_runs = [
            run
            for run in runs
            if any(
                res.get("case") in target_cases and res.get("runtime") in runtimes
                for res in run.get("results", ())
            )
        ]
        if not panel_runs and runs:
            panel_runs = [runs[-1]]
        values = [
            _history_milliseconds(res, metric)
            for run in panel_runs
            for res in run.get("results", ())
            if res.get("case") in target_cases and res.get("runtime") in runtimes
        ]
        maximum = max((value for value in values if value is not None), default=0)
        upper = maximum * 1.2 if maximum else 1.0
        if not math.isfinite(upper):
            upper = maximum
        typed_attr = (
            f' data-typed-case="{html.escape(typed_case)}"'
            if typed_case is not None
            else ""
        )
        n_runs = len(panel_runs)
        runs_badge = f"{n_runs} RUN" if n_runs == 1 else f"{n_runs} RUNS"
        subtitle = (
            f"UNTYPED VS TYPED · MILLISECONDS · {runs_badge} · INDEPENDENT SCALE"
            if typed_rows is not None
            else f"MILLISECONDS · {runs_badge} · INDEPENDENT SCALE"
        )
        svg.extend(
            [
                f'<g data-case="{html.escape(case)}"{typed_attr}>',
                f'<rect x="{px}" y="{py}" width="{panel_width}" height="{panel_height}" fill="#fffdf8" stroke="#dedfd8"/>',
                f'<text x="{px + 22}" y="{py + 34}" font-size="18" font-weight="650">{html.escape(case.replace("_", " "))}</text>',
                f'<text x="{px + 22}" y="{py + 58}" font-size="11" fill="#596564">{subtitle}</text>',
            ]
        )
        if typed_rows is not None:
            svg.extend(
                [
                    f'<line x1="{right - 166}" y1="{py + 28}" x2="{right - 144}" y2="{py + 28}" stroke="#596564" stroke-width="2"/>',
                    f'<circle cx="{right - 155}" cy="{py + 28}" r="3" fill="#596564"/>',
                    f'<text x="{right - 138}" y="{py + 32}" font-size="11" fill="#596564">untyped</text>',
                    f'<line x1="{right - 82}" y1="{py + 28}" x2="{right - 60}" y2="{py + 28}" stroke="#596564" stroke-width="2" stroke-dasharray="4,3"/>',
                    f'<circle cx="{right - 71}" cy="{py + 28}" r="3" fill="#fffdf8" stroke="#596564" stroke-width="1.5"/>',
                    f'<text x="{right - 54}" y="{py + 32}" font-size="11" fill="#596564">typed</text>',
                ]
            )
        for tick in range(5):
            value = upper * (tick / 4)
            y = zero - plot_height * (tick / 4)
            svg.append(
                f'<line x1="{left}" y1="{y:.2f}" x2="{right}" y2="{y:.2f}" stroke="#e2e3dc"/>'
            )
            svg.append(
                f'<text x="{left - 10}" y="{y + 4:.2f}" font-size="11" fill="#596564" text-anchor="end">{value:.3g}</text>'
            )
        svg.append(
            f'<line data-zero-y="{zero}" x1="{left}" y1="{zero}" x2="{right}" y2="{zero}" stroke="#788582"/>'
        )
        xs = [
            (left + right) / 2
            if n_runs <= 1
            else left + (right - left) * (idx / (n_runs - 1))
            for idx in range(n_runs)
        ]
        if n_runs == 1:
            svg.append(
                f'<text x="{xs[0]:.2f}" y="{zero + 16}" text-anchor="middle" font-size="10" fill="#596564">{html.escape(_short_run_label(panel_runs[0]))}</text>'
            )
        elif n_runs >= 2:
            svg.append(
                f'<text x="{left}" y="{zero + 16}" text-anchor="start" font-size="10" fill="#596564">{html.escape(_short_run_label(panel_runs[0]))}</text>'
            )
            if n_runs >= 3:
                mid = n_runs // 2
                svg.append(
                    f'<text x="{xs[mid]:.2f}" y="{zero + 16}" text-anchor="middle" font-size="10" fill="#596564">{html.escape(_short_run_label(panel_runs[mid]))}</text>'
                )
            svg.append(
                f'<text x="{right}" y="{zero + 16}" text-anchor="end" font-size="10" fill="#596564">{html.escape(_short_run_label(panel_runs[-1]))}</text>'
            )
        variants = (
            (("untyped", case, False),)
            if typed_case is None
            else (("untyped", case, False), ("typed", typed_case, True))
        )
        for runtime in runtimes:
            color = colors[runtime]
            for variant, vcase, is_typed in variants:
                v_attrs = (
                    f' data-variant="{variant}" data-case="{html.escape(vcase)}"'
                    if typed_case is not None
                    else ""
                )
                dash = ' stroke-dasharray="5,4"' if is_typed else ""
                segments = []
                current_segment = []
                circles = []
                for r_idx, run in enumerate(panel_runs):
                    res = next(
                        (
                            r
                            for r in run.get("results", ())
                            if r.get("case") == vcase and r.get("runtime") == runtime
                        ),
                        None,
                    )
                    val = _history_milliseconds(res, metric)
                    if val is None:
                        if len(current_segment) >= 2:
                            segments.append(current_segment)
                        current_segment = []
                    else:
                        x = xs[r_idx]
                        y = zero - (val / upper) * plot_height
                        current_segment.append(f"{x:.2f},{y:.2f}")
                        run_stamp = str(run.get("created_utc", ""))
                        circle_style = (
                            f'fill="#fffdf8" stroke="{color}" stroke-width="1.8"'
                            if is_typed
                            else f'fill="{color}"'
                        )
                        v_title = f" ({variant})" if typed_case is not None else ""
                        circles.append(
                            f'<circle data-runtime="{html.escape(runtime)}"{v_attrs} data-run="{html.escape(run_stamp)}" data-ms="{val:.17g}" cx="{x:.2f}" cy="{y:.2f}" r="3.5" {circle_style}><title>{html.escape(runtime)}{v_title} @ {html.escape(run_stamp)}: {val:.6g} ms</title></circle>'
                        )
                if len(current_segment) >= 2:
                    segments.append(current_segment)
                for seg in segments:
                    pts = " ".join(seg)
                    svg.append(
                        f'<polyline data-runtime="{html.escape(runtime)}"{v_attrs} fill="none" stroke="{color}" stroke-width="2"{dash} points="{pts}"/>'
                    )
                svg.extend(circles)
        lane = (right - left) / max(1, len(runtimes))
        latest_run = panel_runs[-1] if panel_runs else None
        for runtime_index, runtime in enumerate(runtimes):
            center = left + lane * (runtime_index + 0.5)
            parts = []
            for _, vcase, _ in variants:
                res = (
                    next(
                        (
                            r
                            for r in latest_run.get("results", ())
                            if r.get("case") == vcase and r.get("runtime") == runtime
                        ),
                        None,
                    )
                    if latest_run is not None
                    else None
                )
                val = _history_milliseconds(res, metric)
                if val is not None:
                    parts.append(f"{val:.6g} ms")
                elif res is None:
                    parts.append("missing")
                elif res.get("status") != "ok":
                    parts.append(str(res["status"]))
                else:
                    parts.append("not available")
            summary_label = f"{_runtime_label(runtime)}: {' / '.join(parts)}"
            svg.append(
                f'<text data-runtime="{html.escape(runtime)}" x="{center:.2f}" y="{zero + 38}" text-anchor="middle" font-size="12" font-weight="600">{html.escape(summary_label)}</text>'
            )
        svg.append("</g>")
    if not panels:
        svg.append(
            f'<text x="36" y="{top + 35}">No scenario measurements in this report.</text>'
        )
    svg.append(
        f'<text x="36" y="{height - 18}" font-size="11" fill="#596564">Compare runtimes within a panel, not heights across panels. Failures are not timings.</text>'
    )
    svg.append("</g></svg>")
    return "\n".join(svg)


def dashboard(report):
    rows = []
    for row, paired_base, variant, untyped_row in _ordered_dashboard_rows(report):
        case_cell = html.escape(row["case"])
        if variant == "typed" and untyped_row is not None:
            typed_ms = _milliseconds(row, "kernel_seconds")
            untyped_ms = _milliseconds(untyped_row, "kernel_seconds")
            if typed_ms is not None and untyped_ms is not None and untyped_ms > 0:
                case_cell += (
                    f"<small>typed / untyped: {typed_ms / untyped_ms:.3g}</small>"
                )
        cells = [
            case_cell,
            html.escape(row["runtime"]),
            html.escape(row["status"]),
        ]
        if row["status"] == "ok":
            for metric in ("kernel_seconds", "process_seconds"):
                summary = row["summary"][metric]
                cells.append(
                    f"{summary['median'] * 1000:.3f} ms "
                    f"<small>σ {summary['stdev'] * 1000:.3f}</small>"
                )
            time_ratio = _ratio(row, "kernel_seconds")
            cells.append(f"{time_ratio:.3g}" if time_ratio is not None else "—")
        else:
            cells.extend(["—", "—", "—"])
        row_attrs = ""
        if paired_base is not None:
            row_attrs = (
                f' class="paired-{variant}"'
                f' data-paired-case="{html.escape(paired_base)}"'
                f' data-variant="{variant}"'
            )
        rows.append(
            f"<tr{row_attrs}>"
            + "".join("<td>" + cell + "</td>" for cell in cells)
            + "</tr>"
        )
        if row.get("error"):
            rows.append(
                '<tr><td colspan="6"><details><summary>Failure details</summary><pre>'
                + html.escape(row["error"])
                + "</pre></details></td></tr>"
            )
    successful = sum(row["status"] == "ok" for row in report["rows"])
    content = "\n".join(rows)
    baseline = html.escape(report["baseline"])
    stamp = html.escape(report["created_utc"])
    platform = html.escape(report["platform"])
    context = {
        key: report[key]
        for key in ("platform", "harness_python", "affinity", "options")
        if key in report
    }
    provenance = "".join(
        "<article><h3>"
        + html.escape(runtime["name"])
        + "</h3><pre>"
        + html.escape(json.dumps(runtime, indent=2))
        + "</pre></article>"
        for runtime in report["runtimes"]
    )
    jobs = report.get("options", {}).get("jobs", 1)
    concurrency = (
        f"{jobs} concurrent scenario jobs; timings may include contention."
        if jobs > 1
        else "One scenario job."
    )
    attempts = report.get("measurement_attempts", [])
    cpu_evidence = ""
    if attempts:
        last = attempts[-1]
        cpu_evidence = (
            "<section><h2>CPU-noise checks</h2><p>Measurement status: "
            + html.escape(last["status"])
            + ". Only accepted timing attempts supply published measurements. "
            "CPU checks detect average external activity, not every brief burst.</p>"
            "<details><summary>CPU utilization and attempt evidence</summary><pre>"
            + html.escape(
                json.dumps(
                    [
                        {key: value for key, value in attempt.items() if key != "rows"}
                        for attempt in attempts
                    ],
                    indent=2,
                )
            )
            + "</pre></details><p>Rejected raw rows are retained in results.json "
            "and measurement-attempt JSON files.</p></section>"
        )
    return f"""<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Mule performance · {stamp}</title>
<style>
:root{{color-scheme:light}}*{{box-sizing:border-box}}
body{{margin:0;background:#faf8f3;color:#202b2b;font:16px/1.65 system-ui,sans-serif}}
main{{max-width:1200px;margin:auto;padding:44px 32px}}
header{{padding:24px 0 32px;border-bottom:1px solid #dedfd8}}
h1{{font-size:clamp(36px,6vw,60px);line-height:1.1;letter-spacing:-.04em;margin:22px 0}}h2{{font-size:26px;letter-spacing:-.025em}}h3{{font-size:17px}}p{{max-width:80ch}}header p{{color:#596564}}
.eyebrow{{font-size:12px;font-weight:700;letter-spacing:.18em;color:#c14c32}}
.pill{{display:inline-block;padding:6px 12px;border:1px solid #dedfd8;margin:0 8px 8px 0;font-size:13px}}
a{{color:#a13c26;text-underline-offset:4px}}a:focus-visible,summary:focus-visible,[tabindex]:focus-visible{{outline:3px solid #267a77;outline-offset:4px}}section{{margin:40px 0}}
figure{{margin:0}}.chart,.table{{overflow-x:auto;border:1px solid #dedfd8}}img{{width:100%;min-width:900px;display:block}}figcaption{{padding:12px 0;font-size:13px;color:#596564}}
table{{width:100%;border-collapse:collapse;font-size:13px;white-space:nowrap;font-variant-numeric:tabular-nums}}th,td{{text-align:left;padding:12px 16px;border-bottom:1px solid #dedfd8}}
tr.paired-untyped td{{border-bottom:1px dashed #dedfd8;background:#fffdf8}}tr.paired-typed td{{background:#f4f1ea;border-bottom:2px solid #c9cac2}}
th{{background:#eeeee6}}small{{display:block;color:#596564}}pre{{font:12px/1.6 ui-monospace,monospace;white-space:pre-wrap;overflow-wrap:anywhere}}article{{border-top:1px solid #dedfd8;padding:12px 0}}summary{{cursor:pointer}}footer{{border-top:1px solid #dedfd8;padding-top:20px;font-size:13px;color:#596564}}
@media(max-width:600px){{main{{padding:24px 18px}}section{{margin:28px 0}}}}
</style></head><body><main><header><div class="eyebrow">MULE / PERFORMANCE LABORATORY</div><h1>Measure the work.<br>Keep the evidence.</h1>
<p>{platform}<br>{stamp}</p>
<span class="pill">Baseline: {baseline}</span><span class="pill">{successful} validated / {len(report["rows"])} comparisons</span>
<p><a href="results.json">Raw results &amp; provenance</a> · <a href="kernel.svg">Kernel SVG</a> · <a href="process.svg">Process SVG</a> · <a href="trend.svg">Trend SVG</a></p></header>
<section aria-labelledby="reading"><h2 id="reading">How to read this report</h2><p>Vertical bars show median elapsed time in milliseconds for the same scenario and input. Shorter bars mean faster execution.
Each scenario has an independent linear vertical scale starting at zero. Compare runtimes within a panel, not bar heights across panels.
The baseline appears alongside every other runtime. Unavailable or failed results have no bar.
Table ratios divide runtime elapsed time by baseline elapsed time; lower is better. A ratio of 1 means equal time.</p><p>Preparation, warmups, output, and result checks are excluded from kernel time. Whole-process time includes
startup, preparation, warmups, measured calls, and output; compilation is separate.
σ describes sample variation, not a confidence interval.
Short kernels can be dominated by measurement overhead; increase workload size before drawing conclusions.</p><p>{concurrency}</p></section>
{cpu_evidence}
<section aria-label="Kernel comparison"><figure><div class="chart" role="region" aria-label="Scrollable kernel chart" tabindex="0"><img src="kernel.svg" alt="Vertical bars compare median kernel milliseconds for each runtime within each scenario. Shorter is faster. Independent zero-based linear scales."></div><figcaption>Kernel elapsed medians. Preparation and compilation are excluded. Each scenario has its own scale.</figcaption></figure></section>
<section aria-label="Whole-process comparison"><figure><div class="chart" role="region" aria-label="Scrollable whole-process chart" tabindex="0"><img src="process.svg" alt="Vertical bars compare median whole-process milliseconds by runtime and scenario. Shorter is faster. Independent zero-based linear scales."></div><figcaption>Whole-process elapsed medians. Includes startup and setup, not compilation. Each scenario has its own scale.</figcaption></figure></section>
<section aria-label="Historical trend"><figure><div class="chart" role="region" aria-label="Scrollable trend chart" tabindex="0"><img src="trend.svg" alt="Trend lines and points compare median kernel milliseconds over recorded runs for each runtime within each scenario. Lower is faster. Independent zero-based linear scales."></div><figcaption>Historical kernel elapsed medians across recorded runs (oldest on left, latest on right). Each scenario has its own scale.</figcaption></figure></section>
<section aria-labelledby="measurements"><h2 id="measurements">Every measurement</h2><div class="table" role="region" aria-label="Scrollable results table" tabindex="0"><table><thead><tr><th scope="col">Scenario</th><th scope="col">Runtime</th><th scope="col">Status</th>
<th scope="col">Kernel median</th><th scope="col">Process median</th><th scope="col">Kernel runtime / {baseline}</th></tr></thead><tbody>{content}</tbody></table></div></section>
<section aria-labelledby="provenance"><h2 id="provenance">Reproduce, then compare</h2><details><summary>Platform, run options, runtime commands, and revisions</summary><pre>{html.escape(json.dumps(context, indent=2))}</pre>{provenance}</details><p>Raw results retain samples, hashes, commands, and build diagnostics. A commit plus a tracked-diff digest identifies the measured working tree, not necessarily a clean release.</p></section>
<footer>Generated locally by Mule’s dependency-free performance harness. No external fonts, scripts, or services.</footer></main></body></html>"""


def _resolve_history_runs(report, directory, history_runs):
    if history_runs is not None:
        return history_runs
    configured = report.get("options", {}).get("history")
    if configured:
        path = Path(configured)
        if path.is_file():
            return history.load(path)
    resolved = directory.resolve()
    for candidate in (
        resolved.parents[1] / "perf/history.jsonl"
        if len(resolved.parents) > 1
        else None,
        resolved.parents[1] / "history.jsonl" if len(resolved.parents) > 1 else None,
    ):
        if candidate is not None and candidate.is_file():
            return history.load(candidate)
    return []


def write_report(report, directory, history_runs=None):
    resolved_runs = _resolve_history_runs(report, directory, history_runs)
    (directory / "results.json").write_text(json.dumps(report, indent=2) + "\n")
    (directory / "kernel.svg").write_text(chart(report, "kernel_seconds"))
    (directory / "process.svg").write_text(chart(report, "process_seconds"))
    (directory / "trend.svg").write_text(trend_chart(report, resolved_runs))
    (directory / "index.html").write_text(dashboard(report))


def _measured_revision(report):
    for runtime in report.get("runtimes", ()):
        if runtime.get("name", "").lower().startswith("mule") and runtime.get(
            "revision"
        ):
            return runtime["revision"]
    for runtime in report.get("runtimes", ()):
        if runtime.get("revision"):
            return runtime["revision"]
    return None


def update_site(report, root, history_runs=None):
    """Update the local GitHub Pages site assets and dated snapshot captions."""
    if history_runs is None:
        history_runs = history.load(root / "perf/history.jsonl")
    assets = root / "docs/assets"
    if assets.is_dir():
        write_report(report, assets, history_runs=history_runs)
    stamp = report["created_utc"]
    dt = datetime.strptime(stamp, "%Y%m%dT%H%M%S.%fZ")
    month_day = f"{dt.strftime('%B')} {dt.day}"
    year = f"{dt.year}"
    date_label = f"{month_day}, {year}"
    rev = _measured_revision(report)
    short_commit = rev["commit"][:9] if rev and rev.get("commit") else None
    dirty = (
        rev.get("tracked_diff_sha256") not in (None, history._CLEAN_DIFF)
        if rev
        else False
    )

    index_html = root / "docs/index.html"
    if index_html.is_file():
        text = index_html.read_text(encoding="utf-8")
        text = re.sub(
            r"<figcaption>[A-Za-z]+ \d+, \d{4} · ",
            f"<figcaption>{date_label} · ",
            text,
            count=1,
        )
        if short_commit:
            suffix = " with uncommitted tracked changes." if dirty else "."
            text = re.sub(
                r"Measured checkout: <code>[0-9a-f]+</code>(?: with uncommitted tracked changes)?\.",
                f"Measured checkout: <code>{short_commit}</code>{suffix}",
                text,
                count=1,
            )
        index_html.write_text(text, encoding="utf-8")

    docs_readme = root / "docs/README.md"
    if docs_readme.is_file():
        text = docs_readme.read_text(encoding="utf-8")
        text = re.sub(
            r"(preserves the complete recorded suite run from\n)`[0-9TZ.]+`\.",
            rf"\1`{stamp}`.",
            text,
            count=1,
        )
        if short_commit:
            readme_checkout = (
                f"The measured checkout was `{short_commit}` with\n"
                "uncommitted tracked changes, identified by its recorded diff digest."
                if dirty
                else f"The measured checkout was `{short_commit}`."
            )
            text = re.sub(
                r"The measured checkout was `[0-9a-f]+`(?: with\nuncommitted tracked changes, identified by its recorded diff digest)?\.",
                readme_checkout,
                text,
                count=1,
            )
        docs_readme.write_text(text, encoding="utf-8")

    agents_md = root / "AGENTS.md"
    if agents_md.is_file():
        text = agents_md.read_text(encoding="utf-8")
        text = re.sub(
            r"from the recorded [A-Za-z]+ \d+,\n\d{4} suite run\.",
            f"from the recorded {month_day},\n{year} suite run.",
            text,
            count=1,
        )
        if short_commit:
            agents_checkout = (
                f"The measured checkout\nwas `{short_commit}` with uncommitted tracked changes."
                if dirty
                else f"The measured checkout\nwas `{short_commit}`."
            )
            text = re.sub(
                r"The measured checkout\nwas `[0-9a-f]+`(?: with uncommitted tracked changes)?\.",
                agents_checkout,
                text,
                count=1,
            )
        agents_md.write_text(text, encoding="utf-8")
