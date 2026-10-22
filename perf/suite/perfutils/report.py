"""Dependency-free HTML dashboard and standalone SVG comparison charts."""

import html
import json
import math
import re
from datetime import datetime

from . import history

COLORS = ("#c14c32", "#267a77", "#71558a", "#a47116")


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
    panel_width = max(
        532,
        len(runtimes) * 140 + 88,
        max((len(case) for case in groups), default=20) * 10 + 48,
    )
    panel_height = 330
    gap, margin, top = 24, 28, 170
    width = 2 * panel_width + gap + 2 * margin
    height = top + max(1, math.ceil(len(groups) / 2)) * (panel_height + gap) + 40
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
    for index, (case, case_rows) in enumerate(groups.items()):
        px = margin + (index % 2) * (panel_width + gap)
        py = top + (index // 2) * (panel_height + gap)
        left, right = px + 70, px + panel_width - 24
        zero, plot_height = py + 265, 164
        values = [_milliseconds(row, metric) for row in case_rows.values()]
        maximum = max((value for value in values if value is not None), default=0)
        upper = maximum * 1.2 if maximum else 1.0
        if not math.isfinite(upper):
            upper = maximum
        svg.extend(
            [
                f'<g data-case="{html.escape(case)}">',
                f'<rect x="{px}" y="{py}" width="{panel_width}" height="{panel_height}" fill="#fffdf8" stroke="#dedfd8"/>',
                f'<text x="{px + 22}" y="{py + 34}" font-size="18" font-weight="650">{html.escape(case.replace("_", " "))}</text>',
                f'<text x="{px + 22}" y="{py + 58}" font-size="11" fill="#596564">MILLISECONDS · INDEPENDENT SCALE</text>',
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
        svg.append("</g>")
    if not groups:
        svg.append(
            f'<text x="36" y="{top + 35}">No scenario measurements in this report.</text>'
        )
    svg.append(
        f'<text x="36" y="{height - 18}" font-size="11" fill="#596564">Compare runtimes within a panel, not bar heights across panels. Failures are not timings.</text>'
    )
    svg.append("</g></svg>")
    return "\n".join(svg)


def dashboard(report):
    rows = []
    for row in report["rows"]:
        cells = [html.escape(row[key]) for key in ("case", "runtime", "status")]
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
        rows.append(
            "<tr>" + "".join("<td>" + cell + "</td>" for cell in cells) + "</tr>"
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
th{{background:#eeeee6}}small{{display:block;color:#596564}}pre{{font:12px/1.6 ui-monospace,monospace;white-space:pre-wrap;overflow-wrap:anywhere}}article{{border-top:1px solid #dedfd8;padding:12px 0}}summary{{cursor:pointer}}footer{{border-top:1px solid #dedfd8;padding-top:20px;font-size:13px;color:#596564}}
@media(max-width:600px){{main{{padding:24px 18px}}section{{margin:28px 0}}}}
</style></head><body><main><header><div class="eyebrow">MULE / PERFORMANCE LABORATORY</div><h1>Measure the work.<br>Keep the evidence.</h1>
<p>{platform}<br>{stamp}</p>
<span class="pill">Baseline: {baseline}</span><span class="pill">{successful} validated / {len(report["rows"])} comparisons</span>
<p><a href="results.json">Raw results &amp; provenance</a> · <a href="kernel.svg">Kernel SVG</a> · <a href="process.svg">Process SVG</a></p></header>
<section aria-labelledby="reading"><h2 id="reading">How to read this report</h2><p>Vertical bars show median elapsed time in milliseconds for the same scenario and input. Shorter bars mean faster execution.
Each scenario has an independent linear vertical scale starting at zero. Compare runtimes within a panel, not bar heights across panels.
The baseline appears alongside every other runtime. Unavailable or failed results have no bar.
Table ratios divide runtime elapsed time by baseline elapsed time; lower is better. A ratio of 1 means equal time.</p><p>Preparation, warmups, output, and result checks are excluded from kernel time. Whole-process time includes
startup, preparation, warmups, measured calls, and output; compilation is separate.
σ describes sample variation, not a confidence interval.
Short kernels can be dominated by measurement overhead; increase workload size before drawing conclusions.</p><p>{concurrency}</p></section>
<section aria-label="Kernel comparison"><figure><div class="chart" role="region" aria-label="Scrollable kernel chart" tabindex="0"><img src="kernel.svg" alt="Vertical bars compare median kernel milliseconds for each runtime within each scenario. Shorter is faster. Independent zero-based linear scales."></div><figcaption>Kernel elapsed medians. Preparation and compilation are excluded. Each scenario has its own scale.</figcaption></figure></section>
<section aria-label="Whole-process comparison"><figure><div class="chart" role="region" aria-label="Scrollable whole-process chart" tabindex="0"><img src="process.svg" alt="Vertical bars compare median whole-process milliseconds by runtime and scenario. Shorter is faster. Independent zero-based linear scales."></div><figcaption>Whole-process elapsed medians. Includes startup and setup, not compilation. Each scenario has its own scale.</figcaption></figure></section>
<section aria-labelledby="measurements"><h2 id="measurements">Every measurement</h2><div class="table" role="region" aria-label="Scrollable results table" tabindex="0"><table><thead><tr><th scope="col">Scenario</th><th scope="col">Runtime</th><th scope="col">Status</th>
<th scope="col">Kernel median</th><th scope="col">Process median</th><th scope="col">Kernel runtime / {baseline}</th></tr></thead><tbody>{content}</tbody></table></div></section>
<section aria-labelledby="provenance"><h2 id="provenance">Reproduce, then compare</h2><details><summary>Platform, run options, runtime commands, and revisions</summary><pre>{html.escape(json.dumps(context, indent=2))}</pre>{provenance}</details><p>Raw results retain samples, hashes, commands, and build diagnostics. A commit plus a tracked-diff digest identifies the measured working tree, not necessarily a clean release.</p></section>
<footer>Generated locally by Mule’s dependency-free performance harness. No external fonts, scripts, or services.</footer></main></body></html>"""


def write_report(report, directory):
    (directory / "results.json").write_text(json.dumps(report, indent=2) + "\n")
    (directory / "kernel.svg").write_text(chart(report, "kernel_seconds"))
    (directory / "process.svg").write_text(chart(report, "process_seconds"))
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


def update_site(report, root):
    """Update the local GitHub Pages site assets and dated snapshot captions."""
    assets = root / "docs/assets"
    if assets.is_dir():
        write_report(report, assets)
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
