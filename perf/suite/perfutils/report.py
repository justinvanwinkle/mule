"""Dependency-free HTML dashboard and standalone SVG comparison charts."""

import html
import json
import math

COLORS = ("#c14c32", "#267a77", "#71558a", "#a47116")


def _ratio(row, metric):
    value = row.get("speedup", {}).get(metric)
    if row["status"] != "ok" or value is None:
        return None
    return value if math.isfinite(value) and value > 0 else None


def chart(report, metric):
    rows = [row for row in report["rows"] if row["runtime"] != report["baseline"]]
    runtime_names = [
        runtime["name"]
        for runtime in report["runtimes"]
        if runtime["name"] != report["baseline"]
    ]
    groups = {}
    for row in rows:
        groups.setdefault(row["case"], []).append(row)
        if row["runtime"] not in runtime_names:
            runtime_names.append(row["runtime"])
    colors = {
        name: COLORS[0]
        if name.lower().startswith("mule")
        else COLORS[1]
        if name.lower().startswith("sbcl")
        else COLORS[2 + index % 2]
        for index, name in enumerate(runtime_names)
    }
    width = max(1120, max((len(case) for case in groups), default=20) * 8 + 850)
    left, right = width - 820, 200
    top = 190 + max(0, len(runtime_names) - 2) * 24
    group_height = max(64, len(runtime_names) * 26 + 20)
    bottom = top + max(1, len(groups)) * group_height
    height = bottom + 85
    values = [
        math.log2(value) for row in rows if (value := _ratio(row, metric)) is not None
    ]
    step = max(1, math.ceil((max([1.0, *values]) - min([-1.0, *values])) / 8))
    low = math.floor(min([-1.0, *values]) / step) * step
    high = math.ceil(max([1.0, *values]) / step) * step

    def x(value):
        return left + (value - low) / (high - low) * (width - left - right)

    svg = [
        (
            f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {width} {height}" '
            'role="img" aria-labelledby="title description">'
        ),
        '<title id="title">Performance relative to '
        + html.escape(report["baseline"])
        + "</title>",
        '<desc id="description">Logarithmic ratios. Values above one mean faster. Each scenario has one lane per candidate runtime. Failures have no plotted point.</desc>',
        f'<rect width="{width}" height="{height}" fill="#faf8f3"/>',
        '<g font-family="system-ui,sans-serif" font-size="14" fill="#202b2b">',
        '<text x="36" y="37" font-size="11" font-weight="700" letter-spacing="2" fill="#c14c32">MULE / PERFORMANCE</text>',
        '<text x="36" y="77" font-size="30" font-weight="650">'
        + (
            "Kernel performance"
            if metric == "kernel_seconds"
            else "Whole-process performance"
        )
        + "</text>",
        (
            f'<text x="36" y="105" fill="#596564">{html.escape(report["created_utc"])} · '
            f"{report.get('options', {}).get('samples', '?')} samples per comparison</text>"
        ),
        f'<text x="36" y="132" fill="#596564">Baseline: {html.escape(report["baseline"])} = 1× · Larger ratios are faster</text>',
    ]
    zero = x(0)
    for index, name in enumerate(runtime_names):
        lx = 42 if index != 1 else 340
        ly = 159 + max(0, index - 1) * 24
        svg.append(
            f'<circle cx="{lx}" cy="{ly - 4}" r="5" fill="{colors[name]}"/><text x="{lx + 14}" y="{ly}" fill="{colors[name]}" font-weight="650">{html.escape(name)}</text>'
        )
    for tick in range(low, high + 1, step):
        position = x(tick)
        svg.append(
            f'<line x1="{position:.2f}" y1="{top - 10}" x2="{position:.2f}" y2="{bottom}" stroke="#dedfd8"/>'
        )
        svg.append(
            f'<text x="{position:.2f}" y="{bottom + 25}" text-anchor="middle" font-size="12" fill="#596564">{2.0**tick:.3g}×</text>'
        )
    svg.append(
        f'<line id="baseline" x1="{zero:.2f}" y1="{top - 10}" x2="{zero:.2f}" y2="{bottom}" stroke="#687775" stroke-width="1.5"/>'
    )
    for index, (case, case_rows) in enumerate(groups.items()):
        gy = top + index * group_height
        if index % 2 == 0:
            svg.append(
                f'<rect x="24" y="{gy}" width="{width - 48}" height="{group_height}" fill="#202b2b" opacity=".025"/>'
            )
        svg.append(
            f'<text x="36" y="{gy + group_height / 2 + 5}" font-weight="550">{html.escape(case.replace("_", " "))}</text>'
        )
        for row in case_rows:
            y = gy + 23 + runtime_names.index(row["runtime"]) * 26
            value = _ratio(row, metric)
            color = colors[row["runtime"]]
            if value is None:
                status = row["status"] if row["status"] != "ok" else "not available"
                svg.append(
                    f'<text x="{width - right + 18}" y="{y + 4}" font-size="12" fill="#8d4232">{html.escape(status)}</text>'
                )
                continue
            endpoint = x(math.log2(value))
            svg.append(
                f'<line x1="{zero:.2f}" y1="{y}" x2="{endpoint:.2f}" y2="{y}" stroke="{color}" stroke-width="2" opacity=".4"/>'
            )
            svg.append(
                f'<circle data-ratio="{value:.12g}" data-runtime="{html.escape(row["runtime"])}" cx="{endpoint:.2f}" cy="{y}" r="5.5" fill="{color}" stroke="#faf8f3" stroke-width="1.5"><title>{html.escape(row["runtime"])}: {value:.3g}×</title></circle>'
            )
            svg.append(
                f'<text x="{width - right + 18}" y="{y + 5}" fill="{color}" font-weight="650">{value:.3g}×</text>'
            )
    if not rows:
        svg.append(
            f'<text x="36" y="{top + 35}">No candidate comparisons in this report.</text>'
        )
    svg.append(
        f'<text x="{left}" y="{bottom + 53}" font-size="10" letter-spacing="1.5" fill="#596564">SLOWER</text>'
    )
    svg.append(
        f'<text x="{width - right}" y="{bottom + 53}" font-size="10" letter-spacing="1.5" text-anchor="end" fill="#596564">FASTER</text>'
    )
    svg.append(
        f'<text x="36" y="{height - 12}" font-size="11" fill="#596564">Median ratios · log₂ scale · failures are not timings</text>'
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
            speedup = _ratio(row, "kernel_seconds")
            cells.append(f"{speedup:.3g}×" if speedup is not None else "—")
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
<section aria-labelledby="reading"><h2 id="reading">How to read this report</h2><p>Each dot compares median times for the same scenario and input. Ratios above 1× mean faster than the baseline.
The logarithmic axis gives equal space to equal multiplicative changes. Each runtime has its own lane within each scenario.
Baseline points are implicit at 1×. Failures contribute no speedup.</p><p>Preparation, warmups, output, and result checks are excluded from kernel time. Whole-process time includes
startup, preparation, warmups, and measured calls; compilation is separate. Each ratio compares
the same scenario and input. σ describes sample variation, not a confidence interval.
Short kernels can be dominated by measurement overhead; increase workload size before drawing conclusions.</p><p>{concurrency}</p></section>
<section aria-label="Kernel comparison"><figure><div class="chart" role="region" aria-label="Scrollable kernel chart" tabindex="0"><img src="kernel.svg" alt="Kernel ratios by scenario and runtime. Exact timings appear in the table below."></div><figcaption>Kernel medians. Preparation and compilation are excluded.</figcaption></figure></section>
<section aria-label="Whole-process comparison"><figure><div class="chart" role="region" aria-label="Scrollable whole-process chart" tabindex="0"><img src="process.svg" alt="Whole-process ratios by scenario and runtime. Exact timings appear in the table below."></div><figcaption>Whole-process medians. Includes startup and setup, not compilation.</figcaption></figure></section>
<section aria-labelledby="measurements"><h2 id="measurements">Every measurement</h2><div class="table" role="region" aria-label="Scrollable results table" tabindex="0"><table><thead><tr><th scope="col">Scenario</th><th scope="col">Runtime</th><th scope="col">Status</th>
<th scope="col">Kernel median</th><th scope="col">Process median</th><th scope="col">Kernel ratio</th></tr></thead><tbody>{content}</tbody></table></div></section>
<section aria-labelledby="provenance"><h2 id="provenance">Reproduce, then compare</h2><details><summary>Platform, run options, runtime commands, and revisions</summary><pre>{html.escape(json.dumps(context, indent=2))}</pre>{provenance}</details><p>Raw results retain samples, hashes, commands, and build diagnostics. A commit plus a tracked-diff digest identifies the measured working tree, not necessarily a clean release.</p></section>
<footer>Generated locally by Mule’s dependency-free performance harness. No external fonts, scripts, or services.</footer></main></body></html>"""


def write_report(report, directory):
    (directory / "results.json").write_text(json.dumps(report, indent=2) + "\n")
    (directory / "kernel.svg").write_text(chart(report, "kernel_seconds"))
    (directory / "process.svg").write_text(chart(report, "process_seconds"))
    (directory / "index.html").write_text(dashboard(report))
