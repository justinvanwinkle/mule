"""Dependency-free HTML dashboard and standalone SVG comparison charts."""

import html
import json
import math

COLORS = ("#4f46e5", "#0891b2", "#c026d3", "#d97706")


def chart(report, metric):
    rows = [row for row in report["rows"] if row.get("speedup", {}).get(metric)]
    runtime_names = [runtime["name"] for runtime in report["runtimes"]]
    width, left, right = 960, 350, 110
    height = 85 + 31 * max(1, len(rows))
    values = [math.log2(row["speedup"][metric]) for row in rows]
    low = min([-1.0] + values)
    high = max([1.0] + values)

    def x(value):
        return left + (value - low) / (high - low) * (width - left - right)

    svg = [
        (
            f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {width} {height}" '
            'role="img" aria-label="Speedup relative to the baseline for the same workload">'
        ),
        f'<rect width="{width}" height="{height}" rx="16" fill="#fff"/>',
        '<g font-family="system-ui,sans-serif" font-size="13" fill="#334155">',
        '<text x="20" y="27" font-size="16" font-weight="700">'
        + ("Kernel speedup" if metric == "kernel_seconds" else "Whole-process speedup")
        + "</text>",
        (
            f'<text x="20" y="48">Baseline: {html.escape(report["baseline"])} · '
            "log₂ scale · larger ratios are faster</text>"
        ),
    ]
    zero = x(0)
    svg.append(f'<path d="M {zero:.2f} 60 V {height - 18}" stroke="#94a3b8"/>')
    svg.append(f'<text x="{zero + 4:.2f}" y="65">1×</text>')
    for index, row in enumerate(rows):
        y = 80 + index * 31
        value = row["speedup"][metric]
        endpoint = x(math.log2(value))
        label = html.escape(f"{row['case']} · {row['runtime']}")
        svg.append(f'<text x="20" y="{y + 14}" font-size="11">{label}</text>')
        svg.append(
            f'<rect x="{min(zero, endpoint):.2f}" y="{y}" '
            f'width="{max(1, abs(endpoint - zero)):.2f}" height="19" rx="3" '
            f'fill="{COLORS[runtime_names.index(row["runtime"]) % len(COLORS)]}"/>'
        )
        svg.append(f'<text x="{width - right + 12}" y="{y + 14}">{value:.3g}×</text>')
    if not rows:
        svg.append('<text x="20" y="89">No validated comparisons yet.</text>')
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
            speedup = row.get("speedup", {}).get("kernel_seconds")
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
    return f"""<!doctype html>
<html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width">
<title>Mule performance · {stamp}</title>
<style>
body{{margin:0;background:#f1f5f9;color:#0f172a;font:15px system-ui,sans-serif}}
main{{max-width:1160px;margin:auto;padding:36px 24px}}
header{{background:linear-gradient(125deg,#172554,#312e81);color:white;border-radius:22px;padding:30px}}
h1{{font-size:34px;margin:0 0 12px}}p{{line-height:1.6}}header p{{color:#c7d2fe}}
.pill{{display:inline-block;padding:7px 12px;background:#ffffff18;border-radius:10px;margin:0 8px 8px 0}}
a{{color:#4338ca}}header a{{color:#a5f3fc}}section{{margin:24px 0}}
img{{width:100%;display:block;border-radius:16px}}.table{{overflow:auto;background:white;border-radius:16px}}
table{{width:100%;border-collapse:collapse;font-size:13px}}th,td{{text-align:left;padding:12px;border-bottom:1px solid #e2e8f0}}
th{{background:#e0e7ff}}small{{display:block;color:#64748b}}pre{{white-space:pre-wrap;overflow-wrap:anywhere}}
</style><main><header><h1>Mule performance laboratory</h1>
<p>{platform}<br>{stamp}</p>
<span class="pill">Baseline: {baseline}</span><span class="pill">{successful} validated / {len(report["rows"])} comparisons</span>
<p><a href="results.json">Raw results &amp; provenance</a> · <a href="kernel.svg">Kernel SVG</a> · <a href="process.svg">Process SVG</a></p></header>
<section><p>Preparation and result checks are excluded from kernel time. Whole-process time includes
startup, preparation, warmups, and measured calls; compilation is separate. Each ratio compares
the same scenario and input. σ describes sample variation, not a confidence interval.
Short kernels can be dominated by measurement overhead; increase workload size before drawing conclusions.</p></section>
<section><img src="kernel.svg" alt="Kernel speedup chart"></section>
<section><img src="process.svg" alt="Whole-process speedup chart"></section>
<section class="table"><table><thead><tr><th>Scenario</th><th>Runtime</th><th>Status</th>
<th>Kernel median</th><th>Process median</th><th>Kernel speedup</th></tr></thead><tbody>{content}</tbody></table></section>
<p>Each scenario has one implementation. Failures contribute no speedup.</p></main></html>"""


def write_report(report, directory):
    (directory / "results.json").write_text(json.dumps(report, indent=2) + "\n")
    (directory / "kernel.svg").write_text(chart(report, "kernel_seconds"))
    (directory / "process.svg").write_text(chart(report, "process_seconds"))
    (directory / "index.html").write_text(dashboard(report))
