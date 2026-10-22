import xml.etree.ElementTree as ET

import pytest

from perf.suite.perfutils.report import chart, dashboard, update_site

NS = {"svg": "http://www.w3.org/2000/svg"}


def report(seconds=(0.010, 0.020, 0.040)):
    runtimes = ("cpython", "mule-o2", "sbcl")
    return {
        "baseline": "cpython",
        "created_utc": "20261019T011706.939668Z",
        "platform": "Linux & x86-64",
        "options": {"samples": 5, "seed": 42},
        "runtimes": [{"name": name} for name in runtimes],
        "rows": [
            {
                "case": "scenario_one",
                "runtime": name,
                "status": "ok",
                "summary": {"kernel_seconds": {"median": value}},
                "time_ratio": {"kernel_seconds": 999},
            }
            for name, value in zip(runtimes, seconds, strict=False)
        ],
    }


def bars(root):
    return root.findall(".//svg:rect[@data-ms]", NS)


def test_vertical_bars_include_baseline_and_scale_linearly_from_zero():
    root = ET.fromstring(chart(report(), "kernel_seconds"))
    plotted = bars(root)
    assert [bar.attrib["data-runtime"] for bar in plotted] == [
        "cpython",
        "mule-o2",
        "sbcl",
    ]
    heights = [float(bar.attrib["height"]) for bar in plotted]
    assert heights[1] == pytest.approx(2 * heights[0], abs=0.02)
    assert heights[2] == pytest.approx(4 * heights[0], abs=0.02)
    zero = root.find(".//*[@data-zero-y]")
    assert zero is not None
    for bar in plotted:
        assert float(bar.attrib["y"]) + float(bar.attrib["height"]) == pytest.approx(
            float(zero.attrib["data-zero-y"]), abs=0.02
        )
    assert [float(bar.attrib["data-ms"]) for bar in plotted] == [10, 20, 40]
    assert "10 ms" in " ".join(root.itertext())


def test_equal_times_have_equal_heights_and_runtime_labels_below_bars():
    root = ET.fromstring(chart(report((0.1, 0.1, 0.1)), "kernel_seconds"))
    assert len({bar.attrib["height"] for bar in bars(root)}) == 1
    labels = root.findall(".//svg:text[@data-runtime]", NS)
    assert len(labels) == 3
    for label, bar in zip(labels, bars(root), strict=True):
        assert float(label.attrib["y"]) > float(bar.attrib["y"]) + float(
            bar.attrib["height"]
        )


@pytest.mark.parametrize("seconds", [-1, float("nan"), float("inf"), None])
def test_invalid_times_remain_visible_without_misleading_bars(seconds):
    root = ET.fromstring(chart(report((seconds,)), "kernel_seconds"))
    assert bars(root) == []
    assert "not available" in " ".join(root.itertext())


def test_zero_elapsed_time_is_a_zero_height_bar():
    root = ET.fromstring(chart(report((0,)), "kernel_seconds"))
    assert float(bars(root)[0].attrib["height"]) == 0


def test_failed_comparison_never_plots_stale_timing():
    source = report((0.1,))
    source["rows"][0]["status"] = "incorrect_result"
    root = ET.fromstring(chart(source, "kernel_seconds"))
    assert bars(root) == []
    assert "incorrect_result" in " ".join(root.itertext())


def test_scenarios_use_independent_scales_with_consistent_runtime_order():
    source = report()
    source["rows"] += [
        dict(
            row,
            case="scenario_two",
            summary={
                "kernel_seconds": {
                    "median": row["summary"]["kernel_seconds"]["median"] * 100
                }
            },
        )
        for row in reversed(source["rows"])
    ]
    root = ET.fromstring(chart(source, "kernel_seconds"))
    panels = root.findall(".//svg:g[@data-case]", NS)
    assert len(panels) == 2
    first, second = [bars(panel) for panel in panels]
    assert [bar.attrib["data-runtime"] for bar in first] == [
        bar.attrib["data-runtime"] for bar in second
    ]
    assert [bar.attrib["height"] for bar in first] == [
        bar.attrib["height"] for bar in second
    ]
    assert [bar.attrib["fill"] for bar in first] == [
        bar.attrib["fill"] for bar in second
    ]


def test_svg_escapes_labels_and_contains_accessible_descriptions():
    source = report((0.1,))
    source["rows"][0]["case"] = 'parse <tag> & "value"'
    root = ET.fromstring(chart(source, "kernel_seconds"))
    assert 'parse <tag> & "value"' in " ".join(root.itertext())
    assert root.find("svg:title", NS) is not None
    assert root.find("svg:desc", NS) is not None


def test_empty_report_and_missing_runtime_rows_render_without_bars():
    root = ET.fromstring(chart(report(()), "kernel_seconds"))
    assert bars(root) == []
    source = report((0.1,))
    root = ET.fromstring(chart(source, "kernel_seconds"))
    assert len(bars(root)) == 1
    assert "missing" in " ".join(root.itertext())


def test_dashboard_escapes_failure_details_and_provenance():
    source = report((0.1,))
    source["rows"][0].update(status="build_failed", error="<script>bad()</script>")
    source["runtimes"][1]["version"] = "<version & build>"
    page = dashboard(source)
    assert "&lt;script&gt;bad()&lt;/script&gt;" in page
    assert "&lt;version &amp; build&gt;" in page
    assert "<script>" not in page


@pytest.mark.parametrize("ratio", [0, 0.5, 1, 3])
def test_dashboard_displays_elapsed_time_ratio(ratio):
    source = report((0.1,))
    row = source["rows"][0]
    row["summary"] = {
        metric: {"median": 0.1, "stdev": 0}
        for metric in ("kernel_seconds", "process_seconds")
    }
    row["time_ratio"]["kernel_seconds"] = ratio
    page = dashboard(source)
    assert f"<td>{ratio:.3g}</td>" in page


@pytest.mark.parametrize("ratio", [None, -1, float("nan"), float("inf")])
def test_dashboard_displays_unavailable_ratio(ratio):
    source = report((0.1,))
    row = source["rows"][0]
    row["summary"] = {
        metric: {"median": 0.1, "stdev": 0}
        for metric in ("kernel_seconds", "process_seconds")
    }
    row["time_ratio"]["kernel_seconds"] = ratio
    assert "<td>—</td>" in dashboard(source)


def test_update_site_writes_assets_and_updates_captions(tmp_path):
    (tmp_path / "docs/assets").mkdir(parents=True)
    (tmp_path / "docs/index.html").write_text(
        "<figcaption>October 18, 2026 · 14 workloads</figcaption>\n"
        "<p>Measured checkout: <code>2d5e473ee</code> with uncommitted tracked changes.</p>\n",
        encoding="utf-8",
    )
    (tmp_path / "docs/README.md").write_text(
        "preserves the complete recorded suite run from\n"
        "`20261018T000000.000000Z`. The measured checkout was `2d5e473ee` with\n"
        "uncommitted tracked changes, identified by its recorded diff digest.\n",
        encoding="utf-8",
    )
    (tmp_path / "AGENTS.md").write_text(
        "from the recorded October 18,\n"
        "2026 suite run.\n"
        "The measured checkout\n"
        "was `2d5e473ee` with uncommitted tracked changes.\n",
        encoding="utf-8",
    )
    source = report((0.1, 0.2, 0.3))
    source["created_utc"] = "20261019T173501.070793Z"
    source["runtimes"][1]["revision"] = {
        "commit": "ae24f3227abcdef0123456789",
        "tracked_diff_sha256": "dirty",
    }
    for row in source["rows"]:
        row["summary"]["process_seconds"] = {"median": 0.1, "stdev": 0.01}
        row["summary"]["kernel_seconds"]["stdev"] = 0.01
    update_site(source, tmp_path)
    assert (tmp_path / "docs/assets/results.json").is_file()
    assert (tmp_path / "docs/assets/kernel.svg").is_file()
    assert (tmp_path / "docs/assets/process.svg").is_file()
    assert (tmp_path / "docs/assets/index.html").is_file()
    assert "October 19, 2026 · " in (tmp_path / "docs/index.html").read_text()
    assert "<code>ae24f3227</code>" in (tmp_path / "docs/index.html").read_text()
    assert "20261019T173501.070793Z" in (tmp_path / "docs/README.md").read_text()
    assert "`ae24f3227`" in (tmp_path / "docs/README.md").read_text()
    assert "October 19,\n2026 suite run." in (tmp_path / "AGENTS.md").read_text()
    assert "was `ae24f3227`" in (tmp_path / "AGENTS.md").read_text()
