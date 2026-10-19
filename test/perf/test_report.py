import xml.etree.ElementTree as ET

import pytest

from perf.suite.perfutils.report import chart, dashboard

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
                "speedup": {"kernel_seconds": 999},
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
