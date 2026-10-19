import xml.etree.ElementTree as ET

import pytest

from perf.suite.perfutils.report import chart, dashboard

NS = {"svg": "http://www.w3.org/2000/svg"}


def report(ratios=(0.25, 1.0, 4.0)):
    return {
        "baseline": "cpython",
        "created_utc": "20261019T011706.939668Z",
        "platform": "Linux & x86-64",
        "options": {"samples": 5, "seed": 42},
        "runtimes": [{"name": "cpython"}, {"name": "mule-o2"}],
        "rows": [
            {
                "case": f"scenario_{index}",
                "runtime": "mule-o2",
                "status": "ok",
                "speedup": {"kernel_seconds": ratio},
            }
            for index, ratio in enumerate(ratios)
        ],
    }


def test_log_plot_places_equal_ratio_steps_at_equal_distances():
    root = ET.fromstring(chart(report(), "kernel_seconds"))
    points = root.findall(".//*[@data-ratio]")
    xs = [float(point.attrib["cx"]) for point in points]
    assert len(xs) == 3
    assert xs[0] < xs[1] < xs[2]
    assert xs[1] - xs[0] == pytest.approx(xs[2] - xs[1], abs=0.02)
    baseline = root.find(".//*[@id='baseline']")
    assert baseline is not None
    assert float(baseline.attrib["x1"]) == pytest.approx(xs[1], abs=0.02)


@pytest.mark.parametrize("ratio", [0, -1, float("nan"), float("inf"), None])
def test_invalid_ratios_remain_visible_without_plotted_values(ratio):
    root = ET.fromstring(chart(report((ratio,)), "kernel_seconds"))
    assert root.findall(".//*[@data-ratio]") == []
    text = " ".join(root.itertext())
    assert "scenario 0" in text
    assert "not available" in text


def test_failed_comparison_never_contributes_a_mark_even_with_stale_ratio():
    source = report((100,))
    source["rows"][0]["status"] = "incorrect_result"
    root = ET.fromstring(chart(source, "kernel_seconds"))
    assert root.findall(".//*[@data-ratio]") == []
    assert "incorrect_result" in " ".join(root.itertext())


def test_svg_escapes_labels_and_removes_redundant_baseline_points():
    source = report((2,))
    source["rows"][0]["case"] = 'parse <tag> & "value"'
    source["rows"].append(
        {"case": "baseline", "runtime": "cpython", "status": "ok",
         "speedup": {"kernel_seconds": 1}}
    )
    root = ET.fromstring(chart(source, "kernel_seconds"))
    assert len(root.findall(".//*[@data-ratio]")) == 1
    assert 'parse <tag> & "value"' in " ".join(root.itertext())
    assert root.find("svg:title", NS) is not None
    assert root.find("svg:desc", NS) is not None


@pytest.mark.parametrize("baseline_only", [False, True])
def test_empty_and_baseline_only_reports_render_without_candidate_points(baseline_only):
    source = report(())
    if baseline_only:
        source["rows"] = [{"case": "one", "runtime": "cpython", "status": "ok"}]
    root = ET.fromstring(chart(source, "kernel_seconds"))
    assert root.findall(".//*[@data-ratio]") == []


def test_dashboard_escapes_failure_details_and_provenance():
    source = report((2,))
    source["rows"][0].update(status="build_failed", error="<script>bad()</script>")
    source["runtimes"][1]["version"] = "<version & build>"
    page = dashboard(source)
    assert "&lt;script&gt;bad()&lt;/script&gt;" in page
    assert "&lt;version &amp; build&gt;" in page
    assert "<script>" not in page
